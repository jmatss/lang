use std::collections::{hash_map::Entry, HashMap, HashSet};

use log::debug;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    hash::DerefType,
    hash_map::TyEnvHashMap,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::{
        contains::contains_generic_shallow,
        get::{get_gens, get_ident},
        is::is_solved,
        replace::replace_gen_impls,
        to_string::to_string_type_id,
        ty::{SolveCond, Ty},
        type_id::TypeId,
    },
};

use crate::ty::solve::set_generic_names;

use super::nested_collector::GenericNestedCollector;

/// Iterates through the tokens and gathers all ADTs containing generics.
/// Only the ADTs implementing the generic will be stored, any ADT containing a
/// generic declaration ex. `Struct<T>` will NOT be saved, only ex.
/// "Struct<i64>" & "Struct<f32>".
///
/// This information will be used to create new instances of the ADTs where
/// the generics are "implemented". Ex. "Struct<T>" might be implemented as
/// "Struct<i64>" & "Struct<f32>".
pub struct GenericAdtCollector<'a> {
    /// Will contain all types containing generics. This will then be used to
    /// create new generic ADTs with the specific generic implementations.
    /// The LangPath key is the path of the ADT/type. The TypeId values are the
    /// actual types containing generic implentations/instances.
    pub generic_adts: TyEnvHashMap<LangPath, Vec<TypeId>>,

    /// Will contain all NESTED types containing generics. These are types containing
    /// generics that aren't part of the "using" structure.
    ///
    /// # Example
    ///
    /// If there are two structs `A` and `B`, both containing generic parameters.
    /// If `A` contains a function that creates a `B`, where the generic type
    /// for `B` is the generic type for `A`, that use will be considered a nested
    /// generic struct.
    ///
    /// ```no_run
    /// struct B<T2>;
    /// struct A<T1>;
    /// impl A
    /// {
    ///     fn test_func()
    ///     {
    ///         var b = B<T1>{}
    ///     }
    /// }
    /// ```
    ///
    /// These cases are special since the generic `T1` in this case isn't replaced
    /// until a later stage. All other types of generics are solved at this stage.
    /// This means that the use of `B<T1>` is not solved and thus structs of `B`
    /// will not be created for every `T1`.
    ///
    /// The entrys in this map will be moved/converted into `self.generic_adts`
    /// before this stage is done. This one will be left empty.
    ///
    /// The first LangPath key is the path of the containing ADT (`A` in the example)
    /// and the second String key is the name of the method in which the vector
    /// of types was found in.
    nested_generic_adts: TyEnvHashMap<LangPath, HashMap<String, Vec<TypeId>>>,

    /// A list of all ADT's in the dependecy order. This list will be in reverse
    /// order compared to how it is calculated. This means that ADT `A` depending
    /// on ADT `B` would come before in the list. This is done since `A` should
    /// "expand" all its generic implementations before `B` is "calculated".
    dependency_order_rev: &'a [LangPath],

    errors: Vec<LangError>,
}

impl<'a> GenericAdtCollector<'a> {
    pub fn new(dependency_order_rev: &'a [LangPath]) -> Self {
        Self {
            generic_adts: TyEnvHashMap::default(),
            nested_generic_adts: TyEnvHashMap::default(),
            dependency_order_rev,
            errors: Vec::default(),
        }
    }

    /// If the given type represents a type that contains generics, this function
    /// will insert those into `self.generic_adts`. This map will in a later
    /// stage be used to create all the ADTs containing the different generic types.
    /// This function also adds the names for the generics if they aren't already
    /// set and that information is attainable.
    fn collect_generic_adt(&mut self, ctx: &mut TraverseCtx, type_id: TypeId) -> LangResult<()> {
        // Do not create a "copy" of the actual ADT type that contains the
        // generic declarations, should only create "copies" for the ADTs
        // that "implements" the generics.
        if contains_generic_shallow(&ctx.ty_env.lock().unwrap(), type_id)? {
            return Ok(());
        }

        let ty_clone = ctx.ty_env.lock().unwrap().ty_clone(type_id)?;

        let adt_path_with_gens = match ty_clone {
            Ty::CompoundType(inner_ty, ..) if inner_ty.gens().is_some() => {
                if let Some(adt_path) = inner_ty.get_ident() {
                    adt_path.without_gens()
                } else {
                    return Ok(());
                }
            }

            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                return self.collect_generic_adt(ctx, type_id_i);
            }

            _ => return Ok(()),
        };

        let mut ty_env_guard = ctx.ty_env.lock().unwrap();

        // Set names of generics if they aren't set already.
        set_generic_names(&mut ty_env_guard, &ctx.ast_ctx, type_id)?;

        let deref_type = DerefType::None;
        let contains_key =
            self.generic_adts
                .contains_key(&ty_env_guard, deref_type, &adt_path_with_gens)?;

        if contains_key {
            let type_ids = self
                .generic_adts
                .get_mut(&ty_env_guard, deref_type, &adt_path_with_gens)?
                .unwrap();

            if !type_ids.contains(&type_id) {
                type_ids.push(type_id);
            }
        } else {
            let type_ids = vec![type_id];
            self.generic_adts
                .insert(&ty_env_guard, deref_type, adt_path_with_gens, type_ids)?;
        }

        Ok(())
    }

    fn collect_nested(
        &mut self,
        ctx: &mut TraverseCtx,
        ast_tokens: &mut [AstToken],
    ) -> LangResult<()> {
        for ast_token in ast_tokens {
            let header = if let AstToken::Block(Block { header, .. }) = ast_token {
                header
            } else {
                continue;
            };

            let adt_path_without_gens = match header {
                BlockHeader::Implement(adt_path, ..) => adt_path.without_gens(),
                BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                    let adt = adt.read().unwrap();
                    adt.module.clone_push(&adt.name, None, Some(adt.file_pos))
                }
                _ => continue,
            };

            let mut collector = GenericNestedCollector::new();
            if let Err(mut errs) = traverse(ctx, &mut collector, ast_token) {
                self.errors.append(&mut errs);
            }

            let ty_env_guard = ctx.ty_env.lock().unwrap();

            for (method_name, type_ids) in collector.nested_generic_adts.iter() {
                for type_id in type_ids {
                    let deref_type = DerefType::None;
                    let contains_key = self.nested_generic_adts.contains_key(
                        &ty_env_guard,
                        deref_type,
                        &adt_path_without_gens,
                    )?;

                    if contains_key {
                        let inner_map = self
                            .nested_generic_adts
                            .get_mut(&ty_env_guard, deref_type, &adt_path_without_gens)?
                            .unwrap();

                        match inner_map.entry(method_name.clone()) {
                            Entry::Occupied(mut o_inner) => {
                                if !o_inner.get().contains(type_id) {
                                    o_inner.get_mut().push(*type_id);
                                }
                            }
                            Entry::Vacant(v_inner) => {
                                v_inner.insert(vec![*type_id]);
                            }
                        }
                    } else {
                        let mut m = HashMap::default();
                        m.insert(method_name.clone(), vec![*type_id]);
                        self.nested_generic_adts.insert(
                            &ty_env_guard,
                            deref_type,
                            adt_path_without_gens.clone(),
                            m,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    fn convert_nested_to_regular(&mut self, ctx: &mut TraverseCtx) -> LangResult<()> {
        debug!("self.nested_generic_adts: {:#?}", self.nested_generic_adts);

        // Converts all `self.nested_generic_adts` into regular `self.generic_adts`.
        for adt_path in self.dependency_order_rev.iter() {
            let adt_path_without_gens = adt_path.without_gens();

            let nested_gen_adt = {
                let deref_type = DerefType::None;
                let ty_env_guard = ctx.ty_env.lock().unwrap();

                if self.nested_generic_adts.contains_key(
                    &ty_env_guard,
                    deref_type,
                    &adt_path_without_gens,
                )? {
                    self.nested_generic_adts
                        .remove(&ty_env_guard, deref_type, &adt_path_without_gens)?
                        .unwrap()
                } else {
                    continue;
                }
            };

            for nested_adt_type_ids in nested_gen_adt.values() {
                for nested_adt_type_id in nested_adt_type_ids {
                    let path_res = get_ident(&ctx.ty_env.lock().unwrap(), *nested_adt_type_id)?;
                    let nested_adt_path_without_gens = match path_res {
                        Some(path) => path.without_gens(),
                        None => {
                            return Err(ctx.ast_ctx.err(format!(
                                "Nested ADT type doesn't contain ident: {:#?}",
                                nested_adt_type_id
                            )));
                        }
                    };

                    self.convert_nested_adt_from_adt(
                        ctx,
                        &adt_path_without_gens,
                        *nested_adt_type_id,
                        &nested_adt_path_without_gens,
                    )?;
                }
            }
        }

        Ok(())
    }

    fn convert_nested_adt_from_adt(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path_without_gens: &LangPath,
        nested_adt_type_id: TypeId,
        nested_adt_path_without_gens: &LangPath,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock().unwrap();

        let gen_adt_type_ids = if let Some(gen_adt_type_ids) =
            self.generic_adts
                .get(&ty_env_guard, DerefType::None, &adt_path_without_gens)?
        {
            gen_adt_type_ids.clone()
        } else {
            // This can happen if there are no implementation of the `outer` ADT.
            // If that is the case, the `inner` nested ADT will never be used.
            return Ok(());
        };

        // Go through all implementations of the ADT `adt_path_without_gens` and
        // use those generic instances to create new ADT instances for the nested
        // one.
        for gen_adt_type_id in gen_adt_type_ids {
            set_generic_names(&mut ty_env_guard, &ctx.ast_ctx, gen_adt_type_id)?;

            let gens = if let Some(gens) = get_gens(&ty_env_guard, gen_adt_type_id)? {
                gens.clone()
            } else {
                return Err(ctx.ast_ctx.err(format!(
                    "Expected type with id {} to contain generics.",
                    gen_adt_type_id
                )));
            };

            let new_nested_adt_type_id = if let Some(new_type_id) =
                replace_gen_impls(&mut ty_env_guard, &ctx.ast_ctx, nested_adt_type_id, &gens)?
            {
                new_type_id
            } else {
                // Since we at this point knows that `nested_adt_type_id` contains
                // generics; if no Generic/GenericInstance was replaced in the
                // if-statement above, it means that the `generics` variable gotten
                // from the ADT doesn't have a replacement for that generic.
                //
                // Since generics from function declaration are solved already,
                // this should be an already solved generic. Assume that is the
                // case and the inferred type will replace this generic just before
                // the new ADT is created in `generic_adt_creator.rs`.
                let check_inf = true;
                let solve_cond = SolveCond::new().excl_gen_inst().excl_gen();
                if is_solved(&ty_env_guard, nested_adt_type_id, check_inf, solve_cond)? {
                    nested_adt_type_id
                } else {
                    return Err(LangError::new(
                        format!(
                            "Found unsolvable generics when converting nested ADT to ADT. \
                            Nested ADT ty: {:#?}, generic instances to replace with: {:#?}",
                            ty_env_guard.ty(nested_adt_type_id),
                            gens
                        ),
                        LangErrorKind::AnalyzeError,
                        None,
                    ));
                }
            };

            let deref_type = DerefType::None;
            let contains_key = self.generic_adts.contains_key(
                &ty_env_guard,
                deref_type,
                nested_adt_path_without_gens,
            )?;

            if contains_key {
                let type_ids = self
                    .generic_adts
                    .get_mut(&ty_env_guard, deref_type, nested_adt_path_without_gens)?
                    .unwrap();

                if !type_ids.contains(&new_nested_adt_type_id) {
                    type_ids.push(new_nested_adt_type_id);
                }
            } else {
                let new_nested_adt_type_ids = vec![new_nested_adt_type_id];
                self.generic_adts.insert(
                    &ty_env_guard,
                    deref_type,
                    nested_adt_path_without_gens.clone(),
                    new_nested_adt_type_ids,
                )?;
            }
        }

        Ok(())
    }

    /// Removes duplicates from `self.generic_adts`. Since types might be identical
    /// even though they might refer to different type IDs, this function will
    /// convert every type to its final string from and compare those to find
    /// duplicates instead of comparing type IDs.
    fn remove_duplicates(&mut self, ctx: &TraverseCtx) -> LangResult<()> {
        for type_ids in self.generic_adts.values_mut() {
            let mut seen_ty_strings = HashSet::new();
            let mut i = 0;

            while i < type_ids.len() {
                let type_id = *type_ids.get(i).unwrap();
                let ty_string = to_string_type_id(&ctx.ty_env.lock().unwrap(), type_id)?;

                if seen_ty_strings.contains(&ty_string) {
                    type_ids.swap_remove(i);
                } else {
                    seen_ty_strings.insert(ty_string);
                    i += 1;
                }
            }
        }

        Ok(())
    }
}

impl<'a> Visitor for GenericAdtCollector<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Default,
            body,
            ..
        } = &mut block
        {
            if let Err(err) = self.collect_nested(ctx, body) {
                self.errors.push(err);
            }
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.collect_generic_adt(ctx, *type_id) {
            self.errors.push(err);
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        if let Err(err) = self.convert_nested_to_regular(ctx) {
            self.errors.push(err);
            return;
        }

        debug!(
            "Before removal of duplicates -- self.generic_adts: {:#?}",
            self.generic_adts
        );

        if let Err(err) = self.remove_duplicates(ctx) {
            self.errors.push(err);
            return;
        }

        for type_ids in self.generic_adts.values_mut() {
            for type_id in type_ids {
                match ctx.ty_env.lock().unwrap().inferred_type(*type_id) {
                    Ok(inf_type_id) => *type_id = inf_type_id,
                    Err(err) => self.errors.push(err),
                }
            }
        }

        debug!(
            "After removal of duplicates -- self.generic_adts: {:#?}",
            self.generic_adts
        );
    }
}