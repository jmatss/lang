use std::collections::{hash_map::Entry, HashMap, HashSet};

use either::Either;
use log::debug;

use common::{
    ctx::ast_ctx::AstCtx,
    eq::{generics_eq, path_eq},
    error::{LangError, LangErrorKind, LangResult},
    hash::DerefType,
    hash_map::TyEnvHashMap,
    hash_set::TyEnvHashSet,
    order::order_step2_strings,
    path::LangPath,
    traverse::traverse_ctx::TraverseCtx,
    ty::{
        generics::Generics,
        get::get_gens,
        is::is_solved,
        replace::replace_gen_impls,
        to_string::{to_string_path, to_string_type_id},
        ty::SolveCond,
        ty_env::TyEnv,
        type_id::TypeId,
    },
};

use crate::ty::solve::set_generic_names;

use super::nested_collector::{NestedAdtInfo, NestedFnInfo};

// TODO: Merge logic between the convert functions. A lot of the logic is shared
//       and can be moved into shared functions to prevent all the duplicate code.

/// Converts "nested generic" ADTs and methods/functions into "regular generic"
/// ADTs and methods/functions. See the definition of "nested generics" in the
/// `GenericNestedCollector` struct.
pub struct GenericNestedConverter<'a> {
    /// These generics was collected in `GenericCollector`.
    generic_adts: &'a mut TyEnvHashMap<LangPath, Vec<TypeId>>,
    generic_methods: &'a mut TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,
    generic_fns: &'a mut TyEnvHashMap<LangPath, Vec<Generics>>,

    /// A list of all ADT's in the dependecy order. This list will be in reverse
    /// order compared to how it is calculated. This means that ADT `A` depending
    /// on ADT `B` would come before in the list. This is done since `A` should
    /// "expand" all its generic implementations before `B` is "calculated".
    dependency_order_rev: &'a [LangPath],

    errors: Vec<LangError>,
}

impl<'a> GenericNestedConverter<'a> {
    pub fn new(
        dependency_order_rev: &'a [LangPath],
        generic_adts: &'a mut TyEnvHashMap<LangPath, Vec<TypeId>>,
        generic_methods: &'a mut TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,
        generic_fns: &'a mut TyEnvHashMap<LangPath, Vec<Generics>>,
    ) -> Self {
        Self {
            generic_adts,
            generic_methods,
            generic_fns,
            dependency_order_rev,
            errors: Vec::default(),
        }
    }

    pub fn convert_nested_adts(
        &mut self,
        ctx: &mut TraverseCtx,
        nested_generic_adts: &'a TyEnvHashMap<NestedAdtInfo, Vec<TypeId>>,
    ) -> LangResult<()> {
        let mut nested_adt_in_adt: TyEnvHashMap<_, Vec<_>> = TyEnvHashMap::default();
        let mut nested_adt_in_fn = Vec::default();

        // There are two ways that one needs to convert the nested ADTs. They
        // are either found in a method(/adt) or a free-standing function.
        // In the method/adt case, we want to do fast look-ups on the ADT path,
        // that is why the hashmap is used.
        for entry in nested_generic_adts.iter() {
            match &entry.0.fn_decl_adt_or_module {
                Either::Left(adt_path) => {
                    // TODO: Implemet `entry` function in `TyEnvHashMap` and
                    //       change all uses of this logic to the new entry logic.
                    let contains = nested_adt_in_adt.contains_key(
                        &ctx.ty_env.lock(),
                        DerefType::None,
                        adt_path,
                    )?;
                    if contains {
                        let inner_vec = nested_adt_in_adt
                            .get_mut(&ctx.ty_env.lock(), DerefType::None, adt_path)?
                            .unwrap();
                        inner_vec.push(entry);
                    } else {
                        nested_adt_in_adt.insert(
                            &ctx.ty_env.lock(),
                            DerefType::Deep,
                            adt_path.clone(),
                            vec![entry],
                        )?;
                    }
                }
                Either::Right(_) => {
                    nested_adt_in_fn.push(entry);
                }
            }
        }

        // Convert all nested ADTs found in other ADTs(/methods) into regular
        // `self.generic_adts`.
        for adt_path in self.dependency_order_rev.iter() {
            let adt_path_without_gens = adt_path.without_gens();
            let nested_adts = {
                let deref_type = DerefType::None;
                let ty_env_guard = ctx.ty_env.lock();

                if let Some(nested_adts) =
                    nested_adt_in_adt.get(&ty_env_guard, deref_type, &adt_path_without_gens)?
                {
                    nested_adts
                } else {
                    continue;
                }
            };

            for (adt_info, nested_adt_type_ids) in nested_adts {
                for nested_adt_type_id in nested_adt_type_ids.iter() {
                    self.convert_nested_adt_in_adt(ctx, adt_info, *nested_adt_type_id)?;
                }
            }
        }

        // Convert all nested ADTs found in free-standing functions.
        for (adt_info, nested_adt_type_ids) in nested_adt_in_fn {
            for nested_adt_type_id in nested_adt_type_ids {
                self.convert_nested_adt_in_fn(ctx, adt_info, *nested_adt_type_id)?;
            }
        }

        self.remove_duplicates(ctx)
    }

    pub fn convert_nested_fns(
        &mut self,
        ctx: &mut TraverseCtx,
        nested_generic_fns: &'a TyEnvHashMap<NestedFnInfo, Vec<Generics>>,
    ) -> LangResult<()> {
        let mut converted_fns = TyEnvHashSet::default();

        // Convert all nested methods that is a call to a method in its own
        // ADT into regular `self.generic_methods`. They need special logic
        // since the parent method needs to be "expanded"/"created" before the
        // nested child method.
        for adt_path in self.dependency_order_rev.iter() {
            let nested_method_order =
                match Self::order(&ctx.ty_env.lock(), adt_path, nested_generic_fns) {
                    Ok(order) => order,
                    Err(err) => {
                        self.errors.push(err);
                        continue;
                    }
                };

            for (method_info, gens) in nested_method_order {
                for nested_gens in gens {
                    self.convert_nested_method_in_method(ctx, method_info, nested_gens)?;
                }
                converted_fns.insert(&ctx.ty_env.lock(), DerefType::Deep, method_info)?;
            }
        }

        // Convert the rest of the nested methods/functions into "reglar" generic
        // methods/functions.
        for (fn_info, gens) in nested_generic_fns.iter() {
            if !converted_fns.contains(&ctx.ty_env.lock(), DerefType::Deep, &fn_info)? {
                let fn_info = fn_info.clone();
                let gens = gens.clone();
                for nested_gens in gens {
                    match (
                        &fn_info.fn_decl_adt_or_module,
                        &fn_info.fn_call_adt_or_module,
                    ) {
                        // Nested method in method.
                        (Either::Left(_), Either::Left(_)) => {
                            self.convert_nested_method_in_method(ctx, &fn_info, &nested_gens)?
                        }
                        // Nested free-standing function in method.
                        (Either::Left(_), Either::Right(_)) => {
                            self.convert_nested_fn_in_method(ctx, &fn_info, &nested_gens)?
                        }
                        // Nested method in free-standing function.
                        (Either::Right(_), Either::Left(_)) => {
                            self.convert_nested_method_in_fn(ctx, &fn_info, &nested_gens)?
                        }
                        // Nested function in function.
                        (Either::Right(_), Either::Right(_)) => {
                            self.convert_nested_fn_in_fn(ctx, &fn_info, &nested_gens)?
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn convert_nested_adt_in_adt(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_info: &NestedAdtInfo,
        nested_adt_type_id: TypeId,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let fn_decl_adt_path = if let Either::Left(adt_path) = &adt_info.fn_decl_adt_or_module {
            adt_path
        } else {
            unreachable!("Should be ADT -- adt_info: {:#?}", adt_info);
        };
        let nested_adt_path_without_gens = &adt_info.adt_path;

        let adt_gens = self.get_adt_gens(&mut ty_env_guard, ctx.ast_ctx, fn_decl_adt_path)?;
        let method_gens = if let Some(fn_decl_name) = &adt_info.fn_decl_name {
            self.get_method_gens(&ty_env_guard, fn_decl_adt_path, fn_decl_name)
        } else {
            None
        };

        let gen_impls = if let Some(gen_impls) = combine_generics(method_gens, adt_gens) {
            gen_impls
        } else {
            return Ok(());
        };

        for gen_impl in gen_impls {
            let new_nested_adt_type_id = if let Some(new_type_id) = replace_gen_impls(
                &mut ty_env_guard,
                ctx.ast_ctx,
                nested_adt_type_id,
                &gen_impl,
            )? {
                new_type_id
            } else {
                // Since we at this point knows that `nested_adt_type_id` contains
                // generics; if no Generic/GenericInstance was replaced in the
                // if-statement above, it means that the `generics` variable gotten
                // from the ADT doesn't have a replacement for that generic.
                //
                // Since generics from function declaration are solved already,
                // this should be an already solved generic. Assume that this is
                // the case and the inferred type will replace this generic just
                // before the new ADT is created in `generic_adt_creator.rs`.
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
                            gen_impl,
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

    fn convert_nested_adt_in_fn(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_info: &NestedAdtInfo,
        nested_adt_type_id: TypeId,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let fn_decl_name = if let Some(fn_decl_name) = &adt_info.fn_decl_name {
            fn_decl_name
        } else {
            unreachable!("Should be fn -- adt_info: {:#?}", adt_info);
        };
        let fn_decl_full_path = if let Either::Right(module) = &adt_info.fn_decl_adt_or_module {
            module.clone_push(fn_decl_name, None, module.file_pos)
        } else {
            unreachable!("Should be fn -- adt_info: {:#?}", adt_info);
        };
        let nested_adt_path_without_gens = &adt_info.adt_path;

        let gen_impls = if let Some(gen_impls) = self.get_fn_gens(&ty_env_guard, &fn_decl_full_path)
        {
            gen_impls
        } else {
            return Ok(());
        };

        // Iterate through all uses (their generics) of the "parent" function.
        // Replace the generics referencing parent generic names in the nested
        // ADT and create a new ADT for every use of the parent function.
        for gen_impl in &gen_impls {
            let new_nested_adt_type_id = if let Some(new_type_id) =
                replace_gen_impls(&mut ty_env_guard, ctx.ast_ctx, nested_adt_type_id, gen_impl)?
            {
                new_type_id
            } else {
                // Since we at this point knows that `nested_adt_type_id` contains
                // generics; if no Generic/GenericInstance was replaced in the
                // if-statement above, it means that the `generics` variable gotten
                // from the ADT doesn't have a replacement for that generic.
                //
                // Since generics from function declaration are solved already,
                // this should be an already solved generic. Assume that this is
                // the case and the inferred type will replace this generic just
                // before the new ADT is created in `generic_adt_creator.rs`.
                let check_inf = true;
                let solve_cond = SolveCond::new().excl_gen_inst().excl_gen();
                if is_solved(&ty_env_guard, nested_adt_type_id, check_inf, solve_cond)? {
                    nested_adt_type_id
                } else {
                    return Err(LangError::new(
                        format!(
                            "Found unsolvable generics when converting nested ADT in function. \
                            Nested ADT ty: {:#?}, generic instances to replace with: {:#?}",
                            ty_env_guard.ty(nested_adt_type_id),
                            gen_impl
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

    /// The given entry should be a nested method found inside another method.
    fn convert_nested_method_in_method(
        &mut self,
        ctx: &mut TraverseCtx,
        fn_info: &NestedFnInfo,
        nested_gens: &Generics,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let fn_decl_name = &fn_info.fn_decl_name;
        let fn_decl_adt_path = if let Either::Left(adt_path) = &fn_info.fn_decl_adt_or_module {
            adt_path
        } else {
            unreachable!("Should be method -- fn_info: {:#?}", fn_info);
        };
        let fn_call_name = &fn_info.fn_call_name;
        let fn_call_adt_path = if let Either::Left(adt_path) = &fn_info.fn_call_adt_or_module {
            adt_path
        } else {
            unreachable!("Should be method -- fn_info: {:#?}", fn_info);
        };

        let adt_gens = self.get_adt_gens(&mut ty_env_guard, ctx.ast_ctx, fn_decl_adt_path)?;
        let method_gens = self.get_method_gens(&ty_env_guard, fn_decl_adt_path, fn_decl_name);

        let gen_impls = if let Some(gen_impls) = combine_generics(method_gens, adt_gens) {
            gen_impls
        } else {
            return Ok(());
        };

        // TODO: Do we need to do this or can we be sure that the `nested_gens`
        //       has the names set? In that case we can just get the names from
        //       the `nested_gens` variable.
        let method_gen_names = {
            let method = ctx
                .ast_ctx
                .get_method(&ty_env_guard, fn_call_adt_path, fn_call_name)?;
            let method = method.read();

            if let Some(method_gens) = &method.generics {
                method_gens.iter_names().cloned().collect::<Vec<_>>()
            } else {
                unreachable!(
                    "fn_call_adt_path: {}, fn_call_name: {}",
                    to_string_path(&ty_env_guard, fn_call_adt_path),
                    fn_call_name,
                );
            }
        };

        // Iterate through all uses (their generics) of the "parent" ADT and
        // method. Replace the generics referencing parent generic names in the
        // nested method and create a new method (of the nested method) for every
        // use of the parent ADT/method.
        for gen_impl in gen_impls {
            let mut new_gens = Generics::new();
            for (name, type_id) in method_gen_names.iter().zip(nested_gens.iter_types()) {
                let type_id =
                    match replace_gen_impls(&mut ty_env_guard, ctx.ast_ctx, *type_id, &gen_impl)? {
                        Some(new_type_id) => new_type_id,
                        None => *type_id,
                    };
                new_gens.insert(name.clone(), type_id);
            }

            if generics_eq(&ty_env_guard, &new_gens, nested_gens, DerefType::Deep)? {
                continue;
            }

            let contains_key = self.generic_methods.contains_key(
                &ty_env_guard,
                DerefType::Deep,
                fn_call_adt_path,
            )?;

            if contains_key {
                let map_inner = self
                    .generic_methods
                    .get_mut(&ty_env_guard, DerefType::Deep, fn_call_adt_path)?
                    .unwrap();

                match map_inner.entry(fn_call_name.clone()) {
                    Entry::Occupied(mut o_inner) => {
                        let mut contains_gens = false;
                        for inner_gens in o_inner.get() {
                            if generics_eq(&ty_env_guard, inner_gens, &new_gens, DerefType::Deep)? {
                                contains_gens = true;
                                break;
                            }
                        }

                        if !contains_gens {
                            o_inner.get_mut().push(new_gens);
                        }
                    }
                    Entry::Vacant(v_inner) => {
                        v_inner.insert(vec![new_gens.clone()]);
                    }
                }
            } else {
                let mut map_inner = HashMap::default();
                map_inner.insert(fn_call_name.clone(), vec![new_gens.clone()]);
                self.generic_methods.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    fn_call_adt_path.clone(),
                    map_inner,
                )?;
            }
        }

        Ok(())
    }

    /// The given entry should be a nested method found inside a free-standing
    /// function.
    fn convert_nested_method_in_fn(
        &mut self,
        ctx: &mut TraverseCtx,
        fn_info: &NestedFnInfo,
        nested_gens: &Generics,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let fn_decl_name = &fn_info.fn_decl_name;
        let fn_decl_full_path = if let Either::Right(module) = &fn_info.fn_decl_adt_or_module {
            module.clone_push(fn_decl_name, None, module.file_pos)
        } else {
            unreachable!("Should be fn -- fn_info: {:#?}", fn_info);
        };
        let fn_call_name = &fn_info.fn_call_name;
        let fn_call_adt_path = if let Either::Left(adt_path) = &fn_info.fn_call_adt_or_module {
            adt_path
        } else {
            unreachable!("Should be method -- fn_info: {:#?}", fn_info);
        };

        let gen_impls = if let Some(gen_impls) = self.get_fn_gens(&ty_env_guard, &fn_decl_full_path)
        {
            gen_impls
        } else {
            return Ok(());
        };

        // TODO: Do we need to do this or can we be sure that the `nested_gens`
        //       has the names set? In that case we can just get the names from
        //       the `nested_gens` variable.
        let method_gen_names = {
            let method = ctx
                .ast_ctx
                .get_method(&ty_env_guard, fn_call_adt_path, fn_call_name)?;
            let method = method.read();

            if let Some(method_gens) = &method.generics {
                method_gens.iter_names().cloned().collect::<Vec<_>>()
            } else {
                unreachable!(
                    "fn_call_adt_path: {}, fn_call_name: {}",
                    to_string_path(&ty_env_guard, fn_call_adt_path),
                    fn_call_name,
                );
            }
        };

        // Iterate through all uses (their generics) of the "parent" function.
        // Replace the generics referencing parent generic names in the nested
        // method and create a new method (of the nested method) for every
        // use of the parent function.
        for gen_impl in &gen_impls {
            let mut new_gens = Generics::new();
            for (name, type_id) in method_gen_names.iter().zip(nested_gens.iter_types()) {
                let type_id =
                    match replace_gen_impls(&mut ty_env_guard, ctx.ast_ctx, *type_id, gen_impl)? {
                        Some(new_type_id) => new_type_id,
                        None => *type_id,
                    };
                new_gens.insert(name.clone(), type_id);
            }

            // TODO: In what case can this happen? Can this be removed?
            if generics_eq(&ty_env_guard, &new_gens, nested_gens, DerefType::Deep)? {
                continue;
            }

            let contains_key = self.generic_methods.contains_key(
                &ty_env_guard,
                DerefType::Deep,
                fn_call_adt_path,
            )?;

            if contains_key {
                let map_inner = self
                    .generic_methods
                    .get_mut(&ty_env_guard, DerefType::Deep, fn_call_adt_path)?
                    .unwrap();

                match map_inner.entry(fn_call_name.clone()) {
                    Entry::Occupied(mut o_inner) => {
                        let mut contains_gens = false;
                        for inner_gens in o_inner.get() {
                            if generics_eq(&ty_env_guard, inner_gens, &new_gens, DerefType::Deep)? {
                                contains_gens = true;
                                break;
                            }
                        }

                        if !contains_gens {
                            o_inner.get_mut().push(new_gens);
                        }
                    }
                    Entry::Vacant(v_inner) => {
                        v_inner.insert(vec![new_gens.clone()]);
                    }
                }
            } else {
                let mut map_inner = HashMap::default();
                map_inner.insert(fn_call_name.clone(), vec![new_gens.clone()]);
                self.generic_methods.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    fn_call_adt_path.clone(),
                    map_inner,
                )?;
            }
        }

        Ok(())
    }

    /// The given entry should be a free-standing founction found inside another
    /// free-standing function.
    fn convert_nested_fn_in_fn(
        &mut self,
        ctx: &mut TraverseCtx,
        fn_info: &NestedFnInfo,
        nested_gens: &Generics,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let fn_decl_name = &fn_info.fn_decl_name;
        let fn_decl_full_path = if let Either::Right(module) = &fn_info.fn_decl_adt_or_module {
            module.clone_push(fn_decl_name, None, module.file_pos)
        } else {
            unreachable!("Should be fn -- fn_info: {:#?}", fn_info);
        };
        let fn_call_name = &fn_info.fn_call_name;
        let fn_call_full_path = if let Either::Right(module) = &fn_info.fn_call_adt_or_module {
            module.clone_push(fn_call_name, None, module.file_pos)
        } else {
            unreachable!("Should be fn -- fn_info: {:#?}", fn_info);
        };

        let gen_impls = if let Some(gen_impls) = self.get_fn_gens(&ty_env_guard, &fn_decl_full_path)
        {
            gen_impls
        } else {
            return Ok(());
        };

        // TODO: Do we need to do this or can we be sure that the `nested_gens`
        //       has the names set? In that case we can just get the names from
        //       the `nested_gens` variable.
        let fn_gen_names = {
            let func = ctx.ast_ctx.get_fn(&ty_env_guard, &fn_call_full_path)?;
            let func = func.read();

            if let Some(fn_gens) = &func.generics {
                fn_gens.iter_names().cloned().collect::<Vec<_>>()
            } else {
                unreachable!(
                    "fn_call_full_path: {}",
                    to_string_path(&ty_env_guard, &fn_call_full_path),
                );
            }
        };

        // Iterate through all uses (their generics) of the "parent" function.
        // Replace the generics referencing parent generic names in the nested
        // function and create a new function (of the nested function) for every
        // use of the parent function.
        for gen_impl in gen_impls {
            let mut new_gens = Generics::new();
            for (name, type_id) in fn_gen_names.iter().zip(nested_gens.iter_types()) {
                let type_id =
                    match replace_gen_impls(&mut ty_env_guard, ctx.ast_ctx, *type_id, &gen_impl)? {
                        Some(new_type_id) => new_type_id,
                        None => *type_id,
                    };
                new_gens.insert(name.clone(), type_id);
            }

            // TODO: In what case can this happen? Can this be removed?
            if generics_eq(&ty_env_guard, &new_gens, nested_gens, DerefType::Deep)? {
                continue;
            }

            let contains_key = self.generic_fns.contains_key(
                &ty_env_guard,
                DerefType::Deep,
                &fn_call_full_path,
            )?;

            if contains_key {
                let vec_inner = self
                    .generic_fns
                    .get_mut(&ty_env_guard, DerefType::Deep, &fn_call_full_path)?
                    .unwrap();

                let mut contains_gens = false;
                for inner_gens in vec_inner.iter() {
                    if generics_eq(&ty_env_guard, inner_gens, &new_gens, DerefType::Deep)? {
                        contains_gens = true;
                        break;
                    }
                }

                if !contains_gens {
                    vec_inner.push(new_gens);
                }
            } else {
                self.generic_fns.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    fn_call_full_path.clone(),
                    vec![new_gens.clone()],
                )?;
            }
        }

        Ok(())
    }

    /// The given entry should be a nested free-standing function found inside a
    /// method.
    fn convert_nested_fn_in_method(
        &mut self,
        ctx: &mut TraverseCtx,
        fn_info: &NestedFnInfo,
        nested_gens: &Generics,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let fn_decl_name = &fn_info.fn_decl_name;
        let fn_decl_adt_path = if let Either::Left(adt_path) = &fn_info.fn_decl_adt_or_module {
            adt_path
        } else {
            unreachable!("Should be method -- fn_info: {:#?}", fn_info);
        };
        let fn_call_name = &fn_info.fn_call_name;
        let fn_call_full_path = if let Either::Right(module) = &fn_info.fn_call_adt_or_module {
            module.clone_push(fn_call_name, None, module.file_pos)
        } else {
            unreachable!("Should be fn -- fn_info: {:#?}", fn_info);
        };

        let adt_gens = self.get_adt_gens(&mut ty_env_guard, ctx.ast_ctx, fn_decl_adt_path)?;
        let method_gens = self.get_method_gens(&ty_env_guard, fn_decl_adt_path, fn_decl_name);

        let gen_impls = if let Some(gen_impls) = combine_generics(method_gens, adt_gens) {
            gen_impls
        } else {
            return Ok(());
        };

        // TODO: Do we need to do this or can we be sure that the `nested_gens`
        //       has the names set? In that case we can just get the names from
        //       the `nested_gens` variable.
        let fn_gen_names = {
            let func = ctx.ast_ctx.get_fn(&ty_env_guard, &fn_call_full_path)?;
            let func = func.read();

            if let Some(fn_gens) = &func.generics {
                fn_gens.iter_names().cloned().collect::<Vec<_>>()
            } else {
                unreachable!(
                    "fn_call_full_path: {}",
                    to_string_path(&ty_env_guard, &fn_call_full_path),
                );
            }
        };

        // Iterate through all uses (their generics) of the "parent" method.
        // Replace the generics referencing parent generic names in the nested
        // function and create a new function (of the nested function) for every
        // use of the parent method.
        for gen_impl in gen_impls {
            let mut new_gens = Generics::new();
            for (name, type_id) in fn_gen_names.iter().zip(nested_gens.iter_types()) {
                let type_id =
                    match replace_gen_impls(&mut ty_env_guard, ctx.ast_ctx, *type_id, &gen_impl)? {
                        Some(new_type_id) => new_type_id,
                        None => *type_id,
                    };
                new_gens.insert(name.clone(), type_id);
            }

            // TODO: In what case can this happen? Can this be removed?
            if generics_eq(&ty_env_guard, &new_gens, nested_gens, DerefType::Deep)? {
                continue;
            }

            let contains_key = self.generic_fns.contains_key(
                &ty_env_guard,
                DerefType::Deep,
                &fn_call_full_path,
            )?;

            if contains_key {
                let vec_inner = self
                    .generic_fns
                    .get_mut(&ty_env_guard, DerefType::Deep, &fn_call_full_path)?
                    .unwrap();

                let mut contains_gens = false;
                for inner_gens in vec_inner.iter() {
                    if generics_eq(&ty_env_guard, inner_gens, &new_gens, DerefType::Deep)? {
                        contains_gens = true;
                        break;
                    }
                }

                if !contains_gens {
                    vec_inner.push(new_gens);
                }
            } else {
                self.generic_fns.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    fn_call_full_path.clone(),
                    vec![new_gens.clone()],
                )?;
            }
        }

        Ok(())
    }

    /// Figures out the order in which the nested methods for the ADT `adt_path`
    /// needs to be "handled"/ "expanded". This function should only be used for
    /// nested methods, not nested free-standing functions.
    ///
    /// The returned vector will contain the entries of methods in the order in
    /// which they should be converted to "regular" generic methods. Ensuring the
    /// correct order is needed to make sure that the a "child" method instance
    /// is create for every use of the "parent" method.
    fn order<'ord>(
        ty_env: &TyEnv,
        adt_path: &LangPath,
        nested_generic_methods: &'ord TyEnvHashMap<NestedFnInfo, Vec<Generics>>,
    ) -> LangResult<Vec<(&'ord NestedFnInfo, &'ord Vec<Generics>)>> {
        let mut nested_methods = Vec::default();
        let mut references: HashMap<String, HashSet<String>> = HashMap::default();

        // Populate the `references` map with information in which order the
        // methods reference each other.
        for entry in nested_generic_methods.iter() {
            let fn_info = entry.0;

            let nested_adt_path =
                if let Either::Left(nested_adt_path) = &fn_info.fn_call_adt_or_module {
                    nested_adt_path
                } else {
                    continue;
                };

            let parent_adt_path =
                if let Either::Left(parent_adt_path) = &fn_info.fn_decl_adt_or_module {
                    parent_adt_path
                } else {
                    continue;
                };

            // Should only return results for nested methods found in the
            // given `adt_path`.
            if path_eq(ty_env, parent_adt_path, adt_path, DerefType::Deep)? {
                nested_methods.push(entry);
            }

            // When we sort the order in which the nested methods should be
            // created, we only care about calls to methods in the same ADT.
            if !path_eq(ty_env, parent_adt_path, nested_adt_path, DerefType::Deep)? {
                continue;
            }

            let fn_decl_name = &fn_info.fn_decl_name;
            let fn_call_name = &fn_info.fn_call_name;

            match references.entry(fn_decl_name.clone()) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(fn_call_name) {
                        o.get_mut().insert(fn_call_name.clone());
                    }
                }
                Entry::Vacant(v) => {
                    let mut s = HashSet::default();
                    s.insert(fn_call_name.clone());
                    v.insert(s);
                }
            }
        }

        // No nested method calls to methods in the same ADT. This means that
        // we don't have to care about the order of those nested methods.
        // Do early return with unsorted vector.
        if references.is_empty() {
            return Ok(nested_methods);
        }

        // Figure out the order of the methods by looking at how they reference
        // each other.
        match order_step2_strings(&references) {
            Ok(order) => {
                debug!(
                    "method -- references: {:#?}, order: {:#?}",
                    references, order
                );

                // Sort the method entries according to the given `order`.
                // Any nested methods where the order doesn't matter will be
                // given `usize::MAX` as their precedence. These will be put
                // last in the vector.
                let order_lookup = order
                    .into_iter()
                    .enumerate()
                    .map(|(i, name)| (name, i))
                    .collect::<HashMap<_, _>>();
                nested_methods.sort_by(|a, b| {
                    let a_precedence = order_lookup.get(&a.0.fn_call_name).unwrap_or(&usize::MAX);
                    let b_precedence = order_lookup.get(&b.0.fn_call_name).unwrap_or(&usize::MAX);
                    a_precedence.cmp(b_precedence)
                });

                Ok(nested_methods)
            }
            Err(cyc_err) => Err(LangError::new(
                format!(
                    "Cyclic dependency between method \"{}\" and \"{}\" in ADT \"{}\".",
                    cyc_err.0,
                    cyc_err.1,
                    to_string_path(ty_env, adt_path)
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
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
                let ty_string = to_string_type_id(&ctx.ty_env.lock(), type_id)?;

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

    // Fetch all uses of generics specified on the parent ADT.
    fn get_adt_gens(
        &self,
        ty_env: &mut TyEnv,
        ast_ctx: &AstCtx,
        adt_path: &LangPath,
    ) -> LangResult<Option<Vec<Generics>>> {
        Ok(
            if let Some(adt_type_ids) = self
                .generic_adts
                .get(ty_env, DerefType::Deep, adt_path)
                .ok()
                .flatten()
            {
                let mut adt_gens = Vec::default();
                for adt_type_id in adt_type_ids {
                    set_generic_names(ty_env, ast_ctx, *adt_type_id)?;
                    if let Some(adt_gen) = get_gens(ty_env, *adt_type_id)? {
                        adt_gens.push(adt_gen.clone());
                    }
                }

                if !adt_gens.is_empty() {
                    Some(adt_gens)
                } else {
                    None
                }
            } else {
                None
            },
        )
    }

    // Fetch all uses of generics specified on the parent method.
    fn get_method_gens(
        &self,
        ty_env: &TyEnv,
        adt_path: &LangPath,
        method_name: &str,
    ) -> Option<Vec<Generics>> {
        self.generic_methods
            .get(ty_env, DerefType::Deep, adt_path)
            .ok()
            .flatten()
            .map(|s| s.get(method_name))
            .flatten()
            .cloned()
    }

    // Fetch all uses of generics specified on the function.
    fn get_fn_gens(&self, ty_env: &TyEnv, fn_full_path: &LangPath) -> Option<Vec<Generics>> {
        self.generic_fns
            .get(ty_env, DerefType::Deep, fn_full_path)
            .ok()
            .flatten()
            .cloned()
    }
}

/// If generics are declared for both the parent ADT and method, new `Generic`s
/// will be created that contains the combination of both ADT and method gens.
/// If only one of them is specified, that specific vector will be returned.
/// If no generics are specific on either ADT or method, None is returned.
fn combine_generics(
    method_gens: Option<Vec<Generics>>,
    adt_gens: Option<Vec<Generics>>,
) -> Option<Vec<Generics>> {
    match (method_gens, adt_gens) {
        // If generics are declared for both the parent ADT and method,
        // new `Generic`s will be created that contains the combination of
        // both ADT and method gens.
        (Some(method_gens), Some(adt_gens)) => {
            let mut gen_impls = Vec::with_capacity(method_gens.len() * adt_gens.len());
            for method_gen in &method_gens {
                for adt_gen in &adt_gens {
                    let mut combined_gen = Generics::new();
                    for (name, type_id) in method_gen.iter_names().zip(method_gen.iter_types()) {
                        combined_gen.insert(name.clone(), *type_id);
                    }
                    for (name, type_id) in adt_gen.iter_names().zip(adt_gen.iter_types()) {
                        combined_gen.insert(name.clone(), *type_id);
                    }
                    gen_impls.push(combined_gen);
                }
            }
            Some(gen_impls)
        }

        (None, Some(gens)) | (Some(gens), None) => Some(gens),

        // No use of the "parent" ADT or method found. This means that the
        // nested call will never be done and we therefore do not have to
        // convert it, it will be removed and never used.
        (None, None) => None,
    }
}
