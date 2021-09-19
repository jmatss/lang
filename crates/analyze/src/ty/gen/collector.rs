use std::collections::{hash_map::Entry, HashMap};

use common::{
    eq::generics_eq,
    error::{LangError, LangResult},
    hash::DerefType,
    hash_map::TyEnvHashMap,
    path::LangPath,
    token::expr::FnCall,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        contains::contains_generic_shallow, generics::Generics, get::get_ident, is::is_tuple,
        to_string::to_string_path, ty::Ty, type_id::TypeId,
    },
};

use crate::ty::solve::set_generic_names;

/// Iterates through the tokens and gathers all ADTs, functions and methods
/// containing generics. Only the ADTs/funcs implementing the generic will be
/// stored, any ADT/func containing a generic declaration ex. `Struct<T>` will
/// NOT be saved, only ex. "Struct<i64>" & "func<f32>()".
///
/// This information will be used to create new instances of the ADTs and the
/// methods where the generics are "implemented". Ex. "Struct<T>" might be
/// implemented as "Struct<i64>" & "Struct<f32>".
pub struct GenericCollector {
    /// Will contain all types containing generics. This will then be used to
    /// create new generic ADTs with the specific generic implementations.
    ///
    /// The LangPath key is the path of the ADT/type. The TypeId values are the
    /// actual types containing generic implentations/instances. The LangPaths
    /// does NOT contain any generics.
    pub generic_adts: TyEnvHashMap<LangPath, Vec<TypeId>>,

    /// The first LangPath is the path of the ADT that this method belongs to and
    /// the second key string is the name of the method.
    pub generic_methods: TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,

    /// Similar to `generic_methods`, but this will contain "free-standing"
    /// functions that aren't tied to an ADT. The LangPath is the name of the
    /// function including module.
    pub generic_fns: TyEnvHashMap<LangPath, Vec<Generics>>,

    errors: Vec<LangError>,
}

impl GenericCollector {
    pub fn new() -> Self {
        Self {
            generic_adts: TyEnvHashMap::default(),
            generic_methods: TyEnvHashMap::default(),
            generic_fns: TyEnvHashMap::default(),
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
        if contains_generic_shallow(&ctx.ty_env.lock(), type_id)? {
            return Ok(());
        }

        let ty_clone = ctx.ty_env.lock().ty_clone(type_id)?;

        let adt_path_without_gens = match ty_clone {
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

        let mut ty_env_guard = ctx.ty_env.lock();

        // Set names of generics if they aren't set already.
        set_generic_names(&mut ty_env_guard, ctx.ast_ctx, type_id)?;

        let deref_type = DerefType::None;
        let contains_key =
            self.generic_adts
                .contains_key(&ty_env_guard, deref_type, &adt_path_without_gens)?;

        if contains_key {
            let type_ids = self
                .generic_adts
                .get_mut(&ty_env_guard, deref_type, &adt_path_without_gens)?
                .unwrap();

            if !type_ids.contains(&type_id) {
                type_ids.push(type_id);
            }
        } else {
            let type_ids = vec![type_id];
            self.generic_adts
                .insert(&ty_env_guard, deref_type, adt_path_without_gens, type_ids)?;
        }

        Ok(())
    }

    /// If the method called in the `method_call` on the ADT with type `adt_type_id`
    /// contains generics, store the generics in `self.generic_methods`. These
    /// will then be used to create copies of the method where the generics have
    /// been implemented.
    fn collect_generic_method(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_type_id: TypeId,
        method_call: &FnCall,
    ) -> LangResult<()> {
        let method_call_gens = if let Some(fn_call_gens) = &method_call.generics {
            fn_call_gens.clone()
        } else {
            return Ok(());
        };

        let method_name = method_call.name.clone();

        let adt_path = if let Some(adt_path) = get_ident(&ctx.ty_env.lock(), adt_type_id)? {
            adt_path
        } else {
            return Ok(());
        };

        let method =
            ctx.ast_ctx
                .get_method(&ctx.ty_env.lock(), &adt_path.without_gens(), &method_name)?;
        let method = method.read();

        if let Some(method_gens) = &method.generics {
            let method_gen_names = method_gens.iter_names().cloned().collect::<Vec<_>>();

            // Skip any methods that don't have their generic declarations
            // "implemented", only store method calls that have had the generics
            // "implemented" in `self.generic_methods`.
            for gen_type_id in method_call_gens.iter_types() {
                if contains_generic_shallow(&ctx.ty_env.lock(), *gen_type_id)? {
                    return Ok(());
                }
            }

            // Ensure that the method call has the same amount of generic impls
            // as the method declaration has declared.
            if method_call_gens.len_types() != method_gen_names.len() {
                return Err(ctx.ast_ctx.err(format!(
                    "Method call \"{}\" on ADT \"{}\" has {} generics specified, \
                    but the method declaration has {} generics.",
                    &method_name,
                    to_string_path(&ctx.ty_env.lock(), &adt_path),
                    method_call_gens.len_types(),
                    method_gen_names.len()
                )));
            }

            let mut new_gens = Generics::new();
            for (name, ty) in method_gen_names.iter().zip(method_call_gens.iter_types()) {
                new_gens.insert(name.clone(), *ty);
            }

            let ty_env_guard = ctx.ty_env.lock();
            let contains_key = self.generic_methods.contains_key(
                &ty_env_guard,
                DerefType::None,
                &adt_path.without_gens(),
            )?;

            // Insert the new generic types into `self.generic_methods`. These
            // generics will then be used when creating copies of the method.
            if contains_key {
                let map_inner = self
                    .generic_methods
                    .get_mut(&ty_env_guard, DerefType::None, &adt_path.without_gens())?
                    .unwrap();

                match map_inner.entry(method_name) {
                    Entry::Occupied(mut vec_inner) => {
                        let mut contains_gens = false;
                        for inner_gens in vec_inner.get() {
                            if generics_eq(&ty_env_guard, inner_gens, &new_gens, DerefType::Deep)? {
                                contains_gens = true;
                                break;
                            }
                        }

                        if !contains_gens {
                            vec_inner.get_mut().push(new_gens);
                        }
                    }
                    Entry::Vacant(vec_inner) => {
                        vec_inner.insert(vec![new_gens]);
                    }
                }
            } else {
                let mut map_inner = HashMap::default();
                map_inner.insert(method_name, vec![new_gens]);
                self.generic_methods.insert(
                    &ty_env_guard,
                    DerefType::None,
                    adt_path.without_gens(),
                    map_inner,
                )?;
            }

            Ok(())
        } else {
            Err(ctx.ast_ctx.err(format!(
                "Method call \"{}\" on ADT \"{}\" has {} generics specified, \
                but the method declaration has no generics declared.",
                &method_name,
                to_string_path(&ctx.ty_env.lock(), &adt_path),
                method_call_gens.len_types(),
            )))
        }
    }

    /// If the function called in the `fn_call` contains generics, store the
    /// generics in `self.generic_fns`. These will then be used to create copies
    /// of the function where the generics have been implemented.
    fn collect_generic_fn(&mut self, ctx: &mut TraverseCtx, fn_call: &FnCall) -> LangResult<()> {
        let fn_call_gens = if let Some(fn_call_gens) = &fn_call.generics {
            fn_call_gens.clone()
        } else {
            return Ok(());
        };

        let fn_path_without_gens = fn_call
            .module
            .clone_push(&fn_call.name, None, fn_call.file_pos);

        let func = ctx
            .ast_ctx
            .get_fn(&ctx.ty_env.lock(), &fn_path_without_gens)?;
        let func = func.read();

        if let Some(fn_gens) = &func.generics {
            let fn_gen_names = fn_gens.iter_names().cloned().collect::<Vec<_>>();

            // Skip any functions that doesn't have their generic declarations
            // implemented, only save function calls that have had the generics
            // implemented in with "real" types.
            for gen_type_id in fn_call_gens.iter_types() {
                if contains_generic_shallow(&ctx.ty_env.lock(), *gen_type_id)? {
                    return Ok(());
                }
            }

            // Ensure that the function call has the same amount of generic impls
            // as the function declaration has declared.
            if fn_call_gens.len_types() != fn_gen_names.len() {
                return Err(ctx.ast_ctx.err(format!(
                    "Function call to \"{}\" has {} generics specified, \
                    but the function declaration has {} generics.",
                    &to_string_path(&ctx.ty_env.lock(), &fn_path_without_gens),
                    fn_call_gens.len_types(),
                    fn_gen_names.len()
                )));
            }

            let mut new_gens = Generics::new();
            for (name, ty) in fn_gen_names.iter().zip(fn_call_gens.iter_types()) {
                new_gens.insert(name.clone(), *ty);
            }

            let ty_env_guard = ctx.ty_env.lock();

            if let Some(prev_gens) =
                self.generic_fns
                    .get_mut(&ty_env_guard, DerefType::None, &fn_path_without_gens)?
            {
                let mut contains_gens = false;
                for prev_gen in prev_gens.iter() {
                    if generics_eq(&ty_env_guard, prev_gen, &new_gens, DerefType::Deep)? {
                        contains_gens = true;
                        break;
                    }
                }

                if !contains_gens {
                    prev_gens.push(new_gens);
                }
            } else {
                self.generic_fns.insert(
                    &ty_env_guard,
                    DerefType::None,
                    fn_path_without_gens,
                    vec![new_gens],
                )?;
            }

            Ok(())
        } else {
            Err(ctx.ast_ctx.err(format!(
                "Function call to \"{}\" has {} generics specified, \
                but the function declaration has no generics declared.",
                &to_string_path(&ctx.ty_env.lock(), &fn_path_without_gens),
                fn_call_gens.len_types(),
            )))
        }
    }
}

impl Visitor for GenericCollector {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        let is_tuple = match is_tuple(&ctx.ty_env.lock(), *type_id) {
            Ok(is_tuple) => is_tuple,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        if !is_tuple {
            if let Err(err) = self.collect_generic_adt(ctx, *type_id) {
                self.errors.push(err);
            }
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Some(adt_type_id) = &fn_call.method_adt {
            if let Err(err) = self.collect_generic_method(ctx, *adt_type_id, fn_call) {
                self.errors.push(err);
            }
        } else if let Err(err) = self.collect_generic_fn(ctx, fn_call) {
            self.errors.push(err);
        }
    }
}
