use std::collections::{hash_map::Entry, HashMap, HashSet};

use log::debug;

use common::{
    eq::{generics_eq, path_eq},
    error::{LangError, LangErrorKind, LangResult},
    hash::DerefType,
    hash_map::TyEnvHashMap,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
        expr::FnCall,
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::{
        contains::contains_generic_shallow, generics::Generics, get::get_ident,
        replace::replace_gen_impls, to_string::to_string_path, ty_env::TyEnv, type_id::TypeId,
    },
};

use crate::util::order::order_step2_strings;

use super::nested_collector::{GenericNestedCollector, NestedMethodInfo};

/// Iterates through the tokens and gathers all uses of function and methods
/// containing generics. Only the function/method calls implementing the generics
/// will be stored (ex. `func<i64>()` & `func<f32>()`), not the functions/methods
/// only containing the generic declarations (ex. `func<T>()`).
///
/// This information will be used to create new instances of the functions and
/// methods where the generics are "implemented". Ex. `func<T>()` might be
/// implemented as `func<i64>()` or `func<f32>()`.
pub struct GenericFnCollector<'a> {
    /// The first LangPath is the path of the ADT that this method belongs to and
    /// the second key string is the name of the method.
    pub generic_methods: TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,

    /// The first LangPath is the name of the ADT in which the information
    /// was found.
    nested_generic_methods: TyEnvHashMap<LangPath, TyEnvHashMap<NestedMethodInfo, Vec<Generics>>>,

    /// Similar to `generic_methods`, but this will contain "free-standing"
    // functions that aren't tied to a ADT.
    pub generic_fns: TyEnvHashMap<LangPath, Vec<Generics>>,

    /// A list of all ADT's in the dependecy order. This list will be in reverse
    /// order compared to how it is calculated. This means that ADT `A` depending
    /// on ADT `B` would come before in the list. This is done since `A` should
    /// "expand" all its generic implementations before `B` is "calculated".
    dependency_order_rev: &'a [LangPath],

    errors: Vec<LangError>,
}

impl<'a> GenericFnCollector<'a> {
    pub fn new(dependency_order_rev: &'a [LangPath]) -> Self {
        Self {
            generic_methods: TyEnvHashMap::default(),
            nested_generic_methods: TyEnvHashMap::default(),
            generic_fns: TyEnvHashMap::default(),
            dependency_order_rev,
            errors: Vec::default(),
        }
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

    /// Collects any nested method calls containing generics.
    fn collect_nested(
        &mut self,
        ctx: &mut TraverseCtx,
        ast_tokens: &mut Vec<AstToken>,
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
                    let adt = adt.read();
                    adt.module.clone_push(&adt.name, None, Some(adt.file_pos))
                }
                _ => continue,
            };

            let mut collector = GenericNestedCollector::new();
            if let Err(mut errs) = traverse(ctx, &mut collector, ast_token) {
                self.errors.append(&mut errs);
                continue;
            }

            for (nested_info, gens_vec) in collector
                .nested_generic_methods
                .keys()
                .zip(collector.nested_generic_methods.values())
            {
                for gens in gens_vec {
                    let ty_env_guard = ctx.ty_env.lock();
                    let contains_key = self.nested_generic_methods.contains_key(
                        &ty_env_guard,
                        DerefType::None,
                        &adt_path_without_gens,
                    )?;

                    // Insert the new generic types into `self.generic_methods`. These
                    // generics will then be used when creating copies of the method.
                    if contains_key {
                        let map_inner = self
                            .nested_generic_methods
                            .get_mut(&ty_env_guard, DerefType::None, &adt_path_without_gens)?
                            .unwrap();

                        let contains_key_inner =
                            map_inner.contains_key(&ty_env_guard, DerefType::Deep, nested_info)?;

                        if contains_key_inner {
                            let vec_inner = map_inner
                                .get_mut(&ty_env_guard, DerefType::Deep, nested_info)?
                                .unwrap();

                            let mut contains_gens = false;
                            for gen_inner in vec_inner.iter() {
                                if generics_eq(&ty_env_guard, gen_inner, gens, DerefType::Deep)? {
                                    contains_gens = true;
                                }
                            }

                            if !contains_gens {
                                vec_inner.push(gens.clone());
                            }
                        } else {
                            map_inner.insert(
                                &ty_env_guard,
                                DerefType::None,
                                nested_info.clone(),
                                vec![gens.clone()],
                            )?;
                        }
                    } else {
                        let mut map_outer = TyEnvHashMap::default();
                        map_outer.insert(
                            &ty_env_guard,
                            DerefType::None,
                            nested_info.clone(),
                            vec![gens.clone()],
                        )?;
                        self.nested_generic_methods.insert(
                            &ty_env_guard,
                            DerefType::None,
                            adt_path_without_gens.clone(),
                            map_outer,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    fn convert_nested_to_regular(&mut self, ctx: &mut TraverseCtx) -> LangResult<()> {
        debug!(
            "self.nested_generic_methods: {:#?}",
            self.nested_generic_methods
        );

        // Converts all `self.nested_generic_methods` into regular `self.generic_methods`.
        for adt_path in self.dependency_order_rev.iter() {
            let nested_gen_methods = if self.nested_generic_methods.contains_key(
                &ctx.ty_env.lock(),
                DerefType::Deep,
                adt_path,
            )? {
                self.nested_generic_methods
                    .remove(&ctx.ty_env.lock(), DerefType::Deep, adt_path)?
                    .unwrap()
            } else {
                continue;
            };

            // Figures out and returns a list of the methods in the order that
            // they should be iterated.
            let method_order =
                match Self::order(&ctx.ty_env.lock(), adt_path, nested_gen_methods.keys()) {
                    Ok(order) => order,
                    Err(err) => {
                        self.errors.push(err);
                        continue;
                    }
                };

            // TODO: Fix better performance. This logic is shit in every way possible.
            // First iterate through the methods that have references to other methods
            // in the same structure. The referencing methods needs to be converted
            // before the method that it is referencing
            for order_method_name in method_order.iter() {
                for (method_info, nested_method_tys) in
                    nested_gen_methods.keys().zip(nested_gen_methods.values())
                {
                    if &method_info.method_call_name == order_method_name {
                        for generics in nested_method_tys.iter() {
                            self.convert_nested_method_from_method(
                                ctx,
                                adt_path,
                                &method_info,
                                generics,
                            )?;
                        }
                    }
                }
            }

            // Iterates through the methods one more time but this times converts
            // all methods, not only the ones that needs to be converted in a
            // specific order. This logic can't create duplicates since the functions
            // that is called check duplicates before inserting the methods into
            // `self.generic_methods`.
            for (method_info, nested_method_tys) in
                nested_gen_methods.keys().zip(nested_gen_methods.values())
            {
                for generics in nested_method_tys.iter() {
                    self.convert_nested_method_from_method(ctx, &adt_path, &method_info, generics)?;
                }
            }
        }

        Ok(())
    }

    fn convert_nested_method_from_method(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        method_info: &NestedMethodInfo,
        generics: &Generics,
    ) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();

        let func_name = &method_info.func_name;
        let method_call_name = &method_info.method_call_name;
        let method_adt_path = &method_info.method_adt_path;

        let gen_func_tys = if let Some(gen_func_tys) = self
            .generic_methods
            .get(&ty_env_guard, DerefType::Deep, adt_path)
            .ok()
            .flatten()
            .map(|s| s.get(func_name))
            .flatten()
        {
            gen_func_tys.clone()
        } else {
            return Ok(());
        };

        let method_gen_names = {
            let method =
                ctx.ast_ctx
                    .get_method(&ty_env_guard, method_adt_path, method_call_name)?;
            let method = method.read();

            if let Some(method_gens) = &method.generics {
                method_gens.iter_names().cloned().collect::<Vec<_>>()
            } else {
                unreachable!(
                    "method_adt_path: {}, method_call_name: {}",
                    to_string_path(&ty_env_guard, &method_adt_path),
                    method_call_name,
                );
            }
        };

        for generics_impls in gen_func_tys {
            // Iterate through the types and replace any generics found in the
            // function declaration. Also add names of the generics into `Generics`
            // if they are missing (might have no effect if the names are set already).
            let mut new_gens = Generics::new();
            for (name, type_id) in method_gen_names.iter().zip(generics.iter_types()) {
                let type_id = match replace_gen_impls(
                    &mut ty_env_guard,
                    &ctx.ast_ctx,
                    *type_id,
                    &generics_impls,
                )? {
                    Some(new_type_id) => new_type_id,
                    None => *type_id,
                };
                new_gens.insert(name.clone(), type_id);
            }

            if generics_eq(&ty_env_guard, &new_gens, generics, DerefType::Deep)? {
                continue;
            }

            // The method call is done on the structure `method_structure_name`, so
            // use that instead of the `struct_name` which represents the structure
            // in which this call was found.
            let contains_key = self.generic_methods.contains_key(
                &ty_env_guard,
                DerefType::Deep,
                method_adt_path,
            )?;

            if contains_key {
                let map_inner = self
                    .generic_methods
                    .get_mut(&ty_env_guard, DerefType::Deep, method_adt_path)?
                    .unwrap();

                match map_inner.entry(method_call_name.clone()) {
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
                map_inner.insert(method_call_name.clone(), vec![new_gens.clone()]);
                self.generic_methods.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    method_adt_path.clone(),
                    map_inner,
                )?;
            }
        }

        Ok(())
    }

    /// Figures out the order in which the nested methods needs to be "handled"/
    /// "expanded". The returned vector will contain the names/paths of the methods in
    /// the order in which they should be converted to "regular" generic methods.
    fn order<'o, I>(
        ty_env: &TyEnv,
        adt_path: &LangPath,
        nested_method_infos: I,
    ) -> LangResult<Vec<String>>
    where
        I: IntoIterator<Item = &'o NestedMethodInfo>,
    {
        // The String key is the name of the method and the value set contains the
        // methods that are nested/referenced from the given key method.
        let mut references: HashMap<String, HashSet<String>> = HashMap::default();

        // Populate the `references` map with information in the loop below.
        for nested_method_info in nested_method_infos {
            // We only care about methods that are referenced from the same
            // ADT.
            if !path_eq(
                ty_env,
                adt_path,
                &nested_method_info.method_adt_path,
                DerefType::Deep,
            )? {
                continue;
            }

            let func_name = &nested_method_info.func_name;
            let referenced_method_name = &nested_method_info.method_call_name;

            match references.entry(func_name.clone()) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(referenced_method_name) {
                        o.get_mut().insert(referenced_method_name.clone());
                    }
                }
                Entry::Vacant(v) => {
                    let mut s = HashSet::default();
                    s.insert(referenced_method_name.clone());
                    v.insert(s);
                }
            }
        }

        // Figure out the order of the methods by looking at how they reference
        // each other.
        match order_step2_strings(&references) {
            Ok(order) => {
                debug!(
                    "method -- references: {:#?}, order: {:#?}",
                    references, order
                );
                Ok(order)
            }
            Err(cyc_err) => Err(LangError::new(
                format!(
                    "Cyclic dependency between method \"{}\" and \"{}\" in ADT \"{}\".",
                    cyc_err.0,
                    cyc_err.1,
                    to_string_path(ty_env, &adt_path)
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }
}

impl<'a> Visitor for GenericFnCollector<'a> {
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

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Some(adt_type_id) = &fn_call.method_adt {
            if let Err(err) = self.collect_generic_method(ctx, *adt_type_id, fn_call) {
                self.errors.push(err);
            }
        } else if let Err(err) = self.collect_generic_fn(ctx, fn_call) {
            self.errors.push(err);
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        if let Err(err) = self.convert_nested_to_regular(ctx) {
            self.errors.push(err);
        }
        debug!(
            "self.generic_methods: {:#?}\nself.generic_fns: {:#?}",
            self.generic_methods, self.generic_fns
        );
    }
}
