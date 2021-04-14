use std::collections::{hash_map::Entry, HashMap, HashSet};

use log::debug;

use common::{
    ctx::{traverse_ctx::TraverseCtx, ty_ctx::TyCtx, ty_env::TyEnv},
    error::{LangError, LangErrorKind, LangResult},
    path::{LangPath, LangPathPart},
    token::{ast::AstToken, block::BlockHeader, expr::FnCall},
    traverse::{traverser::AstTraverser, visitor::Visitor},
    ty::{generics::Generics, ty::Ty},
    TypeId,
};

use crate::util::order::{dependency_order_from_ctx, order_step2_strings};

/// Used to store information about a nested method.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct NestedMethodInfo {
    pub method_name: String,
    pub method_call_name: String,
    pub method_adt_path: LangPath,
}

/// Iterates through the tokens and gathers all ADT/methods containing generics.
/// Only the ADTs/methods implementing the generic will be stored, any ADT/generic
/// containing the generic ex. `Struct<T>` will NOT be saved, only ex.
/// "Struct<i64>" & "Struct<f32>".
///
/// This information will be used to create new instances of the ADTs where
/// the generics are "implemented". Ex. "Struct<T>" might be implemented as
/// "Struct<i64>" & "Struct<f32>".
pub struct GenericCollector {
    /// Will contain all types containing generics. This will then be used to
    /// create new generic ADTs with the specific generic implementations.
    /// The LangPath key is the path of the ADT/type. The TypeId values are the
    /// actual types containing generic implentations/instances.
    pub generic_adts: HashMap<LangPath, Vec<TypeId>>,

    /// Will contain all NESTED types containing generics. These are types containing
    /// generics that aren't part of the "using" structure.
    ///
    /// # Example
    ///
    /// If there are two structs `A` and `B`, both containing generic parameters.
    /// If `A` contains a function that creates a `B`, where the generic type
    /// for `B` is the generic type for `A`, that use  will be considered a nested
    /// generic struct.
    ///
    /// ```no_run
    /// struct B<T2>;
    /// struct A<T1>;
    /// implement A
    /// {
    ///     function test_func()
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
    /// and the second String key is the name of the method in which these `Ty`s
    /// was found.
    nested_generic_adts: HashMap<LangPath, HashMap<String, Vec<TypeId>>>,

    /// The first LangPath is the path of the ADT and the second string is
    /// the name of the method. The LangPath will NOT contain any references
    /// to generics.
    pub generic_methods: HashMap<LangPath, HashMap<String, Vec<Generics>>>,

    /// See explanation for `nested_generic_adts` & `generic_methods`.
    /// The first LangPath is the name of the ADT in which the information
    /// was found.
    nested_generic_methods: HashMap<LangPath, HashMap<NestedMethodInfo, Vec<Generics>>>,

    /// A list of all structure names in the dependecy order. This list will be
    /// in reverse order compared to how it is calculated. This means that a
    /// structure `A` depending on structure `B` would come before in the list.
    /// This is done since `A` should "expand" all its generic implementations
    /// before `B` is "calculated".
    dependency_order_rev: Vec<LangPath>,

    errors: Vec<LangError>,
}

impl GenericCollector {
    pub fn new() -> Self {
        Self {
            generic_adts: HashMap::default(),
            nested_generic_adts: HashMap::default(),
            generic_methods: HashMap::default(),
            nested_generic_methods: HashMap::default(),
            // Will be overwritten and set when the "default block" is visted.
            dependency_order_rev: Vec::with_capacity(0),
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
        if ctx.ty_ctx.ty_env.contains_generic_shallow(type_id)? {
            return Ok(());
        }

        let ty = ctx.ty_ctx.ty_env.ty(type_id)?.clone();

        let path = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => {
                if generics.is_empty() {
                    return Ok(());
                }

                let mut path = inner_ty.get_ident().unwrap();
                let last_part = path.pop().unwrap();
                path.push(LangPathPart(last_part.0, None));
                path
            }

            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                self.collect_generic_adt(ctx, type_id_i)?;
                return Ok(());
            }

            _ => return Ok(()),
        };

        // Set names of generics if they aren't set already.
        ctx.ty_ctx.set_generic_names(&ctx.ast_ctx, type_id)?;

        match self.generic_adts.entry(path) {
            Entry::Occupied(mut o) => {
                if !o.get().contains(&type_id) {
                    o.get_mut().push(type_id);
                }
            }
            Entry::Vacant(v) => {
                v.insert(vec![type_id]);
            }
        }

        Ok(())
    }

    /// If the method with name `method_name`, on the ADT with type `adt_type_id`
    /// contains generics; this function will save all "implementing"
    /// methods where the generics have been given a "real type instance".
    /// This function also adds the names for the generics if they aren't already
    /// set.
    fn collect_generic_method(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_type_id: TypeId,
        method_call: &FnCall,
    ) -> LangResult<()> {
        let method_call_generics = if let Some(fn_call_generics) = &method_call.generics {
            fn_call_generics.clone()
        } else {
            return Ok(());
        };

        // Do not "store" any types containing generic declarations inside
        // `generic_methods`, only store method calls that have had the generics
        // "implemented".
        for gen_type_id in method_call_generics.iter_types() {
            if ctx.ty_ctx.ty_env.contains_generic_shallow(*gen_type_id)? {
                return Ok(());
            }
        }

        let method_name = method_call.name.clone();

        let old_path = if let Some(mut old_path) = ctx.ty_ctx.ty_env.get_ident(adt_type_id)? {
            // Make sure no generics are in the path.
            let last_part = old_path.pop().unwrap();
            old_path.push(LangPathPart(last_part.0, None));
            old_path
        } else {
            return Ok(());
        };

        let method = ctx
            .ast_ctx
            .get_method(&ctx.ty_ctx, &old_path, &method_name)?;
        let method = method.borrow();

        if let Some(func_generics) = &method.generics {
            let func_generic_names = func_generics.iter_names().cloned().collect::<Vec<_>>();

            // Ensure that the func call has the same amount of generic impls
            // as the func declaration has declared.
            if method_call_generics.len_types() != func_generic_names.len() {
                let err = ctx.ast_ctx.err(format!(
                    "Func call for method \"{}\" has {} generics, but func decl expected {}.",
                    &method_name,
                    method_call_generics.len_types(),
                    func_generic_names.len()
                ));
                return Err(err);
            }

            let mut new_generics = Generics::new();
            for (name, ty) in func_generic_names
                .iter()
                .zip(method_call_generics.iter_types())
            {
                new_generics.insert(name.clone(), *ty);
            }

            // Insert the new generic types into `self.generic_funcs`. These
            // generics will then be used when creating copies of the method.
            match self.generic_methods.entry(old_path) {
                Entry::Occupied(mut o_outer) => match o_outer.get_mut().entry(method_name) {
                    Entry::Occupied(mut o_inner) => {
                        if !o_inner.get().contains(&new_generics) {
                            o_inner.get_mut().push(new_generics);
                        }
                    }
                    Entry::Vacant(v_inner) => {
                        v_inner.insert(vec![new_generics]);
                    }
                },
                Entry::Vacant(v_outer) => {
                    let mut m = HashMap::default();
                    m.insert(method_name, vec![new_generics]);
                    v_outer.insert(m);
                }
            }

            Ok(())
        } else {
            let err = ctx.ast_ctx.err(format!(
                "Func call for method \"{}\" on struct \"{}\"has generics, but func decl doesn't.",
                &method_name,
                ctx.ty_ctx.to_string_path(&old_path)
            ));
            Err(err)
        }
    }

    fn collect_nested(&mut self, ctx: &mut TraverseCtx, ast_tokens: &mut Vec<AstToken>) {
        for ast_token in ast_tokens {
            // Need to do this ugly hack to make the borrow checker happy.
            // We wan't to borrow the `adt_path` and use it in the logic below,
            // but at the same time borrow the `ast_token` which contains the
            // `adt_path`.
            let impl_opt = match ast_token {
                AstToken::Block(BlockHeader::Implement(adt_path, _), ..) => {
                    if ctx.ast_ctx.get_adt(&ctx.ty_ctx, adt_path).is_ok() {
                        Some(adt_path.clone())
                    } else {
                        None
                    }
                }

                _ => continue,
            };

            if let Some(adt_path) = impl_opt {
                let mut nested_collector = NestedGenericCollector::new();
                let mut traverser = AstTraverser::from_ctx(ctx);
                if let Err(errs) = traverser
                    .add_visitor(&mut nested_collector)
                    .traverse_token(ast_token)
                    .take_errors()
                {
                    self.errors.extend(errs.into_iter());
                }

                for (method_name, type_ids) in nested_collector.nested_generic_adts.iter() {
                    for type_id in type_ids {
                        match self.nested_generic_adts.entry(adt_path.clone()) {
                            Entry::Occupied(mut o_outer) => {
                                match o_outer.get_mut().entry(method_name.clone()) {
                                    Entry::Occupied(mut o_inner) => {
                                        if !o_inner.get().contains(type_id) {
                                            o_inner.get_mut().push(*type_id);
                                        }
                                    }
                                    Entry::Vacant(v_inner) => {
                                        v_inner.insert(vec![*type_id]);
                                    }
                                }
                            }
                            Entry::Vacant(v_outer) => {
                                let mut m = HashMap::default();
                                m.insert(method_name.clone(), vec![*type_id]);
                                v_outer.insert(m);
                            }
                        }
                    }
                }

                for (nested_info, generics_vec) in nested_collector.nested_generic_methods.iter() {
                    for generics in generics_vec {
                        match self.nested_generic_methods.entry(adt_path.clone()) {
                            Entry::Occupied(mut o_outer) => {
                                match o_outer.get_mut().entry(nested_info.clone()) {
                                    Entry::Occupied(mut o_inner) => {
                                        if !o_inner.get().contains(&generics) {
                                            o_inner.get_mut().push(generics.clone());
                                        }
                                    }
                                    Entry::Vacant(v_inner) => {
                                        v_inner.insert(vec![generics.clone()]);
                                    }
                                }
                            }
                            Entry::Vacant(v_outer) => {
                                let mut m = HashMap::default();
                                m.insert(nested_info.clone(), vec![generics.clone()]);
                                v_outer.insert(m);
                            }
                        }
                    }
                }
            }
        }
    }

    fn convert_nested_to_regular(&mut self, ctx: &mut TraverseCtx) {
        debug!(
            "self.nested_generic_adts: {:#?}, self.nested_generic_methods: {:#?}",
            self.nested_generic_adts, self.nested_generic_methods
        );

        // Converts all `self.nested_generic_adts` into regular `self.generic_adts`.
        for adt_path in self.dependency_order_rev.clone().iter() {
            if !self.nested_generic_adts.contains_key(adt_path) {
                continue;
            }

            let nested_gen_adt = self.nested_generic_adts.remove(adt_path).unwrap();
            for (method_name, nested_adt_type_ids) in nested_gen_adt.iter() {
                for nested_adt_type_id in nested_adt_type_ids.iter() {
                    let nested_adt_path = match ctx.ty_ctx.ty_env.get_ident(*nested_adt_type_id) {
                        Ok(Some(path)) => path,
                        Ok(None) => {
                            let err = ctx.ast_ctx.err(format!(
                                "Nested ADT type doesn't contain ident: {:#?}",
                                nested_adt_type_id
                            ));
                            self.errors.push(err);
                            return;
                        }
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if let Err(err) = self.convert_nested_adt_from_adt(
                        ctx,
                        &adt_path,
                        *nested_adt_type_id,
                        &nested_adt_path,
                    ) {
                        self.errors.push(err);
                    }
                    if let Err(err) = self.convert_nested_adt_from_method(
                        &mut ctx.ty_ctx.ty_env,
                        &adt_path,
                        method_name,
                        *nested_adt_type_id,
                        &nested_adt_path,
                    ) {
                        self.errors.push(err);
                    }
                }
            }
        }

        // Converts all `self.nested_generic_methods` into regular `self.generic_methods`.
        for adt_path in self.dependency_order_rev.clone().iter() {
            if !self.nested_generic_methods.contains_key(adt_path) {
                continue;
            }

            let nested_gen_methods = self.nested_generic_methods.remove(adt_path).unwrap();

            // Figures out and returns a list of the methods in the order that
            // they should be iterated.
            let method_order =
                match GenericCollector::order(&ctx.ty_ctx, adt_path, nested_gen_methods.keys()) {
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
                for (method_info, nested_method_tys) in &nested_gen_methods {
                    if &method_info.method_call_name == order_method_name {
                        for generics in nested_method_tys.iter() {
                            self.convert_nested_method_from_adt(
                                ctx,
                                adt_path,
                                &method_info,
                                generics,
                            );
                            self.convert_nested_method_from_method(
                                ctx,
                                adt_path,
                                &method_info,
                                generics,
                            );
                        }
                    }
                }
            }

            // Iterates through the methods one more time but this times converts
            // all methods, not only the ones that needs to be converted in a
            // specific order. This logic can't create duplicates since the functions
            // that is called check duplicates before inserting the methods into
            // `self.generic_methods`.
            for (method_info, nested_method_tys) in &nested_gen_methods {
                for generics in nested_method_tys.iter() {
                    self.convert_nested_method_from_adt(ctx, &adt_path, &method_info, generics);
                    self.convert_nested_method_from_method(ctx, &adt_path, &method_info, generics);
                }
            }
        }
    }

    fn convert_nested_adt_from_adt(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        nested_adt_type_id: TypeId,
        nested_adt_path: &LangPath,
    ) -> LangResult<()> {
        let gen_adt_type_ids = if let Some(gen_adt_type_ids) = self.generic_adts.get(adt_path) {
            gen_adt_type_ids.clone()
        } else {
            // This can happen if there are no implementation of a
            // ADT that contains generics.
            return Ok(());
        };

        // Go through all implementations of the ADT `adt_path` and use those
        // generic instances to create new ADT instances for the nested one.
        // This will be done for all generics of the outer ADT.
        for gen_adt_type_id in gen_adt_type_ids {
            let gen_adt_ty = ctx.ty_ctx.ty_env.ty(gen_adt_type_id)?.clone();

            let generics = if let Ty::CompoundType(_, generics, ..) = gen_adt_ty {
                generics
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Generic instance type not compound: {:#?}",
                    gen_adt_type_id
                ));
                return Err(err);
            };

            let nested_adt_type_id = if let Some(new_type_id) = ctx
                .ty_ctx
                .ty_env
                .replace_gen_impls(nested_adt_type_id, &generics)?
            {
                new_type_id
            } else {
                nested_adt_type_id
            };

            match self.generic_adts.entry(nested_adt_path.clone()) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(&nested_adt_type_id) {
                        o.get_mut().push(nested_adt_type_id);
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![nested_adt_type_id]);
                }
            };
        }

        Ok(())
    }

    fn convert_nested_adt_from_method(
        &mut self,
        ty_env: &mut TyEnv,
        adt_path: &LangPath,
        method_name: &str,
        nested_adt_type_id: TypeId,
        nested_adt_path: &LangPath,
    ) -> LangResult<()> {
        let gen_func_tys = if let Some(gen_func_tys) = self
            .generic_methods
            .get(adt_path)
            .map(|s| s.get(method_name))
            .flatten()
        {
            gen_func_tys
        } else {
            return Ok(());
        };

        // Go through all implementations of the method `method_name` and
        // use those generic instances to create new instances for the nested
        // ADT generics.
        for generics in gen_func_tys {
            let nested_adt_type_id = if let Some(new_type_id) =
                ty_env.replace_gen_impls(nested_adt_type_id, &generics)?
            {
                new_type_id
            } else {
                nested_adt_type_id
            };

            match self.generic_adts.entry(nested_adt_path.without_gens()) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(&nested_adt_type_id) {
                        o.get_mut().push(nested_adt_type_id);
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![nested_adt_type_id]);
                }
            };
        }

        Ok(())
    }

    fn convert_nested_method_from_adt(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        method_info: &NestedMethodInfo,
        generics: &Generics,
    ) {
        let method_call_name = &method_info.method_call_name;
        let method_adt_path = &method_info.method_adt_path;

        let gen_adt_tys = if let Some(gen_adt_tys) = self.generic_adts.get(adt_path) {
            gen_adt_tys.clone()
        } else {
            return;
        };

        let method_gen_names =
            match ctx
                .ast_ctx
                .get_method(&ctx.ty_ctx, method_adt_path, method_call_name)
            {
                Ok(method) => {
                    if let Some(method_gens) = &method.borrow().generics {
                        method_gens.iter_names().cloned().collect::<Vec<_>>()
                    } else {
                        unreachable!(
                            "method_adt_path: {}, method_call_name: {}",
                            ctx.ty_ctx.to_string_path(&method_adt_path),
                            method_call_name,
                        );
                    }
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

        for gen_adt_type_id in gen_adt_tys {
            let gen_adt_ty = match ctx.ty_ctx.ty_env.ty(gen_adt_type_id) {
                Ok(gen_adt_ty) => gen_adt_ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let adt_gens = if let Ty::CompoundType(_, adt_gens, ..) = gen_adt_ty {
                adt_gens
            } else {
                let err = ctx.ast_ctx.err(format!(
                    "Generic instance type not compound: {:#?}",
                    gen_adt_ty
                ));
                self.errors.push(err);
                return;
            };

            // Iterate through the types and replace any generics found in the
            // function declaration. Also add names of the generics into `Generics`
            // if they are missing (might have no effect if the names are set already).
            let mut new_generics = Generics::new();
            for (name, type_id) in method_gen_names.iter().zip(generics.iter_types()) {
                let type_id = match ctx.ty_ctx.ty_env.replace_gen_impls(*type_id, &generics) {
                    Ok(Some(new_type_id)) => new_type_id,
                    Ok(None) => *type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let type_id = match ctx.ty_ctx.ty_env.replace_gen_impls(type_id, &adt_gens) {
                    Ok(Some(new_type_id)) => new_type_id,
                    Ok(None) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                new_generics.insert(name.clone(), type_id);
            }

            if &new_generics == generics {
                continue;
            }

            // The method call is done on the structure `method_structure_name`, so
            // use that instead of the `struct_name` which represents the structure
            // in which this call was found.
            match self.generic_methods.entry(method_adt_path.clone()) {
                Entry::Occupied(mut o_outer) => {
                    match o_outer.get_mut().entry(method_call_name.clone()) {
                        Entry::Occupied(mut o_inner) => {
                            if !o_inner.get().contains(&new_generics) {
                                o_inner.get_mut().push(new_generics.clone());
                            }
                        }
                        Entry::Vacant(v_inner) => {
                            v_inner.insert(vec![new_generics.clone()]);
                        }
                    }
                }
                Entry::Vacant(v_outer) => {
                    let mut m = HashMap::default();
                    m.insert(method_call_name.clone(), vec![new_generics.clone()]);
                    v_outer.insert(m);
                }
            };
        }
    }

    fn convert_nested_method_from_method(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        method_info: &NestedMethodInfo,
        generics: &Generics,
    ) {
        let method_name = &method_info.method_name;
        let method_call_name = &method_info.method_call_name;
        let method_adt_path = &method_info.method_adt_path;

        let gen_func_tys = if let Some(gen_func_tys) = self
            .generic_methods
            .get(adt_path)
            .map(|s| s.get(method_name))
            .flatten()
        {
            gen_func_tys.clone()
        } else {
            return;
        };

        let method_gen_names =
            match ctx
                .ast_ctx
                .get_method(&ctx.ty_ctx, method_adt_path, method_call_name)
            {
                Ok(method) => {
                    if let Some(method_gens) = &method.borrow().generics {
                        method_gens.iter_names().cloned().collect::<Vec<_>>()
                    } else {
                        unreachable!(
                            "method_adt_path: {}, method_call_name: {}",
                            ctx.ty_ctx.to_string_path(&method_adt_path),
                            method_call_name,
                        );
                    }
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

        for generics_impls in gen_func_tys {
            // Iterate through the types and replace any generics found in the
            // function declaration. Also add names of the generics into `Generics`
            // if they are missing (might have no effect if the names are set already).
            let mut new_generics = Generics::new();
            for (name, type_id) in method_gen_names.iter().zip(generics.iter_types()) {
                let type_id = match ctx
                    .ty_ctx
                    .ty_env
                    .replace_gen_impls(*type_id, &generics_impls)
                {
                    Ok(Some(new_type_id)) => new_type_id,
                    Ok(None) => *type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                new_generics.insert(name.clone(), type_id);
            }

            if &new_generics == generics {
                continue;
            }

            // The method call is done on the structure `method_structure_name`, so
            // use that instead of the `struct_name` which represents the structure
            // in which this call was found.
            match self.generic_methods.entry(method_adt_path.clone()) {
                Entry::Occupied(mut o_outer) => {
                    match o_outer.get_mut().entry(method_call_name.clone()) {
                        Entry::Occupied(mut o_inner) => {
                            if !o_inner.get().contains(&new_generics) {
                                o_inner.get_mut().push(new_generics.clone());
                            }
                        }
                        Entry::Vacant(v_inner) => {
                            v_inner.insert(vec![new_generics.clone()]);
                        }
                    }
                }
                Entry::Vacant(v_outer) => {
                    let mut m = HashMap::default();
                    m.insert(method_call_name.clone(), vec![new_generics.clone()]);
                    v_outer.insert(m);
                }
            };
        }
    }

    /// Figures out the order in which the nested methods needs to be "handled"/
    /// "expanded". The returned vector will contain the names/paths of the methods in
    /// the order in which they should be converted to "regular" generic methods.
    fn order<'a, I>(
        ty_ctx: &TyCtx,
        adt_path: &LangPath,
        nested_method_infos: I,
    ) -> LangResult<Vec<String>>
    where
        I: IntoIterator<Item = &'a NestedMethodInfo>,
    {
        // The String key is the name of the method and the value set contains the
        // methods that are nested/referenced from the given key method.
        let mut references: HashMap<String, HashSet<String>> = HashMap::default();

        // Populate the `references` map with information in the loop below.
        for nested_method_info in nested_method_infos {
            // We only care about methods that are referenced from the same
            // ADT.
            if adt_path != &nested_method_info.method_adt_path {
                continue;
            }

            let method_name = &nested_method_info.method_name;
            let referenced_method_name = &nested_method_info.method_call_name;

            match references.entry(method_name.clone()) {
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
                    ty_ctx.to_string_path(&adt_path)
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
        for (_, type_ids) in self.generic_adts.iter_mut() {
            let mut seen_ty_strings = HashSet::new();
            let mut i = 0;

            while i < type_ids.len() {
                let type_id = *type_ids.get(i).unwrap();
                let ty_string = ctx.ty_ctx.to_string_type_id(type_id)?;

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

impl Visitor for GenericCollector {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        let include_impls = true;
        let full_paths = false;

        match dependency_order_from_ctx(ctx, ast_token, include_impls, full_paths) {
            Ok(mut dependency_order) => {
                dependency_order.reverse();
                self.dependency_order_rev = dependency_order;
            }
            Err(mut errs) => {
                self.errors.append(&mut errs);
            }
        }

        if let AstToken::Block(BlockHeader::Default, .., body) = &mut ast_token {
            self.collect_nested(ctx, body);
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.collect_generic_adt(ctx, *type_id) {
            self.errors.push(err);
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Some(adt_type_id) = &fn_call.method_adt {
            if let Err(err) = self.collect_generic_method(ctx, *adt_type_id, fn_call) {
                self.errors.push(err);
            }
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        self.convert_nested_to_regular(ctx);

        debug!(
            "Before removal of duplicates -- self.generic_adts: {:#?}, self.generic_methods: {:#?}",
            self.generic_adts, self.generic_methods
        );

        if let Err(err) = self.remove_duplicates(ctx) {
            self.errors.push(err);
            return;
        }

        for type_ids in self.generic_adts.values_mut() {
            for type_id in type_ids {
                match ctx.ty_ctx.inferred_type(*type_id) {
                    Ok(inf_type_id) => *type_id = inf_type_id,
                    Err(err) => self.errors.push(err),
                }
            }
        }

        debug!(
            "After removal of duplicates -- self.generic_adts: {:#?}, self.generic_methods: {:#?}",
            self.generic_adts, self.generic_methods
        );
    }
}

struct NestedGenericCollector {
    /// The key String is the name of the function that this type/generics was
    /// found in.
    pub nested_generic_adts: HashMap<String, Vec<TypeId>>,

    /// The first String is the name of the function that this type/generics was
    /// found in and the second String is the name of the function call.
    pub nested_generic_methods: HashMap<NestedMethodInfo, Vec<Generics>>,

    /// Contains the name of the current function that is being visited.
    cur_func_name: String,

    errors: Vec<LangError>,
}

impl NestedGenericCollector {
    fn new() -> Self {
        Self {
            nested_generic_adts: HashMap::default(),
            nested_generic_methods: HashMap::default(),
            cur_func_name: "".into(),
            errors: Vec::default(),
        }
    }

    fn collect_nested_generic_adts(
        &mut self,
        ty_env: &mut TyEnv,
        type_id: TypeId,
    ) -> LangResult<()> {
        if !ty_env.contains_generic_shallow(type_id)? {
            return Ok(());
        }

        let ty = ty_env.ty(type_id)?.clone();
        match ty {
            Ty::CompoundType(inner_ty, generics, _) => {
                let mut contains_generic = false;
                for gen_type_id in generics.iter_types() {
                    if ty_env.contains_generic_shallow(*gen_type_id)? {
                        contains_generic = true;
                    }
                }

                if contains_generic && inner_ty.is_adt() {
                    match self.nested_generic_adts.entry(self.cur_func_name.clone()) {
                        Entry::Occupied(mut o) => {
                            if !o.get().contains(&type_id) {
                                o.get_mut().push(type_id);
                            }
                        }
                        Entry::Vacant(v) => {
                            v.insert(vec![type_id]);
                        }
                    }
                }
            }
            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                self.collect_nested_generic_adts(ty_env, type_id_i)?;
            }
            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    self.collect_nested_generic_adts(ty_env, type_id_i)?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn collect_nested_generic_methods(
        &mut self,
        ty_env: &mut TyEnv,
        fn_call: &FnCall,
    ) -> LangResult<()> {
        if fn_call.method_adt.is_none() {
            return Ok(());
        }

        let fn_call_generics = if let Some(fn_call_generics) = &fn_call.generics {
            if !fn_call_generics.is_empty() {
                fn_call_generics
            } else {
                return Ok(());
            }
        } else {
            return Ok(());
        };

        let mut contains_nested_generic = false;
        for gen_type_id in fn_call_generics.iter_types() {
            if ty_env.contains_generic_shallow(*gen_type_id)? {
                contains_nested_generic = true;
            }
        }

        if contains_nested_generic {
            let method_adt_path = ty_env
                .get_ident(*fn_call.method_adt.as_ref().unwrap())?
                .unwrap();
            let key = NestedMethodInfo {
                method_name: self.cur_func_name.clone(),
                method_call_name: fn_call.name.clone(),
                method_adt_path,
            };

            match self.nested_generic_methods.entry(key) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(fn_call_generics) {
                        o.get_mut().push(fn_call_generics.clone());
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![fn_call_generics.clone()]);
                }
            }
        }

        Ok(())
    }
}

impl Visitor for NestedGenericCollector {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Fn(func), ..) = ast_token {
            self.cur_func_name = func.borrow().name.clone();
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if fn_call.is_fn_ptr_call {
            return;
        }

        if let Err(err) = self.collect_nested_generic_methods(&mut ctx.ty_ctx.ty_env, fn_call) {
            self.errors.push(err);
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.collect_nested_generic_adts(&mut ctx.ty_ctx.ty_env, *type_id) {
            self.errors.push(err);
        }
    }
}
