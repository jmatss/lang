use std::collections::{hash_map::Entry, HashMap, HashSet};

use log::debug;

use common::{
    ctx::{traverse_ctx::TraverseCtx, ty_ctx::TyCtx},
    error::{LangError, LangErrorKind, LangResult},
    path::LangPath,
    token::{ast::AstToken, block::BlockHeader, expr::FnCall},
    traverse::{traverser::traverse, visitor::Visitor},
    ty::generics::Generics,
    TypeId,
};

use crate::util::order::order_step2_strings;

use super::generic_nested_collector::{GenericNestedCollector, NestedMethodInfo};

/// Iterates through the tokens and gathers all methods containing generics.
/// Only the methods implementing the generic will be stored, any generic
/// containing the generic ex. "func<T>()" will NOT be saved, only ex.
/// "func<i64>()" & "func<f32>()".
///
/// This information will be used to create new instances of the methods where
/// the generics are "implemented". Ex. "func<T>()" might be implemented as
/// "func<i64>()" & "func<f32>()".
pub struct GenericFnCollector<'a> {
    /// The first LangPath is the path of the ADT that this method belongs to and
    /// the second key string is the name of the method.
    pub generic_methods: HashMap<LangPath, HashMap<String, Vec<Generics>>>,

    /// The first LangPath is the name of the ADT in which the information
    /// was found.
    nested_generic_methods: HashMap<LangPath, HashMap<NestedMethodInfo, Vec<Generics>>>,

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
            generic_methods: HashMap::default(),
            nested_generic_methods: HashMap::default(),
            dependency_order_rev,
            errors: Vec::default(),
        }
    }

    /// If the method with name `method_name`, on the ADT with type `adt_type_id`
    /// contains generics; this function will save all "implementing" methods
    /// where the generics have been given a "real type instance".
    /// This function also adds the names for the generics if they aren't already
    /// set.
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

        let adt_path = if let Some(adt_path) = ctx.ty_ctx.ty_env.get_ident(adt_type_id)? {
            adt_path
        } else {
            return Ok(());
        };

        let method = ctx
            .ast_ctx
            .get_method(&ctx.ty_ctx, &adt_path.without_gens(), &method_name)?;
        let method = method.borrow();

        if let Some(method_gens) = &method.generics {
            let method_gen_names = method_gens.iter_names().cloned().collect::<Vec<_>>();

            // Skip any methods that don't have their generic declarations
            // "implemented", only store method calls that have had the generics
            // "implemented" in `self.generic_methods`. OBS! This is only for
            // generics declared on the function, generic declarations on the ADT
            // is allowed at this point. This is because the ADT generics will
            // be resolved at a later time, so it is not a problem here.
            for gen_type_id in method_call_gens.iter_types() {
                if ctx
                    .ty_ctx
                    .ty_env
                    .contains_generic_with_name(*gen_type_id, &method_gen_names)?
                {
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
                    ctx.ty_ctx.to_string_path(&adt_path),
                    method_call_gens.len_types(),
                    method_gen_names.len()
                )));
            }

            let mut new_gens = Generics::new();
            for (name, ty) in method_gen_names.iter().zip(method_call_gens.iter_types()) {
                new_gens.insert(name.clone(), *ty);
            }

            // Insert the new generic types into `self.generic_methods`. These
            // generics will then be used when creating copies of the method.
            match self.generic_methods.entry(adt_path.without_gens()) {
                Entry::Occupied(mut o_outer) => match o_outer.get_mut().entry(method_name) {
                    Entry::Occupied(mut o_inner) => {
                        if !o_inner.get().contains(&new_gens) {
                            o_inner.get_mut().push(new_gens);
                        }
                    }
                    Entry::Vacant(v_inner) => {
                        v_inner.insert(vec![new_gens]);
                    }
                },
                Entry::Vacant(v_outer) => {
                    let mut m = HashMap::default();
                    m.insert(method_name, vec![new_gens]);
                    v_outer.insert(m);
                }
            }

            Ok(())
        } else {
            Err(ctx.ast_ctx.err(format!(
                "Method call \"{}\" on ADT \"{}\" has {} generics specified, \
                but the method declaration has no generics declared.",
                &method_name,
                ctx.ty_ctx.to_string_path(&adt_path),
                method_call_gens.len_types(),
            )))
        }
    }

    /// Collects any nested method calls containing generics.
    fn collect_nested(&mut self, ctx: &mut TraverseCtx, ast_tokens: &mut Vec<AstToken>) {
        for ast_token in ast_tokens {
            // Need to do this ugly hack to make the borrow checker happy.
            // We want to borrow the `adt_path` and use it in the logic below,
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
                let mut collector = GenericNestedCollector::new();
                if let Err(mut errs) = traverse(ctx, &mut collector, ast_token) {
                    self.errors.append(&mut errs);
                    continue;
                }

                for (nested_info, gens_vec) in collector.nested_generic_methods.iter() {
                    for gens in gens_vec {
                        match self.nested_generic_methods.entry(adt_path.without_gens()) {
                            Entry::Occupied(mut o_outer) => {
                                match o_outer.get_mut().entry(nested_info.clone()) {
                                    Entry::Occupied(mut o_inner) => {
                                        if !o_inner.get().contains(&gens) {
                                            o_inner.get_mut().push(gens.clone());
                                        }
                                    }
                                    Entry::Vacant(v_inner) => {
                                        v_inner.insert(vec![gens.clone()]);
                                    }
                                }
                            }
                            Entry::Vacant(v_outer) => {
                                let mut m = HashMap::default();
                                m.insert(nested_info.clone(), vec![gens.clone()]);
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
            "self.nested_generic_methods: {:#?}",
            self.nested_generic_methods
        );

        // Converts all `self.nested_generic_methods` into regular `self.generic_methods`.
        for adt_path in self.dependency_order_rev.iter() {
            let nested_gen_methods = if self.nested_generic_methods.contains_key(adt_path) {
                self.nested_generic_methods.remove(adt_path).unwrap()
            } else {
                continue;
            };

            // Figures out and returns a list of the methods in the order that
            // they should be iterated.
            let method_order = match Self::order(&ctx.ty_ctx, adt_path, nested_gen_methods.keys()) {
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
                    self.convert_nested_method_from_method(ctx, &adt_path, &method_info, generics);
                }
            }
        }
    }

    fn convert_nested_method_from_method(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        method_info: &NestedMethodInfo,
        generics: &Generics,
    ) {
        let func_name = &method_info.func_name;
        let method_call_name = &method_info.method_call_name;
        let method_adt_path = &method_info.method_adt_path;

        let gen_func_tys = if let Some(gen_func_tys) = self
            .generic_methods
            .get(adt_path)
            .map(|s| s.get(func_name))
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
                let type_id =
                    match ctx
                        .ty_ctx
                        .replace_gen_impls(&ctx.ast_ctx, *type_id, &generics_impls)
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
    fn order<'o, I>(
        ty_ctx: &TyCtx,
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
            if adt_path != &nested_method_info.method_adt_path {
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
                    ty_ctx.to_string_path(&adt_path)
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

    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Default, .., body) = &mut ast_token {
            self.collect_nested(ctx, body);
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
        debug!("self.generic_methods: {:#?}", self.generic_methods);
    }
}
