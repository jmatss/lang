use super::context::TypeContext;
use crate::{
    block::BlockInfo,
    util::order::{dependency_order, order_step2},
};
use common::{
    error::{CustomResult, LangError, LangErrorKind},
    token::{ast::AstToken, block::BlockHeader, expr::FuncCall},
    traverser::{AstTraverser, TraverseContext},
    ty::{generics::Generics, ty::Ty},
    visitor::Visitor,
    BlockId,
};
use core::panic;
use log::{debug, warn};
use std::collections::{hash_map::Entry, HashMap, HashSet};

/// Used to store information about a nested method.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct NestedMethodInfo {
    pub method_name: String,
    pub method_call_name: String,
    pub method_structure_name: String,
}

/// Iterates through the tokens and gathers all structures/methods containing
/// generics. Only the structures/methods implementing the generic will be stored,
/// any structure/generic containing the generic ex. `Struct<T>` will NOT be saved,
/// only ex. "Struct<i64>" & "Struct<f32>".
///
/// This information will be used to create new instances of the structures where
/// the generics are "implemented". Ex. "Struct<T>" might be implemented as
/// "Struct<i64>" & "Struct<f32>".
pub struct GenericCollector<'a, 'tctx> {
    type_context: &'a mut TypeContext<'tctx>,

    /// Will contain all types containing generics. This will then be used to
    /// create new generic structures with the specific generic implementations.
    /// The String key is the name of the structure/type. The Ty values are the
    /// actual types containing generic implentations/instances.
    pub generic_structs: HashMap<String, Vec<Ty>>,

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
    /// The entrys in this map will be moved/converted into `self.generic_structs`
    /// before this stage is done. This one will be left empty.
    ///
    /// The first String key is the name of the containing struct (`A` in the example)
    /// and the second String key is the name of the method in which these `Ty`s
    /// was found.
    nested_generic_structs: HashMap<String, HashMap<String, Vec<Ty>>>,

    /// The first string is the name of the structure and the second string is
    /// the name of the method.
    pub generic_methods: HashMap<String, HashMap<String, Vec<Generics>>>,

    /// See explanation for `nested_generic_structs` & `generic_methods`.
    /// The first String is the name of the structure in which the information
    /// was found.
    nested_generic_methods: HashMap<String, HashMap<NestedMethodInfo, Vec<Generics>>>,

    /// A list of all structure names in the dependecy order. This list will be
    /// in reverse order compared to how it is calculated. This means that a
    /// structure `A` depending on structure `B` would come before in the list.
    /// This is done since `A` should "expand" all its generic implementations
    /// before `B` is "calculated".
    dependency_order: Vec<String>,

    errors: Vec<LangError>,
}

impl<'a, 'tctx> GenericCollector<'a, 'tctx> {
    pub fn new(type_context: &'a mut TypeContext<'tctx>) -> Self {
        Self {
            type_context,
            generic_structs: HashMap::default(),
            nested_generic_structs: HashMap::default(),
            generic_methods: HashMap::default(),
            nested_generic_methods: HashMap::default(),
            // Will be overwritten and set when the "default block" is visted.
            dependency_order: Vec::with_capacity(0),
            errors: Vec::default(),
        }
    }

    /// If the given type represents a type that contains generics, this function
    /// will insert those into `self.generic_structs`. This map will in a later
    /// stage be used to create all the structures containing the different
    /// generic types.
    /// This function also adds the names for the generics if they aren't already
    /// set and that information is attainable.
    fn collect_generic_struct(&mut self, ty: &mut Ty) {
        // Do not create a "copy" of the actual structure type that contains the
        // generic declarations, should only create "copies" for the structures
        // that "implements" the generics.
        if ty.contains_generic() {
            return;
        }

        let ident = match ty {
            Ty::CompoundType(inner_ty, generics, ..) => {
                if !generics.is_empty() {
                    inner_ty.to_string()
                } else {
                    return;
                }
            }

            Ty::Pointer(ty_box, ..) | Ty::Array(ty_box, ..) => {
                self.collect_generic_struct(ty_box);
                return;
            }

            _ => return,
        };

        // TODO: Don't hardcode default id.
        let id = BlockInfo::DEFAULT_BLOCK_ID;

        // Set names of generics if they aren't set already.
        if let Err(err) = self.type_context.set_generic_names(ty, id) {
            self.errors.push(err);
            return;
        }

        match self.generic_structs.entry(ident) {
            Entry::Occupied(mut o) => {
                if !o.get().contains(ty) {
                    o.get_mut().push(ty.clone());
                }
            }
            Entry::Vacant(v) => {
                v.insert(vec![ty.clone()]);
            }
        }
    }

    /// If the method with name `method_name`, on the structure with type
    /// `structure_ty`, contains generics; this function will save all "implementing"
    /// methods where the generics have been given a "real type instance".
    /// This function also adds the names for the generics if they aren't already
    /// set.
    fn collect_generic_method(
        &mut self,
        structure_ty: &Ty,
        method_call: &FuncCall,
        block_id: BlockId,
    ) {
        let method_call_generics = if let Some(func_call_generics) = &method_call.generics {
            func_call_generics.clone()
        } else {
            return;
        };

        // Do not "store" any types containing generic declarations inside
        // `generic_methods`, only store method callsthat have had the generics
        // "implemented".
        if method_call_generics
            .iter_types()
            .any(|ty| ty.contains_generic())
        {
            return;
        }

        let method_name = method_call.name.clone();

        let structure_name = if let Some(structure_name) = structure_ty.get_ident() {
            structure_name
        } else {
            return;
        };

        let method = match self.type_context.analyze_context.get_method(
            &structure_name,
            &method_name,
            block_id,
        ) {
            Ok(method) => method,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let method = method.borrow();

        if let Some(func_generic_names) = &method.generic_names {
            // Ensure that the func call has the same amount of generic impls
            // as the func declaration has declared.
            if method_call_generics.len_types() != func_generic_names.len() {
                let err = self.type_context.analyze_context.err(format!(
                    "Func call for method \"{}\" has {} generics, but func decl expected {}.",
                    &method_name,
                    method_call_generics.len_types(),
                    func_generic_names.len()
                ));
                self.errors.push(err);
                return;
            }

            let mut new_generics = Generics::new();
            for (name, ty) in func_generic_names
                .iter()
                .zip(method_call_generics.iter_types())
            {
                new_generics.insert(name.clone(), ty.clone());
            }

            // Insert the new generic types into `self.generic_funcs`. These
            // generics will then be used when creating copies of the method.
            match self.generic_methods.entry(structure_name) {
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
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Func call for method \"{}\" on struct \"{}\"has generics, but func decl doesn't.",
                &method_name, &structure_name
            ));
            self.errors.push(err);
        }
    }

    fn collect_nested(&mut self, ast_tokens: &mut Vec<AstToken>) {
        for ast_token in ast_tokens {
            // Need to do this ugly hack to make the borrow checker happy.
            // We wan't to borrow the `struct_name` and use it in the logic below,
            // but at the same time borrow the `ast_token` which contains the
            // `struct_name`.
            let impl_opt = match ast_token {
                AstToken::Block(BlockHeader::Implement(struct_name, _), _, block_id, _) => {
                    if self
                        .type_context
                        .analyze_context
                        .get_struct(struct_name, *block_id)
                        .is_ok()
                    {
                        Some(struct_name.clone())
                    } else {
                        None
                    }
                }

                _ => continue,
            };

            if let Some(structure_name) = impl_opt {
                let mut nested_collector = NestedGenericCollector::new(&structure_name);
                let mut traverser = AstTraverser::new();
                if let Err(errs) = traverser
                    .add_visitor(&mut nested_collector)
                    .traverse_token(ast_token)
                    .take_errors()
                {
                    self.errors.extend(errs.into_iter());
                }

                for (method_name, tys) in nested_collector.nested_generic_structs.iter() {
                    for ty in tys {
                        match self.nested_generic_structs.entry(structure_name.clone()) {
                            Entry::Occupied(mut o_outer) => {
                                match o_outer.get_mut().entry(method_name.clone()) {
                                    Entry::Occupied(mut o_inner) => {
                                        if !o_inner.get().contains(&ty) {
                                            o_inner.get_mut().push(ty.clone());
                                        }
                                    }
                                    Entry::Vacant(v_inner) => {
                                        v_inner.insert(vec![ty.clone()]);
                                    }
                                }
                            }
                            Entry::Vacant(v_outer) => {
                                let mut m = HashMap::default();
                                m.insert(method_name.clone(), vec![ty.clone()]);
                                v_outer.insert(m);
                            }
                        }
                    }
                }

                for (nested_info, generics_vec) in nested_collector.nested_generic_methods.iter() {
                    for generics in generics_vec {
                        match self.nested_generic_methods.entry(structure_name.clone()) {
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

    fn convert_nested_to_regular(&mut self) {
        warn!("BEFORE self.generic_structs: {:#?}", self.generic_structs);
        warn!("BEFORE self.generic_methods: {:#?}", self.generic_methods);

        warn!(
            "self.nested_generic_structs: {:#?}",
            self.nested_generic_structs
        );

        // Converts all `self.nested_generic_structs` into regular `self.generic_structs`.
        for struct_name in self.dependency_order.clone().iter().rev() {
            if !self.nested_generic_structs.contains_key(struct_name) {
                continue;
            }

            let nested_gen_struct = self.nested_generic_structs.remove(struct_name).unwrap();
            for (method_name, nested_struct_tys) in nested_gen_struct.iter() {
                for nested_struct_ty in nested_struct_tys.iter() {
                    let nested_ident = if let Some(nested_ident) = nested_struct_ty.get_ident() {
                        nested_ident
                    } else {
                        let err = self.type_context.analyze_context.err(format!(
                            "Nested struct type doesn't contain ident: {:#?}",
                            nested_struct_ty
                        ));
                        self.errors.push(err);
                        return;
                    };

                    self.convert_nested_struct_from_struct(
                        &struct_name,
                        nested_struct_ty,
                        &nested_ident,
                    );
                    self.convert_nested_struct_from_method(
                        &struct_name,
                        method_name,
                        nested_struct_ty,
                        &nested_ident,
                    );
                }
            }
        }

        warn!(
            "self.dependency_order: {:#?}, self.nested_generic_methods: {:#?}",
            self.dependency_order, self.nested_generic_methods
        );

        // Converts all `self.nested_generic_methods` into regular `self.generic_methods`.
        for struct_name in self.dependency_order.clone().iter().rev() {
            warn!("loop struct_name: {}", struct_name);
            if !self.nested_generic_methods.contains_key(struct_name) {
                continue;
            }

            let nested_gen_methods = self.nested_generic_methods.remove(struct_name).unwrap();

            // Figures out and returns a list of the methods in the order that
            // they should be iterated.
            let method_order = match GenericCollector::order(struct_name, nested_gen_methods.keys())
            {
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
                warn!("order_method_name: {}", order_method_name);
                for (method_info, nested_method_tys) in &nested_gen_methods {
                    if &method_info.method_call_name == order_method_name {
                        warn!("INSIDE -- method_info: {:#?}", method_info);
                        for generics in nested_method_tys.iter() {
                            self.convert_nested_method_from_struct(
                                &struct_name,
                                &method_info,
                                generics,
                            );
                            self.convert_nested_method_from_method(
                                &struct_name,
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
                    self.convert_nested_method_from_struct(&struct_name, &method_info, generics);
                    self.convert_nested_method_from_method(&struct_name, &method_info, generics);
                }
            }
        }

        warn!("AFTER self.generic_structs: {:#?}", self.generic_structs);
        warn!("AFTER self.generic_methods: {:#?}", self.generic_methods);
    }

    fn convert_nested_struct_from_struct(
        &mut self,
        struct_name: &str,
        nested_struct_ty: &Ty,
        nested_ident: &str,
    ) {
        let gen_struct_tys = if let Some(gen_struct_tys) = self.generic_structs.get(struct_name) {
            gen_struct_tys.clone()
        } else {
            // This can happen if there are no implementation of a
            // struct that contains generics.
            return;
        };

        // Go through all implementations of the struct `struct_name` and
        // use those generic instances to create new structs instances for
        // the nested one. This will be done for all generics of the
        // outer struct.
        for gen_struct_ty in gen_struct_tys {
            let generics = if let Ty::CompoundType(_, generics, ..) = gen_struct_ty {
                generics
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Generic instance type not compound: {:#?}",
                    gen_struct_ty
                ));
                self.errors.push(err);
                return;
            };

            let mut nested_ty_clone = nested_struct_ty.clone();
            nested_ty_clone.replace_generics_impl(&generics);

            if &nested_ty_clone == nested_struct_ty {
                continue;
            }

            match self.generic_structs.entry(nested_ident.into()) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(&nested_ty_clone) {
                        o.get_mut().push(nested_ty_clone.clone());
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![nested_ty_clone.clone()]);
                }
            };
        }
    }

    fn convert_nested_struct_from_method(
        &mut self,
        struct_name: &str,
        method_name: &str,
        nested_struct_ty: &Ty,
        nested_ident: &str,
    ) {
        let gen_func_tys = if let Some(gen_func_tys) = self
            .generic_methods
            .get(struct_name)
            .map(|s| s.get(method_name))
            .flatten()
        {
            gen_func_tys
        } else {
            return;
        };

        // Go through all implementations of the method `method_Name` and
        // use those generic instances to create new method instances for
        // the nested one.
        for generics in gen_func_tys {
            let mut nested_ty_clone = nested_struct_ty.clone();
            nested_ty_clone.replace_generics_impl(&generics);

            if &nested_ty_clone == nested_struct_ty {
                continue;
            }

            match self.generic_structs.entry(nested_ident.into()) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(&nested_ty_clone) {
                        o.get_mut().push(nested_ty_clone.clone());
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![nested_ty_clone.clone()]);
                }
            };
        }
    }

    fn convert_nested_method_from_struct(
        &mut self,
        struct_name: &str,
        method_info: &NestedMethodInfo,
        generics: &Generics,
    ) {
        let method_call_name = &method_info.method_call_name;
        let method_structure_name = &method_info.method_structure_name;

        let gen_struct_tys = if let Some(gen_struct_tys) = self.generic_structs.get(struct_name) {
            gen_struct_tys.clone()
        } else {
            return;
        };

        // TODO:
        let block_id = BlockInfo::DEFAULT_BLOCK_ID;

        let method_gen_names = match self.type_context.analyze_context.get_method(
            method_structure_name,
            method_call_name,
            block_id,
        ) {
            Ok(method) => {
                if let Some(method_gen_names) = &method.borrow().generic_names {
                    method_gen_names.clone()
                } else {
                    panic!(
                        "method_structure_name: {}, method_call_name: {}, block_id: {}",
                        method_structure_name, method_call_name, block_id
                    );
                }
            }
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        warn!(
            "DEF456 -- struct_name: {}, method_info: {:#?}, ",
            struct_name, method_info
        );

        for gen_struct_ty in gen_struct_tys {
            warn!("gen_struct_ty: {:#?}", gen_struct_ty);
            let struct_generics = if let Ty::CompoundType(_, struct_generics, ..) = gen_struct_ty {
                struct_generics
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Generic instance type not compound: {:#?}",
                    gen_struct_ty
                ));
                self.errors.push(err);
                return;
            };

            warn!("BEFORE generics: {:#?}", generics);

            // Iterate through the types and replace any generics found in the
            // function declaration. Also add names of the generics into `Generics`
            // if they are missing (might have no effect if the names are set already).
            let mut new_generics = Generics::new();
            for (name, ty) in method_gen_names.iter().zip(generics.iter_types()) {
                let mut new_ty = ty.clone();
                new_ty.replace_generics_impl(&struct_generics);
                new_generics.insert(name.clone(), new_ty);
            }

            warn!("AFTER new_generics: {:#?}", new_generics);

            if &new_generics == generics {
                continue;
            }

            // The method call is done on the structure `method_structure_name`, so
            // use that instead of the `struct_name` which represents the structure
            // in which this call was found.
            match self.generic_methods.entry(method_structure_name.into()) {
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
        struct_name: &str,
        method_info: &NestedMethodInfo,
        generics: &Generics,
    ) {
        let method_name = &method_info.method_name;
        let method_call_name = &method_info.method_call_name;
        let method_structure_name = &method_info.method_structure_name;

        warn!(
            "ABC123, method_name: {}, struct_name: {}, method_call_name: {}, method_structure_name: {}",
            method_name, struct_name, method_call_name, method_structure_name
        );

        let gen_func_tys = if let Some(gen_func_tys) = self
            .generic_methods
            .get(struct_name)
            .map(|s| s.get(method_name))
            .flatten()
        {
            gen_func_tys.clone()
        } else {
            warn!("NOT!");
            return;
        };

        warn!(
            "ABC123, method_name: {}, method_call_name: {}, method_structure_name: {}, gen_func_tys: {:#?}",
            method_name, method_call_name, method_structure_name, gen_func_tys
        );

        // TODO:
        let block_id = BlockInfo::DEFAULT_BLOCK_ID;

        let method_gen_names = match self.type_context.analyze_context.get_method(
            method_structure_name,
            method_call_name,
            block_id,
        ) {
            Ok(method) => {
                if let Some(method_gen_names) = &method.borrow().generic_names {
                    method_gen_names.clone()
                } else {
                    panic!(
                        "method_structure_name: {}, method_call_name: {}, block_id: {}",
                        method_structure_name, method_call_name, block_id
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
            for (name, ty) in method_gen_names.iter().zip(generics.iter_types()) {
                let mut new_ty = ty.clone();
                new_ty.replace_generics_impl(&generics_impls);
                new_generics.insert(name.clone(), new_ty);
            }

            if &new_generics == generics {
                continue;
            }

            // The method call is done on the structure `method_structure_name`, so
            // use that instead of the `struct_name` which represents the structure
            // in which this call was found.
            match self.generic_methods.entry(method_structure_name.into()) {
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
    /// "expanded". The returned vector will contain the names of the methods in
    /// the order in which they should be converted to "regular" generic methods.
    fn order<I>(structure_name: &str, nested_method_infos: I) -> CustomResult<Vec<String>>
    where
        I: IntoIterator<Item = &'a NestedMethodInfo>,
    {
        // The String key is the name of the method and the value set contains the
        // methods that are nested/referenced from the given key method.
        let mut references: HashMap<String, HashSet<String>> = HashMap::default();

        // Populate the `references` map with information in the loop below.
        for nested_method_info in nested_method_infos {
            // We only care about methods that are referenced from the same
            // structure.
            if structure_name != nested_method_info.method_structure_name {
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
        match order_step2(&references) {
            Ok(order) => {
                debug!(
                    "method -- references: {:#?}, order: {:#?}",
                    references, order
                );
                Ok(order)
            }
            Err(cyc_err) => Err(LangError::new(
                format!(
                    "Cyclic dependency between method \"{}\" and \"{}\" in structure \"{}\".",
                    cyc_err.0, cyc_err.1, structure_name
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }
}

impl<'a, 'tctx> Visitor for GenericCollector<'a, 'tctx> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let include_impls = true;
        let full_names = false;
        match dependency_order(ast_token, include_impls, full_names) {
            Ok(dependency_order) => {
                self.dependency_order = dependency_order;
            }
            Err(mut errs) => {
                self.errors.append(&mut errs);
            }
        }

        if let AstToken::Block(BlockHeader::Default, .., body) = &mut ast_token {
            self.collect_nested(body);
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.collect_generic_struct(ty);
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        if let Some(structure_ty) = &func_call.method_structure {
            self.collect_generic_method(structure_ty, func_call, ctx.block_id);
        }
    }

    fn visit_eof(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {
        warn!("self.generic_structs: {:#?}", self.generic_structs);
        self.convert_nested_to_regular();
    }
}

struct NestedGenericCollector<'a> {
    structure_name: &'a str,

    /// The key String is the name of the function that this type/generics was
    /// found in.
    pub nested_generic_structs: HashMap<String, Vec<Ty>>,

    /// The first String is the name of the function that this type/generics was
    /// found in and the second String is the name of the function call.
    pub nested_generic_methods: HashMap<NestedMethodInfo, Vec<Generics>>,

    /// Contains the name of the current function that is being visited.
    cur_func_name: String,

    errors: Vec<LangError>,
}

impl<'a> NestedGenericCollector<'a> {
    fn new(structure_name: &'a str) -> Self {
        Self {
            structure_name,
            nested_generic_structs: HashMap::default(),
            nested_generic_methods: HashMap::default(),
            cur_func_name: "".into(),
            errors: Vec::default(),
        }
    }

    fn collect_nested_generic_structs(&mut self, ty: &Ty) {
        if !ty.contains_generic() {
            return;
        } else if let Some(ty_ident) = ty.get_ident() {
            if self.structure_name == ty_ident {
                return;
            }
        }

        match ty {
            Ty::CompoundType(inner_ty, generics, _) => {
                let contains_generic = generics.iter_types().any(|ty| ty.contains_generic());
                if contains_generic && inner_ty.is_structure() {
                    match self
                        .nested_generic_structs
                        .entry(self.cur_func_name.clone())
                    {
                        Entry::Occupied(mut o) => {
                            if !o.get().contains(ty) {
                                o.get_mut().push(ty.clone());
                            }
                        }
                        Entry::Vacant(v) => {
                            v.insert(vec![ty.clone()]);
                        }
                    }
                }
            }
            Ty::Pointer(ty_i, _) | Ty::Array(ty_i, _, _) => {
                self.collect_nested_generic_structs(ty_i);
            }
            Ty::Expr(expr, _) => {
                if let Ok(ty_i) = expr.get_expr_type() {
                    self.collect_nested_generic_structs(&ty_i);
                }
            }
            _ => (),
        }
    }

    fn collect_nested_generic_methods(&mut self, func_call: &FuncCall) {
        let func_call_generics = if let Some(func_call_generics) = &func_call.generics {
            if !func_call_generics.is_empty() {
                func_call_generics
            } else {
                return;
            }
        } else {
            return;
        };

        let contains_nested_generic = func_call_generics
            .iter_types()
            .any(|ty| ty.contains_generic());
        if contains_nested_generic {
            let key = NestedMethodInfo {
                method_name: self.cur_func_name.clone(),
                method_call_name: func_call.name.clone(),
                method_structure_name: func_call
                    .method_structure
                    .as_ref()
                    .unwrap()
                    .get_ident()
                    .unwrap(),
            };

            match self.nested_generic_methods.entry(key) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(func_call_generics) {
                        o.get_mut().push(func_call_generics.clone());
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![func_call_generics.clone()]);
                }
            }
        }
    }
}

impl<'a> Visitor for NestedGenericCollector<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_func(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Function(func), ..) = ast_token {
            self.cur_func_name = func.borrow().name.clone();
        }
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        self.collect_nested_generic_methods(func_call);
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.collect_nested_generic_structs(ty);
    }
}
