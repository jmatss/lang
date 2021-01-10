use super::context::TypeContext;
use crate::block::BlockInfo;
use common::{
    error::LangError,
    token::{ast::AstToken, block::BlockHeader},
    traverser::{AstTraverser, TraverseContext},
    ty::ty::Ty,
    visitor::Visitor,
};
use std::collections::{hash_map::Entry, HashMap};

/// Iterates through the tokens and gathers all structures containing generics.
/// This information will be used to create new instances of the structures where
/// the generics are "implemented". Ex. "Struct<T>" might be implemented as
/// "Struct<i64>" & "Struct<f32>".
pub struct GenericStructsCollector<'a, 'tctx> {
    type_context: &'a mut TypeContext<'tctx>,

    /// Will contain all types containing generics. This will then be used to
    /// create new generic structures with the specific generic implementations.
    /// The String key is the name of the structure/type. The Ty values are the
    /// actual types containing generic implentations/instances.
    pub generic_structs: HashMap<String, Vec<Ty>>,

    /// Will contain all NESTED types containing generics. These are types containing
    /// generics that aren't part of the "using" structure.
    /// Ex. if there are two struct `A` and `B`, both containing generic parameters `T`.
    /// If one calls a function in `B` from `A`, that use of `B` will be considered
    /// a nested generic struct.
    ///
    /// These cases are special since the generic `T` in this case isn't replaced
    /// until a later stage. All other types of generics are solved at this stage.
    ///
    /// The String key is the name of the containing struct (`A` in the example)
    /// and the values are the contained structs (`B` in the example).
    ///
    /// The entrys in this map will be moved/converted into `self.generic_structs`
    /// before this stage is done. This one will be left empty.
    nested_generic_structs: HashMap<String, Vec<Ty>>,

    errors: Vec<LangError>,
}

impl<'a, 'tctx> GenericStructsCollector<'a, 'tctx> {
    pub fn new(type_context: &'a mut TypeContext<'tctx>) -> Self {
        Self {
            type_context,
            generic_structs: HashMap::default(),
            nested_generic_structs: HashMap::default(),
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

    fn collect_nested_generic_struct(&mut self, ast_tokens: &mut Vec<AstToken>) {
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

            if let Some(struct_name) = impl_opt {
                let mut nested_collector = NestedGenericStructsCollector::new(&struct_name);
                let mut traverser = AstTraverser::new();
                if let Err(errs) = traverser
                    .add_visitor(&mut nested_collector)
                    .traverse_token(ast_token)
                    .take_errors()
                {
                    self.errors.extend(errs.into_iter());
                }

                for (struct_name, tys) in nested_collector.nested_generic_structs.iter() {
                    for ty in tys {
                        match self.nested_generic_structs.entry(struct_name.clone()) {
                            Entry::Occupied(mut o) => {
                                if !o.get().contains(&ty) {
                                    o.get_mut().push(ty.clone());
                                }
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![ty.clone()]);
                            }
                        }
                    }
                }
            }
        }
    }
}

impl<'a, 'tctx> Visitor for GenericStructsCollector<'a, 'tctx> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Default, .., body) = &mut ast_token {
            self.collect_nested_generic_struct(body);
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.collect_generic_struct(ty);
    }

    fn visit_eof(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {
        // At this stage all `self.generic_structs` and `self.nested_generic_structs`
        // have been "collected". It is time to convert all nested generic structs
        // into "regular" generic structs.
        for (struct_name, nested_struct_tys) in std::mem::take(&mut self.nested_generic_structs) {
            for nested_struct_ty in nested_struct_tys.iter() {
                let nested_ident = if let Some(nested_ident) = nested_struct_ty.get_ident() {
                    nested_ident
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "Nested struct type doesn't contain ident: {:#?}",
                        nested_struct_ty
                    ));
                    self.errors.push(err);
                    continue;
                };

                let gen_struct_tys =
                    if let Some(gen_struct_tys) = self.generic_structs.get(&struct_name) {
                        gen_struct_tys.clone()
                    } else {
                        // This can happen if there are no implementation of a
                        // struct that contains generics.
                        continue;
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

                    match self.generic_structs.entry(nested_ident.clone()) {
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
        }
    }
}

pub struct NestedGenericStructsCollector<'a> {
    struct_name: &'a str,
    pub nested_generic_structs: HashMap<String, Vec<Ty>>,
    errors: Vec<LangError>,
}

impl<'a> NestedGenericStructsCollector<'a> {
    pub fn new(struct_name: &'a str) -> Self {
        Self {
            struct_name,
            nested_generic_structs: HashMap::default(),
            errors: Vec::default(),
        }
    }

    fn collect_nested_generic_structs(&mut self, ty: &Ty) {
        if !ty.contains_generic() {
            return;
        } else if let Some(ty_ident) = ty.get_ident() {
            if self.struct_name == ty_ident {
                return;
            }
        }

        match ty {
            Ty::CompoundType(inner_ty, generics, _) => {
                let contains_generic = generics.iter_types().any(|ty| ty.contains_generic());
                if contains_generic && inner_ty.is_structure() {
                    match self.nested_generic_structs.entry(self.struct_name.into()) {
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
}

impl<'a> Visitor for NestedGenericStructsCollector<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.collect_nested_generic_structs(ty);
    }
}
