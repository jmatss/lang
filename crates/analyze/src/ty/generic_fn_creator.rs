use std::{collections::HashMap, rc::Rc};

use log::debug;

use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::LangError,
    path::LangPath,
    token::{ast::AstToken, block::BlockHeader},
    traverse::{traverser::traverse_with_deep_copy, visitor::Visitor},
    ty::generics::Generics,
};

use super::generic_replace::GenericsReplacer;

/// Iterate through all functions that take generic parameters. Creates new
/// instances of them replacing the generics with actual implementations.
pub struct GenericFnCreator {
    /// The first string is the path of the ADT and the second string is the name
    /// of the method.
    generic_methods: HashMap<LangPath, HashMap<String, Vec<Generics>>>,

    errors: Vec<LangError>,
}

impl GenericFnCreator {
    pub fn new(generic_methods: HashMap<LangPath, HashMap<String, Vec<Generics>>>) -> Self {
        debug!("generic_methods: {:#?}", generic_methods);

        Self {
            generic_methods,
            errors: Vec::default(),
        }
    }

    fn create_method_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path: &LangPath,
        impl_body: &mut Vec<AstToken>,
    ) {
        if let Some(generic_methods) = self.generic_methods.get(adt_path).cloned() {
            let mut idx = 0;

            while idx < impl_body.len() {
                // TODO: Change so that every method doesn't need to be cloned.
                let method = impl_body.get(idx).cloned().unwrap();

                let (method_name, has_gens) =
                    if let AstToken::Block(BlockHeader::Fn(func), ..) = &method {
                        let func = func.borrow();
                        (
                            func.name.clone(),
                            func.generics
                                .as_ref()
                                .map_or(false, |gens| !gens.is_empty()),
                        )
                    } else {
                        panic!()
                    };

                // If this method exists in `generic_methods`, this is a method
                // with generics that is used somewhere in the code base with
                // "implemented"/"instances" of the generics. Go through all
                // implementations of the generics and create a new method for
                // every generic impl.
                if generic_methods.contains_key(&method_name) {
                    for method_generics in generic_methods.get(&method_name).unwrap() {
                        let mut new_method = method.clone();

                        let mut generics_replacer = GenericsReplacer::new_func(method_generics);

                        if let Err(mut errs) = traverse_with_deep_copy(
                            ctx,
                            &mut generics_replacer,
                            &mut new_method,
                            idx,
                        ) {
                            self.errors.append(&mut errs);
                            return;
                        }

                        // Set the generic impls on the new copy of the function.
                        let func = if let AstToken::Block(BlockHeader::Fn(func), ..) = &new_method {
                            func.borrow_mut().generics = Some(method_generics.clone());
                            Rc::clone(func)
                        } else {
                            panic!()
                        };

                        // Insert the method into the structure.
                        if let Err(err) = ctx.ast_ctx.insert_method(&ctx.ty_ctx, adt_path, func) {
                            self.errors.push(err);
                            return;
                        }

                        // Insert the method into the AST.
                        impl_body.insert(idx, new_method);

                        idx += 1;
                    }

                    // When the logic above is done creating all generic impls for
                    // the method, `idx` will point to the old method with no generic
                    // impls, it should be removed.
                    // Remove it from both the structure and the AST.
                    if let Err(err) = ctx
                        .ast_ctx
                        .remove_method(&ctx.ty_ctx, adt_path, &method_name)
                    {
                        self.errors.push(err);
                        return;
                    }

                    impl_body.remove(idx);
                } else if has_gens {
                    // If the method has generics, but isn't in the `generic_methods`
                    // map, it means that it is a method that isn't used anywhere
                    // in the code base; remove it.
                    if let Err(err) = ctx
                        .ast_ctx
                        .remove_method(&ctx.ty_ctx, adt_path, &method_name)
                    {
                        self.errors.push(err);
                        return;
                    }

                    impl_body.remove(idx);
                } else {
                    idx += 1;
                }
            }
        }
    }
}

impl Visitor for GenericFnCreator {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Default, .., body) = &mut ast_token {
            let mut i = 0;

            // Iterate through all the implement blocks in the AST. For every method
            // that exists in `self.generic_methods`, create new clones where the
            // generics are "implemented".
            while i < body.len() {
                let mut body_token = body.get_mut(i).expect("Known to be in bounds.");

                if let AstToken::Block(BlockHeader::Implement(adt_path, _), .., impl_body) =
                    &mut body_token
                {
                    let adt_path = adt_path.clone();

                    if self.generic_methods.contains_key(&adt_path) {
                        self.create_method_instance(ctx, &adt_path, impl_body);
                    }

                    // Go through the methods in the impl block one more time.
                    // Remove any methods that have generics declared, but never
                    // implemented. This is methods that isn't used anywhere in
                    // the code and must be removed since the generics are never
                    // replaced.
                    let mut idx = 0;
                    while idx < impl_body.len() {
                        let method = impl_body.get(idx).unwrap();

                        if let AstToken::Block(BlockHeader::Fn(func), ..) = method {
                            let contains_gens_decl = func
                                .borrow()
                                .generics
                                .as_ref()
                                .map(|gens| !gens.is_empty())
                                .unwrap_or(false);
                            let contains_gens_impl = self
                                .generic_methods
                                .get(&adt_path)
                                .map(|impls| impls.contains_key(&func.borrow().name))
                                .unwrap_or(false);

                            if contains_gens_decl && !contains_gens_impl {
                                if let Err(err) = ctx.ast_ctx.remove_method(
                                    &ctx.ty_ctx,
                                    &adt_path,
                                    &func.borrow().name,
                                ) {
                                    self.errors.push(err);
                                    return;
                                }

                                impl_body.remove(idx);
                            } else {
                                idx += 1;
                            }
                        }
                    }
                }

                i += 1;
            }
        }
    }

    // TODO: Implement similar generic logic as for structs.
    fn visit_trait(&mut self, _ast_token: &mut AstToken, _ctx: &mut TraverseCtx) {}
    fn visit_enum(&mut self, _ast_token: &mut AstToken, _ctx: &mut TraverseCtx) {}
}
