use std::{collections::HashMap, sync::Arc};

use log::debug;

use common::{
    error::{LangError, LangResult},
    hash::DerefType,
    hash_map::TyEnvHashMap,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse_with_deep_copy, visitor::Visitor},
    ty::generics::Generics,
};

use super::generic_replace::GenericsReplacer;

/// Iterate through all functions that take generic parameters. Creates new
/// instances of them replacing the generics with actual implementations.
pub struct GenericFnCreator {
    /// The first string is the path of the ADT and the second string is the name
    /// of the method.
    generic_methods: TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,

    errors: Vec<LangError>,
}

impl GenericFnCreator {
    pub fn new(generic_methods: TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>) -> Self {
        debug!("generic_methods: {:#?}", generic_methods);

        Self {
            generic_methods,
            errors: Vec::default(),
        }
    }

    fn create_method_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path_without_gens: &LangPath,
        body: &mut Vec<AstToken>,
    ) -> Result<(), Vec<LangError>> {
        let mut generic_methods_opt = self
            .generic_methods
            .get(
                &ctx.ty_env.lock().unwrap(),
                DerefType::Deep,
                adt_path_without_gens,
            )
            .map_err(|err| vec![err])?
            .cloned();

        if let Some(generic_methods) = &mut generic_methods_opt {
            let mut idx = 0;

            while idx < body.len() {
                // TODO: Change so that every method doesn't need to be cloned.
                let method_token = body.get(idx).cloned().unwrap();

                let method = if let AstToken::Block(Block {
                    header: BlockHeader::Fn(func),
                    ..
                }) = &method_token
                {
                    Arc::clone(func)
                } else {
                    idx += 1;
                    continue;
                };

                let (method_name, has_gens) = {
                    let method = method.read().unwrap();
                    (
                        method.name.clone(),
                        method
                            .generics
                            .as_ref()
                            .map_or(false, |gens| !gens.is_empty()),
                    )
                };

                // If this method exists in `generic_methods`, this is a method
                // with generics that is used somewhere in the code base with
                // "implemented"/"instances" of the generics. Go through all
                // implementations of the generics and create a new method for
                // every generic impl.
                if generic_methods.contains_key(&method_name) {
                    for method_generics in generic_methods.get_mut(&method_name).unwrap() {
                        let mut new_method = method_token.clone();

                        // Before creating the new functions, make sure that the
                        // generics are fully inferred.
                        for gen_type_id in method_generics.iter_types_mut() {
                            let inf_type_id = ctx
                                .ty_env
                                .lock()
                                .unwrap()
                                .inferred_type(*gen_type_id)
                                .map_err(|e| vec![e])?;

                            if *gen_type_id != inf_type_id {
                                *gen_type_id = inf_type_id;
                            }
                        }

                        let mut generics_replacer = GenericsReplacer::new_func(method_generics);
                        traverse_with_deep_copy(ctx, &mut generics_replacer, &mut new_method, idx)?;

                        // Set the generic impls on the new copy of the function.
                        let func = if let AstToken::Block(Block {
                            header: BlockHeader::Fn(func),
                            ..
                        }) = &new_method
                        {
                            func.as_ref().write().unwrap().generics = Some(method_generics.clone());
                            Arc::clone(func)
                        } else {
                            unreachable!()
                        };

                        // Insert the method into the ADT.
                        ctx.ast_ctx
                            .insert_method(&ctx.ty_env.lock().unwrap(), adt_path_without_gens, func)
                            .map_err(|err| vec![err])?;

                        // Insert the method into the AST.
                        body.insert(idx, new_method);

                        idx += 1;
                    }

                    // When the logic above is done creating all generic impls for
                    // the method, `idx` will point to the old method with no generic
                    // impls, it should be removed.
                    // Remove it from both the structure and the AST.
                    ctx.ast_ctx
                        .remove_method(
                            &ctx.ty_env.lock().unwrap(),
                            adt_path_without_gens,
                            &method_name,
                        )
                        .map_err(|err| vec![err])?;

                    body.remove(idx);
                } else if has_gens {
                    // If the method has generics, but isn't in the `generic_methods`
                    // map, it means that it is a method that isn't used anywhere
                    // in the code base; remove it.
                    ctx.ast_ctx
                        .remove_method(
                            &ctx.ty_env.lock().unwrap(),
                            adt_path_without_gens,
                            &method_name,
                        )
                        .map_err(|err| vec![err])?;

                    body.remove(idx);
                } else {
                    idx += 1;
                }
            }
        }

        Ok(())
    }

    // Go through the methods in the given impl/ADT `body.
    // Remove any methods that have generics declared, but never implemented.
    // This is methods that isn't used anywhere in the code and must be removed
    // since the generics are never replaced/solved.
    fn remove_unused_methods(
        &mut self,
        ctx: &mut TraverseCtx,
        adt_path_without_gens: &LangPath,
        body: &mut Vec<AstToken>,
    ) -> LangResult<()> {
        let mut idx = 0;
        while idx < body.len() {
            let child_token = body.get(idx).unwrap();
            let mut was_removed = false;

            if let AstToken::Block(Block {
                header: BlockHeader::Fn(method),
                ..
            }) = child_token
            {
                let contains_gens_decl = method
                    .as_ref()
                    .read()
                    .unwrap()
                    .generics
                    .as_ref()
                    .map(|gens| !gens.is_empty())
                    .unwrap_or(false);
                let contains_gens_impl = self
                    .generic_methods
                    .get(
                        &ctx.ty_env.lock().unwrap(),
                        DerefType::Deep,
                        &adt_path_without_gens,
                    )
                    .ok()
                    .flatten()
                    .map(|impls| impls.contains_key(&method.as_ref().read().unwrap().name))
                    .unwrap_or(false);

                if contains_gens_decl && !contains_gens_impl {
                    ctx.ast_ctx.remove_method(
                        &ctx.ty_env.lock().unwrap(),
                        &adt_path_without_gens,
                        &method.as_ref().read().unwrap().name,
                    )?;
                    body.remove(idx);

                    was_removed = true;
                }
            }

            if !was_removed {
                idx += 1;
            }
        }

        Ok(())
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

    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let default_body = if let Block {
            header: BlockHeader::Default,
            body: default_body,
            ..
        } = block
        {
            default_body
        } else {
            unreachable!();
        };

        for body_token in default_body {
            let (adt_path_without_gens, child_body) = if let AstToken::Block(Block {
                header,
                body: child_body,
                ..
            }) = body_token
            {
                let path_without_gens = match header {
                    BlockHeader::Implement(adt_path, ..) => adt_path.clone(),
                    BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                        let adt = adt.read().unwrap();
                        adt.module.clone_push(&adt.name, None, Some(adt.file_pos))
                    }
                    _ => continue,
                };

                (path_without_gens, child_body)
            } else {
                continue;
            };

            if let Err(err) = self.create_method_instance(ctx, &adt_path_without_gens, child_body) {
                self.errors.extend(err);
                return;
            }

            if let Err(err) = self.remove_unused_methods(ctx, &adt_path_without_gens, child_body) {
                self.errors.push(err);
            }
        }
    }
}
