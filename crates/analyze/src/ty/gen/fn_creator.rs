use std::{collections::HashMap, sync::Arc};

use log::debug;

use common::{
    ctx::block_ctx::BlockCtx,
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
    BlockId,
};

use super::replace::GenericsReplacer;

/// Iterate through all functions that take generic parameters. Creates new
/// instances of them replacing the generics with actual implementations.
pub struct GenericFnCreator {
    /// The first string is the path of the ADT and the string used as a key in
    /// the inner map is the name of the method.
    generic_methods: TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,

    /// The key is the path to the function without generics and the values is
    /// a vector containing all implementation of thhe generucs.
    generic_fns: TyEnvHashMap<LangPath, Vec<Generics>>,

    errors: Vec<LangError>,
}

impl GenericFnCreator {
    pub fn new(
        generic_methods: TyEnvHashMap<LangPath, HashMap<String, Vec<Generics>>>,
        generic_fns: TyEnvHashMap<LangPath, Vec<Generics>>,
    ) -> Self {
        debug!(
            "generic_methods: {:#?}\ngeneric_fns: {:#?}",
            generic_methods, generic_fns
        );

        Self {
            generic_methods,
            generic_fns,
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
            .get(&ctx.ty_env.lock(), DerefType::Deep, adt_path_without_gens)
            .map_err(|err| vec![err])?
            .cloned();

        let generic_methods = if let Some(generic_methods) = &mut generic_methods_opt {
            generic_methods
        } else {
            return Ok(());
        };

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
                let method = method.read();
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

                    // TODO: Can this be removed?
                    // Before creating the new functions, make sure that the
                    // generics are fully inferred.
                    for gen_type_id in method_generics.iter_types_mut() {
                        let inf_type_id = ctx
                            .ty_env
                            .lock()
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
                        func.write().generics = Some(method_generics.clone());
                        Arc::clone(func)
                    } else {
                        unreachable!()
                    };

                    // Insert the method into the ADT.
                    ctx.ast_ctx
                        .insert_method(&ctx.ty_env.lock(), adt_path_without_gens, func)
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
                    .remove_method(&ctx.ty_env.lock(), adt_path_without_gens, &method_name)
                    .map_err(|err| vec![err])?;

                body.remove(idx);
            } else if has_gens {
                // If the method has generics, but isn't in the `generic_methods`
                // map, it means that it is a method that isn't used anywhere
                // in the code base; remove it.
                ctx.ast_ctx
                    .remove_method(&ctx.ty_env.lock(), adt_path_without_gens, &method_name)
                    .map_err(|err| vec![err])?;

                body.remove(idx);
            } else {
                idx += 1;
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
                    .read()
                    .generics
                    .as_ref()
                    .map(|gens| !gens.is_empty())
                    .unwrap_or(false);
                let contains_gens_impl = self
                    .generic_methods
                    .get(&ctx.ty_env.lock(), DerefType::Deep, &adt_path_without_gens)
                    .ok()
                    .flatten()
                    .map(|impls| impls.contains_key(&method.read().name))
                    .unwrap_or(false);

                if contains_gens_decl && !contains_gens_impl {
                    ctx.ast_ctx.remove_method(
                        &ctx.ty_env.lock(),
                        &adt_path_without_gens,
                        &method.read().name,
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

    /// The `default_body` is the body of the default block. The `default_idx`
    /// is the index of the given function token inside the `default_body` that
    /// is being "handled" in this function.
    ///
    /// The returned `usize` indicates how many functions that was created.
    fn create_fn_instance(
        &mut self,
        ctx: &mut TraverseCtx,
        default_body: &mut Vec<AstToken>,
        default_idx: BlockId,
    ) -> Result<usize, Vec<LangError>> {
        let block_id = BlockCtx::DEFAULT_BLOCK_ID;

        let fn_token = default_body.get(default_idx).unwrap();

        let func = if let AstToken::Block(Block {
            header: BlockHeader::Fn(func),
            ..
        }) = fn_token
        {
            func
        } else {
            unreachable!();
        };

        let fn_path_without_gens = {
            let func = func.read();
            func.module
                .clone_push(&func.name, None, Some(func.file_pos))
        };

        let mut generic_fns_opt = self
            .generic_fns
            .get(&ctx.ty_env.lock(), DerefType::Deep, &fn_path_without_gens)
            .map_err(|err| vec![err])?
            .cloned();

        let generic_fns = if let Some(generic_fns) = &mut generic_fns_opt {
            generic_fns
        } else {
            return Ok(0);
        };

        let new_fn_token = fn_token.clone();

        for (idx, gens) in generic_fns.iter_mut().enumerate() {
            let mut new_fn = new_fn_token.clone();

            // TODO: Can this be removed?
            // Before creating the new functions, make sure that the
            // generics are fully inferred.
            for gen_type_id in gens.iter_types_mut() {
                let inf_type_id = ctx
                    .ty_env
                    .lock()
                    .inferred_type(*gen_type_id)
                    .map_err(|e| vec![e])?;

                if *gen_type_id != inf_type_id {
                    *gen_type_id = inf_type_id;
                }
            }

            let mut generics_replacer = GenericsReplacer::new_func(gens);
            traverse_with_deep_copy(ctx, &mut generics_replacer, &mut new_fn, idx)?;

            // Set the generic impls on the new copy of the function.
            let func = if let AstToken::Block(Block {
                header: BlockHeader::Fn(func),
                ..
            }) = &new_fn
            {
                func.write().generics = Some(gens.clone());
                Arc::clone(func)
            } else {
                unreachable!()
            };

            let fn_path_with_gens = {
                let func = func.read();
                func.module
                    .clone_push(&func.name, func.generics.as_ref(), Some(func.file_pos))
            };

            // Insert the function into the lookups.
            ctx.ast_ctx
                .fns
                .insert(
                    &ctx.ty_env.lock(),
                    DerefType::Deep,
                    (fn_path_with_gens, block_id),
                    func,
                )
                .map_err(|e| vec![e])?;

            // Insert the function into the AST at the position after the
            // original function without the generics implemented.
            default_body.insert(default_idx + 1, new_fn);
        }

        // When the logic above is done creating all generic impls for the
        // function, `idx` will point to the old function with no generic impls,
        // it should be removed. Remove it from both the lookups and the AST.
        ctx.ast_ctx
            .fns
            .remove(
                &ctx.ty_env.lock(),
                DerefType::None,
                &(fn_path_without_gens, block_id),
            )
            .map_err(|err| vec![err])?;

        default_body.remove(default_idx);

        Ok(generic_fns.len())
    }

    /// The `default_body` is the body of the default block. The `default_idx`
    /// is the index of the given function token inside the `default_body` that
    /// is being "handled" in this function.
    ///
    /// The returned `bool` indicates if the function was removed or not.
    fn remove_unused_fns(
        &mut self,
        ctx: &mut TraverseCtx,
        default_body: &mut Vec<AstToken>,
        default_idx: BlockId,
    ) -> LangResult<bool> {
        let block_id = BlockCtx::DEFAULT_BLOCK_ID;

        let fn_token = default_body.get(default_idx).unwrap();

        let func = if let AstToken::Block(Block {
            header: BlockHeader::Fn(func),
            ..
        }) = fn_token
        {
            func
        } else {
            unreachable!();
        };
        let func = func.read();

        let fn_path_without_gens = func
            .module
            .clone_push(&func.name, None, Some(func.file_pos));

        let contains_gens_decl = func
            .generics
            .as_ref()
            .map(|gens| !gens.is_empty())
            .unwrap_or(false);
        let contains_gens_impl = self
            .generic_fns
            .get(&ctx.ty_env.lock(), DerefType::None, &fn_path_without_gens)?
            .is_some();

        std::mem::drop(func);

        if contains_gens_decl && !contains_gens_impl {
            ctx.ast_ctx.fns.remove(
                &ctx.ty_env.lock(),
                DerefType::None,
                &(fn_path_without_gens, block_id),
            )?;
            default_body.remove(default_idx);

            Ok(true)
        } else {
            Ok(false)
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

        // Create and remove methods tied to ADTs.
        for body_token in default_body.iter_mut() {
            let (adt_path_without_gens, child_body) = if let AstToken::Block(Block {
                header,
                body: child_body,
                ..
            }) = body_token
            {
                let path_without_gens = match header {
                    BlockHeader::Implement(adt_path, ..) => adt_path.clone(),
                    BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                        let adt = adt.read();
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

        // Create and remove free-standing functions.
        let mut idx = 0;
        while idx < default_body.len() {
            // Will be set to true if a function was removed in the logic below.
            let mut fn_was_removed = false;

            if let AstToken::Block(Block {
                header: BlockHeader::Fn(..),
                ..
            }) = default_body.get(idx).unwrap()
            {
                let amount_of_fns_created = match self.create_fn_instance(ctx, default_body, idx) {
                    Ok(0) => 0,
                    // -1 since the original fn (with no generics) is removed.
                    Ok(amount) => amount - 1,
                    Err(err) => {
                        self.errors.extend(err);
                        return;
                    }
                };

                idx += amount_of_fns_created;

                if amount_of_fns_created == 0 {
                    fn_was_removed = match self.remove_unused_fns(ctx, default_body, idx) {
                        Ok(removed) => removed,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                }
            }

            if !fn_was_removed {
                idx += 1;
            }
        }
    }
}
