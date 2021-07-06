use std::sync::{Arc, RwLock};

use common::{
    ctx::block_ctx::BlockCtx,
    error::{LangError, LangResult},
    hash::DerefType,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Adt, Block, BlockHeader},
        stmt::{ExternalDecl, Stmt},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::to_string::to_string_path,
    BlockId,
};

/// Gathers information about all type declarations found in the AST and inserts
/// them into the `analyze_context`. This includes structs, enums, unions and traits.
pub struct DeclTypeAnalyzer {
    errors: Vec<LangError>,
}

impl DeclTypeAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    /// Adds the ADT to the lookup tables for ADTs. Also normalizes the name of
    /// the ADT with the correct module path. Assign the `module` variable stored
    /// inside the ADT the value of the module that it is declared in.
    fn decl_new_adt(
        &mut self,
        ctx: &mut TraverseCtx,
        adt: &Arc<RwLock<Adt>>,
        id: BlockId,
    ) -> LangResult<()> {
        let module = ctx.ast_ctx.get_module(id)?.unwrap_or_else(LangPath::empty);
        adt.as_ref().write().unwrap().module = module.clone();

        let adt_full_path = {
            let adt_lock = adt.as_ref().read().unwrap();
            module.clone_push(&adt_lock.name, None, Some(adt_lock.file_pos))
        };

        let ty_env_guard = ctx.ty_env.lock().unwrap();

        // TODO: Add file positions to error message.
        if ctx.ast_ctx.get_adt(&ty_env_guard, &adt_full_path).is_ok() {
            return Err(ctx.ast_ctx.err(format!(
                "A ADT with name \"{}\" already defined.",
                to_string_path(&ty_env_guard, &adt_full_path)
            )));
        }

        let parent_id = ctx.ast_ctx.get_parent_id(id)?;

        let key = (adt_full_path, parent_id);
        ctx.ast_ctx
            .adts
            .insert(&ty_env_guard, DerefType::None, key, Arc::clone(adt))?;

        Ok(())
    }
}

impl Visitor for DeclTypeAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        ctx.ast_ctx.file_pos = ast_token.file_pos().cloned().unwrap_or_default();
    }

    fn visit_struct(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(struct_),
            id,
            ..
        } = &mut block
        {
            if let Err(err) = self.decl_new_adt(ctx, struct_, *id) {
                self.errors.push(err);
            }
        }
    }

    fn visit_enum(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Enum(enum_),
            id,
            ..
        } = &mut block
        {
            if let Err(err) = self.decl_new_adt(ctx, enum_, *id) {
                self.errors.push(err);
            }
        }
    }

    fn visit_union(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(union),
            id,
            ..
        } = &mut block
        {
            if let Err(err) = self.decl_new_adt(ctx, union, *id) {
                self.errors.push(err);
            }
        }
    }

    // TODO: Merge logic with ADTs.
    fn visit_trait(&mut self, mut block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Trait(trait_),
            id,
            ..
        } = &mut block
        {
            // The trait will be added in the scope of its parent, so fetch
            // the block id for the parent.
            let parent_id = match ctx.ast_ctx.get_parent_id(*id) {
                Ok(parent_id) => parent_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let trait_full_path = match ctx.ast_ctx.get_module(*id) {
                Ok(module_opt) => {
                    let module = module_opt.unwrap_or_else(LangPath::empty);
                    trait_.as_ref().write().unwrap().module = module.clone();

                    let trait_ = trait_.as_ref().read().unwrap();
                    module.clone_push(&trait_.name, None, Some(trait_.file_pos))
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let ty_env_guard = ctx.ty_env.lock().unwrap();

            // TODO: Add file positions to error message.
            if ctx
                .ast_ctx
                .get_trait(&ty_env_guard, &trait_full_path)
                .is_ok()
            {
                self.errors.push(ctx.ast_ctx.err(format!(
                    "A trait with name \"{}\" already defined.",
                    to_string_path(&ty_env_guard, &trait_full_path)
                )));
                return;
            }

            let key = (trait_full_path, parent_id);
            if let Err(err) =
                ctx.ast_ctx
                    .traits
                    .insert(&ty_env_guard, DerefType::None, key, Arc::clone(trait_))
            {
                self.errors.push(err);
            }
        }
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::ExternalDecl(ExternalDecl::Struct(struct_), ..) = stmt {
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a struct.
            // External declarations should always be in the default block.
            let key = {
                let struct_ = struct_.as_ref().read().unwrap();
                let path = struct_
                    .module
                    .clone_push(&struct_.name, None, Some(struct_.file_pos));
                (path, BlockCtx::DEFAULT_BLOCK_ID)
            };

            if let Err(err) = ctx.ast_ctx.adts.insert(
                &ctx.ty_env.lock().unwrap(),
                DerefType::Shallow,
                key,
                Arc::clone(struct_),
            ) {
                self.errors.push(err);
            };
        }
    }
}
