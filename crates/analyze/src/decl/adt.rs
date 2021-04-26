use std::{cell::RefCell, rc::Rc};

use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::LangError,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader},
    },
    traverse::visitor::Visitor,
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
    fn decl_new_adt(&mut self, ctx: &mut TraverseCtx, adt: &Rc<RefCell<Adt>>, id: BlockId) {
        let parent_id = match ctx.ast_ctx.get_parent_id(id) {
            Ok(parent_id) => parent_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let adt_full_path = match ctx.ast_ctx.get_module(id) {
            Ok(module_opt) => {
                let module = module_opt.unwrap_or_else(LangPath::empty);
                adt.borrow_mut().module = module.clone();
                module.clone_push(&adt.borrow().name, None)
            }
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // TODO: Add file positions to error message.
        if ctx.ast_ctx.get_adt(&ctx.ty_ctx, &adt_full_path).is_ok() {
            let err = ctx.ast_ctx.err(format!(
                "A ADT with name \"{}\" already defined.",
                ctx.ty_ctx.to_string_path(&adt_full_path)
            ));
            self.errors.push(err);
            return;
        }

        let key = (adt_full_path, parent_id);
        ctx.ast_ctx.adts.insert(key, Rc::clone(adt));
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

    fn visit_struct(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Struct(struct_), _, id, ..) = &mut ast_token {
            self.decl_new_adt(ctx, struct_, *id);
        }
    }

    fn visit_enum(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Enum(enum_), _, id, ..) = &mut ast_token {
            self.decl_new_adt(ctx, enum_, *id);
        }
    }

    fn visit_union(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Union(union), _, id, ..) = &mut ast_token {
            self.decl_new_adt(ctx, union, *id);
        }
    }

    // TODO: Merge logic with ADTs.
    fn visit_trait(&mut self, mut ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Trait(trait_), _, trait_id, ..) = &mut ast_token {
            // The trait will be added in the scope of its parent, so fetch
            // the block id for the parent.
            let parent_id = match ctx.ast_ctx.get_parent_id(*trait_id) {
                Ok(parent_id) => parent_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let trait_full_path = match ctx.ast_ctx.get_module(*trait_id) {
                Ok(module_opt) => {
                    let module = module_opt.unwrap_or_else(LangPath::empty);
                    trait_.borrow_mut().module = module.clone();
                    module.clone_push(&trait_.borrow().name, None)
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // TODO: Add file positions to error message.
            if ctx.ast_ctx.get_trait(&ctx.ty_ctx, &trait_full_path).is_ok() {
                let err = ctx.ast_ctx.err(format!(
                    "A trait with name \"{}\" already defined.",
                    &ctx.ty_ctx.to_string_path(&trait_full_path)
                ));
                self.errors.push(err);
            }

            let key = (trait_full_path, parent_id);
            ctx.ast_ctx.traits.insert(key, Rc::clone(trait_));
        }
    }
}
