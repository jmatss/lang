use std::cell::RefCell;

use common::{
    error::{LangError, LangResult},
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{AdtInit, FnCall},
    },
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
    BlockId,
};

use crate::context::AnalyzeContext;

/// Tries to solve partial paths (LangPath) in the code. This is done by trying
/// to prepend "use" statements with paths found in the code. If a match is found,
/// the partial path will be replaced with the full path.
pub struct PathResolver<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a> PathResolver<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn replace_inner_path(&self, inner_ty: &mut InnerTy, id: BlockId) -> LangResult<()> {
        match inner_ty {
            InnerTy::Struct(path) | InnerTy::Enum(path) | InnerTy::Union(path) => {
                let mut full_path = self
                    .analyze_context
                    .borrow()
                    .calculate_adt_full_path(path, id)?;
                if let (Some(to), Some(from)) = (full_path.file_pos_mut(), path.file_pos()) {
                    *to = *from;
                }
                *path = full_path;
            }

            InnerTy::Trait(path) => {
                let mut full_path = self
                    .analyze_context
                    .borrow()
                    .calculate_trait_full_path(path, id)?;
                if let (Some(to), Some(from)) = (full_path.file_pos_mut(), path.file_pos()) {
                    *to = *from;
                }
                *path = full_path;
            }

            _ => (),
        }

        Ok(())
    }

    fn resolve_ty_path(&mut self, ty: &mut Ty, id: BlockId) {
        // TODO: From what I know, only Compound types needs to be solved here
        //       since they are created for enum accesses during parsing.
        //       Is this a correct assumption?
        match ty {
            Ty::CompoundType(inner_ty, gens, ..) => {
                for gen_ty in gens.iter_types_mut() {
                    self.resolve_ty_path(gen_ty, id);
                }
                if let Err(err) = self.replace_inner_path(inner_ty, id) {
                    self.errors.push(err);
                    return;
                }
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, ..) => {
                for ty_i in gen_tys.iter_mut().chain(param_tys) {
                    self.resolve_ty_path(ty_i, id);
                }
                if let Some(ret_ty) = ret_ty_opt {
                    self.resolve_ty_path(ret_ty, id);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(expr_ty) = expr.get_expr_type_mut() {
                    self.resolve_ty_path(expr_ty, id);
                }
            }

            Ty::Array(box_ty, ..)
            | Ty::Pointer(box_ty, ..)
            | Ty::UnknownAdtMember(box_ty, ..)
            | Ty::UnknownAdtMethod(box_ty, ..)
            | Ty::UnknownMethodArgument(box_ty, ..)
            | Ty::UnknownMethodGeneric(box_ty, ..)
            | Ty::UnknownArrayMember(box_ty, ..) => self.resolve_ty_path(box_ty, id),

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }
    }
}

impl<'a> Visitor for PathResolver<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &TraverseContext) {
        if fn_call.is_fn_ptr_call || fn_call.is_method {
            // Method calls already solved during the "MethodAnalyzer" stage.
            return;
        }

        let half_path = fn_call.module.clone_push(&fn_call.name, None);

        match self
            .analyze_context
            .borrow()
            .calculate_fn_full_path(&half_path, ctx.block_id)
        {
            Ok(mut full_path) => {
                full_path.pop();
                fn_call.module = full_path;
            }
            Err(err) => {
                self.errors.push(err);
            }
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &TraverseContext) {
        let half_path = adt_init.module.clone_push(&adt_init.name, None);

        match self
            .analyze_context
            .borrow()
            .calculate_adt_full_path(&half_path, ctx.block_id)
        {
            Ok(mut full_path) => {
                full_path.pop();
                adt_init.module = full_path;
            }
            Err(err) => {
                self.errors.push(err);
            }
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(path, _), ..) = ast_token {
            let analyze_context = self.analyze_context.borrow();

            if let Ok(full_path) = analyze_context.calculate_adt_full_path(path, ctx.block_id) {
                *path = full_path
            } else if let Ok(full_path) =
                analyze_context.calculate_trait_full_path(path, ctx.block_id)
            {
                *path = full_path
            } else {
                let mut err = analyze_context.err_adt(
                    format!(
                        "Unable to find full path for type defined in impl block: {}",
                        path
                    ),
                    path,
                );
                err = analyze_context.err_trait(err.msg, path);
                self.errors.push(err);
            }
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, ctx: &TraverseContext) {
        self.resolve_ty_path(ty, ctx.block_id);
    }
}
