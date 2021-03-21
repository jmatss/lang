use std::cell::RefCell;

use common::{
    error::{LangError, LangResult},
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{AdtInit, FnCall},
    },
    traverser::TraverseContext,
    ty::ty::Ty,
    visitor::Visitor,
    BlockId, TypeId,
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

    fn replace_inner_path(&mut self, type_id: TypeId, block_id: BlockId) -> LangResult<()> {
        let old_path = match self.analyze_context.borrow().ty_env.get_ident(type_id) {
            Ok(Some(path)) => path,
            _ => return Ok(()),
        };

        let inner_ty = match self.analyze_context.borrow().ty_env.get_inner(type_id) {
            Ok(inner_ty) => inner_ty.clone(),
            _ => return Ok(()),
        };

        if inner_ty.is_adt() {
            let path = inner_ty.get_ident().unwrap();
            let full_path = self
                .analyze_context
                .borrow()
                .calculate_adt_full_path(&path, block_id)?;

            let mut analyze_context = self.analyze_context.borrow_mut();
            let inner_ty_mut = analyze_context.ty_env.get_inner_mut(type_id)?;

            *inner_ty_mut.get_ident_mut().unwrap() = full_path;
        } else if inner_ty.is_trait() {
            let path = inner_ty.get_ident().unwrap();
            let full_path = self
                .analyze_context
                .borrow()
                .calculate_trait_full_path(&path, block_id)?;

            let mut analyze_context = self.analyze_context.borrow_mut();
            let inner_ty_mut = analyze_context.ty_env.get_inner_mut(type_id)?;

            *inner_ty_mut.get_ident_mut().unwrap() = full_path;
        }

        Ok(())
    }

    fn resolve_ty_path(&mut self, type_id: TypeId, block_id: BlockId) -> LangResult<()> {
        // TODO: From what I know, only Compound types needs to be solved here
        //       since they are created for enum accesses during parsing.
        //       Is this a correct assumption?
        let ty = self.analyze_context.borrow().ty_env.ty(type_id)?.clone();
        match ty {
            Ty::CompoundType(inner_ty, gens, ..) => {
                for gen_type_id in gens.iter_types() {
                    self.resolve_ty_path(*gen_type_id, block_id)?;
                }
                self.replace_inner_path(type_id, block_id)?;
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, ..) => {
                for ty_i in gen_tys.iter().chain(param_tys.iter()) {
                    self.resolve_ty_path(*ty_i, block_id)?;
                }
                if let Some(ret_type_id) = ret_ty_opt {
                    self.resolve_ty_path(ret_type_id, block_id)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.resolve_ty_path(expr_type_id, block_id)?;
                }
            }

            Ty::Array(inner_type_id, ..)
            | Ty::Pointer(inner_type_id, ..)
            | Ty::UnknownAdtMember(inner_type_id, ..)
            | Ty::UnknownAdtMethod(inner_type_id, ..)
            | Ty::UnknownMethodArgument(inner_type_id, ..)
            | Ty::UnknownMethodGeneric(inner_type_id, ..)
            | Ty::UnknownArrayMember(inner_type_id, ..) => {
                self.resolve_ty_path(inner_type_id, block_id)?;
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(())
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

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseContext) {
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

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseContext) {
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

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseContext) {
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

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseContext) {
        if let Err(err) = self.resolve_ty_path(*type_id, ctx.block_id) {
            self.errors.push(err);
        }
    }
}
