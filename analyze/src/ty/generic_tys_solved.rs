use std::collections::HashSet;

use common::{
    ctx::{traverse_ctx::TraverseCtx, ty_ctx::TyCtx},
    error::{LangError, LangErrorKind, LangResult},
    traverse::visitor::Visitor,
    ty::ty::Ty,
    TypeId,
};

/// This checker should be ran after every generic related step is done.
/// At this point there should be no generic types in the AST, they should all
/// have been "solved" and replaced by their respective types.
/// This checker will iterate through all types in the AST and makes sure that
/// no generic types exists.
pub struct GenericTysSolvedChecker {
    seen_type_ids: HashSet<TypeId>,
    errors: Vec<LangError>,
}

impl GenericTysSolvedChecker {
    pub fn new() -> Self {
        Self {
            seen_type_ids: HashSet::default(),
            errors: Vec::default(),
        }
    }

    // TODO: Add better error reporting. For now the error is reported by the
    //       type that is incorrect. But might make more sense to let potential
    //       "wrapping" types report the error because that will give more
    //       information regarding the error.
    fn assert_generics_solved(&mut self, ty_ctx: &TyCtx, type_id: TypeId) -> LangResult<()> {
        let inf_type_id = ty_ctx.inferred_type(type_id)?;

        if self.seen_type_ids.contains(&inf_type_id) {
            return Ok(());
        } else {
            self.seen_type_ids.insert(inf_type_id);
        }

        let ty_file_pos = ty_ctx.ty_env.file_pos(inf_type_id).cloned();
        match ty_ctx.ty_env.ty(inf_type_id)?.clone() {
            ty @ Ty::Generic(..) => {
                let err = LangError::new(
                    format!(
                        "Found unsolved generic type. type_id: {}, inf_type_id: {}, ty: {:#?}",
                        type_id, inf_type_id, ty
                    ),
                    LangErrorKind::AnalyzeError,
                    ty_file_pos,
                );
                self.errors.push(err);
            }
            ty @ Ty::GenericInstance(..) => {
                let err = LangError::new(
                    format!(
                        "Found unsolved generic instance type. type_id: {}, inf_type_id: {}, ty: {:#?}",
                        type_id, inf_type_id, ty
                    ),
                    LangErrorKind::AnalyzeError,
                    ty_file_pos,
                );
                if !self.errors.contains(&err) {
                    self.errors.push(err);
                }
            }

            Ty::CompoundType(_, gen_tys, _) => {
                for gen_type_id in gen_tys.iter_types() {
                    self.assert_generics_solved(ty_ctx, *gen_type_id)?;
                }
            }

            Ty::Array(ty_box, expr_opt, _) => {
                self.assert_generics_solved(ty_ctx, ty_box)?;
                if let Some(expr) = expr_opt {
                    match expr.get_expr_type() {
                        Ok(expr_type_id) => self.assert_generics_solved(ty_ctx, expr_type_id)?,
                        Err(err) => {
                            self.errors.push(err);
                        }
                    }
                }
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, _) => {
                if let Some(ret_type_id) = ret_ty_opt {
                    self.assert_generics_solved(ty_ctx, ret_type_id)?;
                }
                for type_id_i in gen_tys.iter().chain(param_tys.iter()) {
                    self.assert_generics_solved(ty_ctx, *type_id_i)?;
                }
            }

            Ty::Expr(expr, _) => match expr.get_expr_type() {
                Ok(expr_type_id) => self.assert_generics_solved(ty_ctx, expr_type_id)?,
                Err(err) => self.errors.push(err),
            },

            Ty::Pointer(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.assert_generics_solved(ty_ctx, type_id_i)?;
            }
            Ty::Any(..) => (),
        }

        Ok(())
    }
}

impl Visitor for GenericTysSolvedChecker {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.assert_generics_solved(&ctx.ty_ctx, *type_id) {
            self.errors.push(err);
        }
    }
}
