use std::collections::HashSet;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::ast::AstToken,
    ty::{
        get::get_file_pos, substitution_sets::sub_sets_debug_print, ty::Ty, ty_env::TyEnv,
        type_id::TypeId,
    },
};

use crate::{traverse_ctx::TraverseCtx, visitor::Visitor};

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
    fn assert_generics_solved(&mut self, ty_env: &TyEnv, type_id: TypeId) -> LangResult<()> {
        let inf_type_id = ty_env.inferred_type(type_id)?;

        if self.seen_type_ids.contains(&inf_type_id) {
            return Ok(());
        } else {
            self.seen_type_ids.insert(inf_type_id);
        }

        let ty_file_pos = get_file_pos(&ty_env, inf_type_id).cloned();
        let ty = ty_env.ty(inf_type_id)?.clone();
        match ty {
            Ty::Generic(..) => {
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
            Ty::GenericInstance(..) => {
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
                    self.assert_generics_solved(ty_env, *gen_type_id)?;
                }
            }

            Ty::Array(ty_box, expr_opt, _) => {
                self.assert_generics_solved(ty_env, ty_box)?;
                if let Some(expr) = expr_opt {
                    match expr.get_expr_type() {
                        Ok(expr_type_id) => self.assert_generics_solved(ty_env, expr_type_id)?,
                        Err(err) => {
                            self.errors.push(err);
                        }
                    }
                }
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, _) => {
                if let Some(ret_type_id) = ret_ty_opt {
                    self.assert_generics_solved(ty_env, ret_type_id)?;
                }
                for type_id_i in gen_tys.iter().chain(param_tys.iter()) {
                    self.assert_generics_solved(ty_env, *type_id_i)?;
                }
            }

            Ty::Expr(expr, _) => match expr.get_expr_type() {
                Ok(expr_type_id) => self.assert_generics_solved(ty_env, expr_type_id)?,
                Err(err) => self.errors.push(err),
            },

            Ty::Pointer(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.assert_generics_solved(ty_env, type_id_i)?;
            }
            Ty::Any(..) => (),
        }

        Ok(())
    }
}

impl Visitor for GenericTysSolvedChecker {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        sub_sets_debug_print(&ctx.ty_env);
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.assert_generics_solved(&ctx.ty_env.lock().unwrap(), *type_id) {
            self.errors.push(err);
        }
    }
}
