use std::collections::HashSet;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{get::get_file_pos, ty::Ty, ty_env::TyEnv, type_id::TypeId},
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
    fn assert_generics_solved(
        &mut self,
        ty_env: &TyEnv,
        type_id: TypeId,
        traverse_file_pos: &FilePosition,
    ) -> LangResult<()> {
        let inf_type_id = ty_env.inferred_type(type_id)?;

        if self.seen_type_ids.contains(&inf_type_id) {
            return Ok(());
        } else {
            self.seen_type_ids.insert(inf_type_id);
        }

        let file_pos = if let Some(file_pos) = get_file_pos(ty_env, inf_type_id) {
            file_pos
        } else {
            traverse_file_pos
        };

        let ty = ty_env.ty(inf_type_id)?.clone();
        match ty {
            Ty::Generic(..) => {
                return Err(LangError::new(
                    format!(
                        "Found unsolved generic type. type_id: {}, inf_type_id: {}, ty: {:#?}",
                        type_id, inf_type_id, ty
                    ),
                    LangErrorKind::AnalyzeError,
                    Some(*file_pos),
                ));
            }
            Ty::GenericInstance(..) => {
                return Err(LangError::new(
                    format!(
                        "Found unsolved generic instance type. type_id: {}, inf_type_id: {}, ty: {:#?}",
                        type_id, inf_type_id, ty,
                    ),
                    LangErrorKind::AnalyzeError,
                    Some(*file_pos),
                ));
            }

            Ty::CompoundType(inner_ty, ..) => {
                if let Some(gens) = inner_ty.gens() {
                    for gen_type_id in gens.iter_types() {
                        self.assert_generics_solved(ty_env, *gen_type_id, traverse_file_pos)?;
                    }
                }
            }

            Ty::Array(type_id_i, expr_opt, _) => {
                self.assert_generics_solved(ty_env, type_id_i, traverse_file_pos)?;
                if let Some(expr) = expr_opt {
                    let expr_type_id = expr.get_expr_type()?;
                    self.assert_generics_solved(ty_env, expr_type_id, traverse_file_pos)?
                }
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, _) => {
                if let Some(ret_type_id) = ret_ty_opt {
                    self.assert_generics_solved(ty_env, ret_type_id, traverse_file_pos)?;
                }
                for type_id_i in gen_tys.iter().chain(param_tys.iter()) {
                    self.assert_generics_solved(ty_env, *type_id_i, traverse_file_pos)?;
                }
            }

            Ty::Expr(expr, _) => match expr.get_expr_type() {
                Ok(expr_type_id) => {
                    self.assert_generics_solved(ty_env, expr_type_id, traverse_file_pos)?
                }
                Err(err) => self.errors.push(err),
            },

            Ty::Pointer(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownFnArgument(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.assert_generics_solved(ty_env, type_id_i, traverse_file_pos)?;
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

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.assert_generics_solved(&ctx.ty_env.lock(), *type_id, &ctx.file_pos())
        {
            if !self.errors.contains(&err) {
                self.errors.push(err);
            }
        }
    }
}
