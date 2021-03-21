use common::{
    error::{LangError, LangErrorKind, LangResult},
    traverser::TraverseContext,
    ty::{environment::TypeEnvironment, ty::Ty},
    visitor::Visitor,
    TypeId,
};

/// This checker should be ran after every generic related step is done.
/// At this point there should be no generic types in the AST, they should all
/// have been "solved" and replaced by their respective types.
/// This checker will iterate through all types in the AST and makes sure that
/// no generic types exists.
pub struct GenericTysSolvedChecker<'a> {
    ty_env: &'a mut TypeEnvironment,
    errors: Vec<LangError>,
}

impl<'a> GenericTysSolvedChecker<'a> {
    pub fn new(ty_env: &'a mut TypeEnvironment) -> Self {
        Self {
            ty_env,
            errors: Vec::default(),
        }
    }

    // TODO: Add better error reporting. For now the error is reported by the
    //       type that is incorrect. But might make more sense to let potential
    //       "wrapping" types report the error because that will give more
    //       information regarding the error.
    fn assert_generics_solved(&mut self, type_id: TypeId) -> LangResult<()> {
        let ty = self.ty_env.ty(type_id)?.clone();
        let ty_file_pos = self.ty_env.file_pos(type_id).cloned();
        match ty {
            Ty::Generic(ident, type_info) => {
                let err = LangError::new(
                    format!(
                        "Found unsolved generic type \"{}\". Type info: {:#?}",
                        ident, type_info
                    ),
                    LangErrorKind::AnalyzeError,
                    ty_file_pos,
                );
                self.errors.push(err);
            }
            Ty::GenericInstance(ident, _, type_info) => {
                let err = LangError::new(
                    format!(
                        "Found unsolved generic instance type \"{}\". Type info: {:#?}",
                        ident, type_info
                    ),
                    LangErrorKind::AnalyzeError,
                    ty_file_pos,
                );
                self.errors.push(err);
            }

            Ty::CompoundType(_, gen_tys, _) => {
                for gen_type_id in gen_tys.iter_types() {
                    self.assert_generics_solved(*gen_type_id)?;
                }
            }

            Ty::Array(ty_box, expr_opt, _) => {
                self.assert_generics_solved(ty_box)?;
                if let Some(expr) = expr_opt {
                    match expr.get_expr_type() {
                        Ok(expr_type_id) => self.assert_generics_solved(expr_type_id)?,
                        Err(err) => {
                            self.errors.push(err);
                        }
                    }
                }
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, _) => {
                if let Some(ret_type_id) = ret_ty_opt {
                    self.assert_generics_solved(ret_type_id)?;
                }
                for type_id_i in gen_tys.iter().chain(param_tys.iter()) {
                    self.assert_generics_solved(*type_id_i)?;
                }
            }

            Ty::Expr(expr, _) => match expr.get_expr_type() {
                Ok(expr_type_id) => self.assert_generics_solved(expr_type_id)?,
                Err(err) => self.errors.push(err),
            },

            Ty::Pointer(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                self.assert_generics_solved(type_id_i)?;
            }
            Ty::Any(_) => (),
        }

        Ok(())
    }
}

impl<'a> Visitor for GenericTysSolvedChecker<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, _ctx: &mut TraverseContext) {
        if let Err(err) = self.assert_generics_solved(*type_id) {
            self.errors.push(err);
        }
    }
}
