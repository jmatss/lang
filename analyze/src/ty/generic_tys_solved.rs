use common::{
    error::{LangError, LangErrorKind},
    traverser::TraverseContext,
    ty::ty::Ty,
    visitor::Visitor,
};

/// This checker should be ran after every generic related step is done.
/// At this point there should be no generic types in the AST, they should all
/// have been "solved" and replaced by their respective types.
/// This checker will iterate through all types in the AST and makes sure that
/// no generic types exists.
pub struct GenericTysSolvedChecker {
    errors: Vec<LangError>,
}

impl GenericTysSolvedChecker {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    // TODO: Add better error reporting. For now the error is reported by the
    //       type that is incorrect. But might make more sense to let potential
    //       "wrapping" types report the error because that will give more
    //       information regarding the error.
    fn assert_generics_solved(&mut self, ty: &Ty) {
        match ty {
            Ty::Generic(ident, type_info) => {
                let err = LangError::new(
                    format!(
                        "Found unsolved generic type \"{}\". Type info: {:#?}",
                        ident, type_info
                    ),
                    LangErrorKind::AnalyzeError,
                    ty.file_pos().cloned(),
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
                    ty.file_pos().cloned(),
                );
                self.errors.push(err);
            }

            Ty::CompoundType(_, gen_tys, _) => {
                for gen_ty in gen_tys.iter_types() {
                    self.assert_generics_solved(gen_ty);
                }
            }

            Ty::Array(ty_box, expr_opt, _) => {
                self.assert_generics_solved(ty_box);
                if let Some(expr) = expr_opt {
                    match expr.get_expr_type() {
                        Ok(expr_ty) => self.assert_generics_solved(&expr_ty),
                        Err(err) => {
                            self.errors.push(err);
                        }
                    }
                }
            }

            Ty::Fn(gen_tys, param_tys, ret_ty_opt, _) => {
                if let Some(ret_ty) = ret_ty_opt {
                    self.assert_generics_solved(ret_ty);
                }
                for ty in gen_tys.iter().chain(param_tys.iter()) {
                    self.assert_generics_solved(ty);
                }
            }

            Ty::Expr(expr, _) => match expr.get_expr_type() {
                Ok(expr_ty) => self.assert_generics_solved(&expr_ty),
                Err(err) => self.errors.push(err),
            },

            Ty::Pointer(ty_box, ..)
            | Ty::UnknownAdtMember(ty_box, ..)
            | Ty::UnknownAdtMethod(ty_box, ..)
            | Ty::UnknownMethodArgument(ty_box, ..)
            | Ty::UnknownMethodGeneric(ty_box, ..)
            | Ty::UnknownArrayMember(ty_box, ..) => {
                self.assert_generics_solved(ty_box);
            }
            Ty::Any(_) => (),
        }
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

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        self.assert_generics_solved(ty);
    }
}
