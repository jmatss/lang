use common::{
    error::LangError,
    token::stmt::Stmt,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{inner_ty::InnerTy, ty::Ty},
};

// TODO: A new `void` type might be created similar to the rust `()` type that
//       can be allowed in more places. For now prohibit variable to be assigned
//       the `void` type.
/// Finds uses of the `void` type in invalid places.
///
/// Functions without return type returns `void`, so need to make sure that any
/// function calls to those functions aren't used in places where `void` isn't
/// valid.
///
/// Since most of the incorrect uses of `void` types will be found during the
/// regular type inference, this analyzer only checks some edge cases which
/// isn't caught during the type inference.
///
/// List of prohibited places that are checked:
///  * Variable assignment  (includes parameters, no void arguments allowed)
///  * Variable initialization
pub struct VoidAnalyzer {
    errors: Vec<LangError>,
}

impl VoidAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for VoidAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let var = var.read().unwrap();
            if let Some(type_id) = var.ty {
                let ty_env_guard = ctx.ty_env.lock().unwrap();
                let ty = match ty_env_guard.ty(type_id) {
                    Ok(ty) => ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Ty::CompoundType(InnerTy::Void, ..) = ty {
                    self.errors.push(ctx.ast_ctx.err(format!(
                        "Tried to initialize variable named \"{}\" with value of type `void`.",
                        var.name,
                    )))
                }
            }
        }
    }

    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        if let Stmt::Assignment(_, _, rhs_expr, ..) = stmt {
            let type_id = match rhs_expr.get_expr_type() {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let ty_env_guard = ctx.ty_env.lock().unwrap();
            let ty = match ty_env_guard.ty(type_id) {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            if let Ty::CompoundType(InnerTy::Void, ..) = ty {
                self.errors.push(
                    ctx.ast_ctx
                        .err("Tried to assign value with type `void`.".into()),
                )
            }
        }
    }
}
