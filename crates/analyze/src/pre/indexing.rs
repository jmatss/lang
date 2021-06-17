use std::sync::{Arc, RwLock};

use log::debug;

use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::{LangError, LangErrorKind},
    token::{
        expr::Expr,
        op::{BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    traverse::visitor::Visitor,
};

/// Rewrites "access" operations to make them "easier" to work with.
/// Binary ADT member indexing and union "is" matches will be written into
/// un ops.
pub struct IndexingAnalyzer {
    errors: Vec<LangError>,
}

impl IndexingAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    /// A struct/union access will have been rewritten to a un op instead of the
    /// original before this function is called (see the `visit_expr()`) function.
    /// This is true since the expressions are traversed in order.
    fn extract_member_name(&mut self, expr: &Expr) -> Option<String> {
        if let Expr::Op(Op::UnOp(un_op)) = expr {
            if let UnOperator::AdtAccess(member_name, ..) = &un_op.operator {
                return Some(member_name.clone());
            }
        }

        let err = LangError::new(
            format!("UnionIs on expr that wasn't a ADT access: {:#?}", expr),
            LangErrorKind::AnalyzeError,
            expr.file_pos().cloned(),
        );
        self.errors.push(err);
        None
    }
}

impl Visitor for IndexingAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &mut TraverseCtx) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            match bin_op.operator {
                // Struct or union access.
                BinOperator::Dot => {
                    let var = if let Some(var) = bin_op.rhs.eval_to_var() {
                        var
                    } else {
                        return;
                    };

                    debug!(
                        "Rewriting adt access -- lhs: {:#?}, rhs: {:#?}",
                        bin_op.lhs, &var
                    );

                    let adt_access = UnOperator::AdtAccess(var.name.clone(), None);
                    let un_op = UnOp::new(adt_access, bin_op.lhs.clone(), expr.file_pos().cloned());
                    *expr = Expr::Op(Op::UnOp(un_op));
                }

                // A "is" match. Currently only works for unions where the lhs
                // is a single variable that it binds to.
                BinOperator::Is => {
                    let var = if let Some(var) = bin_op.lhs.eval_to_var() {
                        var
                    } else {
                        panic!("lhs of \"is\" isn't variable: {:#?}", bin_op.lhs);
                    };

                    debug!(
                        "Rewriting is union match -- lhs: {:#?}, rhs: {:#?}",
                        &var, bin_op.rhs
                    );

                    let member_name =
                        if let Some(member_name) = self.extract_member_name(&bin_op.rhs) {
                            member_name
                        } else {
                            // The error is already reported in the
                            // `extract_member_name` function.
                            return;
                        };

                    let var_decl = Box::new(Stmt::VariableDecl(
                        Arc::new(RwLock::new(var.clone())),
                        var.file_pos,
                    ));

                    let union_is = UnOperator::UnionIs(member_name, var_decl);
                    let un_op = UnOp::new(union_is, bin_op.rhs.clone(), expr.file_pos().cloned());
                    *expr = Expr::Op(Op::UnOp(un_op));
                }

                _ => (),
            }
        }
    }
}
