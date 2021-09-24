use std::sync::Arc;

use either::Either;
use log::debug;
use parking_lot::RwLock;

use common::{
    ctx::ast_ctx::AstCtx,
    error::{LangError, LangResult},
    token::{
        expr::Expr,
        lit::Lit,
        op::{BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
};

/// Rewrites "access" operations to make them "easier" to work with.
/// Binary ADT member indexing and union "is" matches will be written into
/// un ops.
///
/// Also reports errors if the rhs of a `Dot` operation is a numeric literal.
/// This is not supported as is very likely a misswrite of an array or tuple
/// access operation.
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
    fn extract_member(
        &mut self,
        ast_ctx: &AstCtx,
        expr: &Expr,
    ) -> LangResult<Either<String, usize>> {
        if let Expr::Op(Op::UnOp(un_op)) = expr {
            if let UnOperator::AdtAccess(member, ..) = &un_op.operator {
                return Ok(member.clone());
            }
        }
        Err(ast_ctx.err(format!(
            "UnionIs on expr that wasn't a ADT access: {:#?}",
            expr
        )))
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

    fn visit_expr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            match bin_op.operator {
                // Struct or union access.
                // If this is a dot operator with a int or float literal to the
                // right, something has probably gone wrong, return an error.
                // The most likely scenario is that someone have forgotten to
                // wrap the literal in either square brackets (array access) or
                // parenthesis (tuple access).
                BinOperator::Dot => {
                    let member = if let Some(var) = bin_op.rhs.eval_to_var() {
                        Either::Left(var.name.clone())
                    } else if matches!(
                        *bin_op.rhs,
                        Expr::Lit(Lit::Integer(..) | Lit::Float(..), ..)
                    ) {
                        self.errors.push(
                            ctx.ast_ctx.err(
                                "Found numeric literal to the right of a `Dot` operation. \
                                Have you forgotten to add square brackets or parenthesis around \
                                the numeric literal (array access or tuple access respectively)?"
                                    .into(),
                            ),
                        );
                        return;
                    } else {
                        return;
                    };

                    debug!(
                        "Rewriting adt access -- lhs: {:#?}, rhs: {:#?}",
                        &bin_op.lhs, &bin_op.rhs
                    );

                    let is_const = bin_op.lhs.is_const();
                    let un_op = UnOp::new(
                        UnOperator::AdtAccess(member),
                        bin_op.lhs.clone(),
                        is_const,
                        expr.file_pos().cloned(),
                    );
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

                    let member = match self.extract_member(ctx.ast_ctx, &bin_op.rhs) {
                        Ok(member) => member,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    let var_decl = Box::new(Stmt::VariableDecl(
                        Arc::new(RwLock::new(var.clone())),
                        var.file_pos,
                    ));

                    let is_const = bin_op.rhs.is_const();
                    let un_op = UnOp::new(
                        UnOperator::UnionIs(member, var_decl),
                        bin_op.rhs.clone(),
                        is_const,
                        expr.file_pos().cloned(),
                    );
                    *expr = Expr::Op(Op::UnOp(un_op));
                }

                _ => (),
            }
        }
    }
}
