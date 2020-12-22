use common::{
    error::LangError,
    token::{
        expr::Expr,
        op::{BinOperator, Op, UnOp, UnOperator},
    },
    traverser::TraverseContext,
    visitor::Visitor,
};
use log::debug;

/// Gathers information about indexing of variables. This includes array indexing,
/// struct indexing and method calls etc. This analyzer traverses through all
/// expressions and tried to deduce the correct indexing. "Markers" will be
/// inserted into the AST.
pub struct IndexingAnalyzer {}

impl IndexingAnalyzer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Visitor for IndexingAnalyzer {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }

    /// Wraps struct accesses into a new un op that replaces the old binary
    /// Dot operation. The index and the type of the member will be parsed in
    /// a later stage of the analyzing (after "type analyzing" have been ran).
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let BinOperator::Dot = bin_op.operator {
                if let Some(var) = bin_op.rhs.eval_to_var() {
                    debug!(
                        "Rewriting struct access -- lhs var: {:?}, rhs var: {:?}",
                        bin_op.lhs, &var
                    );

                    let struct_access = UnOperator::StructAccess(var.name.clone(), None, None);
                    let un_op =
                        UnOp::new(struct_access, bin_op.lhs.clone(), expr.file_pos().cloned());
                    *expr = Expr::Op(Op::UnOp(un_op));

                    debug!("expr after rewrite: {:?}", expr);
                }
            }
        }
    }
}
