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

/// Gathers information about indexing on variables. This includes struct member
/// indexing and enum member indexing. Those kind of expression will be rewritten
/// in the AST so that it is easier to work with them.
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
    fn visit_expr(&mut self, expr: &mut Expr, ctx: &TraverseContext) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let Some(var) = bin_op.rhs.eval_to_var() {
                match bin_op.operator {
                    // Struct access.
                    BinOperator::Dot => {
                        debug!(
                            "Rewriting struct access -- lhs var: {:?}, rhs var: {:?}",
                            bin_op.lhs, &var
                        );

                        let struct_access = UnOperator::StructAccess(var.name.clone(), None);
                        let un_op =
                            UnOp::new(struct_access, bin_op.lhs.clone(), expr.file_pos().cloned());
                        *expr = Expr::Op(Op::UnOp(un_op));

                        debug!("expr after rewrite: {:?}", expr);
                    }

                    // TODO: Will this double colon access always be for enums only?
                    //       Is there a possibility that this might be used for
                    //       static/const as well access in the future?
                    // Enum access.
                    BinOperator::DoubleColon => {
                        debug!(
                            "Rewriting enum access -- lhs var: {:?}, rhs var: {:?}",
                            bin_op.lhs, &var
                        );

                        let enum_access = UnOperator::EnumAccess(var.name.clone(), ctx.block_id);
                        let un_op =
                            UnOp::new(enum_access, bin_op.lhs.clone(), expr.file_pos().cloned());
                        *expr = Expr::Op(Op::UnOp(un_op));

                        debug!("expr after rewrite: {:?}", expr);
                    }

                    _ => (),
                }
            }
        }
    }
}
