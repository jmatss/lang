use common::{
    error::LangError,
    token::{
        expr::Expr,
        lit::Lit,
        op::{Op, UnOperator},
    },
};

use crate::{traverse_ctx::TraverseCtx, visitor::Visitor};

/// Rewrites any unary operators for integer & float literals where the `UnOperator`
/// is `UnOperator::Negative`. The unary operation will be removed and a minus sign
/// will be set inside the literal string instead. This is needed to allow for
/// creating the min value of any signed types.
///
/// For example, given the statement:
///   `var x: i8 = -128`
/// The RHS would be parsed as a unary operation with the operator being Negative
/// which would look something like this:
///   `UnOp(Negative, 128)`
/// During code generation the 128 would be compiled first and then it would be
/// negated. The problem is that 128 > i8::MAX so this doesn't work.
///
/// Therefore we rewrite any cases of this so that the literal contains the minus
/// sign and the code generator knows that we want a -128 which is a valid number.
pub struct SignedLiteralsAnalyzer {}

impl SignedLiteralsAnalyzer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Visitor for SignedLiteralsAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        None
    }

    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &mut TraverseCtx) {
        if let Expr::Op(Op::UnOp(un_op)) = expr {
            if let UnOperator::Negative = un_op.operator {
                match un_op.value.as_mut() {
                    Expr::Lit(Lit::Float(lit_str), ..)
                    | Expr::Lit(Lit::Integer(lit_str, ..), ..) => {
                        *lit_str = format!("-{}", lit_str);
                        *expr = (*un_op.value).clone();
                    }
                    _ => (),
                }
            }
        }
    }
}
