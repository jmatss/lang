use std::cell::RefCell;

use common::{
    error::LangError,
    token::{
        expr::{Argument, Expr},
        op::{BinOperator, Op},
    },
    traverser::TraverseContext,
    visitor::Visitor,
};

use crate::AnalyzeContext;

/// Iterates through all method calls and inserts "this"/"self" into the calls
/// as the first argument. The bin ops representing the method call will be
/// transformed into a single FunctionCall expr where the lhs will have been
/// moved into the first parameter of the function call.
pub struct MethodAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

// TODO: Where should the name "this" be fetched from?
const THIS_VAR_NAME: &str = "this";

impl<'a> MethodAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for MethodAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// Modify the AST for any method calls (either on a object or static)
    /// represented as binary operations into a FuncCall expression.
    ///
    /// This function will also add "this" as the first argument of the method
    /// calls that are called on a instance. This can either be "this" by value
    /// or by pointer depending on the modified attached to the method.
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            let analyze_context = self.analyze_context.borrow();

            if let Some(method_call) = bin_op.rhs.eval_to_func_call() {
                match bin_op.operator {
                    // Instance structure access/method call.
                    BinOperator::Dot => {
                        let arg =
                            Argument::new(Some(THIS_VAR_NAME.into()), None, *bin_op.lhs.clone());

                        method_call.arguments.insert(0, arg);
                        method_call.is_method = true;

                        *expr = Expr::FuncCall(method_call.clone());
                    }

                    // Static structure access/method call. The lhs should be a
                    // hardcoded path/struct in this case, so can set the type
                    // of the `method_structure` directly.
                    BinOperator::DoubleColon => match bin_op.lhs.as_ref() {
                        Expr::Type(lhs_ty, ..) => {
                            method_call.is_method = true;
                            method_call.method_structure = Some(lhs_ty.clone());

                            *expr = Expr::FuncCall(method_call.clone());
                        }

                        _ => {
                            let err = analyze_context.err(format!(
                                "Lhs of DoubleColon expected to be a type, was: {:#?}",
                                bin_op.lhs
                            ));
                            self.errors.push(err);
                            return;
                        }
                    },

                    _ => (),
                }
            }
        }
    }
}
