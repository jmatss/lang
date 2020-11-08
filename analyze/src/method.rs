use std::cell::RefCell;

use common::{
    error::LangError,
    token::ast::Token,
    token::op::UnOperator,
    token::{
        ast::AstToken,
        expr::{Argument, Expr},
        op::{BinOperator, Op, UnOp},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    types::Type,
    visitor::Visitor,
};

use crate::AnalyzeContext;

/// Iterates through all method calls and inserts "this"/"self" into the calls
/// as the first argument. The Dot bin op representing the method call will
/// be transformed into a single FunctionCall expr where the lhs will have been
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

    // TODO: This whole "this" logic should be changed.
    /// Since `this` is sent as a reference to a method, it will always need to
    /// be dereferenced before use. So all "this" in the source code needs to be
    /// rewritten as "this.*".
    /// This is done in the default block since doing it in the "visit_expr"
    /// block causes infinite recursion.
    fn analyze_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Var(var) => {
                if var.name == THIS_VAR_NAME {
                    let new_expr = Expr::Op(Op::UnOp(UnOp::new(
                        UnOperator::Deref,
                        Box::new(expr.clone()),
                    )));
                    *expr = new_expr;
                }
            }
            Expr::FuncCall(func_call) => {
                for arg in func_call.arguments.iter_mut() {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expr::StructInit(struct_init) => {
                for arg in struct_init.arguments.iter_mut() {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expr::ArrayInit(array_init) => {
                for arg in array_init.arguments.iter_mut() {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expr::Op(Op::BinOp(bin_op)) => {
                self.analyze_expr(&mut bin_op.lhs);
                self.analyze_expr(&mut bin_op.rhs);
            }
            Expr::Op(Op::UnOp(un_op)) => {
                self.analyze_expr(&mut un_op.value);
            }
            _ => (),
        }
    }

    fn analyze(&mut self, ast_token: &mut AstToken) {
        match &mut ast_token.token {
            Token::Expr(expr) => self.analyze_expr(expr),
            Token::Stmt(stmt) => match stmt {
                Stmt::Return(expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.analyze_expr(expr);
                    }
                }
                Stmt::Yield(expr) => self.analyze_expr(expr),
                Stmt::Defer(expr) => self.analyze_expr(expr),
                Stmt::DeferExec(expr) => self.analyze_expr(expr),
                Stmt::Assignment(_, lhs, rhs) => {
                    self.analyze_expr(lhs);
                    self.analyze_expr(rhs);
                }
                Stmt::VariableDecl(_, expr_opt) => {
                    if let Some(expr) = expr_opt {
                        self.analyze_expr(expr);
                    }
                }
                _ => (),
            },
            Token::Block(_, _, body) => {
                for body_token in body {
                    self.analyze(body_token);
                }
            }
            Token::Empty | Token::EOF => (),
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

    fn visit_default_block(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.analyze(ast_token);
    }

    // TODO: Should there be a option to either call a method with reference or
    //       value? Currently always uses reference to "this"/"self".
    /// If the expression is a binary Dot operation with a function in the rhs,
    /// this is a method call. Modify the AST by moving the lhs expr of the Dot
    /// into the first argument of the method call and then replace the whole
    /// expr Dot with a FuncCall token.
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            let analyze_context = self.analyze_context.borrow();

            if let Some(method_call) = bin_op.rhs.eval_to_func_call() {
                match bin_op.operator {
                    // Struct access/method call.
                    BinOperator::Dot => {
                        // TODO: Don't always send "this" as a reference.
                        let this_ref = Expr::Op(Op::UnOp(UnOp::new(
                            UnOperator::Address,
                            Box::new(*bin_op.lhs.clone()),
                        )));

                        let arg = Argument::new(Some(THIS_VAR_NAME.into()), this_ref);
                        method_call.arguments.insert(0, arg);
                        method_call.is_method = true;

                        *expr = Expr::FuncCall(method_call.clone());
                    }

                    // Static struct access/method call.
                    BinOperator::DoubleColon => match bin_op.lhs.as_ref() {
                        Expr::Type(Type::CompoundType(ident, _)) => {
                            method_call.is_method = true;
                            method_call.method_struct = Some(ident.clone());

                            *expr = Expr::FuncCall(method_call.clone());
                        }
                        _ => {
                            let err = analyze_context.err(format!(
                                "Lhs of DoubleColon not a Struct type, was: {:?}",
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
