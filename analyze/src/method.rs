use common::{
    error::LangError,
    token::ast::Token,
    token::op::UnOperator,
    token::{
        ast::AstToken,
        expr::{Argument, ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, BinOperator, Op, UnOp},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    visitor::Visitor,
};

/// Iterates through all method calls and inserts "this"/"self" into the calls
/// as the first argument. The Dot bin op representing the method call will
/// be transformed into a single FunctionCall expr where the lhs will have been
/// moved into the first parameter of the function call.
pub struct MethodAnalyzer {}

// TODO: Where should the name "this" be fetched from?
const THIS_VAR_NAME: &str = "this";

impl MethodAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

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
            Token::EOF => (),
        }
    }
}

impl Visitor for MethodAnalyzer {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        self.analyze(ast_token);
    }

    // TODO: Should there be a option to either call a method with reference or
    //       value? Currently always uses reference to "this"/"self".
    /// If the expression is a binary Dot operation with a function in the rhs,
    /// this is a method call. Modify the AST by moving the lhs expr of the Dot
    /// into the first argument of the method call and then replace the whole
    /// expr Dot with a FuncCall token.
    fn visit_expr(&mut self, expr: &mut Expr, ctx: &TraverseContext) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let BinOperator::Dot = bin_op.operator {
                if let Some(method_call) = bin_op.rhs.eval_to_func_call() {
                    // Get reference to "this"/"self".
                    let this_ref = Expr::Op(Op::UnOp(UnOp::new(
                        UnOperator::Address,
                        Box::new(*bin_op.lhs.clone()),
                    )));

                    let arg = Argument::new(Some(THIS_VAR_NAME.into()), this_ref);
                    method_call.arguments.insert(0, arg);
                    method_call.is_method = true;

                    *expr = Expr::FuncCall(method_call.clone());
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_func(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_enum(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_interface(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_anon(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_if(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_if_case(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_match(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_match_case(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_for(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_while(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_test(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_yield(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_break(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_continue(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_use(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_package(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_defer(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_defer_exec(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_modifier(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_lit(&mut self, expr: &mut Expr, ctx: &TraverseContext) {}

    fn visit_var(&mut self, var: &mut Var, ctx: &TraverseContext) {}

    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {}

    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {}

    fn visit_array_init(&mut self, expr: &mut ArrayInit, ctx: &TraverseContext) {}

    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &TraverseContext) {}

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {}
}
