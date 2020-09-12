use crate::AnalyzeContext;
use common::{
    error::{LangError, LangErrorKind::AnalyzeError},
    token::{
        ast::AstToken,
        expr::{Argument, ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, BinOperator, Op, UnOp},
        stmt::Stmt,
    },
    types::Type,
    visitor::Visitor,
};

/// Iterates through all method calls and inserts "this"/"self" into the calls
/// as the first argument. The Dot bin op representing the method call will
/// be transformed into a single FunctionCall expr where the lhs will have been
/// moved into the first parameter of the function call.
pub struct MethodAnalyzer<'a> {
    analyze_context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> MethodAnalyzer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for MethodAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(_, id, _) = ast_token {
            self.analyze_context.cur_block_id = *id;
        }
    }

    /// If the expression is a binary Dot operation with a function in the rhs,
    /// this is a method call. Modify the AST by moving the lhs expr of the Dot
    /// into the first argument of the method call and then replace the whole
    /// expr Dot with a FuncCall token.
    fn visit_expr(&mut self, expr: &mut Expr) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let BinOperator::Dot = bin_op.operator {
                if let Some(method_call) = bin_op.rhs.eval_to_func_call() {
                    // Get the name of the type of the lhs expression. This should
                    // be a struct since this is a method call.
                    let struct_name = match bin_op.lhs.get_expr_type() {
                        Ok(Type::Custom(struct_name)) => struct_name.clone(),
                        Ok(ty) => {
                            let err = LangError::new(
                                format!("Lhs of Dot method call wasn't struct, type: {:?}", ty),
                                AnalyzeError {
                                    line_nr: 0,
                                    column_nr: 0,
                                },
                            );
                            self.errors.push(err);
                            return;
                        }
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    // TODO: Where should the name "this" be fetched from?
                    const THIS_VAR_NAME: &str = "this";

                    let arg = Argument::new(Some(THIS_VAR_NAME.into()), *bin_op.lhs.clone());
                    method_call.arguments.insert(0, arg);
                    method_call.method_struct = Some(struct_name);

                    *expr = Expr::FuncCall(method_call.clone());
                }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken) {}

    fn visit_default_block(&mut self, ast_token: &mut AstToken) {}

    fn visit_func(&mut self, ast_token: &mut AstToken) {}

    fn visit_struct(&mut self, ast_token: &mut AstToken) {}

    fn visit_enum(&mut self, ast_token: &mut AstToken) {}

    fn visit_interface(&mut self, ast_token: &mut AstToken) {}

    fn visit_impl(&mut self, ast_token: &mut AstToken) {}

    fn visit_anon(&mut self, ast_token: &mut AstToken) {}

    fn visit_if(&mut self, ast_token: &mut AstToken) {}

    fn visit_if_case(&mut self, ast_token: &mut AstToken) {}

    fn visit_match(&mut self, ast_token: &mut AstToken) {}

    fn visit_match_case(&mut self, ast_token: &mut AstToken) {}

    fn visit_for(&mut self, ast_token: &mut AstToken) {}

    fn visit_while(&mut self, ast_token: &mut AstToken) {}

    fn visit_test(&mut self, ast_token: &mut AstToken) {}

    fn visit_return(&mut self, stmt: &mut Stmt) {}

    fn visit_yield(&mut self, stmt: &mut Stmt) {}

    fn visit_break(&mut self, stmt: &mut Stmt) {}

    fn visit_continue(&mut self, stmt: &mut Stmt) {}

    fn visit_use(&mut self, stmt: &mut Stmt) {}

    fn visit_package(&mut self, stmt: &mut Stmt) {}

    fn visit_defer(&mut self, stmt: &mut Stmt) {}

    fn visit_defer_exec(&mut self, stmt: &mut Stmt) {}

    fn visit_assignment(&mut self, stmt: &mut Stmt) {}

    fn visit_var_decl(&mut self, stmt: &mut Stmt) {}

    fn visit_extern_decl(&mut self, stmt: &mut Stmt) {}

    fn visit_modifier(&mut self, stmt: &mut Stmt) {}

    fn visit_lit(&mut self, expr: &mut Expr) {}

    fn visit_var(&mut self, var: &mut Var) {}

    fn visit_func_call(&mut self, func_call: &mut FuncCall) {}

    fn visit_struct_init(&mut self, struct_init: &mut StructInit) {}

    fn visit_array_init(&mut self, expr: &mut ArrayInit) {}

    fn visit_bin_op(&mut self, bin_op: &mut BinOp) {}

    fn visit_un_op(&mut self, un_op: &mut UnOp) {}
}
