use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::ast::Token,
    token::{
        ast::AstToken,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    visitor::Visitor,
};
use log::debug;

/// Gathers information about indexing of variables. This includes array indexing,
/// struct indexing and method calls etc. This analyzer traverses through all
/// expressions and tried to deduce the correct indexing. "Markers" will be
/// inserted into the AST.
pub struct IndexingAnalyzer<'a> {
    analyze_context: &'a mut AnalyzeContext,
}

impl<'a> IndexingAnalyzer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self { analyze_context }
    }
}

impl<'a> Visitor for IndexingAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }

    fn visit_token(&mut self, ast_token: &mut AstToken) {
        self.analyze_context.cur_line_nr = ast_token.line_nr;
        self.analyze_context.cur_column_nr = ast_token.column_nr;
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {
        if let Token::Block(_, id, _) = &ast_token.token {
            self.analyze_context.cur_block_id = *id;
        }
    }

    /// Wraps struct accesses into a new un op that replaces the old binary
    /// Dot operation. The index and the type of the member will be parsed in
    /// a later stage of the analyzing (after "type analyzing" have been ran).
    fn visit_expr(&mut self, expr: &mut Expr) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let BinOperator::Dot = bin_op.operator {
                if let Some(var) = bin_op.rhs.eval_to_var() {
                    debug!(
                        "Rewriting struct access -- lhs var: {:?}, rhs var: {:?}",
                        bin_op.lhs, &var
                    );

                    let struct_access = UnOperator::StructAccess(var.name.clone(), None, None);
                    let un_op = UnOp::new(struct_access, bin_op.lhs.clone());
                    *expr = Expr::Op(Op::UnOp(un_op));

                    debug!("expr after: {:?}", expr);
                }
            }
        }
    }

    fn visit_bin_op(&mut self, bin_op: &mut BinOp) {}

    fn visit_un_op(&mut self, un_op: &mut UnOp) {}

    fn visit_func(&mut self, ast_token: &mut AstToken) {}

    fn visit_struct(&mut self, ast_token: &mut AstToken) {}

    fn visit_enum(&mut self, ast_token: &mut AstToken) {}

    fn visit_interface(&mut self, ast_token: &mut AstToken) {}

    fn visit_impl(&mut self, ast_token: &mut AstToken) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken) {}

    fn visit_default_block(&mut self, ast_token: &mut AstToken) {}

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
}
