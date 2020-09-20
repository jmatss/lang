use common::{
    error::LangError,
    token::{
        ast::AstToken,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
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
    fn visit_expr(&mut self, expr: &mut Expr, ctx: &TraverseContext) {
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

    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &TraverseContext) {}

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {}

    fn visit_func(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_enum(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_interface(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

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

    fn visit_inc(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

    fn visit_dec(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}

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
}
