use crate::{
    error::LangError,
    token::{
        ast::AstToken,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    visitor::Visitor,
};

/// A "dummy" visitor that is used as a default place holder that is set when
/// a new traverser is created. This visitor shouldn't be used, the traverser
/// should set another visitor before running.
pub struct DummyVisitor {}

#[allow(unused)]
impl Visitor for DummyVisitor {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        unreachable!()
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        unreachable!()
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_eof(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_default_block(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_func(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_interface(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_anon(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_if(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_if_case(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_match(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_match_case(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_for(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_while(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_test(&mut self, ast_token: &mut AstToken) {
        unreachable!()
    }

    fn visit_return(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_yield(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_break(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_continue(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_use(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_package(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_defer(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_defer_exec(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_assignment(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_modifier(&mut self, stmt: &mut Stmt) {
        unreachable!()
    }

    fn visit_lit(&mut self, expr: &mut Expr) {
        unreachable!()
    }

    fn visit_var(&mut self, var: &mut Var) {
        unreachable!()
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall) {
        unreachable!()
    }

    fn visit_struct_init(&mut self, struct_init: &mut StructInit) {
        unreachable!()
    }

    fn visit_array_init(&mut self, expr: &mut ArrayInit) {
        unreachable!()
    }

    fn visit_bin_op(&mut self, bin_op: &mut BinOp) {
        unreachable!()
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp) {
        unreachable!()
    }
}
