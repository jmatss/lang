use crate::{
    ctx::traverse_ctx::TraverseCtx,
    error::LangError,
    token::{
        ast::AstToken,
        expr::{AdtInit, ArrayInit, BuiltInCall, Expr, FnCall, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    TypeId,
};

use super::visitor::Visitor;

/// A "dummy" visitor that is used as a default place holder that is set when
/// a new traverser is created. This visitor shouldn't be used, the traverser
/// should set another visitor before running.
pub struct DummyVisitor {}

#[allow(unused)]
impl Visitor for DummyVisitor {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        unreachable!()
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_expr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_eof(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_fn(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_union(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_trait(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_anon(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_if(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_if_case(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_match(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_match_case(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_for(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_while(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_test(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_yield(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_break(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_continue(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_use(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_package(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_inc(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_dec(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_defer(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_defer_exec(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_modifier(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_lit(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_array_init(&mut self, expr: &mut ArrayInit, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, ctx: &mut TraverseCtx) {
        unreachable!()
    }

    fn visit_type(&mut self, ty: &mut TypeId, ctx: &mut TraverseCtx) {
        unreachable!()
    }
}