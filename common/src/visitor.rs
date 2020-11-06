use std::cell::RefMut;

use crate::{
    error::LangError,
    token::{
        ast::AstToken,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    traverser::TraverseContext,
};

#[allow(unused_variables)]
pub trait Visitor {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }

    /* TOP LEVEL */
    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}
    fn visit_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}
    fn visit_expr(&mut self, expr: &mut Expr, ctx: &TraverseContext) {}
    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {}
    fn visit_eof(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}

    /* BLOCKS */
    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {}
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

    /* STATEMENTS */
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

    /* EXPRESSIONS */
    fn visit_lit(&mut self, expr: &mut Expr, ctx: &TraverseContext) {}
    //fn visit_type(&mut self, expr: &mut Expr, ctx: &TraverseContext) {}
    fn visit_var(&mut self, var: &mut RefMut<Var>, ctx: &TraverseContext) {}
    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {}
    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {}
    fn visit_array_init(&mut self, array_init: &mut ArrayInit, ctx: &TraverseContext) {}

    /* OPERATIONS */
    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &TraverseContext) {}
    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {}

    /* BINARY OPERATIONS */
    /*
    fn visit_bin_in(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_is(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_as(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_of(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_range(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_range_incl(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_dot(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_eq(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_neq(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_lt(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_gt(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_lte(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_gte(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_add(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_sub(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_mul(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_div(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_mod(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_pow(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_bit_and(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_bit_or(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_bit_xor(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_shl(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_shr(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_bool_and(&mut self, bin_op: &mut BinOp) {}
    fn visit_bin_bool_or(&mut self, bin_op: &mut BinOp) {}

    /* UNARY OPERATIONS */
    fn visit_un_deref(&mut self, un_op: &mut UnOp) {}
    fn visit_un_address(&mut self, un_op: &mut UnOp) {}
    fn visit_un_pos(&mut self, un_op: &mut UnOp) {}
    fn visit_un_neg(&mut self, un_op: &mut UnOp) {}
    fn visit_un_array_access(&mut self, un_op: &mut UnOp) {}
    fn visit_un_bit_compliment(&mut self, un_op: &mut UnOp) {}
    fn visit_un_bool_not(&mut self, un_op: &mut UnOp) {}
    */
}
