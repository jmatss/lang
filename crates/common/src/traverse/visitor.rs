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

#[allow(unused_variables)]
pub trait Visitor {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }
    fn take_errors_with_ctx(&mut self, ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        None
    }

    /* TOP LEVEL */
    fn visit_token(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_expr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {}
    fn visit_stmt(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_eof(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    /// Called when the AST have been traversed fully and the end of the AST is reached.
    fn visit_end(&mut self, ctx: &mut TraverseCtx) {}

    /* BLOCKS */
    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_fn(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_enum(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_union(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_trait(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_anon(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_if(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_if_case(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_match(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_match_case(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_for(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_while(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}
    fn visit_test(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {}

    /* STATEMENTS */
    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_yield(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_break(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_continue(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_use(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_package(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_inc(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_dec(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_defer(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_defer_exec(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_extern_decl(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}
    fn visit_modifier(&mut self, stmt: &mut Stmt, ctx: &mut TraverseCtx) {}

    /* EXPRESSIONS */
    fn visit_lit(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {}
    //fn visit_type(&mut self, expr: &mut Expr, ctx: &TraverseContext) {}
    fn visit_var(&mut self, var: &mut Var, ctx: &mut TraverseCtx) {}
    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {}
    fn visit_fn_ptr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {}
    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, ctx: &mut TraverseCtx) {}
    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {}
    fn visit_array_init(&mut self, array_init: &mut ArrayInit, ctx: &mut TraverseCtx) {}

    /* OPERATIONS */
    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &mut TraverseCtx) {}
    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseCtx) {}

    fn visit_type(&mut self, ty: &mut TypeId, ctx: &mut TraverseCtx) {}

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
