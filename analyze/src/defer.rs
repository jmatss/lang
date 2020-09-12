use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::ast::Token,
    token::{
        ast::AstToken,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    visitor::Visitor,
    BlockId,
};
use std::collections::{hash_map::Entry, HashMap};

/// Iterates through all "Defer" stmts and inserts new "DeferExec" stmts in the
/// AST where needed. This will for example be before branching away from the
/// current block or if the block is ending.
pub struct DeferAnalyzer<'a> {
    analyze_context: &'a mut AnalyzeContext,

    /// Contains defer-statements for a specific block. Expressions will be added
    /// to this map continuously during "defer analyzing" when the statement is
    /// seen. This ensures that only the defers that have been "initialized"/"seen"
    /// will be inserted before a branch away which is the expected behaviour.
    pub defer_stmts: HashMap<BlockId, Vec<Expr>>,
}

impl<'a> DeferAnalyzer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            defer_stmts: HashMap::default(),
        }
    }

    fn store_defer(&mut self, expr: Expr, block_id: BlockId) {
        match self.defer_stmts.entry(block_id) {
            Entry::Occupied(ref mut o) => {
                o.get_mut().push(expr);
            }
            Entry::Vacant(v) => {
                v.insert(vec![expr]);
            }
        }
    }

    fn insert_defers(&mut self, i: &mut usize, body: &mut Vec<AstToken>, defers: Vec<Expr>) {
        for expr in defers.into_iter() {
            body.insert(
                *i,
                AstToken {
                    token: Token::Stmt(Stmt::DeferExec(expr)),
                    line_nr: 0,
                    column_nr: 0,
                },
            );
            *i += 1;
        }
    }

    // TODO: The bool parameter to the is some realy weird edge case where it
    //       is only used by one of the three cases. It currently works, but isn't
    //       extendable. Is there a better way to do this?
    /// Given a block ID `id`, returns every deferred expression starting from
    /// the current block and then its parents until the predicate `pred`
    /// evaluates to true.
    /// The defers will be returned in reverse order in the result vector with
    /// the "newest" defers at the start of the returned vector.
    fn get_defers<F>(&mut self, id: BlockId, pred: F) -> Option<Vec<Expr>>
    where
        F: Fn(bool) -> bool,
    {
        let mut defers = Vec::new();

        let mut cur_id = id;
        while let Some(cur_block_info) = self.analyze_context.block_info.get(&cur_id) {
            if let Some(cur_defers) = self.defer_stmts.get(&cur_id) {
                for defer in cur_defers.iter().rev() {
                    defers.push(defer.clone());
                }
            }
            cur_id = cur_block_info.parent_id;

            if pred(cur_block_info.is_branchable_block) {
                break;
            }
        }

        if !defers.is_empty() {
            Some(defers)
        } else {
            None
        }
    }

    /// Given a block ID `id`, returns every deferred expression for this
    /// single block.
    fn get_cur_block_defers(&mut self, id: BlockId) -> Option<Vec<Expr>> {
        self.get_defers(id, |_| true)
    }

    /// Given a block ID `id`, returns every deferred expression for this block
    /// AND all its parent blocks.
    fn get_defers_all_parents(&mut self, id: BlockId) -> Option<Vec<Expr>> {
        self.get_defers(id, |_| false)
    }

    /// Given a block ID `id`, returns every deferred expression for this block
    /// and all parent blocks up to the first "branchable" block (ex. "while"
    /// and "for" blocks).
    fn get_defers_until_branchable(&mut self, id: BlockId) -> Option<Vec<Expr>> {
        self.get_defers(id, |x| x)
    }
}

impl<'a> Visitor for DeferAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        None
    }

    fn visit_token(&mut self, ast_token: &mut AstToken) {
        self.analyze_context.cur_line_nr = ast_token.line_nr;
        self.analyze_context.cur_column_nr = ast_token.column_nr;
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {
        if let Token::Block(_, id, body) = &mut ast_token.token {
            self.analyze_context.cur_block_id = *id;

            let mut i = 0;
            while i < body.len() {
                match &body[i].token {
                    // If the token is a "Defer" statement, store the defer
                    // in the `self.defer_stmts`.
                    Token::Stmt(Stmt::Defer(expr)) => {
                        self.store_defer(expr.clone(), *id);
                    }

                    // This is a branch instruction, insert the stored defers
                    // as instructions before this branch.
                    Token::Stmt(Stmt::Return(_)) => {
                        if let Some(defers) = self.get_defers_all_parents(*id) {
                            self.insert_defers(&mut i, body, defers);
                        }
                    }
                    Token::Stmt(Stmt::Yield(_))
                    | Token::Stmt(Stmt::Break)
                    | Token::Stmt(Stmt::Continue) => {
                        if let Some(defers) = self.get_defers_until_branchable(*id) {
                            self.insert_defers(&mut i, body, defers);
                        }
                    }

                    // For all other tokens and non-branching statements, do nothing.
                    _ => (),
                }
                i += 1;
            }
        }
    }

    fn visit_defer(&mut self, stmt: &mut Stmt) {}

    fn visit_return(&mut self, stmt: &mut Stmt) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt) {}

    fn visit_yield(&mut self, stmt: &mut Stmt) {}

    fn visit_break(&mut self, stmt: &mut Stmt) {}

    fn visit_continue(&mut self, stmt: &mut Stmt) {}

    fn visit_expr(&mut self, expr: &mut Expr) {}

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

    fn visit_use(&mut self, stmt: &mut Stmt) {}

    fn visit_package(&mut self, stmt: &mut Stmt) {}

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
