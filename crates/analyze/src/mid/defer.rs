use std::collections::{hash_map::Entry, HashMap};

use common::{
    ctx::{ast_ctx::AstCtx, traverse_ctx::TraverseCtx},
    error::LangError,
    token::{ast::AstToken, block::BlockHeader, expr::Expr, stmt::Stmt},
    traverse::visitor::Visitor,
    BlockId,
};

/// Iterates through all "Defer" stmts and inserts new "DeferExec" stmts in the
/// AST where needed. This will for example be before branching away from the
/// current block or if the block is ending.
pub struct DeferAnalyzer {
    /// Contains defer-statements for a specific block. Expressions will be added
    /// to this map continuously during "defer analyzing" when the statement is
    /// seen. This ensures that only the defers that have been "initialized"/"seen"
    /// will be inserted before a branch away which is the expected behaviour.
    pub defer_stmts: HashMap<BlockId, Vec<Expr>>,

    errors: Vec<LangError>,
}

impl DeferAnalyzer {
    pub fn new() -> Self {
        Self {
            defer_stmts: HashMap::default(),
            errors: Vec::default(),
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

    /// Inserts the given `defers` into the vector `body` starting at the index
    /// `i` and shifts all following tokens to the right.
    fn insert_defers_into_ast(
        &mut self,
        i: &mut usize,
        body: &mut Vec<AstToken>,
        defers: Vec<Expr>,
    ) {
        for expr in defers.into_iter() {
            body.insert(*i, AstToken::Stmt(Stmt::DeferExec(expr)));
            *i += 1;
        }
    }

    /// Pushes the given `defers` to the end of vector `body`.
    fn push_defers_into_ast(&mut self, body: &mut Vec<AstToken>, defers: Vec<Expr>) {
        for expr in defers.into_iter() {
            body.push(AstToken::Stmt(Stmt::DeferExec(expr)));
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
    fn get_defers<F>(&mut self, ast_ctx: &AstCtx, id: BlockId, pred: F) -> Option<Vec<Expr>>
    where
        F: Fn(bool) -> bool,
    {
        let mut defers = Vec::new();

        let mut cur_id = id;
        while let Some(cur_block_ctx) = ast_ctx.block_ctxs.get(&cur_id) {
            if let Some(cur_defers) = self.defer_stmts.get(&cur_id) {
                for defer in cur_defers.iter().rev() {
                    defers.push(defer.clone());
                }
            }
            cur_id = cur_block_ctx.parent_id;

            if pred(cur_block_ctx.is_branchable_block) {
                break;
            }
        }

        if !defers.is_empty() {
            Some(defers)
        } else {
            None
        }
    }

    /// Given a block ID `id`, returns every deferred expression declared in the
    /// block with ID `id`. It does NOT look at any parents.
    fn get_defers_curr_block(&mut self, ast_ctx: &AstCtx, id: BlockId) -> Option<Vec<Expr>> {
        self.get_defers(ast_ctx, id, |_| true)
    }

    /// Given a block ID `id`, returns every deferred expression for this block
    /// AND all its parent blocks.
    fn get_defers_all_parents(&mut self, ast_ctx: &AstCtx, id: BlockId) -> Option<Vec<Expr>> {
        self.get_defers(ast_ctx, id, |_| false)
    }

    /// Given a block ID `id`, returns every deferred expression for this block
    /// and all parent blocks up to the first "branchable" block (ex. "while"
    /// and "for" blocks).
    fn get_defers_until_branchable(&mut self, ast_ctx: &AstCtx, id: BlockId) -> Option<Vec<Expr>> {
        self.get_defers(ast_ctx, id, |x| x)
    }

    fn traverse_block(&mut self, ast_ctx: &AstCtx, mut ast_token: &mut AstToken) {
        if let AstToken::Block(block_header, _, id, body) = &mut ast_token {
            let mut i = 0;
            while i < body.len() {
                let child_token = &mut body[i];
                match child_token {
                    // If the token is a "Defer" statement, store the defer
                    // in the `self.defer_stmts`.
                    AstToken::Stmt(Stmt::Defer(expr, ..)) => {
                        self.store_defer(expr.clone(), *id);
                    }

                    // This is a branch out of the current function, return all
                    // "outstanding" defers and add them before this return.
                    AstToken::Stmt(Stmt::Return(..)) => {
                        if let Some(defers) = self.get_defers_all_parents(ast_ctx, *id) {
                            self.insert_defers_into_ast(&mut i, body, defers);
                        }
                    }

                    // A branch out of a local scope, get the "outstanding"
                    // deferes introduces in this scope.
                    AstToken::Stmt(Stmt::Yield(..))
                    | AstToken::Stmt(Stmt::Break(..))
                    | AstToken::Stmt(Stmt::Continue(..)) => {
                        if let Some(defers) = self.get_defers_until_branchable(ast_ctx, *id) {
                            self.insert_defers_into_ast(&mut i, body, defers);
                        }
                    }

                    // For all other tokens, traverse recursively so that the
                    // defers are introduced in the correct scope.
                    _ => self.traverse_block(ast_ctx, child_token),
                }
                i += 1;
            }

            let block_ctx = if let Some(block_ctx) = ast_ctx.block_ctxs.get(id) {
                block_ctx
            } else {
                let err = ast_ctx.err(format!(
                    "Unable to find block info for block with ID: {}",
                    id
                ));
                self.errors.push(err);
                return;
            };

            // The end of the blocks scope has been reached. Any defers declared
            // in this scope should be executed at this point.
            if !block_ctx.all_children_contains_returns {
                match block_header {
                    BlockHeader::Anonymous
                    | BlockHeader::If
                    | BlockHeader::IfCase(_)
                    | BlockHeader::Match(_)
                    | BlockHeader::MatchCase(_)
                    | BlockHeader::For(_, _)
                    | BlockHeader::While(_) => {
                        if let Some(defers) = self.get_defers_curr_block(ast_ctx, *id) {
                            self.push_defers_into_ast(body, defers);
                        }
                    }
                    _ => (),
                }
            }
        }
    }
}

impl Visitor for DeferAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        self.traverse_block(&ctx.ast_ctx, ast_token);
    }
}
