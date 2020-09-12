use crate::{AnalyzeContext, BlockInfo};
use common::{
    error::LangError,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    visitor::Visitor,
};
use log::debug;

/// Visits all blocks and gathers information related to them. It will add
/// "BlockInfo"s into the analyze context containing information about block
/// parent, specific branch statements the block contains etc.
pub struct BlockAnalyzer<'a> {
    analyze_context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> BlockAnalyzer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    /// Returns true if the given header is a "root" i.e. a block that creates
    /// a new scope where every block created inside them only has access to
    /// this block + globals.
    fn analyze_is_root(&self, header: &BlockHeader) -> bool {
        match header {
            BlockHeader::Function(_)
            | BlockHeader::Struct(_)
            | BlockHeader::Enum(_)
            | BlockHeader::Interface(_)
            | BlockHeader::Implement(..)
            | BlockHeader::Default => true,
            _ => false,
        }
    }

    /// Returns true if the given header is a "branchable" block i.e. a block
    /// that can contain branch instructions like "break".
    fn analyze_is_branchable(&self, header: &BlockHeader) -> bool {
        match header {
            BlockHeader::Match(_) | BlockHeader::For(_, _) | BlockHeader::While(_) => true,
            _ => false,
        }
    }

    /// Sets the information about if a statement exists in `block_info`.
    fn analyze_stmt(&self, stmt: &Stmt, block_info: &mut BlockInfo) {
        match stmt {
            Stmt::Return(_) => block_info.contains_return = true,
            Stmt::Yield(_) => block_info.contains_yield = true,
            Stmt::Break => block_info.contains_break = true,
            Stmt::Continue => block_info.contains_continue = true,
            Stmt::Defer(_) => block_info.contains_defer = true,

            Stmt::Use(_)
            | Stmt::Package(_)
            | Stmt::Assignment(..)
            | Stmt::VariableDecl(..)
            | Stmt::ExternalDecl(_)
            | Stmt::Modifier(_)
            | Stmt::DeferExec(_) => (),
        }
    }

    fn analyze_block(&mut self, ast_token: &AstToken) {
        if let AstToken::Block(ref header, id, body) = ast_token {
            let is_root_block = self.analyze_is_root(header);
            let is_branchable_block = self.analyze_is_branchable(header);
            let mut block_info = BlockInfo::new(*id, is_root_block, is_branchable_block);

            // Update the mapping from this block to its parent.
            // Do not set a parent for the default block.
            if *id != 0 {
                block_info.parent_id = self.analyze_context.cur_block_id;
            }

            // When iterating through all tokens, look at the If and Ifcases
            // and keep track if all their children contains return statements.
            let mut all_children_contains_returns = true;
            let mut child_count = 0;
            for child_token in body.iter() {
                // Since `analyze_token` recurses, need to re-set the current
                // block to the be the "parent" (`self.context.cur_block_id`).
                self.analyze_context.cur_block_id = *id;

                match child_token {
                    // TODO: Don't want a reference to `child_id` since it prevents
                    //       the "visit_block(token)" to borrow token since it
                    //       will be a duplicate mutable reference borrow.
                    //       Adding mut during the pattern matching prevents this,
                    //       is there a way to do this without mut?
                    AstToken::Block(BlockHeader::If, mut child_id, _)
                    | AstToken::Block(BlockHeader::IfCase(_), mut child_id, _)
                    | AstToken::Block(BlockHeader::Function(_), mut child_id, _)
                    | AstToken::Block(BlockHeader::Match(_), mut child_id, _)
                    | AstToken::Block(BlockHeader::MatchCase(_), mut child_id, _)
                    | AstToken::Block(BlockHeader::For(..), mut child_id, _)
                    | AstToken::Block(BlockHeader::While(..), mut child_id, _)
                    | AstToken::Block(BlockHeader::Test(_), mut child_id, _)
                    | AstToken::Block(BlockHeader::Anonymous, mut child_id, _) => {
                        self.analyze_block(child_token);

                        if let Some(child_block_info) =
                            self.analyze_context.block_info.get(&child_id)
                        {
                            if !child_block_info.all_children_contains_returns {
                                all_children_contains_returns = false;
                            }
                        } else {
                            let err = self.analyze_context.err(format!(
                                "Unable to get block info for ID {} when in ID {}.",
                                child_id, id
                            ));
                            self.errors.push(err);
                            return;
                        }
                        child_count += 1;
                    }

                    AstToken::Block(BlockHeader::Default, ..)
                    | AstToken::Block(BlockHeader::Struct(_), ..)
                    | AstToken::Block(BlockHeader::Enum(_), ..)
                    | AstToken::Block(BlockHeader::Interface(_), ..)
                    | AstToken::Block(BlockHeader::Implement(..), ..) => {
                        self.analyze_block(child_token);
                    }

                    AstToken::Stmt(ref stmt) => self.analyze_stmt(stmt, &mut block_info),

                    AstToken::Expr(_) | AstToken::EOF => (),
                }
            }

            // Set the flag to indicate if all children contains return statements
            // for the current block. If this block doesn't have any children,
            // the value will be set to true if this block itself contains a return.
            block_info.all_children_contains_returns = if child_count > 0 {
                all_children_contains_returns
            } else {
                block_info.contains_return
            };

            self.analyze_context.block_info.insert(*id, block_info);
        }
    }
}

impl<'a> Visitor for BlockAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// All traversing is done from the default block, no other visit function
    /// will be used. The reason being that this needs to be called recursively
    /// on blocks, which currently isn't possible to do with the regular traverser.
    fn visit_default_block(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(_, id, _) = ast_token {
            self.analyze_context.cur_block_id = *id;
        }

        self.analyze_block(ast_token);
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {}

    fn visit_expr(&mut self, expr: &mut Expr) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken) {
        debug!("BLOCK_INFO --\n{:#?}", self.analyze_context.block_info);
    }

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
