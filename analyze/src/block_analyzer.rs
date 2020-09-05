use crate::{AnalyzeContext, BlockInfo};
use common::token::{block::BlockHeader, stmt::Stmt};
use parse::token::{AstToken, AstTokenKind};

pub struct BlockAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

impl<'a> BlockAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find information
    /// related to blocks and their scopes. It gets the parent for all blocks,
    /// if they contain any instructions related to control flow (return, branch etc.).
    pub fn analyze(context: &'a mut AnalyzeContext, ast_root: &mut AstToken) {
        let mut block_analyzer = BlockAnalyzer::new(context);
        block_analyzer.analyze_block(ast_root);
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        // Reset the `cur_block_id` to the default block (== 0).
        context.cur_block_id = 0;
        Self { context }
    }

    fn analyze_block(&mut self, token: &mut AstToken) -> Option<BlockInfo> {
        if let AstTokenKind::Block(ref header, id, ref mut body) = token.kind {
            let is_root_block = self.analyze_is_root(header);
            let is_branchable_block = self.analyze_is_branchable(header);
            let mut block_info = BlockInfo::new(id, is_root_block, is_branchable_block);

            // Update the mapping from this block to its parent.
            // Do not set a parent for the default block.
            if id != 0 {
                block_info.parent_id = self.context.cur_block_id;
            }

            // When iterating through all tokens, look at the If and Ifcases
            // and keep track if all their children contains return statements.
            let mut all_children_contains_returns = true;
            let mut child_count = 0;
            for token in body.iter_mut() {
                // Since `analyze_token` recurses, need to re-set the current
                // block to the be the "parent" (`self.context.cur_block_id`).
                self.context.cur_block_id = id;

                match token.kind {
                    AstTokenKind::Block(BlockHeader::If, ..)
                    | AstTokenKind::Block(BlockHeader::IfCase(_), ..)
                    | AstTokenKind::Block(BlockHeader::Function(_), ..)
                    | AstTokenKind::Block(BlockHeader::Match(_), ..)
                    | AstTokenKind::Block(BlockHeader::MatchCase(_), ..)
                    | AstTokenKind::Block(BlockHeader::For(..), ..)
                    | AstTokenKind::Block(BlockHeader::While(..), ..)
                    | AstTokenKind::Block(BlockHeader::Test(_), ..)
                    | AstTokenKind::Block(BlockHeader::Anonymous, ..) => {
                        if let Some(child_block_info) = self.analyze_block(token) {
                            if !child_block_info.all_children_contains_returns {
                                all_children_contains_returns = false;
                            }
                        }
                        child_count += 1;
                    }

                    AstTokenKind::Block(BlockHeader::Default, ..)
                    | AstTokenKind::Block(BlockHeader::Struct(_), ..)
                    | AstTokenKind::Block(BlockHeader::Enum(_), ..)
                    | AstTokenKind::Block(BlockHeader::Interface(_), ..)
                    | AstTokenKind::Block(BlockHeader::Implement(..), ..) => {
                        self.analyze_block(token)?;
                    }

                    AstTokenKind::Statement(ref stmt) => self.analyze_stmt(stmt, &mut block_info),

                    AstTokenKind::Expression(_) | AstTokenKind::EndOfFile => (),
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

            self.context.block_info.insert(id, block_info.clone());
            Some(block_info)
        } else {
            None
        }
    }

    /// Returns true if the given header is a "root" i.e. a block that creates
    /// a new scope where every block created inside them only has access to
    /// this block + globals.
    fn analyze_is_root(&mut self, header: &BlockHeader) -> bool {
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
    fn analyze_is_branchable(&mut self, header: &BlockHeader) -> bool {
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
            | Stmt::DeferExecution(_) => (),
        }
    }
}
