use super::analyzer::BlockInfo;
use crate::analyze::analyzer::AnalyzeContext;
use crate::parse::token::{BlockHeader, ParseToken, ParseTokenKind, Statement};

pub struct BlockAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

impl<'a> BlockAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find information
    /// related to blocks and their scopes. It gets the parent for all blocks,
    /// if they contain any instructions related to control flow (return, branch etc.).
    pub fn analyze(context: &'a mut AnalyzeContext, ast_root: &mut ParseToken) {
        let mut block_analyzer = BlockAnalyzer::new(context);
        block_analyzer.analyze_block(ast_root);
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        // Reset the `cur_block_id` to the default block (== 0).
        context.cur_block_id = 0;
        Self { context }
    }

    fn analyze_block(&mut self, token: &mut ParseToken) -> Option<BlockInfo> {
        if let ParseTokenKind::Block(ref header, id, ref mut body) = token.kind {
            let is_root_block = self.analyze_is_root(header);
            let mut block_info = BlockInfo::new(id, is_root_block);

            // Update the mapping from this block to its parent.
            // Do not set a parent for the default block.
            if id != 0 {
                block_info.parent_id = self.context.cur_block_id;
            }

            // When iterating through all tokens, look at the If and Ifcases
            // and keep track if all their children contains return statements.
            let mut all_children_contains_returns = true;
            let mut child_count = 0;
            for token in body {
                // Since `analyze_token` recurses, need to re-set the current
                // block to the be the "parent" (`self.context.cur_block_id`).
                self.context.cur_block_id = id;

                match token.kind {
                    ParseTokenKind::Block(BlockHeader::If, ..)
                    | ParseTokenKind::Block(BlockHeader::IfCase(_), ..)
                    | ParseTokenKind::Block(BlockHeader::Function(_), ..)
                    | ParseTokenKind::Block(BlockHeader::Match(_), ..)
                    | ParseTokenKind::Block(BlockHeader::MatchCase(_), ..)
                    | ParseTokenKind::Block(BlockHeader::For(..), ..)
                    | ParseTokenKind::Block(BlockHeader::While(..), ..)
                    | ParseTokenKind::Block(BlockHeader::Test(_), ..) => {
                        if let Some(child_block_info) = self.analyze_block(token) {
                            if !child_block_info.all_children_contains_returns {
                                all_children_contains_returns = false;
                            }
                        }
                        child_count += 1;
                    }

                    ParseTokenKind::Block(BlockHeader::Default, ..)
                    | ParseTokenKind::Block(BlockHeader::Struct(_), ..)
                    | ParseTokenKind::Block(BlockHeader::Enum(_), ..)
                    | ParseTokenKind::Block(BlockHeader::Interface(_), ..) => {
                        self.analyze_block(token)?;
                    }

                    ParseTokenKind::Statement(ref stmt) => self.analyze_stmt(stmt, &mut block_info),

                    ParseTokenKind::Expression(_) | ParseTokenKind::EndOfFile => (),
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
            | BlockHeader::Default => true,
            _ => false,
        }
    }

    /// Sets the information about if a statement exists in `block_info`.
    fn analyze_stmt(&self, stmt: &Statement, block_info: &mut BlockInfo) {
        match stmt {
            Statement::Return(_) => block_info.contains_return = true,
            Statement::Yield(_) => block_info.contains_yield = true,
            Statement::Break => block_info.contains_break = true,
            Statement::Continue => block_info.contains_continue = true,
            Statement::With(_) => block_info.contains_with = true,
            Statement::Defer(_) => block_info.contains_defer = true,

            Statement::Use(_)
            | Statement::Package(_)
            | Statement::Assignment(..)
            | Statement::VariableDecl(..)
            | Statement::ExternalDecl(_)
            | Statement::Modifier(_) => (),
        }
    }
}
