use std::cell::{RefCell, RefMut};

use crate::{block::BlockInfo, AnalyzeContext};
use common::{
    error::LangError,
    token::{ast::AstToken, block::BlockHeader, stmt::Stmt},
    traverser::TraverseContext,
    visitor::Visitor,
};
use log::debug;

/// Visits all blocks and gathers information related to them. It will add
/// "BlockInfo"s into the analyze context containing information about block
/// parent, specific branch statements the block contains etc.
pub struct BlockAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a, 'a_ctx> BlockAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    /// Returns true if the given header is a "root" i.e. a block that creates
    /// a new scope where every block created inside them only has access to
    /// this block + globals.
    #[allow(clippy::match_like_matches_macro)]
    fn is_root(&self, header: &BlockHeader) -> bool {
        match header {
            BlockHeader::Function(_)
            | BlockHeader::Struct(_)
            | BlockHeader::Enum(_)
            | BlockHeader::Trait(_)
            | BlockHeader::Implement(..)
            | BlockHeader::Default => true,
            _ => false,
        }
    }

    /// Returns true if the given header is a "branchable" block i.e. a block
    /// that can contain branch instructions like "break".
    #[allow(clippy::match_like_matches_macro)]
    fn is_branchable(&self, header: &BlockHeader) -> bool {
        match header {
            BlockHeader::Match(_) | BlockHeader::For(_, _) | BlockHeader::While(_) => true,
            _ => false,
        }
    }

    /// Sets the information about if a statement exists in `block_info`.
    fn analyze_stmt(&self, stmt: &Stmt, block_info: &mut BlockInfo) {
        match stmt {
            Stmt::Return(..) => block_info.contains_return = true,
            Stmt::Yield(..) => block_info.contains_yield = true,
            Stmt::Break(..) => block_info.contains_break = true,
            Stmt::Continue(..) => block_info.contains_continue = true,
            Stmt::Defer(..) => block_info.contains_defer = true,

            Stmt::Use(..)
            | Stmt::Package(..)
            | Stmt::Increment(..)
            | Stmt::Decrement(..)
            | Stmt::Assignment(..)
            | Stmt::VariableDecl(..)
            | Stmt::ExternalDecl(..)
            | Stmt::Modifier(_)
            | Stmt::DeferExec(_) => (),
        }
    }

    // TODO: Do this borrowing of analyze_context in a less ugly way.
    fn analyze_block(
        &mut self,
        ast_token: &AstToken,
        analyze_context: &mut RefMut<AnalyzeContext>,
        parent_id: usize,
    ) {
        analyze_context.file_pos = ast_token.file_pos().cloned().unwrap_or_default();

        if let AstToken::Block(ref header, file_pos, id, body) = &ast_token {
            let is_root_block = self.is_root(header);
            let is_branchable_block = self.is_branchable(header);
            let mut block_info =
                BlockInfo::new(*id, file_pos.to_owned(), is_root_block, is_branchable_block);

            // Update the mapping from this block to its parent.
            // Do not set a parent for the default block.
            if *id != 0 {
                block_info.parent_id = parent_id;
            }

            // When iterating through all tokens, look at the If and Ifcases
            // and keep track if all their children contains return statements.
            let mut all_children_contains_returns = true;
            let mut child_count = 0;
            for child_token in body.iter() {
                match child_token {
                    AstToken::Block(BlockHeader::If, _, child_id, _)
                    | AstToken::Block(BlockHeader::IfCase(_), _, child_id, _)
                    | AstToken::Block(BlockHeader::Function(_), _, child_id, _)
                    | AstToken::Block(BlockHeader::Match(_), _, child_id, _)
                    | AstToken::Block(BlockHeader::MatchCase(_), _, child_id, _)
                    | AstToken::Block(BlockHeader::For(..), _, child_id, _)
                    | AstToken::Block(BlockHeader::While(..), _, child_id, _)
                    | AstToken::Block(BlockHeader::Test(_), _, child_id, _)
                    | AstToken::Block(BlockHeader::Anonymous, _, child_id, _) => {
                        self.analyze_block(child_token, analyze_context, *id);

                        if let Some(child_block_info) = analyze_context.block_info.get(&child_id) {
                            if !child_block_info.all_children_contains_returns {
                                all_children_contains_returns = false;
                            }
                        } else {
                            let err = analyze_context.err(format!(
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
                    | AstToken::Block(BlockHeader::Trait(_), ..)
                    | AstToken::Block(BlockHeader::Implement(..), ..) => {
                        self.analyze_block(child_token, analyze_context, *id);
                    }

                    AstToken::Stmt(ref stmt) => self.analyze_stmt(stmt, &mut block_info),

                    AstToken::Empty | AstToken::Comment(..) | AstToken::Expr(_) | AstToken::EOF => {
                        // Do nothing.
                    }
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

            analyze_context.block_info.insert(*id, block_info);
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
    fn visit_default_block(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();
        self.analyze_block(ast_token, &mut analyze_context, usize::MAX);
    }

    fn visit_eof(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {
        debug!(
            "BLOCK_INFO --\n{:#?}",
            self.analyze_context.borrow_mut().block_info
        );
    }
}
