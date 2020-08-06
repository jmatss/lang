use crate::analyze::analyzer::AnalyzeContext;
use crate::parse::token::{BlockHeader, ParseToken, ParseTokenKind};
use crate::CustomResult;

pub struct ScopeAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

impl<'a> ScopeAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find all
    /// parent->child relations and also checks if the parents are "root blocks"
    /// or not.
    pub fn analyze(context: &'a mut AnalyzeContext, ast_root: &mut ParseToken) -> CustomResult<()> {
        let mut scope_analyzer = ScopeAnalyzer::new(context);
        scope_analyzer.analyze_token(ast_root)
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        // Reset the `cur_block_id` to the default block (== 0).
        context.cur_block_id = 0;
        Self { context }
    }

    /// Only "blocks" are of interest in this function since they are the ones
    /// representing scopes.
    fn analyze_token(&mut self, token: &mut ParseToken) -> CustomResult<()> {
        if let ParseTokenKind::Block(ref header, id, ref mut body) = token.kind {
            // Update the mapping from this block to its parent.
            // Do not set a parent for the default block.
            if id != 0 {
                self.context
                    .child_to_parent
                    .insert(id, self.context.cur_block_id);
            }

            // Set information about if this block is a "root" or not in the
            // analyze context.
            let is_root = self.analyze_is_root(header);
            self.context.is_root_block.insert(id, is_root);

            for token in body {
                // Since `analyze_token` recurses, need to re-set the current
                // block to the be the "parent" (`self.context.cur_block_id`)
                // for every iteration of the loop.
                self.context.cur_block_id = id;
                self.analyze_token(token)?;
            }
        }

        Ok(())
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
}
