use super::analyzer::BlockNode;
use crate::analyze::analyzer::AnalyzeContext;
use crate::parse::token::{BlockHeader, ParseToken, ParseTokenKind};
use crate::CustomResult;

pub struct ScopeAnalyzer<'a, 'ctx> {
    context: &'a mut AnalyzeContext<'ctx>,
    cur_parent: BlockNode,
}

impl<'a, 'ctx> ScopeAnalyzer<'a, 'ctx> {
    /// Takes in a the root of the AST and walks the whole tree to find all
    /// parent->child relations and also checks if the parents are "root blocks"
    /// or not.
    pub fn analyze(
        context: &'a mut AnalyzeContext<'ctx>,
        ast_root: &mut ParseToken,
    ) -> CustomResult<()> {
        let mut scope_analyzer = ScopeAnalyzer::new(context);
        scope_analyzer.analyze_token(ast_root)
    }

    fn new(context: &'a mut AnalyzeContext<'ctx>) -> Self {
        let root_block_node = BlockNode::new(0, true);
        Self {
            context,
            cur_parent: root_block_node,
        }
    }

    /// Only blocks are of interest in this function since they are the ones
    /// representing scopes.
    fn analyze_token(&mut self, token: &mut ParseToken) -> CustomResult<()> {
        if let ParseTokenKind::Block(ref header, id, ref mut body) = token.kind {
            self.context
                .child_to_parent
                .insert(id, self.cur_parent.clone());

            let is_root = self.analyze_is_root(header);
            let cur_block = BlockNode::new(id, is_root);

            for token in body {
                // Since `analyze_token` recurses, need to re-set the current
                // block to the parent for every iteration of the loop.
                self.cur_parent = cur_block.clone();
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
