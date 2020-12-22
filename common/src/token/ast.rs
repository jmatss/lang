use super::{block::BlockHeader, expr::Expr, stmt::Stmt};
use crate::{file::FilePosition, BlockId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstToken {
    // TODO: Rust/C block (statement/expression (?)).
    Expr(Expr),
    Stmt(Stmt),
    Block(BlockHeader, BlockId, Vec<AstToken>),
    Empty, // Will be set for removed tokens, should be ignored when visited.
    EOF,
}

impl AstToken {
    /// Gets the file position of the given AstToken. For blocks, this function
    /// will only consider the BlockHeader, the body is not counted into the
    /// FilePosition.
    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self {
            AstToken::Expr(expr) => expr.file_pos(),
            AstToken::Stmt(stmt) => stmt.file_pos(),

            // TODO: Implement FilePosition for Blocks. The whole file pos logic
            //       will probably be completly reworked later on, so implement
            //       it after that point.
            AstToken::Block(block_header, ..) => None,

            AstToken::Empty | AstToken::EOF => None,
        }
    }
}
