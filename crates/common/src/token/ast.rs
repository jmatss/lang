use super::{block::BlockHeader, expr::Expr, stmt::Stmt};
use crate::{file::FilePosition, BlockId};

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstToken {
    // TODO: Rust/C block (statement/expression (?)).
    Expr(Expr),
    Stmt(Stmt),
    Block(BlockHeader, FilePosition, BlockId, Vec<AstToken>),
    Comment(String, CommentType, FilePosition),
    /// Will be set for removed tokens, should be ignored when visited.
    Empty,
    EOF,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CommentType {
    SingleLine,
    MultiLine,
}

impl AstToken {
    /// Gets the file position of the given AstToken. For blocks, this function
    /// will only consider the BlockHeader, the body is not counted into the
    /// FilePosition.
    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self {
            AstToken::Expr(expr) => expr.file_pos(),
            AstToken::Stmt(stmt) => stmt.file_pos(),

            AstToken::Comment(.., file_pos) | AstToken::Block(_, file_pos, ..) => Some(file_pos),

            AstToken::Empty | AstToken::EOF => None,
        }
    }

    /// Returns true if this token is "skippable". This is tokens that aren't
    /// really used generate code, ex. comments and empty tokens.
    pub fn is_skippable(&self) -> bool {
        matches!(self, AstToken::Comment(..) | AstToken::Empty)
    }
}
