use super::{block::BlockHeader, expr::Expr, stmt::Stmt};
use crate::BlockId;

/// Top level token that wraps all other kind of token types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstToken {
    /// The first u64 is the line number annd the second is thte column number.
    // TODO: Rust/C block (statement/expression (?)).
    Expr(Expr),
    Stmt(Stmt),
    Block(BlockHeader, BlockId, Vec<AstToken>),
    EOF,
}
