use super::{block::BlockHeader, expr::Expr, stmt::Stmt};
use crate::BlockId;

/// Top level token that wraps all other kind of token types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstToken {
    pub token: Token,
    pub line_nr: u64,
    pub column_nr: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // TODO: Rust/C block (statement/expression (?)).
    Expr(Expr),
    Stmt(Stmt),
    Block(BlockHeader, BlockId, Vec<AstToken>),
    EOF,
}
