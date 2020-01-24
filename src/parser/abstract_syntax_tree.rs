use crate::parser::token::Token;

pub struct AST {
    nodes: Vec<ASTBlock>,
}

impl AST {
    pub fn new() -> Self {
        AST { nodes: Vec::new() }
    }
}

// Can add extra information in here later if needed.
pub struct ASTBlock {
    header: ASTToken,
    body: Vec<ASTToken>,

    // indent level of the header.
    // normalized by indent_fixed_size (/).
    // FIXME: Probably not needed.
    indent_level: usize,
}

// Can add extra information in here later if needed.
pub struct ASTToken {
    token: Token,
    line_number: usize,
}