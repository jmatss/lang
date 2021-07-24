use common::{
    error::LangError, token::ast::AstToken, token::ast::Token, token::block::BlockHeader,
    token::block::Fn, token::expr::FuncCall, traverser::TraverseContext, visitor::Visitor,
};

use crate::AnalyzeContext;

/// Iterates through all individual control flows of all functions-orders and
/// ensures that all variables have been initialized before their first use.
pub struct UnititializedAnalyzer<'a> {
    analyze_context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> UnititializedAnalyzer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn analyze_fn_body(&mut self, body: &Vec<AstToken>) {
        for ast_token in body {
            match ast_token.token {
                Token::Expr(expr) => {}
                Token::Stmt(stmt) => {}
                Token::Block(header, _, _) => {}
                Token::EOF => {}
            }
        }
    }
}

impl<'a> Visitor for UnititializedAnalyzer<'a> {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Fn(func), _, body) = ast_token.token {}
    }
}
