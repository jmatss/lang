use crate::lexer::simple_token::SimpleToken;
use crate::CustomResult;
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::token_iter::TokenIter;

pub fn parse(simple_tokens: &[SimpleToken], indent_size: usize) -> CustomResult<AST> {
    let mut iter = TokenIter::new(simple_tokens, indent_size);
    iter.parse_abstract_syntax_tree()
}