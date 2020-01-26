use crate::lexer::simple_token::SimpleToken;
use crate::CustomResult;
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::token_iter::TokenIter;

pub fn parse(simple_tokens: &[SimpleToken]) -> CustomResult<AST> {
    let mut iter = TokenIter::new(simple_tokens, 4);
    iter.parse_abstract_syntax_tree()
}