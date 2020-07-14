use crate::lexer::lex_token::LexToken;
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::token_iter::TokenIter;
use crate::CustomResult;

pub fn parse(simple_tokens: &[LexToken], indent_size: usize) -> CustomResult<AST> {
    let mut iter = TokenIter::new(simple_tokens, indent_size);
    iter.parse_abstract_syntax_tree()
}
