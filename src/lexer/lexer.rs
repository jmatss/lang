use crate::lexer::lex_token::LexToken;
use crate::lexer::lex_token_iter::LexTokenIter;
use crate::CustomResult;
use super::lex_token::LexTokenType;

/// Lexes the characters in the source code to LexToken's and returns a vector
/// containing all lex tokens. 
pub fn lex(filename: &str) -> CustomResult<Vec<LexToken>> {
    let mut lex_token_vec = Vec::new();
    let iter = LexTokenIter::new(filename)?;

    let lex_token = iter.next_token()?;
    while lex_token.t != LexTokenType::EndOfFile {
        lex_token_vec.push(lex_token.clone());
        lex_token = iter.next_token()?;
    }

    Ok(lex_token_vec)
}
