use crate::lexer::simple_token_iter::SimpleTokenIter;
use crate::CustomResult;
use crate::lexer::simple_token::SimpleToken;
use crate::error::CustomError;

pub fn lex(filename: &str) -> CustomResult<Vec<SimpleToken>> {
    let mut simple_token_vec = Vec::new();
    let mut simple_token_iter = SimpleTokenIter::new(filename)
        .map_err(|e| CustomError::LexError(e.to_string()))?;

    loop {
        let simple_token = simple_token_iter
            .next_simple_token()
            .map_err(|e| CustomError::LexError(e.to_string()))?;
        simple_token_vec.push(simple_token.clone());

        if simple_token == SimpleToken::EndOfFile {
            break;
        }
    }

    Ok(simple_token_vec)
}