use super::token::LexTokenKind;
use crate::lex::iter::LexTokenIter;
use crate::lex::token::LexToken;
use crate::CustomResult;
use std::fs::File;
use std::io::Read;

/// Lexes the characters in the source code to LexToken's and returns a vector
/// containing all lex tokens.
pub fn lex(filename: &str) -> CustomResult<Vec<LexToken>> {
    let mut file: File = File::open(filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut iter = LexTokenIter::new(&contents);
    let mut lex_token_vec = Vec::new();

    loop {
        let lex_token = iter.next_token()?;
        lex_token_vec.push(lex_token.clone());

        if lex_token.kind == LexTokenKind::EndOfFile {
            break;
        }
    }

    Ok(lex_token_vec)
}
