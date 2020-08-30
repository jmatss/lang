use crate::iter::LexTokenIter;
use crate::token::LexToken;
use crate::token::LexTokenKind;
use common::error::LangError;
use std::fs::File;
use std::io::Read;

/// Lexes the characters in the source code to LexToken's and returns a vector
/// containing all lex tokens.
pub fn lex(filename: &str) -> Result<Vec<LexToken>, Vec<LangError>> {
    let mut file: File = File::open(filename).map_err(|e| vec![e.into()])?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|e| vec![e.into()])?;

    let mut iter = LexTokenIter::new(&contents);
    let mut lex_token_vec = Vec::new();
    let mut errors = Vec::new();

    loop {
        match iter.next_token() {
            Ok(lex_token) => {
                if lex_token.kind == LexTokenKind::EndOfFile {
                    lex_token_vec.push(lex_token);
                    break;
                }
                lex_token_vec.push(lex_token);
            }
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(lex_token_vec)
    } else {
        Err(errors)
    }
}
