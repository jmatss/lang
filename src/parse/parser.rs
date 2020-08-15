use super::token::{BlockHeader, ParseToken, ParseTokenKind};
use crate::error::LangError;
use crate::lex::token::LexToken;
use crate::parse::iter::ParseTokenIter;

pub fn parse(lex_tokens: Vec<LexToken>) -> Result<ParseToken, Vec<LangError>> {
    let mut iter = ParseTokenIter::new(lex_tokens);

    let root_block_header = BlockHeader::Default;
    let root_block_id = iter.reserve_block_id();
    let mut root_block_body = Vec::new();
    let mut errors = Vec::new();

    loop {
        match iter.next_token() {
            Ok(parse_token) => {
                if parse_token.kind == ParseTokenKind::EndOfFile {
                    break;
                }
                root_block_body.push(parse_token);
            }
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        let kind = ParseTokenKind::Block(root_block_header, root_block_id, root_block_body);
        Ok(ParseToken::new(kind, 0, 0))
    } else {
        Err(errors)
    }
}
