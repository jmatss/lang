use super::token::{BlockHeader, ParseToken, ParseTokenKind};
use crate::lex::token::LexToken;
use crate::parse::iter::ParseTokenIter;
use crate::CustomResult;

pub fn parse(lex_tokens: Vec<LexToken>) -> CustomResult<ParseToken> {
    let mut iter = ParseTokenIter::new(lex_tokens);

    let root_block_header = BlockHeader::Default;
    let root_block_id = iter.reserve_block_id();
    let mut root_block_body = Vec::new();

    loop {
        let parse_token = iter.next_token()?;
        if let ParseTokenKind::EndOfFile = parse_token.kind {
            break;
        }
        root_block_body.push(parse_token);
    }

    let kind = ParseTokenKind::Block(root_block_header, root_block_id, root_block_body);
    Ok(ParseToken::new(kind, 0, 0))
}
