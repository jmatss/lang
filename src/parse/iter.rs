use super::{
    ast::BlockId,
    expr_parser::ExprParser,
    keyword_parser::KeyworkParser,
    token::{Argument, BlockHeader, Expression, ParseToken, TypeStruct},
    type_parser::TypeParser,
};
use crate::lex::token::{Keyword, LexToken, LexTokenKind, Symbol};
use crate::{common::iter::TokenIter, error::CustomError::ParseError};
use crate::{parse::ast::AST, CustomResult};

/// Max amount of chars to be put back into the iterator.
const MAX_PUT_BACK: usize = 10;

/// The common stop conditions used when parsing expressions.
pub const DEFAULT_STOP_CONDS: [Symbol; 5] = [
    Symbol::LineBreak,
    Symbol::SemiColon,
    Symbol::Comma,
    Symbol::CurlyBracketBegin,
    Symbol::CurlyBracketEnd,
];

// TODO: Move logic related to parsing of expression to new file.
// TODO: General clean-up of the functions used during parsing.
//       Split the two big (shunting-yard & polish) function to multiple smaller
//       functions
// TODO: Parse keywords.
// TODO: Logic for parsing ifBlocks and matchBlocks.

pub struct ParseTokenIter {
    /// Use to iterate over the LexTokens.
    iter: TokenIter<LexToken>,

    /// The abstract syntax tree that is being created.
    ast: AST,

    /// The ID of the current block. This ID is increment for every block and
    /// every block will be given a unique ID.
    block_id: BlockId,
}

impl ParseTokenIter {
    pub fn new(lex_tokens: Vec<LexToken>) -> Self {
        Self {
            iter: TokenIter::new(lex_tokens.into_iter()),
            ast: AST::new(),
            block_id: 0,
        }
    }

    /// Takes ownership of the current `block_id` so that it can be given to a
    /// block and updates the `self.block_id` to a new unique ID.
    pub fn reserve_block_id(&mut self) -> usize {
        let block_id = self.block_id;
        self.block_id += 1;
        block_id
    }

    /// Returns the next ParseToken from the iterator.
    pub fn next_token(&mut self) -> CustomResult<ParseToken> {
        if let Some(lex_token) = self.iter.next() {
            match lex_token.kind {
                LexTokenKind::Keyword(keyword) => self.parse_keyword(keyword),

                // Skip line breaks, white spaces and semi colons.
                // Call this function recursively to get an "actual" token.
                LexTokenKind::Symbol(Symbol::LineBreak)
                | LexTokenKind::Symbol(Symbol::WhiteSpace(_))
                | LexTokenKind::Symbol(Symbol::SemiColon) => self.next_token(),

                // All identifiers should be parsed as part of expressions since
                // all the identifiers(keywords) used in statements are lexed
                // as Keywords.
                LexTokenKind::Identifier(_)
                | LexTokenKind::Literal(_)
                | LexTokenKind::Symbol(_) => {
                    // Put back the token that was just popped and then parse
                    // everything together as an expression.
                    self.iter.put_back(lex_token);
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    Ok(ParseToken::Expression(expr))
                }

                LexTokenKind::EndOfFile => Ok(ParseToken::EndOfFile),

                // Unknown
                _ => Err(ParseError(format!(
                    "Received unknown token: {:?}.",
                    lex_token
                ))),
            }
        } else {
            Err(ParseError("Received None when parsing next token.".into()))
        }
    }

    /// Returns the next block containing all its ParseTokens. A block is always
    /// started withh "CurlyBracketBegin" and ended with "CurlyBracketEnd".
    pub fn next_block(&mut self, header: BlockHeader) -> CustomResult<ParseToken> {
        let block_tokens = Vec::new();
        let block_id = self.reserve_block_id();

        // Ensure that the block starts with a "CurlyBracketBegin".
        if let Some(lex_token) = self.next_skip_space_line() {
            if let LexTokenKind::Symbol(Symbol::CurlyBracketBegin) = lex_token.kind {
                // Do nothing, block start is OK.
            } else {
                return Err(ParseError(format!(
                    "Received invalid token at start of block: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError("Received None at start of block.".into()));
        }

        loop {
            let token = self.next_token()?;

            // If the next lex token is a "CurlyBracketEnd", the end of the
            // block have been reached. Break and return.
            if let Some(lex_token) = self.peek_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::CurlyBracketEnd) = lex_token.kind {
                    break;
                }
            }
        }

        Ok(ParseToken::Block(header, block_id, block_tokens))
    }

    pub fn parse_keyword(&mut self, keyword: Keyword) -> CustomResult<ParseToken> {
        KeyworkParser::parse(&self, keyword)
    }

    pub fn parse_expr(&mut self, stop_conds: &[Symbol]) -> CustomResult<Expression> {
        ExprParser::parse(&self, stop_conds)
    }

    pub fn parse_type(&mut self) -> CustomResult<TypeStruct> {
        TypeParser::parse(&self)
    }

    pub(super) fn parse_arg_list(&mut self) -> CustomResult<Vec<Argument>> {
        let mut arguments = Vec::new();

        // Skip the start parenthesis of the argument list.
        self.iter.skip(1);

        // Edge case if this argument list contains no items, do early return
        // with empty vector.
        if let Some(next) = self.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = next.kind {
                return Ok(arguments);
            }
        }

        loop {
            // TODO: This assumes that function arguments can NOT set a variable
            //       at the start of a expression, is this OK?
            // Parse the next identifier and then the token after that.
            // Depending on if this is a named argument or not, there are two
            // possibilites:
            //   1. <ident> =  // Named argument.
            //   2. <expr> ... // Un-named argument.
            // See if the first scenario is true. If it is not, assume that
            // the secound scenario is correct.
            let name = None;

            let first_opt = self.peek_skip_space_line();
            let second_opt = self.peek_skip_space_line();
            if let (Some(first), Some(second)) = (first_opt, second_opt) {
                if let LexTokenKind::Identifier(ident) = first.kind {
                    if let LexTokenKind::Symbol(Symbol::Equals) = second.kind {
                        // 1. Named argument.
                        // skip the ident and equals.
                        self.next_skip_space_line();
                        self.next_skip_space_line();

                        name = Some(ident);
                    }
                }

                let stop_conds = [Symbol::Comma, Symbol::ParenthesisEnd];
                let arg = Argument::new(name, self.parse_expr(&stop_conds)?);
                arguments.push(arg);
            } else {
                unreachable!();
            }

            // A argument has just been parsed above. The next character should
            // either be a comma indicating more arguments or a end parenthesis
            // indicating that the argument list have been parsed fully.
            if let Some(lex_token) = self.next_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::Comma) = lex_token.kind {
                    continue;
                } else if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = lex_token.kind {
                    return Ok(arguments);
                } else {
                    return Err(ParseError(format!(
                        "Received invalid LexToken at end of argument in arg list: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(ParseError(
                    "Received None at end of argument in arg list.".into(),
                ));
            }
        }
    }

    #[inline]
    pub fn put_back(&mut self, lex_token: LexToken) {
        self.iter.put_back(lex_token);
    }

    /// Gets the next item from the iterator that is NOT a white space.
    /// If the item at the current position of the iterator is a white space,
    /// it will be skipped and the item after that will be fetched.
    #[inline]
    pub fn next_skip_space(&mut self) -> Option<LexToken> {
        // Since the lexer parses all consecutive white spaces, this code only
        // needs to check for a white space onces, since there is no possiblity
        // that two tokens in a row are white spaces.
        if let Some(current) = self.iter.next() {
            if let LexTokenKind::Symbol(Symbol::WhiteSpace(_)) = current.kind {
                self.iter.next()
            } else {
                Some(current)
            }
        } else {
            None
        }
    }

    /// Gets the next item from the iterator that is NOT a white space or a
    /// line break. Will loop until a non white space/line break is found.
    #[inline]
    pub fn next_skip_space_line(&mut self) -> Option<LexToken> {
        while let Some(current) = self.iter.next() {
            match current.kind {
                LexTokenKind::Symbol(Symbol::WhiteSpace(_))
                | LexTokenKind::Symbol(Symbol::LineBreak) => (),
                _ => return Some(current),
            }
        }

        // The last token should always be a EOF (which isn't a white space or
        // line break) and this point should therefore NOT be reachable.
        unreachable!();
    }

    /// Peeks and clones the item ahead of the current position of the iterator
    /// that is NOT a white space.
    #[inline]
    pub fn peek_skip_space(&mut self) -> Option<LexToken> {
        // Since the lexer parses all consecutive white spaces, this code only
        // needs to check for a white space onces, since there is no possiblity
        // that two tokens in a row are white spaces.
        if let Some(tokens) = self.iter.peek_two() {
            if let LexTokenKind::Symbol(Symbol::WhiteSpace(_)) = tokens.0.kind {
                tokens.1
            } else {
                Some(tokens.0)
            }
        } else {
            None
        }
    }

    /// Peeks and clones the next item from the iterator that is NOT a
    /// white space or a line break. Will loop until a non white space/line break
    /// is found.
    #[inline]
    pub fn peek_skip_space_line(&mut self) -> Option<LexToken> {
        let i = 0;
        while let Some(current) = self.iter.peek_at_n(i) {
            match current.kind {
                LexTokenKind::Symbol(Symbol::WhiteSpace(_))
                | LexTokenKind::Symbol(Symbol::LineBreak) => (),
                _ => return Some(current),
            }
            i += 1;
        }

        // The last token should always be a EOF (which isn't a white space or
        // line break) and this point should therefore NOT be reachable.
        unreachable!();
    }
}
