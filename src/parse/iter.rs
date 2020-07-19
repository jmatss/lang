use super::{
    expr_parser::ExprParser,
    keyword_parser::KeyworkParser,
    token::{
        Argument, BlockHeader, BlockId, Expression, ParseToken, ParseTokenKind, TypeStruct,
        Variable,
    },
    type_parser::TypeParser,
};
use crate::lex::token::{Keyword, LexToken, LexTokenKind, Symbol};
use crate::CustomResult;
use crate::{
    common::iter::TokenIter,
    error::CustomError::{self, ParseError},
};

/// The common stop conditions used when parsing expressions.
pub const DEFAULT_STOP_CONDS: [Symbol; 5] = [
    Symbol::LineBreak,
    Symbol::SemiColon,
    Symbol::Comma,
    Symbol::CurlyBracketBegin,
    Symbol::CurlyBracketEnd,
];

// TODO: Clean up logic for storing line_nr and column_nr for parser.

pub struct ParseTokenIter {
    /// Use to iterate over the LexTokens.
    iter: TokenIter<LexToken>,

    /// The ID of the current block. This ID is increment for every block and
    /// every block will be given a unique ID.
    block_id: BlockId,

    /// Current line number (or rather last seen line number).
    pub cur_line_nr: u64,

    /// Current column number (or rather last seen column number).
    pub cur_column_nr: u64,
}

impl ParseTokenIter {
    pub fn new(lex_tokens: Vec<LexToken>) -> Self {
        Self {
            iter: TokenIter::new(lex_tokens.into_iter()),
            block_id: 0,
            cur_line_nr: 1,
            cur_column_nr: 1,
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
            // TODO: Clean up this mess with a mix of ParseToken/ParseTokenKind
            //       (some arms returns ParseTokens, others cascades ParseTokenKinds).

            self.cur_line_nr = lex_token.line_nr;
            self.cur_column_nr = lex_token.column_nr;

            let kind = match lex_token.kind {
                LexTokenKind::Keyword(keyword) => {
                    return self.parse_keyword(keyword, lex_token.line_nr, lex_token.column_nr);
                }

                // Skip line breaks, white spaces and semi colons.
                // Call this function recursively to get an "actual" token.
                LexTokenKind::Symbol(Symbol::LineBreak)
                | LexTokenKind::Symbol(Symbol::WhiteSpace(_))
                | LexTokenKind::Symbol(Symbol::SemiColon) => {
                    return self.next_token();
                }

                // All identifiers should be parsed as part of expressions since
                // all the identifiers(keywords) used in statements are lexed
                // as Keywords.
                LexTokenKind::Identifier(_)
                | LexTokenKind::Literal(_)
                | LexTokenKind::Symbol(_) => {
                    // Put back the token that was just popped and then parse
                    // everything together as an expression.
                    self.put_back(lex_token)?;
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    ParseTokenKind::Expression(expr)
                }

                LexTokenKind::EndOfFile => ParseTokenKind::EndOfFile,

                // Unknown
                _ => {
                    return Err(ParseError(format!(
                        "Received unknown token: {:?}.",
                        lex_token
                    )))
                }
            };

            Ok(ParseToken::new(kind, self.cur_line_nr, self.cur_column_nr))
        } else {
            Err(ParseError("Received None when parsing next token.".into()))
        }
    }

    /// Returns the next block containing all its ParseTokens. A block is always
    /// started withh "CurlyBracketBegin" and ended with "CurlyBracketEnd".
    pub fn next_block(&mut self, header: BlockHeader) -> CustomResult<ParseToken> {
        let mut block_tokens = Vec::new();
        let block_id = self.reserve_block_id();
        let line_nr: u64;
        let column_nr: u64;

        // Ensure that the block starts with a "CurlyBracketBegin".
        if let Some(lex_token) = self.next_skip_space_line() {
            if let LexTokenKind::Symbol(Symbol::CurlyBracketBegin) = lex_token.kind {
                line_nr = lex_token.line_nr;
                column_nr = lex_token.column_nr;
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
            block_tokens.push(token);

            // If the next lex token is a "CurlyBracketEnd", the end of the
            // block have been reached. Break and return.
            if let Some(lex_token) = self.peek_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::CurlyBracketEnd) = lex_token.kind {
                    self.next_skip_space_line(); // Consume "CurlyBracketEnd".
                    break;
                }
            }
        }

        Ok(ParseToken {
            kind: ParseTokenKind::Block(header, block_id, block_tokens),
            line_nr,
            column_nr,
        })
    }

    pub fn parse_keyword(
        &mut self,
        keyword: Keyword,
        line_nr: u64,
        column_nr: u64,
    ) -> CustomResult<ParseToken> {
        KeyworkParser::parse(self, keyword, line_nr, column_nr)
    }

    pub fn parse_expr(&mut self, stop_conds: &[Symbol]) -> CustomResult<Expression> {
        ExprParser::parse(self, stop_conds)
    }

    pub fn parse_type(&mut self) -> CustomResult<TypeStruct> {
        TypeParser::parse(self)
    }

    /// Parses a list of argument.
    ///   "( [ [<ident> =] <expr> [,]] ... )"
    pub fn parse_arg_list(&mut self) -> CustomResult<Vec<Argument>> {
        let mut arguments = Vec::new();

        // Skip the start parenthesis of the argument list.
        self.iter.skip(1);

        // Edge case if this argument list contains no items, do early return
        // with empty vector.
        if let Some(next) = self.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = next.kind {
                self.next_skip_space_line(); // Consume end parenthesis.
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
            let mut name = None;

            let first_opt = self.peek_skip_space_line();
            let second_opt = self.peek_skip_space_line();
            if let (Some(first), Some(second)) = (first_opt, second_opt) {
                println!("111");
                if let LexTokenKind::Identifier(ident) = first.kind {
                    println!("222");
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
                    println!("PAR END");
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

    /// Parses a list of parameters.
    ///   "( [ <ident> : <type> [,]] ... )"
    pub fn parse_par_list(&mut self) -> CustomResult<Vec<Variable>> {
        let mut parameters = Vec::new();

        // Skip the start parenthesis of the parameter list.
        self.iter.skip(1);

        // Edge case if this parameter list contains no items, do early return
        // with empty vector.
        if let Some(next) = self.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = next.kind {
                return Ok(parameters);
            }
        }

        loop {
            // Parse the name of this specific parameter.
            let ident = if let Some(lex_token) = self.next_skip_space_line() {
                if let LexTokenKind::Identifier(ident) = lex_token.kind {
                    ident
                } else {
                    return Err(ParseError(format!(
                        "Invalid token when parsing ident in par list: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(ParseError(
                    "Received None when parsing ident in par list.".into(),
                ));
            };

            // Next token should be a "Colon", consume it and return error if it
            // isn't a "Colon".
            if let Some(lex_token) = self.next_skip_space() {
                if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                    // Colon already consumed, nothing to do here.
                } else {
                    return Err(ParseError(format!(
                        "Invalid token when parsing colon in par list: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(ParseError(
                    "Received None expecting colon in par list.".into(),
                ));
            }

            let var_type = self.parse_type()?;
            let parameter = Variable::new(ident, Some(var_type), None, false);

            parameters.push(parameter);

            // A parameter has just been parsed above. The next character should
            // either be a comma indicating more parameters or a end parenthesis
            // indicating that the parameter list have been parsed fully.
            if let Some(lex_token) = self.next_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::Comma) = lex_token.kind {
                    continue;
                } else if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = lex_token.kind {
                    return Ok(parameters);
                } else {
                    return Err(ParseError(format!(
                        "Received invalid LexToken at end of parameter in par list: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(ParseError(
                    "Received None at end of parameter in par list.".into(),
                ));
            }
        }
    }

    #[inline]
    pub fn put_back(&mut self, lex_token: LexToken) -> CustomResult<()> {
        self.cur_line_nr = lex_token.line_nr;
        self.cur_column_nr = lex_token.column_nr;
        self.iter.put_back(lex_token)
    }

    /// Gets the next item from the iterator that is NOT a white space.
    /// If the item at the current position of the iterator is a white space,
    /// it will be skipped and the item after that will be fetched.
    #[inline]
    pub fn next_skip_space(&mut self) -> Option<LexToken> {
        // Since the lexer parses all consecutive white spaces, this code only
        // needs to check for a white space onces, since there is no possiblity
        // that two tokens in a row are white spaces.
        if let Some(lex_token) = self.iter.next() {
            if let LexTokenKind::Symbol(Symbol::WhiteSpace(_)) = lex_token.kind {
                if let Some(next_lex_token) = self.iter.next() {
                    self.cur_line_nr = next_lex_token.line_nr;
                    self.cur_column_nr = next_lex_token.column_nr;
                    Some(next_lex_token)
                } else {
                    None
                }
            } else {
                self.cur_line_nr = lex_token.line_nr;
                self.cur_column_nr = lex_token.column_nr;
                Some(lex_token)
            }
        } else {
            None
        }
    }

    /// Gets the next item from the iterator that is NOT a white space or a
    /// line break. Will loop until a non white space/line break is found.
    #[inline]
    pub fn next_skip_space_line(&mut self) -> Option<LexToken> {
        while let Some(lex_token) = self.iter.next() {
            match lex_token.kind {
                LexTokenKind::Symbol(Symbol::WhiteSpace(_))
                | LexTokenKind::Symbol(Symbol::LineBreak) => (),
                _ => {
                    self.cur_line_nr = lex_token.line_nr;
                    self.cur_column_nr = lex_token.column_nr;
                    return Some(lex_token);
                }
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
        let mut i = 0;
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

    // TODO: line nr and column nr are incorrect since they are just updated
    //       after a whole token have been parsed, not for every char.
    //       Try to change so that it gets updated when "common.iter.next"
    //       and "common.iter.putback" is called (need to keep track of it is
    //       a line break that is next/putback).
    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: &str) -> CustomError {
        CustomError::ParseError(format!(
            "{} ({}:{}).",
            msg, self.cur_line_nr, self.cur_column_nr
        ))
    }
}
