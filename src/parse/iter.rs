use super::{
    expr_parser::ExprParser,
    keyword_parser::KeyworkParser,
    token::{
        Argument, BlockHeader, BlockId, Expression, ParseToken, ParseTokenKind, Statement,
        TypeStruct, Variable,
    },
    type_parser::TypeParser,
};
use crate::error::CustomError::CodeGenError;
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

                // If a "line" starts with a identifier this can either be a
                // assignment to this identifier (assignment aren't treated as
                // expression atm) or it will be an expression.
                LexTokenKind::Identifier(ref ident) => {
                    // If true: This is a assignment, the left hand side will
                    //          already have been parsed. Just need to parse the
                    //          right hand side (and also consume assign symbol).
                    // Else: This is a expression, put back the identifier and
                    //       then parse it as if it is a start of a expression.
                    if let Some(var) = self.parse_ident(ident)? {
                        if let Some(next_lex_token) = self.next_skip_space() {
                            if let Some(assign_op) = ParseToken::get_if_stmt_op(&next_lex_token) {
                                let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                                let stmt = Statement::Assignment(assign_op, var, expr);
                                ParseTokenKind::Statement(stmt)
                            } else {
                                return Err(CodeGenError(format!(
                                    "Expected assign operator when parsing ident, got: {:?}",
                                    &lex_token
                                )));
                            }
                        } else {
                            unreachable!("No operator after ident.");
                        }
                    } else {
                        self.put_back(lex_token)?;
                        let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                        ParseTokenKind::Expression(expr)
                    }
                }

                // If a literal or symbol is found, one can assume that they
                // belong to a expression. There is a possibility that they are
                // part of a statement, but then they should never end up here.
                LexTokenKind::Literal(_) | LexTokenKind::Symbol(_) => {
                    // Put back the token that was just popped and then parse
                    // everything together as an expression.
                    self.put_back(lex_token)?;
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    ParseTokenKind::Expression(expr)
                }

                LexTokenKind::EndOfFile => ParseTokenKind::EndOfFile,
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

    // Parses a identifier that either starts a assignment or a expressions.
    // If the next lex token is a assignment symbol or a colon(type specification),
    // it is a stmt, otherwise expr.
    fn parse_ident(&mut self, ident: &str) -> CustomResult<Option<Variable>> {
        if let Some(next_lex_token) = self.peek_skip_space() {
            // If this is a assignment, this function will return a variable
            // wrapped in Some. Otherwise it will be set to None to indicate
            // that this identifier is part of a expression.
            Ok(match next_lex_token.kind {
                LexTokenKind::Symbol(Symbol::Colon) => Some(self.parse_var(ident)?),
                LexTokenKind::Symbol(_) => {
                    if let Some(_assign_op) = ParseToken::get_if_stmt_op(&next_lex_token) {
                        Some(self.parse_var(ident)?)
                    } else {
                        None
                    }
                }
                _ => None,
            })
        } else {
            Err(CodeGenError(
                "next_lex_token None when parsing ident.".into(),
            ))
        }
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

    /// Parses a variable and any type that it might have specified after which
    /// would indicate a declaration.
    pub fn parse_var(&mut self, ident: &str) -> CustomResult<Variable> {
        // If the next token is a colon, a type is specified after this variable.
        // Parse it and set the `var_type` inside the variable.
        let var_type = if let Some(next_token) = self.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Colon) = next_token.kind {
                self.next_skip_space(); // Skip the colon.
                Some(self.parse_type()?)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Variable::new(ident.into(), var_type, None, false))
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

    /// Parses a list of parameters.
    ///   "( [ <ident> : <type> [,]] [...] )"
    /// Using a "..." as a argument indicates that this function support
    /// var_args/is variadic.
    /// The returned bool in the tuple indicates if this is a variadic func.
    pub fn parse_par_list(&mut self) -> CustomResult<(Vec<Variable>, bool)> {
        let mut parameters = Vec::new();
        let mut is_var_arg = false;

        // Skip the start parenthesis of the parameter list.
        self.next_skip_space_line();

        // Edge case if this parameter list contains no items, do early return
        // with empty vector.
        if let Some(next) = self.peek_skip_space_line() {
            if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = next.kind {
                // Consume the end parenthesis of the parameter list.
                self.next_skip_space_line();
                return Ok((parameters, is_var_arg));
            }
        }

        loop {
            if let Some(lex_token) = self.next_skip_space_line() {
                // Parses either a identifier followed by a type or a "TipleDot"
                // which is the indicator for a variadic function.
                match lex_token.kind {
                    LexTokenKind::Identifier(ident) => {
                        let var_type = self.parse_par_list_type()?;
                        let const_ = false;
                        let parameter = Variable::new(ident, Some(var_type), None, const_);

                        parameters.push(parameter);
                    }

                    LexTokenKind::Symbol(Symbol::TripleDot) => is_var_arg = true,

                    _ => {
                        return Err(ParseError(format!(
                            "Invalid token when parsing ident in par list: {:?}",
                            lex_token
                        )))
                    }
                }
            } else {
                return Err(ParseError(
                    "Received None when parsing ident in par list.".into(),
                ));
            };

            // A parameter has just been parsed above. The next character should
            // either be a comma indicating more parameters or a end parenthesis
            // indicating that the parameter list have been parsed fully.
            if let Some(lex_token) = self.next_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::Comma) = lex_token.kind {
                    continue;
                } else if let LexTokenKind::Symbol(Symbol::ParenthesisEnd) = lex_token.kind {
                    return Ok((parameters, is_var_arg));
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

    /// Parses the "type" part of a parameter in a parameter list. The next
    /// token should be a "Colon", consume it or return error if it isn't a
    /// "Colon".
    fn parse_par_list_type(&mut self) -> CustomResult<TypeStruct> {
        if let Some(lex_token) = self.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                self.parse_type()
            } else {
                Err(ParseError(format!(
                    "Invalid token when parsing colon in par list: {:?}",
                    lex_token
                )))
            }
        } else {
            Err(ParseError(
                "Received None expecting colon in par list.".into(),
            ))
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
