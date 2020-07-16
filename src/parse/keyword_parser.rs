use super::{
    iter::{ParseTokenIter, DEFAULT_STOP_CONDS},
    token::{BinaryOperator, BlockHeader, Expression, Operation, ParseToken, Path, Statement},
};
use crate::error::CustomError::ParseError;
use crate::{
    lex::token::{Keyword, LexTokenKind, Symbol},
    CustomResult,
};

pub struct KeyworkParser<'a> {
    iter: &'a ParseTokenIter,
}

impl<'a> KeyworkParser<'a> {
    pub fn parse(iter: &'a ParseTokenIter, keyword: Keyword) -> CustomResult<ParseToken> {
        let keyword_parser = Self { iter };
        keyword_parser.parse_keyword(keyword)
    }

    fn parse_keyword(&mut self, keyword: Keyword) -> CustomResult<ParseToken> {
        match keyword {
            // Parses all the else(x)/else blocks after aswell.
            Keyword::If => self.parse_if(),
            Keyword::Else => Err(ParseError("Else keyword in keyword parser.".into())),

            Keyword::Match => self.parse_match(),

            Keyword::For => self.parse_for(),
            Keyword::While => self.parse_while(),

            Keyword::Return => self.parse_return(),
            Keyword::Yield => self.parse_yield(),
            Keyword::Break => self.parse_break(),
            Keyword::Continue => self.parse_continue(),

            Keyword::Use => self.parse_use(),
            Keyword::Package => self.parse_package(),

            Keyword::Var => self.parse_var(),
            Keyword::Const => self.parse_const(),
            Keyword::Static => (),
            Keyword::Private => (),
            Keyword::Public => (),

            Keyword::Function => (),
            Keyword::Struct => (),
            Keyword::Enum => (),
            Keyword::Interface => (),

            Keyword::Defer => (),
            Keyword::With => (),

            Keyword::Test => (),
        }

        // TODO.
        Ok(ParseToken::EndOfFile)
    }

    /// Parses the matching `IfCase`s into a `If` block. This includes all "else"
    /// blocks aswell.
    ///   "if <expr> { ... } [ [ else <expr> { ... } ] else { ... } ]"
    /// The "if" keyword has already been consumed when this function is called.
    fn parse_if(&mut self) -> CustomResult<ParseToken> {
        let mut if_cases = Vec::new();
        let block_id = self.iter.reserve_block_id();

        loop {
            // If the next lex token is a "CurlyBracketBegin", no expression is
            // given after this "if case". Assume this is the ending "else".
            // Otherwise parse the expression.
            let expr = if let Some(lex_token) = self.iter.peek_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::CurlyBracketBegin) = lex_token.kind {
                    None
                } else {
                    Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
                }
            } else {
                return Err(ParseError(
                    "Received None when looking at \"if case\" expr.".into(),
                ));
            };
            let header = BlockHeader::IfCase(expr);

            let if_case = self.iter.next_block(header)?;
            if_cases.push(if_case);

            // See if the next token is the "else" keyword indicating that this
            // function should keep parsing the "if cases", otherwise it is
            // time to break and return.
            if let Some(lex_token) = self.iter.peek_skip_space_line() {
                if let LexTokenKind::Keyword(Keyword::Else) = lex_token.kind {
                    self.iter.next_skip_space_line(); // Skip the "else" keyword.
                    continue;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(ParseToken::Block(BlockHeader::If, block_id, if_cases))
    }

    /// Parses a `Match` block and all its cases.
    ///   "match <expr> { <expr> { ... } [...] }"
    /// The "match" keyword has already been consumed when this function is called.
    fn parse_match(&mut self) -> CustomResult<ParseToken> {
        let mut match_cases = Vec::new();
        let block_id = self.iter.reserve_block_id();

        let match_expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;

        loop {
            let case_expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
            let header = BlockHeader::MatchCase(case_expr);

            let match_case = self.iter.next_block(header)?;
            match_cases.push(match_case);

            // See if the next token is a "CurlyBracketEnd" one can assume that
            // it is the curly bracket matching the outer "match" statement.
            // Break in that case, otherwise keep parsing cases.
            if let Some(lex_token) = self.iter.peek_skip_space_line() {
                if let LexTokenKind::Symbol(Symbol::CurlyBracketEnd) = lex_token.kind {
                    self.iter.next_skip_space_line(); // Skip the "CurlyBracketEnd".
                    break;
                }
            }
        }

        Ok(ParseToken::Block(
            BlockHeader::Match(match_expr),
            block_id,
            match_cases,
        ))
    }

    /// Parses a for loop block.
    ///   "for <var> in <expr> { ... }"
    /// The "for" keyword has already been consumed when this function is called.
    fn parse_for(&mut self) -> CustomResult<ParseToken> {
        let block_id = self.iter.reserve_block_id();

        // TODO: Can probably just parse this manual in order: <var>, "in", <expr>.
        // The "for" expression should be a binary "In" expression:
        //   "for <var> in <expr> {"
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        let bin_op = if let Expression::Operation(Operation::BinaryOperation(bin_op)) = expr {
            if let BinaryOperator::In = bin_op.operator {
                bin_op
            } else {
                return Err(ParseError(
                    "Binary operator in for is not a \"In\": {}".into(),
                ));
            }
        } else {
            return Err(ParseError("Expression in for is not a \"In\": {}".into()));
        };

        let header = BlockHeader::For(bin_op);
        self.iter.next_block(header)
    }

    /// Parses a while loop block.
    ///   "while <expr> { ... }"
    ///   "while { ... }"
    /// The "while" keyword has already been consumed when this function is called.
    fn parse_while(&mut self) -> CustomResult<ParseToken> {
        let block_id = self.iter.reserve_block_id();

        // If the next lex token is a "CurlyBracketBegin", no expression is
        // given after this "while" keyword. Assume that it means a infinite
        // loop (equivalent to "while(true)").
        let expr = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Symbol(Symbol::CurlyBracketBegin) = lex_token.kind {
                None
            } else {
                Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
            }
        } else {
            return Err(ParseError(
                "Received None when looking at \"while\" expr.".into(),
            ));
        };

        let header = BlockHeader::While(expr);
        self.iter.next_block(header)
    }

    /// Parses a return statement.
    ///   "return <expr>"
    ///   "return"
    /// The "return" keyword has already been consumed when this function is called.
    fn parse_return(&mut self) -> CustomResult<ParseToken> {
        // If the next lex token is a "LineBreak", no expression is given after
        // this "return" keyword. Assume it is a return for a function with no
        // return value.
        let expr = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Symbol(Symbol::LineBreak) = lex_token.kind {
                None
            } else {
                Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
            }
        } else {
            None
        };

        Ok(ParseToken::Statement(Statement::Return(expr)))
    }

    /// Parses a yield statement.
    ///   "yield <expr>"
    /// The "yield" keyword has already been consumed when this function is called.
    fn parse_yield(&mut self) -> CustomResult<ParseToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        Ok(ParseToken::Statement(Statement::Yield(expr)))
    }

    /// Parses a break statement.
    ///   "break"
    /// The "break" keyword has already been consumed when this function is called.
    fn parse_break(&mut self) -> CustomResult<ParseToken> {
        Ok(ParseToken::Statement(Statement::Break))
    }

    /// Parses a continue statement.
    ///   "continue"
    /// The "continue" keyword has already been consumed when this function is called.
    fn parse_continue(&mut self) -> CustomResult<ParseToken> {
        Ok(ParseToken::Statement(Statement::Continue))
    }

    /// Parses a use statement.
    ///   "use <path>"  (where path is a dot separated list of idents)
    /// The "use" keyword has already been consumed when this function is called.
    fn parse_use(&mut self) -> CustomResult<ParseToken> {
        let path_parts = Vec::new();

        loop {
            // Get the ident from the current path part.
            if let Some(lex_token) = self.iter.next_skip_space() {
                if let LexTokenKind::Identifier(ident) = lex_token.kind {
                    path_parts.push(ident);
                } else {
                    return Err(ParseError(format!(
                        "Expected ident when parsing \"use\" path, got: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(ParseError(
                    "Received None when looking at \"use\" path.".into(),
                ));
            }

            // The next symbol should either be a dot which indicates that the
            // path continues; or a line break which indicates that the path
            // has been ended.
            if let Some(lex_token) = self.iter.peek_skip_space() {
                if let LexTokenKind::Symbol(Symbol::Dot) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "Dot".
                    continue;
                } else if let LexTokenKind::Symbol(Symbol::LineBreak) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "LineBreak".
                    break;
                } else {
                    return Err(ParseError(
                        "Received None when looking at \"use\" path.".into(),
                    ));
                }
            } else {
                return Err(ParseError(
                    "Received None when looking at separator in \"use\" path.".into(),
                ));
            }
        }

        let path = Path::new(path_parts);
        Ok(ParseToken::Statement(Statement::Use(path)))
    }

    /// Parses a package statement.
    ///   "package <path>"  (where path is a dot separated list of idents)
    /// The "package" keyword has already been consumed when this function is called.
    fn parse_package(&mut self) -> CustomResult<ParseToken> {
        let path_parts = Vec::new();

        loop {
            // Get the ident from the current path part.
            if let Some(lex_token) = self.iter.next_skip_space() {
                if let LexTokenKind::Identifier(ident) = lex_token.kind {
                    path_parts.push(ident);
                } else {
                    return Err(ParseError(format!(
                        "Expected ident when parsing \"package\" path, got: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(ParseError(
                    "Received None when looking at \"package\" path.".into(),
                ));
            }

            // The next symbol should either be a dot which indicates that the
            // path continues; or a line break which indicates that the path
            // has been ended.
            if let Some(lex_token) = self.iter.peek_skip_space() {
                if let LexTokenKind::Symbol(Symbol::Dot) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "Dot".
                    continue;
                } else if let LexTokenKind::Symbol(Symbol::LineBreak) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "LineBreak".
                    break;
                } else {
                    return Err(ParseError(
                        "Received None when looking at \"package\" path.".into(),
                    ));
                }
            } else {
                return Err(ParseError(
                    "Received None when looking at separator in \"package\" path.".into(),
                ));
            }
        }

        let path = Path::new(path_parts);
        Ok(ParseToken::Statement(Statement::Package(path)))
    }
}
