use super::{
    iter::{ParseTokenIter, DEFAULT_STOP_CONDS},
    token::{
        BlockHeader, Expression, Function, ParseToken, ParseTokenKind, Path, Statement, Variable,
    },
};
use crate::error::CustomError::ParseError;
use crate::{
    lex::token::{Keyword, LexTokenKind, Symbol},
    CustomResult,
};

pub struct KeyworkParser<'a> {
    iter: &'a mut ParseTokenIter,

    // TODO: Rework how line_nr and column_nr are stored/parsed during parsing.
    line_nr: u64,
    column_nr: u64,
}

impl<'a> KeyworkParser<'a> {
    pub fn parse(
        iter: &'a mut ParseTokenIter,
        keyword: Keyword,
        line_nr: u64,
        column_nr: u64,
    ) -> CustomResult<ParseToken> {
        let mut keyword_parser = Self {
            iter,
            line_nr,
            column_nr,
        };
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
            Keyword::Static => Err(ParseError("\"Static\" keyword not implemented.".into())),
            Keyword::Private => Err(ParseError("\"Private\" keyword not implemented.".into())),
            Keyword::Public => Err(ParseError("\"Public\" keyword not implemented.".into())),

            Keyword::Function => self.parse_func(),
            Keyword::Struct => Err(ParseError("\"Struct\" keyword not implemented.".into())),
            Keyword::Enum => Err(ParseError("\"Enum\" keyword not implemented.".into())),
            Keyword::Interface => Err(ParseError("\"Interface\" keyword not implemented.".into())),

            Keyword::Defer => Err(ParseError("\"Defer\" keyword not implemented.".into())),
            Keyword::With => Err(ParseError("\"With\" keyword not implemented.".into())),

            Keyword::Test => Err(ParseError("\"Test\" keyword not implemented.".into())),
        }
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

        let kind = ParseTokenKind::Block(BlockHeader::If, block_id, if_cases);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
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

        let kind = ParseTokenKind::Block(BlockHeader::Match(match_expr), block_id, match_cases);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a for loop block.
    ///   "for <var> in <expr> { ... }"
    /// The "for" keyword has already been consumed when this function is called.
    fn parse_for(&mut self) -> CustomResult<ParseToken> {
        let var = self.parse_var()?;
        let var = if let ParseTokenKind::Expression(Expression::Variable(var)) = var.kind {
            var
        } else {
            return Err(ParseError(
                "Received invalid var when looking at \"for\".".into(),
            ));
        };

        // Ensure that the next token is a "In".
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::In) = lex_token.kind {
                // Do nothing, everything OK.
            } else {
                return Err(ParseError(format!(
                    "Expected \"In\" after for, got: {:?}.",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError(
                "Received None when looking after \"for\"s In symbol.".into(),
            ));
        }

        // TODO: Can probably just parse this manual in order: <var>, "in", <expr>.
        // The "for" expression should be a binary "In" expression:
        //   "for <var> in <expr> {"
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;

        let header = BlockHeader::For(var, expr);
        self.iter.next_block(header)
    }

    /// Parses a while loop block.
    ///   "while <expr> { ... }"
    ///   "while { ... }"
    /// The "while" keyword has already been consumed when this function is called.
    fn parse_while(&mut self) -> CustomResult<ParseToken> {
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

        let kind = ParseTokenKind::Statement(Statement::Return(expr));
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a yield statement.
    ///   "yield <expr>"
    /// The "yield" keyword has already been consumed when this function is called.
    fn parse_yield(&mut self) -> CustomResult<ParseToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        let kind = ParseTokenKind::Statement(Statement::Yield(expr));
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a break statement.
    ///   "break"
    /// The "break" keyword has already been consumed when this function is called.
    fn parse_break(&mut self) -> CustomResult<ParseToken> {
        let kind = ParseTokenKind::Statement(Statement::Break);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a continue statement.
    ///   "continue"
    /// The "continue" keyword has already been consumed when this function is called.
    fn parse_continue(&mut self) -> CustomResult<ParseToken> {
        let kind = ParseTokenKind::Statement(Statement::Continue);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a use statement.
    ///   "use <path>"  (where path is a dot separated list of idents)
    /// The "use" keyword has already been consumed when this function is called.
    fn parse_use(&mut self) -> CustomResult<ParseToken> {
        let mut path_parts = Vec::new();

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
        let kind = ParseTokenKind::Statement(Statement::Use(path));
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a package statement.
    ///   "package <path>"  (where path is a dot separated list of idents)
    /// The "package" keyword has already been consumed when this function is called.
    fn parse_package(&mut self) -> CustomResult<ParseToken> {
        let mut path_parts = Vec::new();

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
        let kind = ParseTokenKind::Statement(Statement::Package(path));
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a var statement.
    ///   "var <ident> [: <type>] [= <expr>]"
    /// The "var" keyword has already been consumed when this function is called.
    fn parse_var(&mut self) -> CustomResult<ParseToken> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Identifier(ident) = lex_token.kind {
                ident
            } else {
                return Err(ParseError(format!(
                    "Expected ident when parsing \"var\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"var\".".into(),
            ));
        };

        // If the next token is a "Colon", parse the type.
        let var_type = if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                self.iter.next_skip_space(); // Consume "Colon".
                Some(self.iter.parse_type()?)
            } else {
                None
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"var <ident>\".".into(),
            ));
        };

        // If the next token is a "Equals" this is an initializer, parse the
        // next expression which will be the assigned value.
        let expr_opt = if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Equals) = lex_token.kind {
                self.iter.next_skip_space(); // Consume "Equals".
                Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
            } else {
                None
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"var <ident> [: <type>]\".".into(),
            ));
        };

        // If `expr` is Some (i.e. this is initialization), a assignment needs
        // to be added into the ParseToken and returned as well.
        let variable = Variable::new(ident, var_type, None, false);
        let var_decl = Statement::VariableDecl(variable, expr_opt);
        let kind = ParseTokenKind::Statement(var_decl);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    /// Parses a const statement.
    ///   "const <ident> : <type> = <expr>"
    /// The "const" keyword has already been consumed when this function is called.
    fn parse_const(&mut self) -> CustomResult<ParseToken> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Identifier(ident) = lex_token.kind {
                ident
            } else {
                return Err(ParseError(format!(
                    "Expected ident when parsing \"const\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"const\".".into(),
            ));
        };

        // The the next token should be a "Colon", parse the type.
        let var_type = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                self.iter.parse_type()?
            } else {
                return Err(ParseError(format!(
                    "Expected type when parsing \"const\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"const <ident>\".".into(),
            ));
        };

        // The the next token should be a "Equals" (assignment), parse the expr.
        let expr = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Equals) = lex_token.kind {
                self.iter.parse_expr(&DEFAULT_STOP_CONDS)?
            } else {
                return Err(ParseError(format!(
                    "Expected expressing when parsing \"const\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError(
                "Received None when looking at \"const\" assigned value.".into(),
            ));
        };

        let variable = Variable::new(ident, Some(var_type), None, true);
        let var_decl = Statement::VariableDecl(variable, Some(expr));
        let kind = ParseTokenKind::Statement(var_decl);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }

    // TODO: Parsing of generics.
    /// Parses a function.
    ///   "function <ident> ( [<ident>: <type>], ... ) [ <type> ] {"
    ///   TODO: "function <ident> [ < <generic>, ... > ] ( [<ident>: <type>], ... ) [ <type> ] {"
    /// The "function" keyword has already been consumed when this function is called.
    fn parse_func(&mut self) -> CustomResult<ParseToken> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Identifier(ident) = lex_token.kind {
                ident
            } else {
                return Err(ParseError(format!(
                    "Expected ident when parsing \"function\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"function\".".into(),
            ));
        };

        let parameters = self.iter.parse_par_list()?;

        // If the next token is a "CurlyBracketBegin", assume that the function
        // returns "void"; otherwise parse the return type.
        let return_type = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Symbol(Symbol::CurlyBracketBegin) = lex_token.kind {
                None
            } else {
                Some(self.iter.parse_type()?)
            }
        } else {
            return Err(ParseError(
                "Received None when looking at token after \"function <ident>\".".into(),
            ));
        };

        let parameters_opt = if !parameters.is_empty() {
            Some(parameters)
        } else {
            None
        };
        // TODO: Generics.
        let generics = None;
        let func = Function::new(ident, generics, parameters_opt, return_type);
        let func_header = BlockHeader::Function(func);

        self.iter.next_block(func_header)
    }
}
