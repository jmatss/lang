use crate::{
    parser::{ParseTokenIter, DEFAULT_STOP_CONDS, KEYWORD_STOP_CONDS},
    token::get_modifier_token,
    type_parser::TypeParser,
};
use common::{
    error::CustomResult,
    file::FilePosition,
    token::{
        ast::AstToken,
        block::{BlockHeader, Enum, Function, Struct},
        expr::Expr,
        lit::Lit,
        stmt::{Modifier, Path, Stmt},
    },
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
    },
    type_info::TypeInfo,
};
use lex::token::{Kw, LexTokenKind, Sym};
use std::{cell::RefCell, rc::Rc};

pub struct KeyworkParser<'a, 'b> {
    iter: &'a mut ParseTokenIter<'b>,
}

impl<'a, 'b> KeyworkParser<'a, 'b> {
    pub fn parse(
        iter: &'a mut ParseTokenIter<'b>,
        keyword: Kw,
        kw_file_pos: FilePosition,
    ) -> CustomResult<AstToken> {
        let mut keyword_parser = Self { iter };
        keyword_parser.parse_keyword(keyword, kw_file_pos)
    }

    fn parse_keyword(&mut self, keyword: Kw, kw_file_pos: FilePosition) -> CustomResult<AstToken> {
        match keyword {
            // Parses all the else(x)/else blocks after aswell.
            Kw::If => self.parse_if(kw_file_pos),
            Kw::Else => Err(self
                .iter
                .err("Else keyword in keyword parser.".into(), Some(kw_file_pos))),
            Kw::Match => self.parse_match(kw_file_pos),

            // Blocks returns AstTokens instead of Token, so need to do early return.
            Kw::For => self.parse_for(kw_file_pos),
            Kw::While => self.parse_while(kw_file_pos),
            Kw::Implement => self.parse_impl(kw_file_pos),
            Kw::Function => self.parse_func(kw_file_pos),

            Kw::Return => self.parse_return(kw_file_pos),
            Kw::Yield => self.parse_yield(kw_file_pos),
            Kw::Break => self.parse_break(kw_file_pos),
            Kw::Continue => self.parse_continue(kw_file_pos),

            Kw::Use => self.parse_use(kw_file_pos),
            Kw::Package => self.parse_package(kw_file_pos),
            Kw::External => self.parse_external(kw_file_pos),

            Kw::Var => self.parse_var_decl(kw_file_pos),
            Kw::Const => {
                // TODO: self.parse_const_decl()
                self.parse_var_decl(kw_file_pos)
            }
            Kw::Static => Err(self.iter.err(
                "\"Static\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),
            Kw::Private => Err(self.iter.err(
                "\"Private\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),
            Kw::Public => Err(self.iter.err(
                "\"Public\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),

            Kw::Struct => self.parse_struct(kw_file_pos),
            Kw::Enum => self.parse_enum(kw_file_pos),
            Kw::Interface => Err(self.iter.err(
                "\"Interface\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),

            Kw::Defer => self.parse_defer(kw_file_pos),

            Kw::Test => Err(self.iter.err(
                "\"Test\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),
        }
    }

    /// Parses the matching `IfCase`s into a `If` block. This includes all "else"
    /// blocks aswell.
    ///   "if <expr> { ... } [ [ else <expr> { ... } ] else { ... } ]"
    /// The "if" keyword has already been consumed when this function is called.
    fn parse_if(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        let mut if_cases = Vec::new();
        let block_id = self.iter.reserve_block_id();

        loop {
            // If the next lex token is a "CurlyBracketBegin", no expression is
            // given after this "if case". Assume this is the ending "else".
            // Otherwise parse the expression.
            let expr = self.iter.parse_expr_allow_empty(&KEYWORD_STOP_CONDS)?;
            if let Some(expr_file_pos) = expr.as_ref().map(|e| e.file_pos().to_owned()).flatten() {
                file_pos.set_end(expr_file_pos)?;
            }
            let header = BlockHeader::IfCase(expr);

            let if_case = self.iter.next_block(header)?;
            if let Some(block_file_pos) = if_case.file_pos() {
                file_pos.set_end(block_file_pos)?;
            }
            if_cases.push(if_case);

            // See if the next token is the "else" keyword indicating that this
            // function should keep parsing the "if cases", otherwise it is
            // time to break and return.
            if let Some((LexTokenKind::Kw(Kw::Else), token_file_pos)) = self
                .iter
                .peek_skip_space_line()
                .map(|token| (token.kind, token.file_pos))
            {
                // Skip the "else" keyword.
                self.iter.next_skip_space_line();

                file_pos.set_end(&token_file_pos)?;
            } else {
                break;
            }
        }

        Ok(AstToken::Block(
            BlockHeader::If,
            file_pos,
            block_id,
            if_cases,
        ))
    }

    /// Parses a `Match` block and all its cases.
    ///   "match <expr> { <expr> { ... } [...] }"
    /// The "match" keyword has already been consumed when this function is called.
    fn parse_match(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        let mut match_cases = Vec::new();
        let block_id = self.iter.reserve_block_id();

        let match_expr = self.iter.parse_expr(&KEYWORD_STOP_CONDS)?;

        if let Some(expr_file_pos) = match_expr.file_pos() {
            file_pos.set_end(expr_file_pos)?;
        } else {
            unreachable!();
        }

        // Ensure that the next token is a CurlyBracketBegin.
        // If it isn't, something has gone wrong, return error.
        let peek_token = self.iter.next_skip_space_line();
        if let Some(LexTokenKind::Sym(Sym::CurlyBracketBegin)) =
            peek_token.as_ref().map(|t| &t.kind)
        {
        } else {
            return Err(self.iter.err(
                format!(
                    "Expected CurlyBracketBegin after match expr, was: {:#?}",
                    peek_token
                ),
                peek_token.map(|token| token.file_pos),
            ));
        }

        loop {
            // See if the next token is a "CurlyBracketEnd" one can assume that
            // it is the curly bracket matching the outer "match" statement.
            // Break in that case, otherwise keep parsing cases.
            if let Some(lex_token) = self.iter.peek_skip_space_line() {
                let case_expr = match lex_token.kind {
                    LexTokenKind::Sym(Sym::CurlyBracketEnd) => {
                        self.iter.next_skip_space_line(); // Skip the "CurlyBracketEnd".

                        // This is the only happy path break from the loop.
                        file_pos.set_end(&lex_token.file_pos)?;
                        break;
                    }

                    // If this is the default block, return None as the expression.
                    // That will be the indication that this is the default block.
                    LexTokenKind::Ident(ref ident) if ident == "default" => {
                        self.iter.next_skip_space_line(); // Skip the "ident".
                        None
                    }

                    // A regular match block.
                    _ => Some(self.iter.parse_expr(&KEYWORD_STOP_CONDS)?),
                };

                let header = BlockHeader::MatchCase(case_expr);
                let match_case = self.iter.next_block(header)?;

                match_cases.push(match_case);
            } else {
                return Err(self.iter.err(
                    "Got None when parsing match cases.".into(),
                    peek_token.map(|token| token.file_pos),
                ));
            }
        }

        if !match_cases.is_empty() {
            Ok(AstToken::Block(
                BlockHeader::Match(match_expr),
                file_pos,
                block_id,
                match_cases,
            ))
        } else {
            Err(self.iter.err(
                format!(
                    "Parsed match block with no cases. Match expr: {:#?}",
                    match_expr
                ),
                Some(file_pos),
            ))
        }
    }

    /// Parses a for loop block.
    ///   "for <var> in <expr> { ... }"
    /// The "for" keyword has already been consumed when this function is called.
    fn parse_for(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        let lex_token = self.iter.next_skip_space_line();
        let ident =
            if let Some(LexTokenKind::Ident(ident)) = lex_token.as_ref().map(|token| &token.kind) {
                ident.clone()
            } else {
                return Err(self.iter.err(
                    format!("Not ident when parsing \"for\" variable: {:?}", lex_token),
                    lex_token.map(|t| t.file_pos),
                ));
            };

        let parse_type = true;
        let parse_value = false;
        let is_const = false;
        let var = self
            .iter
            .parse_var(&ident, parse_type, parse_value, is_const, None, file_pos)?;

        if let Some(var_file_pos) = &var.file_pos {
            file_pos.set_end(var_file_pos)?;
        }

        // Ensure that the next token is a "In".
        let lex_token = self.iter.next_skip_space();
        if let Some(LexTokenKind::Sym(Sym::In)) = lex_token.as_ref().map(|token| &token.kind) {
            // Do nothing, everything OK.
        } else {
            return Err(self.iter.err(
                format!(
                    "Expected \"In\" after \"for\" keyword, got: {:?}.",
                    lex_token
                ),
                lex_token.map(|token| token.file_pos),
            ));
        }

        let expr = self.iter.parse_expr(&KEYWORD_STOP_CONDS)?;

        if let Some(expr_file_pos) = expr.file_pos() {
            file_pos.set_end(expr_file_pos)?;
        } else {
            unreachable!();
        }

        let header = BlockHeader::For(var, expr);
        self.iter.next_block(header)
    }

    /// Parses a while loop block.
    ///   "while <expr> { ... }"
    ///   "while { ... }"
    /// The "while" keyword has already been consumed when this function is called.
    fn parse_while(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        // TODO: Should the `file_pos` be used it here or can the param be removed?

        // If the next lex token is a "CurlyBracketBegin", no expression is
        // given after this "while" keyword. Assume that it means a infinite
        // loop (equivalent to "while(true)").
        let expr = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Sym(Sym::CurlyBracketBegin) = lex_token.kind {
                None
            } else {
                Some(self.iter.parse_expr(&KEYWORD_STOP_CONDS)?)
            }
        } else {
            return Err(self.iter.err(
                "Received None when looking at \"while\" expr.".into(),
                Some(file_pos),
            ));
        };

        let header = BlockHeader::While(expr);
        self.iter.next_block(header)
    }

    /// Parses a return statement.
    ///   "return <expr>"
    ///   "return"
    /// The "return" keyword has already been consumed when this function is called.
    fn parse_return(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        // If the next lex token is a "LineBreak", no expression is given after
        // this "return" keyword. Assume it is a return for a function with no
        // return value.
        let expr = if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Sym(Sym::LineBreak) = lex_token.kind {
                None
            } else {
                Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
            }
        } else {
            None
        };

        if let Some(expr_file_pos) = expr.as_ref().map(|e| e.file_pos()).flatten() {
            file_pos.set_end(expr_file_pos)?;
        }

        Ok(AstToken::Stmt(Stmt::Return(expr, Some(file_pos))))
    }

    /// Parses a yield statement.
    ///   "yield <expr>"
    /// The "yield" keyword has already been consumed when this function is called.
    fn parse_yield(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;

        if let Some(expr_file_pos) = expr.file_pos() {
            file_pos.set_end(expr_file_pos)?;
        } else {
            unreachable!();
        }

        Ok(AstToken::Stmt(Stmt::Yield(expr, Some(file_pos))))
    }

    /// Parses a break statement.
    ///   "break"
    /// The "break" keyword has already been consumed when this function is called.
    fn parse_break(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        Ok(AstToken::Stmt(Stmt::Break(Some(file_pos))))
    }

    /// Parses a continue statement.
    ///   "continue"
    /// The "continue" keyword has already been consumed when this function is called.
    fn parse_continue(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        Ok(AstToken::Stmt(Stmt::Continue(Some(file_pos))))
    }

    /// Parses a use statement.
    ///   "use <path>"  (where path is a dot separated list of idents)
    /// The "use" keyword has already been consumed when this function is called.
    fn parse_use(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        let mut path_parts = Vec::new();

        loop {
            // Get the ident from the current path part.
            let lex_token = self.iter.next_skip_space();
            if let Some(LexTokenKind::Ident(ident)) = lex_token.as_ref().map(|token| &token.kind) {
                path_parts.push(ident.clone());
            } else {
                return Err(self.iter.err(
                    format!(
                        "Expected ident when parsing \"use\" path, got: {:?}",
                        lex_token
                    ),
                    lex_token.map(|t| t.file_pos),
                ));
            }

            // The next symbol should either be a dot which indicates that the
            // path continues; or a line break which indicates that the path
            // has been ended.
            if let Some(lex_token) = self.iter.peek_skip_space() {
                if let LexTokenKind::Sym(Sym::Dot) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "Dot".
                    continue;
                } else if let LexTokenKind::Sym(Sym::LineBreak) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "LineBreak".

                    // The only happy path that exists the loop.
                    file_pos.set_end(&lex_token.file_pos)?;
                    break;
                } else {
                    return Err(self.iter.err(
                        "Received None when looking at \"use\" path.".into(),
                        Some(lex_token.file_pos),
                    ));
                }
            } else {
                return Err(self.iter.err(
                    "Received None when looking at separator in \"use\" path.".into(),
                    Some(file_pos),
                ));
            }
        }

        Ok(AstToken::Stmt(Stmt::Use(Path::new(path_parts, file_pos))))
    }

    /// Parses a package statement.
    ///   "package <path>"  (where path is a dot separated list of idents)
    /// The "package" keyword has already been consumed when this function is called.
    fn parse_package(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        let mut path_parts = Vec::new();

        loop {
            // Get the ident from the current path part.
            let lex_token = self.iter.next_skip_space();
            if let Some(LexTokenKind::Ident(ident)) = lex_token.as_ref().map(|token| &token.kind) {
                path_parts.push(ident.clone());
            } else {
                return Err(self.iter.err(
                    format!(
                        "Expected ident when parsing \"package\" path, got: {:?}",
                        lex_token
                    ),
                    lex_token.map(|t| t.file_pos),
                ));
            }

            // The next symbol should either be a dot which indicates that the
            // path continues; or a line break which indicates that the path
            // has been ended.
            if let Some(lex_token) = self.iter.peek_skip_space() {
                if let LexTokenKind::Sym(Sym::Dot) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "Dot".
                    continue;
                } else if let LexTokenKind::Sym(Sym::LineBreak) = lex_token.kind {
                    self.iter.next_skip_space(); // Consume "LineBreak".

                    // The only happy path that exists the loop.
                    file_pos.set_end(&lex_token.file_pos)?;
                    break;
                } else {
                    return Err(self.iter.err(
                        "Received None when looking at \"package\" path.".into(),
                        Some(lex_token.file_pos),
                    ));
                }
            } else {
                return Err(self.iter.err(
                    "Received None when looking at separator in \"package\" path.".into(),
                    Some(file_pos),
                ));
            }
        }

        Ok(AstToken::Stmt(Stmt::Package(Path::new(
            path_parts, file_pos,
        ))))
    }

    // TODO: External only valid for functions atm, add for variables.
    /// Parses a external statement.
    ///   "external <function_prototype>"
    /// The "external" keyword has already been consumed when this function is called.
    fn parse_external(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        if let Some(lex_token) = self.iter.next_skip_space_line() {
            let func = match lex_token.kind {
                LexTokenKind::Kw(Kw::Function) => self.parse_func_proto(lex_token.file_pos)?,
                _ => {
                    return Err(self.iter.err(
                        format!("Invalid keyword after external keyword: {:?}", lex_token),
                        Some(lex_token.file_pos),
                    ));
                }
            };

            // TODO: This doesn't include the function prototype into the file_pos,
            //       only the extern keyword. The function prototype currently
            //       doesn't store/keep any information about its file_pos.
            //       How should this work?

            Ok(AstToken::Stmt(Stmt::ExternalDecl(
                Rc::new(RefCell::new(func)),
                Some(file_pos),
            )))
        } else {
            Err(self.iter.err(
                "Received None lex token after external keyword.".into(),
                Some(file_pos),
            ))
        }
    }

    /// Parses a var statement.
    ///   "var <ident> [: <type>] [= <expr>]"
    /// The "var" keyword has already been consumed when this function is called.
    fn parse_var_decl(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        // Start by parsing the identifier.
        let lex_token = self.iter.next_skip_space_line();
        let (ident, var_file_pos) = if let Some((LexTokenKind::Ident(ident), var_file_pos)) =
            lex_token
                .as_ref()
                .map(|token| (&token.kind, token.file_pos))
        {
            (ident.clone(), var_file_pos)
        } else {
            return Err(self.iter.err(
                format!("Not ident after parsing \"var\": {:?}", lex_token),
                lex_token.map(|t| t.file_pos),
            ));
        };

        file_pos.set_end(&var_file_pos)?;

        let parse_type = true;
        let parse_value = true;
        let is_const = false;
        let var = Rc::new(RefCell::new(self.iter.parse_var(
            &ident,
            parse_type,
            parse_value,
            is_const,
            None,
            var_file_pos,
        )?));

        if let Some(var_file_pos) = var.borrow().file_pos {
            file_pos.set_end(&var_file_pos)?;
        } else {
            unreachable!();
        }

        let var_decl = Stmt::VariableDecl(var, Some(file_pos));
        Ok(AstToken::Stmt(var_decl))
    }

    /*
    // TODO: Maybe don't force a type, let the type be infered (?).
    /// Parses a const statement.
    ///   "const <ident> : <type> = <expr>"
    /// The "const" keyword has already been consumed when this function is called.
    fn parse_const_decl(&mut self) -> CustomResult<ParseToken> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Identifier(ident) = lex_token.kind {
                ident
            } else {
                return Err(self.iter.err(format!(
                    "Expected ident when parsing \"const\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"const\".".into()));
        };

        // The the next token should be a "Colon", parse the type.
        let var_type = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                self.iter.parse_type()?
            } else {
                return Err(self.iter.err(format!(
                    "Expected type when parsing \"const\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"const <ident>\".".into()));
        };

        // The the next token should be a "Equals" (assignment), parse the expr.
        let expr = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Equals) = lex_token.kind {
                self.iter.parse_expr(&DEFAULT_STOP_CONDS)?
            } else {
                return Err(self.iter.err(format!(
                    "Expected expressing when parsing \"const\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at \"const\" assigned value.".into()));
        };

        let is_const = true;
        let variable = Variable::new(ident, Some(var_type), None, is_const);
        let var_decl = Statement::VariableDecl(variable, Some(expr));
        let kind = ParseTokenKind::Statement(var_decl);
        Ok(ParseToken::new(kind, self.line_nr, self.column_nr))
    }
    */

    /// Parses a function and its body. See `parse_func_proto` for the structure
    /// of a function header/prototype.
    /// The "function" keyword has already been consumed when this function is called.
    fn parse_func(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        let func = self.parse_func_proto(file_pos)?;
        let func_header = BlockHeader::Function(Rc::new(RefCell::new(func)));
        self.iter.next_block(func_header)
    }

    /// Parses a function prototype/header.
    ///   "function [ <modifier>... ] <ident> [ < <generic>, ... > ] ( [<ident>: <type>] [= <default value>], ... ) [ "->" <type> ]"
    /// The "function" keyword has already been consumed when this function is called.
    fn parse_func_proto(&mut self, file_pos: FilePosition) -> CustomResult<Function> {
        let mut modifiers = Vec::new();

        // TODO: Handle FilePosition.

        static THIS: &str = "this";

        // Start by parsing the modifiers and identifier. This will loop until
        // the identifier/name of the function is found.
        let ident = loop {
            if let Some(lex_token) = self.iter.next_skip_space_line() {
                match &lex_token.kind {
                    // function this
                    LexTokenKind::Ident(ident) if ident == THIS => {
                        modifiers.push(Modifier::This);
                    }

                    // function {this}
                    LexTokenKind::Sym(Sym::CurlyBracketBegin) => {
                        let lex_token = self.iter.next_skip_space_line();
                        if let Some(LexTokenKind::Ident(ident)) =
                            lex_token.as_ref().map(|l| &l.kind)
                        {
                            if ident != THIS {
                                return Err(self.iter.err(
                                    format!(
                                        "Expected \"this\" after CurlyBracketBegin in \"function\" header, got: {:?}",
                                        lex_token
                                    ),
                                    Some(lex_token.map(|token|token.file_pos)).flatten()
                                ));
                            }
                        } else {
                            return Err(self.iter.err(
                                format!(
                                    "Expected \"this\" after CurlyBracketBegin in \"function\" header, got: {:?}",
                                    lex_token
                                ),
                                Some(lex_token.map(|token|token.file_pos)).flatten()
                            ));
                        }

                        let lex_token = self.iter.next_skip_space_line();
                        if let Some(LexTokenKind::Sym(Sym::CurlyBracketEnd)) =
                            lex_token.as_ref().map(|l| &l.kind)
                        {
                        } else {
                            return Err(self.iter.err(
                                format!(
                                    "Expected CurlyBracketEnd after \"this\" in \"function\" header, got: {:?}",
                                    lex_token
                                ),
                                Some(lex_token.map(|token|token.file_pos)).flatten()
                            ));
                        }

                        modifiers.push(Modifier::ThisPointer);
                    }

                    LexTokenKind::Ident(ident) => {
                        break ident.clone();
                    }

                    LexTokenKind::Kw(lex_kw) => {
                        if let Some(modifier) = get_modifier_token(&lex_kw) {
                            modifiers.push(modifier);
                        } else {
                            return Err(self.iter.err(
                                format!(
                                    "Invalid keyword when parsing \"function\" header, got: {:?}",
                                    lex_token
                                ),
                                Some(lex_token.file_pos),
                            ));
                        }
                    }

                    _ => {
                        return Err(self.iter.err(
                            format!(
                            "Expected ident or keyword when parsing \"function\" header, got: {:?}",
                            lex_token
                        ),
                            Some(lex_token.file_pos),
                        ))
                    }
                }
            } else {
                return Err(self.iter.err(
                    "Received None when looking at tokens after \"function\".".into(),
                    Some(file_pos),
                ));
            }
        };

        let mut type_parser = TypeParser::new(self.iter, None);
        let (generics, ty_file_pos) = type_parser.parse_type_generics(GenericsKind::Decl)?;
        let generics = generics.iter_names().cloned().collect::<Vec<_>>();
        let generics_opt = if !generics.is_empty() {
            Some(generics)
        } else {
            None
        };

        let start_symbol = Sym::ParenthesisBegin;
        let end_symbol = Sym::ParenthesisEnd;
        let (params, is_var_arg, par_file_pos) =
            self.iter.parse_par_list(start_symbol, end_symbol, None)?;

        // Wrap the params into RC & RefCell.
        let params = params
            .iter()
            .map(|var| Rc::new(RefCell::new(var.clone())))
            .collect::<Vec<_>>();
        let params_opt = if !params.is_empty() {
            Some(params)
        } else {
            None
        };

        // If the next token is a "Arrow" ("->"), assume that the return type
        // of the function is specified afterwards. If there are no arrow,
        // assume that the function returns void.
        let (return_ty, ty_file_pos) = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Sym(Sym::Arrow) = lex_token.kind {
                self.iter.next_skip_space_line(); // Consume the arrow.

                let return_ty = self.iter.parse_type(None)?;
                let return_ty_file_pos = return_ty.file_pos().cloned();
                (Some(return_ty), return_ty_file_pos)
            } else {
                (None, None)
            }
        } else {
            return Err(self.iter.err(
                "Received None when looking at token after \"function <ident>\".".into(),
                Some(file_pos),
            ));
        };

        Ok(Function::new(
            ident,
            generics_opt,
            params_opt,
            return_ty,
            modifiers,
            is_var_arg,
        ))
    }

    /// Parses a struct header.
    ///   "struct <ident> [ < <generic>, ... > ] [{ [<ident>: <type>] [[,] ...] }]"
    /// The "struct" keyword has already been consumed when this function is called.
    fn parse_struct(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        // Start by parsing the identifier.
        let lex_token = self.iter.next_skip_space_line();
        let ident =
            if let Some(LexTokenKind::Ident(ident)) = lex_token.as_ref().map(|token| &token.kind) {
                ident.clone()
            } else {
                return Err(self.iter.err(
                    format!("Not ident after parsing \"struct\": {:?}", lex_token),
                    lex_token.map(|t| t.file_pos),
                ));
            };

        let mut type_parse = TypeParser::new(self.iter, None);
        let (generics, gens_file_pos) = type_parse.parse_type_generics(GenericsKind::Decl)?;

        if let Some(gens_file_pos) = gens_file_pos {
            file_pos.set_end(&gens_file_pos)?;
        }

        let generics = if !generics.is_empty() {
            Some(&generics)
        } else {
            None
        };

        // Parse the members of the struct. If the next token isn't a
        // "CurlyBracketBegin" symbol, assume this is a struct with no members.
        let (members, is_var_arg) = if let Some(LexTokenKind::Sym(Sym::CurlyBracketBegin)) =
            self.iter.peek_skip_space_line().map(|t| t.kind)
        {
            let start_symbol = Sym::CurlyBracketBegin;
            let end_symbol = Sym::CurlyBracketEnd;
            let (members, is_var_arg, par_file_pos) =
                self.iter
                    .parse_par_list(start_symbol, end_symbol, generics)?;

            file_pos.set_end(&par_file_pos)?;

            (members, is_var_arg)
        } else {
            (Vec::default(), false)
        };

        let members = members
            .iter()
            .map(|m| Rc::new(RefCell::new(m.clone())))
            .collect::<Vec<_>>();

        if is_var_arg {
            return Err(self.iter.err(
                format!(
                    "Found invalid var_arg symbol in struct with name: {}",
                    &ident
                ),
                Some(file_pos),
            ));
        }

        let members_opt = if !members.is_empty() {
            Some(members)
        } else {
            None
        };

        let generic_names = generics.map(|gens| gens.iter_names().cloned().collect::<Vec<_>>());
        let implements = None;
        let struct_ = Struct::new(ident, generic_names, implements, members_opt);
        let header = BlockHeader::Struct(Rc::new(RefCell::new(struct_)));

        let block_id = self.iter.reserve_block_id();
        let body = Vec::with_capacity(0);

        Ok(AstToken::Block(header, file_pos, block_id, body))
    }

    // TODO: Should it possible to set values to the individual enum variants?
    //       Should there be possible to set the integer type of the enum?
    /// Parses a enum header.
    ///   "enum <ident> { <ident> [[,] ...] }"
    /// The "enum" keyword has already been consumed when this function is called.
    fn parse_enum(&mut self, mut file_pos: FilePosition) -> CustomResult<AstToken> {
        // Start by parsing the identifier.
        let lex_token = self.iter.next_skip_space_line();
        let ident =
            if let Some(LexTokenKind::Ident(ident)) = lex_token.as_ref().map(|token| &token.kind) {
                ident.clone()
            } else {
                return Err(self.iter.err(
                    format!("Not ident after parsing \"enum\": {:?}", lex_token),
                    lex_token.map(|t| t.file_pos),
                ));
            };

        // Parse the members of the enum.
        let start_symbol = Sym::CurlyBracketBegin;
        let end_symbol = Sym::CurlyBracketEnd;
        let (mut members, is_var_arg, par_file_pos) =
            self.iter.parse_par_list(start_symbol, end_symbol, None)?;

        file_pos.set_end(&par_file_pos)?;

        if is_var_arg {
            return Err(self.iter.err(
                format!("Found invalid var_arg symbol in enum with name: {}", &ident),
                Some(file_pos),
            ));
        }

        const RADIX: u32 = 10;
        let mut members_rc = Vec::default();

        // TODO: This should probably be done somewhere else in a better way.
        //       In the future all enums might not be i32 and might not have the
        //       value of their index position, that might be configurable.
        // TODO: The type of the enum (Enum) and the type of the members (i32)
        //       are different. Can this cause any issues in the future?
        // Wraps all members of the enum into RCs so that they can be stored
        // in look-up tables during the analyze stage.
        // Also assign all members the type `Enum(ident)` and give them their
        // values according to their index position in the enum.
        for (idx, member) in members.iter_mut().enumerate() {
            let member_file_pos = member.file_pos.clone().unwrap();

            let enum_type_info = (ident.clone(), file_pos.to_owned());
            let member_type_info = (member.name.clone(), member_file_pos);
            let member_value_ty = Ty::CompoundType(
                InnerTy::I32,
                Generics::empty(),
                TypeInfo::EnumMember(enum_type_info, member_type_info),
            );

            let enum_ty = Ty::CompoundType(
                InnerTy::Enum(ident.clone()),
                Generics::empty(),
                TypeInfo::Enum(member_file_pos.to_owned()),
            );

            member.ty = Some(enum_ty);
            member.value = Some(Box::new(Expr::Lit(
                Lit::Integer(idx.to_string(), RADIX),
                Some(member_value_ty),
                None,
            )));

            members_rc.push(Rc::new(RefCell::new(member.clone())));
        }

        let members_opt = if !members_rc.is_empty() {
            Some(members_rc)
        } else {
            None
        };

        // TODO: How should the type of the enum be decided? Should it be possible
        //       to specify as a generic on the enum declaration? But in that case
        //       it would take a generic impl instead of a generic decl as structs.
        //       Is this ok?
        let enum_ty = Ty::CompoundType(
            InnerTy::Enum(ident.clone()),
            Generics::empty(),
            TypeInfo::Enum(file_pos.to_owned()),
        );

        let enum_ = Enum::new(ident, enum_ty, members_opt);
        let header = BlockHeader::Enum(Rc::new(RefCell::new(enum_)));

        let block_id = self.iter.reserve_block_id();
        let body = Vec::with_capacity(0);

        Ok(AstToken::Block(header, file_pos, block_id, body))
    }

    // TODO: Generics
    /// Parses a implement header.
    ///   "implement <ident> { [<func> ...] }"
    /// The "implement" keyword has already been consumed when this function is called.
    fn parse_impl(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        // Start by parsing the identifier.
        let lex_token = self.iter.next_skip_space_line();
        let ident =
            if let Some(LexTokenKind::Ident(ident)) = lex_token.as_ref().map(|token| &token.kind) {
                ident.clone()
            } else {
                return Err(self.iter.err(
                    format!("Not ident after parsing \"implement\": {:?}", lex_token),
                    lex_token.map(|t| t.file_pos),
                ));
            };

        let header = BlockHeader::Implement(ident);
        let impl_token = self.iter.next_block(header)?;

        // Iterate through the tokens in the body and make sure that all tokens
        // are functions.
        if let AstToken::Block(BlockHeader::Implement(_), file_pos, _, body) = &impl_token {
            for ast_token in body {
                if let AstToken::Block(BlockHeader::Function(_), ..) = ast_token {
                    // Do nothing, the token is of correct type.
                } else if ast_token.is_skippable() {
                } else {
                    return Err(self.iter.err(
                        format!(
                            "Non function parsed in \"implement\" block: {:#?}.",
                            ast_token
                        ),
                        Some(file_pos.to_owned()),
                    ));
                }
            }
        } else {
            return Err(self.iter.err(
                format!(
                    "Parsed \"implement\" block not a impl block: {:#?}.",
                    impl_token
                ),
                Some(file_pos),
            ));
        }

        Ok(impl_token)
    }

    /// Parses a defer statement.
    ///   "defer <expr>"
    /// The "defer" keyword has already been consumed when this function is called.
    fn parse_defer(&mut self, file_pos: FilePosition) -> CustomResult<AstToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        Ok(AstToken::Stmt(Stmt::Defer(expr, Some(file_pos))))
    }
}
