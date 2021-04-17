use std::{cell::RefCell, collections::HashMap, rc::Rc};

use common::{
    error::LangResult,
    file::FilePosition,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader, Fn, Trait},
        expr::Expr,
        lit::Lit,
        stmt::{Modifier, Stmt},
    },
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
        type_info::TypeInfo,
    },
    TypeId,
};
use lex::token::{Kw, LexTokenKind, Sym};

use crate::parser::{ParseTokenIter, DEFAULT_STOP_CONDS, KEYWORD_STOP_CONDS};

pub(crate) struct KeyworkParser<'a, 'b> {
    iter: &'a mut ParseTokenIter<'b>,
}

impl<'a, 'b> KeyworkParser<'a, 'b> {
    pub(crate) fn parse(
        iter: &'a mut ParseTokenIter<'b>,
        keyword: Kw,
        kw_file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let mut keyword_parser = Self { iter };
        let modifiers = Vec::default();
        keyword_parser.parse_keyword(keyword, modifiers, kw_file_pos)
    }

    fn parse_keyword(
        &mut self,
        keyword: Kw,
        modifiers: Vec<Modifier>,
        kw_file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        match keyword {
            Kw::If => self.parse_if(kw_file_pos),
            Kw::Match => self.parse_match(kw_file_pos),

            Kw::For => self.parse_for(kw_file_pos),
            Kw::While => self.parse_while(kw_file_pos),
            Kw::Implement => self.parse_impl(kw_file_pos),
            Kw::Function => self.parse_fn(modifiers, kw_file_pos),

            Kw::Return => self.parse_return(kw_file_pos),
            Kw::Yield => self.parse_yield(kw_file_pos),
            Kw::Break => self.parse_break(kw_file_pos),
            Kw::Continue => self.parse_continue(kw_file_pos),

            Kw::Use => self.parse_use(kw_file_pos),
            Kw::Module => self.parse_module(kw_file_pos),
            Kw::External => self.parse_external(modifiers, kw_file_pos),

            Kw::Var => self.parse_var_decl(false, kw_file_pos),
            Kw::Const => self.parse_var_decl(true, kw_file_pos),
            Kw::Static => Err(self.iter.err(
                "\"Static\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),

            Kw::Private | Kw::Public | Kw::Hidden => {
                self.parse_modifier(keyword, modifiers, kw_file_pos)
            }

            Kw::Struct => self.parse_struct(modifiers, kw_file_pos),
            Kw::Enum => self.parse_enum(modifiers, kw_file_pos),
            Kw::Union => self.parse_union(modifiers, kw_file_pos),
            Kw::Trait => self.parse_trait(modifiers, kw_file_pos),

            Kw::Defer => self.parse_defer(kw_file_pos),

            Kw::Test => Err(self.iter.err(
                "\"Test\" keyword not implemented.".into(),
                Some(kw_file_pos),
            )),

            Kw::Implements | Kw::Where | Kw::Else => Err(self.iter.err(
                format!(
                    "Unexpected keyword when parsing keyword start: {:#?}",
                    keyword
                ),
                Some(kw_file_pos),
            )),
        }
    }

    /// Parses a modifier (public, private or hidden) and the recursively calls
    /// `parse_keyword()` to parse the "actual" structure/type to which this
    /// modifier is applied on.
    fn parse_modifier(
        &mut self,
        modifier_kw: Kw,
        mut modifiers: Vec<Modifier>,
        kw_file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let modifier = match modifier_kw {
            Kw::Private => Modifier::Private,
            Kw::Public => Modifier::Public,
            Kw::Hidden => Modifier::Hidden,
            _ => {
                return Err(self.iter.err(
                    format!(
                        "Unexpected keyword when parsing modifier: {:#?}",
                        modifier_kw
                    ),
                    Some(kw_file_pos),
                ))
            }
        };
        modifiers.push(modifier);

        let lex_token = if let Some(lex_token) = self.iter.next_skip_space_line() {
            lex_token
        } else {
            return Err(self.iter.err(
                "Got None when looking a token after modifier.".into(),
                Some(kw_file_pos),
            ));
        };

        if let LexTokenKind::Kw(next_keyword) = lex_token.kind {
            self.parse_keyword(next_keyword, modifiers, lex_token.file_pos)
        } else {
            Err(self.iter.err(
                format!(
                    "Token after modifier \"{:?}\" not a keyword, was: {:#?}",
                    modifier_kw, lex_token
                ),
                Some(lex_token.file_pos),
            ))
        }
    }

    /// Parses the matching `IfCase`s into a `If` block. This includes all "else"
    /// blocks aswell. The keyword "else" is used for both "else" and "else if".
    ///
    /// # Examples
    ///
    /// ```no_run
    /// if <expr> { ... }
    /// ```
    ///
    /// ```no_run
    /// if <expr> { ... } else { ... }
    /// ```
    ///
    /// ```no_run
    /// if <expr> { ... } else <expr> { ... }
    /// ```
    ///
    /// The "if" keyword has already been consumed when this function is called.
    fn parse_if(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
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
    ///
    /// # Examples
    ///
    /// ```no_run
    /// match <expr> {
    ///     <case_expr> { ... }
    ///     <case_expr> { ... }
    /// }
    /// ```
    ///
    /// The "match" keyword has already been consumed when this function is called.
    fn parse_match(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
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
        if !matches!(
            peek_token.as_ref().map(|t| &t.kind),
            Some(LexTokenKind::Sym(Sym::CurlyBracketBegin))
        ) {
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

    /// Parses a `for` loop block.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// for <var> in <expr> { ... }
    /// ```
    ///
    /// The "for" keyword has already been consumed when this function is called.
    fn parse_for(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
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

    /// Parses a `while` loop block. A empty while-expression indicates a
    /// inifite loop.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// while <expr> { ... }
    /// ```
    ///
    /// ```no_run
    /// while { ... }
    /// ```
    ///
    /// The "while" keyword has already been consumed when this function is called.
    fn parse_while(&mut self, file_pos: FilePosition) -> LangResult<AstToken> {
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

    /// Parses a `return` statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// return <expr>
    /// ```
    ///
    /// ```no_run
    /// return
    /// ```
    ///
    /// The "return" keyword has already been consumed when this function is called.
    fn parse_return(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
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

    /// Parses a `yield` statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// yield <expr>
    /// ```
    ///
    /// The "yield" keyword has already been consumed when this function is called.
    fn parse_yield(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;

        if let Some(expr_file_pos) = expr.file_pos() {
            file_pos.set_end(expr_file_pos)?;
        } else {
            unreachable!();
        }

        Ok(AstToken::Stmt(Stmt::Yield(expr, Some(file_pos))))
    }

    /// Parses a `break` statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// break
    /// ```
    ///
    /// The "break" keyword has already been consumed when this function is called.
    fn parse_break(&mut self, file_pos: FilePosition) -> LangResult<AstToken> {
        Ok(AstToken::Stmt(Stmt::Break(Some(file_pos))))
    }

    /// Parses a `continue` statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// continue
    /// ```
    ///
    /// The "continue" keyword has already been consumed when this function is called.
    fn parse_continue(&mut self, file_pos: FilePosition) -> LangResult<AstToken> {
        Ok(AstToken::Stmt(Stmt::Continue(Some(file_pos))))
    }

    /// Parses a `use` statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use <path>
    /// ```
    /// (where path is a double colon separated list of idents)
    ///
    /// The "use" keyword has already been consumed when this function is called.
    fn parse_use(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
        Ok(AstToken::Stmt(Stmt::Use(
            self.iter.parse_path(&mut file_pos, GenericsKind::Empty)?,
        )))
    }

    /// Parses a `mod` statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// mod <path>
    /// ```
    /// (where path is a double colon separated list of idents)
    ///
    /// The "package" keyword has already been consumed when this function is called.
    fn parse_module(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
        Ok(AstToken::Stmt(Stmt::Module(
            self.iter.parse_path(&mut file_pos, GenericsKind::Empty)?,
        )))
    }

    // TODO: External only valid for functions atm, add for variables.
    /// Parses a external declaration statement.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// external <fn_prototype>
    /// ```
    ///
    /// The "external" keyword has already been consumed when this function is called.
    fn parse_external(
        &mut self,
        modifiers: Vec<Modifier>,
        file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        if let Some(lex_token) = self.iter.next_skip_space_line() {
            let func = match lex_token.kind {
                LexTokenKind::Kw(Kw::Function) => {
                    self.parse_fn_proto(modifiers, lex_token.file_pos)?
                }
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

    /// Parses a `var` declaration statement: "var <ident> [: <type>] [= <expr>]"
    ///
    /// # Examples
    ///
    /// ```no_run
    /// var x
    /// ```
    ///
    /// ```no_run
    /// var x = 3
    /// ```
    ///
    /// ```no_run
    /// var x: i32 = 3
    /// ```
    ///
    /// The "var" keyword has already been consumed when this function is called.
    fn parse_var_decl(
        &mut self,
        is_const: bool,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
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

    /// Parses a function and its body. See `parse_fn_proto` for the structure
    /// of a function header/prototype.
    /// The "function" keyword has already been consumed when this function is called.
    fn parse_fn(
        &mut self,
        modifiers: Vec<Modifier>,
        file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let func = self.parse_fn_proto(modifiers, file_pos)?;
        let func_header = BlockHeader::Fn(Rc::new(RefCell::new(func)));
        self.iter.next_block(func_header)
    }

    /// Parses a function prototype/header.
    ///   "[ <modifier>... ] function [ <modifier>... ] <ident> [ < <generic>, ... > ] ( [<ident>: <type>] [= <default value>], ... ) [ "->" <type> ]"
    /// The "function" keyword has already been consumed when this function is called.
    fn parse_fn_proto(
        &mut self,
        mut modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<Fn> {
        // TODO: Handle FilePosition.

        static THIS: &str = "this";

        // Start by parsing the modifiers and identifier. This will loop until
        // the identifier/path of the function is found.
        let mut module = loop {
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
                        if !matches!(
                            lex_token.as_ref().map(|l| &l.kind),
                            Some(LexTokenKind::Sym(Sym::CurlyBracketEnd))
                        ) {
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

                    LexTokenKind::Ident(_) => {
                        self.iter.rewind_skip_space()?;
                        break self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;
                    }

                    LexTokenKind::Kw(lex_kw) => {
                        if let Some(modifier) = crate::token::get_modifier_token(&lex_kw) {
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

        let last_part = module.pop().unwrap();
        let name = last_part.0;
        let generics = last_part.1;

        let start_symbol = Sym::ParenthesisBegin;
        let end_symbol = Sym::ParenthesisEnd;
        let (params, is_var_arg, par_file_pos) =
            self.iter
                .parse_par_list(start_symbol, end_symbol, generics.as_ref())?;

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
        let (ret_type_id, ty_file_pos) = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Sym(Sym::Arrow) = lex_token.kind {
                self.iter.next_skip_space_line(); // Consume the arrow.

                let (ret_type_id, ret_file_pos) = self.iter.parse_type(None)?;
                (Some(ret_type_id), Some(ret_file_pos))
            } else {
                (None, None)
            }
        } else {
            return Err(self.iter.err(
                "Received None when looking at token after \"function <ident>\".".into(),
                Some(file_pos),
            ));
        };

        let implements = self.parse_where(generics.as_ref())?;
        Ok(Fn::new(
            name,
            module,
            generics,
            implements,
            params_opt,
            ret_type_id,
            modifiers,
            is_var_arg,
        ))
    }

    /// Parses a struct header.
    ///   "struct <ident> [ < <generic>, ... > ] [where ...] [{ [<ident>: <type>] [[,] ...] }]"
    /// The "struct" keyword has already been consumed when this function is called.
    fn parse_struct(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let mut module = self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;

        let last_part = module.pop().unwrap();
        let name = last_part.0;
        let generics = last_part.1;

        let implements = self.parse_where(generics.as_ref())?;

        // Parse the members of the struct. If the next token isn't a
        // "CurlyBracketBegin" symbol, assume this is a struct with no members.
        let (members, is_var_arg) = if let Some(LexTokenKind::Sym(Sym::CurlyBracketBegin)) =
            self.iter.peek_skip_space_line().map(|t| t.kind)
        {
            let start_symbol = Sym::CurlyBracketBegin;
            let end_symbol = Sym::CurlyBracketEnd;
            let (members, is_var_arg, par_file_pos) =
                self.iter
                    .parse_par_list(start_symbol, end_symbol, generics.as_ref())?;

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
                    &name
                ),
                Some(file_pos),
            ));
        }

        let struct_ = Adt::new_struct(name, module, modifiers, members, generics, implements);
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
    fn parse_enum(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let full_path = self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;

        let mut module = full_path.clone();

        let last_part = module.pop().unwrap();
        let name = last_part.0;

        // Parse the members of the enum.
        let start_symbol = Sym::CurlyBracketBegin;
        let end_symbol = Sym::CurlyBracketEnd;
        let (mut members, is_var_arg, par_file_pos) =
            self.iter.parse_par_list(start_symbol, end_symbol, None)?;

        file_pos.set_end(&par_file_pos)?;

        if is_var_arg {
            return Err(self.iter.err(
                format!("Found invalid var_arg symbol in enum with name: {}", &name),
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

            let enum_type_info = (name.clone(), file_pos.to_owned());
            let member_type_info = (member.name.clone(), member_file_pos);
            let member_value_type_id = self.iter.ty_env.id(&Ty::CompoundType(
                InnerTy::I32,
                Generics::empty(),
                TypeInfo::EnumMember(enum_type_info, member_type_info),
            ))?;

            let enum_type_id = self.iter.ty_env.id(&Ty::CompoundType(
                InnerTy::Enum(full_path.clone()),
                Generics::empty(),
                TypeInfo::Enum(member_file_pos.to_owned()),
            ))?;

            member.ty = Some(enum_type_id);
            member.value = Some(Box::new(Expr::Lit(
                Lit::Integer(idx.to_string(), RADIX),
                Some(member_value_type_id),
                None,
            )));

            members_rc.push(Rc::new(RefCell::new(member.clone())));
        }

        // TODO: How should the type of the enum be decided? Should it be possible
        //       to specify as a generic on the enum declaration? But in that case
        //       it would take a generic impl instead of a generic decl as structs.
        //       Is this ok?
        let enum_type_id = self.iter.ty_env.id(&Ty::CompoundType(
            InnerTy::Enum(full_path),
            Generics::empty(),
            TypeInfo::Enum(file_pos.to_owned()),
        ))?;

        let enum_ = Adt::new_enum(name, module, modifiers, members_rc, Some(enum_type_id));
        let header = BlockHeader::Enum(Rc::new(RefCell::new(enum_)));

        let block_id = self.iter.reserve_block_id();
        let body = Vec::with_capacity(0);

        Ok(AstToken::Block(header, file_pos, block_id, body))
    }

    /// Parses a union header.
    ///   "union <ident> [ < <generic>, ... > ] [where ...] [{ [<ident>: <type>] [[,] ...] }]"
    /// The "union" keyword has already been consumed when this function is called.
    fn parse_union(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let mut module = self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;

        let last_part = module.pop().unwrap();
        let name = last_part.0;
        let generics = last_part.1;

        let implements = self.parse_where(generics.as_ref())?;

        // Parse the members of the union. If the next token isn't a
        // "CurlyBracketBegin" symbol, assume this is a enum with no members.
        let (members, is_var_arg) = if let Some(LexTokenKind::Sym(Sym::CurlyBracketBegin)) =
            self.iter.peek_skip_space_line().map(|t| t.kind)
        {
            let start_symbol = Sym::CurlyBracketBegin;
            let end_symbol = Sym::CurlyBracketEnd;
            let (members, is_var_arg, par_file_pos) =
                self.iter
                    .parse_par_list(start_symbol, end_symbol, generics.as_ref())?;

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
                format!("Found invalid var_arg symbol in union with name: {}", &name),
                Some(file_pos),
            ));
        }

        let union = Adt::new_union(name, module, modifiers, members, generics, implements);
        let header = BlockHeader::Union(Rc::new(RefCell::new(union)));

        let block_id = self.iter.reserve_block_id();
        let body = Vec::with_capacity(0);

        Ok(AstToken::Block(header, file_pos, block_id, body))
    }

    // TODO: Generics
    /// Parses a trait header.
    ///   "trait <ident> [ < <generic>, ... > ] { [<func> ...] }"
    /// The "trait" keyword has already been consumed when this function is called.
    fn parse_trait(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let mut module = self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;

        let last_part = module.pop().unwrap();
        let name = last_part.0;
        let generics = last_part.1;

        // Consume the expected CurlyBracketBegin.
        let start_token = self.iter.next_skip_space_line();
        if !matches!(
            start_token.as_ref().map(|t| &t.kind),
            Some(LexTokenKind::Sym(Sym::CurlyBracketBegin))
        ) {
            return Err(self.iter.err(
                format!(
                    "Expected CurlyBracketBegin at start if trait block, got: {:#?}",
                    start_token
                ),
                start_token.map(|t| t.file_pos),
            ));
        }

        // TODO: Allow to set default bodies for the functions?
        let mut methods = Vec::default();

        let mut modifiers = Vec::default();

        while let Some(lex_token) = self.iter.next_skip_space_line() {
            file_pos.set_end(&lex_token.file_pos)?;

            match lex_token.kind {
                LexTokenKind::Kw(kw) if kw.is_modifier() => {
                    let modifier = match kw {
                        Kw::Private => Modifier::Private,
                        Kw::Public => Modifier::Public,
                        Kw::Hidden => Modifier::Hidden,
                        _ => panic!("TODO: Modifier \"{:?}\"", kw),
                    };
                    modifiers.push(modifier);
                }

                LexTokenKind::Kw(Kw::Function) => {
                    let method = self.parse_fn_proto(modifiers, lex_token.file_pos)?;
                    methods.push(method);
                    modifiers = Vec::default();
                }

                // End of trait block.
                LexTokenKind::Sym(Sym::CurlyBracketEnd) => break,

                _ => {
                    return Err(self.iter.err(
                        format!(
                            "Unexpected token when parsing functions in trait: {:#?}",
                            lex_token
                        ),
                        Some(lex_token.file_pos),
                    ))
                }
            }
        }

        let generic_names = generics.map(|gens| gens.iter_names().cloned().collect::<Vec<_>>());

        let trait_ = Trait::new(name, module, generic_names, methods, modifiers);
        let header = BlockHeader::Trait(Rc::new(RefCell::new(trait_)));

        let block_id = self.iter.reserve_block_id();
        let body = Vec::with_capacity(0);

        Ok(AstToken::Block(header, file_pos, block_id, body))
    }

    // TODO: Generics
    /// Parses a implement header. This can either be a impl for the methods of
    /// a structure or it can also be a impl of a trait for a structure.
    ///   "implement <ident> [for <ident>] { [<func> ...] }"
    /// The "implement" keyword has already been consumed when this function is called.
    fn parse_impl(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
        let first_path = self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;

        // If the next token is a "for" keyword, this is a impl for a trait
        // and the previosly parsed `ident` is the name of the trait. If not,
        // the previous `ident` is the name of a structure.
        let (adt_path, trait_path) = if let Some(LexTokenKind::Kw(Kw::For)) =
            self.iter.peek_skip_space_line().as_ref().map(|t| &t.kind)
        {
            self.iter.next_skip_space_line();
            let adt_path = self.iter.parse_path(&mut file_pos, GenericsKind::Decl)?;

            (adt_path, Some(first_path))
        } else {
            (first_path, None)
        };

        let header = BlockHeader::Implement(adt_path, trait_path);
        let impl_token = self.iter.next_block(header)?;

        // Iterate through the tokens in the body and make sure that all tokens
        // are functions.
        if let AstToken::Block(BlockHeader::Implement(..), file_pos, _, body) = &impl_token {
            for ast_token in body {
                if let AstToken::Block(BlockHeader::Fn(_), ..) = ast_token {
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

    /// Parses a where clause. Every "implements" statement are parsed to the
    /// end of the line.
    ///   "where [<ident> implements <trait> [,<trait>]...]"
    /// The "where" keyword has NOT been parsed at this point. If the next token
    /// isn't the "where" keyword, no where clause exists so a None should
    /// be returned.
    /// A "where" clause should be ended either with a CurlyBracketBegin or
    /// a semi colon.
    fn parse_where(
        &mut self,
        generics: Option<&Generics>,
    ) -> LangResult<Option<HashMap<String, Vec<TypeId>>>> {
        // Next token isn't a "where" keyword => early None return.
        if let Some(LexTokenKind::Kw(Kw::Where)) = self.iter.peek_skip_space_line().map(|t| t.kind)
        {
            self.iter.next_skip_space_line();
        } else {
            return Ok(None);
        }

        let mut implements = HashMap::new();

        loop {
            // Start by parsing the identifier.
            let lex_token = self.iter.next_skip_space_line();
            let ident = if let Some(LexTokenKind::Ident(ident)) =
                lex_token.as_ref().map(|token| &token.kind)
            {
                ident.clone()
            } else {
                return Err(self.iter.err(
                    format!(
                        "Expected ident when parsing \"where\" clause, got: {:#?}",
                        lex_token
                    ),
                    lex_token.map(|t| t.file_pos),
                ));
            };

            // Consume and make sure that the next token is a "implements" keyword .
            let lex_token = self.iter.next_skip_space_line();
            if !matches!(
                lex_token.as_ref().map(|t| &t.kind),
                Some(LexTokenKind::Kw(Kw::Implements))
            ) {
                return Err(self.iter.err(
                    format!(
                        "Expected \"implements\" when parsing \"where\" clause, got: {:#?}",
                        lex_token
                    ),
                    lex_token.map(|t| t.file_pos),
                ));
            }

            let mut types = Vec::default();
            loop {
                let (type_id, _) = self.iter.parse_type(generics)?;
                types.push(type_id);

                // TODO: Should trailing commas be allowed?
                // If the next token is a comma, continue parsing types. If it
                // is a line break symbol, stop parsing the types. Else, unexpected
                // symbol; return error.
                if let Some(next_token) = self.iter.next_skip_space() {
                    if next_token.is_break_symbol() {
                        self.iter.rewind_skip_space()?;
                        break;
                    } else if let LexTokenKind::Sym(Sym::Comma) = next_token.kind {
                        continue;
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Expected \"implements\" when parsing \"where\" clause, got: {:#?}",
                                lex_token
                            ),
                            Some(next_token.file_pos),
                        ));
                    }
                } else {
                    return Err(self.iter.err(
                        "Got back None when parsing \"where\" clause.".into(),
                        None, // TODO: Filepos
                    ));
                }
            }

            implements.insert(ident, types);

            if let Some(LexTokenKind::Sym(Sym::CurlyBracketBegin)) =
                self.iter.peek_skip_space_line().as_ref().map(|t| &t.kind)
            {
                break;
            } else if let Some(LexTokenKind::Sym(Sym::SemiColon)) =
                self.iter.peek_skip_space().as_ref().map(|t| &t.kind)
            {
                self.iter.next_skip_space();
                break;
            }
        }

        Ok(Some(implements))
    }

    /// Parses a defer statement.
    ///   "defer <expr>"
    /// The "defer" keyword has already been consumed when this function is called.
    fn parse_defer(&mut self, file_pos: FilePosition) -> LangResult<AstToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        Ok(AstToken::Stmt(Stmt::Defer(expr, Some(file_pos))))
    }
}