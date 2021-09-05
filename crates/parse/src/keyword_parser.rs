use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use parking_lot::RwLock;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{AdtBuilder, Block, BlockHeader, Fn, Trait},
        expr::{Expr, Var},
        lit::Lit,
        stmt::{ExternalDecl, Modifier, Stmt},
    },
    ty::{
        generics::GenericsKind, inner_ty::InnerTy, to_string::to_string_path, ty::Ty,
        type_info::TypeInfo,
    },
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

            Kw::Where | Kw::Else | Kw::FunctionPointer => Err(self.iter.err(
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

        Ok(AstToken::Block(Block {
            header: BlockHeader::If,
            body: if_cases,
            id: block_id,
            file_pos,
        }))
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
            Ok(AstToken::Block(Block {
                header: BlockHeader::Match(match_expr),
                body: match_cases,
                id: block_id,
                file_pos,
            }))
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
    #[allow(clippy::unnecessary_wraps)]
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
    #[allow(clippy::unnecessary_wraps)]
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
        Ok(AstToken::Stmt(Stmt::Use(self.iter.parse_path(
            &mut file_pos,
            GenericsKind::Empty,
            false,
        )?)))
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
        Ok(AstToken::Stmt(Stmt::Module(self.iter.parse_path(
            &mut file_pos,
            GenericsKind::Empty,
            false,
        )?)))
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
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        if let Some(lex_token) = self.iter.next_skip_space_line() {
            let ext_decl = match lex_token.kind {
                LexTokenKind::Kw(Kw::Function) => {
                    // TODO: This doesn't include the function prototype into the
                    //       file_pos, only the extern keyword. The function
                    //       prototype currently doesn't store/keep any information
                    //       about its file_pos. How should this work?
                    ExternalDecl::Fn(Arc::new(RwLock::new(
                        self.parse_fn_proto(modifiers, lex_token.file_pos)?,
                    )))
                }

                LexTokenKind::Kw(Kw::Struct) => {
                    // TODO: Should the ident be parsed as a `LangPath` instead?
                    //       Is there a possiblity that the external declaration
                    //       is inside a module/namespace?
                    let next_lex_token = self.iter.next_skip_space_line();
                    let ident = if let Some(LexTokenKind::Ident(ident)) =
                        next_lex_token.clone().map(|t| t.kind)
                    {
                        ident
                    } else {
                        return Err(self.iter.err(
                            format!("Expected ident after `ext struct`, found: {:?}", lex_token),
                            Some(lex_token.file_pos),
                        ));
                    };

                    file_pos.set_end(&next_lex_token.unwrap().file_pos)?;

                    let mut builder = AdtBuilder::new_struct();
                    builder
                        .name(ident)
                        .modifiers(modifiers)
                        .file_pos(file_pos)
                        .has_definition(false);

                    let ext_struct = builder.build()?;
                    ExternalDecl::Struct(Arc::new(RwLock::new(ext_struct)))
                }

                _ => {
                    return Err(self.iter.err(
                        format!("Invalid keyword after external keyword: {:?}", lex_token),
                        Some(lex_token.file_pos),
                    ));
                }
            };

            Ok(AstToken::Stmt(Stmt::ExternalDecl(ext_decl, Some(file_pos))))
        } else {
            Err(self.iter.err(
                "Received None lex token after external keyword.".into(),
                Some(file_pos),
            ))
        }
    }

    /// Parses a `var`/`const` declaration statement:
    ///   "[var | const] <ident> [: <type>] [= <expr>]"
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
    /// The "var"/"const" keyword has already been consumed when this function
    /// is called.
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
        let var = Arc::new(RwLock::new(self.iter.parse_var(
            &ident,
            parse_type,
            parse_value,
            is_const,
            None,
            var_file_pos,
        )?));

        if let Some(var_file_pos) = var.read().file_pos {
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
        let func_header = BlockHeader::Fn(Arc::new(RwLock::new(func)));
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
                        break self
                            .iter
                            .parse_path(&mut file_pos, GenericsKind::Decl, false)?;
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

        let (gens, decl_impls) = self.iter.parse_gens_decl(&mut file_pos)?;

        let start_symbol = Sym::ParenthesisBegin;
        let end_symbol = Sym::ParenthesisEnd;
        let (params, is_var_arg, par_file_pos) =
            self.iter
                .parse_par_list(start_symbol, end_symbol, gens.as_ref())?;

        file_pos.set_end(&par_file_pos)?;

        // Wrap the params into RC & RefCell.
        let params = params
            .iter()
            .map(|var| Arc::new(RwLock::new(var.clone())))
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

        if let Some(ret_ty_file_pos) = &ty_file_pos {
            file_pos.set_end(ret_ty_file_pos)?;
        }

        let where_impls = self.parse_where(&mut file_pos)?;

        let fn_full_path = to_string_path(
            &self.iter.ty_env.lock(),
            &module.clone_push(&name, gens.as_ref(), Some(file_pos)),
        );

        let impls = Self::combine_impls(
            decl_impls,
            where_impls,
            &fn_full_path,
            Some(file_pos).as_ref(),
        )?;

        // TODO: How should the `file_pos` of the function be decided? Currently
        //       it will only include the function header, it probably should
        //       also contain the body.

        Ok(Fn::new(
            name,
            module,
            gens,
            file_pos,
            impls,
            params_opt,
            ret_type_id,
            modifiers,
            is_var_arg,
        ))
    }

    /// Parses a struct.
    ///   "struct <ident> [ < <generic>, ... > ] [where ...] [{ ... }]"
    /// The "struct" keyword has already been consumed when this function is called.
    fn parse_struct(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let mut builder = AdtBuilder::new_struct();
        let block_id = self.iter.reserve_block_id();

        self.parse_adt_header(&mut builder, &mut file_pos)?;
        let body = self.parse_adt_body(&mut builder, &mut file_pos)?;

        builder
            .modifiers(modifiers)
            .has_definition(true)
            .file_pos(file_pos);
        let struct_ = builder.build()?;

        Ok(AstToken::Block(Block {
            header: BlockHeader::Struct(Arc::new(RwLock::new(struct_))),
            body,
            id: block_id,
            file_pos,
        }))
    }

    /// Parses a union.
    ///   "union <ident> [ < <generic>, ... > ] [where ...] [{ ... }]"
    /// The "union" keyword has already been consumed when this function is called.
    fn parse_union(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let mut builder = AdtBuilder::new_union();
        let block_id = self.iter.reserve_block_id();

        self.parse_adt_header(&mut builder, &mut file_pos)?;
        let body = self.parse_adt_body(&mut builder, &mut file_pos)?;

        builder
            .modifiers(modifiers)
            .has_definition(true)
            .file_pos(file_pos);
        let union = builder.build()?;

        Ok(AstToken::Block(Block {
            header: BlockHeader::Union(Arc::new(RwLock::new(union))),
            body,
            id: block_id,
            file_pos,
        }))
    }

    // TODO: Should it possible to set values to the individual enum variants?
    //       Should there be possible to set the integer type of the enum?
    /// Parses a enum block.
    ///   "enum <ident> { [<MEMBER_NAME>] ... }"
    /// The "enum" keyword has already been consumed when this function is called.
    fn parse_enum(
        &mut self,
        modifiers: Vec<Modifier>,
        mut file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        let block_id = self.iter.reserve_block_id();
        let full_path = self
            .iter
            .parse_path(&mut file_pos, GenericsKind::Decl, false)?;

        let next_token = self.iter.next_skip_space_line();
        let next_kind = next_token.as_ref().map(|t| t.kind.clone());
        if !matches!(next_kind, Some(LexTokenKind::Sym(Sym::CurlyBracketBegin))) {
            return Err(self.iter.err(
                format!(
                    "Expected CurlyBracketBegin at start of enum body, but found: {:#?}",
                    next_token
                ),
                next_token.map(|t| t.file_pos),
            ));
        }

        let mut module = full_path.clone();
        let last_part = module.pop().unwrap();
        let name = last_part.0;

        let mut members = Vec::default();

        // Only expects idents in the enum body, nothing else is expected atm.
        loop {
            let peek_token = self.iter.peek_skip_space_line();
            let peek_kind = peek_token.map(|t| t.kind);
            if let Some(LexTokenKind::Sym(Sym::CurlyBracketEnd)) = peek_kind {
                let curly_bracket_end = self.iter.next_skip_space_line();
                file_pos.set_end(&curly_bracket_end.unwrap().file_pos)?;
                break;
            }

            let next_token = self.iter.next_skip_space_line();
            let next_kind = next_token.as_ref().map(|t| t.kind.clone());
            if let Some(LexTokenKind::Ident(member_name)) = next_kind {
                let member_file_pos = next_token.unwrap().file_pos.to_owned();
                let is_const = true;

                let member = Var::new(
                    member_name,
                    None,
                    None,
                    None,
                    Some(member_file_pos),
                    None,
                    is_const,
                );
                members.push(member);
            } else {
                return Err(self.iter.err(
                    format!(
                        "Unexpected token in enum body block, was: {:#?}.",
                        next_token
                    ),
                    next_token.map(|t| t.file_pos),
                ));
            };
        }

        // TODO: How should the type of the enum be decided? Should it be possible
        //       to specify as a generic on the enum declaration? But in that case
        //       it would take a generic impl instead of a generic decl as structs.
        //       Is this ok?
        let enum_type_id = self.iter.ty_env.lock().id(&Ty::CompoundType(
            InnerTy::Enum(full_path.clone()),
            TypeInfo::Enum(file_pos.to_owned()),
        ))?;

        let mut builder = AdtBuilder::new_enum();
        builder
            .module(module)
            .name(name.clone())
            .modifiers(modifiers)
            .file_pos(file_pos)
            .has_definition(true)
            .enum_ty(enum_type_id);

        const RADIX: u32 = 10;

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
            let mut ty_env_guard = self.iter.ty_env.lock();
            let member_file_pos = member.file_pos.unwrap();

            let enum_type_info = (name.clone(), file_pos.to_owned());
            let member_type_info = (member.name.clone(), member_file_pos);
            let member_value_type_id = ty_env_guard.id(&Ty::CompoundType(
                InnerTy::I32,
                TypeInfo::EnumMember(enum_type_info, member_type_info),
            ))?;

            let enum_type_id = ty_env_guard.id(&Ty::CompoundType(
                InnerTy::Enum(full_path.clone()),
                TypeInfo::Enum(member_file_pos.to_owned()),
            ))?;

            member.ty = Some(enum_type_id);
            member.value = Some(Box::new(Expr::Lit(
                Lit::Integer(idx.to_string(), RADIX),
                Some(member_value_type_id),
                None,
            )));

            builder.insert_member(&Arc::new(RwLock::new(member.clone())));
        }

        let enum_ = builder.build()?;

        Ok(AstToken::Block(Block {
            header: BlockHeader::Enum(Arc::new(RwLock::new(enum_))),
            body: Vec::with_capacity(0),
            id: block_id,
            file_pos,
        }))
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
        let block_id = self.iter.reserve_block_id();
        let mut module = self
            .iter
            .parse_path(&mut file_pos, GenericsKind::Decl, true)?;

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
        let mut method_names = HashSet::new();
        let mut duplicate_method_names = HashSet::new();

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
                    file_pos.set_end(&method.file_pos)?;
                    modifiers = Vec::default();

                    if method_names.contains(&method.name) {
                        duplicate_method_names.insert(method.name.clone());
                    } else {
                        method_names.insert(method.name.clone());
                    }
                    methods.push(method);
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

        if !duplicate_method_names.is_empty() {
            let trait_path = module.clone_push(&name, None, Some(file_pos));
            let trait_name = to_string_path(&self.iter.ty_env.lock(), &trait_path);
            let mut method_names = duplicate_method_names
                .iter()
                .map(|name| format!(" * {}", name))
                .collect::<Vec<_>>();
            method_names.sort();
            return Err(self.iter.err(
                format!(
                    "Found mutiple methods with same name in trait \"{}\":\n{}",
                    trait_name,
                    method_names.join("\n")
                ),
                Some(file_pos),
            ));
        }

        let generic_names = generics.map(|gens| gens.iter_names().cloned().collect::<Vec<_>>());
        let trait_ = Trait::new(name, module, generic_names, file_pos, methods, modifiers);

        Ok(AstToken::Block(Block {
            header: BlockHeader::Trait(Arc::new(RwLock::new(trait_))),
            body: Vec::with_capacity(0),
            id: block_id,
            file_pos,
        }))
    }

    /// Parses a ADT header (module, name, generics and where-impls).
    ///   " <ident> [ < <generic>, ... > ] [where ...]"
    /// The ADT type identifier (struct/union) will already have been consumed
    /// when this function is called.
    fn parse_adt_header(
        &mut self,
        builder: &mut AdtBuilder,
        file_pos: &mut FilePosition,
    ) -> LangResult<()> {
        let mut module = self.iter.parse_path(file_pos, GenericsKind::Decl, false)?;

        let last_part = module.pop().unwrap();
        let name = last_part.0;

        let (gens, decl_impls) = self.iter.parse_gens_decl(file_pos)?;
        let where_impls = self.parse_where(file_pos)?;

        let adt_full_path = to_string_path(
            &self.iter.ty_env.lock(),
            &module.clone_push(&name, gens.as_ref(), Some(*file_pos)),
        );

        let impls = Self::combine_impls(decl_impls, where_impls, &adt_full_path, Some(file_pos))?;

        builder
            .module(module)
            .name(name)
            .generics(gens)
            .impls(impls);

        Ok(())
    }

    /// Parses a ADT body i.e. the variables and methods belonging to the
    /// specific ADT.
    ///   "[{ [<VARIABLE> | <FUNC>] ... }]"
    /// The returned vector is the methods parsed in the body (this will become
    /// the body tokens of the ADT).
    fn parse_adt_body(
        &mut self,
        builder: &mut AdtBuilder,
        file_pos: &mut FilePosition,
    ) -> LangResult<Vec<AstToken>> {
        let next_token = self.iter.peek_skip_space_line();
        let next_kind = next_token.map(|t| t.kind);
        if matches!(next_kind, Some(LexTokenKind::Sym(Sym::CurlyBracketBegin))) {
            self.iter.next_skip_space_line();
        } else {
            // Early return if this is an empty ADT body.
            return Ok(Vec::with_capacity(0));
        }

        let mut method_tokens = Vec::default();

        loop {
            let next_token = self.iter.peek_skip_space_line();
            let next_kind = next_token.as_ref().map(|t| t.kind.clone());

            // If true: Matches the ending CurlyBracketEnd and breaks out of the
            // loop, the body of this ADT have been parsed fully.
            if matches!(next_kind, Some(LexTokenKind::Sym(Sym::CurlyBracketEnd))) {
                self.iter.next_skip_space_line();
                break;
            }

            let token = match self.iter.next_token() {
                Ok(token) => token,
                Err(err) => {
                    return Err(self.iter.err(
                        format!(
                            "Unable to parse token in ADT body block. Nested err: {}",
                            &err.msg
                        ),
                        err.file_pos,
                    ))
                }
            };
            file_pos.set_end(token.file_pos().unwrap())?;

            match &token {
                // Parsing ADT methods.
                AstToken::Block(Block {
                    header: BlockHeader::Fn(..),
                    ..
                }) => {
                    // OBS! The methods aren't inserted into the ADT "look-up"
                    //      at this point, only the tokens are parsed and
                    //      returned from this function. The methods are inserted
                    //      into the ADT "look-up" in during the "decl" analyzing
                    //      stage.
                    method_tokens.push(token);
                }

                // Parsing ADT member variables.
                AstToken::Stmt(Stmt::VariableDecl(var, ..)) => {
                    builder.insert_member(var);
                }

                token if token.is_skippable() => (),

                _ => {
                    return Err(self.iter.err(
                        format!("Unexpected token in ADT body block, was: {:#?}.", token),
                        token.file_pos().cloned(),
                    ));
                }
            }
        }

        Ok(method_tokens)
    }

    /// Parses a implement block. A "impl" block implements a specific trait
    /// for a specific ADT.
    ///   "impl <ADT_NAME>: <TRAIT_NAME> [<GENERIC_IMPLS>] { [<FUNC> ...] }"
    /// The "impl" keyword has already been consumed when this function is called.
    fn parse_impl(&mut self, mut file_pos: FilePosition) -> LangResult<AstToken> {
        let adt_path = self
            .iter
            .parse_path(&mut file_pos, GenericsKind::Decl, true)?;

        let next_token = self.iter.next_skip_space_line();
        let next_kind = next_token.clone().map(|t| t.kind);
        if !matches!(next_kind, Some(LexTokenKind::Sym(Sym::Colon))) {
            return Err(self.iter.err(
                format!(
                    "Expected Colon after impl ADT ident, got: {:#?}.",
                    next_token
                ),
                Some(file_pos.to_owned()),
            ));
        }

        // TODO: How should the `file_pos` be updated before the parsing of the
        //       `trait_path`? Which FilePosition should be used?
        let trait_path = self
            .iter
            .parse_path(&mut file_pos, GenericsKind::Impl, true)?;

        let header = BlockHeader::Implement(adt_path, trait_path);
        let impl_token = self.iter.next_block(header)?;

        let impl_body = if let AstToken::Block(Block {
            header: BlockHeader::Implement(..),
            body: impl_body,
            ..
        }) = &impl_token
        {
            impl_body
        } else {
            return Err(self.iter.err(
                format!(
                    "Parsed \"implement\" block not a impl block: {:#?}.",
                    impl_token
                ),
                Some(file_pos),
            ));
        };

        // Iterate through the tokens inside the impl body block and make sure
        // that all tokens are functions.
        for ast_token in impl_body {
            if let AstToken::Block(Block {
                header: BlockHeader::Fn(..),
                ..
            }) = ast_token
            {
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

        Ok(impl_token)
    }

    /// Parses a where clause. Every "implements" statement are parsed to the
    /// end of the line.
    ///   "where [<ident> : <trait> [,<trait>]...]"
    /// The "where" keyword has NOT been parsed at this point. If the next token
    /// isn't the "where" keyword, no where clause exists so a None should
    /// be returned. A "where" clause should be ended either with a CurlyBracketBegin
    /// or a semi colon.
    fn parse_where(
        &mut self,
        file_pos: &mut FilePosition,
    ) -> LangResult<Option<HashMap<String, Vec<LangPath>>>> {
        // Next token isn't a "where" keyword => early None return.
        if let Some(LexTokenKind::Kw(Kw::Where)) = self.iter.peek_skip_space_line().map(|t| t.kind)
        {
            self.iter.next_skip_space_line();
        } else {
            return Ok(None);
        }

        let mut impls = HashMap::new();

        loop {
            // Start by parsing the generic identifier (ex. "T").
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

            // Consume and make sure that the next token is a "colon" symbol .
            let lex_token = self.iter.next_skip_space_line();
            if !matches!(
                lex_token.as_ref().map(|t| &t.kind),
                Some(LexTokenKind::Sym(Sym::Colon))
            ) {
                return Err(self.iter.err(
                    format!(
                        "Expected \"Colon\" when parsing \"where\" clause, got: {:#?}",
                        lex_token
                    ),
                    lex_token.map(|t| t.file_pos),
                ));
            }

            let mut paths = Vec::default();
            loop {
                let path = self.iter.parse_path(file_pos, GenericsKind::Impl, true)?;
                paths.push(path);

                // TODO: Should trailing commas be allowed?
                // If the next token is a comma, continue parsing types. If it
                // is a line break symbol or CurlyBracketBegin (indicating the
                // start of the body), stop parsing the types. Else, unexpected
                // symbol; return error.
                if let Some(next_token) = self.iter.next_skip_space() {
                    if next_token.is_break_symbol()
                        || matches!(next_token.kind, LexTokenKind::Sym(Sym::CurlyBracketBegin))
                    {
                        self.iter.rewind_skip_space()?;
                        break;
                    } else if let LexTokenKind::Sym(Sym::Comma) = next_token.kind {
                        continue;
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Bad token when parsing \"where\" clause, got: {:#?}",
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

            impls.insert(ident, paths);

            if let Some(LexTokenKind::Sym(Sym::CurlyBracketBegin)) =
                self.iter.peek_skip_space_line().as_ref().map(|t| &t.kind)
            {
                break;
            }
        }

        Ok(Some(impls))
    }

    /// Parses a defer statement.
    ///   "defer <expr>"
    /// The "defer" keyword has already been consumed when this function is called.
    fn parse_defer(&mut self, file_pos: FilePosition) -> LangResult<AstToken> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        Ok(AstToken::Stmt(Stmt::Defer(expr, Some(file_pos))))
    }

    /// Combine the optional trait impls parsed from the decl position with the
    /// optional traits impls parsed from the where-clause.
    ///
    /// The `print_path` and `file_pos` are used for error messages only. The
    /// path should be the path of the ADT/fn where the impls are taken from.
    fn combine_impls(
        decl_impls: Option<HashMap<String, Vec<LangPath>>>,
        where_impls: Option<HashMap<String, Vec<LangPath>>>,
        print_path: &str,
        file_pos: Option<&FilePosition>,
    ) -> LangResult<Option<HashMap<String, Vec<LangPath>>>> {
        Ok(match (decl_impls, where_impls) {
            (Some(decl_impls), Some(where_impls)) => {
                let gen_names = decl_impls
                    .keys()
                    .chain(where_impls.keys())
                    .collect::<HashSet<_>>();

                // If both implement traits for one specific generic, then
                // we can't be sure which one is correct. An error should be
                // returned in that case.
                for gen_name in gen_names {
                    if decl_impls.contains_key(gen_name) && where_impls.contains_key(gen_name) {
                        return Err(LangError::new(
                            format!(
                                "Impls specified on generic \"{}\" at two places in decl \"{:?}\".",
                                gen_name, print_path
                            ),
                            LangErrorKind::ParseError,
                            file_pos.cloned(),
                        ));
                    }
                }

                Some(
                    decl_impls
                        .into_iter()
                        .chain(where_impls.into_iter())
                        .collect::<HashMap<_, _>>(),
                )
            }
            (None, Some(impls)) | (Some(impls), None) => Some(impls),
            (None, None) => None,
        })
    }
}
