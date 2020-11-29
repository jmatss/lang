use std::{cell::RefCell, rc::Rc};

use crate::{
    parser::{ParseTokenIter, DEFAULT_STOP_CONDS},
    token::get_modifier_token,
    type_parser::TypeParser,
};
use common::{
    error::CustomResult,
    token::ast::Token,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function, Struct},
        expr::Var,
        stmt::{Modifier, Path, Stmt},
    },
    ty::generics::GenericsKind,
};
use lex::token::{Kw, LexTokenKind, Sym};

pub struct KeyworkParser<'a, 'b> {
    iter: &'a mut ParseTokenIter<'b>,

    // TODO: Rework how line_nr and column_nr are stored/parsed during parsing.
    line_nr: u64,
    column_nr: u64,
}

impl<'a, 'b> KeyworkParser<'a, 'b> {
    pub fn parse(
        iter: &'a mut ParseTokenIter<'b>,
        keyword: Kw,
        line_nr: u64,
        column_nr: u64,
    ) -> CustomResult<AstToken> {
        let mut keyword_parser = Self {
            iter,
            line_nr,
            column_nr,
        };
        keyword_parser.parse_keyword(keyword)
    }

    fn parse_keyword(&mut self, keyword: Kw) -> CustomResult<AstToken> {
        let token = match keyword {
            // Parses all the else(x)/else blocks after aswell.
            Kw::If => self.parse_if(),
            Kw::Else => Err(self.iter.err("Else keyword in keyword parser.".into())),
            Kw::Match => self.parse_match(),

            // Blocks returns AstTokens instead of Token, so need to do early return.
            Kw::For => return self.parse_for(),
            Kw::While => return self.parse_while(),
            Kw::Implement => return self.parse_impl(),
            Kw::Function => return self.parse_func(),

            Kw::Return => self.parse_return(),
            Kw::Yield => self.parse_yield(),
            Kw::Break => self.parse_break(),
            Kw::Continue => self.parse_continue(),

            Kw::Use => self.parse_use(),
            Kw::Package => self.parse_package(),
            Kw::External => self.parse_external(),

            Kw::Var => self.parse_var_decl(),
            Kw::Const => {
                // TODO: self.parse_const_decl()
                self.parse_var_decl()
            }
            Kw::Static => Err(self.iter.err("\"Static\" keyword not implemented.".into())),
            Kw::Private => Err(self.iter.err("\"Private\" keyword not implemented.".into())),
            Kw::Public => Err(self.iter.err("\"Public\" keyword not implemented.".into())),

            Kw::Struct => self.parse_struct(),
            Kw::Enum => Err(self.iter.err("\"Enum\" keyword not implemented.".into())),
            Kw::Interface => Err(self
                .iter
                .err("\"Interface\" keyword not implemented.".into())),

            Kw::Defer => self.parse_defer(),

            Kw::Test => Err(self.iter.err("\"Test\" keyword not implemented.".into())),
        }?;

        Ok(AstToken {
            token,
            line_nr: self.line_nr,
            column_nr: self.column_nr,
        })
    }

    /// Parses the matching `IfCase`s into a `If` block. This includes all "else"
    /// blocks aswell.
    ///   "if <expr> { ... } [ [ else <expr> { ... } ] else { ... } ]"
    /// The "if" keyword has already been consumed when this function is called.
    fn parse_if(&mut self) -> CustomResult<Token> {
        let mut if_cases = Vec::new();
        let block_id = self.iter.reserve_block_id();

        loop {
            // If the next lex token is a "CurlyBracketBegin", no expression is
            // given after this "if case". Assume this is the ending "else".
            // Otherwise parse the expression.
            let expr = if let Some(lex_token) = self.iter.peek_skip_space_line() {
                if let LexTokenKind::Sym(Sym::CurlyBracketBegin) = lex_token.kind {
                    None
                } else {
                    Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None when looking at \"if case\" expr.".into()));
            };
            let header = BlockHeader::IfCase(expr);

            let if_case = self.iter.next_block(header)?;
            if_cases.push(if_case);

            // See if the next token is the "else" keyword indicating that this
            // function should keep parsing the "if cases", otherwise it is
            // time to break and return.
            if let Some(lex_token) = self.iter.peek_skip_space_line() {
                if let LexTokenKind::Kw(Kw::Else) = lex_token.kind {
                    self.iter.next_skip_space_line(); // Skip the "else" keyword.
                    continue;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(Token::Block(BlockHeader::If, block_id, if_cases))
    }

    /// Parses a `Match` block and all its cases.
    ///   "match <expr> { <expr> { ... } [...] }"
    /// The "match" keyword has already been consumed when this function is called.
    fn parse_match(&mut self) -> CustomResult<Token> {
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
                if let LexTokenKind::Sym(Sym::CurlyBracketEnd) = lex_token.kind {
                    self.iter.next_skip_space_line(); // Skip the "CurlyBracketEnd".
                    break;
                }
            }
        }

        Ok(Token::Block(
            BlockHeader::Match(match_expr),
            block_id,
            match_cases,
        ))
    }

    /// Parses a for loop block.
    ///   "for <var> in <expr> { ... }"
    /// The "for" keyword has already been consumed when this function is called.
    fn parse_for(&mut self) -> CustomResult<AstToken> {
        let ident = if let Some(lex_token) = self.iter.next_skip_space_line() {
            if let LexTokenKind::Ident(ident) = lex_token.kind {
                ident
            } else {
                return Err(self.iter.err(format!(
                    "Not ident when parsing \"for\" variable: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self.iter.err("None when parsing \"for\" variable.".into()));
        };
        let var = self.iter.parse_var_type(&ident)?;

        // Ensure that the next token is a "In".
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::In) = lex_token.kind {
                // Do nothing, everything OK.
            } else {
                return Err(self
                    .iter
                    .err(format!("Expected \"In\" after for, got: {:?}.", lex_token)));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking after \"for\"s In symbol.".into()));
        }

        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;

        let header = BlockHeader::For(var, expr);
        self.iter.next_block(header)
    }

    /// Parses a while loop block.
    ///   "while <expr> { ... }"
    ///   "while { ... }"
    /// The "while" keyword has already been consumed when this function is called.
    fn parse_while(&mut self) -> CustomResult<AstToken> {
        // If the next lex token is a "CurlyBracketBegin", no expression is
        // given after this "while" keyword. Assume that it means a infinite
        // loop (equivalent to "while(true)").
        let expr = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Sym(Sym::CurlyBracketBegin) = lex_token.kind {
                None
            } else {
                Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at \"while\" expr.".into()));
        };

        let header = BlockHeader::While(expr);
        self.iter.next_block(header)
    }

    /// Parses a return statement.
    ///   "return <expr>"
    ///   "return"
    /// The "return" keyword has already been consumed when this function is called.
    fn parse_return(&mut self) -> CustomResult<Token> {
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

        Ok(Token::Stmt(Stmt::Return(expr)))
    }

    /// Parses a yield statement.
    ///   "yield <expr>"
    /// The "yield" keyword has already been consumed when this function is called.
    fn parse_yield(&mut self) -> CustomResult<Token> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        Ok(Token::Stmt(Stmt::Yield(expr)))
    }

    /// Parses a break statement.
    ///   "break"
    /// The "break" keyword has already been consumed when this function is called.
    fn parse_break(&mut self) -> CustomResult<Token> {
        Ok(Token::Stmt(Stmt::Break))
    }

    /// Parses a continue statement.
    ///   "continue"
    /// The "continue" keyword has already been consumed when this function is called.
    fn parse_continue(&mut self) -> CustomResult<Token> {
        Ok(Token::Stmt(Stmt::Continue))
    }

    /// Parses a use statement.
    ///   "use <path>"  (where path is a dot separated list of idents)
    /// The "use" keyword has already been consumed when this function is called.
    fn parse_use(&mut self) -> CustomResult<Token> {
        let mut path_parts = Vec::new();

        loop {
            // Get the ident from the current path part.
            if let Some(lex_token) = self.iter.next_skip_space() {
                if let LexTokenKind::Ident(ident) = lex_token.kind {
                    path_parts.push(ident);
                } else {
                    return Err(self.iter.err(format!(
                        "Expected ident when parsing \"use\" path, got: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None when looking at \"use\" path.".into()));
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
                    break;
                } else {
                    return Err(self
                        .iter
                        .err("Received None when looking at \"use\" path.".into()));
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None when looking at separator in \"use\" path.".into()));
            }
        }

        let path = Path::new(path_parts);
        Ok(Token::Stmt(Stmt::Use(path)))
    }

    /// Parses a package statement.
    ///   "package <path>"  (where path is a dot separated list of idents)
    /// The "package" keyword has already been consumed when this function is called.
    fn parse_package(&mut self) -> CustomResult<Token> {
        let mut path_parts = Vec::new();

        loop {
            // Get the ident from the current path part.
            if let Some(lex_token) = self.iter.next_skip_space() {
                if let LexTokenKind::Ident(ident) = lex_token.kind {
                    path_parts.push(ident);
                } else {
                    return Err(self.iter.err(format!(
                        "Expected ident when parsing \"package\" path, got: {:?}",
                        lex_token
                    )));
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None when looking at \"package\" path.".into()));
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
                    break;
                } else {
                    return Err(self
                        .iter
                        .err("Received None when looking at \"package\" path.".into()));
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None when looking at separator in \"package\" path.".into()));
            }
        }

        let path = Path::new(path_parts);
        Ok(Token::Stmt(Stmt::Package(path)))
    }

    // TODO: External only valid for functions atm, add for variables.
    /// Parses a external statement.
    ///   "external <function_prototype>"
    /// The "external" keyword has already been consumed when this function is called.
    fn parse_external(&mut self) -> CustomResult<Token> {
        if let Some(lex_token) = self.iter.next_skip_space_line() {
            let func = match lex_token.kind {
                LexTokenKind::Kw(Kw::Function) => self.parse_func_proto()?,
                _ => {
                    return Err(self.iter.err(format!(
                        "Invalid keyword after external keyword: {:?}",
                        lex_token
                    )));
                }
            };

            Ok(Token::Stmt(Stmt::ExternalDecl(Rc::new(RefCell::new(func)))))
        } else {
            Err(self
                .iter
                .err("Received None lex token after external keyword.".into()))
        }
    }

    /// Parses a var statement.
    ///   "var <ident> [: <type>] [= <expr>]"
    /// The "var" keyword has already been consumed when this function is called.
    fn parse_var_decl(&mut self) -> CustomResult<Token> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Ident(ident) = lex_token.kind {
                ident
            } else {
                return Err(self.iter.err(format!(
                    "Expected ident when parsing \"var\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"var\".".into()));
        };

        // If the next token is a "Colon", parse the type.
        let var_type = if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Sym(Sym::Colon) = lex_token.kind {
                self.iter.next_skip_space(); // Consume "Colon".
                Some(self.iter.parse_type(None)?)
            } else {
                None
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"var <ident>\".".into()));
        };

        // If the next token is a "Equals" this is an initializer, parse the
        // next expression which will be the assigned value.
        let expr_opt = if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Sym(Sym::Equals) = lex_token.kind {
                self.iter.next_skip_space(); // Consume "Equals".
                Some(self.iter.parse_expr(&DEFAULT_STOP_CONDS)?)
            } else {
                None
            }
        } else {
            return Err(self.iter.err(
                "Received None when looking at token after \"var <ident> [: <type>]\".".into(),
            ));
        };

        let is_const = false;
        let var = Rc::new(RefCell::new(Var::new(
            ident, var_type, None, None, is_const,
        )));
        let var_decl = Stmt::VariableDecl(var, expr_opt);
        Ok(Token::Stmt(var_decl))
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
    fn parse_func(&mut self) -> CustomResult<AstToken> {
        let func = self.parse_func_proto()?;
        let func_header = BlockHeader::Function(Rc::new(RefCell::new(func)));
        self.iter.next_block(func_header)
    }

    // TODO: Parsing of generics.
    /// Parses a function prototype/header.
    ///   "function [ <modifier>... ] <ident> ( [<ident>: <type>] [= <default value>], ... ) [ "->" <type> ]"
    ///   TODO: "function [ <modifier>... ] <ident> [ < <generic>, ... > ] ( [<ident>: <type>] [= <default value>], ... ) [ "->" <type> ]"
    /// The "function" keyword has already been consumed when this function is called.
    fn parse_func_proto(&mut self) -> CustomResult<Function> {
        let mut modifiers = Vec::new();

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
                                return Err(self.iter.err(format!(
                                    "Expected \"this\" after CurlyBracketBegin in \"function\" header, got: {:?}",
                                    lex_token
                                )));
                            }
                        } else {
                            return Err(self.iter.err(format!(
                                "Expected \"this\" after CurlyBracketBegin in \"function\" header, got: {:?}",
                                lex_token
                            )));
                        }

                        let lex_token = self.iter.next_skip_space_line();
                        if let Some(LexTokenKind::Sym(Sym::CurlyBracketEnd)) =
                            lex_token.as_ref().map(|l| &l.kind)
                        {
                        } else {
                            return Err(self.iter.err(format!(
                                "Expected CurlyBracketEnd after \"this\" in \"function\" header, got: {:?}",
                                lex_token
                            )));
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
                            return Err(self.iter.err(format!(
                                "Invalid keyword when parsing \"function\" header, got: {:?}",
                                lex_token
                            )));
                        }
                    }

                    _ => {
                        return Err(self.iter.err(format!(
                            "Expected ident or keyword when parsing \"function\" header, got: {:?}",
                            lex_token
                        )))
                    }
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None when looking at tokens after \"function\".".into()));
            }
        };

        let start_symbol = Sym::ParenthesisBegin;
        let end_symbol = Sym::ParenthesisEnd;
        let (params, is_var_arg) = self.iter.parse_par_list(start_symbol, end_symbol, None)?;

        // Wrap the params into RC & RefCell.
        let params = params
            .iter()
            .map(|var| Rc::new(RefCell::new(var.clone())))
            .collect::<Vec<_>>();

        // If the next token is a "Arrow" ("->"), assume that the return type
        // of the function is specified afterwards. If there are no arrow,
        // assume that the function returns void.
        let return_type = if let Some(lex_token) = self.iter.peek_skip_space_line() {
            if let LexTokenKind::Sym(Sym::Arrow) = lex_token.kind {
                // Consume the arrow.
                self.iter.next_skip_space_line();
                Some(self.iter.parse_type(None)?)
            } else {
                None
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"function <ident>\".".into()));
        };

        let params_opt = if !params.is_empty() {
            Some(params)
        } else {
            None
        };
        // TODO: Generics.
        let generics = None;
        Ok(Function::new(
            ident,
            generics,
            params_opt,
            return_type,
            modifiers,
            is_var_arg,
        ))
    }

    /// Parses a struct header.
    ///   "struct <ident> [ < <generic>, ... > ] { [<ident>: <type>] [[,] ...] }"
    /// The "struct" keyword has already been consumed when this function is called.
    fn parse_struct(&mut self) -> CustomResult<Token> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space_line() {
            if let LexTokenKind::Ident(ident) = lex_token.kind {
                ident
            } else {
                return Err(self.iter.err(format!(
                    "Expected ident when parsing \"struct\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"struct\".".into()));
        };

        let mut type_parse = TypeParser::new(self.iter, None);
        let generics = type_parse.parse_type_generics(GenericsKind::Decl)?;

        let generics = if !generics.is_empty() {
            Some(&generics)
        } else {
            None
        };

        // Parse the members of the struct.
        let start_symbol = Sym::CurlyBracketBegin;
        let end_symbol = Sym::CurlyBracketEnd;
        let (members, is_var_arg) = self
            .iter
            .parse_par_list(start_symbol, end_symbol, generics)?;

        let mut members = members
            .iter()
            .map(|m| Rc::new(RefCell::new(m.clone())))
            .collect::<Vec<_>>();

        if is_var_arg {
            return Err(self.iter.err(format!(
                "Found invalid var_arg symbol in struct with name: {}",
                &ident
            )));
        }

        for member in &mut members {
            if let Some(ret_ty) = &mut member.borrow_mut().ret_type {
                // Replace any generics with the type "Generic" instead of
                // the type "CompoundType".
                if let Some(gens) = generics {
                    ret_ty.replace_generics(&gens.iter_names().cloned().collect::<Vec<_>>());
                }
            } else {
                self.iter.err(format!(
                    "Member \"{}\" in struct \"{}\" has no type set.",
                    &member.borrow().name,
                    &ident
                ));
            }
        }

        let members_opt = if !members.is_empty() {
            Some(members)
        } else {
            None
        };

        let mut struct_ = Struct::new(ident);
        struct_.generic_params =
            generics.map(|gens| gens.iter_names().cloned().collect::<Vec<_>>());
        struct_.members = members_opt;
        let header = BlockHeader::Struct(Rc::new(RefCell::new(struct_)));

        let block_id = self.iter.reserve_block_id();
        let body = Vec::with_capacity(0);

        Ok(Token::Block(header, block_id, body))
    }

    // TODO: Generics
    /// Parses a implement header.
    ///   "implement <ident> { [<func> ...] }"
    /// The "implement" keyword has already been consumed when this function is called.
    fn parse_impl(&mut self) -> CustomResult<AstToken> {
        // Start by parsing the identifier
        let ident = if let Some(lex_token) = self.iter.next_skip_space_line() {
            if let LexTokenKind::Ident(ident) = lex_token.kind {
                ident
            } else {
                return Err(self.iter.err(format!(
                    "Expected ident when parsing \"implement\", got: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self
                .iter
                .err("Received None when looking at token after \"implement\".".into()));
        };

        let header = BlockHeader::Implement(ident);
        let impl_token = self.iter.next_block(header)?;

        // Iterate through the tokens in the body and make sure that all tokens
        // are functions.
        if let Token::Block(BlockHeader::Implement(_), _, body) = &impl_token.token {
            for ast_token in body {
                if let Token::Block(BlockHeader::Function(_), ..) = ast_token.token {
                    // Do nothing, the token is of correct type.
                } else {
                    return Err(self.iter.err(format!(
                        "Non function parsed in \"implement\" block: {:#?}.",
                        ast_token
                    )));
                }
            }
        } else {
            return Err(self.iter.err(format!(
                "Parsed \"implement\" block not a impl block: {:#?}.",
                impl_token
            )));
        }

        Ok(impl_token)
    }

    /// Parses a defer statement.
    ///   "defer <expr>"
    /// The "defer" keyword has already been consumed when this function is called.
    fn parse_defer(&mut self) -> CustomResult<Token> {
        let expr = self.iter.parse_expr(&DEFAULT_STOP_CONDS)?;
        Ok(Token::Stmt(Stmt::Defer(expr)))
    }
}
