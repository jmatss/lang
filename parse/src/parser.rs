use crate::{
    expr_parser::ExprParser, keyword_parser::KeyworkParser, token::get_if_stmt_op,
    type_parser::TypeParser,
};
use common::{
    error::{CustomResult, LangError, LangErrorKind::ParseError},
    iter::TokenIter,
    token::ast::Token,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{Argument, Expr, Var},
        stmt::{Path, Stmt},
    },
    types::Type,
    BlockId,
};
use lex::token::{Kw, LexToken, LexTokenKind, Sym};
use log::debug;

/// The common stop conditions used when parsing expressions.
pub const DEFAULT_STOP_CONDS: [Sym; 4] = [
    Sym::LineBreak,
    Sym::SemiColon,
    Sym::Comma,
    Sym::CurlyBracketBegin,
];

/// The stop conditions used when one wants to parse either a "regular"
/// expressions or a expression that is the lhs of a assignment.
/// Other that the `DEFAULT_STOP_CONDS` this array will contains all assignment
/// symbols plus the Colon symbol to stop on types.
pub const DEFAULT_ASSIGN_STOP_CONDS: [Sym; 16] = [
    Sym::LineBreak,
    Sym::SemiColon,
    Sym::Comma,
    Sym::CurlyBracketBegin,
    Sym::Colon,
    Sym::Equals,
    Sym::AssignAddition,
    Sym::AssignSubtraction,
    Sym::AssignMultiplication,
    Sym::AssignDivision,
    Sym::AssignModulus,
    Sym::AssignBitAnd,
    Sym::AssignBitOr,
    Sym::AssignBitXor,
    Sym::AssignShiftLeft,
    Sym::AssignShiftRight,
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

    /// Contains references to "use" statements. They will be pushed when parsed
    /// and the popped when they have been included/imported.
    pub uses: Vec<Path>,

    /// Contains the blocks that are children of the "root" block.
    pub root_block_body: Vec<AstToken>,
    pub root_block_id: BlockId,
}

impl Default for ParseTokenIter {
    fn default() -> Self {
        ParseTokenIter::new()
    }
}

impl ParseTokenIter {
    pub fn new() -> Self {
        let root_block_id = 0;
        let start_block_id = 1;

        Self {
            iter: TokenIter::new(Vec::default()),
            block_id: start_block_id,
            cur_line_nr: 1,
            cur_column_nr: 1,
            uses: Vec::default(),
            root_block_body: Vec::default(),
            root_block_id,
        }
    }

    pub fn set_lex_tokens(&mut self, lex_tokens: Vec<LexToken>) {
        self.iter = TokenIter::new(lex_tokens);
    }

    pub fn take_root_block(&mut self) -> AstToken {
        let token = Token::Block(
            BlockHeader::Default,
            self.root_block_id,
            std::mem::take(&mut self.root_block_body),
        );
        AstToken {
            token,
            line_nr: 1,
            column_nr: 1,
        }
    }

    pub fn parse(&mut self) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::new();

        // Keep track of the tokens that have been parsed for the current `parse`
        // call. This is needed so that they can be added infront of the previous
        // tokens in one go. This makes it so that the contents of a file included
        // with a "use" is put before the file containing the "use".
        let mut cur_block_body = Vec::new();

        loop {
            match self.next_token() {
                Ok(parse_token) => {
                    match parse_token.token {
                        Token::EOF => {
                            cur_block_body.push(parse_token);
                            break;
                        }
                        Token::Stmt(Stmt::Use(ref path)) => {
                            self.uses.push(path.clone());
                        }
                        _ => (),
                    }

                    cur_block_body.push(parse_token);
                }
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            // Move all previous tokens to the end of this vec and then replace
            // the old vec with the new one containing all tokens.
            cur_block_body.append(&mut self.root_block_body);
            self.root_block_body = cur_block_body;
            Ok(())
        } else {
            Err(errors)
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
    pub fn next_token(&mut self) -> CustomResult<AstToken> {
        if let Some(lex_token) = self.iter.next() {
            // TODO: Clean up this mess with a mix of ParseToken/ParseTokenKind
            //       (some arms returns ParseTokens, others cascades ParseTokenKinds).

            self.cur_line_nr = lex_token.line_nr;
            self.cur_column_nr = lex_token.column_nr;

            // Skip any "break" and white space symbols. Call this function
            // recursively to get an "actual" token.
            if lex_token.is_break_symbol() {
                return self.next_token();
            } else if let LexTokenKind::Sym(Sym::WhiteSpace(_)) = lex_token.kind {
                return self.next_token();
            }

            Ok(match lex_token.kind {
                LexTokenKind::Kw(keyword) => {
                    self.parse_keyword(keyword, lex_token.line_nr, lex_token.column_nr)?
                }

                // Skip line breaks, white spaces and semi colons. Just return
                // the symbol and let the caller decide what to do with it.
                // Call this function recursively to get an "actual" token.
                LexTokenKind::Sym(Sym::LineBreak)
                | LexTokenKind::Sym(Sym::WhiteSpace(_))
                | LexTokenKind::Sym(Sym::SemiColon) => self.next_token()?,

                // If a "line" starts with a identifier this can either be a
                // assignment to a variable (assignment aren't treated as
                // expression atm) or it will be an expression.
                LexTokenKind::Ident(_) => {
                    // Put back the ident, it is a part of a expression.
                    self.rewind()?;

                    // The first parsed expr can either be the lhs or the rhs.
                    // This will be figure out later in this block.
                    let stop_conds = DEFAULT_ASSIGN_STOP_CONDS;
                    let expr = self.parse_expr(&stop_conds)?;

                    // If the next token after the expr is a assign operator,
                    // this is a assignment. Otherwise, this is just a "regular" expr.
                    let token = if let Some(next) = self.peek_skip_space() {
                        if let Some(assign_op) = get_if_stmt_op(&next) {
                            self.next_skip_space(); // Consume the assign op.
                            let rhs = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                            let stmt = Stmt::Assignment(assign_op, expr, rhs);
                            Token::Stmt(stmt)
                        } else {
                            Token::Expr(expr)
                        }
                    } else {
                        // If there are no more tokens after this expr has been
                        // parsed, there can be no rhs, this is NOT a assignment.
                        Token::Expr(expr)
                    };

                    AstToken {
                        token,
                        line_nr: lex_token.line_nr,
                        column_nr: lex_token.column_nr,
                    }
                }

                // A anonymous block.
                LexTokenKind::Sym(Sym::CurlyBracketBegin) => {
                    // Put back the CurlyBracketBegin, it is expected to be the
                    // first symbol found in the `self.next_block` function.
                    self.rewind()?;
                    self.next_block(BlockHeader::Anonymous)?
                }

                LexTokenKind::Sym(Sym::Increment) => {
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    let token = Token::Stmt(Stmt::Increment(expr));

                    AstToken {
                        token,
                        line_nr: lex_token.line_nr,
                        column_nr: lex_token.column_nr,
                    }
                }
                LexTokenKind::Sym(Sym::Decrement) => {
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    let token = Token::Stmt(Stmt::Decrement(expr));

                    AstToken {
                        token,
                        line_nr: lex_token.line_nr,
                        column_nr: lex_token.column_nr,
                    }
                }

                // Error if the iterator finds a lonely symbol of these types:
                LexTokenKind::Sym(Sym::ParenthesisEnd)
                | LexTokenKind::Sym(Sym::CurlyBracketEnd)
                | LexTokenKind::Sym(Sym::PointyBracketEnd)
                | LexTokenKind::Sym(Sym::SquareBracketEnd) => {
                    let msg = format!(
                        "Found end symbol with no corresponding start: {:?}",
                        lex_token
                    );
                    return Err(self.err(msg));
                }

                // If a literal or symbol is found, one can assume that they
                // belong to a expression. There is a possibility that they are
                // part of a statement, but then they should never end up here.
                LexTokenKind::Lit(_) | LexTokenKind::Sym(_) => {
                    // Put back the token that was just popped and then parse
                    // everything together as an expression.
                    self.rewind()?;
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    let token = Token::Expr(expr);

                    AstToken {
                        token,
                        line_nr: lex_token.line_nr,
                        column_nr: lex_token.column_nr,
                    }
                }

                LexTokenKind::EOF => AstToken {
                    token: Token::EOF,
                    line_nr: lex_token.line_nr,
                    column_nr: lex_token.column_nr,
                },
            })
        } else {
            Ok(AstToken {
                token: Token::EOF,
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            })
        }
    }

    /// Returns the next block containing all its ParseTokens. A block is always
    /// started withh "CurlyBracketBegin" and ended with "CurlyBracketEnd".
    pub fn next_block(&mut self, header: BlockHeader) -> CustomResult<AstToken> {
        let mut block_tokens = Vec::new();
        let block_id = self.reserve_block_id();
        let line_nr: u64;
        let column_nr: u64;

        // Ensure that the block starts with a "CurlyBracketBegin".
        if let Some(lex_token) = self.next_skip_space_line() {
            if let LexTokenKind::Sym(Sym::CurlyBracketBegin) = lex_token.kind {
                line_nr = lex_token.line_nr;
                column_nr = lex_token.column_nr;
            } else {
                return Err(self.err(format!(
                    "Received invalid token at start of block: {:?}",
                    lex_token
                )));
            }
        } else {
            return Err(self.err("Received None at start of block.".into()));
        }

        loop {
            // If the next lex token is a "CurlyBracketEnd", the end of the
            // block have been reached. Break and return.
            if let Some(lex_token) = self.peek_skip_space_line() {
                if let LexTokenKind::Sym(Sym::CurlyBracketEnd) = lex_token.kind {
                    self.next_skip_space_line(); // Consume "CurlyBracketEnd".
                    break;
                }
            }

            let token = self.next_token()?;
            block_tokens.push(token);
        }

        let token = Token::Block(header, block_id, block_tokens);
        Ok(AstToken {
            token,
            line_nr,
            column_nr,
        })
    }

    pub fn parse_keyword(
        &mut self,
        keyword: Kw,
        line_nr: u64,
        column_nr: u64,
    ) -> CustomResult<AstToken> {
        KeyworkParser::parse(self, keyword, line_nr, column_nr)
    }

    pub fn parse_expr(&mut self, stop_conds: &[Sym]) -> CustomResult<Expr> {
        ExprParser::parse(self, stop_conds)
    }

    pub fn parse_type(&mut self, generics: Option<&Vec<String>>) -> CustomResult<Type> {
        TypeParser::parse(self, generics)
    }

    /// Parses a variable and any type that it might have specified after which
    /// would indicate a declaration.
    pub fn parse_var_type(&mut self, ident: &str) -> CustomResult<Var> {
        // If the next token is a colon, a type is specified after this variable.
        // Parse it and set the `var_type` inside the variable.
        let var_type = if let Some(next_token) = self.peek_skip_space() {
            if let LexTokenKind::Sym(Sym::Colon) = next_token.kind {
                self.next_skip_space(); // Skip the colon.
                Some(self.parse_type(None)?)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Var::new(ident.into(), var_type, None, None, false))
    }

    /// Parses a list of arguments. This can be used on generic list containing
    /// arguments ex, function calls and struct initialization.
    ///   "<start_symbol> [ [<ident> =] <expr> [,]] ... <end_symbol>"
    pub fn parse_arg_list(
        &mut self,
        start_symbol: Sym,
        end_symbol: Sym,
    ) -> CustomResult<Vec<Argument>> {
        let mut arguments = Vec::new();

        debug!(
            "Parsing arg list, start symbol: {:?}, end symbol: {:?}",
            start_symbol, end_symbol
        );

        // Skip the first symbol and ensure that it is the `start_symbol`.
        if let Some(start_token) = self.next_skip_space_line() {
            match start_token.kind {
                LexTokenKind::Sym(s) if s == start_symbol => (),
                _ => {
                    return Err(self.err(format!(
                        "Bad start symbol when parsing arg list. Expected: {:?}, got: {:?}",
                        start_symbol, start_token
                    )))
                }
            }
        } else {
            return Err(self.err("Received None when parsing `start_token` in arg list.".into()));
        }

        // Edge case if this arg list contains no items, do early return with
        // empty vector.
        if let Some(next) = self.peek_skip_space_line() {
            match next.kind {
                LexTokenKind::Sym(s) if s == end_symbol => {
                    // Consume the `end_symbol` of the list and return.
                    self.next_skip_space_line();
                    return Ok(arguments);
                }
                _ => (),
            }
        }

        loop {
            // TODO: This assumes that arguments can NOT set a variable
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
            let second_opt = self.peek_skip_space_line_n(1);
            if let (Some(first), Some(second)) = (first_opt, second_opt) {
                if let LexTokenKind::Ident(ident) = &first.kind {
                    if let LexTokenKind::Sym(Sym::Equals) = second.kind {
                        // 1. Named argument.
                        // skip the ident and equals.
                        self.next_skip_space_line();
                        self.next_skip_space_line();

                        name = Some(ident.clone());
                    }
                }

                // Edge case if the stop condition is a parenthesis. The reason
                // is that parenthesis are allowed in expression, so one doesn't
                // want to exit to early in the expression. The "ExprParser"
                // itself has logic internally that stop the parsing if a
                // parenthesis is found that doesn't belong to the expression,
                // so this should not be a problem.
                let arg = if end_symbol == Sym::ParenthesisEnd {
                    Argument::new(name, self.parse_expr(&[Sym::Comma])?)
                } else {
                    Argument::new(name, self.parse_expr(&[Sym::Comma, end_symbol.clone()])?)
                };

                arguments.push(arg);
            } else {
                unreachable!();
            }

            // A argument has just been parsed above. The next character should
            // either be a comma indicating more arguments or `end_symbol`
            // indicating that the list have been parsed fully. To allow
            // for trailing commas, also end parsing if this is a comma and the
            // next token is a `end_symbol`.
            if let Some(lex_token) = self.next_skip_space_line() {
                match lex_token.kind {
                    LexTokenKind::Sym(Sym::Comma) => {
                        // Makes a extra check to allow for trailing commas.
                        if let Some(next) = self.peek_skip_space_line() {
                            match next.kind {
                                LexTokenKind::Sym(s) if s == end_symbol => {
                                    self.next_skip_space_line();
                                    return Ok(arguments);
                                }
                                _ => (),
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(s) if s == end_symbol => {
                        return Ok(arguments);
                    }
                    _ => {
                        return Err(self.err(format!(
                            "Received invalid LexToken at end of argument in list: {:?}",
                            lex_token
                        )))
                    }
                }
            } else {
                return Err(self.err("Received None at end of argument in list.".into()));
            }
        }
    }

    /// Parses a list of parameters. This can be used for a generic lists
    /// contanining variables and their types, ex. function params and structs.
    ///   "<start_symbol> [ <ident> : <type> [= <default value>] [,]] [...] <end_symbol>"
    /// Using a "..." as a argument indicates that this function support
    /// var_args/is variadic.
    /// The returned bool in the tuple indicates if this list contains a
    /// "variadic symbol".
    pub fn parse_par_list(
        &mut self,
        start_symbol: Sym,
        end_symbol: Sym,
        generics: Option<&Vec<String>>,
    ) -> CustomResult<(Vec<Var>, bool)> {
        let mut parameters = Vec::new();
        let mut is_var_arg = false;

        // Skip the first symbol and ensure that it is the `start_symbol`.
        if let Some(start_token) = self.next_skip_space_line() {
            match start_token.kind {
                LexTokenKind::Sym(s) if s == start_symbol => (),
                _ => {
                    return Err(self.err(format!(
                        "Bad start symbol when parsing param list. Expected: {:?}, got: {:?}",
                        start_symbol, start_token
                    )))
                }
            }
        } else {
            return Err(self.err("Received None when parsing `start_token` in param list.".into()));
        }

        // Edge case if this param list contains no items, do early return with
        // empty vector.
        if let Some(next) = self.peek_skip_space_line() {
            match next.kind {
                LexTokenKind::Sym(s) if s == end_symbol => {
                    // Consume the `end_symbol` of the list and return.
                    self.next_skip_space_line();
                    return Ok((parameters, is_var_arg));
                }
                _ => (),
            }
        }

        loop {
            if let Some(lex_token) = self.next_skip_space_line() {
                // Parses either a identifier followed by a type or a "TripleDot"
                // which is the indicator for a variadic function.
                match lex_token.kind {
                    LexTokenKind::Ident(ident) => {
                        let var_type = self.parse_colon_type(generics)?;

                        // Parse any default value for the parameter if it is
                        // specified which is indicated with a equals sign.
                        let default_value =
                            if let Some(peek_lex_token) = self.peek_skip_space_line() {
                                if let LexTokenKind::Sym(Sym::Equals) = peek_lex_token.kind {
                                    self.next_skip_space_line();

                                    // TODO: The default stop conditions stops
                                    //       on a curly bracket begin. This will
                                    //       prevent a struct init to be used as
                                    //       a default value. Is this ok?
                                    Some(Box::new(self.parse_expr(&DEFAULT_STOP_CONDS)?))
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                        let const_ = false;

                        let parameter =
                            Var::new(ident, Some(var_type), None, default_value, const_);
                        parameters.push(parameter);
                    }

                    LexTokenKind::Sym(Sym::TripleDot) => is_var_arg = true,

                    _ => {
                        return Err(self.err(format!(
                            "Invalid token when parsing ident in param list: {:?}",
                            lex_token
                        )));
                    }
                }
            } else {
                return Err(self.err("Received None when parsing ident in param list.".into()));
            };

            // A parameter has just been parsed above. The next character should
            // either be a comma indicating more parameters or `end_symbol`
            // indicating that the list have been parsed fully. To allow
            // for trailing commas, also end parsing if this is a comma and the
            // next token is a `end_symbol`.
            if let Some(lex_token) = self.next_skip_space_line() {
                match lex_token.kind {
                    LexTokenKind::Sym(Sym::Comma) => {
                        // Makes a extra check to allow for trailing commas.
                        if let Some(next) = self.peek_skip_space_line() {
                            match next.kind {
                                LexTokenKind::Sym(s) if s == end_symbol => {
                                    self.next_skip_space_line();
                                    return Ok((parameters, is_var_arg));
                                }
                                _ => (),
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(s) if s == end_symbol => {
                        return Ok((parameters, is_var_arg));
                    }
                    _ => {
                        return Err(self.err(format!(
                            "Received invalid LexToken at end of parameter in list: {:?}",
                            lex_token
                        )))
                    }
                }
            } else {
                return Err(self.err("Received None at end of parameter in list.".into()));
            }
        }
    }

    /// Parses a type including the starting colon.
    fn parse_colon_type(&mut self, generics: Option<&Vec<String>>) -> CustomResult<Type> {
        if let Some(lex_token) = self.next_skip_space() {
            if let LexTokenKind::Sym(Sym::Colon) = lex_token.kind {
                self.parse_type(generics)
            } else {
                Err(self.err(format!(
                    "Invalid token when parsing colon before type: {:?}",
                    lex_token
                )))
            }
        } else {
            Err(self.err("Received None expecting colon at start of type.".into()))
        }
    }

    /// Inserest a item at the current iterator position.
    pub fn insert(&mut self, item: LexToken) {
        self.iter.insert(item);
    }

    /// Removes the item at the current iterator position. Returns the removed item.
    pub fn remove(&mut self) -> LexToken {
        self.iter.remove()
    }

    #[inline]
    pub fn rewind(&mut self) -> CustomResult<()> {
        if self.iter.rewind() {
            if let Some(cur_token) = self.iter.peek() {
                self.cur_line_nr = cur_token.line_nr;
                self.cur_column_nr = cur_token.column_nr;
            }
            Ok(())
        } else {
            Err(self.err("Tried to rewind to before the iterator (pos < 0).".into()))
        }
    }

    #[inline]
    pub fn rewind_skip_space(&mut self) -> CustomResult<()> {
        loop {
            self.rewind()?;

            if let Some(cur_token) = self.iter.peek() {
                match cur_token.kind {
                    LexTokenKind::Sym(Sym::WhiteSpace(_)) => continue,
                    _ => return Ok(()),
                }
            } else {
                return Err(self.err("Got None when peeking during rewind.".into()));
            }
        }
    }

    /// Gets the next item from the iterator that is NOT a white space.
    /// If the item at the current position of the iterator is a white space,
    /// it will be skipped and the item after that will be fetched.
    #[inline]
    pub fn next_skip_space(&mut self) -> Option<LexToken> {
        while let Some(lex_token) = self.iter.next() {
            self.cur_line_nr = lex_token.line_nr;
            self.cur_column_nr = lex_token.column_nr;

            match lex_token.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_)) => {
                    continue;
                }
                _ => return Some(lex_token),
            }
        }

        // The last token should always be a EOF (which isn't a white space or
        // line break) and this point should therefore NOT be reachable.
        unreachable!();
    }

    /// Gets the next item from the iterator that is NOT a white space or a
    /// line break (including semi colon). Will loop until a non white space/line
    /// break is found.
    #[inline]
    pub fn next_skip_space_line(&mut self) -> Option<LexToken> {
        while let Some(lex_token) = self.iter.next() {
            self.cur_line_nr = lex_token.line_nr;
            self.cur_column_nr = lex_token.column_nr;

            match lex_token.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_))
                | LexTokenKind::Sym(Sym::LineBreak)
                | LexTokenKind::Sym(Sym::SemiColon) => {
                    continue;
                }
                _ => return Some(lex_token),
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
        // needs to check for a white space ones, since there is no possiblity
        // that two tokens in a row are white spaces.
        if let Some(tokens) = self.iter.peek_two() {
            if let LexTokenKind::Sym(Sym::WhiteSpace(_)) = tokens.0.kind {
                tokens.1
            } else {
                Some(tokens.0)
            }
        } else {
            None
        }
    }

    /// Peeks and clones the next item from the iterator that is NOT a
    /// white space or a line break (including semi colon). Will loop until a non
    /// white space/line break is found.
    #[inline]
    pub fn peek_skip_space_line(&mut self) -> Option<LexToken> {
        self.peek_skip_space_line_n(0)
    }

    /// Peeks and clones the item `peek_count` items from the current iterator
    /// position. This count does NOT include the space/lines/semiColin. Will
    /// loop until a non white space/line break is found.
    #[inline]
    pub fn peek_skip_space_line_n(&mut self, peek_count: usize) -> Option<LexToken> {
        let mut i = 0;
        let mut cur_count = 0;
        while let Some(current) = self.iter.peek_at_n(i) {
            match current.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_))
                | LexTokenKind::Sym(Sym::LineBreak)
                | LexTokenKind::Sym(Sym::SemiColon) => (),
                _ => {
                    if cur_count == peek_count {
                        return Some(current);
                    }
                    cur_count += 1;
                }
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
    /// When a error is found, the iterator will move forward until a "break"
    /// symbol is found. This is done to try and find a good new starting point
    /// to continue parsing from.
    pub(super) fn err(&mut self, msg: String) -> LangError {
        while let Some(lex_token) = self.iter.next() {
            if lex_token.is_break_symbol() {
                break;
            }
        }
        LangError::new_backtrace(
            msg,
            ParseError {
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            },
            true,
        )
    }
}
