use log::debug;

use common::{
    ctx::{block_ctx::BlockCtx, ty_env::TyEnv},
    error::{LangError, LangErrorKind::ParseError, LangResult},
    file::FilePosition,
    iter::TokenIter,
    path::{LangPath, LangPathBuilder},
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{Argument, Expr, Var},
        stmt::Stmt,
    },
    ty::generics::{Generics, GenericsKind},
    BlockId, TypeId,
};
use lex::token::{Kw, LexToken, LexTokenKind, Sym};

use crate::{
    expr_parser::ExprParser, keyword_parser::KeyworkParser, token::get_if_stmt_op,
    type_parser::TypeParser,
};

/// The common stop conditions used when parsing expressions.
pub const DEFAULT_STOP_CONDS: [Sym; 4] = [
    Sym::LineBreak,
    Sym::SemiColon,
    Sym::Comma,
    Sym::CurlyBracketEnd,
];

/// Stop conditions used when one knows that the expression will be ended with
/// a "CurlyBracketBegin". This allows ex. linebreaks in expressions which allows
/// for some more leeway when writting the expression. These stop conditions
/// should always be preffered over the other ones.
///
/// This is commonly when parsing keywords where the layout is predetermined.
/// For example a expression after a "if" will ALWAYS be ended with a curly
/// bracket.
pub const KEYWORD_STOP_CONDS: [Sym; 1] = [Sym::CurlyBracketBegin];

/// The stop conditions used when one wants to parse either a "regular"
/// expressions or a expression that is the lhs of a assignment.
/// Other that the `DEFAULT_STOP_CONDS` this array will contains all assignment
/// symbols plus the Colon symbol to stop on types.
pub const DEFAULT_ASSIGN_STOP_CONDS: [Sym; 16] = [
    Sym::LineBreak,
    Sym::SemiColon,
    Sym::Comma,
    Sym::CurlyBracketEnd,
    Sym::Colon,
    Sym::Equals,
    Sym::AssignAdd,
    Sym::AssignSub,
    Sym::AssignMul,
    Sym::AssignDiv,
    Sym::AssignMod,
    Sym::AssignBitAnd,
    Sym::AssignBitOr,
    Sym::AssignBitXor,
    Sym::AssignShiftLeft,
    Sym::AssignShiftRight,
];

// TODO: Clean up logic for storing line_nr and column_nr for parser.

pub struct ParseTokenIter<'a> {
    /// Use to iterate over the LexTokens.
    iter: TokenIter<'a, LexToken>,

    /// The ID of the current block. This ID is increment for every block and
    /// every block will be given a unique ID.
    block_id: BlockId,

    /// Contains information about the file position of the current lex token
    /// that is being parsed. This will be used to create better errors messages.
    file_pos: FilePosition,

    /// Contains information about all types.
    pub ty_env: TyEnv,

    /// Contains the blocks that are children of the "default" block.
    pub block_body: Vec<AstToken>,
}

impl<'a> Default for ParseTokenIter<'a> {
    fn default() -> Self {
        ParseTokenIter::new()
    }
}

impl<'a> ParseTokenIter<'a> {
    pub fn new() -> Self {
        let start_block_id = 1;

        Self {
            iter: TokenIter::new(<&mut [LexToken]>::default()),
            block_id: start_block_id,
            file_pos: FilePosition::default(),
            ty_env: TyEnv::default(),
            block_body: Vec::default(),
        }
    }

    /// Called when all files have been parsed and one wants to get the new whole
    /// AST. When this function is called, a EOF is added to the end of the AST.
    pub fn take_root_block(&mut self) -> AstToken {
        self.block_body.push(AstToken::EOF);

        AstToken::Block(
            BlockHeader::Default,
            FilePosition::default(),
            BlockCtx::DEFAULT_BLOCK_ID,
            std::mem::take(&mut self.block_body),
        )
    }

    pub fn parse(&mut self, lex_tokens: &mut [LexToken]) -> Result<(), Vec<LangError>> {
        // Safety: The `lex_tokens` are "leaked" here since they won't outlive
        //         or live as long as this parser, so they can't be stored in
        //         the parser in safe rust. It is safe to leak since `self.iter`
        //         which contains the leaked `lex_tokens` will be replaced
        //         before this function returns and will thus stop being leaked.
        if let Some(slice_ref) = unsafe { (lex_tokens as *mut [LexToken]).as_mut() } {
            self.iter = TokenIter::new(slice_ref);
        } else {
            panic!("Unable to leak lex_tokens.");
        }

        let result = unsafe { self.parse_priv() };

        // Reset the iter to be empty to prevent possible use-after-free.
        self.iter = TokenIter::new(<&mut [LexToken]>::default());

        result
    }

    unsafe fn parse_priv(&mut self) -> Result<(), Vec<LangError>> {
        let mut errors = Vec::new();

        loop {
            match self.next_token() {
                Ok(AstToken::EOF) => {
                    self.block_body.push(AstToken::EOF);
                    break;
                }
                Ok(parse_token) => self.block_body.push(parse_token),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Takes ownership of the current `block_id` so that it can be given to a
    /// block and updates the `self.block_id` to a new unique ID.
    pub fn reserve_block_id(&mut self) -> BlockId {
        let block_id = self.block_id;
        self.block_id += 1;
        block_id
    }

    /// Returns the block id of the current block that is being parsed.
    pub fn current_block_id(&self) -> usize {
        self.block_id - 1
    }

    /// Returns the next ParseToken from the iterator.
    pub fn next_token(&mut self) -> LangResult<AstToken> {
        if let Some(lex_token) = self.next_skip_space_line() {
            // TODO: Clean up this mess with a mix of ParseToken/ParseTokenKind
            //       (some arms returns ParseTokens, others cascades ParseTokenKinds).
            debug!("lex_token: {:#?}", lex_token);

            let mut file_pos = lex_token.file_pos.to_owned();

            Ok(match lex_token.kind {
                LexTokenKind::Kw(keyword) => self.parse_keyword(keyword, lex_token.file_pos)?,

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
                    if let Some(next) = self.peek_skip_space() {
                        if let Some(assign_op) = get_if_stmt_op(&next) {
                            self.next_skip_space(); // Consume the assign op.
                            let rhs = self.parse_expr(&DEFAULT_STOP_CONDS)?;

                            if let Some(file_pos_last) = rhs.file_pos() {
                                // The start of this `file_pos_last` will be the same
                                // as the `file_pos` inside `expr`.
                                file_pos.set_end(file_pos_last)?;
                            } else {
                                unreachable!(
                                    "No file_pos set for expr in rhs of assignment: {:#?}",
                                    rhs
                                );
                            }

                            AstToken::Stmt(Stmt::Assignment(assign_op, expr, rhs, Some(file_pos)))
                        } else {
                            AstToken::Expr(expr)
                        }
                    } else {
                        // If there are no more tokens after this expr has been
                        // parsed, there can be no rhs, this is NOT a assignment.
                        AstToken::Expr(expr)
                    }
                }

                // A anonymous block.
                LexTokenKind::Sym(Sym::CurlyBracketBegin) => {
                    // Put back the CurlyBracketBegin, it is expected to be the
                    // first symbol found in the `self.next_block` function.
                    self.rewind()?;
                    self.next_block(BlockHeader::Anonymous)?
                }

                LexTokenKind::Comment(msg, comment_type) => {
                    AstToken::Comment(msg, comment_type, self.file_pos.to_owned())
                }

                LexTokenKind::Sym(Sym::Increment) => {
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    AstToken::Stmt(Stmt::Increment(expr, Some(self.file_pos.to_owned())))
                }
                LexTokenKind::Sym(Sym::Decrement) => {
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    AstToken::Stmt(Stmt::Decrement(expr, Some(self.file_pos.to_owned())))
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
                    return Err(self.err(msg, Some(lex_token.file_pos)));
                }

                // If a literal or symbol is found, one can assume that they
                // belong to a expression. There is a possibility that they are
                // part of a statement, but then they should never end up here.
                LexTokenKind::Lit(_) | LexTokenKind::Sym(_) => {
                    // Put back the token that was just popped and then parse
                    // everything together as an expression.
                    self.rewind()?;
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;
                    AstToken::Expr(expr)
                }

                LexTokenKind::EOF => AstToken::EOF,
            })
        } else {
            Ok(AstToken::EOF)
        }
    }

    /// Returns the next block containing all its ParseTokens. A block is always
    /// started with "CurlyBracketBegin" and ended with "CurlyBracketEnd".
    pub fn next_block(&mut self, header: BlockHeader) -> LangResult<AstToken> {
        let mut block_tokens = Vec::new();
        let block_id = self.reserve_block_id();

        let mut file_pos = self.peek_file_pos()?;

        // Ensure that the block starts with a "CurlyBracketBegin".
        if let Some(lex_token) = self.next_skip_space_line() {
            if let LexTokenKind::Sym(Sym::CurlyBracketBegin) = lex_token.kind {
            } else {
                return Err(self.err(
                    format!("Received invalid token at start of block: {:?}", lex_token),
                    Some(lex_token.file_pos),
                ));
            }
        } else {
            return Err(self.err("Received None at start of block.".into(), Some(file_pos)));
        }

        loop {
            // If the next lex token is a "CurlyBracketEnd", the end of the
            // block have been reached. Break and return.
            if let Some(lex_token) = self.peek_skip_space_line() {
                if let LexTokenKind::Sym(Sym::CurlyBracketEnd) = lex_token.kind {
                    self.next_skip_space_line(); // Consume "CurlyBracketEnd".

                    file_pos.set_end(&lex_token.file_pos)?;
                    break;
                }
            }

            let token = self.next_token()?;
            block_tokens.push(token);
        }

        Ok(AstToken::Block(header, file_pos, block_id, block_tokens))
    }

    /// Peeks at the next token in the iterator and returns its FilePosition.
    pub fn peek_file_pos(&mut self) -> LangResult<FilePosition> {
        if let Some(lex_token) = self.peek_skip_space() {
            Ok(lex_token.file_pos)
        } else {
            Err(self.err("Iter returned None when peeking FilePosition.".into(), None))
        }
    }

    pub fn parse_keyword(
        &mut self,
        keyword: Kw,
        kw_file_pos: FilePosition,
    ) -> LangResult<AstToken> {
        KeyworkParser::parse(self, keyword, kw_file_pos)
    }

    pub fn parse_expr(&mut self, stop_conds: &[Sym]) -> LangResult<Expr> {
        if let Some(expr) = ExprParser::parse(self, stop_conds)? {
            Ok(expr)
        } else {
            let file_pos = self.iter.peek().map(|token| token.file_pos.to_owned());
            Err(self.err("Expression was empty when not allowed.".into(), file_pos))
        }
    }

    /// Parse the next expression. The next expression is allowed to be empty.
    pub fn parse_expr_allow_empty(&mut self, stop_conds: &[Sym]) -> LangResult<Option<Expr>> {
        ExprParser::parse(self, stop_conds)
    }

    pub fn parse_type(
        &mut self,
        generics: Option<&Generics>,
    ) -> LangResult<(TypeId, FilePosition)> {
        self.parse_type_with_path(generics, LangPathBuilder::default())
    }

    pub fn parse_type_with_path(
        &mut self,
        generics: Option<&Generics>,
        path_builder: LangPathBuilder,
    ) -> LangResult<(TypeId, FilePosition)> {
        TypeParser::parse(self, generics, path_builder)
    }

    // TODO: Currently doesn't handle FilePosition. How should this be done?
    //       The returned value will just contain a None file_pos.
    /// Parses a variable, including type and default value. If `parse_type` is
    /// set, a potential type will be parsed following the given variable.
    /// If `parse_value` is set, a potential value will be parsed following the
    /// given variable.
    pub fn parse_var(
        &mut self,
        ident: &str,
        parse_type: bool,
        parse_value: bool,
        is_const: bool,
        generics: Option<&Generics>,
        mut file_pos: FilePosition,
    ) -> LangResult<Var> {
        // TODO: Handle file_pos.
        let (ty, ty_file_pos) = if let Some(next_token) = self.peek_skip_space() {
            match next_token.kind {
                LexTokenKind::Sym(Sym::Colon) if parse_type => {
                    self.next_skip_space(); // Skip the colon.
                    let (type_id, ty_file_pos) = self.parse_type(generics)?;

                    file_pos.set_end(&ty_file_pos)?;
                    (Some(type_id), Some(ty_file_pos))
                }
                _ => (None, None),
            }
        } else {
            (None, None)
        };

        let value = if let Some(next_token) = self.peek_skip_space() {
            match next_token.kind {
                LexTokenKind::Sym(Sym::Equals) if parse_value => {
                    self.next_skip_space(); // Skip the Equals.
                    let expr = self.parse_expr(&DEFAULT_STOP_CONDS)?;

                    if let Some(file_pos_last) = expr.file_pos() {
                        file_pos.set_end(file_pos_last)?;
                    } else {
                        unreachable!("Value for var doesn't have a file_pos: {:#?}", expr);
                    }

                    Some(Box::new(expr))
                }
                _ => None,
            }
        } else {
            None
        };

        Ok(Var::new(
            ident.into(),
            ty,
            None,
            value,
            Some(file_pos),
            ty_file_pos,
            is_const,
        ))
    }

    /// Parses a list of arguments. This can be used on generic list containing
    /// arguments ex, function calls and struct initialization.
    ///   "<start_symbol> [ [<ident> =] <expr> [,]] ... <end_symbol>"
    pub fn parse_arg_list(
        &mut self,
        start_symbol: Sym,
        end_symbol: Sym,
    ) -> LangResult<(Vec<Argument>, FilePosition)> {
        let mut arguments = Vec::new();

        let mut file_pos = self.peek_file_pos()?;

        debug!(
            "Parsing arg list, start symbol: {:?}, end symbol: {:?}, file_pos: {:#?}",
            start_symbol, end_symbol, file_pos
        );

        // Skip the first symbol and ensure that it is the `start_symbol`.
        if let Some(start_token) = self.next_skip_space_line() {
            match start_token.kind {
                LexTokenKind::Sym(s) if s == start_symbol => (),
                _ => {
                    return Err(self.err(
                        format!(
                            "Bad start symbol when parsing arg list. Expected: {:?}, got: {:?}",
                            start_symbol, start_token
                        ),
                        Some(start_token.file_pos),
                    ))
                }
            }
        } else {
            // TODO: Where should the `file_pos` be fetched from?
            return Err(self.err(
                "Received None when parsing `start_token` in arg list.".into(),
                Some(file_pos),
            ));
        }

        // Edge case if this arg list contains no items, do early return with
        // empty vector.
        if let Some(next) = self.peek_skip_space_line() {
            match next.kind {
                LexTokenKind::Sym(s) if s == end_symbol => {
                    // Consume the `end_symbol` of the list and return.
                    self.next_skip_space_line();

                    file_pos.set_end(&next.file_pos)?;
                    return Ok((arguments, file_pos));
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
            let mut name_file_pos = None;

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
                        name_file_pos = Some(first.file_pos);
                    }
                };

                // Edge case if the stop condition is a parenthesis. The reason
                // is that parenthesis are allowed in expression, so one doesn't
                // want to exit to early in the expression. The "ExprParser"
                // itself has logic internally that stop the parsing if a
                // parenthesis is found that doesn't belong to the expression,
                // so this should not be a problem.
                let expr = if end_symbol == Sym::ParenthesisEnd {
                    self.parse_expr(&[Sym::Comma])?
                } else {
                    self.parse_expr(&[Sym::Comma, end_symbol.clone()])?
                };

                if let Some(name_file_pos) = &mut name_file_pos {
                    if let Some(expr_file_pos) = expr.file_pos() {
                        name_file_pos.set_end(&expr_file_pos)?;
                    }
                }
                let arg = Argument::new(name, name_file_pos, expr);

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

                                    file_pos.set_end(&next.file_pos)?;
                                    return Ok((arguments, file_pos));
                                }
                                _ => (),
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(s) if s == end_symbol => {
                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((arguments, file_pos));
                    }
                    _ => {
                        return Err(self.err(
                            format!(
                                "Received invalid LexToken at end of argument in list: {:?}",
                                lex_token
                            ),
                            Some(lex_token.file_pos),
                        ))
                    }
                }
            } else {
                return Err(self.err(
                    "Received None at end of argument in list.".into(),
                    Some(file_pos),
                ));
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
        generics: Option<&Generics>,
    ) -> LangResult<(Vec<Var>, bool, FilePosition)> {
        let mut parameters = Vec::new();
        let mut is_var_arg = false;

        let mut file_pos = self.peek_file_pos()?;

        // Skip the first symbol and ensure that it is the `start_symbol`.
        if let Some(start_token) = self.next_skip_space_line() {
            match start_token.kind {
                LexTokenKind::Sym(s) if s == start_symbol => (),
                _ => {
                    return Err(self.err(
                        format!(
                            "Bad start symbol when parsing param list. Expected: {:?}, got: {:?}",
                            start_symbol, start_token
                        ),
                        Some(start_token.file_pos),
                    ))
                }
            }
        } else {
            return Err(self.err(
                "Received None when parsing `start_token` in param list.".into(),
                Some(file_pos),
            ));
        }

        // Edge case if this param list contains no items, do early return with
        // empty vector.
        if let Some(next) = self.peek_skip_space_line() {
            match next.kind {
                LexTokenKind::Sym(s) if s == end_symbol => {
                    // Consume the `end_symbol` of the list and return.
                    self.next_skip_space_line();

                    file_pos.set_end(&next.file_pos)?;
                    return Ok((parameters, is_var_arg, file_pos));
                }
                _ => (),
            }
        }

        loop {
            if let Some(lex_token) = self.next_skip_space_line() {
                // Parses either a identifier followed by a type/value or a
                // "TripleDot" which is the indicator for a variadic function.
                match lex_token.kind {
                    LexTokenKind::Ident(ident) => {
                        let parse_type = true;
                        let parse_value = true;
                        let is_const = false;
                        let parameter = self.parse_var(
                            &ident,
                            parse_type,
                            parse_value,
                            is_const,
                            generics,
                            lex_token.file_pos,
                        )?;

                        parameters.push(parameter);
                    }

                    LexTokenKind::Sym(Sym::TripleDot) => is_var_arg = true,

                    _ => {
                        return Err(self.err(
                            format!(
                                "Invalid token when parsing ident in param list: {:?}",
                                lex_token
                            ),
                            Some(lex_token.file_pos),
                        ));
                    }
                }
            } else {
                // TODO: Where should the `file_pos` be fetched from? Can probably
                //       use one of the previous file_poses parsed in the logic above.
                return Err(self.err(
                    "Received None when parsing ident in param list.".into(),
                    Some(file_pos),
                ));
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

                                    file_pos.set_end(&next.file_pos)?;
                                    return Ok((parameters, is_var_arg, file_pos));
                                }
                                _ => (),
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(s) if s == end_symbol => {
                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((parameters, is_var_arg, file_pos));
                    }
                    _ => {
                        return Err(self.err(
                            format!(
                                "Received invalid LexToken at end of parameter in list: {:?}",
                                lex_token
                            ),
                            Some(lex_token.file_pos),
                        ))
                    }
                }
            } else {
                // TODO: Where should the `file_pos` be fetched from? Can probably
                //       use one of the previous file_poses parsed in the logic above.
                return Err(self.err(
                    "Received None at end of parameter in list.".into(),
                    Some(file_pos),
                ));
            }
        }
    }

    /// Parses a double colon separate list of identifiers into a path.
    /// A identifier part of a path may contain a String plus a list of generics.
    pub(crate) fn parse_path(
        &mut self,
        file_pos: &mut FilePosition,
        gens_kind: GenericsKind,
    ) -> LangResult<LangPath> {
        let mut path_builder = LangPathBuilder::default();

        loop {
            let (ident, ident_file_pos) = if let Some(lex_token) = self.next_skip_space() {
                if let LexTokenKind::Ident(ident) = lex_token.kind {
                    (ident, lex_token.file_pos)
                } else {
                    return Err(self.err(
                        format!(
                            "Expected ident when parsing path, got: {:#?}.\nPath builder: {:#?}",
                            lex_token, path_builder
                        ),
                        Some(lex_token.file_pos),
                    ));
                }
            } else {
                return Err(self.err(
                    format!("Got None when parsing path: {:#?}", path_builder),
                    Some(*file_pos),
                ));
            };

            file_pos.set_end(&ident_file_pos)?;

            let next_token_kind = self.peek_skip_space().map(|t| t.kind);
            if let Some(LexTokenKind::Sym(Sym::PointyBracketBegin)) = next_token_kind {
                let mut type_parser = TypeParser::new(self, None);
                let (gens, gens_file_pos) = type_parser.parse_type_generics(gens_kind.clone())?;

                if let Some(gens_file_pos) = gens_file_pos {
                    file_pos.set_end(&gens_file_pos)?;
                }

                let generics = if let Some(gens) = gens {
                    gens
                } else {
                    Generics::empty()
                };

                path_builder.add_path_gen(&ident, &generics);
            } else {
                path_builder.add_path(&ident);
            }

            // If the next token is a double colon, continue parsing the path.
            // If a end symbol is found, the end of the path has been found,
            // break out of this loop and return from the function.
            // Otherwise, found unexpected symbol, something has gone wrong.
            if let Some(lex_token) = self.peek_skip_space() {
                if let LexTokenKind::Sym(Sym::DoubleColon) = lex_token.kind {
                    self.next_skip_space();
                    continue;
                /*
                } else if lex_token.is_break_symbol() {
                    self.next_skip_space();
                    break;
                */
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        path_builder.file_pos(*file_pos);
        Ok(path_builder.build())
    }

    /// Replaces the lex token at the current position with the value of `item`.
    /// Returns the old token that was replaced.
    pub fn replace(&mut self, item: LexToken) -> Option<LexToken> {
        self.iter.replace(item)
    }

    #[inline]
    pub fn pos(&mut self) -> usize {
        self.iter.pos()
    }

    #[inline]
    pub fn rewind(&mut self) -> LangResult<()> {
        if self.iter.rewind() {
            if let Some(cur_token) = self.iter.peek() {
                self.file_pos = cur_token.file_pos;
            }
            Ok(())
        } else {
            Err(self.err(
                "Tried to rewind to before the iterator (pos < 0).".into(),
                None,
            ))
        }
    }

    #[inline]
    pub fn rewind_to_pos(&mut self, mark: usize) {
        self.iter.rewind_to_pos(mark);
    }

    #[inline]
    pub fn rewind_skip_space(&mut self) -> LangResult<()> {
        loop {
            self.rewind()?;

            if let Some(cur_token) = self.iter.peek() {
                match cur_token.kind {
                    LexTokenKind::Sym(Sym::WhiteSpace(_)) => continue,
                    _ => return Ok(()),
                }
            } else {
                return Err(self.err("Got None when peeking during rewind.".into(), None));
            }
        }
    }

    /// Gets the next item from the iterator that is NOT a white space.
    /// If the item at the current position of the iterator is a white space,
    /// it will be skipped and the item after that will be fetched.
    /// Also skips comments.
    #[inline]
    pub fn next_skip_space(&mut self) -> Option<LexToken> {
        while let Some(lex_token) = self.iter.next() {
            self.file_pos = lex_token.file_pos;

            match lex_token.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_)) | LexTokenKind::Comment(..) => {
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
    /// Also skips comments.
    #[inline]
    pub fn next_skip_space_line(&mut self) -> Option<LexToken> {
        while let Some(lex_token) = self.iter.next() {
            self.file_pos = lex_token.file_pos;

            match lex_token.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_))
                | LexTokenKind::Sym(Sym::LineBreak)
                | LexTokenKind::Sym(Sym::SemiColon)
                | LexTokenKind::Comment(..) => {
                    continue;
                }
                _ => return Some(lex_token),
            }
        }

        // The last token should always be a EOF (which isn't a white space or
        // line break) and this point should therefore NOT be reachable.
        unreachable!();
    }

    /// Peeks and clones the item behind the current position of the iterator
    /// that is NOT a white space.
    /// Also skips comments.
    #[inline]
    pub fn peek_skip_space(&mut self) -> Option<LexToken> {
        let mut i = 0;
        while let Some(current) = self.iter.peek_at_n(i) {
            match current.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_)) | LexTokenKind::Comment(..) => (),
                _ => {
                    return Some(current);
                }
            }
            i += 1;
        }
        None
    }

    /// Peeks and clones the next item from the iterator that is NOT a
    /// white space or a line break (including semi colon). Will loop until a non
    /// white space/line break is found.
    /// Also skips comments.
    #[inline]
    pub fn peek_skip_space_line(&mut self) -> Option<LexToken> {
        self.peek_skip_space_line_n(0)
    }

    /// Peeks and clones the item `peek_count` items from the current iterator
    /// position. This count does NOT include the space/lines/semiColin. Will
    /// loop until a non white space/line break is found.
    /// Also skips comments.
    #[inline]
    pub fn peek_skip_space_line_n(&mut self, peek_count: usize) -> Option<LexToken> {
        let mut i = 0;
        let mut cur_count = 0;
        while let Some(current) = self.iter.peek_at_n(i) {
            match current.kind {
                LexTokenKind::Sym(Sym::WhiteSpace(_))
                | LexTokenKind::Sym(Sym::LineBreak)
                | LexTokenKind::Sym(Sym::SemiColon)
                | LexTokenKind::Comment(..) => (),
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
    pub(super) fn err(&mut self, msg: String, file_pos: Option<FilePosition>) -> LangError {
        while let Some(lex_token) = self.iter.next() {
            if lex_token.is_break_symbol() {
                break;
            } else if lex_token.is_eof() {
                self.iter.rewind();
                break;
            }
        }
        LangError::new(msg, ParseError, file_pos)
    }
}
