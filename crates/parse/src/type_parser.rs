use common::{
    error::LangResult,
    file::FilePosition,
    path::LangPathBuilder,
    token::expr::Expr,
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        to_string::to_string_path,
        ty::Ty,
        ty_env::TyEnv,
        type_id::TypeId,
        type_info::TypeInfo,
    },
};
use lex::token::{Kw, LexToken, LexTokenKind, Sym};

use crate::parser::{ParseTokenIter, DEFAULT_ASSIGN_STOP_CONDS};

pub struct TypeParser<'a, 'b> {
    iter: &'a mut ParseTokenIter<'b>,
    generics: Option<&'a Generics>,
}

impl<'a, 'b> TypeParser<'a, 'b> {
    pub fn new(iter: &'a mut ParseTokenIter<'b>, generics: Option<&'a Generics>) -> Self {
        Self { iter, generics }
    }

    pub fn parse(
        iter: &'a mut ParseTokenIter<'b>,
        generics: Option<&'a Generics>,
        path_builder: LangPathBuilder,
    ) -> LangResult<(TypeId, FilePosition)> {
        Self::new(iter, generics).parse_type_with_path(path_builder)
    }

    fn parse_type(&mut self) -> LangResult<(TypeId, FilePosition)> {
        self.parse_type_with_path(LangPathBuilder::default())
    }

    /// Valid type formats:
    ///   X            // Basic type.
    ///   ns::X        // Basic type with added module/namespace.
    ///   {X}          // Pointer to type (is the {X} syntax weird/ambiguous (?))
    ///   X<T>         // Type with generic argument.
    ///   X<T, V>      // Type with multiple generic arguments.
    ///   [X]          // Array of type X with unknown size (slice).
    ///   [X: 3]       // Array of type X with size 3.
    ///   [X: _]       // Array of type X with infered size.
    ///   @f<G>(T)     // Built in func with name f, generic G and arg type T.
    ///   fn(T) -> R   // Function ptr with arg type T and return type R.
    ///
    /// Test examples:
    ///   C                     Lang
    ///   uint32_t *(*x)[]      x: {[{u32}]}
    ///   char *x               x: {char}
    fn parse_type_with_path(
        &mut self,
        mut path_builder: LangPathBuilder,
    ) -> LangResult<(TypeId, FilePosition)> {
        let mut file_pos = self.iter.peek_file_pos()?;

        if let Some(lex_token) = self.iter.next_skip_space() {
            match lex_token.kind {
                // Ident.
                LexTokenKind::Ident(ref ident) => {
                    path_builder.add_path(ident).file_pos(file_pos);

                    if let Some(LexTokenKind::Sym(Sym::DoubleColon)) =
                        self.iter.peek_skip_space().map(|t| t.kind)
                    {
                        // This is a module/name space ident, call this function
                        // recursively to parse the whole type.
                        self.iter.next_skip_space();
                        let (type_id, ty_file_pos) = self.parse_type_with_path(path_builder)?;
                        file_pos.set_end(&ty_file_pos)?;

                        Ok((type_id, file_pos))
                    } else {
                        // This is a ident that represents a name of a type/ADT.
                        let generics = match self.parse_type_generics(GenericsKind::Impl)? {
                            (generics, Some(file_pos_last)) => {
                                file_pos.set_end(&file_pos_last)?;
                                generics
                            }
                            (generics, None) => generics,
                        };
                        let generics = generics.unwrap();

                        // Wrap the current ident into a "Generic" if it exists in
                        // the `self.generics` map. Otherwise return it as a
                        // "UnknownIdent" wrapped in a "CompoundType".
                        if let Some(true) = self.generics.map(|g| g.contains(ident)) {
                            if !generics.is_empty() {
                                panic!("TODO: Generic decl in type has generics itself.");
                            }

                            let unique_id = self.iter.ty_env.lock().unwrap().new_unique_id();
                            let type_info = TypeInfo::Generic(file_pos);
                            Ok((
                                self.iter.ty_env.lock().unwrap().id(&Ty::Generic(
                                    ident.clone(),
                                    unique_id,
                                    type_info,
                                ))?,
                                file_pos,
                            ))
                        } else {
                            // TODO: Remove need for this.
                            let tmp_ty_env = TyEnv::default();

                            let inner_ty = InnerTy::ident_to_type(
                                &to_string_path(&tmp_ty_env, &path_builder.build()),
                                self.iter.current_block_id(),
                            );

                            Ok((
                                self.iter.ty_env.lock().unwrap().id(&Ty::CompoundType(
                                    inner_ty,
                                    generics,
                                    TypeInfo::Default(file_pos),
                                ))?,
                                file_pos,
                            ))
                        }
                    }
                }

                // Pointer.
                LexTokenKind::Sym(Sym::CurlyBracketBegin) => {
                    self.iter.rewind_skip_space()?; // Rewind the CurlyBracketBegin.

                    let (type_id, ty_file_pos) = self.parse_type_pointer()?;
                    file_pos.set_end(&ty_file_pos)?;

                    Ok((type_id, ty_file_pos))
                }

                // Array/slice.
                LexTokenKind::Sym(Sym::SquareBracketBegin) => {
                    self.iter.rewind_skip_space()?; // Rewind the SquareBracketBegin.

                    let (type_id, ty_file_pos) = self.parse_type_array()?;
                    file_pos.set_end(&ty_file_pos)?;

                    Ok((type_id, file_pos))
                }

                // Function pointer.
                LexTokenKind::Kw(Kw::Function) => {
                    // The generics will be used when using the function to make
                    // a call inside the body. It will be appended to the name of
                    // the function call as usual.
                    let generics = match self.parse_type_generics(GenericsKind::Impl)? {
                        (generics, Some(file_pos_last)) => {
                            file_pos.set_end(&file_pos_last)?;
                            generics
                        }
                        (generics, None) => {
                            file_pos.set_end(&lex_token.file_pos)?;
                            generics
                        }
                    };
                    let gens = generics.unwrap().iter_types().cloned().collect::<Vec<_>>();

                    let start_symbol = Sym::ParenthesisBegin;
                    let end_symbol = Sym::ParenthesisEnd;
                    let (params, params_file_pos) =
                        self.parse_type_list(start_symbol, end_symbol)?;
                    file_pos.set_end(&params_file_pos)?;

                    // If the next token is a "Arrow" ("->"), assume that the return type
                    // of the function is specified afterwards. If there are no arrow,
                    // assume that the function returns void.
                    let (ret_type_id, ret_ty_file_pos) =
                        if let Some(lex_token) = self.iter.peek_skip_space_line() {
                            if let LexTokenKind::Sym(Sym::Arrow) = lex_token.kind {
                                self.iter.next_skip_space_line(); // Consume the arrow.
                                let (ret_type_id, ret_file_pos) = self.iter.parse_type(None)?;

                                (Some(ret_type_id), Some(ret_file_pos))
                            } else {
                                (None, None)
                            }
                        } else {
                            return Err(self.iter.err(
                                "Received None when looking for ret ty for function pointer token."
                                    .into(),
                                Some(file_pos),
                            ));
                        };

                    if let Some(ret_ty_file_pos) = ret_ty_file_pos {
                        file_pos.set_end(&ret_ty_file_pos)?;
                    }

                    Ok((
                        self.iter.ty_env.lock().unwrap().id(&Ty::Fn(
                            gens,
                            params,
                            ret_type_id,
                            TypeInfo::Default(file_pos),
                        ))?,
                        file_pos,
                    ))
                }

                // Built in call.
                LexTokenKind::Sym(Sym::At) => {
                    self.iter.rewind_skip_space()?; // Rewind the At.

                    // Need to add stop conditions for if the the start/end of
                    // a generic list is reached.
                    let mut stop_conds = DEFAULT_ASSIGN_STOP_CONDS.to_vec();
                    stop_conds.push(Sym::PointyBracketBegin);
                    stop_conds.push(Sym::PointyBracketEnd);
                    stop_conds.push(Sym::ShiftRight);
                    let expr = self.iter.parse_expr(&stop_conds)?;

                    if let Some(file_pos_last) = expr.file_pos() {
                        file_pos.set_end(&file_pos_last)?;
                    } else {
                        unreachable!("file_pos not set for expr: {:#?}", expr);
                    }

                    if let Expr::BuiltInCall(..) = expr {
                        Ok((
                            self.iter
                                .ty_env
                                .lock()
                                .unwrap()
                                .id(&Ty::Expr(Box::new(expr), TypeInfo::Default(file_pos)))?,
                            file_pos,
                        ))
                    } else {
                        Err(self.iter.err(
                            format!("\"At\"(@) in type NOT built-in call, was: {:#?}", expr),
                            Some(lex_token.file_pos),
                        ))
                    }
                }

                _ => Err(self.iter.err(
                    format!("Invalid type token: {:?}", lex_token),
                    Some(lex_token.file_pos),
                )),
            }
        } else {
            // TODO: Where to get file_pos from?
            Err(self
                .iter
                .err("Received None when parsing type.".into(), None))
        }
    }

    /// Parses a list of types inside a generic "tag" (<..>).
    ///   X<T>      // Type with generic argument.
    ///   X<T, V>   // Type with multiple generic arguments.
    pub(crate) fn parse_type_generics(
        &mut self,
        kind: GenericsKind,
    ) -> LangResult<(Option<Generics>, Option<FilePosition>)> {
        let mut generics = Generics::new();

        let mut file_pos = self.iter.peek_file_pos()?;
        let pos = self.iter.pos();

        // If the next token isn't a "PointyBracketBegin" there are no generic
        // list, just return a empty generics.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if !matches!(lex_token.kind, LexTokenKind::Sym(Sym::PointyBracketBegin)) {
                self.iter.rewind_to_pos(pos);
                return Ok((Some(generics), None));
            }
        }

        // Sanity check to see if this is a generic list with no items inside.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                LexTokenKind::Sym(Sym::PointyBracketEnd) | LexTokenKind::Sym(Sym::ShiftRight) => {
                    return Err(self
                        .iter
                        .err("Empty generic list.".into(), Some(lex_token.file_pos)));
                }
                _ => (),
            }
        }

        loop {
            // Parse the next item in the list as either a identifier(name) or
            // a type depending if this is a decl or impl generic list.
            let cur_file_pos = match kind {
                GenericsKind::Decl => {
                    let (ident, cur_file_pos) = self.next_ident()?;
                    generics.insert_name(ident);
                    cur_file_pos
                }
                GenericsKind::Impl => {
                    let (type_id, cur_file_pos) = self.parse_type()?;
                    generics.insert_type(type_id);
                    cur_file_pos
                }
                GenericsKind::Empty => file_pos.to_owned(),
            };

            let pos = self.iter.pos();

            // End of a type in the generic list. The next token should either
            // be a comma if there are more arguments in the list or a
            // "PointyBracketEnd" if the generic list have been parsed fully.
            // It might also be a "ShiftRight" if this it is two "PointyBracketEnd"
            // following each other, need to rewrite the token in that case.
            if let Some(lex_token) = self.iter.next_skip_space() {
                match lex_token.kind {
                    LexTokenKind::Sym(Sym::Comma) => {
                        // Makes a extra check to allow for trailing commas.
                        if let Some(next) = self.iter.peek_skip_space_line() {
                            if let LexTokenKind::Sym(Sym::PointyBracketEnd) = next.kind {
                                self.iter.next_skip_space_line();

                                file_pos.set_end(&next.file_pos)?;
                                return Ok((Some(generics), Some(file_pos)));
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(Sym::PointyBracketEnd) => {
                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((Some(generics), Some(file_pos)));
                    }
                    LexTokenKind::Sym(Sym::ShiftRight) => {
                        self.iter.rewind_to_pos(pos);

                        let kind = LexTokenKind::Sym(Sym::PointyBracketEnd);
                        let token = LexToken::new(kind, lex_token.file_pos.to_owned());
                        self.iter.replace(token);

                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((Some(generics), Some(file_pos)));
                    }
                    _ => {
                        return Err(self.iter.err(
                            format!(
                                "Received unexpected token after argument in generic list: {:?}",
                                lex_token
                            ),
                            Some(lex_token.file_pos),
                        ));
                    }
                }
            } else {
                return Err(self.iter.err(
                    "Received None after argument in generic list.".into(),
                    Some(cur_file_pos),
                ));
            }
        }
    }

    // TODO: Merge this logic with the parsing of generic lists.
    /// Parses a list of types between a `start_symbol` and `end_symbol`.
    /// This can for example be point bracket when parsing a generic list or
    /// parethesis when parsing a function pointer argument list.
    pub(crate) fn parse_type_list(
        &mut self,
        start_symbol: Sym,
        end_symbol: Sym,
    ) -> LangResult<(Vec<TypeId>, FilePosition)> {
        let mut type_ids = Vec::default();

        let mut file_pos = self.iter.peek_file_pos()?;

        // If the next token isn't a `start_symbol` there are no type list,
        // something has gone wrong.
        if let Some(lex_token) = self.iter.next_skip_space() {
            match lex_token.kind {
                LexTokenKind::Sym(sym) if sym == start_symbol => {
                    // Do nothing, parse type list in logic underneath.
                }
                _ => {
                    return Err(self.iter.err(format!("Expected start_symbol \"{:#?}\" when parsing start of type list, got: {:#?}", start_symbol, lex_token), Some(file_pos)));
                }
            }
        }

        // If this is a empty type list, do early return with empty list.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                LexTokenKind::Sym(sym) if sym == end_symbol => {
                    self.iter.next_skip_space();

                    file_pos.set_end(&lex_token.file_pos)?;
                    return Ok((Vec::with_capacity(0), file_pos));
                }
                _ => (),
            }
        }

        loop {
            let (type_id, ty_file_pos) = self.parse_type()?;
            type_ids.push(type_id);

            file_pos.set_end(&ty_file_pos)?;

            if let Some(lex_token) = self.iter.next_skip_space() {
                match lex_token.kind {
                    LexTokenKind::Sym(Sym::Comma) => {
                        // Makes a extra check to allow for trailing commas.
                        if let Some(next) = self.iter.peek_skip_space_line() {
                            match next.kind {
                                LexTokenKind::Sym(sym) if sym == end_symbol => {
                                    self.iter.next_skip_space_line();

                                    file_pos.set_end(&next.file_pos)?;
                                    return Ok((type_ids, file_pos));
                                }
                                _ => (),
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(sym) if sym == end_symbol => {
                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((type_ids, file_pos));
                    }
                    _ => {
                        return Err(self.iter.err(
                            format!(
                                "Received unexpected token after argument in type list: {:?}",
                                lex_token
                            ),
                            Some(lex_token.file_pos),
                        ));
                    }
                }
            } else {
                return Err(self.iter.err(
                    "Received None after argument in type list.".into(),
                    Some(ty_file_pos),
                ));
            }
        }
    }

    /// Gets the next lex token and assumes that it is a identifier. If it isn't,
    /// a error will be returned.
    fn next_ident(&mut self) -> LangResult<(String, FilePosition)> {
        if let Some(lex_token) = self.iter.next_skip_space_line() {
            if let LexTokenKind::Ident(ident) = lex_token.kind {
                Ok((ident, lex_token.file_pos))
            } else {
                Err(self.iter.err(
                    format!(
                        "Expected next to be ident in generic decl, was: {:?}",
                        lex_token
                    ),
                    Some(lex_token.file_pos),
                ))
            }
        } else {
            // TODO: Where to fetch file_pos from?
            Err(self
                .iter
                .err("Got None when parsing generic decl.".into(), None))
        }
    }

    /// Parses a pointer type.
    ///   {X}       // Pointer to type (is the {X} syntax be weird/ambiguous?)
    fn parse_type_pointer(&mut self) -> LangResult<(TypeId, FilePosition)> {
        let mut file_pos = self.iter.peek_file_pos()?;

        self.iter.next_skip_space(); // Consume the CurlyBracketBegin.
        let (type_id, ty_file_pos) = self.parse_type()?;

        // At this point the token should be a "CurlyBracketEnd" since this is
        // the end of the pointer.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::CurlyBracketEnd) = lex_token.kind {
                file_pos.set_end(&lex_token.file_pos)?;
                Ok((
                    self.iter
                        .ty_env
                        .lock()
                        .unwrap()
                        .id(&Ty::Pointer(type_id, TypeInfo::Default(file_pos)))?,
                    file_pos,
                ))
            } else {
                Err(self.iter.err(
                    format!(
                        "Expected curlybrace at end of pointer type, got: {:?}.",
                        lex_token
                    ),
                    Some(lex_token.file_pos),
                ))
            }
        } else {
            // TODO: Where to fetch file_pos from?
            Err(self.iter.err(
                "Received None at end of pointer type.".into(),
                Some(ty_file_pos),
            ))
        }
    }

    /// Parses array type.
    ///   [X]       // Array of type X with unknown size (slice).
    ///   [X: 3]    // Array of type X with size 3.
    ///   [X: _]    // Array of type X with infered size.
    fn parse_type_array(&mut self) -> LangResult<(TypeId, FilePosition)> {
        let mut file_pos = self.iter.peek_file_pos()?;

        self.iter.next_skip_space(); // Consume the SquareBracketBegin.
        let (arr_type_id, arr_ty_file_pos) = self.parse_type()?;

        file_pos.set_end(&arr_ty_file_pos)?;

        // At this point the token can either be a "Colon" to indicate that this
        // array type has a size specificed or it can be a "SquareBracketEnd"
        // which indicates the end of this array.
        let size = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::SquareBracketEnd) = lex_token.kind {
                None
            } else if let LexTokenKind::Sym(Sym::Colon) = lex_token.kind {
                let stop_conds = [Sym::SquareBracketEnd];
                let expr = self.iter.parse_expr(&stop_conds)?;

                if let Some(expr_file_pos) = expr.file_pos() {
                    file_pos.set_end(expr_file_pos)?;
                } else {
                    unreachable!();
                }

                Some(Box::new(expr))
            } else {
                // TODO: Where to fetch file_pos from?
                return Err(self.iter.err(
                    format!("Received invalid token in array type: {:?}.", lex_token),
                    Some(lex_token.file_pos),
                ));
            }
        } else {
            // TODO: Where to fetch file_pos from?
            return Err(self
                .iter
                .err("Received None at end of array type.".into(), Some(file_pos)));
        };

        // The next token must be a "SquareBracketEnd" or something has
        // gone wrong.
        if let Some(next_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::SquareBracketEnd) = next_token.kind {
                file_pos.set_end(&next_token.file_pos)?;
                Ok((
                    self.iter.ty_env.lock().unwrap().id(&Ty::Array(
                        arr_type_id,
                        size,
                        TypeInfo::Default(file_pos),
                    ))?,
                    file_pos,
                ))
            } else {
                Err(self.iter.err(
                    format!(
                        "Received invalid token in end of array type: {:?}.",
                        next_token
                    ),
                    Some(next_token.file_pos),
                ))
            }
        } else {
            Err(self.iter.err(
                "Received None at end of array type with size.".into(),
                Some(file_pos),
            ))
        }
    }
}
