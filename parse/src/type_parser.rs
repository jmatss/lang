use crate::parser::{ParseTokenIter, DEFAULT_ASSIGN_STOP_CONDS};
use common::{
    error::CustomResult,
    file::FilePosition,
    token::expr::Expr,
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
    },
    type_info::TypeInfo,
};
use lex::token::{LexToken, LexTokenKind, Sym};

pub struct TypeParser<'a, 'b> {
    iter: &'a mut ParseTokenIter<'b>,
    generics: Option<&'a Generics>,
}

// TODO: Need to accept "right shift" (>>) as part of a type when generics are
//       implemented.

impl<'a, 'b> TypeParser<'a, 'b> {
    pub fn new(iter: &'a mut ParseTokenIter<'b>, generics: Option<&'a Generics>) -> Self {
        Self { iter, generics }
    }

    pub fn parse(
        iter: &'a mut ParseTokenIter<'b>,
        generics: Option<&'a Generics>,
    ) -> CustomResult<Ty> {
        Self::new(iter, generics).parse_type()
    }

    /// Valid type formats:
    ///   X         // Basic type.
    ///   {X}       // Pointer to type (is the {X} syntax weird/ambiguous (?))
    ///   X<T>      // Type with generic argument.
    ///   X<T, V>   // Type with multiple generic arguments.
    ///   [X]       // Array of type X with unknown size (slice).
    ///   [X: 3]    // Array of type X with size 3.
    ///   [X: _]    // Array of type X with infered size.
    ///
    /// Test examples:
    ///   C                     Lang
    ///   uint32_t *(*x)[]      x: {[{u32}]}
    ///   char *x               x: {char}
    fn parse_type(&mut self) -> CustomResult<Ty> {
        let mut file_pos = self.iter.peek_file_pos()?;

        if let Some(lex_token) = self.iter.next_skip_space() {
            match lex_token.kind {
                // Ident.
                LexTokenKind::Ident(ref ident) => {
                    // Parse generics. If this type has generics specified, add
                    // them as the "end" to the `file_pos`. Otherwise, the `ident`
                    // of the type will be the "end" file pos.
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

                    // Wrap the current ident into a "Generic" if it exists in
                    // the `self.generics` map. Otherwise return it as a
                    // "UnknownIdent" wrapped in a "CompoundType".
                    if let Some(true) = self.generics.map(|g| g.contains(ident)) {
                        file_pos.set_end(&lex_token.file_pos)?;

                        if !generics.is_empty() {
                            panic!("TODO: Generic decl in type has generics itself.");
                        }

                        let type_info = TypeInfo::Generic(file_pos);
                        Ok(Ty::Generic(ident.clone(), type_info))
                    } else {
                        let inner_ty = InnerTy::ident_to_type(&ident, self.iter.current_block_id());
                        let type_info = TypeInfo::Default(file_pos);
                        Ok(Ty::CompoundType(inner_ty, generics, type_info))
                    }
                }

                // Pointer.
                LexTokenKind::Sym(Sym::CurlyBracketBegin) => {
                    self.iter.rewind_skip_space()?; // Rewind the CurlyBracketBegin.

                    let ty = self.parse_type_pointer()?;
                    file_pos.set_end(&ty.file_pos().unwrap())?;

                    Ok(ty)
                }

                // Array/slice.
                LexTokenKind::Sym(Sym::SquareBracketBegin) => {
                    self.iter.rewind_skip_space()?; // Rewind the SquareBracketBegin.

                    let ty = self.parse_type_array()?;
                    file_pos.set_end(&ty.file_pos().unwrap())?;

                    Ok(ty)
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
                        Ok(Ty::Expr(Box::new(expr), TypeInfo::Default(file_pos)))
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
    ) -> CustomResult<(Generics, Option<FilePosition>)> {
        let mut generics = Generics::new();

        let mut file_pos = self.iter.peek_file_pos()?;
        let mark = self.iter.mark();

        // If the next token isn't a "PointyBracketBegin" there are no generic
        // list, just return a empty generics.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::PointyBracketBegin) = lex_token.kind {
                // Do nothing, parse generic list in logic underneath.
            } else {
                self.iter.rewind_to_mark(mark);
                return Ok((generics, None));
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
                    let ty = self.parse_type()?;
                    generics.insert_type(ty.clone());
                    *ty.file_pos().unwrap()
                }
                _ => panic!("Bad GenericsKind: {:#?}", generics),
            };

            let mark = self.iter.mark();

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
                                return Ok((generics, Some(file_pos)));
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(Sym::PointyBracketEnd) => {
                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((generics, Some(file_pos)));
                    }
                    LexTokenKind::Sym(Sym::ShiftRight) => {
                        self.iter.rewind_to_mark(mark);

                        let kind = LexTokenKind::Sym(Sym::PointyBracketEnd);
                        let token = LexToken::new(kind, lex_token.file_pos.to_owned());
                        self.iter.replace(token);

                        file_pos.set_end(&lex_token.file_pos)?;
                        return Ok((generics, Some(file_pos)));
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
                // TODO: Where to fetch file_pos from?
                return Err(self.iter.err(
                    "Received None after argument in generic list.".into(),
                    Some(cur_file_pos),
                ));
            }
        }
    }

    /// Gets the next lex token and assumes that it is a identifier. If it isn't,
    /// a error will be returned.
    fn next_ident(&mut self) -> CustomResult<(String, FilePosition)> {
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
    fn parse_type_pointer(&mut self) -> CustomResult<Ty> {
        let mut file_pos = self.iter.peek_file_pos()?;

        self.iter.next_skip_space(); // Consume the CurlyBracketBegin.
        let ty = self.parse_type()?;

        // At this point the token should be a "CurlyBracketEnd" since this is
        // the end of the pointer.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::CurlyBracketEnd) = lex_token.kind {
                file_pos.set_end(&lex_token.file_pos)?;
                Ok(Ty::Pointer(Box::new(ty), TypeInfo::Default(file_pos)))
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
                ty.file_pos().cloned(),
            ))
        }
    }

    /// Parses array type.
    ///   [X]       // Array of type X with unknown size (slice).
    ///   [X: 3]    // Array of type X with size 3.
    ///   [X: _]    // Array of type X with infered size.
    fn parse_type_array(&mut self) -> CustomResult<Ty> {
        let mut file_pos = self.iter.peek_file_pos()?;

        self.iter.next_skip_space(); // Consume the SquareBracketBegin.
        let gen_ty = self.parse_type()?;

        // At this point the token can either be a "Colon" to indicate that this
        // array type has a size specificed or it can be a "SquareBracketEnd"
        // which indicates the end of this array.
        let size = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::SquareBracketEnd) = lex_token.kind {
                None
            } else if let LexTokenKind::Sym(Sym::Colon) = lex_token.kind {
                if let Some(next_token) = self.iter.peek_skip_space() {
                    // If the symbol is a underscore, the size of the array
                    // should be infered (currently the parse output doesn't
                    // differ between this and just not having a size at all (?)).
                    if let LexTokenKind::Sym(Sym::UnderScore) = next_token.kind {
                        // TODO: Need to parse tokens.
                        panic!("TODO: Underscore in array dimension.");
                    } else {
                        let stop_conds = [Sym::SquareBracketEnd];
                        let expr = self.iter.parse_expr(&stop_conds)?;

                        if let Some(expr_file_pos) = expr.file_pos() {
                            file_pos.set_end(expr_file_pos)?;
                        } else {
                            unreachable!();
                        }

                        Some(Box::new(expr))
                    }
                } else {
                    return Err(self.iter.err(
                        "Received None when looking at symbol after colon in array type.".into(),
                        Some(lex_token.file_pos),
                    ));
                }
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
                Ok(Ty::Array(
                    Box::new(gen_ty),
                    size,
                    TypeInfo::Default(file_pos),
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
