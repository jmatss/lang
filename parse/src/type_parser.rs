use crate::parser::{ParseTokenIter, DEFAULT_ASSIGN_STOP_CONDS};
use common::{
    error::CustomResult,
    token::expr::Expr,
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
    },
};
use lex::token::{LexToken, LexTokenKind, Sym};
use log::warn;

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
        let mut type_parser = Self::new(iter, generics);
        type_parser.parse_type()
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
        if let Some(lex_token) = self.iter.next_skip_space() {
            match lex_token.kind {
                // Ident.
                LexTokenKind::Ident(ref ident) => {
                    // Wrap the current ident into a "Generic" if it exists in
                    // the `self.generics` map. Otherwise return it as a
                    // "UnknownIdent" wrapped in a "CompoundType".
                    if let Some(true) = self.generics.map(|g| g.contains(ident)) {
                        Ok(Ty::Generic(ident.clone(), None))
                    } else {
                        let inner_ty = InnerTy::ident_to_type(&ident, self.iter.current_block_id());
                        let generics = self.parse_type_generics(GenericsKind::Impl)?;

                        Ok(Ty::CompoundType(inner_ty, generics))
                    }
                }

                // Pointer.
                LexTokenKind::Sym(Sym::CurlyBracketBegin) => self.parse_type_pointer(),

                // Array/slice.
                LexTokenKind::Sym(Sym::SquareBracketBegin) => self.parse_type_array(),

                // Built in call.
                LexTokenKind::Sym(Sym::At) => {
                    self.iter.rewind_skip_space()?;

                    // Need to add stop conditions for if the the start/end of
                    // a generic list is reached.
                    let mut stop_conds = DEFAULT_ASSIGN_STOP_CONDS.to_vec();
                    stop_conds.push(Sym::PointyBracketBegin);
                    stop_conds.push(Sym::PointyBracketEnd);
                    stop_conds.push(Sym::ShiftRight);

                    let expr = self.iter.parse_expr(&stop_conds)?;
                    if let Expr::BuiltInCall(..) = expr {
                        Ok(Ty::Expr(Box::new(expr)))
                    } else {
                        Err(self.iter.err(format!(
                            "\"At\"(@) in type NOT built-in call, was: {:#?}",
                            expr
                        )))
                    }
                }

                _ => Err(self
                    .iter
                    .err(format!("Invalid type token: {:?}", lex_token))),
            }
        } else {
            Err(self.iter.err("Received None when parsing type.".into()))
        }
    }

    /// Parses a list of types inside a generic "tag" (<..>).
    ///   X<T>      // Type with generic argument.
    ///   X<T, V>   // Type with multiple generic arguments.
    pub(crate) fn parse_type_generics(&mut self, kind: GenericsKind) -> CustomResult<Generics> {
        let mut generics = Generics::new();

        warn!("INSIDE GENERICS!");

        let mark = self.iter.mark();

        // If the next token isn't a "PointyBracketBegin" there are no generic
        // list, just return a empty generics.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::PointyBracketBegin) = lex_token.kind {
                // Do nothing, parse generic list in logic underneath.
            } else {
                self.iter.rewind_to_mark(mark);
                return Ok(generics);
            }
        }

        warn!("INSIDE GENERICS 2!");

        // Sanity check to see if this is a generic list with no items inside.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                LexTokenKind::Sym(Sym::PointyBracketEnd) | LexTokenKind::Sym(Sym::ShiftRight) => {
                    return Err(self.iter.err("Empty generic list.".into()));
                }
                _ => (),
            }
        }

        warn!("INSIDE GENERICS 3!");

        loop {
            // Parse the next item in the list as either a identifier(name) or
            // a type depending if this is a decl or impl generic list.
            match kind {
                GenericsKind::Decl => generics.insert_name(self.next_ident()?),
                GenericsKind::Impl => generics.insert_type(self.parse_type()?),
                _ => panic!("Bad GenericsKind: {:#?}", generics),
            }

            warn!("INSIDE GENERICS 4!");

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
                                return Ok(generics);
                            }
                        }
                        continue;
                    }
                    LexTokenKind::Sym(Sym::PointyBracketEnd) => {
                        return Ok(generics);
                    }
                    LexTokenKind::Sym(Sym::ShiftRight) => {
                        self.iter.rewind_to_mark(mark);

                        let kind = LexTokenKind::Sym(Sym::PointyBracketEnd);
                        let token = LexToken::new(kind, lex_token.file_pos.to_owned());
                        self.iter.replace(token);

                        return Ok(generics);
                    }
                    _ => {
                        return Err(self.iter.err(format!(
                            "Received unexpected token after argument in generic list: {:?}",
                            lex_token
                        )));
                    }
                }
            } else {
                return Err(self
                    .iter
                    .err("Received None after argument in generic list.".into()));
            }
        }
    }

    /// Gets the next lex token and assumes that it is a identifier. If it isn't,
    /// a error will be returned.
    fn next_ident(&mut self) -> CustomResult<String> {
        if let Some(lex_token) = self.iter.next_skip_space_line() {
            if let LexTokenKind::Ident(ident) = lex_token.kind {
                Ok(ident)
            } else {
                Err(self.iter.err(format!(
                    "Expected next to be ident in generic decl, was: {:?}",
                    lex_token
                )))
            }
        } else {
            Err(self.iter.err("Got Nonewhen parsing generic decl.".into()))
        }
    }

    /// Parses a pointer type.
    ///   {X}       // Pointer to type (is the {X} syntax be weird/ambiguous?)
    fn parse_type_pointer(&mut self) -> CustomResult<Ty> {
        // The "CurlyBracketBegin" has already been skipped.
        // Parse the type and then wrap it into a Pointer type.
        let ty = self.parse_type()?;
        let ptr_ty = Ty::Pointer(Box::new(ty));

        // At this point the token should be a "CurlyBracketEnd" since this is
        // the end of the pointer.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::CurlyBracketEnd) = lex_token.kind {
                Ok(ptr_ty)
            } else {
                Err(self.iter.err(format!(
                    "Expected curlybrace at end of pointer type, got: {:?}.",
                    lex_token
                )))
            }
        } else {
            Err(self
                .iter
                .err("Received None at end of pointer type.".into()))
        }
    }

    /// Parses array type.
    ///   [X]       // Array of type X with unknown size (slice).
    ///   [X: 3]    // Array of type X with size 3.
    ///   [X: _]    // Array of type X with infered size.
    fn parse_type_array(&mut self) -> CustomResult<Ty> {
        // The "SquareBracketBegin" has already been skipped.
        let gen_ty = self.parse_type()?;

        // At this point the token can either be a "Colon" to indicate that this
        // array type has a size specificed or it can be a "SquareBracketEnd"
        // which indicates the end of this array.
        let size = if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::SquareBracketEnd) = lex_token.kind {
                None
            } else if let LexTokenKind::Sym(Sym::Colon) = lex_token.kind {
                if let Some(lex_tok) = self.iter.peek_skip_space() {
                    // If the symbol is a underscore, the size of the array
                    // should be infered (currently the parse output doesn't
                    // differ between this and just not having a size at all (?)).
                    if let LexTokenKind::Sym(Sym::UnderScore) = lex_tok.kind {
                        None
                    } else {
                        let stop_conds = [Sym::SquareBracketEnd];
                        Some(Box::new(self.iter.parse_expr(&stop_conds)?))
                    }
                } else {
                    return Err(self.iter.err(
                        "Received None when looking at symbol after colon in array type.".into(),
                    ));
                }
            } else {
                return Err(self.iter.err(format!(
                    "Received invalid token in array type: {:?}.",
                    lex_token
                )));
            }
        } else {
            return Err(self.iter.err("Received None at end of array type.".into()));
        };

        let array_ty = Ty::Array(Box::new(gen_ty), size);

        // The next token must be a "SquareBracketEnd" or something has
        // gone wrong.
        if let Some(next_lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Sym(Sym::SquareBracketEnd) = next_lex_token.kind {
                Ok(array_ty)
            } else {
                Err(self.iter.err(format!(
                    "Received invalid token in end of array type: {:?}.",
                    next_lex_token
                )))
            }
        } else {
            Err(self
                .iter
                .err("Received None at end of array type with size.".into()))
        }
    }
}
