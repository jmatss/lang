use crate::parser::ParseTokenIter;
use common::{
    error::CustomResult,
    variable_type::{Type, TypeStruct},
};
use lex::token::{LexTokenKind, Symbol};

pub struct TypeParser<'a> {
    iter: &'a mut ParseTokenIter,
}

// TODO: Need to accept "right shift" (>>) as part of a type when generics are
//       implemented.

impl<'a> TypeParser<'a> {
    pub fn parse(iter: &'a mut ParseTokenIter) -> CustomResult<TypeStruct> {
        let mut type_parser = Self { iter };
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
    fn parse_type(&mut self) -> CustomResult<TypeStruct> {
        if let Some(lex_token) = self.iter.next_skip_space() {
            match lex_token.kind {
                // Ident.
                LexTokenKind::Identifier(ident) => {
                    let type_enum = Type::ident_to_type(&ident);
                    let generics = self.parse_type_generics()?;
                    Ok(TypeStruct::new(type_enum, generics))
                }

                // Pointer.
                LexTokenKind::Symbol(Symbol::CurlyBracketBegin) => self.parse_type_pointer(),

                // Array/slice.
                LexTokenKind::Symbol(Symbol::SquareBracketBegin) => self.parse_type_array(),

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
    fn parse_type_generics(&mut self) -> CustomResult<Option<Vec<TypeStruct>>> {
        // If the next token isn't a "PointyBracketBegin" there are no generic
        // list, just return None.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::PointyBracketBegin) = lex_token.kind {
                // Do nothing, parse generic list in logic underneath.
            } else {
                self.iter.rewind()?;
                return Ok(None);
            }
        }

        // Sanity check to see if this is a generic list with no items inside.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::PointyBracketEnd) = lex_token.kind {
                return Err(self.iter.err("Empty generic list.".into()));
            }
        }

        let mut generics = Vec::new();

        loop {
            // Iterate a parse one generic type at a time in the list.
            let generic = self.parse_type()?;
            generics.push(generic);

            // End of a type in the generic list. The next token should either
            // be a comma if there are more arguments in the list or a
            // PointyBracketEnd if the generic list have been parsed fully.
            if let Some(lex_token) = self.iter.next_skip_space() {
                match lex_token.kind {
                    LexTokenKind::Symbol(Symbol::Comma) => {
                        continue;
                    }
                    LexTokenKind::Symbol(Symbol::PointyBracketEnd) => {
                        return Ok(Some(generics));
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

    /// Parses a pointer type.
    ///   {X}       // Pointer to type (is the {X} syntax be weird/ambiguous?)
    fn parse_type_pointer(&mut self) -> CustomResult<TypeStruct> {
        // The "CurlyBracketBegin" has already been skipped.
        let mut type_struct = self.parse_type()?;

        // Wrap the parsed type into the Pointer enum.
        type_struct.t = Type::Pointer(Box::new(type_struct.clone()));

        // At this point the token should be a "CurlyBracketEnd" since this is
        // the end of the pointer.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::CurlyBracketEnd) = lex_token.kind {
                Ok(type_struct)
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
    fn parse_type_array(&mut self) -> CustomResult<TypeStruct> {
        // The "SquareBracketBegin" has already been skipped.
        let mut type_struct = self.parse_type()?;

        // At this point the token can either be a "Colon" to indicate that this
        // array type has a size specificed or it can be a "SquareBracketEnd"
        // which indicates the end of this array.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::SquareBracketEnd) = lex_token.kind {
                // Wrap the inner type into the Array enum and set the size
                // to None to indicate that it is unknown.
                type_struct.t = Type::Array(Box::new(type_struct.clone()), None);
                Ok(type_struct)
            } else if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                if let Some(lex_tok) = self.iter.peek_skip_space() {
                    // If the symbol is a underscore, the size of the array
                    // should be infered (currently the parse output doesn
                    // differ between this and just not having a size at all (?)).
                    if let LexTokenKind::Symbol(Symbol::UnderScore) = lex_tok.kind {
                        type_struct.t = Type::Array(Box::new(type_struct.clone()), None);
                    } else {
                        // Parse the next expression and assume it is the size of the
                        // array. Wrap the inner type into the Array enum and set the
                        // parsed expression as the size.
                        let stop_conds = [Symbol::SquareBracketEnd];
                        let size = Box::new(self.iter.parse_expr(&stop_conds)?);
                        type_struct.t = Type::Array(Box::new(type_struct.clone()), Some(size));
                    }
                } else {
                    unreachable!();
                }

                // The next token must be a "SquareBracketEnd" or something has
                // gone wrong.
                if let Some(next_lex_token) = self.iter.next_skip_space() {
                    if let LexTokenKind::Symbol(Symbol::SquareBracketEnd) = next_lex_token.kind {
                        Ok(type_struct)
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
            } else {
                Err(self.iter.err(format!(
                    "Received invalid token in array type: {:?}.",
                    lex_token
                )))
            }
        } else {
            Err(self.iter.err("Received None at end of array type.".into()))
        }
    }
}
