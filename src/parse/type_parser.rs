use super::{iter::ParseTokenIter, token::TypeStruct};
use crate::error::CustomError::ParseError;
use crate::{
    common::variable_type::Type,
    lex::token::{LexTokenKind, Symbol},
    CustomResult,
};

pub struct TypeParser<'a> {
    iter: &'a ParseTokenIter,
}

impl<'a> TypeParser<'a> {
    pub fn parse(iter: &'a ParseTokenIter) -> CustomResult<TypeStruct> {
        let type_parser = Self { iter };
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
        loop {
            if let Some(lex_token) = self.iter.next_skip_space() {
                match lex_token.kind {
                    // Ident.
                    LexTokenKind::Identifier(ident) => {
                        let type_enum = Type::ident_to_type(&ident);
                        let generics = self.parse_type_generics()?;

                        let type_struct = if generics.len() == 0 {
                            TypeStruct::new(type_enum, None)
                        } else {
                            TypeStruct::new(type_enum, Some(generics))
                        };
                    }

                    // Pointer.
                    LexTokenKind::Symbol(Symbol::CurlyBracketBegin) => {
                        return self.parse_type_pointer()
                    }

                    // Array/slice.
                    LexTokenKind::Symbol(Symbol::SquareBracketBegin) => {
                        return self.parse_type_array()
                    }

                    _ => (),
                }
            }
        }
    }

    /// Parses a list of types inside a generic "tag" (<..>).
    ///   X<T>      // Type with generic argument.
    ///   X<T, V>   // Type with multiple generic arguments.
    fn parse_type_generics(&mut self) -> CustomResult<Vec<TypeStruct>> {
        // Skip the "PointyBracketBegin".
        self.iter.next_skip_space();

        // Sanity check to see if this is a generic list with no items inside.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::PointyBracketEnd) = lex_token.kind {
                return Err(ParseError("Empty generic list.".into()));
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
                        return Ok(generics);
                    }
                    _ => {
                        return Err(ParseError(format!(
                            "Received unexpected token after argument in generic list: {:?}",
                            lex_token
                        )))
                    }
                }
            } else {
                return Err(ParseError(
                    "Received None after argument in generic list.".into(),
                ));
            }
        }
    }

    /// Parses a pointer type.
    ///   {X}       // Pointer to type (is the {X} syntax be weird/ambiguous?)
    fn parse_type_pointer(&mut self) -> CustomResult<TypeStruct> {
        // The "CurlyBracketBegin" has already been skipped.
        let type_struct = self.parse_type()?;

        // Wrap the parsed type into the Pointer enum.
        type_struct.t = Type::Pointer(Box::new(type_struct.t));

        // At this point the token should be a "CurlyBracketEnd" since this is
        // the end of the pointer.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::CurlyBracketEnd) = lex_token.kind {
                Ok(type_struct)
            } else {
                Err(ParseError(format!(
                    "Expected curlybrace at end of pointer type, got: {:?}.",
                    lex_token
                )))
            }
        } else {
            Err(ParseError("Received None at end of pointer type.".into()))
        }
    }

    /// Parses array type.
    ///   [X]       // Array of type X with unknown size (slice).
    ///   [X: 3]    // Array of type X with size 3.
    ///   [X: _]    // Array of type X with infered size.
    fn parse_type_array(&mut self) -> CustomResult<TypeStruct> {
        // The "SquareBracketBegin" has already been skipped.
        let type_struct = self.parse_type()?;

        // At this point the token can either be a "Colon" to indicate that this
        // array type has a size specificed or it can be a "SquareBracketEnd"
        // which indicates the end of this array.
        if let Some(lex_token) = self.iter.next_skip_space() {
            if let LexTokenKind::Symbol(Symbol::SquareBracketEnd) = lex_token.kind {
                // Wrap the inner type into the Array enum and set the size
                // to None to indicate that it is unknown.
                type_struct.t = Type::Array(Box::new(type_struct.t), None);
                Ok(type_struct)
            } else if let LexTokenKind::Symbol(Symbol::Colon) = lex_token.kind {
                if let Some(lex_tok) = self.iter.peek_skip_space() {
                    // If the symbol is a underscore, the size of the array
                    // should be infered (currently the parse output doesn
                    // differ between this and just not having a size at all (?)).
                    if let LexTokenKind::Symbol(Symbol::UnderScore) = lex_tok.kind {
                        type_struct.t = Type::Array(Box::new(type_struct.t), None);
                    } else {
                        // Parse the next expression and assume it is the size of the
                        // array. Wrap the inner type into the Array enum and set the
                        // parsed expression as the size.
                        let stop_conds = [Symbol::SquareBracketEnd];
                        let size = Box::new(self.iter.parse_expr(&stop_conds)?);
                        type_struct.t = Type::Array(Box::new(type_struct.t), Some(size));
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
                        Err(ParseError(format!(
                            "Received invalid token in end of array type: {:?}.",
                            next_lex_token
                        )))
                    }
                } else {
                    Err(ParseError(
                        "Received None at end of array type with size.".into(),
                    ))
                }
            } else {
                Err(ParseError(format!(
                    "Received invalid token in array type: {:?}.",
                    lex_token
                )))
            }
        } else {
            Err(ParseError("Received None at end of array type.".into()))
        }
    }
}
