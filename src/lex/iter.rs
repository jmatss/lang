use super::token::LexTokenKind;
use crate::error::CustomError::LexError;
use crate::lex::token::{LexToken, Literal, Symbol};
use crate::{common::iter::TokenIter, CustomResult};
use std::fs::File;
use std::io::Read;

/// Max amount of chars to be put back into the iterator.
const MAX_PUT_BACK: usize = 10;

pub struct LexTokenIter {
    /// Use to iterate over the character tokens.
    iter: TokenIter<char>,

    /// Current line number.
    line_nr: u64,

    /// Current column number.
    column_nr: u64,
}

impl LexTokenIter {
    pub fn new(filename: &str) -> std::io::Result<Self> {
        // TODO: dont read whole file in one go.
        let mut file: File = File::open(filename)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;

        Ok(Self {
            iter: TokenIter::new(string.chars()),
            line_nr: 1,
            column_nr: 1,
        })
    }

    /// Gets the next LexToken from the iterator.
    pub fn next_token(&mut self) -> CustomResult<LexToken> {
        // A radix of 10 will match all type of numbers since non decimal numbers
        // are prefixed with "0x", "0b" or "0o" (which starts with radix 10).
        const RADIX: u32 = 10;

        let lex_token: LexToken;
        lex_token.lineNr = self.line_nr;
        lex_token.columnNr = self.column_nr;

        lex_token.kind = if let Some((c1, c2, c3)) = self.iter.peek_three() {
            if LexTokenIter::valid_linebreak(c1, c2) {
                self.get_linebreak()?
            } else if LexTokenIter::valid_whitespace(c1) {
                self.get_whitespaces()?
            } else if LexTokenIter::valid_number(c1, RADIX) {
                self.get_number()?
            } else if LexTokenIter::valid_identifier_start(c1) {
                let ident: String = self.get_identifier_string()?;

                // Check if this identifier is a valid:
                //   keyword        ("if", "enum" etc.)
                //   symbol         ("and", "as" etc.)
                //   bool literal   (true/false)
                if let Some(symbol_type) = LexToken::get_if_symbol(&ident) {
                    symbol_type
                } else if let Some(keyword_type) = LexToken::get_if_keyword(&ident) {
                    keyword_type
                } else if let Some(bool_type) = LexToken::get_if_bool(&ident) {
                    bool_type
                } else {
                    LexTokenKind::Identifier(ident)
                }
            } else if let Some(symbol_type_tup) = LexToken::get_if_symbol_three_chars(c1, c2, c3) {
                let (symbol_type, n) = symbol_type_tup;

                // Add special cases for string- and char literals.
                // They start and end with " or '.
                match symbol_type {
                    LexTokenKind::Symbol(Symbol::DoubleQuote) => self.get_literal_string()?,
                    LexTokenKind::Symbol(Symbol::SingleQuote) => self.get_literal_char()?,
                    _ => {
                        self.iter.skip(n);
                        symbol_type
                    }
                }
            } else {
                return Err(LexError(format!(
                    "Didn't match a symbol token when c: {:?}, c2: {:?} and c2: {:?}",
                    c1, c2, c3
                )));
            }
        } else {
            LexTokenKind::EndOfFile
        };

        Ok(lex_token)
    }

    #[inline]
    fn valid_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    #[inline]
    fn valid_identifier(c: char) -> bool {
        LexTokenIter::valid_identifier_start(c) || c.is_numeric()
    }

    #[inline]
    fn valid_number(c: char, radix: u32) -> bool {
        c.is_digit(radix)
    }

    #[inline]
    fn valid_linebreak(c: char, c_next: Option<char>) -> bool {
        c == '\n' || (c == '\r' && c_next.map_or(false, |x| x == '\n'))
    }

    // char.is_whitespace() includes linebreaks, so need to check valid_linebreak first.
    #[inline]
    fn valid_whitespace(c: char) -> bool {
        c.is_whitespace()
    }

    /// Returns the identifier at the current position of the iterator.
    fn get_identifier_string(&mut self) -> CustomResult<String> {
        let mut result = String::new();

        while let Some(c) = self.iter.next() {
            if LexTokenIter::valid_identifier(c) {
                result.push(c);
            } else {
                self.iter.put_back(c);
                break;
            }
        }

        self.column_nr += result.chars().count() as u64;

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(LexError(
                "Empty result in get_identifier_string().".to_string(),
            ))
        }
    }

    // TODO: number containing scientifical notaion (e/E).
    /// Returns the number (int or float) at the current position of the iterator.
    fn get_number(&mut self) -> CustomResult<LexTokenKind> {
        let radix = if let Some(('0', Some(sep_char))) = self.iter.peek_two() {
            // Remove the assumed prefix.
            self.iter.skip(2);

            match sep_char.to_ascii_uppercase() {
                'x' => 16,
                'b' => 2,
                'o' => 8,
                _ => {
                    // Put back the chars since they aren't part of a prefix,
                    // this is just a decimal number that start with '0'.
                    self.iter.put_back(sep_char);
                    self.iter.put_back('0');
                    10
                }
            }
        } else {
            // Decimal.
            10
        };

        let mut is_float = false;
        let mut number = self.get_integer(radix)?;

        // If this number contains a dot, assume it is a float number.
        // FIXME: (?) might not need to check number after Dot to allow for
        // example "1." instead of needing to write "1.0".
        if let Some(('.', Some(next))) = self.iter.peek_two() {
            if LexTokenIter::valid_number(next, radix) {
                self.iter.skip(1); // Remove dot.
                number = [number, self.get_integer(radix)?].join(".");

                is_float = true;
            }
        }

        self.column_nr += number.chars().count() as u64;

        if is_float {
            Ok(LexTokenKind::Literal(Literal::Float(number)))
        } else {
            Ok(LexTokenKind::Literal(Literal::Integer(number, radix)))
        }
    }

    /// Returns the integer at the current position of the iterator with
    /// the radix/base `radix`.
    fn get_integer(&mut self, radix: u32) -> CustomResult<String> {
        let mut numbers = Vec::new();

        while let Some(c) = self.iter.next() {
            if LexTokenIter::valid_number(c, radix) {
                numbers.push(c);
            } else {
                self.iter.put_back(c);
                break;
            }
        }

        Ok(numbers.into_iter().collect())
    }

    // TODO: Fix escape chars etc. Ex:
    //      "abc\"abc"
    //  will cause an error.
    /// Returns the string or char literal at the current position of the iterator.
    fn get_literal(&mut self, literal_symbol: Symbol) -> CustomResult<String> {
        let mut char_vec = Vec::new();

        self.iter.skip(1); // Remove the start "symbol" (single or double-quote).

        loop {
            if let Some(current) = self.iter.next() {
                match LexToken::get_if_symbol_char(current) {
                    // Break if the current character is the `literal_symbol`
                    // which means that the end of the literal have been found.
                    Some((LexTokenKind::Symbol(s), _)) if s == literal_symbol => break,
                    _ => char_vec.push(current),
                }
            } else {
                return Err(LexError(
                    "Reached EOF while parsing char(s) in get_literal.".into(),
                ));
            }
        }

        // Need to add two because the begin and end quotes needs to be
        // counted as well.
        self.column_nr += 2 + char_vec.len() as u64;

        Ok(char_vec.iter().collect())
    }

    /// Returns the string literal at the current position of the iterator.
    fn get_literal_string(&mut self) -> CustomResult<LexTokenKind> {
        Ok(LexTokenKind::Literal(Literal::StringLiteral(
            self.get_literal(Symbol::DoubleQuote)?,
        )))
    }

    /// Returns the char literal at the current position of the iterator.
    fn get_literal_char(&mut self) -> CustomResult<LexTokenKind> {
        Ok(LexTokenKind::Literal(Literal::CharLiteral(
            self.get_literal(Symbol::SingleQuote)?,
        )))
    }

    /// Returns the line break at the current position of the iterator.
    fn get_linebreak(&mut self) -> CustomResult<LexTokenKind> {
        let c1 = self.iter.next();
        let c2 = self.iter.next();
        if let Some('\n') = c1 {
            self.line_nr += 1;
            self.column_nr = 1;

            Ok(LexTokenKind::Symbol(Symbol::LineBreak))
        } else if let (Some('\r'), Some('\n')) = (c1, c2) {
            self.line_nr += 1;
            self.column_nr += 2;

            Ok(LexTokenKind::Symbol(Symbol::LineBreak))
        } else {
            Err(LexError(
                "No linebreak character received in get_linebreak.".to_string(),
            ))
        }
    }

    /// Returns all the consecutive white spaces at the current position of the
    /// iterator.
    fn get_whitespaces(&mut self) -> CustomResult<LexTokenKind> {
        let mut count = 0;

        while let Some(c) = self.iter.next() {
            let c_next = self.iter.peek();

            // Since line breaks counts as whitespaces in rust, need to manually make
            // sure that the current char(s) isn't a linebreak in the if-statement.
            if LexTokenIter::valid_whitespace(c) && !LexTokenIter::valid_linebreak(c, c_next) {
                count += 1;
            } else {
                self.iter.put_back(c);
                break;
            }
        }

        self.column_nr += count as u64;

        Ok(LexTokenKind::Symbol(Symbol::WhiteSpace(count)))
    }
}