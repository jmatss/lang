use super::lex_token::LexTokenType;
use crate::error::CustomError::{self, LexError};
use crate::lexer::lex_token::{LexToken, Literal, Symbol};
use crate::CustomResult;
use std::collections::linked_list::LinkedList;
use std::fs::File;
use std::io::Read;
use std::str::Chars;

const MAX_PUT_BACK: usize = 10;

pub struct LexTokenIter<'a> {
    buff: LinkedList<char>,
    iter: Chars<'a>,

    /// Current line number.
    line_nr: u64,

    /// Current column number.
    column_nr: u64,
}

impl<'a> LexTokenIter<'a> {
    pub fn new(filename: &str) -> std::io::Result<Self> {
        // TODO: dont read whole file in one go.
        let mut file: File = File::open(filename)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;

        Ok(Self {
            buff: LinkedList::new(),
            iter: string.chars(),
            line_nr: 1,
            column_nr: 1,
        })
    }

    /// Gets the next LexToken from the iterator.
    pub fn next_token(&mut self) -> CustomResult<LexToken> {
        // TODO: Fix for more radixes.
        const RADIX: u32 = 10;

        // TODO: Add edge case for "bx1010" to represent binary literals.

        let lex_token: LexToken;
        lex_token.lineNr = self.line_nr;
        lex_token.columnNr = self.column_nr;
        lex_token.t = if let Some((c1, c2, c3)) = self.peek_three() {
            if LexTokenIter::valid_identifier_start(c1) {
                let identifier: String = self.get_identifier_string()?;

                // Check if this a symbol that have a valid identifier name(ex. "and", "as")
                // or if it is a valid keyword ("enum", "return" etc.).
                if let Some(symbol_type) = LexToken::get_if_symbol(&identifier) {
                    symbol_type
                } else if let Some(keyword_type) = LexToken::get_if_keyword(&identifier) {
                    keyword_type
                } else {
                    LexTokenType::Identifier(identifier)
                }
            } else if LexTokenIter::valid_number(c1, RADIX) {
                self.get_number(RADIX)?
            } else if LexTokenIter::valid_linebreak(c1, c2) {
                self.get_linebreak()?
            } else if LexTokenIter::valid_whitespace(c1) {
                self.get_whitespaces()?
            } else if let Some(symbol_type_tup) = LexToken::get_if_symbol_three_chars(c1, c2, c3) {
                let (symbol_type, n) = symbol_type_tup;

                // Add special cases for string- and char literals.
                // They start and end with " or '.
                match symbol_type {
                    LexTokenType::Symbol(Symbol::DoubleQuote) => self.get_literal_string()?,
                    LexTokenType::Symbol(Symbol::SingleQuote) => self.get_literal_char()?,
                    _ => {
                        self.skip(n);
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
            LexTokenType::EndOfFile
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

        while let Some(c) = self.next() {
            if LexTokenIter::valid_identifier(c) {
                result.push(c);
            } else {
                self.put_back(c);
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

    // TODO: Hex/binary numbers with prefix 0x/0b
    // Fetches both integeres and floats.
    /// Returns the number (int or float) at the current position of the iterator.
    fn get_number(&mut self, radix: u32) -> CustomResult<LexTokenType> {
        let mut number = self.get_integer(radix)?;

        // If the next simple token is a Dot and the char after that is a number,
        // this is a float.
        // FIXME: (?) might not need to check number after Dot to allow for
        // example "1." instead of needing to write "1.0".
        if let Some(('.', Some(next))) = self.peek_two() {
            if LexTokenIter::valid_number(next, radix) {
                self.skip(1); // Remove dot.
                number = [number, self.get_integer(radix)?].join(".");
            }
        }

        self.column_nr += number.chars().count() as u64;

        Ok(LexTokenType::Literal(Literal::Number(number)))
    }

    /// Returns the integer at the current position of the iterator.
    fn get_integer(&mut self, radix: u32) -> CustomResult<String> {
        let mut numbers = Vec::new();

        while let Some(c) = self.next() {
            if LexTokenIter::valid_number(c, radix) {
                numbers.push(c);
            } else {
                self.put_back(c);
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

        self.next(); // Remove the start "symbol" (single or double-quote).

        loop {
            if let Some(current) = self.next() {
                match LexToken::get_if_symbol_char(current) {
                    // Break if the current character is the `literal_symbol`
                    // which means that the end of the literal have been found.
                    Some((LexTokenType::Symbol(s), _)) if s == literal_symbol => break,
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
    fn get_literal_string(&mut self) -> CustomResult<LexTokenType> {
        Ok(LexTokenType::Literal(Literal::StringLiteral(
            self.get_literal(Symbol::DoubleQuote)?,
        )))
    }

    /// Returns the char literal at the current position of the iterator.
    fn get_literal_char(&mut self) -> CustomResult<LexTokenType> {
        Ok(LexTokenType::Literal(Literal::CharLiteral(
            self.get_literal(Symbol::SingleQuote)?,
        )))
    }

    /// Returns the line break at the current position of the iterator.
    fn get_linebreak(&mut self) -> CustomResult<LexTokenType> {
        let c = self.next().ok_or_else(|| {
            LexError("Reached EOF while parsing first char in get_linebreak.".to_string())
        })?;

        let c1 = self.next();
        let c2 = self.next();
        if let Some('\n') = c1 {
            self.line_nr += 1;
            self.column_nr = 1;

            Ok(LexTokenType::Symbol(Symbol::LineBreak))
        } else if let (Some('\r'), Some('\n')) = (c1, c2) {
            self.line_nr += 1;
            self.column_nr += 2;

            Ok(LexTokenType::Symbol(Symbol::LineBreak))
        } else {
            Err(LexError(
                "No linebreak character received in get_linebreak.".to_string(),
            ))
        }
    }

    /// Returns all the consecutive white spaces at the current position of the
    /// iterator.
    fn get_whitespaces(&mut self) -> CustomResult<LexTokenType> {
        let mut count = 0;

        while let Some(c) = self.next() {
            let c_next = self.peek();

            // Since line breaks counts as whitespaces in rust, need to manually make
            // sure that the current char(s) isn't a linebreak in the if-statement.
            if LexTokenIter::valid_whitespace(c) && !LexTokenIter::valid_linebreak(c, c_next) {
                count += 1;
            } else {
                self.put_back(c);
                break;
            }
        }

        self.column_nr += count as u64;

        Ok(LexTokenType::Symbol(Symbol::WhiteSpace(count)))
    }

    /// Get the next character from the iterator.
    #[inline]
    fn next(&mut self) -> Option<char> {
        if !self.buff.is_empty() {
            Some(self.buff.pop_front()?)
        } else {
            self.iter.next()
        }
    }

    /// Puts back a character into the iterator.
    #[inline]
    fn put_back(&mut self, c: char) -> CustomResult<()> {
        if self.buff.len() < MAX_PUT_BACK {
            Ok(self.buff.push_front(c))
        } else {
            Err(CustomError::LexError("Push back buffer was full".into()))
        }
    }

    /// Skips the next `n` characters in the iterator.
    #[inline]
    fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    /// Peeks `n` positions ahead of the current position of the iterator.
    /// The `n` can not be greater that the max capacity of the push back buffer
    /// which is set to MAX_PUT_BACK.
    #[inline]
    fn peek_n(&mut self, n: usize) -> Vec<char> {
        let mut chars = Vec::with_capacity(n);

        for _ in 0..n {
            if let Some(c) = self.next() {
                chars.push(c);
            } else {
                break;
            }
        }

        for c in chars.iter().rev() {
            self.put_back(c.clone());
        }

        chars
    }

    /// Peeks 1 position ahead of the current position of the iterator.
    #[inline]
    fn peek(&mut self) -> Option<char> {
        let chars = self.peek_n(1);
        if chars.len() == 1 {
            Some(chars[0])
        } else {
            None
        }
    }

    /// Peeks at the two upcoming characters in the iterator.
    #[inline]
    fn peek_two(&mut self) -> Option<(char, Option<char>)> {
        let chars = self.peek_n(2);
        Some(match chars.len() {
            2 => (chars[0], Some(chars[1])),
            1 => (chars[0], None),
            _ => return None,
        })
    }

    /// Peeks at the three upcoming characters in the iterator.
    #[inline]
    fn peek_three(&mut self) -> Option<(char, Option<char>, Option<char>)> {
        let chars = self.peek_n(3);
        Some(match chars.len() {
            3 => (chars[0], Some(chars[1]), Some(chars[2])),
            2 => (chars[0], Some(chars[1]), None),
            1 => (chars[0], None, None),
            _ => return None,
        })
    }
}
