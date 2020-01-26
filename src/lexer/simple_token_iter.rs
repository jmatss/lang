use crate::error::CustomError::LexError;
use crate::lexer::simple_token::{SimpleToken, Symbol, Literal};
use std::fs::File;
use std::io::Read;
use std::vec::IntoIter;
use std::collections::linked_list::LinkedList;
use crate::CustomResult;

const BUFFER_SIZE: usize = 1 << 16;
const MAX_PUT_BACK: usize = 5;

pub struct SimpleTokenIter {
    buf: LinkedList<char>,
    it: IntoIter<char>,
}

impl SimpleTokenIter {
    pub fn new(filename: &str) -> std::io::Result<SimpleTokenIter> {
        // TODO: dont read whole file in one go.
        let mut file = File::open(filename)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;

        Ok(SimpleTokenIter {
            buf: LinkedList::new(),
            it: string.chars().collect::<Vec<_>>().into_iter(),
        })
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        if !self.buf.is_empty() {
            Some(self.buf.pop_front()?)
        } else {
            self.it.next()
        }
    }

    #[inline]
    fn put_back(&mut self, c: char) {
        if self.buf.len() >= MAX_PUT_BACK {
            panic!("Max put_back size reached.");
        }
        self.buf.push_front(c);
    }

    pub fn next_simple_token(&mut self) -> CustomResult<SimpleToken> {
        // TODO: Fix for more radixes.
        const RADIX: u32 = 10;

        if let Some((c1, c2, c3)) = self.peek_three() {
            if SimpleTokenIter::valid_identifier_start(c1) {
                let id: String = self.get_identifier_string()?;

                // Check if this a symbol that have a valid identifier name(and, or, not, in, is, as).
                if let Some(symbol_token) = Symbol::lookup_identifier(&id) {
                    Ok(symbol_token)
                } else {
                    Ok(SimpleToken::Identifier(id))
                }
            } else if SimpleTokenIter::valid_number(c1, RADIX) {
                self.get_number(RADIX)
            } else if SimpleTokenIter::valid_linebreak(c1, c2) {
                self.get_linebreak()
            } else if SimpleTokenIter::valid_whitespace(c1) {
                self.get_whitespaces()
            } else if let Some(symbol_token) = Symbol::lookup_three(c1, c2, c3) {

                // Add special cases for string- and char literals.
                let (token, n) = symbol_token;
                match token {
                    SimpleToken::Symbol(Symbol::DoubleQuote) => {
                        let (literal_token, m) = self.get_string_literal()?;
                        self.skip(m);
                        Ok(literal_token)
                    }
                    SimpleToken::Symbol(Symbol::SingleQuote) => {
                        let (literal_token, m) = self.get_char_literal()?;
                        self.skip(m);
                        Ok(literal_token)
                    }
                    _ => {
                        self.skip(n);
                        Ok(token)
                    }
                }
            } else {
                Err(LexError(
                    format!("Didn't match a symbol token when c: {:?} and c_next: {:?}", c1, c2)
                ))
            }
        } else {
            Ok(SimpleToken::EndOfFile)
        }
    }

    #[inline]
    pub fn valid_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    #[inline]
    pub fn valid_identifier(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
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

    fn get_identifier_string(&mut self) -> CustomResult<String> {
        let mut result = String::new();
        while let Some(c) = self.next() {
            if !SimpleTokenIter::valid_identifier(c) {
                self.put_back(c);
                break;
            }
            result.push(c);
        }

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(LexError("Empty result in next().".to_string()))
        }
    }

    // TODO: Hex/binary numbers with prefix 0x/0b
    //  Check cast (as) and suffix etc.
    pub fn get_number(&mut self, radix: u32) -> CustomResult<SimpleToken> {
        let mut number = self.get_integer(radix)?;
        if let Some('.') = self.peek() {    // True if float number.
            self.skip(1);                // Remove dot.
            number = [number, self.get_integer(radix)?].join(".");
        }

        Ok(SimpleToken::Number(number))
    }

    fn get_integer(&mut self, radix: u32) -> CustomResult<String> {
        let mut numbers = Vec::new();
        while let Some(c) = self.next() {
            if SimpleTokenIter::valid_number(c, radix) {
                numbers.push(c);
            } else {
                self.put_back(c);
                break;
            }
        }

        Ok(numbers.into_iter().collect())
    }

    fn get_linebreak(&mut self) -> CustomResult<SimpleToken> {
        let c = self.next()
            .ok_or(LexError("Reached EOF while parsing first char in get_linebreak.".to_string()))?;

        if c == '\n' {
            Ok(SimpleToken::Symbol(Symbol::LineBreak))
        } else if c == '\r' {
            let c_next = self.next()
                .ok_or(LexError("Reached EOF after a '\\r' had been parsed.".to_string()))?;

            if c_next == '\n' {
                Ok(SimpleToken::Symbol(Symbol::LineBreak))
            } else {
                Err(LexError("Didn't receive a '\\n' after a '\\r'.".to_string()))
            }
        } else {
            Err(LexError("No linebreak character received in get_linebreak.".to_string()))
        }
    }

    // TODO: Fix escape chars etc. Ex:
    //      "abc\"abc"
    //  will cause an error.
    fn get_string_literal(&mut self) -> CustomResult<(SimpleToken, usize)> {
        let mut char_vec = Vec::new();
        self.next();    // Remove the DoubleQuote start.

        loop {
            let current = self.next()
                .ok_or(LexError("Reached EOF while parsing char in get_string_literal.".to_string()))?;

            if let Some((SimpleToken::Symbol(Symbol::DoubleQuote), _)) = Symbol::lookup_one(current) {
                break;
            } else {
                char_vec.push(current);
            }
        }

        // Add +2 to size to include the removed quotes.
        Ok((
            SimpleToken::Literal(Literal::StringLiteral(char_vec.iter().collect())),
            char_vec.len() + 2,
        ))
    }

    // TODO: Fix escape chars etc.
    fn get_char_literal(&mut self) -> CustomResult<(SimpleToken, usize)> {
        let mut char_vec = Vec::new();
        self.next();    // Remove the SingleQuote start.

        loop {
            let current = self.next()
                .ok_or(LexError("Reached EOF while parsing char in get_char_literal.".to_string()))?;

            if let Some((SimpleToken::Symbol(Symbol::SingleQuote), _)) = Symbol::lookup_one(current) {
                break;
            } else {
                char_vec.push(current);
            }
        }

        // Add +2 to size to include the removed quotes.
        Ok((
            SimpleToken::Literal(Literal::CharLiteral(char_vec.iter().collect())),
            char_vec.len() + 2,
        ))
    }

    fn get_whitespaces(&mut self) -> CustomResult<SimpleToken> {
        let mut count = 0;

        while let Some(c) = self.next() {
            if c.is_whitespace() {
                count += 1;
            } else {
                self.put_back(c);
                break;
            }
        }

        Ok(SimpleToken::Symbol(Symbol::WhiteSpace(count)))
    }

    #[inline]
    pub fn skip(&mut self, n: usize) {
        for i in 0..n {
            self.next();
        }
    }

    #[inline]
    pub fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.next() {
            self.put_back(c);
            Some(c)
        } else {
            None
        }
    }

    #[inline]
    pub fn peek_three(&mut self) -> Option<(char, Option<char>, Option<char>)> {
        let c1 = if let Some(c) = self.next() {
            c
        } else {
            return None;
        };

        let c2 = if let Some(c) = self.next() {
            Some(c)
        } else {
            None
        };

        let c3 = if let Some(c) = self.next() {
            Some(c)
        } else {
            None
        };

        if let Some(c) = c3 {
            self.put_back(c);
        }
        if let Some(c) = c2 {
            self.put_back(c);
        }
        self.put_back(c1);

        Some((c1, c2, c3))
    }
}