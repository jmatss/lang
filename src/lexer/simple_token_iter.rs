use crate::error::CustomError::LexError;
use crate::lexer::simple_token::{SimpleToken, Symbol, Literal};
use std::fs::File;
use std::io::Read;
use std::vec::IntoIter;
use std::collections::linked_list::LinkedList;
use crate::CustomResult;

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
                if let Some(symbol_token) = SimpleToken::lookup_identifier(&id) {
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
            } else if let Some(symbol_token) = SimpleToken::lookup_three(c1, c2, c3) {

                // Add special cases for string- and char literals.
                let (token, n) = symbol_token;
                match token {
                    SimpleToken::Symbol(Symbol::DoubleQuote) =>
                        Ok(self.get_literal(Symbol::DoubleQuote)?),

                    SimpleToken::Symbol(Symbol::SingleQuote) =>
                        Ok(self.get_literal(Symbol::SingleQuote)?),

                    _ => {
                        self.skip(n);
                        Ok(token)
                    }
                }
            } else {
                Err(LexError(format!(
	                   "Didn't match a symbol token when c: {:?}, c2: {:?} and c2: {:?}",
                        c1, c2, c3
                )))
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
        SimpleTokenIter::valid_identifier_start(c) || c.is_numeric()
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
            if SimpleTokenIter::valid_identifier(c) {
                result.push(c);
            } else {
                self.put_back(c);
                break;
            }
        }

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(LexError("Empty result in get_identifier_string().".to_string()))
        }
    }

    // TODO: Hex/binary numbers with prefix 0x/0b
    // Fetches both integeres and floats.
    pub fn get_number(&mut self, radix: u32) -> CustomResult<SimpleToken> {
        let mut number = self.get_integer(radix)?;

        // If the next simple token is a Dot and the char after that is a number, this is a float.
        // FIXME: (?) might not need to check number after Dot to allow for 
        // example "1." instead of needing to write "1.0".
        if let Some(('.', Some(next))) = self.peek_two() {
            if SimpleTokenIter::valid_number(next, radix) {
                self.skip(1);    // Remove dot.
                number = [number, self.get_integer(radix)?].join(".");
            }
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
            .ok_or_else(|| LexError("Reached EOF while parsing first char in get_linebreak.".to_string()))?;

        if c == '\n' {
            Ok(SimpleToken::Symbol(Symbol::LineBreak))
        } else if c == '\r' {
            let c_next = self.next()
                .ok_or_else(|| LexError("Reached EOF after a '\\r' had been parsed.".to_string()))?;

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
    fn get_literal(&mut self, literal_symbol: Symbol) -> CustomResult<SimpleToken> {
        let mut char_vec = Vec::new();
        self.next();    // Remove the start "symbol" (single or double-quote).

        loop {
            let current = self.next()
                .ok_or_else(|| LexError("Reached EOF while parsing char(s) in get_literal.".to_string()))?;
                
            match SimpleToken::lookup_one(current) {
                Some((SimpleToken::Symbol(s), _)) if s == literal_symbol => 
                    break,
                _ => 
                    char_vec.push(current)
            }
        }

        Ok(SimpleToken::Literal(Literal::StringLiteral(char_vec.iter().collect())))
    }

    fn get_whitespaces(&mut self) -> CustomResult<SimpleToken> {
        let mut count = 0;

        while let Some(c) = self.next() {
            let (_, c_next) = self.peek_two()
                .ok_or_else(|| LexError("Empty peek during get_whitespaces.".to_string()))?;
                
            // Since linebreaks counts as whitespaces in rust, need to manually make
            // sure that the current char(s) isn't a linebreak in the if-statement.
            if SimpleTokenIter::valid_whitespace(c) && !SimpleTokenIter::valid_linebreak(c, c_next) {
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
        for _ in 0..n {
            self.next();
        }
    }

    #[inline]
    pub fn peek_n(&mut self, n: usize) -> Vec<char> {
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

    #[inline]
    pub fn peek_two(&mut self) -> Option<(char, Option<char>)> {
        let chars = self.peek_n(2);
        Some(
            match chars.len() {
                2 => (chars[0], Some(chars[1])),
                1 => (chars[0], None),
                _ => return None
            }
        )
    }

    #[inline]
    pub fn peek_three(&mut self) -> Option<(char, Option<char>, Option<char>)> {
        let chars = self.peek_n(3);
        Some(
            match chars.len() {
                3 => (chars[0], Some(chars[1]), Some(chars[2])),
                2 => (chars[0], Some(chars[1]), None),
                1 => (chars[0], None, None),
                _ => return None
            }
        )
    }
}