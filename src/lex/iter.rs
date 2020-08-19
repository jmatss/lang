use super::token::LexTokenKind;
use crate::error::{LangError, LangErrorKind::LexError};
use crate::lex::token::{LexToken, Literal, Symbol};
use crate::{common::iter::TokenIter, CustomResult};

pub struct LexTokenIter {
    /// Use to iterate over the character tokens.
    iter: TokenIter<char>,

    /// Current line number (or rather last seen line number).
    cur_line_nr: u64,

    /// Current column number (or rather last seen column number).
    cur_column_nr: u64,
}

impl LexTokenIter {
    pub fn new(content: &str) -> Self {
        // TODO: This copies all chars, change to not make a copy if possible.
        Self {
            iter: TokenIter::new(content.chars().collect::<Vec<_>>()),
            cur_line_nr: 1,
            cur_column_nr: 1,
        }
    }

    /// Gets the next LexToken from the iterator.
    pub fn next_token(&mut self) -> CustomResult<LexToken> {
        // A radix of 10 will match all type of numbers since non decimal numbers
        // are prefixed with "0x", "0b" or "0o" (which starts with radix 10).
        const RADIX: u32 = 10;

        let line_nr = self.cur_line_nr;
        let column_nr = self.cur_column_nr;

        let kind = if let Some((c1, c2, c3)) = self.iter.peek_three() {
            if LexTokenIter::valid_linebreak(c1, c2) {
                self.get_linebreak()?
            } else if LexTokenIter::valid_whitespace(c1) {
                self.get_whitespaces()?
            } else if LexTokenIter::valid_number(c1, RADIX) {
                self.get_number()?
            } else if LexTokenIter::valid_identifier_start(c1) {
                let ident: String = self.get_ident()?;

                // Check if this identifier is a valid:
                //   keyword        ("if", "enum" etc.)
                //   symbol         ("and", "as" etc.)
                //   bool literal   (true/false)
                if let Some(symbol_kind) = LexToken::get_if_symbol(&ident) {
                    symbol_kind
                } else if let Some(keyword_kind) = LexToken::get_if_keyword(&ident) {
                    keyword_kind
                } else if let Some(bool_kind) = LexToken::get_if_bool(&ident) {
                    bool_kind
                } else {
                    LexTokenKind::Identifier(ident)
                }
            } else if let Some(symbol_kind_tup) = LexToken::get_if_symbol_three_chars(c1, c2, c3) {
                let (symbol_kind, n) = symbol_kind_tup;

                // Add special cases for string- and char literals, they start
                // and end with " or '.
                // Also filter out comments. If a comment is found, debug print
                // the comment and parse the next token after the comment.
                match symbol_kind {
                    // TODO: Add multiline comments.
                    LexTokenKind::Symbol(Symbol::CommentSingleLine) => {
                        let _ = self.get_comment_single(n)?;
                        // TODO: Weird to be the only return statement, do this
                        //       in a better way.
                        return self.next_token();
                    }
                    LexTokenKind::Symbol(Symbol::DoubleQuote) => self.get_lit_string()?,
                    LexTokenKind::Symbol(Symbol::SingleQuote) => self.get_lit_char()?,
                    _ => {
                        self.iter.skip(n);
                        self.cur_column_nr += n as u64;
                        symbol_kind
                    }
                }
            } else {
                let msg = format!(
                    "Didn't match a symbol token when c: {:?}, c2: {:?} and c2: {:?}",
                    c1, c2, c3
                );
                // Consume a token to move the iterator forward.
                self.iter.next();
                return Err(self.err(msg));
            }
        } else {
            LexTokenKind::EndOfFile
        };

        Ok(LexToken::new(kind, line_nr, column_nr))
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
    fn get_ident(&mut self) -> CustomResult<String> {
        let mut result = String::new();

        while let Some(c) = self.iter.next() {
            if LexTokenIter::valid_identifier(c) {
                result.push(c);
            } else {
                self.iter.put_back(c)?;
                break;
            }
        }

        self.cur_column_nr += result.chars().count() as u64;

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(self.err("Empty result in get_ident().".into()))
        }
    }

    // TODO: number containing scientifical notaion (e/E).
    /// Returns the number (int or float) at the current position of the iterator.
    fn get_number(&mut self) -> CustomResult<LexTokenKind> {
        let radix = if let Some(('0', Some(sep_char))) = self.iter.peek_two() {
            // Remove the assumed prefix.
            self.iter.skip(2);

            match sep_char.to_ascii_uppercase() {
                'X' => 16,
                'B' => 2,
                'O' => 8,
                _ => {
                    // Put back the chars since they aren't part of a prefix,
                    // this is just a decimal number that start with '0'.
                    self.iter.put_back(sep_char)?;
                    self.iter.put_back('0')?;
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

        self.cur_column_nr += number.chars().count() as u64;

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
                self.iter.put_back(c)?;
                break;
            }
        }

        Ok(numbers.into_iter().collect())
    }

    // https://doc.rust-lang.org/reference/tokens.html
    // Valid escape chars:
    //   \x41   ("raw byte" escape (hex))
    //   \n     (newline)
    //   \r     (carriage return)
    //   \t     (tab)
    //   \\     (backslash)
    //   \0     (null)
    //   \'     (single quote)
    //   \"     (double quote)
    //
    // TODO: Unicode escape. Example: \u{7FFF}  (24-bit, up to 6 digits)
    // TODO: Formatting string. Example "num x: {x}"  (== format!("num x: {}", x))
    /// Returns the string or char literal at the current position of the iterator.
    /// Will also escape any escape characters in the process.
    fn get_lit(&mut self, literal_symbol: Symbol) -> CustomResult<String> {
        let mut chars = Vec::new();

        // Remove the start "symbol" (single or double-quote).
        // `column_count` starts at 1 since it includes the start symbol.
        self.iter.skip(1);
        let mut column_count = 1;

        // Iterate through all char tokens in the string literal and add them to
        // the `chars` vector until the end of the literal.
        // In this loop all escaped characters will be substituted with the
        // corresponding "raw" escape symbols.
        let mut prev_slash = false;
        while let Some(ch) = self.iter.next() {
            column_count += 1;
            match ch {
                '\\' if prev_slash => {
                    chars.push('\\');
                    prev_slash = false;
                }
                '\\' if !prev_slash => {
                    prev_slash = true;
                }

                // TODO: Should upper 'X' be allowed?
                'x' if prev_slash => {
                    let new_ch = self.escape_raw_byte()?;
                    chars.push(new_ch);
                    prev_slash = false;
                }

                'n' if prev_slash => {
                    chars.push('\n');
                    prev_slash = false;
                }
                'r' if prev_slash => {
                    chars.push('\r');
                    prev_slash = false;
                }
                't' if prev_slash => {
                    chars.push('\t');
                    prev_slash = false;
                }

                '0' if prev_slash => {
                    chars.push('\0');
                    prev_slash = false;
                }
                '\"' if prev_slash => {
                    chars.push('\"');
                    prev_slash = false;
                }
                '\'' if prev_slash => {
                    chars.push('\'');
                    prev_slash = false;
                }

                // For every char that isn't escaped, if it is the `literal_symbol`,
                // this is the end of the literal, break out of the loop.
                // For all other characters, just add them to the literal result.
                _ => {
                    if let Some((LexTokenKind::Symbol(s), _)) = LexToken::get_if_symbol_char(ch) {
                        if s == literal_symbol {
                            break;
                        }
                    }
                    chars.push(ch);
                    prev_slash = false;
                }
            }
        }

        self.cur_column_nr += column_count;

        Ok(chars.iter().collect())
    }

    /// Escapes a character sequence in the format: "0xAA" inside a string or
    /// char literal into a single character. The two digits are hex.
    fn escape_raw_byte(&mut self) -> CustomResult<char> {
        let radix = 16;
        let first = self
            .iter
            .next()
            .and_then(|ch| ch.to_digit(radix))
            .ok_or_else(|| self.err("None when parsing first digit \"raw byte\".".into()))?;
        let second = self
            .iter
            .next()
            .and_then(|ch| ch.to_digit(radix))
            .ok_or_else(|| self.err("None when parsing second digit \"raw byte\".".into()))?;

        let num = (first << 4) | second;
        if let Some(new_ch) = std::char::from_u32(num) {
            Ok(new_ch)
        } else {
            Err(self.err(format!(
                "Unable to convert escaped \"raw byte\" integer to char: {}",
                num
            )))
        }
    }

    /// Returns the string literal at the current position of the iterator.
    fn get_lit_string(&mut self) -> CustomResult<LexTokenKind> {
        Ok(LexTokenKind::Literal(Literal::StringLiteral(
            self.get_lit(Symbol::DoubleQuote)?,
        )))
    }

    /// Returns the char literal at the current position of the iterator.
    fn get_lit_char(&mut self) -> CustomResult<LexTokenKind> {
        // Since this is a char literal, need to make sure that the given
        // literal has the character length "1".
        let char_lit = self.get_lit(Symbol::SingleQuote)?;
        if char_lit.chars().count() == 1 {
            Ok(LexTokenKind::Literal(Literal::CharLiteral(char_lit)))
        } else {
            Err(self.err(format!(
                "Char literal length not 1, len is {}. The literal: {}",
                char_lit.chars().count(),
                char_lit
            )))
        }
    }

    /// Returns the line break at the current position of the iterator.
    fn get_linebreak(&mut self) -> CustomResult<LexTokenKind> {
        self.cur_line_nr += 1;
        self.cur_column_nr = 1;

        if let Some(peek_chars) = self.iter.peek_two() {
            if let ('\n', _) = peek_chars {
                self.iter.skip(1);
                Ok(LexTokenKind::Symbol(Symbol::LineBreak))
            } else if let ('\r', Some('\n')) = peek_chars {
                self.iter.skip(2);
                Ok(LexTokenKind::Symbol(Symbol::LineBreak))
            } else {
                Err(self.err("No linebreak character received in get_linebreak.".into()))
            }
        } else {
            Err(self.err("Received None in get_linebreak.".into()))
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
                self.iter.put_back(c)?;
                break;
            }
        }

        self.cur_column_nr += count as u64;

        Ok(LexTokenKind::Symbol(Symbol::WhiteSpace(count)))
    }

    /// Lexes a single line comment and returns the comment contents.
    /// Does NOT consume the linebreak ending the comment.
    /// The n is the size of the "CommentSingleLine" symbol.
    fn get_comment_single(&mut self, n: usize) -> CustomResult<String> {
        let mut comment = Vec::new();

        // Consume the "CommentSingleLine" symbol.
        for _ in 0..n {
            self.iter.next();
        }

        while let Some(c) = self.iter.next() {
            if LexTokenIter::valid_linebreak(c, self.iter.peek()) {
                self.iter.put_back(c)?;
                break;
            }
            comment.push(c);
        }

        Ok(comment.iter().collect())
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new_backtrace(
            msg,
            LexError {
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            },
            true,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_identifier_start() {
        let valid_chars = ['a', 'x', '_'];
        let invalid_chars = ['0', '-', '.', '/', '#'];
        for ch in valid_chars.iter() {
            assert!(LexTokenIter::valid_identifier_start(*ch));
        }
        for ch in invalid_chars.iter() {
            assert!(!LexTokenIter::valid_identifier_start(*ch));
        }
    }

    #[test]
    fn test_valid_identifier() {
        let valid_chars = ['a', 'x', '_', '0'];
        let invalid_chars = ['-', '.', '/', '#'];
        for ch in valid_chars.iter() {
            assert!(LexTokenIter::valid_identifier(*ch));
        }
        for ch in invalid_chars.iter() {
            assert!(!LexTokenIter::valid_identifier(*ch));
        }
    }

    #[test]
    fn test_valid_number() {
        const DEC_RADIX: u32 = 10;
        const HEX_RADIX: u32 = 16;
        let invalid_dec = ['a', 'g', 'x', '-', '.', '/', '#'];
        let invalid_hex = ['g', 'x', '-', '.', '/', '#'];

        for ch in '0'..='9' {
            assert!(LexTokenIter::valid_number(ch, DEC_RADIX));
        }
        for ch in '0'..='9' {
            assert!(LexTokenIter::valid_number(ch, HEX_RADIX));
        }
        for ch in 'a'..='f' {
            assert!(LexTokenIter::valid_number(ch, HEX_RADIX));
        }
        for ch in 'A'..='F' {
            assert!(LexTokenIter::valid_number(ch, HEX_RADIX));
        }

        for ch in invalid_dec.iter() {
            assert!(!LexTokenIter::valid_number(*ch, DEC_RADIX));
        }
        for ch in invalid_hex.iter() {
            assert!(!LexTokenIter::valid_number(*ch, HEX_RADIX));
        }
    }

    #[test]
    fn test_valid_linebreak() {
        let valid_break = [('\n', None), ('\r', Some('\n'))];
        let invalid_break = [('a', None), ('\r', None), ('\r', Some('a'))];

        for (ch1, ch2) in valid_break.iter() {
            assert!(LexTokenIter::valid_linebreak(*ch1, *ch2));
        }
        for (ch1, ch2) in invalid_break.iter() {
            assert!(!LexTokenIter::valid_linebreak(*ch1, *ch2));
        }
    }

    #[test]
    fn test_valid_whitespace() {
        let valid_space = [' ', '\t'];
        let invalid_space = ['a', '#', '_'];

        for ch in valid_space.iter() {
            assert!(LexTokenIter::valid_whitespace(*ch));
        }
        for ch in invalid_space.iter() {
            assert!(!LexTokenIter::valid_whitespace(*ch));
        }
    }

    #[test]
    fn test_get_lit() {
        let input = "\"abc 123 åäö\"";
        let expected = "abc 123 åäö";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::DoubleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape backslash.
        let input = "\'\\\\\'";
        let expected = "\\";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape "raw byte".
        let input = "\'\\x41\'";
        let expected = "A";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape newline.
        let input = "\'\\n\'";
        let expected = "\n";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape carriage return.
        let input = "\'\\r\'";
        let expected = "\r";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape tab.
        let input = "\'\\t\'";
        let expected = "\t";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape null.
        let input = "\'\\0\'";
        let expected = "\0";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape single quote.
        let input = "\'\\\'\'";
        let expected = "\'";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape double quote.
        let input = "\'\\\"\'";
        let expected = "\"";
        let actual = LexTokenIter::new(input)
            .get_lit(Symbol::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);
    }
}
