use super::token::LexTokenKind;
use crate::token::LexToken;
use crate::token::Sym;
use common::error::LangError;
use common::{
    error::{CustomResult, LangErrorKind::LexError},
    iter::TokenIter,
    token::lit::Lit,
};
use log::debug;
use std::fs::File;
use std::io::Read;

/// Lexes the characters in the source code to LexToken's and returns a vector
/// containing all lex tokens.
pub fn lex(filename: &str) -> Result<Vec<LexToken>, Vec<LangError>> {
    let mut contents = String::new();

    File::open(filename)
        .and_then(|mut f| f.read_to_string(&mut contents))
        .map_err(|e| vec![e.into()])?;

    let mut iter = LexTokenIter::new(&contents);
    let mut lex_token_vec = Vec::new();
    let mut errors = Vec::new();

    loop {
        match iter.next_token() {
            Ok(lex_token) if lex_token.is_eof() => {
                lex_token_vec.push(lex_token);
                break;
            }
            Ok(lex_token) => lex_token_vec.push(lex_token),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(lex_token_vec)
    } else {
        Err(errors)
    }
}

pub struct LexTokenIter {
    /// Use to iterate over the character tokens.
    iter: TokenIter<char>,

    /// Current line number (or rather last seen line number).
    line_nr: u64,

    /// Current column number (or rather last seen column number).
    column_nr: u64,
}

impl LexTokenIter {
    pub fn new(content: &str) -> Self {
        // TODO: This copies all chars, change to not make a copy if possible.
        Self {
            iter: TokenIter::new(content.chars().collect::<Vec<_>>()),
            line_nr: 1,
            column_nr: 1,
        }
    }

    /// Gets the next LexToken from the iterator.
    pub fn next_token(&mut self) -> CustomResult<LexToken> {
        // A radix of 10 will match all type of numbers since non decimal numbers
        // are prefixed with "0x", "0b" or "0o" (which starts with valid
        // radix 10 numbers).
        const RADIX: u32 = 10;

        let line_nr = self.line_nr;
        let column_nr = self.column_nr;

        let kind = if let Some((c1, c2, c3)) = self.iter.peek_three() {
            if LexTokenIter::valid_linebreak(c1, c2) {
                self.get_linebreak()?
            } else if LexTokenIter::valid_whitespace(c1) {
                self.get_whitespaces()
            } else if LexTokenIter::valid_number(c1, RADIX) {
                self.get_number()
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
                    LexTokenKind::Ident(ident)
                }
            } else if let Some(symbol_kind_tup) = LexToken::get_if_symbol_three_chars(c1, c2, c3) {
                let (symbol_kind, n) = symbol_kind_tup;

                // Add special cases for string- and char literals, they start
                // and end with " or '
                // Also filter out comments. If a comment is found, debug print
                // the comment and parse the next token after the comment.
                match symbol_kind {
                    LexTokenKind::Sym(Sym::CommentSingleLine) => {
                        let comment = self.get_comment_single(n);
                        debug!("Single-line comment: {:?}", comment);
                        return self.next_token();
                    }
                    LexTokenKind::Sym(Sym::CommentMultiLineBegin) => {
                        let comment = self.get_comment_multi(n);
                        debug!("Multi-line comment: {:?}", comment);
                        return self.next_token();
                    }
                    LexTokenKind::Sym(Sym::DoubleQuote) => self.get_lit_string()?,
                    LexTokenKind::Sym(Sym::SingleQuote) => self.get_lit_char()?,
                    _ => {
                        self.iter.skip(n);
                        self.column_nr += n as u64;
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
            LexTokenKind::EOF
        };

        let lex_token = LexToken::new(kind, line_nr, column_nr);
        debug!("{:?}", lex_token);
        Ok(lex_token)
    }

    #[inline]
    fn valid_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    #[inline]
    fn valid_identifier(c: char) -> bool {
        LexTokenIter::valid_identifier_start(c) || LexTokenIter::valid_number(c, 10)
    }

    #[inline]
    fn valid_number(c: char, radix: u32) -> bool {
        c.is_digit(radix)
    }

    #[inline]
    fn valid_linebreak(c: char, c_next: Option<char>) -> bool {
        LexTokenIter::valid_linebreak_n(c) || LexTokenIter::valid_linebreak_rn(c, c_next)
    }

    #[inline]
    fn valid_linebreak_n(c: char) -> bool {
        c == '\n'
    }

    #[inline]
    fn valid_linebreak_rn(c: char, c_next: Option<char>) -> bool {
        c == '\r' && c_next.map_or(false, |x| x == '\n')
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
                self.iter.rewind();
                break;
            }
        }

        self.column_nr += result.chars().count() as u64;

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(self.err("Empty result in get_ident().".into()))
        }
    }

    // TODO: number containing scientifical notaion (e/E).
    /// Returns the number (int or float) at the current position of the iterator.
    fn get_number(&mut self) -> LexTokenKind {
        let radix = if let Some(('0', Some(sep_char))) = self.iter.next_two() {
            match sep_char.to_ascii_uppercase() {
                'X' => 16,
                'B' => 2,
                'O' => 8,
                _ => 10,
            }
        } else {
            // Decimal.
            10
        };

        if radix == 10 {
            // Rewind the position of the iterator to include the two read
            // characters from above since they aren't part of a prefix.
            self.iter.rewind_n(2);
        } else {
            self.column_nr += 2;
        }

        // Parse the number. If the this is a integer, the whole number will
        // be parsed. If this is a float, the number after the dot will be parsed
        // in the logic below.
        let mut number = self.get_integer(radix);

        // Will be set to true if the column number should be decrement with one.
        // This is true for floats that doesn't have a number after the "dot".
        let mut dec_column_nr = false;

        // If this number contains a dot, assume it is a float number.
        // Currently a float number doesn't need to have a number after the dot,
        // ex. "1." is allowed and is treated as "1.0".
        let is_float = if let Some(('.', Some(next))) = self.iter.peek_two() {
            self.iter.skip(1); // Skip over the "dot" in the iterator.

            if LexTokenIter::valid_number(next, radix) {
                number = [number, self.get_integer(radix)].join(".");
                dec_column_nr = false;
            } else {
                number.push_str(".0");
                dec_column_nr = true;
            }

            true
        } else {
            false
        };

        self.column_nr += number.chars().count() as u64;
        if dec_column_nr {
            self.column_nr -= 1;
        }

        if is_float {
            LexTokenKind::Lit(Lit::Float(number))
        } else {
            LexTokenKind::Lit(Lit::Integer(number, radix))
        }
    }

    /// Returns the integer at the current position of the iterator with
    /// the radix/base `radix`.
    fn get_integer(&mut self, radix: u32) -> String {
        let mut numbers = Vec::new();

        while let Some(c) = self.iter.next() {
            if LexTokenIter::valid_number(c, radix) {
                numbers.push(c);
            } else {
                self.iter.rewind();
                break;
            }
        }

        numbers.into_iter().collect()
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
    fn get_lit(&mut self, literal_symbol: Sym) -> CustomResult<String> {
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
                    if let Some((LexTokenKind::Sym(s), _)) = LexToken::get_if_symbol_char(ch) {
                        if s == literal_symbol {
                            break;
                        }
                    }
                    chars.push(ch);
                    prev_slash = false;
                }
            }
        }

        self.column_nr += column_count;

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
        Ok(LexTokenKind::Lit(Lit::String(
            self.get_lit(Sym::DoubleQuote)?,
        )))
    }

    /// Returns the char literal at the current position of the iterator.
    fn get_lit_char(&mut self) -> CustomResult<LexTokenKind> {
        // Since this is a char literal, need to make sure that the given
        // literal has the character length "1".
        let char_lit = self.get_lit(Sym::SingleQuote)?;
        if char_lit.chars().count() == 1 {
            Ok(LexTokenKind::Lit(Lit::Char(char_lit)))
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
        match self.iter.peek_two() {
            Some(('\n', _)) => self.iter.skip(1),
            Some(('\r', Some('\n'))) => self.iter.skip(2),
            fail => {
                return Err(self.err(format!("Found bad line break in get_linebreak: {:?}", fail)))
            }
        }

        self.line_nr += 1;
        self.column_nr = 1;

        Ok(LexTokenKind::Sym(Sym::LineBreak))
    }

    /// Returns all the consecutive white spaces at the current position of the
    /// iterator.
    fn get_whitespaces(&mut self) -> LexTokenKind {
        let mut count = 0;

        while let Some(c) = self.iter.next() {
            let c_next = self.iter.peek();

            // Since line breaks counts as whitespaces in rust, need to manually make
            // sure that the current char(s) isn't a linebreak in the if-statement.
            if LexTokenIter::valid_whitespace(c) && !LexTokenIter::valid_linebreak(c, c_next) {
                count += 1;
            } else {
                self.iter.rewind();
                break;
            }
        }

        self.column_nr += count as u64;

        LexTokenKind::Sym(Sym::WhiteSpace(count))
    }

    /// Lexes a single line comment and returns the comment contents.
    /// Does NOT consume the linebreak ending the comment.
    /// The n is the size of the "CommentSingleLine" symbol.
    fn get_comment_single(&mut self, n: usize) -> String {
        let mut comment = Vec::new();

        // Consume the "CommentSingleLine" symbol.
        for _ in 0..n {
            self.iter.next();
        }

        while let Some(c) = self.iter.next() {
            if LexTokenIter::valid_linebreak(c, self.iter.peek()) {
                self.iter.rewind();
                break;
            }
            comment.push(c);
        }

        let comment_str = comment.iter().collect::<String>();
        self.column_nr += (n + comment_str.chars().count()) as u64;

        comment_str
    }

    /// Lexes a multi line comment until the matching "CommentMultiLineEnd" is
    /// found. Comment can be "recursive". This function returns the comment contents.
    /// The n is the size of the "CommentMultiLineBegin" symbol.
    fn get_comment_multi(&mut self, n: usize) -> String {
        let mut comment = Vec::new();

        // Consume the "CommentMultiLineBegin" symbol.
        for _ in 0..n {
            self.iter.next();
        }

        // Keep a count of all "CommentMultiLineBegin" seen. This variable is
        // incremented for every "CommentMultiLineBegin" and decrement for every
        // "CommentMultiLineEnd". When this variable is back to zero, the comment
        // has been fully consumed.
        let mut multi_line_comment_begin_count = 1;

        // Used to prevent "\r\n" being treated as two linebreaks (first \r\n
        // and then \n).
        let mut prev_was_linebreak_rn = false;

        // Keeps track of the current column count when parsing the current line
        // of the comment. When parsing the last line, this column will be used
        // to update the `self.cur_column_nr`. Starts with `n` for the first line
        // which is the length of the "CommentMultiLineBegin".
        let mut line_column_count = n;

        // Used to indicate if this comment streched over multiple lines or just
        // a single one. If it is only on one line, the "column count" for this
        // comment will be added to the current `self.cur_column_nr`. Otherwise
        // it will just overwrite it with the current col count in this function.
        let mut multi_line = false;

        while let Some(c1) = self.iter.next() {
            let (c2, c3) = if let Some(chars) = self.iter.peek_two() {
                let (c2, c3) = chars;
                (Some(c2), c3)
            } else {
                panic!("None when peeking in multi line comment.")
            };

            if let Some((lex_token_kind, sym_len)) = LexToken::get_if_symbol_three_chars(c1, c2, c3)
            {
                match lex_token_kind {
                    LexTokenKind::Sym(Sym::CommentMultiLineBegin) => {
                        multi_line_comment_begin_count += 1;
                    }
                    LexTokenKind::Sym(Sym::CommentMultiLineEnd) => {
                        multi_line_comment_begin_count -= 1;
                    }
                    _ => (),
                }

                if multi_line_comment_begin_count == 0 {
                    // -1 because the first char has already been "consumed".
                    self.iter.skip(sym_len - 1);
                    line_column_count += sym_len - 1;
                    break;
                }
            }

            // Increment line number if a linebreak is found. Make sure to not
            // count any "\n" after a "\r\n" was seen to prevent counting those
            // linebreaks twice.
            if LexTokenIter::valid_linebreak(c1, c2) {
                if !(prev_was_linebreak_rn && LexTokenIter::valid_linebreak_n(c1)) {
                    self.line_nr += 1;
                }
                line_column_count = 0;
                multi_line = true;
            } else {
                line_column_count += 1;
            }

            prev_was_linebreak_rn = LexTokenIter::valid_linebreak_rn(c1, c2);

            comment.push(c1);
        }

        if multi_line {
            self.column_nr = (line_column_count) as u64;
        } else {
            self.column_nr += (n + line_column_count) as u64;
        }

        comment.iter().collect()
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new_backtrace(
            msg,
            LexError {
                line_nr: self.line_nr,
                column_nr: self.column_nr,
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
            .get_lit(Sym::DoubleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape backslash.
        let input = "\'\\\\\'";
        let expected = "\\";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape "raw byte".
        let input = "\'\\x41\'";
        let expected = "A";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape newline.
        let input = "\'\\n\'";
        let expected = "\n";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape carriage return.
        let input = "\'\\r\'";
        let expected = "\r";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape tab.
        let input = "\'\\t\'";
        let expected = "\t";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape null.
        let input = "\'\\0\'";
        let expected = "\0";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape single quote.
        let input = "\'\\\'\'";
        let expected = "\'";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape double quote.
        let input = "\'\\\"\'";
        let expected = "\"";
        let actual = LexTokenIter::new(input)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);
    }
}
