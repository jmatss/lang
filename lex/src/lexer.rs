use super::token::LexTokenKind;
use crate::token::Sym;
use crate::{char_iter::CharIter, token::LexToken};
use common::{
    error::LangError,
    file::{FileId, FileInfo, FilePosition},
};
use common::{
    error::{CustomResult, LangErrorKind::LexError},
    token::lit::Lit,
};
use log::debug;

/// Lexes the characters in the source code to LexToken's and returns a vector
/// containing all lex tokens.
pub fn lex(file_nr: FileId, file_info: &FileInfo) -> Result<Vec<LexToken>, Vec<LangError>> {
    let mut content = std::fs::read(&file_info.full_path()).map_err(|e| vec![e.into()])?;

    let mut iter = LexTokenIter::new(&mut content, file_nr);
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

pub struct LexTokenIter<'a> {
    /// Use to iterate over the character tokens.
    iter: CharIter<'a>,

    /// Number of the file that is currently being parsed. This is mapped to a
    /// directory/filename.
    file_nr: u64,

    /// The offset (in bytes) inside the current file.
    offset: u64,

    /// Current line number (or rather last seen line number).
    line: u64,

    /// Current column number (or rather last seen column number).
    column: u64,

    /// Keeps track of the previously created file position. This can be handly
    /// ex. when finding errors. Before doing any calculations, the position is
    /// stored in this variable. If a error happens while the new position isn't
    /// fully calculated, this variable will information about the current valid
    /// token position.
    file_pos: FilePosition,
}

impl<'a> LexTokenIter<'a> {
    pub fn new(content: &'a mut [u8], file_nr: u64) -> Self {
        Self {
            iter: CharIter::new(content),
            file_nr,
            offset: 0,
            line: 1,
            column: 1,
            file_pos: FilePosition::default(),
        }
    }

    /// Gets the next LexToken from the iterator.
    fn next_token(&mut self) -> CustomResult<LexToken> {
        // A radix of 10 will match all type of numbers since non decimal numbers
        // are prefixed with "0x", "0b" or "0o" (which starts with valid
        // radix 10 numbers).
        const RADIX: u32 = 10;

        // Save the current position in the file before starting to lex the token.
        // At this point, the offset can't be calculated. It will be set if the
        // token is fully lexed correctly (at the end of this function).
        self.file_pos = FilePosition::new(self.file_nr, self.offset, self.line, self.column);

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
            } else if let Some((symbol_kind, n)) = LexToken::get_if_symbol_three_chars(c1, c2, c3) {
                // Add special cases for string- and char literals, they start
                // and end with " or '. Also handle comments here since they
                // also start with symbols (// and /*).
                match symbol_kind {
                    LexTokenKind::Sym(Sym::CommentSingleLine) => self.get_comment_single(n),
                    LexTokenKind::Sym(Sym::CommentMultiLineBegin) => self.get_comment_multi(n),
                    LexTokenKind::Sym(Sym::DoubleQuote) => self.get_lit_string()?,
                    LexTokenKind::Sym(Sym::SingleQuote) => self.get_lit_char()?,
                    _ => {
                        self.iter.skip(n);
                        self.inc_column_and_offset(n as u64);
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

        // Set the ending file positions when the whole token has been parsed.
        self.file_pos
            .set_end_offset(self.offset)?
            .set_end_pos(self.line, self.column);

        let lex_token = LexToken::new(kind, self.file_pos);
        debug!("{:?}", lex_token);
        Ok(lex_token)
    }

    /// Helper function to increase both `self.column` and `self.offset` at the
    /// same time. This can be done ex. when parsing symbols since it is known
    /// that every symbol character is one byte (UTF-8 len).
    ///
    /// This function can NOT be used when parsing characters that might be UTF-8
    /// characters which are greater than one byte. This is because `self.column`
    /// counts in UTF-8 characters while `self.offset` counts bytes.
    #[inline]
    fn inc_column_and_offset(&mut self, n: u64) {
        self.column += n;
        self.offset += n;
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

        self.column += result.chars().count() as u64;
        self.offset += result.len() as u64;

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(self.err("Empty result in get_ident().".into()))
        }
    }

    // TODO: number containing scientifical notaion (e/E).
    /// Returns the number (int or float) at the current position of the iterator.
    fn get_number(&mut self) -> LexTokenKind {
        let radix = if let Some(('0', Some(sep_char))) = self.iter.peek_two() {
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

        if radix != 10 {
            // Skip and count the prefix for numbers that isn't radix 10.
            self.iter.skip(2);
            self.inc_column_and_offset(2);
        }

        // Parse the number. If the this is a integer, the whole number will
        // be parsed. If this is a float, the number after the dot will be parsed
        // in the logic below.
        let mut number = self.get_integer(radix);

        // Will be set to true if the column number should be decrement with one.
        // This is true for floats that doesn't have a number after the "dot".
        let mut zero_char_appended = false;

        // If this number contains a dot, assume it is a float number.
        // Currently a float number doesn't need to have a number after the dot,
        // ex. "1." is allowed and is treated as "1.0".
        let is_float = if let Some(('.', Some(next))) = self.iter.peek_two() {
            self.iter.skip(1); // Skip over the "dot" in the iterator.

            if LexTokenIter::valid_number(next, radix) {
                number = [number, self.get_integer(radix)].join(".");
                zero_char_appended = false;
            } else {
                number.push_str(".0");
                zero_char_appended = true;
            }

            true
        } else {
            false
        };

        // Characters for numbers and dots('.') are always 1 byte in UTF-8 so
        // can use the `inc_column_and_offset` safely.
        if zero_char_appended {
            self.inc_column_and_offset(number.len() as u64 - 1);
        } else {
            self.inc_column_and_offset(number.len() as u64);
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
    //   \u{7FFF}  (unicode escape, up to 6 digits (24 bit))
    //   \x41      ("raw byte" escape (hex))
    //   \n        (newline)
    //   \r        (carriage return)
    //   \t        (tab)
    //   \\        (backslash)
    //   \0        (null)
    //   \'        (single quote)
    //   \"        (double quote)
    //
    // TODO: Formatting string. Example "num x: {x}"  (== format!("num x: {}", x))
    /// Returns the string or char literal at the current position of the iterator.
    /// Will also escape any escape characters in the process.
    fn get_lit(&mut self, literal_symbol: Sym) -> CustomResult<String> {
        let mut chars = Vec::new();

        // Remove the start "symbol" (single or double-quote).
        // Starts at 1 since they includes the start symbol.
        self.iter.skip(1);
        let mut column_count = 1;
        let mut column_count_bytes = 1;

        // Iterate through all char tokens in the string literal and add them to
        // the `chars` vector until the end of the literal.
        // In this loop all escaped characters will be substituted with the
        // corresponding "raw" escape symbols.
        let mut prev_slash = false;
        while let Some(ch) = self.iter.next() {
            column_count += 1;
            column_count_bytes += ch.len_utf8() as u64;

            match ch {
                '\\' if prev_slash => {
                    chars.push('\\');
                    prev_slash = false;
                }
                '\\' if !prev_slash => {
                    prev_slash = true;
                }

                // Hex escape.
                'x' | 'X' if prev_slash => {
                    let (new_ch, count) = self.escape_hex()?;
                    chars.push(new_ch);

                    column_count += count as u64;
                    column_count_bytes += count as u64;
                    prev_slash = false;
                }
                // Unicode escape.
                'u' | 'U' if prev_slash => {
                    let (new_ch, count) = self.escape_unicode()?;
                    chars.push(new_ch);

                    column_count += count as u64;
                    column_count_bytes += count as u64;
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

        self.column += column_count;
        self.offset += column_count_bytes;

        Ok(chars.iter().collect())
    }

    /// Escapes a character sequence in the format: "\xAA" inside a string or
    /// char literal into a single character. The two digits are hex.
    /// Returns the amount of characters that have been parsed (2).
    fn escape_hex(&mut self) -> CustomResult<(char, usize)> {
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
            Ok((new_ch, 2))
        } else {
            Err(self.err(format!(
                "Unable to convert escaped \"raw byte\" integer to char: {}",
                num
            )))
        }
    }

    /// Escapes a character sequence in the format: "\u{XXXXXX}" inside a string or
    /// char literal into a single character. The amount of numbers (X) should be
    /// between 1 and 6.
    ///
    /// The amount of lexed characters will be returned in the tuple as `usize`.
    /// This count includes the braces as well.
    fn escape_unicode(&mut self) -> CustomResult<(char, usize)> {
        let next_char = self.iter.next();
        if let Some('{') = next_char {
        } else {
            return Err(self.err(format!(
                "Expected `{{`character after `\\x` in unicode escape, got: {:?}",
                next_char
            )));
        }

        const MAX_SIZE: usize = 6;
        const RADIX: u32 = 16;
        let mut chars = Vec::with_capacity(MAX_SIZE);

        let mut i = 0;
        loop {
            let next_char = self.iter.next();
            match next_char {
                Some('}') => {
                    break;
                }

                _ if i > MAX_SIZE => {
                    return Err(self.err(format!(
                        "Found to many chars in unicode escape. Current char: {:?}, prev chars: {:#?}",
                        next_char, chars
                    )));
                }

                Some(c) if LexTokenIter::valid_number(c, RADIX) => {
                    chars.push(c);
                }

                _ => {
                    return Err(self.err(format!(
                        "Got invalid token when parsing unicode escape at index {}: {:#?}",
                        i, next_char
                    )));
                }
            }

            i += 1;
        }

        // The lexed characters are numbers + two braces which all have 1 byte
        // lengths in UTF-8.
        let char_count = chars.len() + 2;

        let num = u32::from_str_radix(&chars.iter().collect::<String>(), RADIX)?;
        if let Some(new_ch) = std::char::from_u32(num) {
            Ok((new_ch, char_count))
        } else {
            Err(self.err(format!(
                "Unable to convert escaped \"unicode\" integer to char: {}",
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
                "Char literal count not 1, len is {}. The literal: {}",
                char_lit.chars().count(),
                char_lit
            )))
        }
    }

    /// Returns the line break at the current position of the iterator.
    fn get_linebreak(&mut self) -> CustomResult<LexTokenKind> {
        match self.iter.peek_two() {
            Some(('\n', _)) => {
                self.iter.skip(1);
                self.inc_column_and_offset(1);
            }
            Some(('\r', Some('\n'))) => {
                self.iter.skip(2);
                self.inc_column_and_offset(2);
            }
            fail => {
                return Err(self.err(format!("Found bad line break in get_linebreak: {:?}", fail)))
            }
        }

        self.line += 1;
        self.column = 1;

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

        self.inc_column_and_offset(count as u64);

        LexTokenKind::Sym(Sym::WhiteSpace(count))
    }

    /// Lexes a single line comment and returns the comment contents.
    /// Does NOT consume the linebreak ending the comment.
    /// The n is the size of the "CommentSingleLine" symbol.
    fn get_comment_single(&mut self, n: usize) -> LexTokenKind {
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
        self.column += (n + comment_str.chars().count()) as u64;
        self.offset += (n + comment_str.len()) as u64;

        LexTokenKind::Comment(comment_str, true)
    }

    /// Lexes a multi line comment until the matching "CommentMultiLineEnd" is
    /// found. Comment can be "recursive". This function returns the comment contents.
    /// The n is the size of the "CommentMultiLineBegin" symbol.
    fn get_comment_multi(&mut self, n: usize) -> LexTokenKind {
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
        let mut column_count_line = n;

        // Keeps track of all bytes parsed. Start with `n` since that is the
        // length of the "CommentMultiLineBegin".
        let mut byte_count = n;

        while let Some(c1) = self.iter.next() {
            column_count_line += 1;
            byte_count += c1.len_utf8();

            let (c2, c3) = if let Some(chars) = self.iter.peek_two() {
                let (c2, c3) = chars;
                (Some(c2), c3)
            } else {
                panic!("None when peeking in multi line comment.")
            };

            if let Some((kind, sym_len)) = LexToken::get_if_symbol_three_chars(c1, c2, c3) {
                match kind {
                    LexTokenKind::Sym(sym @ Sym::CommentMultiLineBegin)
                    | LexTokenKind::Sym(sym @ Sym::CommentMultiLineEnd) => {
                        if let Sym::CommentMultiLineBegin = sym {
                            multi_line_comment_begin_count += 1;
                        } else if let Sym::CommentMultiLineEnd = sym {
                            multi_line_comment_begin_count -= 1;
                        }

                        // `c1` has already has been consumed from the iterator,
                        // so consume (`sym_len` - 1) to account for `c1`s length,
                        // which is 1 because all symbol chars have 1 byte UTF-8 len.
                        let skip_count = sym_len - 1;
                        self.iter.skip(skip_count);

                        column_count_line += skip_count;
                        byte_count += skip_count;
                    }

                    _ => (),
                }

                // If true: All nested comments have been lexed, time to clean-up
                // and return from this function.
                if multi_line_comment_begin_count == 0 {
                    break;
                }
            }

            // Increment line number if a linebreak is found. Make sure to not
            // count any "\n" after a "\r\n" was seen to prevent counting those
            // linebreaks twice.
            if LexTokenIter::valid_linebreak_rn(c1, c2)
                || (!prev_was_linebreak_rn && LexTokenIter::valid_linebreak_n(c1))
            {
                self.line += 1;
                column_count_line = 0;
            }

            prev_was_linebreak_rn = LexTokenIter::valid_linebreak_rn(c1, c2);

            comment.push(c1);
        }

        self.column = column_count_line as u64;
        self.offset += byte_count as u64;

        LexTokenKind::Comment(comment.iter().collect(), false)
    }

    /// Used when returing errors to include current line/column number.
    fn err(&self, msg: String) -> LangError {
        LangError::new(msg, LexError, Some(self.file_pos.to_owned()))
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
        let mut input = "\"abc 123 åäö\"".to_owned();
        let expected = "abc 123 åäö";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::DoubleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape backslash.
        let mut input = "\'\\\\\'".to_owned();
        let expected = "\\";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape "raw byte".
        let mut input = "\'\\x41\'".to_owned();
        let expected = "A";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape unicode.
        let mut input = "\'\\u{980}\'".to_owned();
        let expected = "ঀ";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape newline.
        let mut input = "\'\\n\'".to_owned();
        let expected = "\n";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape carriage return.
        let mut input = "\'\\r\'".to_owned();
        let expected = "\r";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape tab.
        let mut input = "\'\\t\'".to_owned();
        let expected = "\t";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape null.
        let mut input = "\'\\0\'".to_owned();
        let expected = "\0";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape single quote.
        let mut input = "\'\\\'\'".to_owned();
        let expected = "\'";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);

        // Escape double quote.
        let mut input = "\'\\\"\'".to_owned();
        let expected = "\"";
        let actual = LexTokenIter::new(unsafe { input.as_bytes_mut() }, 1)
            .get_lit(Sym::SingleQuote)
            .expect("Unable to parse literal.");
        assert_eq!(expected, actual);
    }
}
