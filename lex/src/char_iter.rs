use common::iter::TokenIter;

/// Wrapper around a `TokenIter<u8>` which allows one to work with chars without
/// having copy the u8 array into chars before use. This iter lets one work
/// directly with the u8 array.
pub struct CharIter<'a> {
    iter: TokenIter<'a, u8>,
}

impl<'a> CharIter<'a> {
    pub fn new(content: &'a mut [u8]) -> Self {
        Self {
            iter: TokenIter::new(content),
        }
    }

    /// Gets the next char from the iterator.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<char> {
        let mut n = 0;
        let byte_size = 4;

        // UTF-8 characters can contain 4 bytes. This loop starts by looking at
        // a single byte and for every iteration increases the amount of bytes
        // that it tries to convert to a character.
        for i in 0..4 {
            if let Some(b) = self.iter.peek_at_n(i) {
                n <<= byte_size;
                n |= b as u32;

                if let Some(c) = std::char::from_u32(n) {
                    self.iter.skip(i + 1);
                    return Some(c);
                }
            } else {
                break;
            }
        }

        None
    }

    /// Get the next two characters from the iterator.
    pub fn next_two(&mut self) -> Option<(char, Option<char>)> {
        if let Some(first) = self.next() {
            Some((first, self.next()))
        } else {
            None
        }
    }

    /// Peeks and clones the next upcoming items in the iterator.
    pub fn peek(&mut self) -> Option<char> {
        let mark = self.iter.mark();
        if let Some(res) = self.next() {
            self.iter.rewind_to_mark(mark);
            Some(res)
        } else {
            None
        }
    }

    /// Peeks and clones the two upcoming items in the iterator.
    pub fn peek_two(&mut self) -> Option<(char, Option<char>)> {
        let mark = self.iter.mark();
        if let Some(c1) = self.next() {
            let c2 = self.next();
            self.iter.rewind_to_mark(mark);
            Some((c1, c2))
        } else {
            None
        }
    }

    /// Peeks and clones the three upcoming items in the iterator.
    pub fn peek_three(&mut self) -> Option<(char, Option<char>, Option<char>)> {
        let mark = self.iter.mark();
        if let Some(c1) = self.next() {
            let c2 = self.next();
            let c3 = self.next();
            self.iter.rewind_to_mark(mark);
            Some((c1, c2, c3))
        } else {
            None
        }
    }

    /// Skips the next `n` characters.
    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.iter.next();
        }
    }

    /// Rewinds the iterator to the previous character. This function can be used
    /// for an arbitrary amount of rewinds. If just a single character needs to
    /// be rewinded after a `next()` call, use `rewind_prev()`.
    pub fn rewind(&mut self) -> bool {
        for i in (1..=4).rev() {
            if self.is_valid_char_of_size(i) {
                return self.iter.rewind_n(i);
            }
        }
        false
    }

    /// Puts back a item into the iterator.
    /// If the returned bool is false, this operation tried to rewind to a
    /// position before the actual iterator (pos < 0).
    pub fn rewind_n(&mut self, n: usize) -> bool {
        for _ in 0..n {
            if !self.rewind() {
                return false;
            }
        }
        true
    }

    /// Checks if the previos character is a valid char of byte length `n`.
    fn is_valid_char_of_size(&mut self, n: usize) -> bool {
        let mut is_valid = false;
        let mark = self.iter.mark();

        if self.iter.rewind_n(n) {
            if let Some(c) = self.next() {
                if c.len_utf8() == n {
                    is_valid = true;
                }
            }
        }

        self.iter.rewind_to_mark(mark);
        is_valid
    }
}
