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
        let (b1, b2, b3, b4) = self.iter.peek_four()?;

        // Check if this is a one byte character.
        let b1 = if let Some(c) = std::char::from_u32(b1 as u32) {
            self.iter.skip(1);
            return Some(c);
        } else {
            b1 as u32
        };

        // Check if this is a two byte character.
        let b2 = if let Some(b2) = b2 {
            let n = (b1 << 8 | (b2 as u32)) as u32;
            if let Some(c) = std::char::from_u32(n) {
                self.iter.skip(2);
                return Some(c);
            } else {
                b2 as u32
            }
        } else {
            self.iter.skip(1);
            return None;
        };

        // Check if this is a three byte character.
        let b3 = if let Some(b3) = b3 {
            let n = (b1 << 16 | b2 << 8 | (b3 as u32)) as u32;
            if let Some(c) = std::char::from_u32(n) {
                self.iter.skip(3);
                return Some(c);
            } else {
                b3 as u32
            }
        } else {
            self.iter.skip(2);
            return None;
        };

        // Check if this is a four byte character.
        if let Some(b4) = b4 {
            let n = (b1 << 24 | b2 << 16 | b3 << 8 | (b4 as u32)) as u32;
            if let Some(c) = std::char::from_u32(n) {
                self.iter.skip(4);
                return Some(c);
            }
        }

        self.iter.skip(3);
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

    /// Peeks and clones the two upcoming items in the iterator.
    pub fn peek(&mut self) -> Option<char> {
        let old_pos = self.iter.pos;

        let res = self.next();

        self.iter.pos = old_pos;
        res
    }

    /// Peeks and clones the two upcoming items in the iterator.
    pub fn peek_two(&mut self) -> Option<(char, Option<char>)> {
        let old_pos = self.iter.pos;

        let res = if let Some(c1) = self.next() {
            Some((c1, self.next()))
        } else {
            None
        };

        self.iter.pos = old_pos;
        res
    }

    /// Peeks and clones the three upcoming items in the iterator.
    pub fn peek_three(&mut self) -> Option<(char, Option<char>, Option<char>)> {
        let old_pos = self.iter.pos;

        let res = if let Some(c1) = self.next() {
            Some((c1, self.next(), self.next()))
        } else {
            None
        };

        self.iter.pos = old_pos;
        res
    }

    /// Skips the next `n` characters.
    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.iter.next();
        }
    }

    pub fn mark(&mut self) -> usize {
        self.iter.mark()
    }

    /// Rewinds the iterator to the previous character.
    pub fn rewind(&mut self) -> bool {
        if self.is_valid_char_of_size(4) {
            self.iter.rewind_n(4)
        } else if self.is_valid_char_of_size(3) {
            self.iter.rewind_n(3)
        } else if self.is_valid_char_of_size(2) {
            self.iter.rewind_n(2)
        } else if self.is_valid_char_of_size(1) {
            self.iter.rewind_n(1)
        } else {
            false
        }
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

    pub fn rewind_to_mark(&mut self, mark: usize) {
        self.iter.rewind_to_mark(mark);
    }

    /// Checks if the previos character is a valid char of byte length `n`.
    fn is_valid_char_of_size(&mut self, n: usize) -> bool {
        let mut is_valid = false;
        let old_pos = self.iter.pos;

        if self.iter.rewind_n(n) {
            if let Some(c) = self.next() {
                if c.len_utf8() == n {
                    is_valid = true;
                }
            }
        }

        self.iter.pos = old_pos;
        is_valid
    }
}
