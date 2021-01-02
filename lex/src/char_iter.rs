use common::iter::TokenIter;

use crate::validations::utf8_char_width;

/// Wrapper around a `TokenIter<u8>` which allows one to work with chars without
/// having copy the u8 array into chars before use. This iter lets one work
/// directly with the u8 array.
pub(crate) struct CharIter<'a> {
    iter: TokenIter<'a, u8>,

    /// Temporary buffer used when looking at UTF-8 encoded characters. This
    /// buffer will be used to store possible characters when trying to figure
    /// out the correct "size" for the characters.
    buf: [u8; 4],
}

impl<'a> CharIter<'a> {
    pub(crate) fn new(content: &'a mut [u8]) -> Self {
        Self {
            iter: TokenIter::new(content),
            buf: [0; 4],
        }
    }

    /// Gets the next char from the iterator.
    #[allow(clippy::should_implement_trait)]
    pub(crate) fn next(&mut self) -> Option<char> {
        let width = utf8_char_width(self.iter.peek()?);
        if width == 0 {
            return None;
        } else if width > 4 {
            unreachable!("`width` can't be greater than 4 (max size of UTF-8 char)");
        }

        // Populate `self.buf` with the next `width` bytes from the iterator.
        // This will be the bytes representing the next UTF-8 character.
        for i in 0..width {
            self.buf[i] = self.iter.next()?;
        }

        let utf_8_slice = &self.buf[0..width];
        std::str::from_utf8(utf_8_slice)
            .ok()
            .map(|s| s.chars().next())
            .flatten()
    }

    /// Peeks and clones the next upcoming items in the iterator.
    pub(crate) fn peek(&mut self) -> Option<char> {
        let mark = self.iter.mark();
        if let Some(res) = self.next() {
            self.iter.rewind_to_mark(mark);
            Some(res)
        } else {
            None
        }
    }

    /// Peeks and clones the two upcoming items in the iterator.
    pub(crate) fn peek_two(&mut self) -> Option<(char, Option<char>)> {
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
    pub(crate) fn peek_three(&mut self) -> Option<(char, Option<char>, Option<char>)> {
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
    pub(crate) fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    /// Rewinds the iterator to the previous character. This function can be used
    /// for an arbitrary amount of rewinds.
    pub(crate) fn rewind(&mut self) -> bool {
        for i in (1..=4).rev() {
            if self.is_valid_char_of_size(i) {
                return self.iter.rewind_n(i);
            }
        }
        false
    }

    /// Checks if the previos character is a valid char of byte length `n`.
    fn is_valid_char_of_size(&mut self, n: usize) -> bool {
        let mut is_valid = false;
        let mark = self.iter.mark();

        if self.iter.rewind_n(n) {
            if let Some(first_b) = self.iter.peek() {
                is_valid = utf8_char_width(first_b) == n;
            }
        }

        self.iter.rewind_to_mark(mark);
        is_valid
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_char_for_valid_chars() {
        // ARRANGE
        let chars = [('a', 1), ('å', 2), ('ঀ', 3), ('𐊀', 4)];
        let mut buf = [0; 4];

        for (expected_char, expected_size) in chars.iter() {
            expected_char.encode_utf8(&mut buf);

            // ACT
            let actual_char = CharIter::new(&mut buf).next().unwrap();

            // ASSERT
            assert_eq!(*expected_char, actual_char);
            assert_eq!(*expected_size, actual_char.len_utf8());
        }
    }

    #[test]
    fn test_next_char_for_invalid_chars() {
        let mut one_byte = [0x80];
        let mut two_byte = [0xe0, 0xaa];
        let mut three_byte = [0xf0, 0xcc, 0xcc];
        let mut four_byte = [0xf5, 0xff, 0xff, 0xff];

        assert!(CharIter::new(&mut one_byte).next().is_none());
        assert!(CharIter::new(&mut two_byte).next().is_none());
        assert!(CharIter::new(&mut three_byte).next().is_none());
        assert!(CharIter::new(&mut four_byte).next().is_none());
    }

    #[test]
    fn test_next_char_for_multiple_chars() {
        // ARRANGE
        let expected_first = Some('a');
        let expected_second = Some('b');
        let mut chars = "ab".to_string();

        // ACT
        let mut iter = CharIter::new(unsafe { chars.as_bytes_mut() });
        let actual_first = iter.next();
        let actual_second = iter.next();

        // ASSERT
        assert_eq!(expected_first, actual_first);
        assert_eq!(expected_second, actual_second);
    }

    #[test]
    fn test_peek_does_not_swallow_char() {
        // ARRANGE
        let expected = Some('a');
        let mut chars = "a".to_string();

        // ACT
        let mut iter = CharIter::new(unsafe { chars.as_bytes_mut() });
        let actual_peek = iter.peek();
        let actual_next = iter.next();

        // ASSERT
        assert_eq!(expected, actual_peek);
        assert_eq!(expected, actual_next);
    }

    #[test]
    fn test_peek_two_for_valid_chars() {
        // ARRANGE
        let expected = Some(('a', Some('b')));
        let mut chars = "ab".to_string();

        // ACT
        let mut iter = CharIter::new(unsafe { chars.as_bytes_mut() });
        let actual = iter.peek_two();

        // ASSERT
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_peek_three_for_valid_chars() {
        // ARRANGE
        let expected = Some(('a', Some('b'), Some('c')));
        let mut chars = "abc".to_string();

        // ACT
        let mut iter = CharIter::new(unsafe { chars.as_bytes_mut() });
        let actual = iter.peek_three();

        // ASSERT
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_skip_for_valid_chars() {
        // ARRANGE
        let values = [(Some('b'), 1), (Some('c'), 2)];
        let mut chars = "abc".to_string();

        for (expected, skip_n) in values.iter() {
            let mut iter = CharIter::new(unsafe { chars.as_bytes_mut() });

            // ACT
            iter.skip(*skip_n);
            let actual = iter.next();

            // ASSERT
            assert_eq!(expected, &actual);
        }
    }

    #[test]
    fn test_rewind_for_valid_chars() {
        // ARRANGE
        let chars = ['a', 'å', 'ঀ', '𐊀'];
        let mut buf = [0; 4];

        for expected_char in chars.iter() {
            expected_char.encode_utf8(&mut buf);

            let mut iter = CharIter::new(&mut buf);
            iter.next();

            // ACT
            let rewind_success = iter.rewind();
            let actual_char = iter.next().unwrap();

            // ASSERT
            assert!(rewind_success);
            assert_eq!(*expected_char, actual_char);
        }
    }

    #[test]
    fn test_is_valid_char_of_size_for_valid_chars() {
        // ARRANGE
        let chars = [('a', 1), ('å', 2), ('ঀ', 3), ('𐊀', 4)];
        let mut buf = [0; 4];

        for (expected_char, expected_size) in chars.iter() {
            expected_char.encode_utf8(&mut buf);

            let mut iter = CharIter::new(&mut buf);
            iter.next();

            // ACT
            let is_valid = iter.is_valid_char_of_size(*expected_size);

            // ASSERT
            assert!(is_valid);
        }
    }

    #[test]
    fn test_is_valid_char_of_size_for_invalid_size() {
        // ARRANGE
        let chars = [('a', 1), ('å', 2), ('ঀ', 3), ('𐊀', 4)];
        let mut buf = [0; 4];

        for (expected_char, expected_size) in chars.iter() {
            expected_char.encode_utf8(&mut buf);

            let mut iter = CharIter::new(&mut buf);
            iter.next();

            // ACT
            // Increment the size sent to `is_valid_char_of_size()` which should
            // make the function call fail
            let is_valid = iter.is_valid_char_of_size(*expected_size + 1);

            // ASSERT
            assert!(!is_valid);
        }
    }
}
