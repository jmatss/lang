pub struct TokenIter<I: Clone> {
    pos: usize,
    iter: Vec<I>,
}

impl<I: Clone> TokenIter<I> {
    pub fn new(items: Vec<I>) -> Self {
        Self {
            pos: 0,
            iter: items,
        }
    }

    /// Get the next item from the iterator.
    #[inline]
    pub fn next(&mut self) -> Option<I> {
        let item = self.iter.get(self.pos);
        self.pos += 1;
        item.cloned()
    }

    /// Rewinds back one character in the iterator.
    /// If the returned bool is false, this operation tried to rewind to a
    /// position before the actual iterator (pos < 0).
    #[inline]
    pub fn rewind(&mut self) -> bool {
        if self.pos > 0 {
            self.pos -= 1;
            true
        } else {
            false
        }
    }

    /// Puts back a character into the iterator.
    /// If the returned bool is false, this operation tried to rewind to a
    /// position before the actual iterator (pos < 0).
    #[inline]
    pub fn rewind_n(&mut self, n: usize) -> bool {
        if n <= self.pos {
            self.pos -= n;
            true
        } else {
            self.pos = 0;
            false
        }
    }

    /// Skips the next `n` items in the iterator.
    #[inline]
    pub fn skip(&mut self, n: usize) {
        self.pos += n;
    }

    /// Peeks and clones the item at the current position of the iterator.
    #[inline]
    pub fn peek(&mut self) -> Option<I> {
        self.peek_at_n(0)
    }

    /// Peeks at the item at the current position plus `n` of the iterator and
    /// returns it as a cloned item. A `n` of 0 returns the item that the
    /// iterator are currently pointing at.
    #[inline]
    pub fn peek_at_n(&mut self, n: usize) -> Option<I> {
        self.iter.get(self.pos + n).cloned()
    }

    /// Peeks and clones the two upcoming items in the iterator.
    #[inline]
    pub fn peek_two(&mut self) -> Option<(I, Option<I>)> {
        if let Some(first) = self.peek_at_n(0) {
            Some((first, self.peek_at_n(1)))
        } else {
            None
        }
    }

    /// Peeks and clones the three upcoming items in the iterator.
    #[inline]
    pub fn peek_three(&mut self) -> Option<(I, Option<I>, Option<I>)> {
        if let Some(first) = self.peek_at_n(0) {
            Some((first, self.peek_at_n(1), self.peek_at_n(2)))
        } else {
            None
        }
    }
}
