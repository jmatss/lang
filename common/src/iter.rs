// TODO: Is it possible to not have the whole `self.iter` as mutable and just
//       temporarily make the iter mutable in the `replace()` function? It is
//       the only function that needs to modify the iter.

pub struct TokenIter<'a, I: Clone> {
    pub pos: usize,
    iter: &'a mut [I],
}

impl<'a, I: Clone> TokenIter<'a, I> {
    pub fn new(items: &'a mut [I]) -> Self {
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

    /// Get the next two items from the iterator.
    #[inline]
    pub fn next_two(&mut self) -> Option<(I, Option<I>)> {
        if let Some(first) = self.next() {
            Some((first, self.next()))
        } else {
            None
        }
    }

    /// Rewinds back one item in the iterator.
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

    /// Puts back a iter into the iterator.
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

    /// Peeks and clones the four upcoming items in the iterator.
    #[inline]
    pub fn peek_four(&mut self) -> Option<(I, Option<I>, Option<I>, Option<I>)> {
        if let Some(first) = self.peek_at_n(0) {
            Some((
                first,
                self.peek_at_n(1),
                self.peek_at_n(2),
                self.peek_at_n(3),
            ))
        } else {
            None
        }
    }

    /// Replaces the item at the current position with the value of `item`.
    /// Returns the old token that was replaced.
    pub fn replace(&mut self, item: I) -> I {
        let old = self.iter[self.pos].clone();
        self.iter[self.pos] = item;
        old
    }
}
