// TODO: Is it possible to not have the whole `self.iter` as mutable and just
//       temporarily make the iter mutable in the `replace()` function? It is
//       the only function that needs to modify the iter.

pub struct TokenIter<'a, I: Clone> {
    pos: usize,
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
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<I> {
        if let Some(item) = self.iter.get(self.pos) {
            self.pos += 1;
            Some(item.clone())
        } else {
            None
        }
    }

    /// Returns the current position. This can be used to rewind the iterator
    /// to the given position at a later stage.
    #[inline]
    pub fn pos(&mut self) -> usize {
        self.pos
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

    /// Rewinds back the iterator to the given `pos`.
    #[inline]
    pub fn rewind_to_pos(&mut self, pos: usize) -> bool {
        if pos <= self.iter.len() {
            self.pos = pos;
            true
        } else {
            false
        }
    }

    /// Rewinds the iterator `n` steps.
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
    pub fn skip(&mut self, n: usize) -> bool {
        if self.pos + n <= self.iter.len() {
            self.pos += n;
            true
        } else {
            false
        }
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
        self.peek_at_n(0).map(|first| (first, self.peek_at_n(1)))
    }

    pub fn get_items(&self) -> &[I] {
        self.iter
    }

    /// Replaces the item at the current position with the value of `item`.
    /// Returns the old token that was replaced.
    pub fn replace(&mut self, item: I) -> Option<I> {
        if let Some(old) = self.iter.get(self.pos).cloned() {
            self.iter[self.pos] = item;
            Some(old)
        } else {
            None
        }
    }
}
