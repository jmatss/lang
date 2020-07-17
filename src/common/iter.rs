use crate::error::CustomError::GeneralError;
use crate::CustomResult;
use std::collections::LinkedList;

/// Max amount of items to be put back into the iterator.
const MAX_PUT_BACK: usize = 20;

pub struct TokenIter<I: Clone> {
    /// The buffer is used to push back items into the iterator.
    buff: LinkedList<I>,
    iter: Box<dyn Iterator<Item = I>>,
}

impl<I: Clone> TokenIter<I> {
    pub fn new<T>(items: T) -> Self
    where
        T: IntoIterator<Item = I> + 'static,
    {
        Self {
            buff: LinkedList::new(),
            iter: Box::new(items.into_iter()),
        }
    }

    /// Get the next item from the iterator.
    #[inline]
    pub fn next(&mut self) -> Option<I> {
        if !self.buff.is_empty() {
            Some(self.buff.pop_front()?)
        } else {
            self.iter.next()
        }
    }

    /// Puts back a character into the iterator.
    #[inline]
    pub fn put_back(&mut self, item: I) -> CustomResult<()> {
        if self.buff.len() < MAX_PUT_BACK {
            let _ = self.buff.push_front(item);
            Ok(())
        } else {
            Err(GeneralError("Push back buffer was full"))
        }
    }

    /// Skips the next `n` items in the iterator.
    #[inline]
    pub fn skip(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }

    /// Peeks at the item at the current position plus `n` of the iterator and
    /// returns it as a cloned item. A `n` of 0 returns the item that the
    /// iterator are currently pointing at.
    ///
    /// The `n` can not be greater that the max capacity of the push back buffer
    /// (+1) which is set to MAX_PUT_BACK.
    #[inline]
    pub fn peek_at_n(&mut self, n: usize) -> Option<I> {
        let mut items = self.peek_n(n + 1);

        if items.len() == n + 1 {
            Some(items.remove(n))
        } else {
            None
        }
    }

    /// Peeks at the next `n` items (0..n) ahead of the current position of the
    /// iterator and returns them as clones in a vector.
    ///
    /// The `n` can not be greater that the max capacity of the push back buffer
    /// which is set to MAX_PUT_BACK.
    #[inline]
    pub fn peek_n(&mut self, n: usize) -> Vec<I> {
        let mut items = Vec::with_capacity(n);

        for _ in 0..n {
            if let Some(item) = self.next() {
                items.push(item);
            } else {
                break;
            }
        }

        for item in items.iter().rev() {
            let _ = self.put_back(item.clone());
        }

        items
    }

    /// Peeks and clones the item at the current position of the iterator.
    #[inline]
    pub fn peek(&mut self) -> Option<I> {
        let mut peek_items = self.peek_n(1);

        if peek_items.len() == 1 {
            Some(peek_items.remove(0))
        } else {
            None
        }
    }

    /// Peeks and clones the two upcoming items in the iterator.
    #[inline]
    pub fn peek_two(&mut self) -> Option<(I, Option<I>)> {
        let mut peek_items = self.peek_n(2);
        let o1 = peek_items.pop();
        let o2 = peek_items.pop();

        if let (Some(first), Some(second)) = (&o2, &o1) {
            Some((first.clone(), Some(second.clone())))
        } else if let Some(first) = &o1 {
            Some((first.clone(), None))
        } else {
            None
        }
    }

    /// Peeks and clones the three upcoming items in the iterator.
    #[inline]
    pub fn peek_three(&mut self) -> Option<(I, Option<I>, Option<I>)> {
        let mut peek_items = self.peek_n(3);
        let o1 = peek_items.pop();
        let o2 = peek_items.pop();
        let o3 = peek_items.pop();

        if let (Some(first), Some(second), Some(third)) = (&o3, &o2, &o1) {
            Some((first.clone(), Some(second.clone()), Some(third.clone())))
        } else if let (Some(first), Some(second)) = (&o2, &o1) {
            Some((first.clone(), Some(second.clone()), None))
        } else if let Some(first) = o1 {
            Some((first, None, None))
        } else {
            None
        }
    }
}
