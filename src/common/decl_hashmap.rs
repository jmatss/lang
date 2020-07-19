use crate::parse::token::BlockId;
use std::collections::HashMap;

/// Used during parsing/code gen to keep track of all declarations of items.
#[derive(Debug, Clone, PartialEq)]
pub struct DeclHashMap<I> {
    /// The key of the outer map is the identifier/name for a specific declaration
    /// and the value is a hashmap of all declarations with that name.
    ///
    /// The key of the inner map is the BlockId in which this declaration is
    /// declared and the value is some generic `I`.
    outer: HashMap<String, HashMap<BlockId, I>>,
}

impl<I> DeclHashMap<I> {
    pub fn new() -> Self {
        Self {
            outer: HashMap::new(),
        }
    }

    pub fn insert(&mut self, ident: &str, id: BlockId, item: I) {
        let mut inner = if let Some(inner) = self.get_inner_mut(ident) {
            inner
        } else {
            // Need to create the "inner" map if it doesn't exists already.
            let mut inner = HashMap::new();
            self.outer.insert(ident.into(), inner);
            &mut inner
        };
        inner.insert(id, item);
    }

    pub fn get(&self, ident: &str, id: BlockId) -> Option<&I> {
        if let Some(inner) = self.get_inner(ident) {
            inner.get(&id)
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, ident: &str, id: BlockId) -> Option<&mut I> {
        if let Some(inner) = self.get_inner_mut(ident) {
            inner.get_mut(&id)
        } else {
            None
        }
    }

    pub fn get_inner(&self, ident: &str) -> Option<&HashMap<BlockId, I>> {
        self.outer.get(ident)
    }

    pub fn get_inner_mut(&mut self, ident: &str) -> Option<&mut HashMap<BlockId, I>> {
        self.outer.get_mut(ident)
    }
}
