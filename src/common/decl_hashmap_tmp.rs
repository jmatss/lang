use crate::parse::token::BlockId;
use std::collections::HashMap;

// TODO: Temporary hashmap that's ignoring blocks/scopes. Will be removed ones
//       they get implemented.

/// Used during parsing/code gen to keep track of all declarations of items.
#[derive(Debug, Clone, PartialEq)]
pub struct DeclHashMap<I> {
    /// The key of the outer map is the identifier/name for a specific declaration
    /// and the value is some generic `I`.
    outer: HashMap<String, I>,
}

impl<I> DeclHashMap<I> {
    pub fn new() -> Self {
        Self {
            outer: HashMap::new(),
        }
    }

    // TODO: `id` will be used when logic for scopes are implemented.
    #[allow(unused_variables)]
    pub fn insert(&mut self, ident: &str, id: BlockId, item: I) {
        self.outer.insert(ident.into(), item);
    }

    // TODO: `id` will be used when logic for scopes are implemented.
    #[allow(unused_variables)]
    pub fn get(&self, ident: &str, id: BlockId) -> Option<&I> {
        self.outer.get(ident)
    }

    // TODO: `id` will be used when logic for scopes are implemented.
    #[allow(unused_variables)]
    pub fn get_mut(&mut self, ident: &str, id: BlockId) -> Option<&mut I> {
        self.outer.get_mut(ident)
    }
}
