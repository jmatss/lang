use crate::parser::token::Block;

pub struct ActionTree {
    tree: Vec<Block>,
}

impl ActionTree {
    pub fn new() -> Self {
        ActionTree { tree: Vec::new() }
    }
}

pub struct Node {}