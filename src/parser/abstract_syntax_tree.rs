use crate::parser::token::{Token, BlockHeader};
use crate::CustomResult;
use crate::error::CustomError::ParseError;
use std::rc::Rc;
use std::cell::RefCell;

type Index = usize;
type RCToken = Rc<RefCell<ASTToken>>;

// Indices ("Index" type) corresponding to the indices of the blocks structure are used
// to refer to the blocks, and can be used to represent a tree.
// Only blocks/blockHeaders are stored in the "blocks".
// The other types will be stored inside those ASTToken's as children.
#[derive(Debug)]
pub struct AST {
    pub blocks: Vec<RCToken>,
    // current_block == current_parent
    pub current_block: Index,
}

impl AST {
    pub fn new() -> Self {
        // Index of root_block's parent is set to max_value, should never be used.
        let token = Token::BlockHeader(BlockHeader::Default);
        let parent = usize::max_value();
        let line_number = 0;
        let indent_level = 0;
        let index = 0;

        let root_block = Rc::new(
            RefCell::new(
                ASTToken::new_block(token, parent, line_number, indent_level, index)
            )
        );
        AST { blocks: vec![Rc::clone(&root_block)], current_block: index }
    }

    pub fn insert_block(&mut self, token: Token, line_number: usize, indent_level: usize) -> CustomResult<()> {
        let parent = self.current_block;
        let index = self.blocks.len();

        // Ensure that this block is one indent_level above its parent.
        if !self.is_new_indent_level(indent_level, parent) {
            return Err(ParseError(
                format!(
                    "Incorrect indent_level when inserting new block. Expected: {}, got: {}",
                    self.blocks[parent].borrow_mut().indent_level + 1,
                    indent_level
                )
            ));
        }

        let block = Rc::new(
            RefCell::new(
                ASTToken::new_block(token, parent, line_number, indent_level, index)
            )
        );
        self.blocks.push(Rc::clone(&block));
        self.current_block = index;

        self.blocks[parent].borrow_mut().add_child(block)?;
        Ok(())
    }

    pub fn insert_token(&mut self, token: Token, line_number: usize, indent_level: usize) -> CustomResult<()> {
        // "parent" might be equals to "old_parent".
        let old_parent = self.current_block;
        let parent = self.find_correct_parent(indent_level, old_parent);

        let token = Rc::new(
            RefCell::new(
                ASTToken::new(token, parent, line_number, indent_level)
            )
        );

        self.blocks[parent].borrow_mut().add_child(token)?;
        Ok(())
    }

    fn is_new_indent_level(&self, indent_level: usize, parent_index: Index) -> bool {
        let parent_indent_level = self.blocks[parent_index].borrow_mut().indent_level;
        (indent_level == parent_indent_level + 1)
    }

    // FIXME: No error checking on out of bounds etc.
    // Used to find the parent that this "token" should be put into.
    // If it is a "new indent level", traverse the parents upwards until
    // the correct indent level is found.
    fn find_correct_parent(&self, indent_level: usize, mut parent: Index) -> Index {
        if self.is_new_indent_level(indent_level, parent) {
            let current_parent_indent_level = self.blocks[parent].borrow_mut().indent_level;
            let new_parent_indent_level = indent_level - 1;
            // "steps" represents how many parents needs to be traverse to reach the new parent.
            let steps = current_parent_indent_level - new_parent_indent_level;

            for _ in 0..steps {
                parent = self.blocks[parent].borrow_mut().parent;
            }
        }

        parent
    }

    pub fn debug_print(&self) {
        for rc_token in &self.blocks {
            AST::debug_print_priv(rc_token);
        }
    }

    fn debug_print_priv(rc_token: &RCToken) {
        let token = rc_token.borrow();
        println!(
            "{}{:?}",
            " ".repeat(token.indent_level),
            token.token
        );

        if let Some(block) = &token.block_data {
            for rc_token in &block.children {
                AST::debug_print_priv(rc_token);
            }
        }
    }
}

// Can add extra information in here later if needed.
#[derive(Debug)]
pub struct ASTToken {
    pub token: Token,
    pub parent: Index,

    pub line_number: usize,
    pub indent_level: usize,

    pub block_data: Option<ASTBlock>,
}

impl ASTToken {
    pub fn new(token: Token, parent: Index, line_number: usize, indent_level: usize) -> Self {
        ASTToken { token, parent, line_number, indent_level, block_data: None }
    }

    pub fn new_block(
        token: Token,
        parent: Index,
        line_number: usize,
        indent_level: usize,
        index: usize,
    ) -> Self {
        let block_data = ASTBlock { index, children: Vec::new() };
        ASTToken { token, parent, line_number, indent_level, block_data: Some(block_data) }
    }

    pub fn add_child(&mut self, child: RCToken) -> CustomResult<()> {
        let block = self.block_data.as_mut()
            .ok_or(ParseError(
                "Unable to add child to block (probably because this isn't a block)".to_string()
            ))?;

        block.children.push(child);

        Ok(())
    }
}

// Only for block headers.
// Needs its own Index so it can be used as parent,
// and needs to store its children.
#[derive(Debug)]
pub struct ASTBlock {
    pub index: Index,
    pub children: Vec<RCToken>,
}