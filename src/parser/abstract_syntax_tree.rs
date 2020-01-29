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
    // current_parent == "current_block"
    pub current_parent: Index,
}

impl AST {
    pub fn new() -> Self {
        // Index of root_block's parent is set to max_value, should never be used.
        // The indent_level is set to 0 and line_number to 0, should never be used.
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
        AST { blocks: vec![Rc::clone(&root_block)], current_parent: index }
    }

    pub fn insert_block(&mut self, token: Token, line_number: usize, indent_level: usize) -> CustomResult<()> {
        let index = self.blocks.len();
        let parent_index = self.current_parent;
        let parent = self.blocks[parent_index].borrow();

        // Special case if this is the first "real" block added.
        if let Token::BlockHeader(BlockHeader::Default) = parent.token {
            if indent_level != 0 {
                return Err(ParseError(
                    format!(
                        "Incorrect indent_level when inserting new block into Default. Expected: {}, got: {}",
                        0,
                        indent_level
                    )
                ));
            }
        } else if indent_level != parent.indent_level + 1 {
            return Err(ParseError(
                format!(
                    "Incorrect indent_level when inserting new block. Expected: {}, got: {}",
                    parent.indent_level + 1,
                    indent_level
                )
            ));
        }

        // Need to drop parent to allow for a push into self.blocks.
        std::mem::drop(parent);

        let block = Rc::new(
            RefCell::new(
                ASTToken::new_block(token, parent_index, line_number, indent_level, index)
            )
        );
        self.blocks.push(Rc::clone(&block));
        self.current_parent = index;

        self.blocks[parent_index].borrow_mut().add_child(block)?;
        Ok(())
    }

    pub fn insert_token(&mut self, token: Token, line_number: usize, indent_level: usize) -> CustomResult<()> {
        let old_parent = self.current_parent;
        let new_parent = self.find_correct_parent(indent_level, old_parent);

        if new_parent != old_parent {
            self.current_parent = new_parent;
        }

        let token = Rc::new(
            RefCell::new(
                ASTToken::new(token, new_parent, line_number, indent_level)
            )
        );

        self.blocks[new_parent].borrow_mut().add_child(token)?;
        Ok(())
    }

    // FIXME: No error checking on out of bounds etc.
    // Used to find the parent that this "token" should be put into.
    // If it is a "new indent level", traverse the parents upwards until
    // the correct indent level is found.
    fn find_correct_parent(&self, indent_level: usize, mut parent_index: Index) -> Index {
        let parent = self.blocks[parent_index].borrow();

        // Special case if there are no "real" parents.
        if let Token::BlockHeader(BlockHeader::Default) = parent.token {
            parent_index = 0;
        } else {
            let current_parent_indent_level = parent.indent_level;
            let new_parent_indent_level = indent_level - 1;
            // "steps" represents how many parents needs to be traverse to reach the new parent.
            let steps = current_parent_indent_level - new_parent_indent_level;

            for _ in 0..steps {
                parent_index = self.blocks[parent_index].borrow().parent;
            }
        }

        parent_index
    }

    pub fn debug_print(&self) {
        AST::debug_print_priv(&self.blocks[0]);
    }

    fn debug_print_priv(rc_token: &RCToken) {
        let token = rc_token.borrow();
        println!(
            "{}| {}{:?}",
            token.line_number,
            ">".repeat(token.indent_level),
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
            .ok_or_else(|| ParseError(
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