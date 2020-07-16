use super::token::BlockHeader;
use crate::error::CustomError::ParseError;
use crate::parse::token::ParseToken;
use crate::CustomResult;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

/// Number/index of current block scope. Every block will get a unique number
/// that can be used to refer to that specific block.
pub type BlockId = usize;

pub type RCBlock = Rc<RefCell<ASTBlock>>;
pub type RCToken = Rc<RefCell<ASTToken>>;

pub enum RCNode {
    Block(RCBlock),
    Token(RCToken),
}

// Indices (BlockID) corresponding to the indices of the blocks structure are
// used to refer to the blocks, and can be used to represent a tree.
// Only blocks/blockHeaders are stored in the "blocks".
// The other types will be stored inside those ASTBlock's as children.
pub struct AST {
    pub blocks: Vec<RCBlock>,
    pub current_block_id: BlockId,
}

impl AST {
    pub fn new() -> Self {
        // Index of root_block's parent is set to max_value, should never be used.
        // The indent_level is set to 0 and line_number to 0, should never be used.
        let token = ParseToken::Block(BlockHeader::Default, None);
        let index = 0;
        let parent_index = usize::max_value();
        let line_number = 0;
        let indent_level = 0;

        let root_block = ASTBlock::new_rc(token, index, parent_index, line_number, indent_level);
        AST {
            blocks: vec![root_block],
            current_parent_index: index,
        }
    }

    pub fn insert_block(
        &mut self,
        token: ParseToken,
        line_number: usize,
        indent_level: usize,
    ) -> CustomResult<()> {
        let new_parent_index = self.find_correct_parent(indent_level, self.current_parent_index);
        let old_parent: &RefCell<ASTBlock> = self.blocks[self.current_parent_index].borrow();
        let old_parent = old_parent.borrow_mut();
        let current_index = self.blocks.len();

        // Special case if this is the first "real" block added.
        if let ParseToken::Block(Block::Default) = old_parent.token {
            if indent_level != 0 {
                return Err(ParseError(format!(
                    "{}. Expected: {}, got: {}",
                    "Incorrect indent_level when inserting new block into Default", 0, indent_level
                )));
            }
        // Can't indent two new indents at once.
        } else if indent_level > old_parent.indent_level + 1 {
            return Err(ParseError(format!(
                "Incorrect indent_level when inserting new block. Expected: {}, got: {}",
                old_parent.indent_level + 1,
                indent_level
            )));
        }

        // Need to drop old_parent since it is borrowing self.blocks as immutable,
        // need to borrow it as mutable underneath.
        std::mem::drop(old_parent);

        let ast_block = ASTBlock::new_rc(
            token,
            current_index,
            new_parent_index,
            line_number,
            indent_level,
        );
        self.blocks.push(Rc::clone(&ast_block));

        println!(
            "New block with ScopeIndex: {}, inserted into ScopeIndex: {}",
            current_index, new_parent_index
        );

        let ast_block_clone = Rc::clone(&ast_block);
        // Insert this block into the parent and set this block as the new parent.
        self.blocks[new_parent_index]
            .borrow_mut()
            .add_child(RCNode::Block(ast_block_clone))?;
        self.current_parent_index = current_index;
        Ok(())
    }

    pub fn insert_token(
        &mut self,
        token: ParseToken,
        line_number: usize,
        indent_level: usize,
    ) -> CustomResult<()> {
        let new_parent_index = self.find_correct_parent(indent_level, self.current_parent_index);
        self.current_parent_index = new_parent_index;

        println!("Inserting into parent ScopeIndex: {}", new_parent_index);

        let ast_token = ASTToken::new_rc(token, new_parent_index, line_number, indent_level);
        let ast_token_clone = Rc::clone(&ast_token);
        self.blocks[new_parent_index]
            .borrow_mut()
            .add_child(RCNode::Token(ast_token_clone))?;
        Ok(())
    }

    // FIXME: No error checking on out of bounds etc.
    // Used to find the parent that this "token" should be put into.
    // If it is a "new indent level", traverse the parents upwards until
    // the correct indent level is found.
    fn find_correct_parent(&self, indent_level: usize, mut parent_index: ScopeIndex) -> ScopeIndex {
        let parent: &RefCell<ASTBlock> = self.blocks[parent_index].borrow();
        let parent = parent.borrow();

        // Special case if it is to be inserted into the "root" block.
        if indent_level == 0 {
            parent_index = 0;
        } else {
            let current_parent_indent_level = parent.indent_level;
            println!(
                "cur: {}, indent_level: {}",
                current_parent_indent_level, indent_level
            );
            let new_parent_indent_level = indent_level - 1;
            // "steps" represents how many parents needs to be traverse to reach the new parent.
            let steps = current_parent_indent_level - new_parent_indent_level;

            for _ in 0..steps {
                let tmp_parent: &RefCell<ASTBlock> = self.blocks[parent_index].borrow();
                parent_index = tmp_parent.borrow().index;
            }
        }

        parent_index
    }

    pub fn debug_print(&self) {
        AST::debug_print_priv(&RCNode::Block(Rc::clone(&self.blocks[0])));
    }

    fn debug_print_priv(ast_block: &RCNode) {
        match ast_block {
            RCNode::Block(rc_block) => {
                let ast_block: &RefCell<ASTBlock> = rc_block.borrow();
                let ast_block = ast_block.borrow();
                println!(
                    "{}| {}{:?}",
                    ast_block.line_number,
                    ">".repeat(ast_block.indent_level),
                    ast_block.token
                );

                for rc_token in &ast_block.children {
                    AST::debug_print_priv(rc_token);
                }
            }
            RCNode::Token(rc_token) => {
                let ast_token: &RefCell<ASTToken> = rc_token.borrow();
                let ast_block = ast_token.borrow();
                println!(
                    "{}| {}{:?}",
                    ast_block.line_number,
                    ">".repeat(ast_block.indent_level),
                    ast_block.token
                );
            }
        }
    }
}

// Only for block headers.
// Needs its own Index so it can be used as parent,
// and needs to store its children.
pub struct ASTBlock {
    pub token: ParseToken,
    pub index: ScopeIndex,

    pub parent_index: ScopeIndex,
    pub children: Vec<RCNode>,

    pub line_number: usize,
    pub indent_level: usize,
}

impl ASTBlock {
    pub fn new(
        token: ParseToken,
        index: ScopeIndex,
        parent_index: ScopeIndex,
        line_number: usize,
        indent_level: usize,
    ) -> Self {
        Self {
            token,
            index,
            parent_index,
            children: Vec::new(),
            line_number,
            indent_level,
        }
    }

    pub fn new_rc(
        token: ParseToken,
        index: ScopeIndex,
        parent_index: ScopeIndex,
        line_number: usize,
        indent_level: usize,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(ASTBlock::new(
            token,
            index,
            parent_index,
            line_number,
            indent_level,
        )))
    }

    pub fn add_child(&mut self, child: RCNode) -> CustomResult<()> {
        self.children.push(child);
        Ok(())
    }
}

// Can add extra information in here later if needed.
#[derive(Debug)]
pub struct ASTToken {
    // "token" will contain the BlockHeader if this is a "root".
    pub token: ParseToken,
    pub parent_index: ScopeIndex,

    pub line_number: usize,
    pub indent_level: usize,
}

impl ASTToken {
    pub fn new(
        token: ParseToken,
        parent_index: ScopeIndex,
        line_number: usize,
        indent_level: usize,
    ) -> Self {
        ASTToken {
            token,
            parent_index,
            line_number,
            indent_level,
        }
    }

    pub fn new_rc(
        token: ParseToken,
        parent_index: ScopeIndex,
        line_number: usize,
        indent_level: usize,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(ASTToken::new(
            token,
            parent_index,
            line_number,
            indent_level,
        )))
    }
}
