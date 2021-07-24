use std::collections::HashMap;

use crate::{
    basic_block::BasicBlock,
    error::{IrError, IrResult},
    LocalVarIdx,
};

use super::ty::Type;

#[derive(Debug)]
pub enum FuncVisibility {
    /// Will be added as an export in the wasm module. A function with this
    /// visibility needs to implement a function body.
    Export,

    /// Will be imported into the wasm module. A function with this visibility
    /// should not implement a function body.
    Import,

    /// A function that won't be exported from the wasm module. A function with
    /// this visibility needs to implement a function body.
    None,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub visibility: FuncVisibility,
    pub ret_ty: Type,

    /// The parameters of the function will be accessed with their indices, so
    /// there is no need to store their names here. The same is true for the
    /// `locals` which represents the local variables declared in this function.
    pub params: Vec<Type>,
    pub locals: Vec<Type>,

    /// Contains all basic blocks that belongs to this function.
    /// The order of the blocks inside the `basic_blocks` vector is the "actual"
    /// order of the basic blocks and the flow of the program.
    basic_blocks: Vec<BasicBlock>,

    /// The key of the `basic_blocks_indices` are the names of the basic blocks
    /// that currently exists in `basic_blocks` and the values are the indices
    /// of the corressponding blocks inside the `basic_blocks` vector.
    ///
    /// This map is used to allow for fast lookups of basic blocks by their names
    /// but at the same time keeping the order of the blocks.
    basic_blocks_indices: HashMap<String, usize>,
}

impl Func {
    pub fn new(name: String, visibility: FuncVisibility, params: Vec<Type>, ret_ty: Type) -> Self {
        Self {
            name,
            visibility,
            params,
            ret_ty,
            locals: Vec::default(),
            basic_blocks: Vec::default(),
            basic_blocks_indices: HashMap::default(),
        }
    }

    /// Returns the index of the added local.
    pub fn add_local_var(&mut self, ty: Type) -> LocalVarIdx {
        self.locals.push(ty);
        LocalVarIdx(self.locals.len() - 1)
    }

    pub fn get_local_var(&self, local_var_idx: LocalVarIdx) -> Option<&Type> {
        self.locals.get(*local_var_idx)
    }

    /// Creates a new basic block and inserts it into this function.
    ///
    /// If a block with the given name `name` already exists in this function,
    /// the name will be changed (appended with a number) to make it unique.
    /// This new name will be returned as the result of this function call.
    pub fn insert_basic_block(&mut self, name: String) -> IrResult<String> {
        let idx = self.basic_blocks.len();
        self.new_basic_block(idx, name)
    }

    /// Creates a new basic block and inserts it into this function after the
    /// block with name `after`.
    ///
    /// If a block with the given name `name` already exists in this function,
    /// the name will be changed (appended with a number) to make it unique.
    /// This new name will be returned as the result of this function call.
    pub fn insert_basic_block_after(&mut self, name: String, after: &str) -> IrResult<String> {
        if let Some(idx) = self.basic_blocks_indices.get(after).cloned() {
            self.new_basic_block(idx, name)
        } else {
            Err(IrError::new(format!(
                "Unable to find basic block with name \"{}\".",
                after
            )))
        }
    }

    /// Creates a new basic block and inserts it into this function at index `idx`.
    ///
    /// If a block with the given name `name` already exists in this function,
    /// the name will be changed (appended with a number) to make it unique.
    /// This new name will be returned as the result of this function call.
    fn new_basic_block(&mut self, idx: usize, name: String) -> IrResult<String> {
        let mut actual_name = name.clone();
        if self.basic_blocks_indices.contains_key(&name) {
            let mut i = 1;
            while self.basic_blocks_indices.contains_key(&actual_name) {
                actual_name = format!("{}{}", name, i);
                i += 1;
            }
        }

        let basic_block = BasicBlock::new(actual_name.clone());
        self.insert(idx, basic_block)?;

        Ok(actual_name)
    }

    /// Returns the basic block with name `name`.
    pub fn get(&self, name: &str) -> Option<&BasicBlock> {
        let idx = *self.basic_blocks_indices.get(name)?;
        self.get_with_idx(idx)
    }

    /// Returns the basic block with name `name` as mutable.
    pub fn get_mut(&mut self, name: &str) -> Option<&mut BasicBlock> {
        let idx = *self.basic_blocks_indices.get(name)?;
        self.get_with_idx_mut(idx)
    }

    /// Returns the basic block with index `idx`.
    pub fn get_with_idx(&self, idx: usize) -> Option<&BasicBlock> {
        self.basic_blocks.get(idx)
    }

    /// Returns the basic block with index `idx` as mutable.
    pub fn get_with_idx_mut(&mut self, idx: usize) -> Option<&mut BasicBlock> {
        self.basic_blocks.get_mut(idx)
    }

    /// Returns the first basic block of this function.
    pub fn first(&self) -> Option<&BasicBlock> {
        self.basic_blocks.first()
    }

    /// Returns the first basic block of this function as mutable.
    pub fn first_mut(&mut self) -> Option<&mut BasicBlock> {
        self.basic_blocks.first_mut()
    }

    /// Returns the last basic block of this function.
    pub fn last(&self) -> Option<&BasicBlock> {
        self.basic_blocks.last()
    }

    /// Returns the last basic block of this function as mutable.
    pub fn last_mut(&mut self) -> Option<&mut BasicBlock> {
        self.basic_blocks.last_mut()
    }

    /// Removes the basic block at index `idx`, shifting the remanining blocks
    /// to the "left". This is also done for the hashmap used for fast lookups
    /// which means that this is a expensive (linear) operation.
    pub fn remove(&mut self, idx: usize) -> IrResult<BasicBlock> {
        if idx >= self.basic_blocks.len() {
            return Err(IrError::new(format!(
                "remove -- idx > len -- idx: {}, {:#?}",
                idx, self
            )));
        }

        let basic_block = self.basic_blocks.remove(idx);
        self.basic_blocks_indices.remove(basic_block.name());

        for elem_idx in self.basic_blocks_indices.values_mut() {
            if *elem_idx > idx {
                *elem_idx -= 1;
            }
        }

        Ok(basic_block)
    }

    /// Inserts the given basic block at index `idx`, shifting the existing
    /// blocks to the "right". This is also done for the hashmap used for fast
    /// lookups which means that this is a expensive (linear) operation.
    pub fn insert(&mut self, idx: usize, basic_block: BasicBlock) -> IrResult<()> {
        if idx > self.basic_blocks.len() {
            return Err(IrError::new(format!(
                "insert -- idx > len -- idx: {}, {:#?}",
                idx, self
            )));
        }

        let name = basic_block.name().to_string();
        self.basic_blocks.insert(idx, basic_block);

        if idx != self.basic_blocks.len() {
            for elem_idx in self.basic_blocks_indices.values_mut() {
                if *elem_idx >= idx {
                    *elem_idx += 1;
                }
            }
        }

        self.basic_blocks_indices.insert(name, idx);

        Ok(())
    }
}
