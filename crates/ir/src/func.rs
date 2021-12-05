use std::{collections::HashMap, fmt::Debug};

use crate::{
    basic_block::BasicBlock,
    error::{IrError, IrResult},
    LocalVarIdx,
};

use super::ty::Type;

#[derive(Clone)]
pub enum FuncVisibility {
    /// Indicates that this function should be exported/exposed. A function with
    /// this visibility needs to implement a function body.
    Export,

    /// Indicates that this function will be imported. I.e. this is a external
    /// function. A function with this visibility should not implement a function
    /// body.
    Import,

    /// Indicates that this function should NOT be exported/exposed, it should
    /// only be accessable by other functions in this module. A function with
    /// this visibility needs to implement a function body.
    None,
}

impl Debug for FuncVisibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Export => write!(f, "export "),
            Self::Import => write!(f, "import "),
            Self::None => write!(f, ""),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub name: String,
    pub visibility: FuncVisibility,
    pub ret_type: Type,

    /// The parameters of the function will be accessed with their indices, so
    /// there is no need to store their names here. The same is true for the
    /// `locals` which represents the local variables declared in this function.
    pub params: Vec<Type>,
    pub locals: Vec<Type>,

    /// Contains all basic blocks that belongs to this function.
    /// The order of the blocks inside the `basic_blocks` vector is the "actual"
    /// order of the basic blocks and the flow of the program.
    pub basic_blocks: Vec<BasicBlock>,

    /// The key of the `basic_blocks_indices` are the names of the basic blocks
    /// that currently exists in `basic_blocks` and the values are the indices
    /// of the corressponding blocks inside the `basic_blocks` vector.
    ///
    /// This map is used to allow for fast lookups of basic blocks by their names
    /// but at the same time keeping the order of the blocks.
    basic_blocks_indices: HashMap<String, usize>,

    /// Set to true if this function have var_args/variadic parameter list.
    pub is_var_arg: bool,
}

impl FuncDecl {
    pub fn new(
        name: String,
        visibility: FuncVisibility,
        params: Vec<Type>,
        ret_ty: Type,
        is_var_arg: bool,
    ) -> Self {
        Self {
            name,
            visibility,
            params,
            ret_type: ret_ty,
            locals: Vec::default(),
            basic_blocks: Vec::default(),
            basic_blocks_indices: HashMap::default(),
            is_var_arg,
        }
    }

    /// Returns the index of the added local.
    ///
    /// OBS! This only adds the local to the current function as an "existing"
    ///      variable. To be able to access it with a variable name/block_id
    ///      (which is needed when converting from AST to IR), you'll need to
    ///      add it to the `BuildState` through the `add_local_to_cur_func()`
    ///      function (which internally calls this function).
    pub fn add_local_var(&mut self, ty: Type) -> LocalVarIdx {
        self.locals.push(ty);
        LocalVarIdx(self.locals.len() - 1)
    }

    pub fn get_local_var(&self, local_var_idx: LocalVarIdx) -> Option<&Type> {
        self.locals.get(*local_var_idx)
    }

    /// Creates a new basic block and inserts it into this function.
    ///
    /// If a block with the given `label` already exists in this function,
    /// the label will be changed (appended with a number) to make it unique.
    /// This new label will be returned as the result of this function call.
    pub fn insert_new_block(&mut self, label: String) -> IrResult<String> {
        let idx = self.basic_blocks.len();
        self.new_block(idx, label)
    }

    /// Creates a new basic block and inserts it into this function after the
    /// block with label `after`.
    ///
    /// If a block with the given `label` already exists in this function, the
    /// label will be changed (appended with a number) to make it unique.
    /// This new label will be returned as the result of this function call.
    pub fn insert_new_block_after(&mut self, label: String, after: &str) -> IrResult<String> {
        if let Some(idx) = self.basic_blocks_indices.get(after).cloned() {
            self.new_block(idx + 1, label)
        } else {
            Err(IrError::new(format!(
                "Unable to find basic block with name \"{}\".",
                after
            )))
        }
    }

    /// Creates a new basic block and inserts it into this function at index `idx`.
    ///
    /// If a block with the given `label` already exists in this function,
    /// the label will be changed (appended with a number) to make it unique.
    /// This new label will be returned as the result of this function call.
    fn new_block(&mut self, idx: usize, label: String) -> IrResult<String> {
        let mut actual_label = label.clone();
        if self.basic_blocks_indices.contains_key(&label) {
            let mut i = 1;
            while self.basic_blocks_indices.contains_key(&actual_label) {
                actual_label = format!("{}{}", label, i);
                i += 1;
            }
        }

        let basic_block = BasicBlock::new(actual_label.clone());
        self.insert_block(idx, basic_block)?;

        Ok(actual_label)
    }

    /// Returns the basic block with label `label`.
    pub fn get_block(&self, label: &str) -> Option<&BasicBlock> {
        let idx = *self.basic_blocks_indices.get(label)?;
        self.get_block_with_idx(idx)
    }

    /// Returns the basic block with label `label` as mutable.
    pub fn get_block_mut(&mut self, label: &str) -> Option<&mut BasicBlock> {
        let idx = *self.basic_blocks_indices.get(label)?;
        self.get_block_with_idx_mut(idx)
    }

    /// Returns the basic block with index `idx`.
    pub fn get_block_with_idx(&self, idx: usize) -> Option<&BasicBlock> {
        self.basic_blocks.get(idx)
    }

    /// Returns the basic block with index `idx` as mutable.
    pub fn get_block_with_idx_mut(&mut self, idx: usize) -> Option<&mut BasicBlock> {
        self.basic_blocks.get_mut(idx)
    }

    /// Returns the first basic block of this function.
    pub fn first_block(&self) -> Option<&BasicBlock> {
        self.basic_blocks.first()
    }

    /// Returns the first basic block of this function as mutable.
    pub fn first_block_mut(&mut self) -> Option<&mut BasicBlock> {
        self.basic_blocks.first_mut()
    }

    /// Returns the last basic block of this function.
    pub fn last_block(&self) -> Option<&BasicBlock> {
        self.basic_blocks.last()
    }

    /// Returns the last basic block of this function as mutable.
    pub fn last_block_mut(&mut self) -> Option<&mut BasicBlock> {
        self.basic_blocks.last_mut()
    }

    /// Removes the basic block at index `idx`, shifting the remanining blocks
    /// to the "left". This is also done for the hashmap used for fast lookups
    /// which means that this is a expensive (linear) operation.
    pub fn remove_block(&mut self, idx: usize) -> IrResult<BasicBlock> {
        if idx >= self.basic_blocks.len() {
            return Err(IrError::new(format!(
                "remove -- idx > len -- idx: {}, {:#?}",
                idx, self
            )));
        }

        let basic_block = self.basic_blocks.remove(idx);
        self.basic_blocks_indices.remove(&basic_block.label);

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
    pub fn insert_block(&mut self, idx: usize, basic_block: BasicBlock) -> IrResult<()> {
        if idx > self.basic_blocks.len() {
            return Err(IrError::new(format!(
                "insert -- idx > len -- idx: {}, {:#?}",
                idx, self
            )));
        }

        let label = basic_block.label.clone();
        self.basic_blocks.insert(idx, basic_block);

        if idx != self.basic_blocks.len() {
            for elem_idx in self.basic_blocks_indices.values_mut() {
                if *elem_idx >= idx {
                    *elem_idx += 1;
                }
            }
        }

        self.basic_blocks_indices.insert(label, idx);

        Ok(())
    }
}
