use crate::parse::token::BlockId;
use inkwell::{
    basic_block::BasicBlock,
    values::{FunctionValue, PointerValue},
};
use std::collections::HashMap;

/// Contains the state of the generation at a point in time.
/// Having them all grouped is convenient when doing recursive compiling which
/// lets one easily clone and save the "old values" so that they can be re-set
/// after recursive calls.
#[derive(Debug, Clone, PartialEq)]
pub struct CodeGenState<'ctx> {
    /// The ID of the current block that is being compiled.
    pub cur_block_id: BlockId,

    /// Contains the current basic block that instructions are insrted into.
    pub cur_block: Option<BasicBlock<'ctx>>,

    /// Contains a pointer to the current function that is being generated.
    pub cur_func: Option<FunctionValue<'ctx>>,

    /// Contains the previously seen pointer value. This will be updated for
    /// every variable load/store. This variable will be used for example
    /// during indexing into variables.
    ///
    /// Example:
    ///   "var y = abc.x"
    /// first the "y" variable would be saved in this var since it does a store,
    /// then "abc" would be saved since it does a load and then "x" would be saved.
    /// This gives "x" a easy way to get the pointer for "abc" that is supposed
    /// to index into.
    pub prev_ptr_value: Option<PointerValue<'ctx>>,

    /// Merge blocks created for different if and match statements.
    /// Is stored in this struct so that it can be accessable from everywhere
    /// and statements etc. can figure out where to branch.
    pub merge_blocks: HashMap<BlockId, BasicBlock<'ctx>>,
}

impl<'ctx> CodeGenState<'ctx> {
    pub fn new() -> Self {
        Self {
            cur_block_id: 0,
            cur_block: None,
            cur_func: None,
            prev_ptr_value: None,
            merge_blocks: HashMap::default(),
        }
    }
}
