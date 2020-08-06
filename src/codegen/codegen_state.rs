use crate::parse::token::BlockId;
use inkwell::{basic_block::BasicBlock, values::FunctionValue};

/// Contains the state of the generation at a point in time.
/// Having them all grouped is convenient when doing recursive compiling which
/// lets one easily clone and save the "old values" so that they can be re-set
/// after recursive calls.
#[derive(Debug, Clone, PartialEq)]
pub struct CodeGenState<'ctx> {
    /// The ID of the current block that is being compiled.
    pub cur_block_id: BlockId,

    /// Contains the current basic block that instructions are instrted into.
    pub cur_block: Option<BasicBlock<'ctx>>,

    /// Contains a pointer to the current function that is being generated.
    pub cur_func: Option<FunctionValue<'ctx>>,

    /// Contains the basic blocks for the "if cases" of the current if-statement
    /// that is being parsed. Will be set to None if the generator currently isn't
    /// inside a if-statement.
    pub cur_if_cases: Option<Vec<BasicBlock<'ctx>>>,
    pub cur_if_branches: Option<Vec<BasicBlock<'ctx>>>,
    pub cur_merge_block: Option<BasicBlock<'ctx>>,
}

impl<'ctx> CodeGenState<'ctx> {
    pub fn new() -> Self {
        Self {
            cur_block_id: 0,
            cur_block: None,
            cur_func: None,
            cur_if_cases: None,
            cur_if_branches: None,
            cur_merge_block: None,
        }
    }
}
