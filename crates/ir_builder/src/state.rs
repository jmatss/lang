use std::collections::HashMap;

use common::{
    ctx::{analyze_ctx::AnalyzeCtx, block_ctx::BlockCtx},
    error::{LangError, LangErrorKind, LangResult},
    BlockId,
};
use ir::{basic_block::BasicBlock, decl::func::Func, module::Module, Val};

pub(crate) struct BuildState<'ctx, 'a> {
    pub module: &'a mut Module,
    pub analyze_ctx: &'a mut AnalyzeCtx<'ctx>,

    /// The ID of the current block that we are in.
    /// OBS! A `BlockId` is completly unrelated to a `BasicBlock`. This ID is
    ///      used to identify every block scope in the AST.
    pub cur_block_id: BlockId,

    /// Contains the name of current basic block that instructions are inserted
    /// into.
    /// OBS! A `BasicBlock` is completly unrelated to `BlockId`.
    cur_basic_block_name: Option<String>,

    /// Contains the name of the current function that is being built.
    cur_func_name: Option<String>,

    /// Contains the name of the current "branch block" if the current block has
    /// one. This is true for "while" and "for" blocks. This branch block will
    /// then be used when a continue call is done to find the start of the loop.
    pub cur_branch_block_name: Option<String>,

    /// Merge blocks created for different if and match statements.
    /// Is stored in this struct so that it can be accessable from everywhere
    /// and statements etc. can figure out where to branch.
    ///
    /// The String value of them map are basic block names.
    pub merge_blocks: HashMap<BlockId, String>,

    // TODO: Should these only be unique per function? Currently they are unique
    //       for the whole module, but it is possible to reset this count for
    //       every new func.
    /// A number used to create `Val` structs. For every new `Val` created, this
    /// `val` be incremented to ensure that every new `Val` is unique.
    val: usize,
}

impl<'ctx, 'a> BuildState<'ctx, 'a> {
    pub fn new(module: &'a mut Module, analyze_ctx: &'a mut AnalyzeCtx<'ctx>) -> Self {
        Self {
            module,
            analyze_ctx,
            cur_block_id: 0,
            cur_basic_block_name: None,
            cur_func_name: None,
            cur_branch_block_name: None,
            merge_blocks: HashMap::default(),
            val: 0,
        }
    }

    pub fn set_cur_func(&mut self, name: Option<String>) {
        self.cur_func_name = name;
    }

    pub fn cur_func(&self) -> LangResult<&Func> {
        self.cur_func_name
            .map(|name| self.module.get_function(&name))
            .flatten()
            .ok_or(LangError::new(
                "Unable to get current func.".into(),
                LangErrorKind::IrError,
                None,
            ))
    }

    pub fn cur_func_mut(&mut self) -> LangResult<&mut Func> {
        self.cur_func_name
            .map(|name| self.module.get_function_mut(&name))
            .flatten()
            .ok_or(LangError::new(
                "Unable to get current func.".into(),
                LangErrorKind::IrError,
                None,
            ))
    }

    pub fn set_cur_basic_block(&mut self, name: Option<String>) {
        self.cur_basic_block_name = name;
    }

    pub fn cur_basic_block(&self) -> LangResult<&BasicBlock> {
        let cur_func = self.cur_func()?;
        self.cur_basic_block_name
            .map(|name| cur_func.get(&name))
            .flatten()
            .ok_or(LangError::new(
                "Unable to get current basic block.".into(),
                LangErrorKind::IrError,
                None,
            ))
    }

    pub fn cur_basic_block_mut(&mut self) -> LangResult<&mut BasicBlock> {
        let cur_func = self.cur_func_mut()?;
        self.cur_basic_block_name
            .map(|name| cur_func.get_mut(&name))
            .flatten()
            .ok_or(LangError::new(
                "Unable to get current basic block.".into(),
                LangErrorKind::IrError,
                None,
            ))
    }

    /// Returns the BasicBlock representing the "closest" merge block from the
    /// current block. Merge blocks will be created for ex. if-statements
    /// and while-loops.
    pub fn get_merge_block(&self, id: BlockId) -> LangResult<&BasicBlock> {
        let mut cur_id = id;
        loop {
            if let Some(merge_block_name) = self.merge_blocks.get(&cur_id) {
                self.cur_func()?.get(merge_block_name).ok_or(LangError::new(
                    format!(
                        "Unable to find merge block with name: {}.",
                        merge_block_name
                    ),
                    LangErrorKind::IrError,
                    None,
                ));
            } else {
                // Get from the parent scope recursively if possible.
                cur_id = self.get_block_ctx(cur_id)?.parent_id;
            }
        }
    }

    /// Returns the BasicBlock representing the "closest" merge block from the
    /// current block that is "branchable". This is merge blocks that can contain
    /// ex. "break"-statements like while-loops.
    pub(super) fn get_branchable_merge_block(&self, id: BlockId) -> LangResult<&BasicBlock> {
        let mut cur_id = id;
        loop {
            let merge_block = self.get_merge_block(cur_id)?;
            let cur_block_ctx = self.get_block_ctx(cur_id)?;

            if cur_block_ctx.is_branchable_block {
                return Ok(merge_block);
            } else {
                // If this isn't a branchable block, continue looking through parents.
                cur_id = cur_block_ctx.parent_id;
            }
        }
    }

    /// Gets the `BlockCtx` for the block with BlockId `id`.
    pub fn get_block_ctx(&self, id: BlockId) -> LangResult<&BlockCtx> {
        self.analyze_ctx.ast_ctx.block_ctxs.get(&id).ok_or_else(|| {
            LangError::new(
                format!("Unable to find block info for block with id {}", id),
                LangErrorKind::IrError,
                None,
            )
        })
    }

    pub fn new_val(&mut self) -> Val {
        self.val += 1;
        Val(self.val)
    }
}

/// Contains information related to branches in either an if-statement or a
/// match-statement. This will then be sent around to all if-cases so that
/// they can see all information about where to branch etc.
pub(crate) struct BranchInfo {
    /// Vectors are sorted, so the first if case/branch is at index 0 etc.
    /// The String's are the name of the basic blocks.
    pub if_cases: Vec<String>,
    pub if_branches: Vec<String>,
}

impl BranchInfo {
    pub fn new() -> Self {
        Self {
            if_cases: Vec::default(),
            if_branches: Vec::default(),
        }
    }

    /// Returns the "if case" basic block at index `index` if one exists.
    pub fn get_if_case<'state>(
        &self,
        state: &'state BuildState,
        index: usize,
    ) -> LangResult<&'state BasicBlock> {
        if let Some(basic_block_name) = self.if_cases.get(index) {
            if let Some(basic_block) = state.cur_func()?.get(basic_block_name) {
                Ok(basic_block)
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to get if_case with index \"{}\" from func: {}",
                        index,
                        state.cur_func()?.name
                    ),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        } else {
            Err(LangError::new(
                format!("Unable to get if_case with index: {}", index),
                LangErrorKind::IrError,
                None,
            ))
        }
    }

    /// Returns the "if branch" basic block at index `index` if one exists.
    pub fn get_if_branch<'state>(
        &self,
        state: &'state BuildState,
        index: usize,
    ) -> LangResult<&'state BasicBlock> {
        if let Some(basic_block_name) = self.if_branches.get(index) {
            if let Some(basic_block) = state.cur_func()?.get(basic_block_name) {
                Ok(basic_block)
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to get if_branch with index \"{}\" from func: {}",
                        index,
                        state.cur_func()?.name
                    ),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        } else {
            Err(LangError::new(
                format!("Unable to get if_branch with index: {}", index),
                LangErrorKind::IrError,
                None,
            ))
        }
    }
}
