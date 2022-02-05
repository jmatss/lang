use std::collections::HashMap;

use common::{
    ctx::{analyze_ctx::AnalyzeCtx, block_ctx::BlockCtx},
    error::{LangError, LangErrorKind, LangResult},
    BlockId,
};
use ir::{BasicBlock, FuncDecl, GlobalVarIdx, Module, Type, VarIdx};

use crate::{
    builder::InstrBuilder,
    collect::{local::LocalVarMap, param::ParamVarMap},
    into_err, VarModifier,
};

// TODO: Handle `const` variables differently than regular `var`s. Should never
//       be stored/loaded.

pub struct BuildState<'ctx, 'a> {
    pub module: &'a mut Module,
    pub builder: &'a mut InstrBuilder,
    pub analyze_ctx: &'a mut AnalyzeCtx<'ctx>,

    /// The string key is the name of the variable.
    pub globals: HashMap<String, GlobalVarIdx>,
    /// The string key is the name of the func that this variable was found in.
    pub locals: HashMap<String, LocalVarMap>,
    /// The string key is the name of the func that this variable was found in.
    pub params: HashMap<String, ParamVarMap>,

    /// The ID of the current block that we are in.
    /// OBS! A `BlockId` is completly unrelated to a `BasicBlock`. This ID is
    ///      used to identify every block scope in the AST.
    pub cur_block_id: BlockId,

    /// Contains the label of current basic block that instructions are inserted
    /// into.  OBS! A `BasicBlock` is completly unrelated to `BlockId`.
    pub cur_basic_block_label: Option<String>,

    /// Contains the name of the current function that is being built.
    pub cur_func_name: Option<String>,

    /// Contains the label of the current "branch block" if the current block has
    /// one. This is true for "while" and "for" blocks. This branch block will
    /// then be used when a continue call is done to find the start of the loop.
    pub cur_branch_block_label: Option<String>,

    /// Merge blocks created for different if and match statements.
    /// Is stored in this struct so that it can be accessable from everywhere
    /// and statements etc. can figure out where to branch.
    ///
    /// The String value of them map are basic block names.
    pub merge_blocks: HashMap<BlockId, String>,
}

impl<'ctx, 'a> BuildState<'ctx, 'a> {
    pub fn new(
        module: &'a mut Module,
        instr_builder: &'a mut InstrBuilder,
        analyze_ctx: &'a mut AnalyzeCtx<'ctx>,
        globals: HashMap<String, GlobalVarIdx>,
        locals: HashMap<String, LocalVarMap>,
        params: HashMap<String, ParamVarMap>,
    ) -> Self {
        Self {
            module,
            builder: instr_builder,
            analyze_ctx,
            globals,
            locals,
            params,
            cur_block_id: BlockCtx::DEFAULT_BLOCK_ID,
            cur_basic_block_label: None,
            cur_func_name: None,
            cur_branch_block_label: None,
            merge_blocks: HashMap::default(),
        }
    }

    pub fn set_cur_func(&mut self, name: Option<String>) {
        self.cur_func_name = name;
    }

    pub fn cur_func(&self) -> LangResult<&FuncDecl> {
        if let Some(func_name) = &self.cur_func_name {
            if let Some(func) = self.module.get_func(func_name) {
                Ok(func)
            } else {
                Err(LangError::new(
                    "Unable to get current func.".into(),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        } else {
            Err(LangError::new(
                "Cur func name not set.".into(),
                LangErrorKind::IrError,
                None,
            ))
        }
    }

    pub fn cur_func_mut(&mut self) -> LangResult<&mut FuncDecl> {
        if let Some(func_name) = &self.cur_func_name {
            if let Some(func) = self.module.get_func_mut(func_name) {
                Ok(func)
            } else {
                Err(LangError::new(
                    "Unable to get current func.".into(),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        } else {
            Err(LangError::new(
                "Cur func name not set.".into(),
                LangErrorKind::IrError,
                None,
            ))
        }
    }

    /// Conventient wrapper to insert new blocks into the current function.
    pub fn insert_new_block(&mut self, label: String) -> LangResult<String> {
        self.cur_func_mut()?
            .insert_new_block(label)
            .map_err(into_err)
    }

    /// Conventient wrapper to insert new blocks into the current function.
    pub fn insert_new_block_after(&mut self, label: String, after: &str) -> LangResult<String> {
        self.cur_func_mut()?
            .insert_new_block_after(label, after)
            .map_err(into_err)
    }

    pub fn set_cur_block(&mut self, label: Option<String>) {
        self.cur_basic_block_label = label;
    }

    pub fn cur_block(&self) -> LangResult<&BasicBlock> {
        if let Some(block_label) = &self.cur_basic_block_label {
            if let Some(block) = self.cur_func()?.get_block(block_label) {
                Ok(block)
            } else {
                Err(LangError::new(
                    "Unable to get current block.".into(),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        } else {
            Err(LangError::new(
                "Cur block label not set.".into(),
                LangErrorKind::IrError,
                None,
            ))
        }
    }

    pub fn cur_block_mut(&mut self) -> LangResult<&mut BasicBlock> {
        if let Some(block_label) = self.cur_basic_block_label.clone() {
            if let Some(block) = self.cur_func_mut()?.get_block_mut(&block_label) {
                Ok(block)
            } else {
                Err(LangError::new(
                    "Unable to get current block.".into(),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        } else {
            Err(LangError::new(
                "Cur block label not set.".into(),
                LangErrorKind::IrError,
                None,
            ))
        }
    }

    /// Returns the BasicBlock representing the "closest" merge block from the
    /// current block. Merge blocks will be created for ex. if-statements
    /// and while-loops.
    pub fn get_merge_block(&self, id: BlockId) -> LangResult<&BasicBlock> {
        let mut cur_id = id;
        loop {
            if let Some(merge_block_label) = self.merge_blocks.get(&cur_id) {
                return self
                    .cur_func()?
                    .get_block(merge_block_label)
                    .ok_or_else(|| {
                        LangError::new(
                            format!(
                                "Unable to find merge block with label: {}.",
                                merge_block_label
                            ),
                            LangErrorKind::IrError,
                            None,
                        )
                    });
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

    // TODO: Merge with `get()`.
    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// the IR index of the given variable. The given `ident` should contain
    /// any "copy_nr".
    /// This can either be a global, function parameter or local variable.
    pub fn get_var(&self, ident: &str, id: BlockId) -> LangResult<VarIdx> {
        if let Some(global_var_idx) = self.globals.get(ident) {
            Ok(VarIdx::Global(*global_var_idx))
        } else {
            self.get_local_or_param_var(ident, id)
        }
    }

    /// Given a name of a local variable `ident` and a block scope `id`, returns
    /// the index of the variable. This can either be a local or parameter.
    fn get_local_or_param_var(&self, ident: &str, id: BlockId) -> LangResult<VarIdx> {
        let func_name = if let Some(cur_func_name) = &self.cur_func_name {
            cur_func_name
        } else {
            return Err(LangError::new(
                format!("Unable to find var with name \"{}\" (not in func)", ident),
                LangErrorKind::IrError,
                None,
            ));
        };

        if let Some(func_entry) = self.locals.get(func_name) {
            if let Ok((idx, _)) = self.get_local_or_param_var_rec(func_entry, func_name, ident, id)
            {
                return Ok(VarIdx::Local(idx));
            }
        }

        if let Some(func_entry) = self.params.get(func_name) {
            if let Ok((idx, _)) = self.get_local_or_param_var_rec(func_entry, func_name, ident, id)
            {
                return Ok(VarIdx::Param(idx));
            }
        }

        Err(LangError::new(
            format!(
                "Unable to find var with name \"{}\" in function \"{}\".",
                ident, func_name
            ),
            LangErrorKind::IrError,
            None,
        ))
    }

    fn get_local_or_param_var_rec<T: ToOwned<Owned = T>>(
        &self,
        func_entry: &HashMap<(String, BlockId), T>,
        func_name: &str,
        ident: &str,
        id: BlockId,
    ) -> LangResult<T> {
        if let Some(var_idx) = func_entry.get(&(ident.into(), id)) {
            Ok(var_idx.to_owned())
        } else if id == BlockCtx::DEFAULT_BLOCK_ID {
            return Err(LangError::new(
                format!(
                    "Unable to find var with name \"{}\" (currently in func \"{}\")",
                    ident, func_name
                ),
                LangErrorKind::IrError,
                None,
            ));
        } else {
            // Unable to find declaration in the current block scope. See
            // recursively if the declaration exists in a parent scope.
            let parent_id = self.analyze_ctx.ast_ctx.get_parent_id(id)?;

            if id != parent_id {
                self.get_local_or_param_var_rec(func_entry, func_name, ident, parent_id)
            } else {
                Err(LangError::new(
                    format!("Block with id {} is its own parent in block info.", id),
                    LangErrorKind::IrError,
                    None,
                ))
            }
        }
    }

    /// Adds the local with name `name` in block with ID `block_id` into the
    /// `self.cur_func_name`.
    pub fn add_local_to_cur_func(
        &mut self,
        name: String,
        block_id: BlockId,
        ir_type: Type,
        modifier: VarModifier,
    ) -> LangResult<()> {
        let cur_func = self.cur_func_mut()?;
        let cur_func_name = cur_func.name.clone();

        let var_idx = cur_func.add_local_var(ir_type);
        let locals = self.locals.entry(cur_func_name).or_default();
        locals.insert((name, block_id), (var_idx, modifier));
        Ok(())
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
            if let Some(basic_block) = state.cur_func()?.get_block(basic_block_name) {
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
            if let Some(basic_block) = state.cur_func()?.get_block(basic_block_name) {
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
