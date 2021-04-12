use std::convert::TryFrom;

use inkwell::{
    basic_block::BasicBlock,
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValueEnum},
};

use common::{
    ctx::block_ctx::BlockCtx,
    error::{LangError, LangErrorKind::CodeGenError, LangResult},
    BlockId,
};

use super::generator::CodeGen;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn any_into_basic_value(any_value: AnyValueEnum) -> LangResult<BasicValueEnum> {
        BasicValueEnum::try_from(any_value).map_err(|_| {
            LangError::new(
                format!(
                    "Unable to convert AnyValueEnum: {:#?} into BasicValueEnum.",
                    &any_value
                ),
                CodeGenError,
                None,
            )
        })
    }

    pub(super) fn any_into_basic_type(any_type: AnyTypeEnum) -> LangResult<BasicTypeEnum> {
        BasicTypeEnum::try_from(any_type).map_err(|_| {
            LangError::new(
                format!(
                    "Unable to convert AnyTypeEnum: {:#?} into BasicTypeEnum.",
                    &any_type
                ),
                CodeGenError,
                None,
            )
        })
    }

    /// Returns true if all basic values in `values` are const.
    pub(super) fn is_const(values: &[AnyValueEnum<'ctx>]) -> bool {
        for value in values.iter() {
            let is_const = match *value {
                AnyValueEnum::ArrayValue(val) => val.is_const(),
                AnyValueEnum::IntValue(val) => val.is_const(),
                AnyValueEnum::FloatValue(val) => val.is_const(),
                AnyValueEnum::PointerValue(val) => val.is_const(),
                AnyValueEnum::StructValue(val) => {
                    // TODO: Should probably be some way to iterate through all its member
                    //       recursively and figure out if all fields of the struct is
                    //       const. If that is the case, one can assume that the struct
                    //       is also const.
                    // If the struct has no name, this is a constant struct
                    // according to inkwell documentation.
                    val.get_name().to_bytes().is_empty()
                }
                AnyValueEnum::VectorValue(val) => val.is_const(),

                AnyValueEnum::PhiValue(_)
                | AnyValueEnum::FunctionValue(_)
                | AnyValueEnum::InstructionValue(_) => false,
            };

            if !is_const {
                return false;
            }
        }
        true
    }

    /// Returns the BasicBlock representing the "closest" merge block from the
    /// current block. Merge blocks will be created for ex. if-statements
    /// and while-loops.
    pub(super) fn get_merge_block(&self, id: BlockId) -> LangResult<BasicBlock<'ctx>> {
        let mut cur_id = id;
        loop {
            if let Some(merge_block) = self.merge_blocks.get(&cur_id) {
                return Ok(*merge_block);
            } else {
                // Get from the parent scope recursively if possible.
                cur_id = self.get_block_info(cur_id)?.parent_id;
            }
        }
    }

    /// Returns the BasicBlock representing the "closest" merge block from the
    /// current block that is "branchable". This is merge blocks that can contain
    /// ex. "break"-statements like while-loops.
    pub(super) fn get_branchable_merge_block(&self, id: BlockId) -> LangResult<BasicBlock<'ctx>> {
        let mut cur_id = id;
        loop {
            let merge_block = self.get_merge_block(cur_id)?;
            let cur_block_info = self.get_block_info(cur_id)?;

            if cur_block_info.is_branchable_block {
                return Ok(merge_block);
            } else {
                // If this isn't a branchable block, continue looking through parents.
                cur_id = cur_block_info.parent_id;
            }
        }
    }

    fn get_block_info(&self, id: BlockId) -> LangResult<&BlockCtx> {
        self.analyze_ctx.ast_ctx.block_ctxs.get(&id).ok_or_else(|| {
            self.err(
                format!("Unable to find block info for block with id {}", id),
                None,
            )
        })
    }
}
