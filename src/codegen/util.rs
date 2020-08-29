use super::generator::CodeGen;
use crate::error::LangErrorKind::CodeGenError;
use crate::{analyze::analyzer::BlockInfo, error::LangError, parse::token::BlockId, CustomResult};
use inkwell::{
    basic_block::BasicBlock,
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValueEnum},
};
use std::convert::TryFrom;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn any_into_basic_value(any_value: AnyValueEnum) -> CustomResult<BasicValueEnum> {
        BasicValueEnum::try_from(any_value).map_err(|_| {
            LangError::new(
                format!(
                    "Unable to convert AnyValueEnum: {:#?} into BasicValueEnum.",
                    &any_value
                ),
                CodeGenError {
                    line_nr: 0,
                    column_nr: 0,
                },
            )
        })
    }

    pub(super) fn any_into_basic_type(any_type: AnyTypeEnum) -> CustomResult<BasicTypeEnum> {
        BasicTypeEnum::try_from(any_type).map_err(|_| {
            LangError::new(
                format!(
                    "Unable to convert AnyTypeEnum: {:#?} into BasicTypeEnum.",
                    &any_type
                ),
                CodeGenError {
                    line_nr: 0,
                    column_nr: 0,
                },
            )
        })
    }

    /// Returns the BasicBlock representing the "closest" merge block from the
    /// current block. Merge blocks will be created for ex. if-statements
    /// and while-loops.
    pub(super) fn get_merge_block(&self, id: BlockId) -> CustomResult<BasicBlock<'ctx>> {
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
    pub(super) fn get_branchable_merge_block(&self, id: BlockId) -> CustomResult<BasicBlock<'ctx>> {
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

    fn get_block_info(&self, id: BlockId) -> CustomResult<&BlockInfo> {
        self.analyze_context.block_info.get(&id).ok_or_else(|| {
            self.err(format!(
                "Unable to find block info for block with id {}",
                id
            ))
        })
    }
}
