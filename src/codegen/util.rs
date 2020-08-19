use super::generator::CodeGen;
use crate::error::LangErrorKind::CodeGenError;
use crate::{error::LangError, parse::token::BlockId, CustomResult};
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

    /// Returns the BasicBlock representing the merge block for the if-statement
    /// with the block id `id` or the parent scope of the if-case with
    /// block id `id`.
    pub(super) fn get_merge_block(&self, id: BlockId) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(merge_block) = self.merge_blocks.get(&id) {
            Ok(*merge_block)
        } else {
            // Get from the parent scope if possible.
            let parent_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| self.err(format!("Unable to find parent block with id {}", id)))?
                .parent_id;

            if let Some(merge_block) = self.merge_blocks.get(&parent_id) {
                Ok(*merge_block)
            } else {
                Err(self.err(format!(
                    "Unable to find merge block in blocks with id {} and parent {}.",
                    id, parent_id
                )))
            }
        }
    }

    // TODO: Clean up.
    /// Returns the BasicBlock representing a "outer" if block if one exists.
    pub(super) fn get_parent_merge_block(
        &self,
        id: BlockId,
    ) -> CustomResult<Option<BasicBlock<'ctx>>> {
        if self.merge_blocks.get(&id).is_some() {
            let parent_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| self.err(format!("Unable to find parent block with id {}", id)))?
                .parent_id;

            Ok(self.get_merge_block(parent_id).ok())
        } else {
            // The given `id` was the block ID of a if case. First get the ID
            // if the wrapping "If" block. Then get the parent ID of that block
            // to get the sought after merge block.
            let if_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| self.err(format!("Unable to find parent block with id {}", id)))?
                .parent_id;

            let parent_id = self
                .analyze_context
                .block_info
                .get(&if_id)
                .ok_or_else(|| self.err(format!("Unable to find parent block with id {}", id)))?
                .parent_id;

            Ok(self.get_merge_block(parent_id).ok())
        }
    }
}
