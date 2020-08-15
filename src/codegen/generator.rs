use super::codegen_state::CodeGenState;
use crate::analyze::analyzer::AnalyzeContext;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::parse::token;
use crate::parse::token::{ParseToken, Variable};
use crate::CustomResult;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{
    basic_block::BasicBlock,
    types::{AnyTypeEnum, BasicTypeEnum},
};
use std::collections::HashMap;
use std::convert::TryFrom;
use token::{BlockId, ParseTokenKind};

pub(super) struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    pub analyze_context: &'ctx AnalyzeContext,
    pub state: CodeGenState<'ctx>,

    /// Contains pointers to variables that have been compiled.
    pub variables: HashMap<(String, BlockId), PointerValue<'ctx>>,

    /// Contains pointers to variables that have been compiled.
    pub functions: HashMap<(String, BlockId), &'ctx FunctionValue<'ctx>>,
}

pub fn generate<'a, 'ctx>(
    ast_root: &'ctx ParseToken,
    analyze_context: &'ctx AnalyzeContext,
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
) -> CustomResult<()> {
    let mut code_gen = CodeGen::new(context, analyze_context, builder, module);
    code_gen.compile_recursive(ast_root)?;

    // TODO: Temporary solution, loop through all merge blocks and look for all
    //       merge blocks with no instructions. If the merge block has a "wrapping"
    //       if-statement (a nested if-statement), the merge block should branch
    //       to the wrapping merge block.
    //       If there are no wrapping if-statement, just remove the empty merge
    //       block since it (probably) isn't used. This makes the assumption that
    //       the code has no logical flaw, which one shouldn't do.
    for (block_id, merge_block) in &code_gen.state.merge_blocks {
        if merge_block.get_first_instruction().is_none() {
            if let Some(wrapping_merge_block) = code_gen.get_parent_merge_block(*block_id)? {
                if let Some(block_info) = code_gen.analyze_context.block_info.get(block_id) {
                    if block_info.all_children_contains_branches {
                        merge_block.remove_from_function().map_err(|_| {
                            LangError::new(
                                format!(
                                    "Unable to remove empty merge block with block ID: {}",
                                    block_id
                                ),
                                CodeGenError,
                            )
                        })?;
                    } else {
                        code_gen.builder.position_at_end(*merge_block);
                        code_gen
                            .builder
                            .build_unconditional_branch(wrapping_merge_block);
                    }
                }
            } else {
                merge_block.remove_from_function().map_err(|_| {
                    LangError::new(
                        format!(
                            "Unable to remove empty merge block with block ID: {}",
                            block_id
                        ),
                        CodeGenError,
                    )
                })?;
            }
        }
    }
    Ok(())
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        analyze_context: &'ctx AnalyzeContext,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            context,
            analyze_context,
            state: CodeGenState::new(),

            builder,
            module,

            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub(super) fn compile_recursive(&mut self, token: &'ctx ParseToken) -> CustomResult<()> {
        match &token.kind {
            ParseTokenKind::Block(header, id, body) => {
                self.compile_block(header, *id, body)?;
            }
            ParseTokenKind::Statement(stmt) => {
                self.compile_stmt(&stmt)?;
            }
            ParseTokenKind::Expression(expr) => {
                self.compile_expr(expr)?;
            }
            ParseTokenKind::EndOfFile => (),
        }
        Ok(())
    }

    pub(super) fn compile_alloca(&self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        if let Some(var_type) = &var.ret_type {
            Ok(match var_type.t.to_codegen(&self.context)? {
                AnyTypeEnum::ArrayType(ty) => {
                    // TODO: Alloca array, need to figure out constant size first.
                    //self.builder.build_array_alloca(ty, &var.name)
                    return Err(LangError::new("TODO: Alloca array.".into(), CodeGenError));
                }
                AnyTypeEnum::FloatType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::IntType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::PointerType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::StructType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::VectorType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::FunctionType(_) => {
                    return Err(LangError::new(
                        "Tried to alloca function.".into(),
                        CodeGenError,
                    ));
                }
                AnyTypeEnum::VoidType(_) => {
                    return Err(LangError::new(
                        "Tried to alloca void type.".into(),
                        CodeGenError,
                    ));
                }
            })
        } else {
            Err(LangError::new(
                format!("type None when allocaing var: {:?}", &var.name),
                CodeGenError,
            ))
        }
    }

    /// Returns the BasicBlock representing the merge block for the if-statement
    /// with the block id `id` or the parent scope of the if-case with
    /// block id `id`.
    pub(super) fn get_merge_block(&self, id: BlockId) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(merge_block) = self.state.merge_blocks.get(&id) {
            Ok(*merge_block)
        } else {
            // Get from the parent scope if possible.
            let parent_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| {
                    LangError::new(
                        format!("Unable to find parent block with id {}", id),
                        CodeGenError,
                    )
                })?
                .parent_id;

            if let Some(merge_block) = self.state.merge_blocks.get(&parent_id) {
                Ok(*merge_block)
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to find merge block in blocks with id {} and parent {}.",
                        id, parent_id
                    ),
                    CodeGenError,
                ))
            }
        }
    }

    // TODO: Clean up.
    /// Returns the BasicBlock representing a "outer" if block if one exists.
    pub(super) fn get_parent_merge_block(
        &self,
        id: BlockId,
    ) -> CustomResult<Option<BasicBlock<'ctx>>> {
        if self.state.merge_blocks.get(&id).is_some() {
            let parent_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| {
                    LangError::new(
                        format!("Unable to find parent block with id {}", id),
                        CodeGenError,
                    )
                })?
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
                .ok_or_else(|| {
                    LangError::new(
                        format!("Unable to find parent block with id {}", id),
                        CodeGenError,
                    )
                })?
                .parent_id;

            let parent_id = self
                .analyze_context
                .block_info
                .get(&if_id)
                .ok_or_else(|| {
                    LangError::new(
                        format!("Unable to find parent block with id {}", id),
                        CodeGenError,
                    )
                })?
                .parent_id;

            Ok(self.get_merge_block(parent_id).ok())
        }
    }

    // TODO: How should a declaration of a "constant" be enforced?
    pub(super) fn compile_var_decl(&mut self, var: &Variable) -> CustomResult<()> {
        let id = self.state.cur_block_id;
        let key = (var.name.clone(), id);

        if let Some(var_decl) = self.analyze_context.variables.get(&key) {
            debug!("Compiling var decl. Key: {:?}", &key);

            let ptr = self.compile_alloca(var_decl)?;
            self.variables.insert(key, ptr);
            Ok(())
        } else {
            Err(LangError::new(
                format!("No decl for var when compiling var decl: {}", &var.name),
                CodeGenError,
            ))
        }
    }

    pub(super) fn compile_var_store(
        &mut self,
        var: &Variable,
        basic_value: BasicValueEnum,
    ) -> CustomResult<()> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Compile var_store, key: {:?}", &key);

        // Then get the pointer to the declared variable from the map
        // created during code generation.
        if let Some(ptr) = self.variables.get(&key) {
            self.builder.build_store(*ptr, basic_value);
            Ok(())
        } else {
            Err(LangError::new(
                format!("No decl for var `{}` when building store.", &var.name),
                CodeGenError,
            ))
        }
    }

    pub(super) fn compile_var_load(
        &mut self,
        var: &Variable,
    ) -> CustomResult<BasicValueEnum<'ctx>> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Compiling var load. Key: {:?}", &key);

        // Then get the pointer to the declared variable from the map
        // created during code generation.
        if let Some(ptr) = self.variables.get(&key) {
            Ok(self.builder.build_load(*ptr, "load"))
        } else {
            Err(LangError::new(
                format!("No decl for var `{}` when building load.", &var.name),
                CodeGenError,
            ))
        }
    }

    pub(super) fn any_into_basic_value(any_value: AnyValueEnum) -> CustomResult<BasicValueEnum> {
        BasicValueEnum::try_from(any_value).map_err(|_| {
            LangError::new(
                format!(
                    "Unable to convert AnyValueEnum: {:#?} into BasicValueEnum.",
                    any_value
                ),
                CodeGenError,
            )
        })
    }

    pub(super) fn any_into_basic_type(any_type: AnyTypeEnum) -> CustomResult<BasicTypeEnum> {
        BasicTypeEnum::try_from(any_type).map_err(|_| {
            LangError::new(
                format!(
                    "Unable to convert AnyTypeEnum: {:#?} into BasicTypeEnum.",
                    any_type
                ),
                CodeGenError,
            )
        })
    }
}
