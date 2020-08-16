use super::codegen_state::CodeGenState;
use crate::analyze::analyzer::AnalyzeContext;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::parse::token;
use crate::parse::token::{ParseToken, Variable};
use crate::{common::variable_type::Type, CustomResult};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{AnyValueEnum, BasicValueEnum, PointerValue};
use inkwell::{
    basic_block::BasicBlock,
    types::{AnyTypeEnum, BasicTypeEnum},
    AddressSpace,
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

    /// Contains pointers to mutable variables that have been compiled.
    pub variables: HashMap<(String, BlockId), PointerValue<'ctx>>,

    /// Contains constant variables. They can't be used as regular variable
    /// in the code. Keep track of them in this hashmap and do calculations
    /// and update them in here during the codegen process.
    pub constants: HashMap<(String, BlockId), BasicValueEnum<'ctx>>,
}

pub fn generate<'a, 'ctx>(
    ast_root: &'ctx mut ParseToken,
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

            variables: HashMap::default(),
            constants: HashMap::default(),
        }
    }

    pub(super) fn compile_recursive(&mut self, token: &'ctx mut ParseToken) -> CustomResult<()> {
        match &mut token.kind {
            ParseTokenKind::Block(header, id, ref mut body) => {
                self.compile_block(header, *id, body)?;
            }
            ParseTokenKind::Statement(ref mut stmt) => {
                self.compile_stmt(stmt)?;
            }
            ParseTokenKind::Expression(ref mut expr) => {
                self.compile_expr(expr)?;
            }
            ParseTokenKind::EndOfFile => (),
        }
        Ok(())
    }

    pub(super) fn compile_alloca(&self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        if let Some(var_type) = &var.ret_type {
            Ok(match self.compile_type(&var_type.t)? {
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
                format!("type None when allocating var: {:?}", &var.name),
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

            // Constants are never "compiled" into instructions, they are handled
            // "internaly" in this code during compilation.
            if !var.is_const {
                let ptr = self.compile_alloca(var_decl)?;
                self.variables.insert(key, ptr);
                self.state.prev_ptr_value = Some(ptr);
            }

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
        basic_value: BasicValueEnum<'ctx>,
    ) -> CustomResult<()> {
        // Get the block ID of the block in which this variable was declared.
        let block_id = self.state.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Compile var_store, key: {:?}", &key);

        // If this is constant variable, just insert the value into the
        // varirable in the `constants` map. Otherwise, if this is a "regular"
        // variable, create a load instruction of that variable.
        if var.is_const {
            self.constants.insert(key, basic_value);
        } else if let Some(ptr) = self.variables.get(&key) {
            self.state.prev_ptr_value = Some(*ptr);
            self.builder.build_store(*ptr, basic_value);
        } else {
            return Err(LangError::new(
                format!(
                    "No decl for var `{}` in decl block {} when building store.",
                    &var.name, decl_block_id
                ),
                CodeGenError,
            ));
        }

        Ok(())
    }

    pub(super) fn compile_var_load(
        &mut self,
        var: &Variable,
    ) -> CustomResult<BasicValueEnum<'ctx>> {
        let block_id = self.state.cur_block_id;

        if var.is_struct_member {
            // TODO: Need to check for const in this func as well.
            self.compile_var_load_struct_member(var)
        } else {
            // Get the block ID of the block in which this variable was declared.
            let decl_block_id = self
                .analyze_context
                .get_var_decl_scope(&var.name, block_id)?;
            let key = (var.name.clone(), decl_block_id);
            debug!("Compiling var load. Key: {:?}", &key);

            // If the variable to load is a constant, get the value from the
            // internal `constants` hashmap. Otherwise, if it is a "regular"
            // variable, get the pointer created by a "alloca" pointing
            // to the variable on the stack and load that value.
            if var.is_const {
                if let Some(const_value) = self.constants.get(&key) {
                    Ok(*const_value)
                } else {
                    Err(LangError::new(
                        format!("No decl for constant `{}` when building load.", &var.name),
                        CodeGenError,
                    ))
                }
            } else if let Some(ptr) = self.variables.get(&key) {
                self.state.prev_ptr_value = Some(*ptr);
                Ok(self.builder.build_load(*ptr, "load"))
            } else {
                Err(LangError::new(
                    format!("No decl for var `{}` when building load.", &var.name),
                    CodeGenError,
                ))
            }
        }
    }

    // TODO: Need to check for const in this func as well.
    fn compile_var_load_struct_member(
        &mut self,
        var: &Variable,
    ) -> CustomResult<BasicValueEnum<'ctx>> {
        if let Some(prev_ptr_value) = self.state.prev_ptr_value {
            // Get a pointer to the member in the struct. This pointer can
            // then be used to load the value with a regular "load" instruction.
            let member_ptr = self
                .builder
                .build_struct_gep(prev_ptr_value, var.member_index, "struct.gep")
                .map_err(|_| {
                    LangError::new(
                        format!(
                            "Unable to struct_gep for struct {:?}, index {}.",
                            &var.struct_name, var.member_index
                        ),
                        CodeGenError,
                    )
                })?;

            self.state.prev_ptr_value = Some(member_ptr);
            Ok(self.builder.build_load(member_ptr, "struct.member.load"))
        } else {
            Err(LangError::new(
                "No prev_basic_value set when compiling struct member load".into(),
                CodeGenError,
            ))
        }
    }

    pub(super) fn compile_type(&self, ty: &Type) -> CustomResult<AnyTypeEnum<'ctx>> {
        // TODO: What AddressSpace should be used?
        let address_space = AddressSpace::Global;

        Ok(match ty {
            Type::Pointer(ptr) => {
                // Get the type of the inner type and wrap into a "PointerType".
                match self.compile_type(ptr)? {
                    AnyTypeEnum::ArrayType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::FloatType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::FunctionType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::IntType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::PointerType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::StructType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::VectorType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::VoidType(_) => {
                        // TODO: FIXME: Is this OK? Can't use pointer to void, use
                        //              poniter to a igeneric" I8 instead.
                        self.context.i8_type().ptr_type(address_space).into()
                    }
                }
            }
            Type::Array(t, dim_opt) => {
                // TODO: Can fetch the inner type and call "array_type()" on it,
                //       but the function takes a "u32" as argument, so need to
                //       convert the "dim_opt" Expression into a u32 if possible.
                return Err(LangError::new(
                    "TODO: Array. Need to calculate dimension and the return a \"ArrayType\""
                        .into(),
                    CodeGenError,
                ));
            }
            Type::Void => AnyTypeEnum::VoidType(self.context.void_type()),
            Type::Character => AnyTypeEnum::IntType(self.context.i32_type()),
            // TODO: What type should the string be?
            Type::String => {
                AnyTypeEnum::PointerType(self.context.i8_type().ptr_type(address_space))
            }
            Type::Boolean => AnyTypeEnum::IntType(self.context.bool_type()),
            Type::Int => AnyTypeEnum::IntType(self.context.i32_type()),
            Type::Uint => AnyTypeEnum::IntType(self.context.i32_type()),
            Type::Float => AnyTypeEnum::FloatType(self.context.f32_type()),
            Type::I8 => AnyTypeEnum::IntType(self.context.i8_type()),
            Type::U8 => AnyTypeEnum::IntType(self.context.i8_type()),
            Type::I16 => AnyTypeEnum::IntType(self.context.i16_type()),
            Type::U16 => AnyTypeEnum::IntType(self.context.i16_type()),
            Type::I32 => AnyTypeEnum::IntType(self.context.i32_type()),
            Type::U32 => AnyTypeEnum::IntType(self.context.i32_type()),
            Type::F32 => AnyTypeEnum::FloatType(self.context.f32_type()),
            Type::I64 => AnyTypeEnum::IntType(self.context.i64_type()),
            Type::U64 => AnyTypeEnum::IntType(self.context.i64_type()),
            Type::F64 => AnyTypeEnum::FloatType(self.context.f64_type()),
            Type::I128 => AnyTypeEnum::IntType(self.context.i128_type()),
            Type::U128 => AnyTypeEnum::IntType(self.context.i128_type()),
            Type::Custom(ref ident) => {
                if let Some(struct_type) = self.module.get_struct_type(ident) {
                    struct_type.clone().into()
                } else {
                    return Err(LangError::new(
                        format!("Unable to find custom type: {}", ident),
                        CodeGenError,
                    ));
                }
            }
        })
    }

    /// Returns true if the types have the same "base type". Ex. if both values
    /// are int, float, pointer etc.
    pub(crate) fn is_same_base_type(
        &self,
        left_type: BasicTypeEnum<'ctx>,
        right_type: BasicTypeEnum<'ctx>,
    ) -> bool {
        left_type.is_int_type() && right_type.is_int_type()
            || left_type.is_float_type() && right_type.is_float_type()
            || left_type.is_array_type() && right_type.is_array_type()
            || left_type.is_pointer_type() && right_type.is_pointer_type()
            || left_type.is_struct_type() && right_type.is_struct_type()
            || left_type.is_vector_type() && right_type.is_vector_type()
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
