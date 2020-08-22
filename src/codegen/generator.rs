use crate::analyze::analyzer::AnalyzeContext;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::parse::token;
use crate::parse::token::{ParseToken, Variable};
use crate::{common::variable_type::Type, CustomResult};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, InstructionValue, PointerValue};
use inkwell::{
    basic_block::BasicBlock,
    targets::TargetMachine,
    types::{AnyTypeEnum, BasicTypeEnum},
    AddressSpace,
};
use std::collections::HashMap;
use token::{AccessInstruction, BlockId, ParseTokenKind, TypeStruct};

pub(super) struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub target_machine: &'a TargetMachine,

    /// Information parsed during the "Analyzing" stage. This contains ex.
    /// defintions (var, struct, func etc.) and information about the AST blocks.
    pub analyze_context: &'ctx AnalyzeContext,

    /// The ID of the current block that is being compiled.
    pub cur_block_id: BlockId,
    pub cur_line_nr: u64,
    pub cur_column_nr: u64,

    /// Contains the current basic block that instructions are inserted into.
    pub cur_basic_block: Option<BasicBlock<'ctx>>,

    /// Contains a pointer to the current function that is being generated.
    pub cur_func: Option<FunctionValue<'ctx>>,

    /// Merge blocks created for different if and match statements.
    /// Is stored in this struct so that it can be accessable from everywhere
    /// and statements etc. can figure out where to branch.
    pub merge_blocks: HashMap<BlockId, BasicBlock<'ctx>>,

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
    target_machine: &'a TargetMachine,
) -> CustomResult<()> {
    let mut code_gen = CodeGen::new(context, analyze_context, builder, module, target_machine);
    code_gen.compile(ast_root)?;

    // TODO: Temporary solution, loop through all merge blocks and look for all
    //       merge blocks with no terminator instruction. If the merge block has
    //       a "wrapping" block, the merge block should branch to the wrapping
    //       blocks merge block. Otherwise something has gone wrong.
    for (block_id, merge_block) in &code_gen.merge_blocks {
        if merge_block.get_terminator().is_none() {
            if let Some(wrapping_merge_block) = code_gen.get_parent_merge_block(*block_id)? {
                code_gen.builder.position_at_end(*merge_block);
                code_gen
                    .builder
                    .build_unconditional_branch(wrapping_merge_block);
            } else {
                return Err(code_gen.err(format!(
                    "MergeBlock for block with ID {} has no terminator and no wrapping block.",
                    block_id
                )));
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
        target_machine: &'a TargetMachine,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            target_machine,

            analyze_context,

            cur_line_nr: 0,
            cur_column_nr: 0,

            cur_block_id: 0,
            cur_basic_block: None,
            cur_func: None,

            merge_blocks: HashMap::default(),
            variables: HashMap::default(),
            constants: HashMap::default(),
        }
    }

    pub(super) fn compile(&mut self, token: &'ctx mut ParseToken) -> CustomResult<()> {
        self.cur_line_nr = token.line_nr;
        self.cur_column_nr = token.column_nr;

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
            Ok(match self.compile_type(&var_type)? {
                AnyTypeEnum::ArrayType(ty) => {
                    // TODO: Alloca array, need to figure out constant size first.
                    //self.builder.build_array_alloca(ty, &var.name)
                    return Err(self.err("TODO: Alloca array.".into()));
                }
                AnyTypeEnum::FloatType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::IntType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::PointerType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::StructType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::VectorType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err("Tried to alloca function.".into()));
                }
                AnyTypeEnum::VoidType(_) => {
                    return Err(self.err("Tried to alloca void type.".into()));
                }
            })
        } else {
            Err(self.err(format!("type None when allocating var: {:?}", &var.name)))
        }
    }

    pub(super) fn compile_var_decl(&mut self, var: &Variable) -> CustomResult<()> {
        let block_id = self.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);

        if let Some(var_decl) = self.analyze_context.variables.get(&key) {
            debug!("Compiling var decl. Key: {:?}", &key);

            // Constants are never "compiled" into instructions, they are handled
            // "internaly" in this code during compilation.
            if !var.is_const {
                let ptr = self.compile_alloca(var_decl)?;
                self.variables.insert(key, ptr);
            }

            Ok(())
        } else {
            Err(self.err(format!(
                "No decl for variable \"{}\" when compiling var decl.",
                &var.name
            )))
        }
    }

    pub(super) fn compile_var_store(
        &mut self,
        var: &Variable,
        basic_value: BasicValueEnum<'ctx>,
    ) -> CustomResult<InstructionValue<'ctx>> {
        debug!(
            "Compile var_store, var name: {:?}, basic_value: {:?}.",
            &var.name, &basic_value
        );

        // TODO: Const isn't working atm. Need to treat const struct member and
        //       regular variables differently.
        if var.is_const {
            let block_id = self.cur_block_id;
            let decl_block_id = self
                .analyze_context
                .get_var_decl_scope(&var.name, block_id)?;
            let key = (var.name.clone(), decl_block_id);

            self.constants.insert(key, basic_value);
        }

        let ptr = if var.access_instrs.is_some() {
            self.get_var_ptr_access_instrs(var)?
        } else {
            self.get_var_ptr(var)?
        };

        Ok(self.builder.build_store(ptr, basic_value))
    }

    pub(super) fn compile_var_load(
        &mut self,
        var: &Variable,
    ) -> CustomResult<BasicValueEnum<'ctx>> {
        if var.is_const {
            return self.get_const_value(var);
        }

        let ptr = if var.access_instrs.is_some() {
            self.get_var_ptr_access_instrs(var)?
        } else {
            self.get_var_ptr(var)?
        };

        Ok(self.builder.build_load(ptr, "load"))
    }

    fn get_var_ptr(&mut self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        let block_id = self.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Loading variable pointer. Key: {:?}", &key);

        if let Some(var_ptr) = self.variables.get(&key) {
            Ok(*var_ptr)
        } else {
            Err(self.err(format!(
                "Unable to find ptr for variable \"{}\" in decl block ID {}.",
                &var.name, decl_block_id
            )))
        }
    }

    /// This function assumes that the caller have made sure that the given `var`
    /// contains "AccessInstruction"s.
    fn get_var_ptr_access_instrs(&mut self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        let access_instrs = if let Some(ref access_instrs) = var.access_instrs {
            access_instrs
        } else {
            return Err(self.err(format!(
                "No access access instructions found for child var \"{}\".",
                &var.name
            )));
        };

        let struct_name = if let Some((root_var_name, decl_id)) = &var.root_struct_var {
            if let Some(struct_name) = self
                .analyze_context
                .get_struct_name(root_var_name.into(), *decl_id)?
            {
                struct_name
            } else {
                return Err(self.err(format!(
                    "Unable to get struct name for struct var {} in decl id {}.",
                    &root_var_name, decl_id
                )));
            }
        } else {
            // Set an empty string if this isn't a struct.
            String::new()
        };

        debug!(
            "Loading pointer for child var \"{}\" in `access_instrs`: {:?}",
            &var.name, &access_instrs
        );

        // Get the key containg the variable and the var declare block id.
        // It will be the "root" struct variable if this is a struct or it will
        // be "this" `var` if it isnät a struct.
        let key = if !struct_name.is_empty() {
            var.root_struct_var
                .clone()
                .expect("The existence of this val has already been proven.")
        } else {
            let block_id = self.cur_block_id;
            let decl_block_id = self
                .analyze_context
                .get_var_decl_scope(&var.name, block_id)?;
            (var.name.clone(), decl_block_id)
        };

        if let Some(base_ptr) = self.variables.get(&key) {
            let mut current_ptr = *base_ptr;

            // Iterate throw the hierachy of AccessInstruction to end up with
            // a pointer to the sought after variable.
            for access_instr in access_instrs.iter() {
                current_ptr = match access_instr {
                    AccessInstruction::StructMember(_, member_name, member_index_opt) => {
                        let member_index = if let Some(member_index) = member_index_opt {
                            member_index
                        } else {
                            return Err(self.err(format!(
                                "Member index not set for member \"{:?}\" in access_instr: {:?}.",
                                member_name, &access_instrs
                            )));
                        };

                        self.builder
                            .build_struct_gep(current_ptr, *member_index, "struct.gep")
                            .map_err(|_| {
                                self.err(format!(
                                    "Unable to gep from struct {:?}. Member name: {}, index {}.",
                                    &var.access_instrs, member_name, member_index
                                ))
                            })?
                    }
                    AccessInstruction::Deref => {
                        let tmp = self.builder.build_load(current_ptr, "access.instr.load");
                        if !tmp.is_pointer_value() {
                            return Err(self.err(format!(
                                "Deref of member {:#?} in struct_info {:#?} didn't return pointer.",
                                &access_instr, access_instrs
                            )));
                        }
                        tmp.into_pointer_value()
                    }
                    AccessInstruction::Address => current_ptr,
                    AccessInstruction::ArrayAccess(_) => {
                        panic!("TODO: ArrayAccess in struct member ptr load.")
                    }
                };
            }

            Ok(current_ptr)
        } else {
            Err(self.err(format!(
                "Unable to find ptr to variable with key: {:?}",
                &key
            )))
        }
    }

    // TODO: Implement logic to load both regular variables and struct members
    //       if they are const.
    fn get_const_value(&mut self, var: &Variable) -> CustomResult<BasicValueEnum<'ctx>> {
        let block_id = self.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Loading constant pointer. Key: {:?}", &key);

        if let Some(const_value) = self.constants.get(&key) {
            Ok(*const_value)
        } else {
            Err(self.err(format!(
                "Unable to find value for constant \"{}\" in decl block ID {}.",
                &var.name, decl_block_id
            )))
        }
    }

    pub(super) fn compile_type(&self, type_struct: &TypeStruct) -> CustomResult<AnyTypeEnum<'ctx>> {
        // TODO: What AddressSpace should be used?
        let address_space = AddressSpace::Generic;

        Ok(match &type_struct.t {
            Type::Pointer(ref ptr) => {
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
                return Err(self.err(
                    "TODO: Array. Need to calculate dimension and the return a \"ArrayType\""
                        .into(),
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
                    return Err(self.err(format!("Unable to find custom type: {}", ident)));
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

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new_backtrace(
            msg,
            CodeGenError {
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            },
            true,
        )
    }
}
