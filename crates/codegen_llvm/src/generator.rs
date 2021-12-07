use std::collections::{hash_map::Entry, HashMap};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{AnyValueEnum, BasicValue, FunctionValue, PointerValue},
    AddressSpace,
};

use common::{
    error::{
        LangError,
        LangErrorKind::{self, CodeGenError},
        LangResult,
    },
    file::FilePosition,
};
use ir::{Data, FuncVisibility, Type, Val, DUMMY_VAL};

use crate::util::{
    any_into_basic_type, any_into_basic_value, to_data_name, to_global_name, to_local_name,
    to_param_name,
};

pub(super) struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub target_machine: &'a TargetMachine,

    /// The IR representation of the code that will be used to generated the
    /// LLVM code.
    pub ir_module: ir::Module,

    /// Will contain the values that have been compiled in the function that is
    /// currently being built/compiled. The `usize` will be the unique `ir::Val`
    /// value that is given to every evaluated expression.
    /// The Strings in the `params` and `locals` maps are the names of the
    /// variables specified in the LLVM IR.
    /// These map will be reset for every function that is being traversed.
    pub(super) compiled_vals: HashMap<usize, AnyValueEnum<'ctx>>,
    pub(super) compiled_params: HashMap<String, PointerValue<'ctx>>,
    pub(super) compiled_locals: HashMap<String, PointerValue<'ctx>>,

    /// Will contain basic blocks that have been compiled for the function that
    /// is currently being built/compiled.
    /// This map will be reset for every function that is being traversed.
    pub(super) compiled_blocks: HashMap<String, BasicBlock<'ctx>>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        target_machine: &'a TargetMachine,
        ir_module: ir::Module,
    ) -> Self {
        Self {
            context,
            builder,
            module,
            target_machine,
            ir_module,
            compiled_vals: HashMap::default(),
            compiled_params: HashMap::default(),
            compiled_locals: HashMap::default(),
            compiled_blocks: HashMap::default(),
        }
    }

    pub(super) fn compile(&mut self) -> LangResult<()> {
        self.compile_structs()?;
        self.compile_data();
        self.compile_globals()?;
        self.compile_fn_decls()?;
        self.compile_fn_bodies()
    }

    fn compile_structs(&mut self) -> LangResult<()> {
        for name in &self.ir_module.structs_order {
            let struct_type = self.context.opaque_struct_type(name);

            // If the struct has members defined (i.e. not a externally declared
            // struct), add information about its members type.
            if let Some(members) = self.ir_module.structs.get(name).unwrap() {
                let mut member_types = Vec::with_capacity(members.len());
                for member in members {
                    let any_type = self.compile_type(member)?;
                    member_types.push(any_into_basic_type(any_type)?);
                }

                let packed = false;
                struct_type.set_body(member_types.as_ref(), packed);
            }
        }
        Ok(())
    }

    fn compile_data(&mut self) {
        for (idx, data) in self.ir_module.data.iter().enumerate() {
            match data {
                Data::StringLit(lit) => self.compile_data_string(lit, idx),
            }
        }
    }

    fn compile_data_string(&self, lit: &str, idx: usize) {
        let data_name = to_data_name(idx);
        let i8_type = self.context.i8_type();

        let mut bytes = Vec::with_capacity(lit.len());
        for byte in lit.as_bytes() {
            bytes.push(i8_type.const_int(*byte as u64, false));
        }

        let arr_val = i8_type.const_array(&bytes);
        let arr_type = arr_val.get_type();

        let global_val = self
            .module
            .add_global(arr_type, Some(AddressSpace::Const), &data_name);
        global_val.set_initializer(&arr_val.as_basic_value_enum());
    }

    fn compile_globals(&mut self) -> LangResult<()> {
        // TODO: Handle const globals.
        let address_space = AddressSpace::Generic;
        for (idx, (ir_type, lit_opt)) in self.ir_module.global_vars.iter().enumerate() {
            let global_name = to_global_name(idx);
            let any_type = self.compile_type(ir_type)?;

            let global_val = self.module.add_global(
                any_into_basic_type(any_type)?,
                Some(address_space),
                &global_name,
            );

            let init_value = if let Some(lit) = lit_opt {
                self.compile_lit(lit, ir_type)?
            } else {
                self.compile_null(any_type)?
            };

            global_val.set_initializer(&any_into_basic_value(init_value)?);
        }
        Ok(())
    }

    fn compile_fn_decls(&mut self) -> LangResult<()> {
        for (name, func) in &self.ir_module.funcs {
            let mut params = Vec::with_capacity(func.params.len());
            for param in &func.params {
                let any_type = self.compile_type(param)?;
                params.push(any_into_basic_type(any_type)?);
            }

            let fn_type = match self.compile_type(&func.ret_type)? {
                AnyTypeEnum::ArrayType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
                AnyTypeEnum::FloatType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
                AnyTypeEnum::FunctionType(ty) => ty,
                AnyTypeEnum::IntType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
                AnyTypeEnum::PointerType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
                AnyTypeEnum::StructType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
                AnyTypeEnum::VectorType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
                AnyTypeEnum::VoidType(ty) => ty.fn_type(params.as_slice(), func.is_var_arg),
            };

            let linkage = match func.visibility {
                FuncVisibility::Export | FuncVisibility::Import => Linkage::External,
                // TODO: Linkage::Private
                FuncVisibility::None => Linkage::External,
            };

            self.module.add_function(name, fn_type, Some(linkage));
        }
        Ok(())
    }

    fn compile_fn_bodies(&mut self) -> LangResult<()> {
        // TODO: Skip the need for clone here.
        for (name, func) in self.ir_module.funcs.clone() {
            self.compiled_vals.clear();
            self.compiled_params.clear();
            self.compiled_locals.clear();
            self.compiled_blocks.clear();

            // Imported functions should have no bodies, they are externaly declared.
            if matches!(func.visibility, FuncVisibility::Import) {
                continue;
            }

            let fn_val = if let Some(fn_val) = self.module.get_function(&name) {
                fn_val
            } else {
                return Err(self.err(
                    format!("Unable to find LLVM function with name \"{}\".", name),
                    None,
                ));
            };

            // Parameters and locals are compiled into the `entry` block. The
            // end instruction for the entry block is compiled at the end of
            // this for-loop.
            let entry_block = self
                .context
                .append_basic_block(fn_val, &format!("{}.entry", &name));
            self.builder.position_at_end(entry_block);

            for (idx, (param_ir_type, param_value)) in
                func.params.iter().zip(fn_val.get_params()).enumerate()
            {
                let param_name = to_param_name(idx);
                let param_ptr = self.alloc_var(&param_name, param_ir_type)?;
                self.builder.build_store(param_ptr, param_value);
                self.compiled_params.insert(param_name, param_ptr);
            }

            for (idx, local) in func.locals.iter().enumerate() {
                let local_name = to_local_name(idx);
                let local_ptr = self.alloc_var(&local_name, local)?;
                self.compiled_locals.insert(local_name, local_ptr);
            }

            for ir_block in &func.basic_blocks {
                let llvm_block = self.compile_expr_instrs(fn_val, ir_block)?;
                match self.compiled_blocks.entry(ir_block.label.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert(llvm_block);
                    }
                    Entry::Occupied(_) => {
                        return Err(self.err(
                            format!(
                                "Found multiple blocks in function \"{}\" with label: {}",
                                name, ir_block.label
                            ),
                            None,
                        ));
                    }
                }
            }

            // Since `EndInstr`s might branch to other basic blocks that can be
            // declared before/after the current block, we compile all end
            // instructions AFTER all basic blocks have been created in the
            // logic above.
            for basic_block in &func.basic_blocks {
                let compiled_block = *self.compiled_blocks.get(&basic_block.label).unwrap();
                self.builder.position_at_end(compiled_block);

                if let Some(end_instr) = &basic_block.end_instrs {
                    self.compile_end_instr(end_instr)?;
                } else {
                    return Err(self.err(
                        format!(
                            "No end instruction found for block with label: {}",
                            basic_block.label
                        ),
                        None,
                    ));
                }
            }

            self.builder.position_at_end(entry_block);

            // The entry block will be branch to the first "actual" block. If no
            // "actual" block exists, the entry block will return to the caller
            // with a zero/null/void value.
            if let Some(ir_block) = func.basic_blocks.first() {
                let llvm_block = self.compiled_blocks.get(&ir_block.label).unwrap();
                self.builder.build_unconditional_branch(*llvm_block);
            } else if let Some(ret_type) = fn_val.get_type().get_return_type() {
                let any_value = self.compile_null(ret_type.as_any_type_enum())?;
                let basic_value = any_into_basic_value(any_value)?;
                self.builder.build_return(Some(&basic_value));
            } else {
                // void
                self.builder.build_return(None);
            }
        }

        Ok(())
    }

    fn compile_expr_instrs(
        &mut self,
        fn_val: FunctionValue,
        ir_block: &ir::BasicBlock,
    ) -> LangResult<BasicBlock<'ctx>> {
        let llvm_block = self.context.append_basic_block(fn_val, &ir_block.label);
        self.builder.position_at_end(llvm_block);

        for instr in &ir_block.instrs {
            let any_value = self.compile_instr(instr)?;
            self.compiled_vals.insert(instr.val.0, any_value);
        }

        Ok(llvm_block)
    }

    pub(super) fn compile_type(&self, ir_type: &Type) -> LangResult<AnyTypeEnum<'ctx>> {
        // TODO: What AddressSpace should be used?
        let address_space = AddressSpace::Generic;
        Ok(match ir_type {
            Type::Adt(struct_name) => self.module.get_struct_type(struct_name).unwrap().into(),
            Type::Func(func) => todo!(),
            Type::Pointer(inner_ir_type) => {
                let inner_type = self.compile_type(inner_ir_type)?;
                any_into_basic_type(inner_type)?
                    .ptr_type(address_space)
                    .into()
            }
            Type::Array(inner_ir_type, Some(size)) => {
                let inner_type = self.compile_type(inner_ir_type)?;
                any_into_basic_type(inner_type)?
                    .array_type(*size as u32)
                    .into()
            }
            Type::Array(inner_ir_type, None) => {
                todo!("compile_type -- Slice")
            }
            Type::FuncPointer(param_ir_types, ret_ir_type) => {
                let mut param_types = Vec::with_capacity(param_ir_types.len());
                for ir_type in param_ir_types {
                    let any_type = self.compile_type(ir_type)?;
                    param_types.push(any_into_basic_type(any_type)?)
                }

                let ret_type = self.compile_type(ret_ir_type)?;
                let is_var_args = false;
                let fn_type = match ret_type {
                    AnyTypeEnum::ArrayType(ty) => ty.fn_type(&param_types, is_var_args),
                    AnyTypeEnum::FloatType(ty) => ty.fn_type(&param_types, is_var_args),
                    AnyTypeEnum::FunctionType(ty) => ty,
                    AnyTypeEnum::IntType(ty) => ty.fn_type(&param_types, is_var_args),
                    AnyTypeEnum::PointerType(ty) => ty.fn_type(&param_types, is_var_args),
                    AnyTypeEnum::StructType(ty) => ty.fn_type(&param_types, is_var_args),
                    AnyTypeEnum::VectorType(ty) => ty.fn_type(&param_types, is_var_args),
                    AnyTypeEnum::VoidType(ty) => ty.fn_type(&param_types, is_var_args),
                };

                // Need to make the FunctionType sized. This is done by getting
                // a pointer to it. This value can then be used as ex. an argument.
                fn_type.ptr_type(address_space).into()
            }
            Type::Void => self.context.void_type().into(),
            Type::Char => self.context.i32_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::I8 | Type::U8 => self.context.i8_type().into(),
            Type::I16 | Type::U16 => self.context.i16_type().into(),
            Type::I32 | Type::U32 => self.context.i32_type().into(),
            Type::F32 => self.context.f32_type().into(),
            Type::I64 | Type::U64 => self.context.i64_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::I128 | Type::U128 => self.context.i128_type().into(),
        })
    }

    fn alloc_var(&self, name: &str, ir_type: &Type) -> LangResult<PointerValue<'ctx>> {
        Ok(match self.compile_type(ir_type)? {
            AnyTypeEnum::ArrayType(ty) => {
                let sign_extend = false;
                let dim = self
                    .context
                    .i64_type()
                    .const_int(ty.len() as u64, sign_extend);
                self.builder.build_array_alloca(ty, dim, name)
            }
            AnyTypeEnum::FloatType(ty) => self.builder.build_alloca(ty, name),
            AnyTypeEnum::IntType(ty) => self.builder.build_alloca(ty, name),
            AnyTypeEnum::PointerType(ty) => self.builder.build_alloca(ty, name),
            AnyTypeEnum::StructType(ty) => self.builder.build_alloca(ty, name),
            AnyTypeEnum::VectorType(ty) => self.builder.build_alloca(ty, name),
            AnyTypeEnum::FunctionType(_) => {
                return Err(self.err("Tried to alloc function.".into(), None));
            }
            AnyTypeEnum::VoidType(_) => {
                return Err(self.err("Tried to alloc void type.".into(), None));
            }
        })
    }

    pub(crate) fn get_compiled_val(&self, val: &Val) -> LangResult<AnyValueEnum<'ctx>> {
        // TODO: Better way to handle dummy values?
        if val == &DUMMY_VAL {
            return Ok(self.context.struct_type(&[], false).const_zero().into());
        }

        self.compiled_vals.get(&val.0).copied().ok_or_else(|| {
            LangError::new(
                format!("Unable to get compiled value for val: {:?}", val),
                LangErrorKind::CompileError,
                None,
            )
        })
    }

    /// Returns true if the types have the same "base type". Ex. if both values
    /// are int, float, pointer etc.
    pub(crate) fn is_same_base_type(
        &self,
        left_type: BasicTypeEnum<'ctx>,
        right_type: BasicTypeEnum<'ctx>,
    ) -> bool {
        match (left_type, right_type) {
            // TODO: Should we care about what a pointer points to or the bit-size
            //       of ints/floats in this function.
            (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(_))
            | (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_))
            | (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_)) => true,

            (BasicTypeEnum::ArrayType(left_type_i), BasicTypeEnum::ArrayType(right_type_i)) => self
                .is_same_base_type(
                    left_type_i.get_element_type(),
                    right_type_i.get_element_type(),
                ),

            (BasicTypeEnum::VectorType(left_type_i), BasicTypeEnum::VectorType(right_type_i)) => {
                self.is_same_base_type(
                    left_type_i.get_element_type(),
                    right_type_i.get_element_type(),
                )
            }

            (BasicTypeEnum::StructType(left_type_i), BasicTypeEnum::StructType(right_type_i)) => {
                if left_type_i.count_fields() != right_type_i.count_fields() {
                    return false;
                }

                let mut is_same_struct = true;
                for (left_type_mem, right_type_mem) in left_type_i
                    .get_field_types()
                    .into_iter()
                    .zip(right_type_i.get_field_types())
                {
                    if !self.is_same_base_type(left_type_mem, right_type_mem) {
                        is_same_struct = false;
                        break;
                    }
                }
                is_same_struct
            }

            _ => false,
        }
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String, file_pos: Option<FilePosition>) -> LangError {
        LangError::new(msg, CodeGenError, file_pos)
    }
}
