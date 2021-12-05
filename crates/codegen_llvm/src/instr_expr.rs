use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValue, AnyValueEnum, BasicValue},
    AddressSpace,
};
use log::debug;

use common::error::{LangError, LangErrorKind, LangResult};
use ir::{Data, DataIdx, ExprInstr, ExprInstrKind, Lit, Op, Type, Val, VarIdx};

use crate::{
    generator::CodeGen,
    util::{
        any_into_basic_value, is_const, to_data_name, to_global_name, to_local_name, to_param_name,
    },
};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    /// Compiles a expression. If this expression is a regular rvalue expression,
    /// it should be evaluated to a "value". If this expression is a lvalue,
    /// the "last" recurisve call to this function should return a pointer to
    /// the value so that the rhs can be assigned to it.
    ///
    /// Example:
    /// ```ignore
    /// var x = 3
    /// var y = 5
    /// x.* = y.*
    /// ```
    /// In this example the rvalue "y.*" needs to be evaulated to the literal 5
    /// so that it can be assigned to the lhs. The lvalue should be evalutaed
    /// to a pointer to the x memory which will be assigned the new value, it
    /// should NOT be evaluated to the literal 3.
    pub(super) fn compile_instr(&self, instr: &ExprInstr) -> LangResult<AnyValueEnum<'ctx>> {
        debug!("INSTR: {:?}", instr);
        match &instr.kind {
            ExprInstrKind::Lit(lit) => self.compile_lit(lit, &instr.val.1),
            ExprInstrKind::FnCall(name, args) => self.compile_fn_call(name, args),
            ExprInstrKind::FnPtrCall(var_val, args) => self.compile_fn_ptr_call(var_val, args),
            ExprInstrKind::FnPtr(name) => self.compile_fn_ptr(name),
            ExprInstrKind::VarAddress(var_idx) => self.compile_var_address(*var_idx),
            ExprInstrKind::DataAddress(data_idx) => self.compile_data_address(*data_idx),
            ExprInstrKind::Store(ptr, val) => self.compile_store(ptr, val),
            ExprInstrKind::Load(ptr) => self.compile_load(ptr),
            ExprInstrKind::Op(Op::BinOp {
                oper,
                lhs,
                rhs,
                signed,
            }) => self.compile_bin_op(oper, lhs, rhs, &instr.val, *signed),
            ExprInstrKind::Op(Op::UnOp { oper, value }) => self.compile_un_op(oper, value),
            ExprInstrKind::Phi(cases) => self.compile_phi(cases),
            ExprInstrKind::StructInit(name, args) => self.compile_struct_init(name, args),
            ExprInstrKind::ArrayInit(args) => self.compile_array_init(args),
        }
    }

    pub(super) fn compile_lit(&self, lit: &Lit, ir_type: &Type) -> LangResult<AnyValueEnum<'ctx>> {
        match lit {
            Lit::String(data_idx) => self.compile_lit_string(*data_idx),
            Lit::Char(ch) => Ok(self.context.i32_type().const_int(*ch as u64, false).into()),
            Lit::Bool(true) => Ok(self.context.bool_type().const_all_ones().into()),
            Lit::Bool(false) => Ok(self.context.bool_type().const_zero().into()),
            Lit::Integer(lit_int) => self.compile_lit_int(lit_int, ir_type),
            Lit::Float(lit_float) => self.compile_lit_float(lit_float, ir_type),
        }
    }

    fn compile_lit_string(&self, data_idx: DataIdx) -> LangResult<AnyValueEnum<'ctx>> {
        let lit_ptr_value = self.compile_data_address(data_idx)?;
        let lit_basic_value = any_into_basic_value(lit_ptr_value)?;

        let len = match self.ir_module.get_data(data_idx).unwrap() {
            Data::StringLit(lit) => lit.len(),
        };
        let lit_len = self.context.i32_type().const_int(len as u64, false);

        let view_type = self
            .module
            .get_struct_type("std::string::StringView")
            .unwrap();
        Ok(view_type
            .const_named_struct(&[lit_basic_value, lit_len.into()])
            .into())
    }

    fn compile_lit_int(&self, lit: &str, ir_type: &Type) -> LangResult<AnyValueEnum<'ctx>> {
        let radix = 10;
        Ok(match ir_type {
            Type::I8 => {
                let value = i8::from_str_radix(lit, radix)? as u64;
                self.context.i8_type().const_int(value, true).into()
            }
            Type::U8 => {
                let value = u8::from_str_radix(lit, radix)? as u64;
                self.context.i8_type().const_int(value, false).into()
            }
            Type::I16 => {
                let value = i16::from_str_radix(lit, radix)? as u64;
                self.context.i16_type().const_int(value, true).into()
            }
            Type::U16 => {
                let value = u16::from_str_radix(lit, radix)? as u64;
                self.context.i16_type().const_int(value, false).into()
            }
            Type::I32 => {
                let value = i32::from_str_radix(lit, radix)? as u64;
                self.context.i32_type().const_int(value, true).into()
            }
            Type::U32 => {
                let value = u32::from_str_radix(lit, radix)? as u64;
                self.context.i32_type().const_int(value, false).into()
            }
            Type::I64 => {
                let value = i64::from_str_radix(lit, radix)? as u64;
                self.context.i64_type().const_int(value, true).into()
            }
            Type::U64 => {
                let value = u64::from_str_radix(lit, radix)? as u64;
                self.context.i64_type().const_int(value, false).into()
            }
            Type::I128 => {
                let value = i128::from_str_radix(lit, radix)? as u64;
                self.context.i128_type().const_int(value, true).into()
            }
            Type::U128 => {
                let value = u128::from_str_radix(lit, radix)? as u64;
                self.context.i128_type().const_int(value, false).into()
            }
            _ => unreachable!("compile_lit_int -- lit: {}, type: {:?}", lit, ir_type),
        })
    }

    fn compile_lit_float(&self, lit: &str, ir_type: &Type) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ir_type {
            Type::F32 => self.context.f32_type().const_float_from_string(lit).into(),
            Type::F64 => self.context.f64_type().const_float_from_string(lit).into(),
            _ => unreachable!("compile_lit_float -- lit: {}, type: {:?}", lit, ir_type),
        })
    }

    fn compile_fn_call(&self, name: &str, args: &[Val]) -> LangResult<AnyValueEnum<'ctx>> {
        let mut compiled_args = Vec::with_capacity(args.len());
        for arg in args {
            let any_value = self.get_compiled_val(arg)?;
            compiled_args.push(any_into_basic_value(any_value)?);
        }

        let fn_val = if let Some(fn_val) = self.module.get_function(name) {
            fn_val
        } else {
            unreachable!("compile_fn_call -- unable to find fn with name: {}", name);
        };

        let call = self
            .builder
            .build_call(fn_val, &compiled_args, &format!("fn.call.{}", name));

        // Left == BasicValueEnum, Right == InstructionValue.
        // Will be right if the function returns "void", left otherwise.
        Ok(if let Some(ret_val) = call.try_as_basic_value().left() {
            ret_val.into()
        } else {
            self.context.i32_type().const_zero().into()
        })
    }

    fn compile_fn_ptr_call(&self, var_val: &Val, args: &[Val]) -> LangResult<AnyValueEnum<'ctx>> {
        let mut compiled_args = Vec::with_capacity(args.len());
        for arg in args {
            let any_value = self.get_compiled_val(arg)?;
            compiled_args.push(any_into_basic_value(any_value)?);
        }

        // The stored value in `var_val` should be a "VarAccess". This variable
        // should contain a pointer to a function type. So there should be two
        // "levels" of pointers.
        let compiled_val = self.get_compiled_val(var_val)?;
        if !compiled_val.is_pointer_value()
            || !compiled_val
                .into_pointer_value()
                .get_type()
                .get_element_type()
                .is_pointer_type()
            || !compiled_val
                .into_pointer_value()
                .get_type()
                .get_element_type()
                .into_pointer_type()
                .get_element_type()
                .is_function_type()
        {
            return Err(self.err(
                format!(
                    "Variable used for fn pointer call was not a \
                    \"pointer to a pointer contain a fn value\", was: {:#?}",
                    compiled_val
                ),
                None,
            ));
        }

        let var_ptr = compiled_val.into_pointer_value();
        let fn_ptr = self
            .builder
            .build_load(var_ptr, "fn.ptr.call.load")
            .into_pointer_value();

        let call = self
            .builder
            .build_call(fn_ptr, &compiled_args, "fn.ptr.call");

        // Left == BasicValueEnum, Right == InstructionValue.
        // Will be right if the function returns "void", left otherwise.
        Ok(if let Some(ret_val) = call.try_as_basic_value().left() {
            ret_val.into()
        } else {
            self.context.i32_type().const_zero().into()
        })
    }

    /// Need to make the FunctionValue sized so that it can be used as a regular
    /// value ex. as a parameter. This is done by creating a pointer to it and
    /// returning that pointer.
    fn compile_fn_ptr(&self, name: &str) -> LangResult<AnyValueEnum<'ctx>> {
        if let Some(fn_value) = self.module.get_function(name) {
            Ok(fn_value.as_global_value().as_pointer_value().into())
        } else {
            Err(self.err(
                format!(
                    "Unable to find function with name \"{}\" when compiling function pointer.",
                    name
                ),
                None,
            ))
        }
    }

    fn compile_var_address(&self, var_idx: VarIdx) -> LangResult<AnyValueEnum<'ctx>> {
        match var_idx {
            VarIdx::Global(global_idx) => {
                let global_name = to_global_name(global_idx.0);
                Ok(self
                    .module
                    .get_global(&global_name)
                    .unwrap()
                    .as_pointer_value()
                    .into())
            }
            VarIdx::Local(local_idx) => {
                let local_name = to_local_name(local_idx.0);
                if let Some(ptr) = self.compiled_locals.get(&local_name) {
                    Ok((*ptr).into())
                } else {
                    Err(self.err(
                        format!("Unable to find local with idx: {}", local_idx.0),
                        None,
                    ))
                }
            }
            VarIdx::Param(param_idx) => {
                let param_name = to_param_name(param_idx.0);
                if let Some(ptr) = self.compiled_params.get(&param_name) {
                    Ok((*ptr).into())
                } else {
                    Err(self.err(
                        format!("Unable to find param with idx: {}", param_idx.0),
                        None,
                    ))
                }
            }
        }
    }

    fn compile_data_address(&self, data_idx: DataIdx) -> LangResult<AnyValueEnum<'ctx>> {
        let data_name = to_data_name(data_idx.0);
        if let Some(global_value) = self.module.get_global(&data_name) {
            let i8_type = self.context.i8_type();
            Ok(global_value
                .as_pointer_value()
                .const_address_space_cast(i8_type.ptr_type(AddressSpace::Generic))
                .into())
        } else {
            unreachable!("Unable to get global data with idx: {}", data_idx.0);
        }
    }

    fn compile_store(&self, ptr: &Val, store_value: &Val) -> LangResult<AnyValueEnum<'ctx>> {
        let ptr_any_value = self.get_compiled_val(ptr)?;
        let store_any_value = self.get_compiled_val(store_value)?;
        if ptr_any_value.is_pointer_value() {
            let ptr_value = ptr_any_value.into_pointer_value();
            let basic_value = any_into_basic_value(store_any_value)?;
            Ok(self.builder.build_store(ptr_value, basic_value).into())
        } else {
            Err(LangError::new(
                format!("Val \"{:?}\" in compile_store not pointer", ptr),
                LangErrorKind::CompileError,
                None,
            ))
        }
    }

    fn compile_load(&self, ptr: &Val) -> LangResult<AnyValueEnum<'ctx>> {
        let ptr_any_value = self.get_compiled_val(ptr)?;

        if ptr_any_value.is_pointer_value() {
            let ptr_value = ptr_any_value.into_pointer_value();
            Ok(self.builder.build_load(ptr_value, "load").into())
        } else {
            // TODO: Currently there are situations where there will be load
            //       instructions on non-pointer values. This happens for example
            //       when there is a const struct that is never allocated on the
            //       stack and a member is accessed. This members will not be a
            //       pointer, but a load will be done anyways.
            //       This is something that should be handled in `ir_builder` in
            //       the future, but for now let just allow this edge-case here.
            //       We don't have enough information (ex. is_const) in `ir_builder`
            //       to figure handle it there atm.
            //       Example of code that causes this problem:
            //         `var x = InnerStruct { 3.0, 4.0 }.y`
            Ok(ptr_any_value)
            /*
            Err(LangError::new(
                format!("Val \"{:?}\" in compile_load not pointer.", ptr,),
                LangErrorKind::CompileError,
                None,
            ))
            */
        }
    }

    pub fn compile_struct_init(
        &self,
        struct_name: &str,
        args: &[Val],
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let struct_type = if let Some(struct_type) = self.module.get_struct_type(struct_name) {
            struct_type
        } else {
            return Err(self.err(
                format!("Unable to get struct with name \"{}\"", struct_name),
                None,
            ));
        };

        // Checks to see if the amount of arguments are different from the
        // amount of members.
        if args.len() != struct_type.count_fields() as usize {
            return Err(self.err(
                format!(
                    "Wrong amount of args given when init struct: {}. Expected: {}, got: {}",
                    struct_name,
                    struct_type.count_fields(),
                    args.len()
                ),
                None,
            ));
        }

        let mut any_args = Vec::with_capacity(args.len());
        let mut basic_args = Vec::with_capacity(args.len());
        for arg in args {
            let any_value = self.get_compiled_val(arg)?;
            let basic_value = any_into_basic_value(any_value)?;
            any_args.push(any_value);
            basic_args.push(basic_value);
        }

        if is_const(&any_args) {
            Ok(struct_type.const_named_struct(&basic_args).into())
        } else {
            // TODO: Can this be done in a better way? The stack storing/loading
            //       isn't really needed.
            let struct_ptr = self.builder.build_alloca(struct_type, "struct.init");

            for (i, arg_value) in basic_args.iter().enumerate() {
                let member_ptr = self
                    .builder
                    .build_struct_gep(struct_ptr, i as u32, "struct.init.gep")
                    .map_err(|_| {
                        self.err(
                            format!("Unable to GEP struct \"{}\" member {}.", struct_name, i),
                            None,
                        )
                    })?;
                self.builder.build_store(member_ptr, *arg_value);
            }

            Ok(self
                .builder
                .build_load(struct_ptr, "struct.init.load")
                .into())
        }
    }

    pub fn compile_array_init(&self, args: &[Val]) -> LangResult<AnyValueEnum<'ctx>> {
        if args.is_empty() {
            return Err(self.err("Array init with zero arguments.".into(), None));
        }

        // Compile all arguments(all values that will be set to initialize the
        // array members) and save the type of the first argument which will
        // be used to deduce the type of the whole array.
        let mut compiled_args = Vec::with_capacity(args.len());

        // Dummy arg_type to start with. If it is never set, it will never be used.
        let mut arg_type = self.context.i8_type().into();
        for arg in args.iter() {
            let any_value = self.get_compiled_val(arg)?;
            let basic_value = any_into_basic_value(any_value)?;
            arg_type = basic_value.get_type();
            compiled_args.push(basic_value);
        }

        let array_type = match arg_type {
            BasicTypeEnum::ArrayType(ty) => ty.array_type(args.len() as u32),
            BasicTypeEnum::FloatType(ty) => ty.array_type(args.len() as u32),
            BasicTypeEnum::IntType(ty) => ty.array_type(args.len() as u32),
            BasicTypeEnum::PointerType(ty) => ty.array_type(args.len() as u32),
            BasicTypeEnum::StructType(ty) => ty.array_type(args.len() as u32),
            BasicTypeEnum::VectorType(ty) => ty.array_type(args.len() as u32),
        };

        // TODO: Check so that the type of all arguments are the same (since
        //       they are all part of the same array).

        // TODO: What is the size? Is it the length or size in bytes?
        let sign_extend = false;
        let size = self
            .context
            .i32_type()
            .const_int(args.len() as u64, sign_extend);
        let array_ptr = self
            .builder
            .build_array_alloca(array_type, size, "array.init");

        // Since the array will always be allocated on the stack, when geping
        // the array one has to first index into the pointer. So a zero index
        // will always be added as a first index when geping arrays.
        let zero = self.context.i32_type().const_int(0, sign_extend);

        for (i, arg_value) in compiled_args.iter().enumerate() {
            let sign_extend = false;

            let index = self.context.i32_type().const_int(i as u64, sign_extend);
            let member_ptr = unsafe {
                self.builder
                    .build_gep(array_ptr, &[zero, index], "array.init.gep")
            };

            self.builder.build_store(member_ptr, *arg_value);
        }

        Ok(self.builder.build_load(array_ptr, "array.init.load").into())
    }

    pub fn compile_null(&self, ty: AnyTypeEnum<'ctx>) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ty {
            AnyTypeEnum::PointerType(ty) => ty.const_null().into(),

            AnyTypeEnum::ArrayType(ty) => ty.const_zero().into(),
            AnyTypeEnum::FloatType(ty) => ty.const_zero().into(),
            AnyTypeEnum::IntType(ty) => ty.const_zero().into(),
            AnyTypeEnum::StructType(ty) => ty.const_zero().into(),
            AnyTypeEnum::VectorType(ty) => ty.const_zero().into(),

            AnyTypeEnum::FunctionType(_) | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Tried to create null for unsupported type: {:#?}", ty),
                    None,
                ))
            }
        })
    }

    pub fn compile_phi(&self, cases: &[(String, Val)]) -> LangResult<AnyValueEnum<'ctx>> {
        let mut compiled_cases = Vec::with_capacity(cases.len());
        for (label, val) in cases {
            let compiled_value = any_into_basic_value(self.get_compiled_val(val)?)?;
            let compiled_block = if let Some(compiled_block) = self.compiled_blocks.get(label) {
                *compiled_block
            } else {
                return Err(self.err(
                    format!(
                        "Unable to find branch with label \"{}\" when compiling branch.",
                        label
                    ),
                    None,
                ));
            };

            compiled_cases.push((compiled_value, compiled_block))
        }

        // Verify that all cases have the same type. That type will be the type
        // of the `phi_val`.
        let first_type = compiled_cases
            .first()
            .map(|(value, _)| (*value).get_type())
            .unwrap();

        for (idx, (basic_value, _)) in compiled_cases.iter().enumerate() {
            if !self.is_same_base_type(first_type, basic_value.get_type()) {
                return Err(self.err(
                    format!(
                        "First val in phi-block and val at index {0} have different types. \
                        First type: {1:#?}, index {0} type: {2:#?}.",
                        idx,
                        first_type,
                        basic_value.get_type()
                    ),
                    None,
                ));
            }
        }

        let phi_val = self.builder.build_phi(first_type, "bool.and.phi");
        phi_val.add_incoming(
            &compiled_cases
                .iter()
                .map(|(value, block)| (value as &dyn BasicValue, *block))
                .collect::<Vec<_>>(),
        );

        Ok(phi_val.as_any_value_enum())
    }
}
