use either::Either;
use inkwell::{
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{AnyValueEnum, FloatValue, IntValue, PointerValue},
    AddressSpace,
};
use log::debug;

use common::{
    error::LangResult,
    file::FilePosition,
    token::{
        block::AdtKind,
        expr::{AdtInit, ArrayInit, Expr, FnCall},
        lit::{Lit, StringType},
    },
    ty::{get::get_inner, inner_ty::InnerTy, to_string::to_string_path, type_id::TypeId},
};

use crate::generator::CodeGen;

#[derive(Debug, Copy, Clone)]
pub enum ExprTy {
    LValue,
    RValue,
}

impl<'a, 'b, 'ctx> CodeGen<'a, 'b, 'ctx> {
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
    pub(super) fn compile_expr(
        &mut self,
        expr: &mut Expr,
        expr_ty: ExprTy,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let file_pos = expr.file_pos().cloned();

        debug!(
            "compile_expr -- expr_ty: {:?} expr: {:#?}, file_pos: {:#?}",
            expr_ty, expr, file_pos
        );

        let any_value = match expr {
            Expr::Lit(lit, type_id_opt, ..) => self.compile_lit(lit, type_id_opt, file_pos),
            Expr::FnCall(fn_call) => self.compile_fn_call(fn_call),
            Expr::FnPtr(..) => self.compile_fn_ptr(expr),
            Expr::BuiltInCall(built_in_call) => self.compile_built_in_call(built_in_call),
            Expr::Op(op) => self.compile_op(op, expr_ty, file_pos),
            Expr::AdtInit(adt_init) => match adt_init.kind {
                AdtKind::Struct => self.compile_struct_init(adt_init),
                AdtKind::Union => self.compile_union_init(adt_init),
                _ => panic!("Tried to compile AdtInit for kind: {:?}", adt_init.kind),
            },
            Expr::ArrayInit(array_init) => self.compile_array_init(array_init),
            Expr::Type(type_id, ..) => {
                // TODO: Does something need to be done here? Does a proper value
                //       need to be returned? For now just return a dummy value.
                // Returns a "null" value of the given type. For now, this lets
                // one "store types" in variables in a hacky way. In the future
                // it should be possible to store types in variables without
                // having to init them with a value.
                Ok(match self.compile_type(*type_id, file_pos)? {
                    AnyTypeEnum::ArrayType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::FloatType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::IntType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::PointerType(ty) => ty.const_null().into(),
                    AnyTypeEnum::StructType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::VectorType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::FunctionType(ty) => panic!("TODO: compile_exr function type?"),
                    AnyTypeEnum::VoidType(ty) => panic!("TODO: compile_exr void type?"),
                })
            }
            Expr::Var(var) => match expr_ty {
                ExprTy::LValue => self.get_var_ptr(&var).map(|x| x.into()),
                ExprTy::RValue => self.compile_var_load(&var).map(|x| x.into()),
            },
            Expr::Block(_, _) => todo!("codegen: Expr::Block"),
        }?;

        self.prev_expr = Some(any_value);
        Ok(any_value)
    }

    pub fn compile_lit(
        &mut self,
        lit: &Lit,
        type_id_opt: &Option<TypeId>,
        file_pos: Option<FilePosition>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        match lit {
            Lit::String(str_lit, string_type) => {
                self.compile_lit_string(str_lit, string_type, file_pos)
            }

            Lit::Char(char_lit) => {
                if char_lit.chars().count() == 1 {
                    let ch = char_lit.chars().next().unwrap();
                    Ok(AnyValueEnum::IntValue(
                        self.context.i32_type().const_int(ch as u64, false),
                    ))
                } else {
                    Err(self.err("Char literal isn't a single character.".into(), file_pos))
                }
            }

            Lit::Bool(true) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_all_ones(),
            )),
            Lit::Bool(false) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_zero(),
            )),

            Lit::Integer(int_lit, radix) => Ok(AnyValueEnum::IntValue(self.compile_lit_int(
                int_lit,
                type_id_opt,
                *radix,
                file_pos,
            )?)),

            Lit::Float(float_lit) => Ok(AnyValueEnum::FloatValue(self.compile_lit_float(
                float_lit,
                type_id_opt,
                file_pos,
            )?)),
        }
    }

    fn compile_lit_string(
        &mut self,
        str_lit: &str,
        string_type: &StringType,
        file_pos: Option<FilePosition>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // Early return if this is a null-terminated C string. No need to fetch
        // structs and calculate len.
        if matches!(string_type, StringType::C) {
            return Ok(self.compile_lit_string_global(str_lit, true).into());
        }

        let ptr_arg = self.compile_lit_string_global(str_lit, false);
        let len_arg = self
            .context
            .i32_type()
            .const_int(str_lit.len() as u64, false);

        let struct_path = match string_type {
            StringType::Regular => ["std".into(), "StringView".into()].into(),
            StringType::F | StringType::S => ["std".into(), "String".into()].into(),
            StringType::C => unreachable!(),
        };
        let struct_name = to_string_path(&self.analyze_ctx.ty_env.lock().unwrap(), &struct_path);

        let struct_type = if let Some(struct_type) = self.module.get_struct_type(&struct_name) {
            struct_type
        } else {
            return Err(self.err(
                format!(
                    "Unable to get struct \"{}\" when compiling lit string.",
                    struct_name
                ),
                file_pos,
            ));
        };

        Ok(struct_type
            .const_named_struct(&[ptr_arg.into(), len_arg.into()])
            .into())
    }

    /// Compiles the given string literal into an u8 array and adds its as
    /// a new LLVM global. The function returns a pointer to this global.
    ///
    /// If `null_terminated` is set to true, the size of the array will be
    /// `lit.len() + 1` and the last item in the array will be set to zero.
    fn compile_lit_string_global(
        &mut self,
        lit: &str,
        null_terminated: bool,
    ) -> PointerValue<'ctx> {
        // Use a random name. This name will never be used, but just need to
        // make sure that there aren't multiple globals with the same name.
        let name = format!("str.lit.{}", rand::random::<u32>());
        let i8_type = self.context.i8_type();

        let len = if null_terminated {
            lit.len() + 1
        } else {
            lit.len()
        };

        let mut bytes = Vec::with_capacity(len);
        for byte in lit.as_bytes() {
            bytes.push(i8_type.const_int(*byte as u64, false));
        }

        if null_terminated {
            bytes.push(i8_type.const_zero());
        }

        let arr_val = i8_type.const_array(&bytes);
        let arr_type = arr_val.get_type();

        let global_val = self
            .module
            .add_global(arr_type, Some(AddressSpace::Const), &name);
        global_val.set_initializer(&arr_val);

        global_val
            .as_pointer_value()
            .const_address_space_cast(i8_type.ptr_type(AddressSpace::Generic))
    }

    // TODO: Better conversion of the integer literal.
    // TODO: i32 as default.
    fn compile_lit_int(
        &mut self,
        lit: &str,
        type_id_opt: &Option<TypeId>,
        radix: u32,
        file_pos: Option<FilePosition>,
    ) -> LangResult<IntValue<'ctx>> {
        // TODO: Where should the integer literal conversion be made?
        let inner_ty = if let Some(type_id) = type_id_opt {
            let ty_env_guard = self.analyze_ctx.ty_env.lock().unwrap();
            let fwd_type_id = ty_env_guard.forwarded(*type_id);
            get_inner(&ty_env_guard, fwd_type_id)?.clone()
        } else {
            InnerTy::default_int()
        };

        if !inner_ty.is_signed() && lit.starts_with('-') {
            return Err(self.err(
                format!(
                    "Tried to assign negative value to unsigned integer. Type: {:?}, value: {}",
                    inner_ty, lit
                ),
                file_pos,
            ));
        }

        Ok(match inner_ty {
            InnerTy::I8 => {
                let val = i8::from_str_radix(lit, radix)? as u64;
                self.context.i8_type().const_int(val, true)
            }
            InnerTy::U8 => {
                let val = u8::from_str_radix(lit, radix)? as u64;
                self.context.i8_type().const_int(val, false)
            }
            InnerTy::I16 => {
                let val = i16::from_str_radix(lit, radix)? as u64;
                self.context.i16_type().const_int(val, true)
            }
            InnerTy::U16 => {
                let val = u16::from_str_radix(lit, radix)? as u64;
                self.context.i16_type().const_int(val, false)
            }
            InnerTy::I32 => {
                let val = i32::from_str_radix(lit, radix)? as u64;
                self.context.i32_type().const_int(val, true)
            }
            InnerTy::U32 => {
                let val = u32::from_str_radix(lit, radix)? as u64;
                self.context.i32_type().const_int(val, false)
            }
            InnerTy::I64 => {
                let val = i64::from_str_radix(lit, radix)? as u64;
                self.context.i64_type().const_int(val, true)
            }
            InnerTy::U64 => {
                let val = u64::from_str_radix(lit, radix)? as u64;
                self.context.i64_type().const_int(val, false)
            }
            InnerTy::I128 => {
                let val = i128::from_str_radix(lit, radix)? as u64;
                self.context.i128_type().const_int(val, true)
            }
            InnerTy::U128 => {
                let val = u128::from_str_radix(lit, radix)? as u64;
                self.context.i128_type().const_int(val, false)
            }
            _ => {
                return Err(self.err(
                    format!(
                        "Invalid literal integer type. Type ID: {:?}, inner_ty: {:?}",
                        type_id_opt, inner_ty
                    ),
                    file_pos,
                ))
            }
        })
    }

    // TODO: Better conversion of the float literal.
    fn compile_lit_float(
        &mut self,
        lit: &str,
        type_id_opt: &Option<TypeId>,
        file_pos: Option<FilePosition>,
    ) -> LangResult<FloatValue<'ctx>> {
        let inner_ty = if let Some(type_id) = type_id_opt {
            let ty_env_guard = self.analyze_ctx.ty_env.lock().unwrap();
            let fwd_type_id = ty_env_guard.forwarded(*type_id);
            get_inner(&ty_env_guard, fwd_type_id)?.clone()
        } else {
            InnerTy::default_float()
        };

        Ok(match inner_ty {
            InnerTy::F32 => self.context.f32_type().const_float(lit.parse()?),
            InnerTy::F64 => self.context.f64_type().const_float(lit.parse()?),
            _ => {
                return Err(self.err(
                    format!("Invalid literal float type: {:?}", type_id_opt),
                    file_pos,
                ))
            }
        })
    }

    // TODO: Temporarily treats functions return void as return i32 "0".
    //       Should make a custom value ex rusts "()" instead.
    /// Generates a function call. Returns the return value of the compiled
    /// function.
    pub fn compile_fn_call(&mut self, fn_call: &mut FnCall) -> LangResult<AnyValueEnum<'ctx>> {
        let mut args = Vec::with_capacity(fn_call.arguments.len());
        for arg in &mut fn_call.arguments {
            let any_value = self.compile_expr(&mut arg.value, ExprTy::RValue)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            args.push(basic_value);
        }

        // TODO: Implement check for `is_fn_ptr_call`s arg/param count as well.
        let ty_env_guard = self.analyze_ctx.ty_env.lock().unwrap();

        let fn_ptr = if fn_call.is_fn_ptr_call {
            let var_name = &fn_call.name;
            let decl_id = self
                .analyze_ctx
                .ast_ctx
                .get_var_decl_scope(var_name, self.cur_block_id)?;

            let key = (var_name.clone(), decl_id);
            match self.variables.get(&key) {
                Some(ptr_value) => {
                    let fn_ptr = self.builder.build_load(*ptr_value, "fn.ptr.load");
                    if !fn_ptr.is_pointer_value() {
                        unreachable!("ptr_value: {:#?}", ptr_value);
                    } else if let AnyTypeEnum::FunctionType(..) =
                        fn_ptr.into_pointer_value().get_type().get_element_type()
                    {
                        // Happy path, `fn_ptr` is a pointer to a FunctionValue
                        // as expected.
                        Either::Right(fn_ptr.into_pointer_value())
                    } else {
                        unreachable!("ptr_value: {:#?}", ptr_value);
                    }
                }
                None => {
                    return Err(self.err(
                        format!("Unable to find function pointer with key: {:#?}).", key),
                        fn_call.file_pos,
                    ));
                }
            }
        } else if let Some(fn_value) = self.module.get_function(&fn_call.full_name(&ty_env_guard)?)
        {
            // Checks to see if the arguments are fewer that parameters. The
            // arguments are allowed to be greater than parameters since variadic
            // functions are supported to be compatible with C code.
            if fn_call.arguments.len() < fn_value.count_params() as usize {
                return Err(self.err(
                    format!(
                        "Wrong amount of args given when calling func \"{}\". Expected: {}, got: {}",
                        &fn_call.full_name(&ty_env_guard)?,
                        fn_value.count_params(),
                        fn_call.arguments.len()
                    ),
                    fn_call.file_pos,
                ));
            }

            Either::Left(fn_value)
        } else {
            let mut fns = Vec::default();
            if let Some(first_fn_val) = self.module.get_first_function() {
                let fn_name = first_fn_val.get_name().to_str().unwrap();
                fns.push(format!(" * {}", fn_name.to_string()));

                let mut prev_fn_val = first_fn_val;
                while let Some(fn_val) = prev_fn_val.get_next_function() {
                    let fn_name = fn_val.get_name().to_str().unwrap();
                    fns.push(format!(" * {}", fn_name.to_string()));
                    prev_fn_val = fn_val;
                }
            }

            return Err(self.err(
                format!(
                    "Unable to find function with name \"{}\" to call (full name: {:#?}). \
                    List of all declared functions: {}",
                    &fn_call.name,
                    &fn_call.full_name(&ty_env_guard),
                    fns.join("\n"),
                ),
                fn_call.file_pos,
            ));
        };

        let call = self.builder.build_call(fn_ptr, args.as_slice(), "fn.call");

        // Left == BasicValueEnum, Right == InstructionValue.
        // Will be right if the function returns "void", left otherwise.
        Ok(if let Some(ret_val) = call.try_as_basic_value().left() {
            ret_val.into()
        } else {
            self.context.i32_type().const_zero().into()
        })
    }

    /// The FunctionValue will "converted" into a PointerValue to make it sized.
    /// This will be done by first converting it into its GlobalValue.
    pub fn compile_fn_ptr(&mut self, expr: &mut Expr) -> LangResult<AnyValueEnum<'ctx>> {
        let fn_ptr = if let Expr::FnPtr(fn_ptr) = expr {
            fn_ptr
        } else {
            unreachable!("expr not fn_ptr: {:#?}", expr);
        };

        let partial_path =
            fn_ptr
                .module
                .clone_push(&fn_ptr.name, fn_ptr.generics.as_ref(), fn_ptr.file_pos);
        let full_path = self.analyze_ctx.ast_ctx.calculate_fn_full_path(
            &self.analyze_ctx.ty_env.lock().unwrap(),
            &partial_path,
            self.cur_block_id,
        )?;

        if let Some(fn_value) = self.module.get_function(&to_string_path(
            &self.analyze_ctx.ty_env.lock().unwrap(),
            &full_path,
        )) {
            let fn_ptr = fn_value.as_global_value().as_pointer_value();
            Ok(fn_ptr.into())
        } else {
            Err(self.err(
                format!(
                    "Unable to find function with full name {} (compiling fn pointer).",
                    to_string_path(&self.analyze_ctx.ty_env.lock().unwrap(), &full_path)
                ),
                fn_ptr.file_pos.to_owned(),
            ))
        }
    }

    /// Generates a struct creation/initialization.
    pub fn compile_struct_init(
        &mut self,
        struct_init: &mut AdtInit,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let full_name = struct_init.full_name(&self.analyze_ctx.ty_env.lock().unwrap())?;

        let struct_type = if let Some(inner) = self.module.get_struct_type(&full_name) {
            inner
        } else {
            return Err(self.err(
                format!(
                    "Unable to get struct with name \"{}\". Struct init: {:#?}",
                    full_name, struct_init
                ),
                struct_init.file_pos,
            ));
        };

        // Checks to see if the amount of arguments are different from the
        // amount of members.
        if struct_init.arguments.len() != struct_type.count_fields() as usize {
            return Err(self.err(
                format!(
                    "Wrong amount of args given when init struct: {}. Expected: {}, got: {}",
                    &struct_init.name,
                    struct_type.count_fields(),
                    struct_init.arguments.len()
                ),
                struct_init.file_pos,
            ));
        }

        // Compile all arguments(all values that will be set to initialize the
        // struct members).
        let mut args = Vec::with_capacity(struct_init.arguments.len());
        for arg in &mut struct_init.arguments {
            let any_value = self.compile_expr(&mut arg.value, ExprTy::RValue)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            args.push(basic_value);
        }

        let is_const =
            CodeGen::is_const(&args.clone().iter().map(|x| (*x).into()).collect::<Vec<_>>());

        if is_const {
            Ok(struct_type.const_named_struct(&args).into())
        } else {
            // TODO: Can this be done in a better way? The stack storring/loading
            //       isn't really needed.
            let struct_ptr = self.builder.build_alloca(struct_type, "struct.init");

            for (i, arg_value) in args.iter().enumerate() {
                let member_ptr = self
                    .builder
                    .build_struct_gep(struct_ptr, i as u32, "struct.init.gep")
                    .map_err(|_| {
                        self.err(
                            format!(
                                "Unable to GEP struct \"{}\" member {}.",
                                &struct_init.name, i
                            ),
                            struct_init.file_pos,
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

    /// Generates a union creation/initialization.
    pub fn compile_union_init(
        &mut self,
        union_init: &mut AdtInit,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let partial_path = union_init.module.clone_push(
            &union_init.name,
            union_init.generics.as_ref(),
            union_init.file_pos,
        );
        let full_path = self.analyze_ctx.ast_ctx.calculate_adt_full_path(
            &self.analyze_ctx.ty_env.lock().unwrap(),
            &partial_path,
            self.cur_block_id,
        )?;

        let union_type = if let Some(inner) = self.module.get_struct_type(&to_string_path(
            &self.analyze_ctx.ty_env.lock().unwrap(),
            &full_path,
        )) {
            inner
        } else {
            return Err(self.err(
                format!(
                    "Unable to get union with name \"{}\". Union init: {:#?}",
                    to_string_path(&self.analyze_ctx.ty_env.lock().unwrap(), &full_path),
                    union_init
                ),
                union_init.file_pos,
            ));
        };

        let arg = union_init.arguments.first_mut().unwrap();
        let any_value = self.compile_expr(&mut arg.value, ExprTy::RValue)?;
        let basic_value = CodeGen::any_into_basic_value(any_value)?;

        let tag_idx = self.analyze_ctx.ast_ctx.get_adt_member_index(
            &self.analyze_ctx.ty_env.lock().unwrap(),
            &full_path,
            &arg.name.as_ref().unwrap(),
        )?;
        let tag = self.context.i8_type().const_int(tag_idx, false);

        let val_ptr = self
            .builder
            .build_alloca(basic_value.get_type(), "union.init.val");
        self.builder.build_store(val_ptr, basic_value);

        // TODO: const.

        let union_ptr = self.builder.build_alloca(union_type, "union.init");
        let tag_ptr = self
            .builder
            .build_struct_gep(union_ptr, 0, "union.init.tag.gep")
            .map_err(|_| {
                self.err(
                    format!("Unable to GEP union \"{}\" tag.", &union_init.name),
                    union_init.file_pos,
                )
            })?;
        let data_ptr = self
            .builder
            .build_struct_gep(union_ptr, 1, "union.init.data.gep")
            .map_err(|_| {
                self.err(
                    format!("Unable to GEP union \"{}\" data.", &union_init.name),
                    union_init.file_pos,
                )
            })?;

        let inner_any_type = data_ptr.get_type().get_element_type();
        let inner_basic_type = CodeGen::any_into_basic_type(inner_any_type)?;

        // Zero out the memory of the new union.
        match inner_basic_type {
            BasicTypeEnum::ArrayType(ty) => {
                let zero_mem = ty.const_zero();
                self.builder.build_store(data_ptr, zero_mem);
            }
            BasicTypeEnum::FloatType(ty) => {
                let zero_mem = ty.const_zero();
                self.builder.build_store(data_ptr, zero_mem);
            }
            BasicTypeEnum::IntType(ty) => {
                let zero_mem = ty.const_zero();
                self.builder.build_store(data_ptr, zero_mem);
            }
            BasicTypeEnum::PointerType(ty) => {
                let zero_mem = ty.const_zero();
                self.builder.build_store(data_ptr, zero_mem);
            }
            BasicTypeEnum::StructType(ty) => {
                let zero_mem = ty.const_zero();
                self.builder.build_store(data_ptr, zero_mem);
            }
            BasicTypeEnum::VectorType(ty) => {
                let zero_mem = ty.const_zero();
                self.builder.build_store(data_ptr, zero_mem);
            }
        };

        let address_space = Some(AddressSpace::Generic);
        let alignment = self.target_machine.get_target_data().get_abi_alignment(
            &self
                .context
                .ptr_sized_int_type(&self.target_machine.get_target_data(), address_space),
        );

        // Store the correct tag for the union.
        self.builder.build_store(tag_ptr, tag);

        // Copy the init value into an array of the correct size of the inner union type.
        self.builder
            .build_memcpy(
                data_ptr,
                alignment,
                val_ptr,
                alignment,
                basic_value.get_type().size_of().unwrap(),
            )
            .map_err(|msg| self.err(msg.into(), union_init.file_pos))?;

        Ok(self.builder.build_load(union_ptr, "union.init.load").into())
    }

    /// Generates a array creation/initialization.
    pub fn compile_array_init(
        &mut self,
        array_init: &mut ArrayInit,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let args = &mut array_init.arguments;

        if args.is_empty() {
            return Err(self.err(
                "Array init with zero arguments.".into(),
                Some(array_init.file_pos),
            ));
        }

        // Compile all arguments(all values that will be set to initialize the
        // array members) and save the type of the first argument which will
        // be used to deduce the type of the whole array.
        let mut compiled_args = Vec::with_capacity(args.len());

        // Dummy arg_type to start with. If it is never set, it will never be used.
        let mut arg_type = self.context.i8_type().into();
        for arg in args.iter_mut() {
            let any_value = self.compile_expr(&mut arg.value, ExprTy::RValue)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
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

    pub fn compile_null(
        &mut self,
        ty: AnyTypeEnum<'ctx>,
        file_pos: Option<FilePosition>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
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
                    file_pos,
                ))
            }
        })
    }
}
