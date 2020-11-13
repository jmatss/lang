use crate::generator::CodeGen;
use common::{
    error::CustomResult,
    token::{
        expr::{ArrayInit, Expr, FuncCall, StructInit},
        lit::Lit,
    },
    types::Type,
};
use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, FloatValue, IntValue},
    AddressSpace,
};

#[derive(Debug, Copy, Clone)]
pub enum ExprTy {
    LValue,
    RValue,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    /// Compiles a expression. If this expression is a regular rvalue expression,
    /// it should be evaluated to a "value". If this expression is a lvalue,
    /// the "last" recurisve call to this function should return a pointer to
    /// the value so that the rhs can be assigned to it.
    ///
    /// Example:
    /// ```
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
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let any_value = match expr {
            Expr::Lit(lit, ty_opt) => self.compile_lit(lit, ty_opt),
            Expr::FuncCall(func_call) => {
                // Since this is a method call, the function will have been renamed
                // so that its name is unique for the struct instead of unqiue
                // for the whole program. Rename this method call to the same
                // name as the method.
                if let Some(struct_name) = &func_call.method_struct {
                    func_call.name = common::util::to_method_name(struct_name, &func_call.name)
                }
                self.compile_func_call(func_call)
            }
            Expr::Op(op) => self.compile_op(op, expr_ty),
            Expr::StructInit(struct_init) => self.compile_struct_init(struct_init),
            Expr::ArrayInit(array_init) => self.compile_array_init(array_init),
            Expr::Type(ty) => {
                // TODO: Does something need to be done here? Does a proper value
                //       need to be returned? For now just return a dummy value.
                Ok(match self.compile_type(&ty)? {
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
        }?;

        self.prev_expr = Some(any_value);
        Ok(any_value)
    }

    pub fn compile_lit(
        &mut self,
        lit: &Lit,
        ty_opt: &Option<Type>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        match lit {
            Lit::String(str_lit) => {
                // Returns a pointer to the newly created string literal.
                // The string literal will be an array of u8(/i8(?)) with a
                // null terminator.
                // TODO: Probably best to let string literals be a pointer to
                //       an array so one can get the size. But for now it is
                //       casted to a pointer to u8 to be compatible with C code.
                // See: https://github.com/TheDan64/inkwell/issues/32
                let lit_ptr = unsafe {
                    self.builder
                        .build_global_string(str_lit, "str.lit")
                        .as_pointer_value()
                };
                let address_space = AddressSpace::Generic;
                let i8_ptr_type = self.context.i8_type().ptr_type(address_space);
                Ok(lit_ptr.const_cast(i8_ptr_type).into())
            }

            Lit::Char(char_lit) => {
                if char_lit.chars().count() == 1 {
                    if let Some(ch) = char_lit.chars().next() {
                        Ok(AnyValueEnum::IntValue(
                            self.context.i32_type().const_int(ch as u64, false),
                        ))
                    } else {
                        Err(self.err("Unable to get char literal.".into()))
                    }
                } else {
                    Err(self.err("Char literal isn't a single character.".into()))
                }
            }

            Lit::Bool(true) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_all_ones(),
            )),
            Lit::Bool(false) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_zero(),
            )),

            Lit::Integer(int_lit, radix) => Ok(AnyValueEnum::IntValue(
                self.compile_lit_int(int_lit, ty_opt, *radix)?,
            )),

            Lit::Float(float_lit) => Ok(AnyValueEnum::FloatValue(
                self.compile_lit_float(float_lit, ty_opt)?,
            )),
        }
    }

    // TODO: Better conversion of the integer literal.
    // TODO: i32 as default.
    fn compile_lit_int(
        &mut self,
        lit: &str,
        gen_ty_opt: &Option<Type>,
        radix: u32,
    ) -> CustomResult<IntValue<'ctx>> {
        // TODO: Where should the integer literal conversion be made?

        Ok(match gen_ty_opt {
            Some(ty) => match ty {
                Type::I8 => {
                    let val = i8::from_str_radix(lit, radix)? as u64;
                    self.context.i8_type().const_int(val, true)
                }
                Type::U8 => {
                    let val = u8::from_str_radix(lit, radix)? as u64;
                    self.context.i8_type().const_int(val, false)
                }
                Type::I16 => {
                    let val = i16::from_str_radix(lit, radix)? as u64;
                    self.context.i16_type().const_int(val, true)
                }
                Type::U16 => {
                    let val = u16::from_str_radix(lit, radix)? as u64;
                    self.context.i16_type().const_int(val, false)
                }
                Type::I32 => {
                    let val = i32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, true)
                }
                Type::U32 => {
                    let val = u32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, false)
                }
                Type::I64 => {
                    let val = i64::from_str_radix(lit, radix)? as u64;
                    self.context.i64_type().const_int(val, true)
                }
                Type::U64 => {
                    let val = u64::from_str_radix(lit, radix)? as u64;
                    self.context.i64_type().const_int(val, false)
                }
                Type::I128 => {
                    let val = i128::from_str_radix(lit, radix)? as u64;
                    self.context.i128_type().const_int(val, true)
                }
                Type::U128 => {
                    let val = u128::from_str_radix(lit, radix)? as u64;
                    self.context.i128_type().const_int(val, false)
                }
                _ => return Err(self.err(format!("Invalid literal integer type: {:?}", ty))),
            },
            // TODO: What should the default int size be? Signed 32 atm.
            None => {
                let val = i32::from_str_radix(lit, radix)? as u64;
                self.context.i32_type().const_int(val, true)
            }
        })
    }

    // TODO: Better conversion of the float literal.
    // TODO: f32 as default.
    fn compile_lit_float(
        &mut self,
        lit: &str,
        gen_ty_opt: &Option<Type>,
    ) -> CustomResult<FloatValue<'ctx>> {
        Ok(match gen_ty_opt {
            Some(ty) => match ty {
                Type::F32 => self.context.f32_type().const_float(lit.parse()?),
                Type::F64 => self.context.f64_type().const_float(lit.parse()?),
                // TODO: What should the default float size be?
                _ => return Err(self.err(format!("Invalid literal float type: {:?}", ty))),
            },
            // TODO: What should the default float size be? F32 atm.
            None => self.context.f32_type().const_float(lit.parse()?),
        })
    }

    // TODO: Temporarily treats functions return void as return i32 "0".
    //       Should make a custom value ex rusts "()" instead.
    /// Generates a function call. Returns the return value of the compiled
    /// function.
    pub fn compile_func_call(
        &mut self,
        func_call: &mut FuncCall,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        if let Some(func_ptr) = self.module.get_function(&func_call.name) {
            // Checks to see if the arguments are fewer that parameters. The
            // arguments are allowed to be greater than parameters since variadic
            // functions are supported to be compatible with C code.
            if func_call.arguments.len() < func_ptr.count_params() as usize {
                return Err(self.err(format!(
                    "Wrong amount of args given when calling func \"{}\". Expected: {}, got: {}",
                    &func_call.name,
                    func_ptr.count_params(),
                    func_call.arguments.len()
                )));
            }

            let mut args = Vec::with_capacity(func_call.arguments.len());
            for arg in &mut func_call.arguments {
                let any_value = self.compile_expr(&mut arg.value, ExprTy::RValue)?;
                let basic_value = CodeGen::any_into_basic_value(any_value)?;
                args.push(basic_value);
            }

            for (i, param) in func_ptr.get_param_iter().enumerate() {
                if let Some(arg) = args.get_mut(i) {
                    // Checks to see if the types of the parameter and the
                    // argument are the same. If they are different, see if the
                    // type of the argument can be casted to the same type.
                    self.infer_arg_type(i, &func_call.name, &param.get_type(), &arg.get_type())?;
                } else {
                    unreachable!("None when comparing arg and par in func call compile.");
                }
            }

            let call = self.builder.build_call(func_ptr, args.as_slice(), "call");

            // Left == BasicValueEnum, Right == InstructionValue.
            // Will be right if the function returns "void", left otherwise.
            Ok(if let Some(ret_val) = call.try_as_basic_value().left() {
                ret_val.into()
            } else {
                self.context.i32_type().const_zero().into()
            })
        } else {
            Err(self.err(format!(
                "Unable to find function with name {} to call.",
                &func_call.name
            )))
        }
    }

    /// Generates a struct creation/initialization.
    pub fn compile_struct_init(
        &mut self,
        struct_init: &mut StructInit,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let struct_type = if let Some(inner) = self.module.get_struct_type(&struct_init.name) {
            inner
        } else {
            return Err(self.err(format!("Unable to get struct: {}", &struct_init.name)));
        };

        // Checks to see if the amount of arguments are different from the
        // amount of members.
        if struct_init.arguments.len() != struct_type.count_fields() as usize {
            return Err(self.err(format!(
                "Wrong amount of args given when init struct: {}. Expected: {}, got: {}",
                &struct_init.name,
                struct_type.count_fields(),
                struct_init.arguments.len()
            )));
        }

        // Compile all arguments(all values that will be set to initialize the
        // struct members).
        let mut args = Vec::with_capacity(struct_init.arguments.len());
        for arg in &mut struct_init.arguments {
            let any_value = self.compile_expr(&mut arg.value, ExprTy::RValue)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            args.push(basic_value);
        }

        for (i, param) in struct_type.get_field_types().iter().enumerate() {
            if let Some(arg) = args.get_mut(i) {
                // Checks to see if the types of the parameter and the
                // argument are the same. If they are different, see if the
                // type of the argument can be casted to the same type.
                self.infer_arg_type(i, &struct_init.name, &param, &arg.get_type())?;
            } else {
                unreachable!("None when comparing arg and par in struct init compile.");
            }
        }

        // TODO: How should the init of a struct work? Currently the var decl
        //       will create a store for the struct. But in this, to access the
        //       members, one has to get a pointer from somewhere. So for now
        //       this function will also make a store so that it can get a pointer
        //       to it so that the values can be inserted.
        // TODO: If this is a const, create it with "const_named_struct".
        //       Otherwise, create const empty one, and then insert the values
        //       one at a time.
        // if is_const {
        //     let struct_value = struct_type.const_named_struct(args.as_ref());
        // } else {

        // Create a tmp storage for the struct so that the init values can be set.
        // Iterate through all members of the struct and initialize them.
        let struct_ptr = self.builder.build_alloca(struct_type, "struct.init");
        for (i, arg_value) in args.iter().enumerate() {
            let member_ptr = self
                .builder
                .build_struct_gep(struct_ptr, i as u32, "struct.init.gep")
                .map_err(|_| {
                    self.err(format!(
                        "Unable to GEP struct \"{}\" member {}.",
                        &struct_init.name, i
                    ))
                })?;

            self.builder.build_store(member_ptr, *arg_value);
        }

        Ok(self
            .builder
            .build_load(struct_ptr, "struct.init.load")
            .into())
    }

    /// Generates a array creation/initialization.
    pub fn compile_array_init(
        &mut self,
        array_init: &mut ArrayInit,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let args = &mut array_init.arguments;

        if args.is_empty() {
            return Err(self.err("Array init with zero arguments.".into()));
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

    fn infer_arg_type(
        &mut self,
        i: usize,
        func_name: &str,
        param_type: &BasicTypeEnum,
        arg_type: &BasicTypeEnum,
    ) -> CustomResult<()> {
        if arg_type != param_type {
            // TODO: Should be able to convert a {[u8: N]} to a {u8}. This is
            //       useful when working with for example string literals.
            //       Is there a way to see what type a PointerValue is poiting
            //       at through the inkwell API?
            // TODO: Add logic/edge cases where the type of the argument can
            //       be converted to the type of the parameter with no issues.
            Err(self.err(format!(
                "Arg type at index {} wrong type when calling func: {}. Expected: {:?}, got: {:?}",
                i, func_name, param_type, arg_type,
            )))
        } else {
            Ok(())
        }
    }
}
