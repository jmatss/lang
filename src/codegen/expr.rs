use super::generator::CodeGen;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::{
    common::variable_type::Type,
    lex::token::Literal,
    parse::token::{AccessType, Expression, FunctionCall, StructInit, TypeStruct},
    CustomResult,
};
use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, FloatValue, IntValue},
    AddressSpace,
};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_expr(
        &mut self,
        expr: &mut Expression,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        match expr {
            Expression::Literal(lit, ty_opt) => self.compile_lit(lit, ty_opt),
            Expression::Variable(var) => {
                // TODO: Will this always be regular?
                Ok(self.compile_var_load(var, &AccessType::Regular)?.into())
            }
            Expression::FunctionCall(func_call) => self.compile_func_call(func_call),
            Expression::Operation(op) => self.compile_op(op),
            Expression::StructInit(struct_init) => self.compile_struct_init(struct_init),
            Expression::Type(ty) => {
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
        }
    }

    fn compile_lit(
        &mut self,
        lit: &Literal,
        ty_opt: &Option<TypeStruct>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        match lit {
            Literal::StringLiteral(str_lit) => {
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

            Literal::CharLiteral(char_lit) => {
                if char_lit.chars().count() == 1 {
                    if let Some(ch) = char_lit.chars().next() {
                        Ok(AnyValueEnum::IntValue(
                            self.context.i32_type().const_int(ch as u64, false),
                        ))
                    } else {
                        Err(LangError::new(
                            "Unable to get char literal.".into(),
                            CodeGenError,
                        ))
                    }
                } else {
                    Err(LangError::new(
                        "Char literal isn't a single character.".into(),
                        CodeGenError,
                    ))
                }
            }

            Literal::Bool(true) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_all_ones(),
            )),
            Literal::Bool(false) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_zero(),
            )),

            Literal::Integer(int_lit, radix) => Ok(AnyValueEnum::IntValue(
                self.compile_lit_int(int_lit, ty_opt, *radix)?,
            )),

            Literal::Float(float_lit) => Ok(AnyValueEnum::FloatValue(
                self.compile_lit_float(float_lit, ty_opt)?,
            )),
        }
    }

    // TODO: Better conversion of the integer literal.
    // TODO: i32 as default.
    fn compile_lit_int(
        &mut self,
        lit: &str,
        ty_opt: &Option<TypeStruct>,
        radix: u32,
    ) -> CustomResult<IntValue<'ctx>> {
        // TODO: Where should the integer literal conversion be made?

        Ok(match ty_opt {
            Some(type_struct) => match type_struct.t {
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
                // TODO: What should the default inte size be?
                Type::Int => {
                    let val = i32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, true)
                }
                Type::Uint => {
                    let val = u32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, false)
                }
                _ => {
                    return Err(LangError::new(
                        format!("Invalid integer type: {:?}", type_struct.t),
                        CodeGenError,
                    ))
                }
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
        ty_opt: &Option<TypeStruct>,
    ) -> CustomResult<FloatValue<'ctx>> {
        Ok(match ty_opt {
            Some(type_struct) => match type_struct.t {
                Type::F32 => self.context.f32_type().const_float(lit.parse()?),
                Type::F64 => self.context.f64_type().const_float(lit.parse()?),
                // TODO: What should the default float size be?
                Type::Float => self.context.f32_type().const_float(lit.parse()?),
                _ => {
                    return Err(LangError::new(
                        format!("Invalid float type: {:?}", type_struct.t),
                        CodeGenError,
                    ))
                }
            },
            // TODO: What should the default float size be? F32 atm.
            None => self.context.f32_type().const_float(lit.parse()?),
        })
    }

    // TODO: Array access.

    // TODO: Temporarily treats functions return void as return i32 "0".
    //       Should make a custom value ex rusts "()" instead.
    /// Generates a function call. Returns the return value of the compiled
    /// function.
    fn compile_func_call(
        &mut self,
        func_call: &mut FunctionCall,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        if let Some(func_ptr) = self.module.get_function(&func_call.name) {
            // Checks to see if the arguments are fewer that parameters. The
            // arguments are allowed to be greater than parameters since variadic
            // functions are supported.
            if func_call.arguments.len() < func_ptr.count_params() as usize {
                return Err(LangError::new(
                    format!(
                        "Wrong amount of args given when calling func: {}. Expected: {}, got: {}",
                        &func_call.name,
                        func_ptr.count_params(),
                        func_call.arguments.len()
                    ),
                    CodeGenError,
                ));
            }

            let mut args = Vec::with_capacity(func_call.arguments.len());
            for arg in &mut func_call.arguments {
                let any_value = self.compile_expr(&mut arg.value)?;
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
            Err(LangError::new(
                format!(
                    "Unable to find function with name {} to call.",
                    &func_call.name
                ),
                CodeGenError,
            ))
        }
    }

    /// Generates a struct creation/initialization.
    fn compile_struct_init(
        &mut self,
        struct_init: &mut StructInit,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let struct_type = if let Some(inner) = self.module.get_struct_type(&struct_init.name) {
            inner
        } else {
            return Err(LangError::new(
                format!("Unable to get struct: {}", &struct_init.name),
                CodeGenError,
            ));
        };

        // Checks to see if the amount of arguments are different from the
        // amount of members.
        if struct_init.arguments.len() != struct_type.count_fields() as usize {
            return Err(LangError::new(
                format!(
                    "Wrong amount of args given when init struct: {}. Expected: {}, got: {}",
                    &struct_init.name,
                    struct_type.count_fields(),
                    struct_init.arguments.len()
                ),
                CodeGenError,
            ));
        }

        // Compiles all arguments to "codegen".
        let mut args = Vec::with_capacity(struct_init.arguments.len());
        for arg in &mut struct_init.arguments {
            let any_value = self.compile_expr(&mut arg.value)?;
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
                    LangError::new(
                        format!(
                            "Unable to GEP struct \"{}\" member {}.",
                            &struct_init.name, i
                        ),
                        CodeGenError,
                    )
                })?;

            self.builder.build_store(member_ptr, *arg_value);
        }

        Ok(self
            .builder
            .build_load(struct_ptr, "struct.init.load")
            .into())
    }

    fn infer_arg_type(
        &mut self,
        i: usize,
        func_name: &str,
        param_type: &BasicTypeEnum,
        arg_type: &BasicTypeEnum,
    ) -> CustomResult<()> {
        return Ok(());
        if arg_type != param_type {
            // TODO: Should be able to convert a {[u8: N]} to a {u8}. This is
            //       useful when working with for example string literals.
            //       Is there a way to see what type a PointerValue is poiting
            //       at through the inkwell API?
            // TODO: Add logic/edge cases where the type of the argument can
            //       be converted to the type of the parameter with no issues.
            Err(LangError::new(
                format!(
                    "Arg type at index {} wrong type when calling func: {}. Expected: {:?}, got: {:?}",
                i,
                func_name,
                param_type,
                arg_type,
                ),
                CodeGenError,
            ))
        } else {
            Ok(())
        }
    }
}
