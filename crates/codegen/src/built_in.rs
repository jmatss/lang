use inkwell::{values::AnyValueEnum, AddressSpace, IntPredicate};

use common::{
    error::LangResult,
    token::{
        expr::Var,
        expr::{BuiltInCall, Expr},
        lit::Lit,
    },
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
    ARGC_GLOBAL_VAR_NAME, ARGV_GLOBAL_VAR_NAME,
};

use crate::{expr::ExprTy, generator::CodeGen};

#[derive(Debug)]
enum PtrMathOp {
    Add,
    Sub,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    // TODO: Temporarily treats functions return void as return i32 "0".
    //       Should make a custom value ex rusts "()" instead.
    /// Generates a built-in call. Returns the return value of the compiled
    /// built-in function.
    pub fn compile_built_in_call(
        &mut self,
        built_in_call: &mut BuiltInCall,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let file_pos = built_in_call.file_pos.to_owned();

        match built_in_call.name.as_ref() {
            // Gets the size of a specified type. The size is returned as a
            // unsigned 32 bit integer.
            "size" => {
                if let Some(arg_type_id) = built_in_call
                    .generics
                    .as_ref()
                    .map(|gs| gs.iter_types().next())
                    .flatten()
                {
                    let ty = self.compile_type(*arg_type_id, Some(file_pos))?;

                    if let Some(size) = ty.size_of() {
                        Ok(size.const_cast(self.context.i32_type(), false).into())
                    } else {
                        Err(self.analyze_ctx.ast_ctx.err(format!(
                            "Tried to take @size non sized type: {:#?}",
                            built_in_call
                        )))
                    }
                } else {
                    unreachable!("Argument count check in Analyze.");
                }
            }

            // Gets the type of a expression. This built-in call will be "replaced"
            // in the AST before this point is reached, so should never end up here.
            "type" => Err(self.err(
                format!(
                    "Unexpected @type built in call, should not end up here: {:#?}",
                    built_in_call
                ),
                Some(file_pos),
            )),

            // Gets the name of the given variable as a null terminated C string.
            "name" => {
                if let Some(value) = built_in_call.arguments.first().map(|arg| &arg.value) {
                    if let Expr::Var(var) = &value {
                        let name = var.name.clone();

                        let u8_type_id = self.analyze_ctx.ty_ctx.ty_env.id(&Ty::CompoundType(
                            InnerTy::U8,
                            Generics::empty(),
                            TypeInfo::None,
                        ))?;
                        let ptr_type_id = self
                            .analyze_ctx
                            .ty_ctx
                            .ty_env
                            .id(&Ty::Pointer(u8_type_id, TypeInfo::None))?;

                        let mut expr =
                            Expr::Lit(Lit::String(name), Some(ptr_type_id), Some(file_pos));

                        self.compile_expr(&mut expr, ExprTy::RValue)
                    } else {
                        Err(self.err(
                            format!(
                                "Invalid type of argument given to @name(). Should be var: {:#?}",
                                built_in_call
                            ),
                            Some(file_pos),
                        ))
                    }
                } else {
                    unreachable!("Argument count check in Analyze.");
                }
            }

            // Creates a null/empty value of the specified type.
            "null" => {
                if let Some(arg_type_id) = built_in_call
                    .generics
                    .as_ref()
                    .map(|gs| gs.iter_types().next())
                    .flatten()
                {
                    let ty = self.compile_type(*arg_type_id, Some(file_pos))?;
                    self.compile_null(ty, Some(file_pos))
                } else {
                    unreachable!("Argument count check in Analyze.");
                }
            }

            // Checks if the given argument is null/0.
            "is_null" => {
                if let Some(expr) = built_in_call
                    .arguments
                    .first_mut()
                    .map(|arg| &mut arg.value)
                {
                    // TODO: How should this logic work? Can already existing
                    //       pointers be converted to null pointers? Currently
                    //       there will never exist any null pointers.
                    let value = self.compile_expr(expr, ExprTy::RValue)?;

                    // TODO: Which types should be allowed?
                    if value.is_pointer_value() {
                        let ptr = value.into_pointer_value();
                        Ok(self.builder.build_is_null(ptr, "is.null.ptr").into())
                    } else if value.is_int_value() {
                        let int_value = value.into_int_value();
                        let zero = int_value.get_type().const_zero();
                        Ok(self
                            .builder
                            .build_int_compare(IntPredicate::EQ, int_value, zero, "is.null.int")
                            .into())
                    } else {
                        Err(self.err(
                            format!(
                                "Invalid type of argument given to @is_null(): {:#?}",
                                built_in_call
                            ),
                            Some(file_pos),
                        ))
                    }
                } else {
                    unreachable!("Argument count check in Analyze.");
                }
            }

            // Adds the value of the second parameter times the size of
            // the pointer element to the pointer value.
            "ptr_add" => self.compile_ptr_math(built_in_call, PtrMathOp::Add),

            // Subtracts the value of the second parameter times the size of
            // the pointer element to the pointer value.
            "ptr_sub" => self.compile_ptr_math(built_in_call, PtrMathOp::Sub),

            // Gets the amount of CLI arguments used when running the program (`argc`).
            // If no `main` function is found in this module, it will be set to 0.
            "argc" => {
                // Only the name of this variable will be used in `get_var_ptr()`.
                let var = Var::new(
                    ARGC_GLOBAL_VAR_NAME.into(),
                    None,
                    None,
                    None,
                    None,
                    None,
                    false,
                );
                let ptr = self.get_var_ptr(&var)?;
                Ok(self.builder.build_load(ptr, "load.global.argc").into())
            }

            // Gets the CLI arguments used when running the program (`argv`).
            // If no `main` function is found in this module, it will be set to 0.
            "argv" => {
                // Only the name of this variable will be used in `get_var_ptr()`.
                let var = Var::new(
                    ARGV_GLOBAL_VAR_NAME.into(),
                    None,
                    None,
                    None,
                    None,
                    None,
                    false,
                );
                let ptr = self.get_var_ptr(&var)?;
                Ok(self.builder.build_load(ptr, "load.global.argv").into())
            }

            // Gets the filename of the file that this built-in call is in.
            "file" => {
                if let Some(file_info) = self.analyze_ctx.ast_ctx.file_info.get(&file_pos.file_nr) {
                    let filename = file_info.filename.clone();
                    self.compile_lit(&Lit::String(filename), &None, Some(file_pos))
                } else {
                    Err(self.err(
                        format!(
                            "Unable to find file info for file with nr {}. Built-in call: {:#?}",
                            file_pos.file_nr, built_in_call
                        ),
                        Some(file_pos),
                    ))
                }
            }

            // Gets the line number at which this built-in is called.
            "line" => Ok(self
                .context
                .i32_type()
                .const_int(file_pos.line_start, false)
                .into()),

            // Gets the column number at which this built-in is called.
            "column" => Ok(self
                .context
                .i32_type()
                .const_int(file_pos.column_start, false)
                .into()),

            // Creates a unreachable instruction.
            "unreachable" => Ok(self.builder.build_unreachable().into()),

            _ => {
                unreachable!("Bad built in name: {:#?}", built_in_call);
            }
        }
    }

    fn compile_ptr_math(
        &mut self,
        built_in_call: &mut BuiltInCall,
        ptr_math_op: PtrMathOp,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let ptr_arg = built_in_call.arguments.get_mut(0).unwrap();
        let ptr_value = self.compile_expr(&mut ptr_arg.value, ExprTy::RValue)?;
        let basic_ptr_value = CodeGen::any_into_basic_value(ptr_value)?;

        let amount_arg = built_in_call.arguments.get_mut(1).unwrap();
        let amount_value = self.compile_expr(&mut amount_arg.value, ExprTy::RValue)?;
        let basic_amount_value = CodeGen::any_into_basic_value(amount_value)?;

        let address_space = Some(AddressSpace::Generic);
        let ptr_int_type = self
            .context
            .ptr_sized_int_type(&self.target_machine.get_target_data(), address_space);

        let ptr_int_value = self.builder.build_ptr_to_int(
            basic_ptr_value.into_pointer_value(),
            ptr_int_type,
            "ptr.to.int",
        );

        let element_type = ptr_value.into_pointer_value().get_type().get_element_type();
        let ptr_element_size = if let Some(size) = element_type.size_of() {
            size.const_cast(ptr_int_value.get_type(), false)
        } else {
            return Err(self.analyze_ctx.ast_ctx.err(format!(
                "Unable to get size of ptr element during @ptr_add: {:#?}",
                built_in_call
            )));
        };
        let amount_value_ptr_sized = self.builder.build_int_z_extend_or_bit_cast(
            basic_amount_value.into_int_value(),
            ptr_int_value.get_type(),
            "int.to.int",
        );

        let amount_value =
            self.builder
                .build_int_mul(amount_value_ptr_sized, ptr_element_size, "val.mul.size");

        let new_ptr_value = match ptr_math_op {
            PtrMathOp::Add => self
                .builder
                .build_int_add(ptr_int_value, amount_value, "ptr.add"),
            PtrMathOp::Sub => self
                .builder
                .build_int_sub(ptr_int_value, amount_value, "ptr.sub"),
        };
        let new_ptr = self.builder.build_int_to_ptr(
            new_ptr_value,
            basic_ptr_value.get_type().into_pointer_type(),
            "int.to.ptr",
        );

        Ok(new_ptr.into())
    }
}
