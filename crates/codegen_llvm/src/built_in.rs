use std::{borrow::Borrow, sync::Arc};

use inkwell::{values::AnyValueEnum, AddressSpace, IntPredicate};
use parking_lot::RwLock;

use common::{
    error::LangResult,
    file::FilePosition,
    path::LangPath,
    token::{
        expr::Var,
        expr::{Argument, ArrayInit, BuiltInCall, Expr, FnCall, FormatPart},
        lit::{Lit, StringType},
        op::{AssignOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    ty::{
        generics::Generics, get::get_inner, inner_ty::InnerTy, is::is_primitive, ty::Ty,
        ty_env::TyEnv, type_id::TypeId, type_info::TypeInfo,
    },
    ARGC_GLOBAL_VAR_NAME, ARGV_GLOBAL_VAR_NAME,
};

use crate::{expr::ExprTy, generator::CodeGen};

#[derive(Debug)]
enum PtrMathOp {
    Add,
    Sub,
}

impl<'a, 'b, 'ctx> CodeGen<'a, 'b, 'ctx> {
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

            // Gets the name of the given variable as a StringView terminated C string.
            "name" => {
                if let Some(value) = built_in_call.arguments.first().map(|arg| &arg.value) {
                    if let Expr::Var(var) = &value {
                        let name = var.name.clone();
                        self.compile_lit(
                            &Lit::String(name, StringType::Regular),
                            &None,
                            Some(file_pos),
                        )
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

            // Creates a null/empty value of its return type.
            "null" => {
                if let Some(ret_type_id) = built_in_call.ret_type {
                    let ty = self.compile_type(ret_type_id, Some(file_pos))?;
                    self.compile_null(ty, Some(file_pos))
                } else {
                    unreachable!("Type not set for @null: {:#?}", built_in_call);
                }
            }

            // Checks if the given argument is or isn't null/0.
            "is_null" | "is_not_null" => {
                let check_if_true = built_in_call.name == "is_null";

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
                        if check_if_true {
                            Ok(self.builder.build_is_null(ptr, "is.null.ptr").into())
                        } else {
                            Ok(self
                                .builder
                                .build_is_not_null(ptr, "is.not.null.ptr")
                                .into())
                        }
                    } else if value.is_int_value() {
                        let int_value = value.into_int_value();
                        let zero = int_value.get_type().const_zero();
                        if check_if_true {
                            Ok(self
                                .builder
                                .build_int_compare(IntPredicate::EQ, int_value, zero, "is.null.int")
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_compare(
                                    IntPredicate::NE,
                                    int_value,
                                    zero,
                                    "is.not.null.int",
                                )
                                .into())
                        }
                    } else {
                        Err(self.err(
                            format!(
                                "Invalid type of argument given to @is_null/@is_not_null: {:#?}",
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

            // The first argument of the `format` call is a string literal and
            // the rest of the arguments (variadic) are the arguments to the
            // given format string literal.
            "format" => self.compile_format(built_in_call),

            // Creates a instance of an array with a specified size and all values
            // initialized to a specified value.
            "array" => {
                let init_val = built_in_call.arguments.get(0).unwrap().value.clone();
                let arr_dim = built_in_call.arguments.get(1).unwrap().value.clone();

                // TODO: This logic is copied from `compile_type`, merge them
                //       in some way.
                let lit_dim = match &arr_dim {
                    Expr::Lit(Lit::Integer(num, radix), ..) => u32::from_str_radix(num, *radix)
                        .map_err(|_| {
                            self.err(
                                format!(
                                    "Invalid integer found in array dimension of @array: {:#?}",
                                    built_in_call
                                ),
                                Some(built_in_call.file_pos),
                            )
                        })?,
                    _ => {
                        return Err(self.err(
                            format!(
                                "TODO: Invalid expression used as array dimension in @array: {:?}",
                                built_in_call
                            ),
                            Some(built_in_call.file_pos),
                        ))
                    }
                };

                let mut args = Vec::with_capacity(lit_dim as usize);
                for _ in 0..lit_dim {
                    let arg = Argument::new(None, None, init_val.clone());
                    args.push(arg)
                }

                let mut array_init = ArrayInit::new(args, built_in_call.file_pos);
                self.compile_array_init(&mut array_init)
            }

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
            // The filename will have the type StringView.
            "file" => {
                if let Some(file_info) = self.analyze_ctx.ast_ctx.file_info.get(&file_pos.file_nr) {
                    let filename = file_info.filename.clone();
                    self.compile_lit(
                        &Lit::String(filename, StringType::Regular),
                        &None,
                        Some(file_pos),
                    )
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

    /// This function creates a `std::string::String` variable and appends all
    /// the given format arguments to it.
    ///
    /// The arguments to the `@format()` call are expected to be of types
    /// `std::string::StringView`. If they aren't, this function will try to create
    /// StringView's from the values if possible. Currently this is only done for
    /// a subset of  primitive types.
    fn compile_format(
        &mut self,
        built_in_call: &mut BuiltInCall,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let file_pos = Some(built_in_call.file_pos);

        let format_parts = if let Some(format_parts) = &built_in_call.format_parts {
            format_parts
        } else {
            return Err(self.err(
                format!(
                    "Found no `format_parts` in @format() call at pos: {:#?}",
                    built_in_call
                ),
                Some(built_in_call.file_pos),
            ));
        };

        let std_module: LangPath = vec!["std".into()].into();

        let mut string_init_call = FnCall::new(
            "init".into(),
            std_module.clone(),
            vec![Argument::new(
                None,
                None,
                Expr::Lit(
                    Lit::Integer("16".into(), 10),
                    Some(type_id_u32(&mut self.analyze_ctx.ty_env.lock(), file_pos)?),
                    file_pos,
                ),
            )],
            None,
            None,
        );
        string_init_call.is_method = true;
        string_init_call.method_adt = Some(type_id_string(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);
        string_init_call.ret_type = Some(type_id_result_string(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);

        let mut un_op_address = UnOp::new(
            UnOperator::Address,
            Box::new(Expr::FnCall(string_init_call)),
            file_pos,
        );
        un_op_address.ret_type = Some(type_id_string_ptr(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);

        let mut string_get_success_call = FnCall::new(
            "get_success".into(),
            std_module.clone(),
            vec![Argument::new(
                Some("this".into()),
                None,
                Expr::Op(Op::UnOp(un_op_address)),
            )],
            None,
            None,
        );
        string_get_success_call.is_method = true;
        string_get_success_call.method_adt = Some(type_id_result_string(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);
        string_get_success_call.ret_type = Some(type_id_string(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);

        // Create a arbitrary name to reduce the change for collision.
        let var_name = format!(
            "format_{}_{}_{}",
            built_in_call.file_pos.file_nr,
            built_in_call.file_pos.offset,
            built_in_call.file_pos.line_start
        );
        let string_var = Arc::new(RwLock::new(Var::new(
            var_name.clone(),
            Some(type_id_string(
                &mut self.analyze_ctx.ty_env.lock(),
                file_pos,
            )?),
            None,
            None,
            None,
            None,
            false,
        )));
        let mut string_var_expr = Expr::Var(string_var.as_ref().borrow().read().clone());

        let mut string_var_decl = Stmt::VariableDecl(Arc::clone(&string_var), None);
        let mut string_var_assign = Stmt::Assignment(
            AssignOperator::Assignment,
            Expr::Var(string_var.as_ref().borrow().read().clone()),
            Expr::FnCall(string_get_success_call),
            file_pos,
        );

        // Need to insert the variable declaration into the look-up tables so that
        // other parts of the CodeGen can find it during generation.
        let var_key = (var_name.clone(), self.cur_block_id);
        self.analyze_ctx
            .ast_ctx
            .variables
            .insert(var_key, string_var);

        self.compile_stmt(&mut string_var_decl)?;
        self.compile_stmt(&mut string_var_assign)?;

        let mut string_var_address = UnOp::new(
            UnOperator::Address,
            Box::new(string_var_expr.clone()),
            file_pos,
        );
        string_var_address.ret_type = Some(type_id_string_ptr(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);

        let string_var_ptr = Expr::Op(Op::UnOp(string_var_address));

        let mut arg_idx = 0;
        for format_part in format_parts {
            let expr = match format_part {
                FormatPart::String(str_lit) => self.str_lit_to_string_view(str_lit, file_pos)?,
                FormatPart::Arg(expr) => {
                    let type_id = expr.get_expr_type()?;
                    let is_primitive = is_primitive(&self.analyze_ctx.ty_env.lock(), type_id)?;
                    if is_primitive {
                        let expr = self.primitive_to_string_view(expr, &var_name, arg_idx)?;
                        arg_idx += 1;
                        expr
                    } else {
                        expr.clone()
                    }
                }
            };
            self.append_view_to_string(string_var_ptr.clone(), expr, &std_module)?;
        }

        self.compile_expr(&mut string_var_expr, ExprTy::RValue)
    }

    fn append_view_to_string(
        &mut self,
        this_string_ptr: Expr,
        string_view_expr: Expr,
        types_module: &LangPath,
    ) -> LangResult<()> {
        let file_pos = string_view_expr.file_pos().cloned();

        let mut string_append_view_call = FnCall::new(
            "append_view".into(),
            types_module.clone(),
            vec![
                Argument::new(Some("this".into()), None, this_string_ptr),
                Argument::new(None, None, string_view_expr),
            ],
            None,
            file_pos,
        );
        string_append_view_call.is_method = true;
        string_append_view_call.method_adt = Some(type_id_string(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);
        string_append_view_call.ret_type = Some(type_id_result_u32(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);

        self.compile_fn_call(&mut string_append_view_call)?;
        Ok(())
    }

    fn str_lit_to_string_view(
        &mut self,
        str_lit: &str,
        file_pos: Option<FilePosition>,
    ) -> LangResult<Expr> {
        Ok(Expr::Lit(
            Lit::String(str_lit.into(), StringType::Regular),
            Some(type_id_string_view(
                &mut self.analyze_ctx.ty_env.lock(),
                file_pos,
            )?),
            file_pos,
        ))
    }

    fn primitive_to_string_view(
        &mut self,
        primitive_expr: &Expr,
        var_name: &str,
        arg_idx: usize,
    ) -> LangResult<Expr> {
        let file_pos = primitive_expr.file_pos().cloned();
        let type_id = primitive_expr.get_expr_type()?;

        let mut ty_env_guard = self.analyze_ctx.ty_env.lock();

        // TODO: What buffer size should floats have? What is their max char size?
        // `buf_size` is the max amount of bytes that a given primitive type
        // value can occupy in string form.

        let inner_ty = get_inner(&ty_env_guard, type_id)?;
        let primitive_ident = inner_ty.get_primitive_ident();
        let buf_size = match inner_ty {
            InnerTy::I8 => 4,
            InnerTy::U8 => 3,
            InnerTy::I16 => 6,
            InnerTy::U16 => 5,
            InnerTy::I32 => 11,
            InnerTy::U32 => 10,
            InnerTy::F32 => 0,
            InnerTy::I64 => 20,
            InnerTy::U64 => 19,
            InnerTy::F64 => 0,
            InnerTy::I128 => 40,
            InnerTy::U128 => 39,
            _ => panic!("bad format variadic arg type: {:#?}", primitive_expr),
        };

        let primitive_type_id = ty_env_guard.id(&Ty::CompoundType(
            InnerTy::Struct(primitive_ident.into()),
            TypeInfo::DefaultOpt(file_pos),
        ))?;

        let u8_type_id = type_id_u8(&mut ty_env_guard, file_pos)?;
        let u32_type_id = type_id_u32(&mut ty_env_guard, file_pos)?;
        let arr_type_id = ty_env_guard.id(&Ty::Array(
            u8_type_id,
            Some(Box::new(Expr::Lit(
                Lit::Integer(buf_size.to_string(), 10),
                Some(u32_type_id),
                None,
            ))),
            TypeInfo::DefaultOpt(file_pos),
        ))?;
        let arr_ptr_type_id =
            ty_env_guard.id(&Ty::Pointer(arr_type_id, TypeInfo::DefaultOpt(file_pos)))?;

        let buf_var_name = format!("{}_idx_{}", var_name, arg_idx);
        let buf_var = Arc::new(RwLock::new(Var::new(
            buf_var_name.clone(),
            Some(arr_type_id),
            None,
            None,
            None,
            None,
            false,
        )));
        let mut buf_var_decl = Stmt::VariableDecl(Arc::clone(&buf_var), None);

        let buf_var_key = (buf_var_name, self.cur_block_id);
        self.analyze_ctx
            .ast_ctx
            .variables
            .insert(buf_var_key, Arc::clone(&buf_var));

        std::mem::drop(ty_env_guard);

        self.compile_stmt(&mut buf_var_decl)?;

        let mut buf_var_address = UnOp::new(
            UnOperator::Address,
            Box::new(Expr::Var(buf_var.as_ref().borrow().read().clone())),
            None,
        );
        buf_var_address.ret_type = Some(arr_ptr_type_id);

        let mut primitive_to_string_view_call = FnCall::new(
            "to_string_view".into(),
            "".into(),
            vec![
                Argument::new(None, None, primitive_expr.clone()),
                Argument::new(None, None, Expr::Op(Op::UnOp(buf_var_address))),
            ],
            None,
            None,
        );
        primitive_to_string_view_call.is_method = true;
        primitive_to_string_view_call.method_adt = Some(primitive_type_id);
        primitive_to_string_view_call.ret_type = Some(type_id_string_view(
            &mut self.analyze_ctx.ty_env.lock(),
            file_pos,
        )?);

        Ok(Expr::FnCall(primitive_to_string_view_call))
    }
}

pub fn type_id_u8(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    ty_env.id(&Ty::CompoundType(
        InnerTy::U8,
        TypeInfo::DefaultOpt(file_pos),
    ))
}

pub fn type_id_u32(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    ty_env.id(&Ty::CompoundType(
        InnerTy::U32,
        TypeInfo::DefaultOpt(file_pos),
    ))
}

pub fn type_id_string(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    let string_path = ["std".into(), "string".into(), "String".into()].into();
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(string_path),
        TypeInfo::DefaultOpt(file_pos),
    ))
}

pub fn type_id_string_ptr(
    ty_env: &mut TyEnv,
    file_pos: Option<FilePosition>,
) -> LangResult<TypeId> {
    let string_type_id = type_id_string(ty_env, file_pos)?;
    ty_env.id(&Ty::Pointer(string_type_id, TypeInfo::DefaultOpt(file_pos)))
}

pub fn type_id_string_view(
    ty_env: &mut TyEnv,
    file_pos: Option<FilePosition>,
) -> LangResult<TypeId> {
    let view_path = ["std".into(), "string".into(), "StringView".into()].into();
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(view_path),
        TypeInfo::DefaultOpt(file_pos),
    ))
}

pub fn type_id_result_string(
    ty_env: &mut TyEnv,
    file_pos: Option<FilePosition>,
) -> LangResult<TypeId> {
    let result_path: LangPath = ["std".into(), "Result".into()].into();
    let mut gens = Generics::new();
    gens.insert("T".into(), type_id_string(ty_env, file_pos)?);
    gens.insert("E".into(), type_id_string_view(ty_env, file_pos)?);
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(result_path.with_gens(gens)),
        TypeInfo::DefaultOpt(file_pos),
    ))
}

pub fn type_id_result_u32(
    ty_env: &mut TyEnv,
    file_pos: Option<FilePosition>,
) -> LangResult<TypeId> {
    let result_path: LangPath = ["std".into(), "Result".into()].into();
    let mut gens = Generics::new();
    gens.insert("T".into(), type_id_u32(ty_env, file_pos)?);
    gens.insert("E".into(), type_id_string_view(ty_env, file_pos)?);
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(result_path.with_gens(gens)),
        TypeInfo::DefaultOpt(file_pos),
    ))
}
