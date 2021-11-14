use std::sync::Arc;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    path::LangPath,
    token::{
        expr::{Argument, BuiltInCall, Expr, FnCall, FormatPart, Var},
        lit::{Lit, StringType},
        op::{AssignOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    ty::{
        generics::Generics,
        get::{get_gens, get_inner},
        inner_ty::InnerTy,
        is::is_primitive,
        to_string::{to_string_path, to_string_type_id},
        ty::Ty,
        ty_env::TyEnv,
        type_id::TypeId,
        type_info::TypeInfo,
    },
    util::to_generic_name,
    ARGC_GLOBAL_VAR_NAME, ARGV_GLOBAL_VAR_NAME,
};
use ir::{ty::Type, ExprTy, Val, VarIdx, VAL_EMPTY};
use parking_lot::RwLock;

use crate::{
    build::{expr::build_fn_call, stmt::build_stmt},
    into_err, size_of,
    state::BuildState,
    to_ir_type, VarModifier,
};

use super::expr::build_expr;

#[derive(Debug)]
enum PtrMathOp {
    Add,
    Sub,
}

pub fn build_built_in_call(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    match built_in_call.name.as_ref() {
        "size" => build_built_in_size(state, built_in_call),
        "type" => build_built_in_type(state, built_in_call),
        "name" => build_built_in_name(state, built_in_call),
        "null" => build_built_in_null(state, built_in_call),
        "is_null" | "is_not_null" => build_built_in_is_null(state, built_in_call),
        "ptr_add" => build_built_in_ptr_math(state, built_in_call, PtrMathOp::Add),
        "ptr_sub" => build_built_in_ptr_math(state, built_in_call, PtrMathOp::Sub),
        "format" => build_built_in_format(state, built_in_call),
        "array" => build_built_in_array(state, built_in_call),
        "tuple" => build_built_in_tuple(state, built_in_call),
        "argc" => build_built_in_argc(state, built_in_call),
        "argv" => build_built_in_argv(state, built_in_call),
        "file" => build_built_in_file(state, built_in_call),
        "line" => build_built_in_line(state, built_in_call),
        "column" => build_built_in_column(state, built_in_call),
        "unreachable" => build_built_in_unreachable(state),
        _ => {
            unreachable!("Bad built in name: {:#?}", built_in_call);
        }
    }
}

fn build_built_in_size(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    if let Some(arg_type_id) = built_in_call
        .generics
        .as_ref()
        .map(|gs| gs.iter_types().next())
        .flatten()
    {
        let ir_type = to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock(),
            *arg_type_id,
        )?;
        let size = size_of(state.module, ir_type)?;

        let instr = state.builder.u32(&size.to_string());
        state.cur_block_mut()?.push(instr.clone());
        Ok(instr.val)
    } else {
        Err(LangError::new(
            "Incorrect amount of generic arguments given to @size call".into(),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos.to_owned()),
        ))
    }
}

fn build_built_in_type(_state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    Err(LangError::new(
        "Should not end up in build_built_in_type, @type should already have been compiled".into(),
        LangErrorKind::IrError,
        Some(built_in_call.file_pos.to_owned()),
    ))
}

fn build_built_in_name(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    if let Some(value) = built_in_call.arguments.first().map(|arg| &arg.value) {
        if let Expr::Var(var) = &value {
            let instr = state.builder.string_lit(&mut state.module, &var.name);
            state.cur_block_mut()?.push(instr.clone());
            Ok(instr.val)
        } else {
            Err(LangError::new(
                "Invalid type of argument given to @name(). Should be var.".into(),
                LangErrorKind::IrError,
                Some(built_in_call.file_pos.to_owned()),
            ))
        }
    } else {
        Err(LangError::new(
            "Expected one argument in @name call".into(),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos.to_owned()),
        ))
    }
}

fn build_built_in_null(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    if let Some(ret_type_id) = built_in_call.ret_type {
        let ir_type = to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock(),
            ret_type_id,
        )?;

        let instr = state.builder.null(&mut state.module, ir_type)?;
        state.cur_block_mut()?.push(instr.clone());
        Ok(instr.val)
    } else {
        Err(LangError::new(
            "Type not set for @null.".into(),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos.to_owned()),
        ))
    }
}

fn build_built_in_is_null(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    if let Some(expr) = built_in_call.arguments.first().map(|arg| &arg.value) {
        let value = build_expr(state, expr, ExprTy::RValue)?;
        let mut instr = state.builder.is_null(value);
        state.cur_block_mut()?.push(instr.clone());

        if built_in_call.name == "is_not_null" {
            instr = state.builder.bool_not(instr.val);
            state.cur_block_mut()?.push(instr.clone());
        }

        Ok(instr.val)
    } else {
        Err(LangError::new(
            "Expected argument in @is_null/@is_not_null.".into(),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos.to_owned()),
        ))
    }
}

fn build_built_in_ptr_math(
    state: &mut BuildState,
    built_in_call: &BuiltInCall,
    ptr_math_op: PtrMathOp,
) -> LangResult<Val> {
    let ptr_arg = built_in_call.arguments.get(0).unwrap();
    let ptr_value = build_expr(state, &ptr_arg.value, ExprTy::RValue)?;

    let amount_arg = built_in_call.arguments.get(1).unwrap();
    let amount_value = build_expr(state, &amount_arg.value, ExprTy::RValue)?;

    let ptr_element_size = if let Type::Pointer(ir_type_i) = &ptr_value.1 {
        size_of(state.module, *ir_type_i.clone())?
    } else {
        return Err(LangError::new(
            format!("Tried to @ptr_add non pointer type: {:?}", ptr_value.1),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos.to_owned()),
        ));
    };
    // TODO: int/uint.
    let ptr_element_size_value = state.builder.u32(&ptr_element_size.to_string());
    state.cur_block_mut()?.push(ptr_element_size_value.clone());

    // TODO: int/uint.
    let ptr_int_value = state.builder.cast(ptr_value.clone(), Type::U32);
    state.cur_block_mut()?.push(ptr_int_value.clone());

    let mul_instr = state
        .builder
        .mul(amount_value, ptr_element_size_value.val)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(mul_instr.clone());

    let math_instr = match ptr_math_op {
        PtrMathOp::Add => state.builder.add(ptr_int_value.val, mul_instr.val),
        PtrMathOp::Sub => state.builder.sub(ptr_int_value.val, mul_instr.val),
    }
    .map_err(into_err)?;
    state.cur_block_mut()?.push(math_instr.clone());

    let new_ptr_instr = state.builder.cast(math_instr.val, ptr_value.1);
    state.cur_block_mut()?.push(new_ptr_instr.clone());

    Ok(new_ptr_instr.val)
}

fn build_built_in_array(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    let init_val = built_in_call.arguments.get(0).unwrap().value.clone();
    let arr_dim = built_in_call.arguments.get(1).unwrap().value.clone();

    // TODO: Currently only handles literatls. Should be able to handle
    //       compile-time constants in the future.
    let lit_dim = match &arr_dim {
        Expr::Lit(Lit::Integer(num, radix), ..) => {
            u32::from_str_radix(num, *radix).map_err(|_| {
                LangError::new(
                    format!(
                        "Invalid integer found in array dimension of @array: {:#?}",
                        built_in_call
                    ),
                    LangErrorKind::IrError,
                    Some(built_in_call.file_pos),
                )
            })?
        }
        _ => {
            return Err(LangError::new(
                format!(
                    "TODO: Invalid expression used as array dimension in @array: {:?}",
                    built_in_call
                ),
                LangErrorKind::IrError,
                Some(built_in_call.file_pos),
            ))
        }
    };

    let mut args = Vec::with_capacity(lit_dim as usize);
    for _ in 0..lit_dim {
        let arg_val = build_expr(state, &init_val, ExprTy::RValue)?;
        args.push(arg_val)
    }

    let instr = state.builder.array_init(&args).map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    Ok(instr.val)
}

fn build_built_in_tuple(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    let type_id = if let Some(type_id) = built_in_call.ret_type {
        type_id
    } else {
        unreachable!("built-in call type id None: {:#?}", built_in_call);
    };

    let tuple_full_name = {
        let ty_env_guard = state.analyze_ctx.ty_env.lock();
        if let Some(gens) = get_gens(&ty_env_guard, type_id)? {
            to_generic_name(&ty_env_guard, "Tuple", gens)
        } else {
            "Tuple".into()
        }
    };

    let mut args = Vec::with_capacity(built_in_call.arguments.len());
    for arg in &built_in_call.arguments {
        let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;
        args.push(arg_val)
    }

    let instr = state
        .builder
        .struct_init(&mut state.module, &tuple_full_name, &args)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    Ok(instr.val)
}

fn build_built_in_argc(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    load_global_var(state, built_in_call, ARGC_GLOBAL_VAR_NAME)
}

fn build_built_in_argv(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    load_global_var(state, built_in_call, ARGV_GLOBAL_VAR_NAME)
}

fn load_global_var(
    state: &mut BuildState,
    built_in_call: &BuiltInCall,
    name: &str,
) -> LangResult<Val> {
    let var_idx = if let Some(global_var_idx) = state.globals.get(name) {
        VarIdx::Global(*global_var_idx)
    } else {
        return Err(LangError::new(
            format!("Unable to find global var with name: {}", name),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos),
        ));
    };

    let ir_type = if let Some(type_id) = built_in_call.ret_type {
        to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock(),
            type_id,
        )?
    } else {
        unreachable!("built-in call type id None: {:#?}", built_in_call);
    };

    let var_ptr_type = Type::Pointer(Box::new(ir_type));
    let var_ptr_instr = state.builder.var_address(var_idx, var_ptr_type);
    state.cur_block_mut()?.push(var_ptr_instr.clone());

    let load_instr = state.builder.load(var_ptr_instr.val).map_err(into_err)?;
    state.cur_block_mut()?.push(load_instr.clone());

    Ok(load_instr.val)
}

fn build_built_in_file(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    if let Some(file_info) = state
        .analyze_ctx
        .ast_ctx
        .file_info
        .get(&built_in_call.file_pos.file_nr)
    {
        let instr = state
            .builder
            .string_lit(&mut state.module, &file_info.filename);
        state.cur_block_mut()?.push(instr.clone());
        Ok(instr.val)
    } else {
        Err(LangError::new(
            format!(
                "Unable to find file info for file with nr {}",
                built_in_call.file_pos.file_nr
            ),
            LangErrorKind::IrError,
            Some(built_in_call.file_pos),
        ))
    }
}

fn build_built_in_line(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    let instr = state
        .builder
        .i32(&built_in_call.file_pos.line_start.to_string());
    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

fn build_built_in_column(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    let instr = state
        .builder
        .i32(&built_in_call.file_pos.column_start.to_string());
    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

fn build_built_in_unreachable(state: &mut BuildState) -> LangResult<Val> {
    let end_instr = state.builder.unreachable();
    state.cur_block_mut()?.set_end_instr(end_instr);
    Ok(VAL_EMPTY)
}

/// This function creates a `std::string::String` variable and appends all
/// the given format arguments to it.
///
/// The arguments to the `@format()` call are expected to be of types
/// `std::string::StringView`. If they aren't, this function will try to create
/// StringView's from the values if possible. Currently this is only done for
/// a subset of primitive types.
fn build_built_in_format(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    let file_pos = Some(built_in_call.file_pos);

    let format_parts = if let Some(format_parts) = &built_in_call.format_parts {
        format_parts
    } else {
        return Err(LangError::new(
            format!(
                "Found no `format_parts` in @format() call at pos: {:#?}",
                built_in_call
            ),
            LangErrorKind::IrError,
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
                Some(type_id_u32(&mut state.analyze_ctx.ty_env.lock(), file_pos)?),
                file_pos,
            ),
        )],
        None,
        None,
    );
    string_init_call.is_method = true;
    string_init_call.method_adt = Some(type_id_string(
        &mut state.analyze_ctx.ty_env.lock(),
        file_pos,
    )?);
    string_init_call.ret_type = Some(type_id_result_string(
        &mut state.analyze_ctx.ty_env.lock(),
        file_pos,
    )?);

    let mut un_op_address = UnOp::new(
        UnOperator::Address,
        Box::new(Expr::FnCall(string_init_call)),
        false,
        file_pos,
    );
    un_op_address.ret_type = Some(type_id_string_ptr(
        &mut state.analyze_ctx.ty_env.lock(),
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
        &mut state.analyze_ctx.ty_env.lock(),
        file_pos,
    )?);
    string_get_success_call.ret_type = Some(type_id_string(
        &mut state.analyze_ctx.ty_env.lock(),
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
            &mut state.analyze_ctx.ty_env.lock(),
            file_pos,
        )?),
        None,
        None,
        None,
        None,
        false,
    )));
    let string_var_expr = Expr::Var(string_var.read().clone());

    let string_ir_type = ir_type_string(&state.analyze_ctx.ty_env.lock());
    state.add_local_to_cur_func(
        var_name.clone(),
        state.cur_block_id,
        string_ir_type,
        VarModifier::None,
    )?;

    let string_var_decl = Stmt::VariableDecl(Arc::clone(&string_var), None);
    let string_var_assign = Stmt::Assignment(
        AssignOperator::Assignment,
        Expr::Var(string_var.read().clone()),
        Expr::FnCall(string_get_success_call),
        file_pos,
    );

    // Need to insert the variable declaration into the look-up tables so that
    // other parts of the CodeGen can find it during generation.
    let var_key = (var_name.clone(), state.cur_block_id);
    state
        .analyze_ctx
        .ast_ctx
        .variables
        .insert(var_key, string_var);

    build_stmt(state, &string_var_decl)?;
    build_stmt(state, &string_var_assign)?;

    let mut string_var_address = UnOp::new(
        UnOperator::Address,
        Box::new(string_var_expr.clone()),
        false,
        file_pos,
    );
    string_var_address.ret_type = Some(type_id_string_ptr(
        &mut state.analyze_ctx.ty_env.lock(),
        file_pos,
    )?);

    let string_var_ptr = Expr::Op(Op::UnOp(string_var_address));

    let mut arg_idx = 0;
    for format_part in format_parts {
        let expr = match format_part {
            FormatPart::String(str_lit) => str_lit_to_string_view(state, str_lit, file_pos)?,
            FormatPart::Arg(expr) => {
                let type_id = expr.get_expr_type()?;
                let is_primitive = is_primitive(&state.analyze_ctx.ty_env.lock(), type_id)?;
                if is_primitive {
                    let expr = primitive_to_string_view(state, expr, &var_name, arg_idx)?;
                    arg_idx += 1;
                    expr
                } else {
                    expr.clone()
                }
            }
        };
        append_view_to_string(state, string_var_ptr.clone(), expr, &std_module)?;
    }

    build_expr(state, &string_var_expr, ExprTy::RValue)
}

fn append_view_to_string(
    state: &mut BuildState,
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
        &mut state.analyze_ctx.ty_env.lock(),
        file_pos,
    )?);
    string_append_view_call.ret_type = Some(type_id_result_u32(
        &mut state.analyze_ctx.ty_env.lock(),
        file_pos,
    )?);

    build_fn_call(state, &string_append_view_call)?;
    Ok(())
}

fn str_lit_to_string_view(
    state: &mut BuildState,
    str_lit: &str,
    file_pos: Option<FilePosition>,
) -> LangResult<Expr> {
    Ok(Expr::Lit(
        Lit::String(str_lit.into(), StringType::Regular),
        Some(type_id_string_view(
            &mut state.analyze_ctx.ty_env.lock(),
            file_pos,
        )?),
        file_pos,
    ))
}

fn primitive_to_string_view(
    state: &mut BuildState,
    primitive_expr: &Expr,
    var_name: &str,
    arg_idx: usize,
) -> LangResult<Expr> {
    let file_pos = primitive_expr.file_pos().cloned();
    let type_id = primitive_expr.get_expr_type()?;

    // A variable name that will be used to create temporary allocations
    // if needed. The name should be unique.
    let tmp_var_name = format!("{}_idx_{}", var_name, arg_idx);

    let inner_ty = get_inner(&state.analyze_ctx.ty_env.lock(), type_id)?.clone();
    if inner_ty.is_int() {
        int_to_string_view(state, primitive_expr, file_pos, &inner_ty, tmp_var_name)
    } else if inner_ty.is_bool() {
        bool_to_string_view(state, primitive_expr, file_pos, &inner_ty, tmp_var_name)
    } else if inner_ty.is_char() {
        panic!("TODO: format arg for char");
    } else {
        Err(LangError::new(
            format!(
                "Tried to format un-supported type. Given type: {} ({})",
                to_string_type_id(&state.analyze_ctx.ty_env.lock(), type_id)?,
                type_id
            ),
            LangErrorKind::IrError,
            file_pos,
        ))
    }
}

fn int_to_string_view(
    state: &mut BuildState,
    int_expr: &Expr,
    file_pos: Option<FilePosition>,
    inner_ty: &InnerTy,
    buf_var_name: String,
) -> LangResult<Expr> {
    let mut ty_env_guard = state.analyze_ctx.ty_env.lock();

    // TODO: What buffer size should floats have? What is their max char size?
    // `buf_size` is the max amount of bytes that a given primitive type
    // value can occupy in string form.
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
        _ => unreachable!("bad type in int_to_string_view: {:#?}", int_expr),
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

    let buf_var = Arc::new(RwLock::new(Var::new(
        buf_var_name.clone(),
        Some(arr_type_id),
        None,
        None,
        None,
        None,
        false,
    )));
    let buf_var_decl = Stmt::VariableDecl(Arc::clone(&buf_var), None);

    let buf_var_key = (buf_var_name.clone(), state.cur_block_id);
    state
        .analyze_ctx
        .ast_ctx
        .variables
        .insert(buf_var_key, Arc::clone(&buf_var));

    let ir_type = to_ir_type(&state.analyze_ctx.ast_ctx, &ty_env_guard, arr_type_id)?;
    state.add_local_to_cur_func(buf_var_name, state.cur_block_id, ir_type, VarModifier::None)?;

    std::mem::drop(ty_env_guard);

    build_stmt(state, &buf_var_decl)?;

    let mut ty_env_guard = state.analyze_ctx.ty_env.lock();

    let mut buf_var_address = UnOp::new(
        UnOperator::Address,
        Box::new(Expr::Var(buf_var.read().clone())),
        false,
        None,
    );
    buf_var_address.ret_type = Some(arr_ptr_type_id);

    let mut int_to_string_view_call = FnCall::new(
        "to_string_view".into(),
        "".into(),
        vec![
            Argument::new(None, None, int_expr.clone()),
            Argument::new(None, None, Expr::Op(Op::UnOp(buf_var_address))),
        ],
        None,
        None,
    );
    int_to_string_view_call.is_method = true;
    int_to_string_view_call.method_adt = Some(primitive_type_id);
    int_to_string_view_call.ret_type = Some(type_id_string_view(&mut ty_env_guard, file_pos)?);

    Ok(Expr::FnCall(int_to_string_view_call))
}

/// The given `inner_ty` MUST be a boolean type.
fn bool_to_string_view(
    state: &mut BuildState,
    bool_expr: &Expr,
    file_pos: Option<FilePosition>,
    inner_ty: &InnerTy,
    var_name: String,
) -> LangResult<Expr> {
    let mut ty_env_guard = state.analyze_ctx.ty_env.lock();

    if !inner_ty.is_bool() {
        unreachable!("bad type in int_to_string_view: {:#?}", bool_expr);
    }

    let bool_type_id = ty_env_guard.id(&Ty::CompoundType(
        InnerTy::Boolean,
        TypeInfo::DefaultOpt(file_pos),
    ))?;
    let bool_ptr_type_id =
        ty_env_guard.id(&Ty::Pointer(bool_type_id, TypeInfo::DefaultOpt(file_pos)))?;

    let bool_adt_type_id = ty_env_guard.id(&Ty::CompoundType(
        InnerTy::Struct(inner_ty.get_primitive_ident().into()),
        TypeInfo::DefaultOpt(file_pos),
    ))?;

    // TODO: Fix this ugly hack.
    // Since `AsView` takes a pointer to the value that it converts to a view,
    // we need to temporarily allocate the bool in a variable so that we can
    // get a pointer to it.
    let var = Arc::new(RwLock::new(Var::new(
        var_name.clone(),
        Some(bool_type_id),
        None,
        Some(Box::new(bool_expr.clone())),
        None,
        None,
        false,
    )));
    let var_decl = Stmt::VariableDecl(Arc::clone(&var), None);

    let var_key = (var_name.clone(), state.cur_block_id);
    state
        .analyze_ctx
        .ast_ctx
        .variables
        .insert(var_key, Arc::clone(&var));

    let ir_type = to_ir_type(&state.analyze_ctx.ast_ctx, &ty_env_guard, bool_type_id)?;
    state.add_local_to_cur_func(var_name, state.cur_block_id, ir_type, VarModifier::None)?;

    std::mem::drop(ty_env_guard);

    build_stmt(state, &var_decl)?;

    let mut ty_env_guard = state.analyze_ctx.ty_env.lock();

    let mut var_address = UnOp::new(
        UnOperator::Address,
        Box::new(Expr::Var(var.read().clone())),
        false,
        None,
    );
    var_address.ret_type = Some(bool_ptr_type_id);

    let mut bool_to_string_view_call = FnCall::new(
        "as_view".into(),
        "".into(),
        vec![Argument::new(
            Some("this".into()),
            None,
            Expr::Op(Op::UnOp(var_address)),
        )],
        None,
        None,
    );
    bool_to_string_view_call.is_method = true;
    bool_to_string_view_call.method_adt = Some(bool_adt_type_id);
    bool_to_string_view_call.ret_type = Some(type_id_string_view(&mut ty_env_guard, file_pos)?);

    Ok(Expr::FnCall(bool_to_string_view_call))
}

fn type_id_u8(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    ty_env.id(&Ty::CompoundType(
        InnerTy::U8,
        TypeInfo::DefaultOpt(file_pos),
    ))
}

fn type_id_u32(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    ty_env.id(&Ty::CompoundType(
        InnerTy::U32,
        TypeInfo::DefaultOpt(file_pos),
    ))
}

fn type_id_string(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    let string_path = ["std".into(), "string".into(), "String".into()].into();
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(string_path),
        TypeInfo::DefaultOpt(file_pos),
    ))
}

fn ir_type_string(ty_env: &TyEnv) -> Type {
    let string_path = ["std".into(), "string".into(), "String".into()].into();
    let adt_name = to_string_path(ty_env, &string_path);
    Type::Adt(adt_name)
}

fn type_id_string_ptr(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    let string_type_id = type_id_string(ty_env, file_pos)?;
    ty_env.id(&Ty::Pointer(string_type_id, TypeInfo::DefaultOpt(file_pos)))
}

fn type_id_string_view(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    let view_path = ["std".into(), "string".into(), "StringView".into()].into();
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(view_path),
        TypeInfo::DefaultOpt(file_pos),
    ))
}

fn type_id_result_string(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    let result_path: LangPath = ["std".into(), "Result".into()].into();
    let mut gens = Generics::new();
    gens.insert("T".into(), type_id_string(ty_env, file_pos)?);
    gens.insert("E".into(), type_id_string_view(ty_env, file_pos)?);
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(result_path.with_gens(gens)),
        TypeInfo::DefaultOpt(file_pos),
    ))
}

fn type_id_result_u32(ty_env: &mut TyEnv, file_pos: Option<FilePosition>) -> LangResult<TypeId> {
    let result_path: LangPath = ["std".into(), "Result".into()].into();
    let mut gens = Generics::new();
    gens.insert("T".into(), type_id_u32(ty_env, file_pos)?);
    gens.insert("E".into(), type_id_string_view(ty_env, file_pos)?);
    ty_env.id(&Ty::CompoundType(
        InnerTy::Struct(result_path.with_gens(gens)),
        TypeInfo::DefaultOpt(file_pos),
    ))
}
