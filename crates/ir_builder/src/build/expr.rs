use log::debug;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        block::AdtKind,
        expr::{AdtInit, ArrayInit, Expr, FnCall, FnPtr, Var},
        lit::{Lit, StringType},
        op::Op,
    },
    ty::{get::get_inner, inner_ty::InnerTy, to_string::to_string_path, type_id::TypeId},
};
use ir::{instr::ExprInstr, ty::Type, Data, DataIdx, ExprTy, Val};

use crate::{
    build::{
        built_in::build_built_in_call,
        op::{build_bin_op, build_un_op},
    },
    into_err,
    state::BuildState,
    to_ir_type,
};

pub fn build_expr(state: &mut BuildState, expr: &Expr, expr_ty: ExprTy) -> LangResult<Val> {
    debug!(
        "build_expr -- expr_ty: {:?} expr: {:#?}, file_pos: {:#?}",
        expr_ty,
        expr,
        expr.file_pos().cloned()
    );

    match expr {
        Expr::Lit(lit, type_id_opt, ..) => build_lit(state, lit, type_id_opt.as_ref()),
        Expr::Var(var) => match expr_ty {
            ExprTy::LValue => build_var_address(state, var),
            ExprTy::RValue => build_var_address_with_load(state, var),
        },
        Expr::FnCall(fn_call) => build_fn_call(state, fn_call),
        Expr::FnPtr(fn_ptr) => build_fn_ptr(state, fn_ptr),
        Expr::BuiltInCall(built_in_call) => build_built_in_call(state, built_in_call),
        Expr::AdtInit(adt_init) => match adt_init.kind {
            AdtKind::Struct => build_struct_init(state, adt_init),
            AdtKind::Union => build_union_init(state, adt_init),
            _ => panic!("Tried to compile AdtInit for kind: {:?}", adt_init.kind),
        },
        Expr::ArrayInit(array_init) => build_array_init(state, array_init),
        Expr::Op(Op::BinOp(bin_op)) if matches!(expr_ty, ExprTy::LValue) => {
            return Err(LangError::new(
                format!("Bin op not allowed in lvalue: {:?}", bin_op),
                LangErrorKind::IrError,
                bin_op.file_pos,
            ))
        }
        Expr::Op(Op::BinOp(bin_op)) => build_bin_op(state, bin_op),
        Expr::Op(Op::UnOp(un_op)) => build_un_op(state, un_op, expr_ty),

        Expr::Block(_, _) => todo!("build_expr: Block"),

        // TODO: How should this be handled? Where can one specify a type as an
        //       expression? What do we need to do here? Can we ignore it?
        Expr::Type(..) => todo!(),
    }
}

fn build_lit(state: &mut BuildState, lit: &Lit, type_id_opt: Option<&TypeId>) -> LangResult<Val> {
    let expr_instr = match lit {
        Lit::String(str_lit, string_type) => build_lit_string(state, str_lit, string_type)?,
        Lit::Char(char_lit) => {
            if char_lit.chars().count() != 1 {
                return Err(LangError::new(
                    format!("Char lit not one character: {:#?}", char_lit),
                    LangErrorKind::IrError,
                    None,
                ));
            }
            state.builder.char_lit(char_lit.chars().next().unwrap())
        }
        Lit::Bool(bool_lit) => {
            if *bool_lit {
                state.builder.bool_true()
            } else {
                state.builder.bool_false()
            }
        }
        Lit::Integer(int_lit, radix) => build_lit_int(state, int_lit, *radix, type_id_opt)?,
        Lit::Float(float_lit) => build_lit_float(state, float_lit, type_id_opt)?,
    };

    state.cur_block_mut()?.push(expr_instr.clone());
    Ok(expr_instr.val)
}

fn build_lit_string(
    state: &mut BuildState,
    str_lit: &str,
    string_type: &StringType,
) -> LangResult<ExprInstr> {
    let str_len = str_lit.len();
    match string_type {
        StringType::C => {
            let data_idx = build_data(state, str_lit, true);
            let data_type = Type::Pointer(Box::new(Type::U8));
            let data_instr = state.builder.data_address(data_idx, data_type);
            state.cur_block_mut()?.push(data_instr.clone());

            state.builder.load(data_instr.val).map_err(into_err)
        }

        StringType::Regular => {
            let data_idx = build_data(state, str_lit, false);
            let data_type = Type::Pointer(Box::new(Type::U8));
            let data_instr = state.builder.data_address(data_idx, data_type);
            state.cur_block_mut()?.push(data_instr.clone());

            let data_ptr_instr = state.builder.load(data_instr.val).map_err(into_err)?;
            state.cur_block_mut()?.push(data_ptr_instr.clone());

            let len_instr = state.builder.u32(&str_len.to_string());
            state.cur_block_mut()?.push(len_instr.clone());

            let view_path = ["std".into(), "string".into(), "StringView".into()].into();
            let view_name = to_string_path(&state.analyze_ctx.ty_env.lock(), &view_path);
            let view_args = [data_ptr_instr.val, len_instr.val];

            state
                .builder
                .struct_init(&mut state.module, &view_name, &view_args)
                .map_err(into_err)
        }

        StringType::F | StringType::S => {
            let data_idx = build_data(state, str_lit, false);
            let data_type = Type::Pointer(Box::new(Type::U8));
            let data_instr = state.builder.data_address(data_idx, data_type);
            state.cur_block_mut()?.push(data_instr.clone());

            let data_ptr_instr = state.builder.load(data_instr.val).map_err(into_err)?;
            state.cur_block_mut()?.push(data_ptr_instr.clone());

            let len_instr = state.builder.u32(&str_len.to_string());
            state.cur_block_mut()?.push(len_instr.clone());

            let list_path = ["std".into(), "collection".into(), "List".into()].into();
            let list_name = to_string_path(&state.analyze_ctx.ty_env.lock(), &list_path);
            let list_args = [data_ptr_instr.val, len_instr.val.clone(), len_instr.val];

            let list_init_instr = state
                .builder
                .struct_init(&mut state.module, &list_name, &list_args)
                .map_err(into_err)?;
            state.cur_block_mut()?.push(list_init_instr.clone());

            let string_path = ["std".into(), "string".into(), "String".into()].into();
            let string_name = to_string_path(&state.analyze_ctx.ty_env.lock(), &string_path);
            let string_args = [list_init_instr.val];

            state
                .builder
                .struct_init(&mut state.module, &string_name, &string_args)
                .map_err(into_err)
        }
    }
}

fn build_data(state: &mut BuildState, str_lit: &str, null_terminated: bool) -> DataIdx {
    state.module.add_data(Data::StringLit(if null_terminated {
        format!("{}\0", str_lit)
    } else {
        str_lit.into()
    }))
}

/// Converts the given integer literal `int_lit` written in radix `radix` into
/// a integer literal written in radix 10.
macro_rules! parse_with_radix {
    ($int_lit:expr, $radix:expr, $t:ty) => {
        &<$t>::from_str_radix($int_lit, $radix)?.to_string()
    };
}

fn build_lit_int(
    state: &mut BuildState,
    int_lit: &str,
    radix: u32,
    type_id_opt: Option<&TypeId>,
) -> LangResult<ExprInstr> {
    let inner_ty = if let Some(type_id) = type_id_opt {
        let ty_env_guard = state.analyze_ctx.ty_env.lock();
        let fwd_type_id = ty_env_guard.forwarded(*type_id);
        get_inner(&ty_env_guard, fwd_type_id)?.clone()
    } else {
        return Err(LangError::new(
            format!("Type for int literal not set: {:#?}", int_lit),
            LangErrorKind::IrError,
            None,
        ));
    };

    Ok(match inner_ty {
        InnerTy::I8 => state.builder.i8(parse_with_radix!(int_lit, radix, i8)),
        InnerTy::U8 => state.builder.u8(parse_with_radix!(int_lit, radix, u8)),
        InnerTy::I16 => state.builder.i16(parse_with_radix!(int_lit, radix, i16)),
        InnerTy::U16 => state.builder.u16(parse_with_radix!(int_lit, radix, u16)),
        InnerTy::I32 => state.builder.i32(parse_with_radix!(int_lit, radix, i32)),
        InnerTy::U32 => state.builder.u32(parse_with_radix!(int_lit, radix, u32)),
        InnerTy::I64 => state.builder.i64(parse_with_radix!(int_lit, radix, i64)),
        InnerTy::U64 => state.builder.u64(parse_with_radix!(int_lit, radix, u64)),
        InnerTy::I128 => todo!("build_lit_int: i128 not supported"),
        InnerTy::U128 => todo!("build_lit_int: u128 not supported"),
        _ => {
            return Err(LangError::new(
                format!(
                    "Invalid literal integer type. Type ID: {:?}, inner_ty: {:?}",
                    type_id_opt, inner_ty
                ),
                LangErrorKind::IrError,
                None,
            ))
        }
    })
}

fn build_lit_float(
    state: &mut BuildState,
    float_lit: &str,
    type_id_opt: Option<&TypeId>,
) -> LangResult<ExprInstr> {
    let inner_ty = if let Some(type_id) = type_id_opt {
        let ty_env_guard = state.analyze_ctx.ty_env.lock();
        let fwd_type_id = ty_env_guard.forwarded(*type_id);
        get_inner(&ty_env_guard, fwd_type_id)?.clone()
    } else {
        return Err(LangError::new(
            format!("Type for float literal not set: {:#?}", float_lit),
            LangErrorKind::IrError,
            None,
        ));
    };

    Ok(match inner_ty {
        InnerTy::F32 => state.builder.f32(float_lit),
        InnerTy::F64 => state.builder.f64(float_lit),
        _ => {
            return Err(LangError::new(
                format!(
                    "Invalid literal float type. Type ID: {:?}, inner_ty: {:?}",
                    type_id_opt, inner_ty
                ),
                LangErrorKind::IrError,
                None,
            ))
        }
    })
}

fn build_var_address(state: &mut BuildState, var: &Var) -> LangResult<Val> {
    let var_idx = state.get_var(&var.full_name(), state.cur_block_id)?;
    let var_type = to_ir_type(
        &state.analyze_ctx.ast_ctx,
        &state.analyze_ctx.ty_env.lock(),
        var.ty.unwrap(),
    )?;

    let instr = state.builder.var_address(var_idx, var_type);
    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

fn build_var_address_with_load(state: &mut BuildState, var: &Var) -> LangResult<Val> {
    let var_ptr = build_var_address(state, var)?;
    let instr = state.builder.load(var_ptr).map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

pub(crate) fn build_fn_call(state: &mut BuildState, fn_call: &FnCall) -> LangResult<Val> {
    let mut arg_vals = Vec::with_capacity(fn_call.arguments.len());
    for arg in &fn_call.arguments {
        let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;
        arg_vals.push(arg_val);
    }

    let expr_instr = if fn_call.is_fn_ptr_call {
        let fn_ptr_var_idx = state.get_var(&fn_call.name, state.cur_block_id)?;
        state
            .builder
            .fn_ptr_call(&mut state.module, fn_ptr_var_idx, &[])
            .map_err(into_err)?
    } else {
        let fn_full_name = fn_call.full_name(&state.analyze_ctx.ty_env.lock())?;
        state
            .builder
            .fn_call(&mut state.module, &fn_full_name, &arg_vals)
            .map_err(into_err)?
    };

    state.cur_block_mut()?.push(expr_instr.clone());
    Ok(expr_instr.val)
}

fn build_fn_ptr(state: &mut BuildState, fn_ptr: &FnPtr) -> LangResult<Val> {
    let func_path =
        fn_ptr
            .module
            .clone_push(&fn_ptr.name, fn_ptr.generics.as_ref(), fn_ptr.file_pos);
    let func_full_name = to_string_path(&state.analyze_ctx.ty_env.lock(), &func_path);

    if state.module.get_func(&func_full_name).is_none() {
        return Err(LangError::new(
            format!(
                "Unable to find function with name \"{}\" for fn ptr.",
                &func_full_name,
            ),
            LangErrorKind::IrError,
            fn_ptr.file_pos,
        ));
    };

    let fn_ptr_instr = state
        .builder
        .fn_ptr(&mut state.module, &func_full_name)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(fn_ptr_instr.clone());

    Ok(fn_ptr_instr.val)
}

fn build_struct_init(state: &mut BuildState, struct_init: &AdtInit) -> LangResult<Val> {
    let full_name = struct_init.full_name(&state.analyze_ctx.ty_env.lock())?;

    let member_types = if let Some(member_types) = state.module.get_struct(&full_name) {
        member_types
    } else {
        return Err(LangError::new(
            format!(
                "Unable to find struct with name \"{}\". Struct init: {:#?}",
                full_name, struct_init
            ),
            LangErrorKind::IrError,
            struct_init.file_pos,
        ));
    };

    if struct_init.arguments.len() != member_types.len() {
        return Err(LangError::new(
            format!(
                "Wrong amount of args given when init struct: {}. Expected: {}, got: {}",
                &struct_init.name,
                member_types.len(),
                struct_init.arguments.len()
            ),
            LangErrorKind::IrError,
            struct_init.file_pos,
        ));
    }

    // Compile all arguments (all values that will be set to initialize the
    // struct members).
    let mut arg_vals = Vec::with_capacity(struct_init.arguments.len());
    for arg in &struct_init.arguments {
        let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;
        arg_vals.push(arg_val);
    }

    let struct_init_instr = state
        .builder
        .struct_init(&mut state.module, &full_name, &arg_vals)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(struct_init_instr.clone());

    Ok(struct_init_instr.val)
}

fn build_union_init(state: &mut BuildState, union_init: &AdtInit) -> LangResult<Val> {
    let full_path = union_init.module.clone_push(
        &union_init.name,
        union_init.generics.as_ref(),
        union_init.file_pos,
    );
    let full_name = union_init.full_name(&state.analyze_ctx.ty_env.lock())?;

    if union_init.arguments.len() != 1 {
        return Err(LangError::new(
            "Expected exactly one argument when init union.".into(),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    }

    let arg = if let Some(arg) = union_init.arguments.first() {
        arg
    } else {
        return Err(LangError::new(
            "Union init had no argument.".into(),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    };

    let arg_name = if let Some(arg_name) = &arg.name {
        arg_name
    } else {
        return Err(LangError::new(
            "Union init argument wasn't named.".into(),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    };

    let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;

    let member_idx = state.analyze_ctx.ast_ctx.get_adt_member_index(
        &state.analyze_ctx.ty_env.lock(),
        &full_path,
        arg_name,
    )?;
    // TODO: int/unit.
    let idx_instr = state.builder.u32(&member_idx.to_string());
    state.cur_block_mut()?.push(idx_instr.clone());

    let union_init_instr = state
        .builder
        .union_init(&mut state.module, &full_name, arg_val, idx_instr.val)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(union_init_instr.clone());

    Ok(union_init_instr.val)
}

fn build_array_init(state: &mut BuildState, array_init: &ArrayInit) -> LangResult<Val> {
    let args = &array_init.arguments;

    if args.is_empty() {
        return Err(LangError::new(
            "Array init with zero arguments.".into(),
            LangErrorKind::IrError,
            Some(array_init.file_pos),
        ));
    }

    // Compile all arguments (all values that will be set to initialize the
    // array members).
    let mut arg_vals = Vec::with_capacity(args.len());
    for arg in args.iter() {
        let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;
        arg_vals.push(arg_val);
    }

    let array_init_instr = state.builder.array_init(&arg_vals).map_err(into_err)?;
    state.cur_block_mut()?.push(array_init_instr.clone());

    Ok(array_init_instr.val)
}
