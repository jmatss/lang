use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        block::AdtKind,
        expr::{AdtInit, ArrayInit, Expr, FnCall, FnPtr},
        lit::Lit,
        op::Op,
    },
    ty::{get::get_inner, inner_ty::InnerTy, to_string::to_string_path, type_id::TypeId},
};
use ir::{
    decl::ty::Type,
    instruction::{ExprInstr, Instr, Lit as IrLit},
    Data, ExprTy, Val,
};
use log::debug;

use crate::{
    build::{
        built_in::build_built_in_call,
        op::{build_bin_op, build_un_op},
    },
    state::BuildState,
    to_ir_type,
};

pub fn build_expr(state: &mut BuildState, expr: &Expr, expr_ty: ExprTy) -> LangResult<Val> {
    let file_pos = expr.file_pos().cloned();

    debug!(
        "build_expr -- expr_ty: {:?} expr: {:#?}, file_pos: {:#?}",
        expr_ty, expr, file_pos
    );

    let value = match expr {
        Expr::Lit(lit, type_id_opt, ..) => build_lit(state, lit, type_id_opt.as_ref())?,
        Expr::Var(_) => todo!(),
        Expr::FnCall(fn_call) => build_fn_call(state, fn_call)?,
        Expr::FnPtr(fn_ptr) => build_fn_ptr(state, fn_ptr)?,
        Expr::BuiltInCall(built_in_call) => build_built_in_call(state, built_in_call)?,
        Expr::AdtInit(adt_init) => match adt_init.kind {
            AdtKind::Struct => build_struct_init(state, adt_init)?,
            AdtKind::Union => build_union_init(state, adt_init)?,
            _ => panic!("Tried to compile AdtInit for kind: {:?}", adt_init.kind),
        },
        Expr::ArrayInit(array_init) => build_array_init(state, array_init)?,
        Expr::Op(Op::BinOp(bin_op)) if matches!(expr_ty, ExprTy::LValue) => {
            return Err(LangError::new(
                format!("Bin op not allowed in lvalue: {:?}", bin_op),
                LangErrorKind::IrError,
                bin_op.file_pos,
            ))
        }
        Expr::Op(Op::BinOp(bin_op)) => build_bin_op(state, bin_op)?,
        Expr::Op(Op::UnOp(un_op)) => build_un_op(state, un_op, expr_ty)?,
        Expr::Block(_, _) => todo!(),

        // TODO: How should this be handled? Where can one specify a type as an
        //       expression? What do we need to do here? Can we ignore it?
        Expr::Type(..) => todo!(),
    };
}

fn build_lit(state: &mut BuildState, lit: &Lit, type_id_opt: Option<&TypeId>) -> LangResult<Val> {
    let val = state.new_val();
    let expr_instr = match lit {
        Lit::String(str_lit) => {
            let data_idx = state.module.add_data(Data::StringLit(str_lit.into()));
            ExprInstr::Lit(Type::String, IrLit::String(data_idx))
        }
        Lit::Char(ch_lit) => ExprInstr::Lit(Type::Character, IrLit::Char(ch_lit.into())),
        Lit::Bool(bool_lit) => ExprInstr::Lit(Type::Boolean, IrLit::Bool(*bool_lit)),
        Lit::Integer(int_lit, radix) => build_lit_int(state, int_lit, *radix, type_id_opt)?,
        Lit::Float(float_lit) => build_lit_float(state, float_lit, type_id_opt)?,
    };

    let instr = Instr::Expr(val, expr_instr);
    state.cur_basic_block_mut()?.push(instr);

    Ok(val)
}

fn build_lit_int(
    state: &mut BuildState,
    int_lit: &str,
    radix: u32,
    type_id_opt: Option<&TypeId>,
) -> LangResult<ExprInstr> {
    let inner_ty = if let Some(type_id) = type_id_opt {
        let ty_env_guard = state.analyze_ctx.ty_env.lock().unwrap();
        let fwd_type_id = ty_env_guard.forwarded(*type_id);
        get_inner(&ty_env_guard, fwd_type_id)?.clone()
    } else {
        return Err(LangError::new(
            format!("Type for int literal not set: {:#?}", int_lit),
            LangErrorKind::IrError,
            None,
        ));
    };

    let ir_type = match inner_ty {
        InnerTy::I8 => Type::I8,
        InnerTy::U8 => Type::U8,
        InnerTy::I16 => Type::I16,
        InnerTy::U16 => Type::I16,
        InnerTy::I32 => Type::I32,
        InnerTy::U32 => Type::U32,
        InnerTy::I64 => Type::I64,
        InnerTy::U64 => Type::U64,
        InnerTy::I128 => Type::I128,
        InnerTy::U128 => Type::U128,
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
    };

    Ok(ExprInstr::Lit(
        ir_type,
        IrLit::Integer(int_lit.into(), radix),
    ))
}

fn build_lit_float(
    state: &mut BuildState,
    float_lit: &str,
    type_id_opt: Option<&TypeId>,
) -> LangResult<ExprInstr> {
    let inner_ty = if let Some(type_id) = type_id_opt {
        let ty_env_guard = state.analyze_ctx.ty_env.lock().unwrap();
        let fwd_type_id = ty_env_guard.forwarded(*type_id);
        get_inner(&ty_env_guard, fwd_type_id)?.clone()
    } else {
        return Err(LangError::new(
            format!("Type for float literal not set: {:#?}", float_lit),
            LangErrorKind::IrError,
            None,
        ));
    };

    let ir_type = match inner_ty {
        InnerTy::F32 => Type::F32,
        InnerTy::F64 => Type::F64,
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
    };

    Ok(ExprInstr::Lit(ir_type, IrLit::Float(float_lit.into())))
}

fn build_fn_call(state: &mut BuildState, fn_call: &FnCall) -> LangResult<Val> {
    let mut arg_vals = Vec::with_capacity(fn_call.arguments.len());
    for arg in &fn_call.arguments {
        let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;
        arg_vals.push(arg_val);
    }

    let fn_call_ret_val = if fn_call.is_fn_ptr_call {
        let fn_ptr_var_idx = state.get_var(&fn_call.name, state.cur_block_id)?;

        let fn_ptr_val = state.new_val();
        let fn_ptr_instr = Instr::Expr(fn_ptr_val, ExprInstr::Var(fn_ptr_var_idx));
        state.cur_basic_block_mut()?.push(fn_ptr_instr);

        let fn_call_val = state.new_val();
        let fn_call_instr = Instr::Expr(fn_call_val, ExprInstr::FnPtrCall(fn_ptr_val, arg_vals));
        state.cur_basic_block_mut()?.push(fn_call_instr);

        fn_call_val
    } else {
        let func_path =
            fn_call
                .module
                .clone_push(&fn_call.name, fn_call.generics.as_ref(), fn_call.file_pos);
        let func_full_name = to_string_path(&state.analyze_ctx.ty_env.lock().unwrap(), &func_path);

        if state.module.get_function(&func_full_name).is_none() {
            return Err(LangError::new(
                format!(
                    "Unable to find function with name \"{}\" to call.",
                    &func_full_name,
                ),
                LangErrorKind::IrError,
                fn_call.file_pos,
            ));
        };

        let fn_call_val = state.new_val();
        let fn_call_instr = Instr::Expr(fn_call_val, ExprInstr::FnCall(func_full_name, arg_vals));

        fn_call_val
    };

    Ok(fn_call_ret_val)
}

fn build_fn_ptr(state: &mut BuildState, fn_ptr: &FnPtr) -> LangResult<Val> {
    let func_path =
        fn_ptr
            .module
            .clone_push(&fn_ptr.name, fn_ptr.generics.as_ref(), fn_ptr.file_pos);
    let func_full_name = to_string_path(&state.analyze_ctx.ty_env.lock().unwrap(), &func_path);

    if state.module.get_function(&func_full_name).is_none() {
        return Err(LangError::new(
            format!(
                "Unable to find function with name \"{}\" for fn ptr.",
                &func_full_name,
            ),
            LangErrorKind::IrError,
            fn_ptr.file_pos,
        ));
    };

    let fn_ptr_val = state.new_val();
    let fn_ptr_instr = Instr::Expr(fn_ptr_val, ExprInstr::FnPtr(func_full_name));
    state.cur_basic_block_mut()?.push(fn_ptr_instr);

    Ok(fn_ptr_val)
}

fn build_struct_init(state: &mut BuildState, struct_init: &AdtInit) -> LangResult<Val> {
    let full_name = struct_init.full_name(&state.analyze_ctx.ty_env.lock().unwrap())?;

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

    let struct_init_val = state.new_val();
    let struct_ir_type = Type::Struct(full_name, member_types.clone());
    let struct_init_instr = Instr::Expr(
        struct_init_val,
        ExprInstr::StructInit(struct_ir_type, arg_vals),
    );
    state.cur_basic_block_mut()?.push(struct_init_instr);

    Ok(struct_init_val)
}

fn build_union_init(state: &mut BuildState, union_init: &AdtInit) -> LangResult<Val> {
    let full_path = union_init.module.clone_push(
        &union_init.name,
        union_init.generics.as_ref(),
        union_init.file_pos,
    );
    let full_name = union_init.full_name(&state.analyze_ctx.ty_env.lock().unwrap())?;

    if union_init.arguments.len() != 1 {
        return Err(LangError::new(
            format!("Expected exactly one argument when init union.",),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    }

    let arg = if let Some(arg) = union_init.arguments.first() {
        arg
    } else {
        return Err(LangError::new(
            format!("Union init had no argument.",),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    };

    let arg_name = if let Some(arg_name) = &arg.name {
        arg_name
    } else {
        return Err(LangError::new(
            format!("Union init argument wasn't named.",),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    };

    let member_idx = state.analyze_ctx.ast_ctx.get_adt_member_index(
        &state.analyze_ctx.ty_env.lock().unwrap(),
        &full_path,
        arg_name,
    )?;

    let member_types = if let Some(member_types) = state.module.get_union(&full_name) {
        member_types
    } else {
        return Err(LangError::new(
            format!(
                "Unable to find union with name \"{}\". Union init: {:#?}",
                full_name, union_init
            ),
            LangErrorKind::IrError,
            union_init.file_pos,
        ));
    };

    let union_init_val = state.new_val();
    let arg_val = build_expr(state, &arg.value, ExprTy::RValue)?;

    let union_ir_type = Type::Union(full_name, member_types.clone());
    let union_init_instr = Instr::Expr(
        union_init_val,
        ExprInstr::UnionInit(union_ir_type, member_idx as usize, arg_val),
    );
    state.cur_basic_block_mut()?.push(union_init_instr);

    Ok(union_init_val)
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

    let arr_inner_type_id = args.first().unwrap().value.get_expr_type()?;
    let arr_inner_type = Box::new(to_ir_type(
        &state.analyze_ctx.ast_ctx,
        &state.analyze_ctx.ty_env.lock().unwrap(),
        arr_inner_type_id,
    )?);
    let arr_ir_type = Type::Array(arr_inner_type, Some(arg_vals.len() as u32));

    let array_init_val = state.new_val();
    let array_init_instr = Instr::Expr(array_init_val, ExprInstr::ArrayInit(arr_ir_type, arg_vals));
    state.cur_basic_block_mut()?.push(array_init_instr);

    Ok(array_init_val)
}
