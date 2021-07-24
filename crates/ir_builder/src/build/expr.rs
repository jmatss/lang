use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        expr::{Expr, FnCall},
        lit::Lit,
    },
    ty::{get::get_inner, inner_ty::InnerTy, ty::Ty, type_id::TypeId},
};
use ir::{
    decl::ty::Type,
    instruction::{ExprInstr, Instr, Lit as IrLit},
    Data, ExprTy, Val,
};
use log::debug;

use crate::state::BuildState;

fn build_expr(state: &mut BuildState, expr: &Expr, expr_ty: ExprTy) -> LangResult<Val> {
    let file_pos = expr.file_pos().cloned();

    debug!(
        "build_expr -- expr_ty: {:?} expr: {:#?}, file_pos: {:#?}",
        expr_ty, expr, file_pos
    );

    let value = match expr {
        Expr::Lit(lit, type_id_opt, ..) => build_lit(state, lit, type_id_opt.as_ref())?,
        Expr::Var(_) => todo!(),
        Expr::FnCall(fn_call) => build_fn_call(state, fn_call),
        Expr::FnPtr(_) => todo!(),
        Expr::BuiltInCall(_) => todo!(),
        Expr::AdtInit(_) => todo!(),
        Expr::ArrayInit(_) => todo!(),
        Expr::Op(_) => todo!(),
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

    let a = if fn_call.is_fn_ptr_call {};
}
