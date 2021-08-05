use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    token::{
        expr::{Expr, Var},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
    ty::ty_env::TyEnv,
};
use ir::{
    decl::ty::Type,
    instruction::{Cmp, EndInstr, ExprInstr, Instr, Lit, Signed, StmtInstr},
    ExprTy, Val,
};

use crate::{into_err, state::BuildState, to_ir_type};

use super::expr::build_expr;

// TODO: Implement logic to handle const.
pub fn build_bin_op(state: &mut BuildState, bin_op: &BinOp) -> LangResult<Val> {
    let file_pos = bin_op.file_pos.as_ref();

    let ret_type = if let Some(ret_type_id) = &bin_op.ret_type {
        to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock().unwrap(),
            *ret_type_id,
        )?
    } else {
        return Err(LangError::new(
            format!(
                "Type of bin_op \"{:?}\" not know when building bin_op.",
                &bin_op
            ),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ));
    };

    let lhs_val = build_expr(state, &bin_op.lhs, ExprTy::RValue)?;

    // Bool "and" and "or" should be short circuit, so they need to be
    // compiled before the right side is compiled. This is a temporary fix
    // to ensure that they are evaluated before the right hand side is
    // compiled. This logic should be merged better with the other operators.
    match bin_op.operator {
        BinOperator::BoolAnd => {
            return build_bin_op_bool_and(state, &ret_type, lhs_val, &bin_op.rhs, file_pos)
        }
        BinOperator::BoolOr => {
            return build_bin_op_bool_or(state, &ret_type, lhs_val, &bin_op.rhs, file_pos)
        }
        _ => (),
    }

    let rhs_val = build_expr(state, &bin_op.rhs, ExprTy::RValue)?;

    // Since lhs and rhs should be the same type when the signedness actual
    // matters, we can use either one of them to check it.
    let lhs_type = to_ir_type(
        &state.analyze_ctx.ast_ctx,
        &state.analyze_ctx.ty_env.lock().unwrap(),
        bin_op.lhs.get_expr_type()?,
    )?;
    let signed = if matches!(
        lhs_type,
        Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128
    ) {
        Signed::True
    } else {
        Signed::False
    };

    match bin_op.operator {
        BinOperator::In => todo!("TODO: In"),
        BinOperator::Is => todo!("TODO: Is"),
        BinOperator::As => build_bin_op_as(state, lhs_val, ret_type),
        BinOperator::Of => todo!("TODO: Of"),

        BinOperator::Range => todo!("TODO: Range"),
        BinOperator::RangeInclusive => todo!("TODO: RangeInclusive"),
        BinOperator::Dot => todo!("TODO: Dot"),

        BinOperator::Eq
        | BinOperator::Neq
        | BinOperator::Lt
        | BinOperator::Gt
        | BinOperator::Lte
        | BinOperator::Gte => {
            build_bin_op_cmp(state, &bin_op.operator, lhs_val, rhs_val, signed, file_pos)
        }

        BinOperator::Add
        | BinOperator::Sub
        | BinOperator::Mul
        | BinOperator::Div
        | BinOperator::Mod
        | BinOperator::BitAnd
        | BinOperator::BitOr
        | BinOperator::BitXor
        | BinOperator::ShiftLeft
        | BinOperator::ShiftRight => {
            build_bin_op_expr(state, &bin_op.operator, lhs_val, rhs_val, signed, file_pos)
        }

        BinOperator::BoolAnd | BinOperator::BoolOr => unreachable!("{:#?}", bin_op),
    }
}

pub fn build_un_op(state: &mut BuildState, un_op: &UnOp, expr_ty: ExprTy) -> LangResult<Val> {
    let file_pos = un_op.file_pos.as_ref();

    let ret_type = if let Some(ret_type_id) = &un_op.ret_type {
        to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock().unwrap(),
            *ret_type_id,
        )?
    } else {
        return Err(LangError::new(
            format!(
                "Type of un_op \"{:?}\" not know when building un_op.",
                &un_op
            ),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ));
    };

    // TODO: Should this check be moved to somewhere else?
    if let ExprTy::LValue = expr_ty {
        match &un_op.operator {
            UnOperator::Address
            | UnOperator::Positive
            | UnOperator::Negative
            | UnOperator::BitComplement
            | UnOperator::BoolNot => {
                return Err(LangError::new(
                    format!("Invalid un op in lvalue: {:?}", un_op),
                    LangErrorKind::IrError,
                    file_pos.cloned(),
                ))
            }
            _ => (),
        }
    }

    match un_op.operator {
        UnOperator::Deref => build_un_op_deref(state, un_op),
        UnOperator::Address => build_un_op_deref(state, un_op),
        UnOperator::Positive => build_un_op_pos(state, &un_op.value),
        UnOperator::Negative => build_un_op_neg(state, &un_op.value, ret_type),
        UnOperator::Increment => build_un_op_inc(state, &un_op.value),
        UnOperator::Decrement => build_un_op_dec(state, &un_op.value),
        UnOperator::ArrayAccess(_) => build_un_op_array_access(state, &un_op),
        UnOperator::UnionIs(_, _) => todo!(),
        UnOperator::AdtAccess(_, _) => todo!(),
        UnOperator::EnumAccess(_, _) => todo!(),
        UnOperator::BitComplement => todo!(),
        UnOperator::BoolNot => todo!(),
    }
}

fn build_bin_op_bool_and(
    state: &mut BuildState,
    ret_type: &Type,
    lhs_val: Val,
    rhs_expr: &Expr,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    if !matches!(ret_type, Type::Boolean) {
        return Err(LangError::new(
            format!("Invalid type for BinOp::BoolAnd: {:?}", ret_type),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ));
    }

    let start_block_name = state.cur_basic_block_mut()?.name.clone();

    // TODO: Make bool true/false constant vals so that we don't have to add an
    //       extra instruction for every one of them.
    // Add a `false` value into a val that will be used in the phi-instruction.
    let false_val = state.new_val();
    let true_instr = Instr::Expr(false_val, ExprInstr::Lit(Type::Boolean, Lit::Bool(false)));
    state.cur_basic_block_mut()?.push(true_instr);

    // The eval block is to evaluate the rhs only if the lhs
    // evaluates to true to allow for short circuit.
    let eval_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after("bool.and.rhs".into(), &start_block_name)
        .map_err(|e| into_err(e))?;
    let merge_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after("bool.and.merge".into(), &eval_block_name)
        .map_err(|e| into_err(e))?;

    let end_instr = EndInstr::BranchIf(lhs_val, eval_block_name, merge_block_name);
    state.cur_basic_block_mut()?.set_end_instruction(end_instr);

    state.set_cur_basic_block(Some(eval_block_name));
    let rhs_val = build_expr(state, rhs_expr, ExprTy::RValue)?;
    let end_instr = EndInstr::Branch(merge_block_name.clone());
    state.cur_basic_block_mut()?.set_end_instruction(end_instr);

    state.set_cur_basic_block(Some(merge_block_name));
    let phi_val = state.new_val();
    let phi_instr = Instr::Expr(
        phi_val,
        ExprInstr::Phi(vec![
            (start_block_name, false_val),
            (eval_block_name.clone(), rhs_val),
        ]),
    );
    state.cur_basic_block_mut()?.insert(0, phi_instr);

    state.set_cur_basic_block(Some(merge_block_name));
    Ok(phi_val)
}

fn build_bin_op_bool_or(
    state: &mut BuildState,
    ret_type: &Type,
    lhs_val: Val,
    rhs_expr: &Expr,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    if !matches!(ret_type, Type::Boolean) {
        return Err(LangError::new(
            format!("Invalid type for BinOp::BoolOr: {:?}", ret_type),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ));
    }

    let start_block_name = state.cur_basic_block_mut()?.name.clone();

    // TODO: Make bool true/false constant vals so that we don't have to add an
    //       extra instruction for every one of them.
    // Add a `true` value into a val that will be used in the phi-instruction.
    let true_val = state.new_val();
    let true_instr = Instr::Expr(true_val, ExprInstr::Lit(Type::Boolean, Lit::Bool(true)));
    state.cur_basic_block_mut()?.push(true_instr);

    // The eval block is to evaluate the rhs only if the lhs
    // evaluates to false to allow for short circuit.
    let eval_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after("bool.or.rhs".into(), &start_block_name)
        .map_err(|e| into_err(e))?;
    let merge_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after("bool.or.merge".into(), &eval_block_name)
        .map_err(|e| into_err(e))?;

    let end_instr = EndInstr::BranchIf(lhs_val, merge_block_name, eval_block_name);
    state.cur_basic_block_mut()?.set_end_instruction(end_instr);

    state.set_cur_basic_block(Some(eval_block_name));
    let rhs_val = build_expr(state, rhs_expr, ExprTy::RValue)?;
    let end_instr = EndInstr::Branch(merge_block_name.clone());
    state.cur_basic_block_mut()?.set_end_instruction(end_instr);

    state.set_cur_basic_block(Some(merge_block_name));
    let phi_val = state.new_val();
    let phi_instr = Instr::Expr(
        phi_val,
        ExprInstr::Phi(vec![
            (start_block_name, true_val),
            (eval_block_name.clone(), rhs_val),
        ]),
    );
    state.cur_basic_block_mut()?.insert(0, phi_instr);

    state.set_cur_basic_block(Some(merge_block_name));
    Ok(phi_val)
}

fn build_bin_op_as(state: &mut BuildState, lhs_val: Val, rhs_type: Type) -> LangResult<Val> {
    let cast_val = state.new_val();
    let instr = Instr::Expr(cast_val, ExprInstr::Cast(rhs_type, lhs_val));
    state.cur_basic_block_mut()?.push(instr);
    Ok(cast_val)
}

fn build_bin_op_cmp(
    state: &mut BuildState,
    oper: &BinOperator,
    lhs_val: Val,
    rhs_val: Val,
    signed: Signed,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    let cmp = match oper {
        BinOperator::Eq => Cmp::Eq,
        BinOperator::Neq => Cmp::Neq,
        BinOperator::Lt => Cmp::Lt,
        BinOperator::Gt => Cmp::Gt,
        BinOperator::Lte => Cmp::Lte,
        BinOperator::Gte => Cmp::Gte,
        _ => {
            return Err(LangError::new(
                format!("Invalid operator in int compare: {:?}", oper),
                LangErrorKind::IrError,
                file_pos.cloned(),
            ))
        }
    };

    let cmp_val = state.new_val();
    let instr = Instr::Expr(cmp_val, ExprInstr::Cmp(cmp, lhs_val, rhs_val, signed));
    state.cur_basic_block_mut()?.push(instr);
    Ok(cmp_val)
}

fn build_bin_op_expr(
    state: &mut BuildState,
    oper: &BinOperator,
    lhs_val: Val,
    rhs_val: Val,
    signed: Signed,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    let expr_instr = match oper {
        BinOperator::Add => ExprInstr::Add(lhs_val, rhs_val),
        BinOperator::Sub => ExprInstr::Sub(lhs_val, rhs_val),
        BinOperator::Mul => ExprInstr::Mul(lhs_val, rhs_val),
        BinOperator::Div => ExprInstr::Div(lhs_val, rhs_val, signed),
        BinOperator::Mod => ExprInstr::Mod(lhs_val, rhs_val, signed),
        BinOperator::BitAnd => ExprInstr::BitAnd(lhs_val, rhs_val),
        BinOperator::BitOr => ExprInstr::BitOr(lhs_val, rhs_val),
        BinOperator::BitXor => ExprInstr::BitXor(lhs_val, rhs_val),
        BinOperator::ShiftLeft => ExprInstr::Shl(lhs_val, rhs_val),
        BinOperator::ShiftRight => ExprInstr::Shr(lhs_val, rhs_val, signed),
        _ => {
            return Err(LangError::new(
                format!("Bad oper in build_bin_op_expr: {:#?}", oper),
                LangErrorKind::IrError,
                file_pos.cloned(),
            ));
        }
    };

    let ret_val = state.new_val();
    let instr = Instr::Expr(ret_val, expr_instr);
    state.cur_basic_block_mut()?.push(instr);
    Ok(ret_val)
}

// TODO: Need to make sure that we are actually derefencing a pointer (ensure
// that `ptr_val` is an actual pointer).
fn build_un_op_deref(state: &mut BuildState, un_op: &UnOp) -> LangResult<Val> {
    let ptr_val = build_expr(state, &un_op.value, ExprTy::LValue)?;
    let deref_val = state.new_val();
    let instr = Instr::Expr(deref_val, ExprInstr::Deref(ptr_val));
    state.cur_basic_block_mut()?.push(instr);
    Ok(deref_val)
}

fn build_un_op_address(state: &mut BuildState, un_op: &UnOp) -> LangResult<Val> {
    let val = build_expr(state, &un_op.value, ExprTy::LValue)?;
    let address_val = state.new_val();
    let instr = Instr::Expr(address_val, ExprInstr::Address(val));
    state.cur_basic_block_mut()?.push(instr);
    Ok(address_val)
}

// The `Positive` is "ignored", the expression is just compiled as usual.
fn build_un_op_pos(state: &mut BuildState, expr: &Expr) -> LangResult<Val> {
    build_expr(state, &expr, ExprTy::RValue)
}

// TODO: Is it ok to just evalulate the expression as usual and then just do
//       a `0 - expr` subtraction to get the negative number?
fn build_un_op_neg(state: &mut BuildState, expr: &Expr, ir_type: Type) -> LangResult<Val> {
    let zero_val = state.new_val();
    let zero_lit = ExprInstr::Lit(ir_type, Lit::Integer("10".into(), 10));
    let zero_instr = Instr::Expr(zero_val, zero_lit);
    state.cur_basic_block_mut()?.push(zero_instr);

    let sub_val = state.new_val();
    let expr_val = build_expr(state, &expr, ExprTy::RValue)?;
    let sub_instr = Instr::Expr(sub_val, ExprInstr::Sub(zero_val, expr_val));
    state.cur_basic_block_mut()?.push(sub_instr);

    Ok(sub_val)
}

fn build_un_op_inc(state: &mut BuildState, expr: &Expr) -> LangResult<Val> {
    let ptr_val = build_expr(state, expr, ExprTy::LValue)?;
    let inc_val = state.new_val();
    let inc_instr = Instr::Expr(inc_val, ExprInstr::Inc(ptr_val));
    state.cur_basic_block_mut()?.push(inc_instr);
    Ok(inc_val)
}

fn build_un_op_dec(state: &mut BuildState, expr: &Expr) -> LangResult<Val> {
    let ptr_val = build_expr(state, expr, ExprTy::LValue)?;
    let dec_val = state.new_val();
    let dec_instr = Instr::Expr(dec_val, ExprInstr::Dec(ptr_val));
    state.cur_basic_block_mut()?.push(dec_instr);
    Ok(dec_val)
}

//build_un_op_array_access(state, &un_op),
/// Access a member of an array. This function will return a pointer
/// to a allocation containing the actual value. If one wants the pointer
/// to the value or the value itself is up to the caller.
fn build_un_op_array_access(
    state: &mut BuildState,
    un_op: &UnOp,
    expr_ty: ExprTy,
) -> LangResult<Val> {
    let dim = if let UnOperator::ArrayAccess(dim) = &un_op.operator {
        dim
    } else {
        unreachable!("{:#?}", un_op);
    };

    let ptr_val = build_expr(state, &un_op.value, ExprTy::LValue)?;

    let ptr_val = build_expr(state, expr, ExprTy::LValue)?;
    let dec_val = state.new_val();
    let dec_instr = Instr::Expr(dec_val, ExprInstr::Dec(ptr_val));
    state.cur_basic_block_mut()?.push(dec_instr);
    Ok(dec_val)
}
