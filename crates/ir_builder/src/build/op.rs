use either::Either;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    token::{
        block::AdtKind,
        expr::Expr,
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    ty::{get::get_ident, to_string::to_string_path},
};
use ir::{ExprTy, Signed, Type, Val, VarIdx};

use crate::{into_err, state::BuildState, to_ir_type};

use super::{
    expr::build_expr,
    stmt::{build_var_decl, build_var_store},
};

// TODO: Implement logic to handle const.
pub fn build_bin_op(state: &mut BuildState, bin_op: &BinOp) -> LangResult<Val> {
    let file_pos = bin_op.file_pos.as_ref();

    let ret_type = if let Some(ret_type_id) = &bin_op.ret_type {
        to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock(),
            state.module.ptr_size,
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

    // Since lhs and rhs should be the same type when the signedness actual
    // matters, we can use either one of them to check it.
    let lhs_type = to_ir_type(
        &state.analyze_ctx.ast_ctx,
        &state.analyze_ctx.ty_env.lock(),
        state.module.ptr_size,
        bin_op.lhs.get_expr_type()?,
    )?;
    let signed = if lhs_type.is_signed() {
        Signed::True
    } else {
        Signed::False
    };

    match bin_op.operator {
        BinOperator::In => todo!("TODO: In"),
        BinOperator::Is => todo!("TODO: Is"),
        BinOperator::As => build_bin_op_cast(state, lhs_val, ret_type),
        BinOperator::Of => todo!("TODO: Of"),

        BinOperator::Range => todo!("TODO: Range"),
        BinOperator::RangeInclusive => todo!("TODO: RangeInclusive"),
        BinOperator::Dot => unreachable!("Build bin op Dot"),

        BinOperator::Eq
        | BinOperator::Neq
        | BinOperator::Lt
        | BinOperator::Gt
        | BinOperator::Lte
        | BinOperator::Gte => {
            let rhs_val = build_expr(state, &bin_op.rhs, ExprTy::RValue)?;
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
            let rhs_val = build_expr(state, &bin_op.rhs, ExprTy::RValue)?;
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
            &state.analyze_ctx.ty_env.lock(),
            state.module.ptr_size,
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
        UnOperator::Deref => build_un_op_deref(state, un_op, expr_ty),
        UnOperator::Address => build_un_op_address(state, un_op),
        UnOperator::Positive => build_un_op_pos(state, &un_op.value),
        UnOperator::Negative => build_un_op_neg(state, &un_op.value, ret_type),
        UnOperator::Increment => build_un_op_inc(state, &un_op.value),
        UnOperator::Decrement => build_un_op_dec(state, &un_op.value),
        UnOperator::UnionIs(_, ref var_decl) => build_un_op_union_is(state, un_op, var_decl),
        UnOperator::ArrayAccess(_) => build_un_op_array_access(state, un_op, expr_ty),
        UnOperator::AdtAccess(..) => build_un_op_adt_access(state, un_op, expr_ty),
        UnOperator::EnumAccess(_, _) => match expr_ty {
            ExprTy::LValue => Err(LangError::new(
                "Enum access is not allowed as LValue.".into(),
                LangErrorKind::IrError,
                un_op.file_pos,
            )),
            ExprTy::RValue => build_un_op_enum_access(state, un_op),
        },
        UnOperator::BitComplement => todo!("BitComplement"),
        UnOperator::BoolNot => build_un_op_bool_not(state, un_op),
    }
}

fn build_bin_op_bool_and(
    state: &mut BuildState,
    ret_type: &Type,
    lhs_val: Val,
    rhs_expr: &Expr,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    if !matches!(ret_type, Type::Bool) {
        return Err(LangError::new(
            format!("Invalid type for BinOp::BoolAnd: {:?}", ret_type),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ));
    }

    let start_block_name = state.cur_block_mut()?.label.clone();

    let false_instr = state.builder.bool_false();
    state.cur_block_mut()?.push(false_instr.clone());

    // The eval block is to evaluate the rhs only if the lhs
    // evaluates to true to allow for short circuit.
    let eval_block_name = state.insert_new_block_after("bool.and.rhs".into(), &start_block_name)?;
    let merge_block_name =
        state.insert_new_block_after("bool.and.merge".into(), &eval_block_name)?;

    let end_instr = state
        .builder
        .branch_if(lhs_val, &eval_block_name, &merge_block_name);
    state.cur_block_mut()?.set_end_instr(end_instr);

    state.set_cur_block(Some(eval_block_name.clone()));
    let rhs_val = build_expr(state, rhs_expr, ExprTy::RValue)?;

    let end_instr = state.builder.branch(&merge_block_name);
    state.cur_block_mut()?.set_end_instr(end_instr);

    state.set_cur_block(Some(merge_block_name.clone()));

    let phi_instr = state
        .builder
        .phi(&[
            (start_block_name, false_instr.val),
            (eval_block_name.clone(), rhs_val),
        ])
        .map_err(into_err)?;
    state
        .cur_block_mut()?
        .insert(0, phi_instr.clone())
        .map_err(into_err)?;

    state.set_cur_block(Some(merge_block_name));
    Ok(phi_instr.val)
}

fn build_bin_op_bool_or(
    state: &mut BuildState,
    ret_type: &Type,
    lhs_val: Val,
    rhs_expr: &Expr,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    if !matches!(ret_type, Type::Bool) {
        return Err(LangError::new(
            format!("Invalid type for BinOp::BoolOr: {:?}", ret_type),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ));
    }

    let start_block_name = state.cur_block_mut()?.label.clone();

    let true_instr = state.builder.bool_true();
    state.cur_block_mut()?.push(true_instr.clone());

    // The eval block is to evaluate the rhs only if the lhs
    // evaluates to false to allow for short circuit.
    let eval_block_name = state.insert_new_block_after("bool.or.rhs".into(), &start_block_name)?;
    let merge_block_name =
        state.insert_new_block_after("bool.or.merge".into(), &eval_block_name)?;

    let end_instr = state
        .builder
        .branch_if(lhs_val, &merge_block_name, &eval_block_name);
    state.cur_block_mut()?.set_end_instr(end_instr);

    state.set_cur_block(Some(eval_block_name.clone()));
    let rhs_val = build_expr(state, rhs_expr, ExprTy::RValue)?;

    let end_instr = state.builder.branch(&merge_block_name);
    state.cur_block_mut()?.set_end_instr(end_instr);

    state.set_cur_block(Some(merge_block_name.clone()));

    let phi_instr = state
        .builder
        .phi(&[
            (start_block_name, true_instr.val),
            (eval_block_name.clone(), rhs_val),
        ])
        .map_err(into_err)?;
    state
        .cur_block_mut()?
        .insert(0, phi_instr.clone())
        .map_err(into_err)?;

    state.set_cur_block(Some(merge_block_name));
    Ok(phi_instr.val)
}

fn build_bin_op_cast(state: &mut BuildState, lhs_val: Val, ret_type: Type) -> LangResult<Val> {
    let instr = state.builder.cast(lhs_val, ret_type);
    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

fn build_bin_op_cmp(
    state: &mut BuildState,
    oper: &BinOperator,
    mut lhs_val: Val,
    mut rhs_val: Val,
    signed: Signed,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    // Allow compares between two enum values. In that case, "deref" the enum
    // to compare their underlying int types.
    if let (Type::Adt(adt_name_a), Type::Adt(adt_name_b)) = (&lhs_val.1, &rhs_val.1) {
        if adt_name_a != adt_name_b {
            return Err(LangError::new(
                format!(
                    "Compare operation on two different ADTs. First: {}, second: {}",
                    adt_name_a, adt_name_b
                ),
                LangErrorKind::IrError,
                file_pos.cloned(),
            ));
        }

        let members = state
            .module
            .get_struct(adt_name_a)
            .map_err(into_err)?
            .unwrap();
        if members.len() != 1 || !members.first().unwrap().is_int() {
            return Err(LangError::new(
                format!(
                    "Compare operation on two non-enum ADTs. First: {}, second: {}",
                    adt_name_a, adt_name_b
                ),
                LangErrorKind::IrError,
                file_pos.cloned(),
            ));
        }

        let lhs_access = state
            .builder
            .adt_access(&mut state.module, lhs_val, 0)
            .map_err(into_err)?;
        state.cur_block_mut()?.push(lhs_access.clone());

        let lhs_access_load = state.builder.load(lhs_access.val).map_err(into_err)?;
        state.cur_block_mut()?.push(lhs_access_load.clone());

        let rhs_access = state
            .builder
            .adt_access(&mut state.module, rhs_val, 0)
            .map_err(into_err)?;
        state.cur_block_mut()?.push(rhs_access.clone());

        let rhs_access_load = state.builder.load(rhs_access.val).map_err(into_err)?;
        state.cur_block_mut()?.push(rhs_access_load.clone());

        lhs_val = lhs_access_load.val;
        rhs_val = rhs_access_load.val;
    }

    let instr = match oper {
        BinOperator::Eq => state.builder.eq(lhs_val, rhs_val, signed),
        BinOperator::Neq => state.builder.neq(lhs_val, rhs_val, signed),
        BinOperator::Lt => state.builder.lt(lhs_val, rhs_val, signed),
        BinOperator::Gt => state.builder.gt(lhs_val, rhs_val, signed),
        BinOperator::Lte => state.builder.lte(lhs_val, rhs_val, signed),
        BinOperator::Gte => state.builder.gte(lhs_val, rhs_val, signed),
        _ => {
            return Err(LangError::new(
                format!("Invalid operator in int compare: {:?}", oper),
                LangErrorKind::IrError,
                file_pos.cloned(),
            ))
        }
    }
    .map_err(into_err)?;

    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

fn build_bin_op_expr(
    state: &mut BuildState,
    oper: &BinOperator,
    lhs_val: Val,
    rhs_val: Val,
    signed: Signed,
    file_pos: Option<&FilePosition>,
) -> LangResult<Val> {
    let instr = match oper {
        BinOperator::Add => state.builder.add(lhs_val, rhs_val),
        BinOperator::Sub => state.builder.sub(lhs_val, rhs_val),
        BinOperator::Mul => state.builder.mul(lhs_val, rhs_val),
        BinOperator::Div => state.builder.div(lhs_val, rhs_val, signed),
        BinOperator::Mod => state.builder.modu(lhs_val, rhs_val, signed),
        BinOperator::BitAnd => state.builder.bit_and(lhs_val, rhs_val),
        BinOperator::BitOr => state.builder.bit_or(lhs_val, rhs_val),
        BinOperator::BitXor => state.builder.bit_xor(lhs_val, rhs_val),
        BinOperator::ShiftLeft => state.builder.shift_left(lhs_val, rhs_val),
        BinOperator::ShiftRight => state.builder.shift_right(lhs_val, rhs_val, signed),
        _ => {
            return Err(LangError::new(
                format!("Bad oper in build_bin_op_expr: {:#?}", oper),
                LangErrorKind::IrError,
                file_pos.cloned(),
            ));
        }
    }
    .map_err(into_err)?;

    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

// TODO: Need to make sure that we are actually derefencing a pointer (ensure
// that `ptr_val` is an actual pointer).
fn build_un_op_deref(state: &mut BuildState, un_op: &UnOp, expr_ty: ExprTy) -> LangResult<Val> {
    let ptr_val = build_expr(state, &un_op.value, expr_ty)?;
    let instr = state.builder.load(ptr_val).map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());
    Ok(instr.val)
}

fn build_un_op_address(state: &mut BuildState, un_op: &UnOp) -> LangResult<Val> {
    let mut val = build_expr(state, &un_op.value, ExprTy::LValue)?;

    // If the evaluated expression doesn't return a pointer, we need to temporarily
    // allocate space for it on the stack so that we can get a pointer to it.
    if !matches!(val.1, Type::Pointer(..)) {
        let var_type = val.1.clone();
        let var_idx = state.cur_func_mut()?.add_local_var(var_type.clone());

        let var_ptr_instr = state.builder.var_address(VarIdx::Local(var_idx), var_type);
        state.cur_block_mut()?.push(var_ptr_instr.clone());

        let store_instr = state.builder.store(var_ptr_instr.val.clone(), val);
        state.cur_block_mut()?.push(store_instr);

        val = var_ptr_instr.val;
    }

    Ok(val)
}

// The `Positive` is "ignored", the expression is just compiled as usual.
fn build_un_op_pos(state: &mut BuildState, expr: &Expr) -> LangResult<Val> {
    build_expr(state, expr, ExprTy::RValue)
}

// TODO: Is it ok to just evalulate the expression as usual and then just do
//       a `0 - expr` subtraction to get the negative number?
fn build_un_op_neg(state: &mut BuildState, expr: &Expr, ir_type: Type) -> LangResult<Val> {
    let zero_instr = state.builder.int("0", ir_type);
    state.cur_block_mut()?.push(zero_instr.clone());

    let expr_val = build_expr(state, expr, ExprTy::RValue)?;
    let sub_instr = state
        .builder
        .sub(zero_instr.val, expr_val)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(sub_instr.clone());

    Ok(sub_instr.val)
}

fn build_un_op_inc(state: &mut BuildState, expr: &Expr) -> LangResult<Val> {
    build_un_op_inc_or_dec(state, expr, true)
}

fn build_un_op_dec(state: &mut BuildState, expr: &Expr) -> LangResult<Val> {
    build_un_op_inc_or_dec(state, expr, false)
}

fn build_un_op_inc_or_dec(state: &mut BuildState, expr: &Expr, is_inc: bool) -> LangResult<Val> {
    let ptr_val = build_expr(state, expr, ExprTy::LValue)?;

    let load_instr = state.builder.load(ptr_val.clone()).map_err(into_err)?;
    state.cur_block_mut()?.push(load_instr.clone());

    let ir_type = if let Type::Pointer(ir_type) = &ptr_val.1 {
        ir_type
    } else {
        unreachable!("build_un_op_inc_or_dec: load only works on pointers.");
    };

    let one_instr = state.builder.int("1", *ir_type.clone());
    state.cur_block_mut()?.push(one_instr.clone());

    let math_instr = if is_inc {
        state.builder.add(load_instr.val, one_instr.val)
    } else {
        state.builder.sub(load_instr.val, one_instr.val)
    }
    .map_err(into_err)?;
    state.cur_block_mut()?.push(math_instr.clone());

    let store_instr = state.builder.store(ptr_val, math_instr.val.clone());
    state.cur_block_mut()?.push(store_instr);

    Ok(math_instr.val)
}

fn build_un_op_union_is(state: &mut BuildState, un_op: &UnOp, var_decl: &Stmt) -> LangResult<Val> {
    // TODO: The value will be stored into the new var decl here.
    //       This is compiled in the if-case expr which means that the
    //       variable is declared and then stored even though the expr
    //       might not evaluate to true. Change this so that the var
    //       is declared and initialized ONLY if the expr evals to true.
    if let Stmt::VariableDecl(var, ..) = var_decl {
        let var = var.read();
        build_var_decl(state, &var)?;

        let union_access = build_expr(state, &un_op.value, ExprTy::RValue)?;
        build_var_store(state, &var, union_access)?;
    } else {
        unreachable!("{:#?}", var_decl);
    }

    let inner_un_op = if let Expr::Op(Op::UnOp(inner_un_op)) = un_op.value.as_ref() {
        inner_un_op
    } else {
        return Err(LangError::new(
            format!("Rhs value of UnionIs wasn't un op. Value: {:#?}", &un_op),
            LangErrorKind::IrError,
            un_op.file_pos.to_owned(),
        ));
    };

    // The tag that this UnionIs is expected to match against.
    let expected_tag_instr =
        if let UnOperator::AdtAccess(Either::Right(tag)) = &inner_un_op.operator {
            state.builder.u8(&tag.to_string())
        } else {
            return Err(LangError::new(
                format!(
                    "Rhs value of UnionIs wasn't valid AdtAccess. un_op value: {:#?}",
                    &inner_un_op
                ),
                LangErrorKind::IrError,
                inner_un_op.file_pos.to_owned(),
            ));
        };
    state.cur_block_mut()?.push(expected_tag_instr.clone());

    // The actual tag of the current instance of the enum in the code.
    let actual_tag = build_union_tag_access(state, &inner_un_op.value)?;

    let cmp_instr = state
        .builder
        .eq(expected_tag_instr.val, actual_tag, Signed::False)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(cmp_instr.clone());

    Ok(cmp_instr.val)
}

fn build_un_op_array_access(
    state: &mut BuildState,
    un_op: &UnOp,
    expr_ty: ExprTy,
) -> LangResult<Val> {
    let array_val = build_expr(state, &un_op.value, expr_ty)?;

    let idx_val = if let UnOperator::ArrayAccess(idx_expr) = &un_op.operator {
        build_expr(state, idx_expr, ExprTy::RValue)?
    } else {
        unreachable!("{:#?}", un_op);
    };

    let mut instr = state
        .builder
        .array_access(array_val, idx_val)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    if let ExprTy::RValue = expr_ty {
        instr = state.builder.load(instr.val).map_err(into_err)?;
        state.cur_block_mut()?.push(instr.clone());
    }

    Ok(instr.val)
}

fn build_un_op_adt_access(
    state: &mut BuildState,
    un_op: &UnOp,
    expr_ty: ExprTy,
) -> LangResult<Val> {
    let idx = if let UnOperator::AdtAccess(Either::Right(idx)) = &un_op.operator {
        *idx
    } else {
        unreachable!("{:#?}", un_op);
    };
    build_adt_access(state, &un_op.value, idx, expr_ty)
}

fn build_adt_access(
    state: &mut BuildState,
    adt_expr: &Expr,
    idx: usize,
    expr_ty: ExprTy,
) -> LangResult<Val> {
    let adt_val = build_expr(state, adt_expr, expr_ty)?;

    let adt_type_id = adt_expr.get_expr_type()?;
    let path = get_ident(&state.analyze_ctx.ty_env.lock(), adt_type_id)?.unwrap();
    let adt = state
        .analyze_ctx
        .ast_ctx
        .get_adt(&state.analyze_ctx.ty_env.lock(), &path)?;

    // If the current ADT is a union, need to look-up the type of the member that
    // we are accessing (member with idx `idx`). Also need to change the `idx`
    // to 0 which is the location of the member in the IR type.
    let (idx, member_type) = if let AdtKind::Union = adt.read().kind {
        let adt = adt.read();
        let member = adt.members.get(idx).unwrap();
        let ir_type = to_ir_type(
            &state.analyze_ctx.ast_ctx,
            &state.analyze_ctx.ty_env.lock(),
            state.module.ptr_size,
            member.read().ty.unwrap(),
        )?;

        (0, Some(ir_type))
    } else {
        (idx, None)
    };

    let mut instr = state
        .builder
        .adt_access(&mut state.module, adt_val, idx)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    // Need to cast to the correct member type if this was a union access.
    // The instructions above will return a type of {[u8; x]}.
    // `member_type` will only be set to Some if this is a union.
    if let Some(member_type) = member_type {
        let ptr_member_type = Type::Pointer(Box::new(member_type));
        instr = state.builder.cast(instr.val, ptr_member_type);
        state.cur_block_mut()?.push(instr.clone());
    }

    if let ExprTy::RValue = expr_ty {
        instr = state.builder.load(instr.val).map_err(into_err)?;
        state.cur_block_mut()?.push(instr.clone());
    }

    Ok(instr.val)
}

/// Builds an access to the tag of a union. The tag will be the second member
/// of the union struct.
fn build_union_tag_access(state: &mut BuildState, union_expr: &Expr) -> LangResult<Val> {
    let adt_val = build_expr(state, union_expr, ExprTy::RValue)?;
    let tag_idx = 1;

    let mut instr = state
        .builder
        .adt_access(&mut state.module, adt_val, tag_idx)
        .map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    instr = state.builder.load(instr.val).map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    Ok(instr.val)
}

fn build_un_op_enum_access(state: &mut BuildState, un_op: &UnOp) -> LangResult<Val> {
    let member_name = if let UnOperator::EnumAccess(member_name, ..) = &un_op.operator {
        member_name
    } else {
        unreachable!("{:#?}", un_op);
    };

    let adt_path = if let Expr::Type(type_id, ..) = un_op.value.as_ref() {
        get_ident(&state.analyze_ctx.ty_env.lock(), *type_id)?.unwrap()
    } else {
        return Err(LangError::new(
            format!("Unop value in enum access not a enum type: {:#?}", un_op,),
            LangErrorKind::IrError,
            un_op.value.file_pos().cloned(),
        ));
    };
    let adt_full_name = to_string_path(&state.analyze_ctx.ty_env.lock(), &adt_path);

    let member_idx = state.analyze_ctx.ast_ctx.get_adt_member_index(
        &state.analyze_ctx.ty_env.lock(),
        &adt_path,
        member_name,
    )?;

    // TODO: int/uint.
    let idx_instr = state.builder.u64(&member_idx.to_string());
    state.cur_block_mut()?.push(idx_instr.clone());

    let instr = state
        .builder
        .struct_init(&mut state.module, &adt_full_name, &[idx_instr.val])
        .map_err(into_err)?;
    state.cur_block_mut()?.push(instr.clone());

    Ok(instr.val)
}

fn build_un_op_bool_not(state: &mut BuildState, un_op: &UnOp) -> LangResult<Val> {
    let val = build_expr(state, &un_op.value, ExprTy::RValue)?;
    let not_instr = state.builder.bool_not(val);
    state.cur_block_mut()?.push(not_instr.clone());
    Ok(not_instr.val)
}
