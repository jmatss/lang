use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    token::{
        expr::{Expr, Var},
        op::AssignOperator,
        stmt::Stmt,
    },
    ty::{ty::Ty, ty_env::TyEnv, type_id::TypeId},
};
use ir::{ExprTy, Signed, Val, VarIdx};

use crate::{into_err, state::BuildState};

use super::expr::build_expr;

pub(crate) fn build_stmt(state: &mut BuildState, stmt: &Stmt) -> LangResult<()> {
    match stmt {
        Stmt::Return(expr_opt, _) => build_return(state, expr_opt.as_ref()),
        Stmt::Yield(expr, _) => build_yield(state, expr),
        Stmt::Break(_) => build_break(state),
        Stmt::Continue(file_pos) => build_continue(state, file_pos.as_ref()),
        Stmt::Assignment(oper, lhs, rhs, _) => build_assign(state, *oper, lhs, rhs),
        Stmt::VariableDecl(var, _) => build_var_decl(state, &var.read()),

        // Only the "DeferExecution" are compiled into code, the "Defer" is
        // only used during analyzing.
        Stmt::DeferExec(expr) => {
            build_expr(state, expr, ExprTy::RValue)?;
            Ok(())
        }

        // Do nothing here, these are handled in a earlier stage and doesn't
        // matter at this point.
        Stmt::Defer(..) | Stmt::ExternalDecl(..) | Stmt::Use(..) | Stmt::Module(..) => Ok(()),
    }
}

fn build_return(state: &mut BuildState, expr_opt: Option<&Expr>) -> LangResult<()> {
    let expr_val_opt = if let Some(expr) = expr_opt {
        Some(build_expr(state, expr, ExprTy::RValue)?)
    } else {
        None
    };

    let end_instr = state.builder.ret(expr_val_opt);
    state.cur_block_mut()?.set_end_instr(end_instr);
    Ok(())
}

fn build_yield(state: &mut BuildState, expr: &Expr) -> LangResult<()> {
    panic!("TODO: Implement \"yield\" statement.");
}

fn build_break(state: &mut BuildState) -> LangResult<()> {
    let merge_block_label = state
        .get_branchable_merge_block(state.cur_block_id)?
        .label
        .clone();
    let end_instr = state.builder.branch(&merge_block_label);
    state.cur_block_mut()?.set_end_instr(end_instr);
    Ok(())
}

fn build_continue(state: &mut BuildState, file_pos: Option<&FilePosition>) -> LangResult<()> {
    if let Some(branch_block_label) = &state.cur_branch_block_label {
        let end_instr = state.builder.branch(branch_block_label);
        state.cur_block_mut()?.set_end_instr(end_instr);
        Ok(())
    } else {
        Err(LangError::new(
            "Branch block None when compiling \"continue\".".into(),
            LangErrorKind::IrError,
            file_pos.cloned(),
        ))
    }
}

fn build_assign(
    state: &mut BuildState,
    oper: AssignOperator,
    lhs: &Expr,
    rhs: &Expr,
) -> LangResult<()> {
    let lhs_ptr = build_expr(state, lhs, ExprTy::LValue)?;
    let lhs_val_instr = state.builder.load(lhs_ptr.clone()).map_err(into_err)?;
    state.cur_block_mut()?.push(lhs_val_instr.clone());

    let lhs_val = lhs_val_instr.val;
    let rhs_val = build_expr(state, rhs, ExprTy::RValue)?;

    let type_id = lhs.get_expr_type()?;
    let signed = is_signed(&state.analyze_ctx.ty_env.lock(), type_id)?;

    let val = if let AssignOperator::Assignment = oper {
        rhs_val
    } else {
        let instr = match oper {
            // TODO: Check so that the types are valid for these operations.
            AssignOperator::AssignAdd => state.builder.add(lhs_val, rhs_val),
            AssignOperator::AssignSub => state.builder.sub(lhs_val, rhs_val),
            AssignOperator::AssignMul => state.builder.mul(lhs_val, rhs_val),
            AssignOperator::AssignDiv => state.builder.div(lhs_val, rhs_val, signed),
            AssignOperator::AssignMod => state.builder.modu(lhs_val, rhs_val, signed),
            AssignOperator::AssignBitAnd => state.builder.bit_and(lhs_val, rhs_val),
            AssignOperator::AssignBitOr => state.builder.bit_or(lhs_val, rhs_val),
            AssignOperator::AssignBitXor => state.builder.bit_xor(lhs_val, rhs_val),
            AssignOperator::AssignShl => state.builder.shift_left(lhs_val, rhs_val),
            AssignOperator::AssignShr => state.builder.shift_right(lhs_val, rhs_val, signed),
            AssignOperator::Assignment => unreachable!(),
        }
        .map_err(into_err)?;

        state.cur_block_mut()?.push(instr.clone());
        instr.val
    };

    let instr = state.builder.store(lhs_ptr, val);
    state.cur_block_mut()?.push(instr);

    Ok(())
}

// TODO: Handle global init values here?
pub fn build_var_decl(state: &mut BuildState, var: &Var) -> LangResult<()> {
    // The variables have already been collected. Just need to build and insert
    // the init value if any is set.
    if !var.is_global {
        if let Some(init_value) = &var.value {
            let val = build_expr(state, init_value, ExprTy::RValue)?;
            build_var_store(state, var, val)?;
        }
    }
    Ok(())
}

pub fn build_var_store(state: &mut BuildState, var: &Var, val: Val) -> LangResult<()> {
    let var_idx = state.get_var(&var.full_name(), state.cur_block_id)?;
    let var_type = match var_idx {
        VarIdx::Local(idx) => state.cur_func()?.locals.get(idx.0).cloned(),
        VarIdx::Param(idx) => state.cur_func()?.params.get(idx.0).cloned(),
        VarIdx::Global(_) => unreachable!("Global in `build_var_decl`"),
    }
    .unwrap();

    let var_ptr_instr = state.builder.var_address(var_idx, var_type);
    state.cur_block_mut()?.push(var_ptr_instr.clone());

    let expr_instr = state.builder.store(var_ptr_instr.val, val);
    state.cur_block_mut()?.push(expr_instr);

    Ok(())
}

/// Checks if the given type is signed. This returns true if this is a signed
/// integer, returns false for every other type (includingn non-int types).
fn is_signed(ty_env: &TyEnv, id: TypeId) -> LangResult<Signed> {
    let ty = ty_env.ty(id)?;
    Ok(
        if matches!(ty, Ty::CompoundType(inner_ty, ..) if inner_ty.is_signed()) {
            Signed::True
        } else {
            Signed::False
        },
    )
}
