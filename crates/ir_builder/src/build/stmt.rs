use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    token::{expr::Expr, op::AssignOperator, stmt::Stmt},
};
use ir::{ExprTy, instruction::EndInstr};

use crate::{into_err, state::BuildState};

pub(crate) fn build_stmt(state: &mut BuildState, stmt: &Stmt) -> LangResult<()> {
    match stmt {
        Stmt::Return(expr_opt, file_pos) => {
            build_return(state, expr_opt.as_ref(), file_pos.as_ref())
        }
        Stmt::Yield(expr, file_pos) => build_yield(state, expr, file_pos.as_ref()),
        Stmt::Break(file_pos) => build_break(state, file_pos.as_ref()),
        Stmt::Continue(file_pos) => build_continue(state, file_pos.as_ref()),

        Stmt::Assignment(oper, lhs, rhs, file_pos) => {
            build_assign(state, *oper, lhs, rhs, file_pos.as_ref())
        }
        Stmt::VariableDecl(var, file_pos) => {
            let var = var.as_ref().write().unwrap();
            build_var_decl(state, &var, file_pos.as_ref())
        }

        // Only the "DeferExecution" are compiled into code, the "Defer" is
        // only used during analyzing.
        Stmt::DeferExec(expr) => {
            build_expr(expr)?;
            Ok(())
        }

        // Do nothing here, these are handled in a earlier stage and doesn't
        // matter at this point.
        Stmt::Defer(..) | Stmt::ExternalDecl(..) | Stmt::Use(..) | Stmt::Module(..) => Ok(()),
    }
}

fn build_return(
    state: &mut BuildState,
    expr_opt: Option<&Expr>,
    file_pos: Option<&FilePosition>,
) -> LangResult<()> {
    let expr_val_opt = if let Some(expr) = expr_opt {
        Some(build_expr(expr)?)
    } else {
        None
    };

    let end_instr = EndInstr::Return(expr_val_opt);
    let cur_block = state.cur_basic_block_mut()?;
    cur_block.set_end_instruction(Some(end_instr));

    Ok(())
}

fn build_yield(
    state: &mut BuildState,
    expr: &Expr,
    file_pos: Option<&FilePosition>,
) -> LangResult<()> {
    panic!("TODO: Implement \"yield\" statement.");
}

fn build_break(state: &mut BuildState, file_pos: Option<&FilePosition>) -> LangResult<()> {
    let merge_block = state.get_branchable_merge_block(state.cur_block_id)?;
    let end_instr = Some(EndInstr::Branch(merge_block.name.clone()));
    merge_block.set_end_instruction(end_instr);
    Ok(())
}

fn build_continue(state: &mut BuildState, file_pos: Option<&FilePosition>) -> LangResult<()> {
    if let Some(branch_block_name) = &state.cur_branch_block_name {
        let cur_block = state.cur_basic_block_mut()?;
        let end_instr = Some(EndInstr::Branch(branch_block_name.clone()));
        cur_block.set_end_instruction(end_instr);
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
    file_pos: Option<&FilePosition>,
) -> LangResult<()> {
    let lhs_ptr = build_expr(lhs, ExprTy::LValue);

    let rhs_val = build_expr(rhs, ExprTy::RValue);
    let value = match oper {
        AssignOperator::Assignment => rhs_val,
        // TODO: Check so that the types are valid for these operations.
        AssignOperator::AssignAdd => {
            
        }
        AssignOperator::AssignSub => todo!(),
        AssignOperator::AssignMul => todo!(),
        AssignOperator::AssignDiv => todo!(),
        AssignOperator::AssignMod => todo!(),
        AssignOperator::AssignBitAnd => todo!(),
        AssignOperator::AssignBitOr => todo!(),
        AssignOperator::AssignBitXor => todo!(),
        AssignOperator::AssignShl => todo!(),
        AssignOperator::AssignShr => todo!(),
    };
}

fn build_var_decl(
    state: &mut BuildState,
    var: &Var,
    file_pos: Option<&FilePosition>,
) -> LangResult<()> {
}
