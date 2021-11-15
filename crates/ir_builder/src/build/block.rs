use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader, Fn},
        expr::Expr,
    },
    BlockId,
};
use ir::{EndInstr, ExprTy, Type, Val};

use crate::{
    build_token, fn_full_name, into_err,
    state::{BranchInfo, BuildState},
};

use super::expr::build_expr;

pub(crate) fn build_block(state: &mut BuildState, block: &Block) -> LangResult<()> {
    state.cur_block_id = block.id;

    match &block.header {
        BlockHeader::Default => {
            for token in &block.body {
                build_token(state, token)?;
                state.cur_block_id = block.id;
            }
        }
        BlockHeader::Fn(func) => build_func_body(state, &func.read(), &block.body)?,
        BlockHeader::Implement(..) | BlockHeader::Struct(..) | BlockHeader::Union(..) => {
            for body_token in &block.body {
                if let AstToken::Block(Block {
                    header: BlockHeader::Fn(method),
                    body: method_body,
                    id: fn_id,
                    ..
                }) = body_token
                {
                    state.cur_block_id = *fn_id;
                    build_func_body(state, &method.read(), method_body)?;
                }
            }
        }
        BlockHeader::Anonymous => build_anon(state, &block.body)?,

        BlockHeader::If => build_if(state, &block.body)?,
        BlockHeader::IfCase(..) => {
            return Err(LangError::new(
                "Unexpected IfCase in compile_block".into(),
                LangErrorKind::IrError,
                None,
            ))
        }

        BlockHeader::Match(expr) => build_match(state, expr, &block.body, block.id)?,
        BlockHeader::MatchCase(..) => {
            return Err(LangError::new(
                "Unexpected MatchCase in compile_block".into(),
                LangErrorKind::IrError,
                None,
            ))
        }

        BlockHeader::While(expr_opt) => build_while(state, expr_opt.as_ref(), &block.body)?,

        BlockHeader::Enum(_) | BlockHeader::Trait(_) => {
            // All ADTs/traits have already been collected/handled at this stage.
            // Only the methods inside structs and unions are built.
        }

        BlockHeader::For(_, _) => todo!("TODO -- for: {:#?}", block),
        BlockHeader::Test(_) => todo!("TODO -- test: {:#?}", block),
    }

    Ok(())
}

fn build_func_body(state: &mut BuildState, func: &Fn, body: &[AstToken]) -> LangResult<()> {
    let ty_env_guard = state.analyze_ctx.ty_env.lock();

    let fn_full_name = fn_full_name(&ty_env_guard, func)?;
    state.set_cur_func(Some(fn_full_name.clone()));

    let basic_block_label = state.insert_new_block(format!("{}-entry", fn_full_name))?;
    state.set_cur_block(Some(basic_block_label));

    drop(ty_env_guard);

    let func_block_id = state.cur_block_id;
    for token in body {
        state.cur_block_id = func_block_id;
        build_token(state, token)?;
    }

    // Add a "invisible" return at the end of the last block if this is a
    // function with no return type. If an end instruction is manually set it the
    // code, it must be a "return <void>".
    let ir_func = state.cur_func_mut()?;

    if matches!(ir_func.ret_type, Type::Void) {
        let cur_func_name = ir_func.name.clone();

        let last_block = if let Some(last_block) = ir_func.last_block_mut() {
            last_block
        } else {
            return Err(LangError::new(
                format!("No basic block in func: {:?}", cur_func_name),
                LangErrorKind::IrError,
                Some(func.file_pos.to_owned()),
            ));
        };
        let last_block_label = last_block.label.clone();

        match last_block.get_end_instr() {
            None => last_block.set_end_instr(EndInstr::Return(None)),
            Some(EndInstr::Return(None)) => (),
            Some(end_instr) => {
                return Err(LangError::new(
                    format!(
                        "Found bad end instruction in func \"{}\" block \"{}\". \
                        Expected \"return void\" or not set, but found: {:#?}",
                        cur_func_name, last_block_label, end_instr,
                    ),
                    LangErrorKind::IrError,
                    Some(func.file_pos.to_owned()),
                ));
            }
        }
    }

    Ok(())
}

fn build_anon(state: &mut BuildState, body: &[AstToken]) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();

    let anon_block_id = state.cur_block_id;
    for token in body {
        state.cur_block_id = anon_block_id;
        build_token(state, token)?;
    }

    // If all paths in this block doesn't branch away, it needs to branch
    // to a merge block. Otherwise, if all paths branches away, no merge
    // block should be created.
    let block_ctx = state.get_block_ctx(anon_block_id)?;
    if !block_ctx.all_children_contains_return || !block_ctx.contains_return {
        let merge_block_label = state.insert_new_block(format!("{}-anon.merge", cur_func_name))?;
        state
            .merge_blocks
            .insert(anon_block_id, merge_block_label.clone());

        let branch_instr = state.builder.branch(&merge_block_label);
        state.cur_block_mut()?.set_end_instr(branch_instr);

        state.set_cur_block(Some(merge_block_label));
    } else {
        state.set_cur_block(None);
    }

    Ok(())
}

/// All the "ParseToken" in the body should be "IfCase"s.
fn build_if(state: &mut BuildState, body: &[AstToken]) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();
    let cur_block_label = state.cur_block()?.label.clone();
    let if_block_id = state.cur_block_id;

    // Create and store the basic blocks of this if-statement.
    // For every if-case that has am expression (if/elif), an extra block will
    // be created which will contain the branching logic between the cases.
    let mut branch_info = BranchInfo::new();
    branch_info.if_branches.push(cur_block_label.clone());

    let mut prev_block_label = cur_block_label;
    for (i, if_case) in body.iter().enumerate() {
        if let AstToken::Block(Block {
            header: BlockHeader::IfCase(expr_opt),
            ..
        }) = &if_case
        {
            // Skip adding a branch block if this is the first case (since it
            // has the branch block `cur_block`). Also only add a branch block
            // if this `if_case` contains a expression that can be "branched on".
            if i > 0 && expr_opt.is_some() {
                let branch_block_label =
                    state.insert_new_block(format!("{}-if.branch", cur_func_name))?;
                branch_info.if_branches.push(branch_block_label.clone());
                prev_block_label = branch_block_label;
            }

            let case_block_name = state
                .insert_new_block_after(format!("{}-if.case", cur_func_name), &prev_block_label)?;
            prev_block_label = case_block_name.clone();
            branch_info.if_cases.push(case_block_name);
        } else {
            return Err(LangError::new(
                format!("Token in \"If\" block wasn't a \"IfCase\": {:?}", &if_case),
                LangErrorKind::IrError,
                if_case.file_pos().cloned(),
            ));
        }
    }

    // Add a "merge block" that the if-cases will merge to if they don't branch
    // away. The merge block will NOT be created if all if-cases contains a return
    // instruction. This is because there is no possiblity to end up in the merge
    // block in that case, so it would just be empty.
    let block_ctx = state.get_block_ctx(if_block_id)?;
    let merge_block_opt = if !block_ctx.all_children_contains_return {
        let merge_block_label = state
            .insert_new_block_after(format!("{}-if.merge", cur_func_name), &prev_block_label)?;
        state
            .merge_blocks
            .insert(if_block_id, merge_block_label.clone());
        Some(merge_block_label)
    } else {
        None
    };

    // Iterate through all "if cases" in this if-statement and compile them.
    for (case_idx, if_case) in body.iter().enumerate() {
        state.cur_block_id = if_block_id;

        if let AstToken::Block(Block {
            header: BlockHeader::IfCase(expr_opt),
            body: case_body,
            id: case_id,
            ..
        }) = if_case
        {
            build_if_case(state, expr_opt, case_body, *case_id, case_idx, &branch_info)?;
        } else {
            return Err(LangError::new(
                "Token in \"If\" block wasn't a \"IfCase\".".into(),
                LangErrorKind::IrError,
                if_case.file_pos().cloned(),
            ));
        }
    }

    // The if statement has been built complete. If a merge block was created,
    // set it as the current block. Otherwise just keep the old cur block.
    if merge_block_opt.is_some() {
        state.cur_basic_block_label = merge_block_opt;
    }

    Ok(())
}

fn build_if_case(
    state: &mut BuildState,
    case_expr_opt: &Option<Expr>,
    body: &[AstToken],
    block_id: BlockId,
    idx: usize,
    branch_info: &BranchInfo,
) -> LangResult<()> {
    state.cur_block_id = block_id;

    let case_block_label = branch_info.get_if_case(state, idx)?.label.clone();

    // If this is a if case with a expression, the branch condition should be
    // evaluated and branched from the branch block.
    if let Some(case_expr) = case_expr_opt {
        let branch_block_label = branch_info.get_if_branch(state, idx)?.label.clone();

        // If there are no more branch blocks, set the next branch block to
        // the merge block if there are no more if_cases or set it to the
        // last if_case if there is still one left.
        let next_branch_block_label = if idx + 1 >= branch_info.if_branches.len() {
            if idx + 1 >= branch_info.if_cases.len() {
                state.get_merge_block(block_id)?.label.clone()
            } else {
                branch_info.get_if_case(state, idx + 1)?.label.clone()
            }
        } else {
            branch_info.get_if_branch(state, idx + 1)?.label.clone()
        };

        state.set_cur_block(Some(branch_block_label));

        let expr_val = build_expr(state, case_expr, ExprTy::RValue)?;
        let end_instr =
            state
                .builder
                .branch_if(expr_val, &case_block_label, &next_branch_block_label);
        state.cur_block_mut()?.set_end_instr(end_instr);
    }

    // Compile all tokens inside this if-case.
    state.set_cur_block(Some(case_block_label.clone()));
    for token in body {
        state.cur_block_id = block_id;
        build_token(state, token)?;
    }

    // Add a branch to the merge block if the current basic block doesn't have a
    // terminator yet.
    if !state.cur_block_mut()?.has_end_instr() {
        let merge_block_label = state.get_merge_block(block_id)?.label.clone();
        let end_instr = state.builder.branch(&merge_block_label);
        state.cur_block_mut()?.set_end_instr(end_instr);
    }

    Ok(())
}

fn build_match(
    state: &mut BuildState,
    expr: &Expr,
    body: &[AstToken],
    block_id: BlockId,
) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();
    let start_block_label = state.cur_block()?.label.clone();

    let mut cases = Vec::default();
    let mut blocks_without_branch = Vec::default();

    // Iterate through all "match cases" in this match-statement and build them.
    // This will NOT build the default block. It is done in iteration after this
    // to ensure that the default block is built after all other block to keep
    // the "sequential flow".
    for match_case in body {
        state.cur_block_id = block_id;

        if let AstToken::Block(Block {
            header: BlockHeader::MatchCase(Some(case_expr)),
            body: case_body,
            id: case_id,
            ..
        }) = match_case
        {
            build_match_case(
                state,
                case_expr,
                case_body,
                *case_id,
                &mut cases,
                &mut blocks_without_branch,
            )?;
        } else if let AstToken::Block(Block {
            header: BlockHeader::MatchCase(None),
            ..
        }) = match_case
        {
            // Default block will be handled in logic below. Ignore for now.
        } else {
            return Err(LangError::new(
                "Token in \"Match\" block wasn't a \"MatchCase\".".into(),
                LangErrorKind::IrError,
                match_case.file_pos().cloned(),
            ));
        }
    }

    // The default block that control flow will be branched to if no cases matches.
    let mut default_block_label_opt = None;

    // Iterate through the match cases one more time to find the default block.
    // Also ensure that it only exists a single default block. If no default
    // block exists, create a "unreachable" instruction. This might not be
    // correct atm since there might be times when all values aren't covered
    // by the match cases.
    // TODO: Fix the "unreachable" default block. Is not always unreachable.
    for mut match_case in body {
        state.cur_block_id = block_id;

        if let AstToken::Block(Block {
            header: BlockHeader::MatchCase(None),
            body: default_case_body,
            id: case_id,
            ..
        }) = &mut match_case
        {
            build_match_default_case(
                state,
                &mut default_block_label_opt,
                default_case_body,
                *case_id,
                &mut blocks_without_branch,
            )?;
        }
    }

    // If None: No default block found, create a new default block that contains
    // a single unreachable instruction.
    let default_block_label = if let Some(default_block_label) = default_block_label_opt {
        default_block_label
    } else {
        let cur_block_label = state.cur_block()?.label.clone();
        let default_block_label = state
            .insert_new_block_after(format!("{}-match.default", cur_func_name), &cur_block_label)?;

        let end_instr = state.builder.unreachable();
        let default_block = state
            .cur_func_mut()?
            .get_block_mut(&default_block_label)
            .unwrap();
        default_block.set_end_instr(end_instr);

        default_block_label
    };

    state.set_cur_block(Some(default_block_label.clone()));

    // The merge block that all cases will branch to after the switch-statement
    // if they don't branch away themselves.
    // This will become the "current basic block" when this function returns.
    let merge_block_label = state.insert_new_block_after(
        format!("{}-match.merge", cur_func_name),
        &default_block_label,
    )?;

    for block_label in blocks_without_branch {
        let end_instr = state.builder.branch(&merge_block_label);
        let block_without_branch = state.cur_func_mut()?.get_block_mut(&block_label).unwrap();
        block_without_branch.set_end_instr(end_instr);
    }

    state.set_cur_block(Some(start_block_label));

    let match_val = build_expr(state, expr, ExprTy::RValue)?;
    let end_instr = state
        .builder
        .branch_switch(match_val, &default_block_label, &cases)
        .map_err(into_err)?;
    state.cur_block_mut()?.set_end_instr(end_instr);

    state.set_cur_block(Some(merge_block_label));

    Ok(())
}

fn build_match_case(
    state: &mut BuildState,
    case_expr: &Expr,
    body: &[AstToken],
    block_id: BlockId,
    cases: &mut Vec<(Val, String)>,
    blocks_without_branch: &mut Vec<String>,
) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();

    let cur_block_label = &state.cur_block()?.label.clone();
    let case_block_label =
        state.insert_new_block_after(format!("{}-match.case", cur_func_name), cur_block_label)?;

    let expr_val = build_expr(state, case_expr, ExprTy::RValue)?;

    // Compile all tokens inside this match-case.
    state.set_cur_block(Some(case_block_label.clone()));
    for token in body {
        state.cur_block_id = block_id;
        build_token(state, token)?;
    }

    cases.push((expr_val, case_block_label));

    // If the body of the match case doesn't have a ending branch instruction, an
    // "ending" branch needs to be added. Store the current block in
    // `blocks_without_branch`. After this for-loop is done, all the blocks in
    // that vector will be given a branch to the merge block. The merge block
    // will be created after this loop.
    let cur_block = state.cur_block()?;
    if !cur_block.has_end_instr() {
        blocks_without_branch.push(cur_block.label.clone());
    }

    Ok(())
}

fn build_match_default_case(
    state: &mut BuildState,
    default_block_label: &mut Option<String>,
    default_case_body: &[AstToken],
    block_id: BlockId,
    blocks_without_branch: &mut Vec<String>,
) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();

    if default_block_label.is_some() {
        return Err(LangError::new(
            "More than one default block found in match.".into(),
            LangErrorKind::IrError,
            None,
        ));
    }

    let cur_block_label = state.cur_block()?.label.clone();
    let case_block_label = state
        .insert_new_block_after(format!("{}-match.default", cur_func_name), &cur_block_label)?;
    *default_block_label = Some(case_block_label);

    // Compile all tokens inside this default match-case.
    state.set_cur_block(default_block_label.clone());
    for token in default_case_body {
        state.cur_block_id = block_id;
        build_token(state, token)?;
    }

    let cur_block = state.cur_block()?;
    if !cur_block.has_end_instr() {
        blocks_without_branch.push(cur_block.label.clone());
    }

    Ok(())
}

fn build_while(
    state: &mut BuildState,
    expr_opt: Option<&Expr>,
    body: &[AstToken],
) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();
    let cur_block_label = state.cur_block()?.label.clone();
    let while_block_id = state.cur_block_id;

    let branch_block_label = state
        .insert_new_block_after(format!("{}-while.branch", cur_func_name), &cur_block_label)?;
    let body_block_label = state
        .insert_new_block_after(format!("{}-while.body", cur_func_name), &branch_block_label)?;
    let merge_block_label = state
        .insert_new_block_after(format!("{}-while.merge", cur_func_name), &body_block_label)?;

    state
        .merge_blocks
        .insert(while_block_id, merge_block_label.clone());

    let end_instr = state.builder.branch(&branch_block_label);
    state.cur_block_mut()?.set_end_instr(end_instr);

    // If expression is NOT set, treat this as a infinite while loop.
    state.set_cur_block(Some(branch_block_label.clone()));
    let end_instr = if let Some(expr) = expr_opt {
        let expr_val = build_expr(state, expr, ExprTy::RValue)?;
        state
            .builder
            .branch_if(expr_val, &body_block_label, &merge_block_label)
    } else {
        state.builder.branch(&body_block_label)
    };
    state.cur_block_mut()?.set_end_instr(end_instr);

    // Iterate through all "tokens" in this while-loop and compile them.
    state.set_cur_block(Some(body_block_label.clone()));
    for token in body {
        state.cur_block_id = while_block_id;
        state.cur_branch_block_label = Some(branch_block_label.clone());
        build_token(state, token)?;
    }

    // If the block does NOT contain a terminator instruction inside it (return,
    // yield etc.), add a unconditional branch back up to the "while.branch" block.
    if !state.cur_block()?.has_end_instr() {
        let end_instr = state.builder.branch(&branch_block_label);
        state.cur_block_mut()?.set_end_instr(end_instr);
    }

    state.set_cur_block(Some(merge_block_label));
    state.cur_branch_block_label = None;

    Ok(())
}
