use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader, Fn},
        expr::Expr,
    },
    ty::{to_string::to_string_path, ty::Ty},
    util, BlockId,
};
use ir::{decl::ty::Type, instruction::EndInstr, Val};

use crate::{
    build_token, into_err,
    state::{BranchInfo, BuildState},
};

pub(crate) fn build_block(state: &mut BuildState, block: &Block) -> LangResult<()> {
    state.cur_block_id = block.id;

    match &block.header {
        BlockHeader::Default => {
            for token in &block.body {
                build_token(state, token)?;
                state.cur_block_id = block.id;
            }
        }
        BlockHeader::Fn(func) => {
            build_func_body(state, &func.as_ref().read().unwrap(), &block.body)?
        }
        BlockHeader::Implement(..) => {
            for body_token in &block.body {
                if let AstToken::Block(Block {
                    header: BlockHeader::Fn(method),
                    body: method_body,
                    ..
                }) = body_token
                {
                    build_func_body(state, &method.as_ref().read().unwrap(), &method_body)?;
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

        BlockHeader::Struct(_)
        | BlockHeader::Enum(_)
        | BlockHeader::Union(_)
        | BlockHeader::Trait(_) => {
            // All ADTs/traits have already been collected/handled at this stage.
        }

        BlockHeader::For(_, _) => todo!("TODO -- for: {:#?}", block),
        BlockHeader::Test(_) => todo!("TODO -- test: {:#?}", block),
    }

    Ok(())
}

fn build_func_body(state: &mut BuildState, func: &Fn, body: &[AstToken]) -> LangResult<()> {
    let ty_env_guard = state.analyze_ctx.ty_env.lock().unwrap();

    let fn_name = if let Some(adt_type_id) = &func.method_adt {
        let adt_ty = ty_env_guard.ty_clone(*adt_type_id)?;
        if let Ty::CompoundType(inner_ty, adt_gens, ..) = adt_ty {
            let adt_path = inner_ty.get_ident().unwrap();
            let adt_path_without_module = adt_path.last().cloned().unwrap().into();
            util::to_method_name(
                &ty_env_guard,
                &adt_path_without_module,
                Some(&adt_gens),
                &func.name,
                func.generics.as_ref(),
            )
        } else {
            unreachable!("method call on non compund type: {:#?}", func);
        }
    } else {
        func.name.clone()
    };

    let module_path = state
        .analyze_ctx
        .ast_ctx
        .get_module(state.cur_block_id)?
        .unwrap_or_default();
    let full_path = module_path.clone_push(&fn_name, func.generics.as_ref(), Some(func.file_pos));
    let fn_full_name = to_string_path(&ty_env_guard, &full_path);

    let ir_func = if let Some(ir_func) = state.module.get_function_mut(&fn_full_name) {
        ir_func
    } else {
        return Err(LangError::new(
            format!("Unable to find function with name: {}", fn_full_name),
            LangErrorKind::IrError,
            None,
        ));
    };

    let basic_block_name = ir_func
        .insert_basic_block(format!("{}-entry", ir_func.name))
        .map_err(|e| into_err(e))?;

    state.set_cur_basic_block(Some(basic_block_name));
    state.set_cur_func(Some(fn_full_name));

    assert!(ir_func.params.len() != func.parameters.map(|vec| vec.len()).unwrap_or(0));

    drop(ty_env_guard);

    let func_block_id = state.cur_block_id;
    for token in body {
        state.cur_block_id = func_block_id;
        build_token(state, token)?;
    }

    // Add a "invisible" return at the end of the last block if this is a
    // function with no return type. If an end instruction is manually set it the
    // code, it must be a "return <void>".
    if matches!(ir_func.ret_ty, Type::Void) {
        if let Some(last_block) = ir_func.last_mut() {
            match last_block.get_end_instruction() {
                None => last_block.set_end_instruction(EndInstr::Return(None)),
                Some(EndInstr::Return(None)) => (),
                Some(end_instr) => {
                    return Err(LangError::new(
                        format!(
                            "Found bad end instruction in func \"{:?}\". \
                            Expected \"return void\" or not set, but found: {:#?}",
                            &state.cur_func()?.name,
                            end_instr,
                        ),
                        LangErrorKind::IrError,
                        Some(func.file_pos.to_owned()),
                    ))
                }
            }
        } else {
            return Err(LangError::new(
                format!("No basic block in func: {:?}", &state.cur_func()?.name),
                LangErrorKind::IrError,
                Some(func.file_pos.to_owned()),
            ));
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
        let func = state.cur_func_mut()?;

        let merge_block_name = func
            .insert_basic_block(format!("{}-anon.merge", cur_func_name))
            .map_err(|e| into_err(e))?;
        state.merge_blocks.insert(anon_block_id, merge_block_name);

        let cur_block = state.cur_basic_block_mut()?;
        cur_block.set_end_instruction(EndInstr::Branch(merge_block_name));

        state.set_cur_basic_block(Some(merge_block_name));
    } else {
        state.set_cur_basic_block(None);
    }

    Ok(())
}

/// All the "ParseToken" in the body should be "IfCase"s.
fn build_if(state: &mut BuildState, body: &[AstToken]) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();
    let cur_block_name = state.cur_basic_block()?.name.clone();
    let if_block_id = state.cur_block_id;

    // Create and store the basic blocks of this if-statement.
    // For every if-case that has a expression (if/elif), an extra block will be
    // created which will contain the branching logic between the cases.
    let mut prev_block_name = cur_block_name;
    let mut branch_info = BranchInfo::new();

    branch_info.if_branches.push(cur_block_name.clone());
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
                let branch_block_name = state
                    .cur_func_mut()?
                    .insert_basic_block(format!("{}-if.branch", cur_func_name))
                    .map_err(|e| into_err(e))?;

                branch_info.if_branches.push(branch_block_name.clone());
                prev_block_name = branch_block_name;
            }

            let case_block_name = state
                .cur_func_mut()?
                .insert_basic_block_after(format!("{}-if.case", cur_func_name), &prev_block_name)
                .map_err(|e| into_err(e))?;

            prev_block_name = case_block_name.clone();
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
        let merge_block_name = state
            .cur_func_mut()?
            .insert_basic_block_after(format!("{}-if.merge", cur_func_name), &prev_block_name)
            .map_err(|e| into_err(e))?;
        state
            .merge_blocks
            .insert(if_block_id, merge_block_name.clone());

        Some(merge_block_name)
    } else {
        None
    };

    // Iterate through all "if cases" in this if-statement and compile them.
    for (case_idx, mut if_case) in body.iter().enumerate() {
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

    let case_block = branch_info.get_if_case(state, idx)?;

    // If this is a if case with a expression, the branch condition should be
    // evaluated and branched from the branch block.
    if let Some(case_expr) = case_expr_opt {
        let branch_block = branch_info.get_if_branch(state, idx)?;

        // If there are no more branch blocks, set the next branch block to
        // the merge block if there are no more if_cases or set it to the
        // last if_case if there is still one left.
        let next_branch_block = if idx + 1 >= branch_info.if_branches.len() {
            if idx + 1 >= branch_info.if_cases.len() {
                state.get_merge_block(block_id)?
            } else {
                branch_info.get_if_case(state, idx + 1)?
            }
        } else {
            branch_info.get_if_branch(state, idx + 1)?
        };

        state.set_cur_basic_block(Some(branch_block.name.clone()));

        let expr_val = build_expr(case_expr);

        let end_instr = EndInstr::BranchIf(
            expr_val,
            case_block.name.clone(),
            next_branch_block.name.clone(),
        );
        state.cur_basic_block_mut()?.set_end_instruction(end_instr);
    }

    // Compile all tokens inside this if-case.
    state.set_cur_basic_block(Some(case_block.name.clone()));
    for token in body {
        state.cur_block_id = block_id;
        build_token(state, token)?;
    }

    // Add a branch to the merge block if the current basic block doesn't have a
    // terminator yet.
    let cur_basic_block = state.cur_basic_block_mut()?;
    if !cur_basic_block.has_end_instruction() {
        let merge_block = state.get_merge_block(block_id)?;
        let end_instr = EndInstr::Branch(merge_block.name.clone());
        cur_basic_block.set_end_instruction(end_instr);
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
    let start_block_name = state.cur_basic_block()?.name.clone();

    let mut cases = Vec::default();
    let mut blocks_without_branch = Vec::default();

    // Iterate through all "match cases" in this match-statement and compile them.
    // This will NOT compile the default block. It is done in iteration after
    // this to ensure that the default block is generated after all other block
    // to keep the sequential flow.
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
    let mut default_block_name_opt = None;

    // TODO: Fix the "unreachable" default block. Is not always unreachable.
    // Iterate through the match cases one more time to find the default block.
    // Also ensure that it only exists a single default block. If no default
    // block exists, create a "unreachable" instruction. This might not be
    // correct atm since there might be times when all values aren't covered
    // by the match cases.
    for mut match_case in body {
        state.cur_block_id = block_id;

        if let AstToken::Block(Block {
            header: BlockHeader::MatchCase(None),
            body: case_body,
            id: case_id,
            ..
        }) = &mut match_case
        {
            build_match_default_case(
                state,
                &mut default_block_name_opt,
                body,
                *case_id,
                &mut blocks_without_branch,
            )?;
        }
    }

    // If None: No default block found, create a new default block that contains
    // a single unreachable instruction.
    let default_block_name = if let Some(default_block_name) = default_block_name_opt {
        default_block_name
    } else {
        let cur_func = state.cur_func_mut()?;
        let cur_block_name = state.cur_basic_block()?.name();

        let default_block_name = state
            .cur_func_mut()?
            .insert_basic_block_after(format!("{}-match.default", cur_func_name), cur_block_name)
            .map_err(|e| into_err(e))?;

        if let Some(default_block) = cur_func.get_mut(&default_block_name) {
            default_block.set_end_instruction(EndInstr::Unreachable);
        } else {
            unreachable!()
        }

        default_block_name
    };

    state.set_cur_basic_block(Some(default_block_name));

    // The merge block that all cases will branch to after the switch-statement
    // if they don't branch away themselves.
    // This will become the "current basic block" when this function returns.
    let merge_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after(
            format!("{}-match.merge", cur_func_name),
            &default_block_name,
        )
        .map_err(|e| into_err(e))?;

    for block_name in blocks_without_branch {
        if let Some(block_without_branch) = state.cur_func_mut()?.get_mut(&block_name) {
            let end_instr = EndInstr::Branch(merge_block_name.clone());
            block_without_branch.set_end_instruction(end_instr);
        } else {
            unreachable!()
        }
    }

    state.set_cur_basic_block(Some(start_block_name));
    let match_val = build_expr();

    if let Some(start_block) = state.cur_func_mut()?.get_mut(&start_block_name) {
        let end_instr = EndInstr::BranchSwitch(match_val, default_block_name, cases);
        start_block.set_end_instruction(end_instr);
    } else {
        unreachable!()
    }

    state.set_cur_basic_block(Some(merge_block_name));

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

    let cur_block = state.cur_basic_block()?;
    let case_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after(format!("{}-match.case", cur_func_name), cur_block.name())
        .map_err(|e| into_err(e))?;

    let expr_val = build_expr(case_expr);

    // Compile all tokens inside this match-case.
    state.set_cur_basic_block(Some(case_block_name));
    for token in body {
        state.cur_block_id = block_id;
        build_token(state, token)?;
    }

    cases.push((expr_val, case_block_name));

    // If the body of the match case doesn't have a ending branch instruction, an
    // "ending" branch needs to be added. Store the current block in
    // `blocks_without_branch`. After this for-loop is done, all the blocks in
    // that vector will be given a branch to the merge block. The merge block
    // will be created after this loop.
    let cur_block = state.cur_basic_block()?;
    if !cur_block.has_end_instruction() {
        blocks_without_branch.push(cur_block.name.into());
    }

    Ok(())
}

fn build_match_default_case(
    state: &mut BuildState,
    default_block_name: &mut Option<String>,
    body: &[AstToken],
    block_id: BlockId,
    blocks_without_branch: &mut Vec<String>,
) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();

    if default_block_name.is_some() {
        return Err(LangError::new(
            "More than one default block found in match.".into(),
            LangErrorKind::IrError,
            None,
        ));
    }

    let cur_block = state.cur_basic_block()?;
    let new_default_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after(format!("{}-match.default", cur_func_name), cur_block.name())
        .map_err(|e| into_err(e))?;
    *default_block_name = Some(new_default_block_name);

    // Compile all tokens inside this default match-case.
    state.set_cur_basic_block(*default_block_name);
    for token in body {
        state.cur_block_id = block_id;
        build_token(state, token)?;
    }

    let cur_block = state.cur_basic_block()?;
    if !cur_block.has_end_instruction() {
        blocks_without_branch.push(cur_block.name.into());
    }

    Ok(())
}

fn build_while(
    state: &mut BuildState,
    expr_opt: Option<&Expr>,
    body: &[AstToken],
) -> LangResult<()> {
    let cur_func_name = state.cur_func()?.name.clone();
    let cur_block_name = state.cur_basic_block()?.name.clone();
    let while_block_id = state.cur_block_id;

    let branch_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after(format!("{}-while.branch", cur_func_name), &cur_block_name)
        .map_err(|e| into_err(e))?;
    let body_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after(format!("{}-while.body", cur_func_name), &branch_block_name)
        .map_err(|e| into_err(e))?;
    let merge_block_name = state
        .cur_func_mut()?
        .insert_basic_block_after(format!("{}-while.merge", cur_func_name), &body_block_name)
        .map_err(|e| into_err(e))?;

    state.merge_blocks.insert(while_block_id, merge_block_name);

    if let Some(cur_block) = state.cur_func_mut()?.get_mut(&cur_block_name) {
        let end_instr = EndInstr::Branch(branch_block_name.clone());
        cur_block.set_end_instruction(end_instr)
    } else {
        unreachable!()
    }

    // If expression is NOT set, treat this as a infinite while loop.
    state.set_cur_basic_block(Some(branch_block_name.clone()));
    if let Some(expr) = expr_opt {
        let expr_val = build_expr(expr);
        let end_instr =
            EndInstr::BranchIf(expr_val, body_block_name.clone(), merge_block_name.clone());
        state.cur_basic_block_mut()?.set_end_instruction(end_instr);
    } else {
        let end_instr = EndInstr::Branch(body_block_name.clone());
        state.cur_basic_block_mut()?.set_end_instruction(end_instr);
    }

    // Iterate through all "tokens" in this while-loop and compile them.
    state.set_cur_basic_block(Some(body_block_name.clone()));
    for token in body {
        state.cur_block_id = while_block_id;
        state.cur_branch_block_name = Some(branch_block_name.clone());
        build_token(state, token)?;
    }

    // If the block does NOT contain a terminator instruction inside it (return,
    // yield etc.), add a unconditional branch back up to the "while.branch" block.
    if !state.cur_basic_block()?.has_end_instruction() {
        let end_instr = EndInstr::Branch(branch_block_name.clone());
        state.cur_basic_block_mut()?.set_end_instruction(end_instr);
    }

    state.set_cur_basic_block(Some(merge_block_name));
    state.cur_branch_block_name = None;

    Ok(())
}
