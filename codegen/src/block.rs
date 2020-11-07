use crate::{expr::ExprTy, generator::CodeGen};
use common::{
    error::{CustomResult, LangError, LangErrorKind::CodeGenError},
    token::ast::Token,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function, Struct},
        expr::{Expr, Var},
    },
    BlockId,
};
use inkwell::{
    basic_block::BasicBlock,
    module::Linkage,
    types::AnyTypeEnum,
    values::{FunctionValue, PointerValue},
};

/// Contains information related to branches in either a if-statement or a
/// match-statement. This will then be sent around to all if-cases so that
/// they can see all information about where to branch etc.
struct BranchInfo<'ctx> {
    // Vectors are sorted, so the first if case/branch is at index 0 etc.
    pub if_cases: Vec<BasicBlock<'ctx>>,
    pub if_branches: Vec<BasicBlock<'ctx>>,
}

// TODO: What to do about line/column for errors in "BranchInfo"? Currently it
//       has no way of knowing that information.
impl<'ctx> BranchInfo<'ctx> {
    pub fn new() -> Self {
        Self {
            if_cases: Vec::default(),
            if_branches: Vec::default(),
        }
    }

    // TODO: Takes linenr/column_nr to be able to add them to error message.
    //       Find a better way to do this.
    pub fn get_if_case(
        &self,
        index: usize,
        line_nr: u64,
        column_nr: u64,
    ) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_cases.get(index) {
            Ok(*basic_block)
        } else {
            Err(LangError::new(
                format!("Unable to get if_case with index: {}", index),
                CodeGenError { line_nr, column_nr },
            ))
        }
    }

    // TODO: Takes linenr/column_nr to be able to add them to error message.
    //       Find a better way to do this.
    pub fn get_if_branch(
        &self,
        index: usize,
        line_nr: u64,
        column_nr: u64,
    ) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_branches.get(index) {
            Ok(*basic_block)
        } else {
            Err(LangError::new(
                format!("Unable to get if_branch with index: {}", index),
                CodeGenError { line_nr, column_nr },
            ))
        }
    }
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_block(
        &mut self,
        header: &mut BlockHeader,
        id: BlockId,
        body: &mut [AstToken],
    ) -> CustomResult<()> {
        self.cur_block_id = id;

        match header {
            BlockHeader::Default => {
                for token in body {
                    self.compile(token)?
                }
            }
            BlockHeader::Function(func) => {
                self.compile_func(&func, id, body)?;
            }
            BlockHeader::Implement(_) => {
                for ast_token in body {
                    if let Token::Block(BlockHeader::Function(func), func_id, func_body) =
                        &mut ast_token.token
                    {
                        // The method will already have been renamed to be prefixed
                        // with the struct name, so no need to do it here.
                        self.compile_func(&func, *func_id, func_body)?;
                    }
                }
            }
            BlockHeader::Anonymous => {
                self.compile_anon(id, body)?;
            }
            BlockHeader::If => {
                self.compile_if(id, body)?;
            }
            BlockHeader::IfCase(_) => {
                return Err(self.err("Unexpected IfCase in compile_block".into()));
            }

            BlockHeader::Struct(_) | BlockHeader::Enum(_) | BlockHeader::Interface(_) => {
                // All structs, enums and interfaces already compiled at this stage.
            }
            //BlockHeader::Match(expr) => self.compile_match(expr),
            //BlockHeader::MatchCase(expr) => self.compile_match_case(expr),

            //BlockHeader::For(var, expr) => self.compile_for(var, expr),
            BlockHeader::While(expr_opt) => self.compile_while(expr_opt, id, body)?,

            //BlockHeader::With(expr) => self.compile_with(expr),
            //BlockHeader::Defer(expr) => self.compile_defer(expr),

            //BlockHeader::Test(test_func) => self.compile_test_func(expr),
            _ => panic!(format!("TODO: compile_block type: {:?}", header)),
        }
        Ok(())
    }

    fn create_entry_block_alloca(&self, var: &Var) -> CustomResult<PointerValue<'ctx>> {
        if let Some(func) = self.cur_func {
            let entry = func
                .get_first_basic_block()
                .expect("Unable to unwrap first basic block in func.");

            // TODO: FIXME: Currently, a new alloca is added at the end of the
            //       entry block. Can this be a problem? Will this function ever
            //       be called when other instructions have been added?
            self.builder.position_at_end(entry);
            self.alloca_var(var)
        } else {
            Err(self.err(format!(
                "No active cur func when creating var: {}",
                &var.name
            )))
        }
    }

    fn compile_func(
        &mut self,
        func: &Function,
        func_id: BlockId,
        body: &mut [AstToken],
    ) -> CustomResult<()> {
        let fn_val = if let Some(fn_val) = self.module.get_function(&func.name) {
            fn_val
        } else {
            return Err(self.err(format!(
                "Unable to find function with name \"{}\".",
                func.name
            )));
        };

        let entry = self.context.append_basic_block(fn_val, "entry");

        self.cur_basic_block = Some(entry);
        self.cur_func = Some(fn_val);
        self.builder.position_at_end(entry);

        let empty_vec = Vec::default();
        let params = if let Some(params) = &func.parameters {
            params
        } else {
            &empty_vec
        };

        // TODO: How does this work with variadic parameters?
        // Get names for the parameters and alloc space in the functions stack.
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let param = params.get(i).ok_or_else(|| {
                self.err(format!(
                    "No param at index {} for function {}",
                    i, &func.name
                ))
            })?;

            let ptr = self.create_entry_block_alloca(param)?;
            self.builder.build_store(ptr, arg);

            let key = (param.name.clone(), func_id);
            self.variables.insert(key, ptr);
        }

        // Compile the tokens in the body of the function.
        for token in body {
            self.cur_block_id = func_id;
            self.compile(token)?;
        }

        // Add a "invisible" return at the end of the last block if this is a
        // function with no return type. Also check to see if this block
        // contains a return stmt even though it should NOT return anything.
        if func.ret_type.is_none() {
            if let Some(last_block) = fn_val.get_last_basic_block() {
                if last_block.get_terminator().is_none() {
                    self.builder.position_at_end(last_block);
                    self.builder.build_return(None);
                    Ok(())
                } else {
                    Err(self.err(format!(
                        "Found return stmt in func \"{}\", but it has no return type.",
                        &func.name
                    )))
                }
            } else {
                Err(self.err(format!("No basic block in func: {}", &func.name)))
            }
        } else {
            Ok(())
        }
    }

    /// Compoles a function prototype.
    pub(super) fn compile_func_proto(
        &self,
        func: &Function,
        linkage_opt: Option<Linkage>,
    ) -> CustomResult<FunctionValue<'ctx>> {
        let param_types = if let Some(params) = &func.parameters {
            let mut inner_types = Vec::with_capacity(params.len());
            for param in params {
                if let Some(param_type_struct) = &param.ret_type {
                    let any_type = self.compile_type(&param_type_struct)?;
                    let basic_type = CodeGen::any_into_basic_type(any_type)?;
                    inner_types.push(basic_type);
                } else {
                    return Err(self.err(format!(
                        "Bad type for parameter with name\"{}\" in function \"{}\".",
                        &param.name, &func.name
                    )));
                }
            }
            inner_types
        } else {
            Vec::default()
        };

        // Get the return type of the function and create a "codegen" function
        // with a return type of this type. If no `ret_type` is set, this is
        // a function that returns void, create a void function.
        let fn_type = if let Some(ret_type) = &func.ret_type {
            match self.compile_type(&ret_type)? {
                AnyTypeEnum::ArrayType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::FloatType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::FunctionType(ty) => ty,
                AnyTypeEnum::IntType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::PointerType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::StructType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::VectorType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::VoidType(ty) => ty.fn_type(param_types.as_slice(), func.is_var_arg),
            }
        } else {
            self.context
                .void_type()
                .fn_type(param_types.as_slice(), func.is_var_arg)
        };

        Ok(self.module.add_function(&func.name, fn_type, linkage_opt))
    }

    fn compile_anon(&mut self, id: BlockId, body: &mut [AstToken]) -> CustomResult<()> {
        let cur_func = self
            .cur_func
            .ok_or_else(|| self.err("cur_func is None for \"If\".".into()))?;

        let mut cur_block = self
            .cur_basic_block
            .ok_or_else(|| self.err("cur_block is None for \"If\".".into()))?;

        for token in body.iter_mut() {
            self.cur_block_id = id;
            cur_block = self
                .cur_basic_block
                .ok_or_else(|| self.err("cur_block is None for \"While\" body.".into()))?;

            self.builder.position_at_end(cur_block);
            self.compile(token)?;
        }

        // If all paths in this block doesn't branch away, it needs to branch
        // to a merge block. Otherwise, if all paths branches away, no merge
        // block should be created.
        if let Some(block_info) = self.analyze_context.block_info.get(&id) {
            self.cur_basic_block =
                if !block_info.all_children_contains_returns || !block_info.contains_return {
                    let merge_block = self.context.append_basic_block(cur_func, "anon.merge");
                    self.merge_blocks.insert(id, merge_block);

                    self.builder.position_at_end(cur_block);
                    self.builder.build_unconditional_branch(merge_block);

                    self.builder.position_at_end(merge_block);
                    Some(merge_block)
                } else {
                    None
                };

            Ok(())
        } else {
            Err(self.err(format!(
                "Unable to find block info for block with ID: {}",
                id
            )))
        }
    }

    /// All the "ParseToken" in the body should be "IfCase"s.
    fn compile_if(&mut self, id: BlockId, body: &mut [AstToken]) -> CustomResult<()> {
        let cur_block = self
            .cur_basic_block
            .ok_or_else(|| self.err("cur_block is None for \"If\".".into()))?;

        // Create and store the "body" blocks of this if-statement.
        // For every if-case that has a expression (if/elif) a extra block
        // will be created which will contain the branching logic between the
        // cases.
        let mut prev_block = cur_block;
        let mut branch_info = BranchInfo::new();
        branch_info.if_branches.push(cur_block);
        for (i, if_case) in body.iter().enumerate() {
            if let Token::Block(BlockHeader::IfCase(expr_opt), _, _) = &if_case.token {
                // Skip adding a branch block if this is the first case (since it
                // has the branch block `cur_block`). Also only add a branch block
                // if this `if_case` contains a expression that can be "branched on".
                if i > 0 && expr_opt.is_some() {
                    let br_block = self
                        .context
                        .insert_basic_block_after(prev_block, "if.branch");
                    prev_block = br_block;
                    branch_info.if_branches.push(br_block);
                }

                let if_block = self.context.insert_basic_block_after(prev_block, "if.case");
                prev_block = if_block;
                branch_info.if_cases.push(if_block);
            } else {
                return Err(self.err(format!(
                    "Token in \"If\" block wasn't a \"IfCase\": {:?}",
                    &if_case
                )));
            }
        }

        // Add a "merge block" that the if-cases will merge to if they don't
        // branch away. The merge block will NOT be created if all if-cases
        // contains a return instruction. This is because there is no possiblity
        // to end up in the merge block in that case, so it would just be empty.
        let merge_block_opt = if let Some(block_info) = self.analyze_context.block_info.get(&id) {
            if !block_info.all_children_contains_returns {
                let merge_block = self
                    .context
                    .insert_basic_block_after(prev_block, "if.merge");
                self.merge_blocks.insert(id, merge_block);
                Some(merge_block)
            } else {
                None
            }
        } else {
            return Err(self.err(format!(
                "Unable to find block info for block with ID: {}",
                id
            )));
        };

        // Iterate through all "if cases" in this if-statement and compile them.
        for (index, if_case) in body.iter_mut().enumerate() {
            self.cur_block_id = id;

            if let Token::Block(
                BlockHeader::IfCase(ref mut expr_opt),
                inner_id,
                ref mut inner_body,
            ) = &mut if_case.token
            {
                self.compile_if_case(
                    expr_opt,
                    *inner_id,
                    index,
                    inner_body.as_mut_slice(),
                    &branch_info,
                )?;
            } else {
                return Err(self.err("Token in \"If\" block wasn't a \"IfCase\".".into()));
            }
        }

        // The if statement has been compiled complete. If a merge block was
        // created, set it as the current block. Otherwise just keep the old
        // cur block.
        if let Some(merge_block) = merge_block_opt {
            self.cur_basic_block = merge_block_opt;
            self.builder.position_at_end(merge_block);
        }
        Ok(())
    }

    fn compile_if_case(
        &mut self,
        br_expr_opt: &mut Option<Expr>,
        id: BlockId,
        index: usize,
        body: &mut [AstToken],
        branch_info: &BranchInfo<'ctx>,
    ) -> CustomResult<()> {
        let if_case_block = branch_info.get_if_case(index, self.cur_line_nr, self.cur_column_nr)?;

        // If this is a if case with a expression, the branch condition should
        // be evaluated and branched from the branch block.
        if let Some(br_expr) = br_expr_opt {
            let branch_block =
                branch_info.get_if_branch(index, self.cur_line_nr, self.cur_column_nr)?;

            // If there are no more branch blocks, set the next branch block to
            // the merge block if there are no more if_cases or set it to the
            // last if_case if there is still one left.
            let next_branch_block = if index + 1 >= branch_info.if_branches.len() {
                if index + 1 >= branch_info.if_cases.len() {
                    self.get_merge_block(id)?
                } else {
                    branch_info.get_if_case(index + 1, self.cur_line_nr, self.cur_column_nr)?
                }
            } else {
                branch_info.get_if_branch(index + 1, self.cur_line_nr, self.cur_column_nr)?
            };

            // TODO: Return error instead of panicing inside the
            //       "into_int_value()" function.
            self.cur_basic_block = Some(branch_block);
            self.builder.position_at_end(branch_block);
            let expr = self.compile_expr(br_expr, ExprTy::RValue)?.into_int_value();
            self.builder
                .build_conditional_branch(expr, if_case_block, next_branch_block);
        }

        // Compile all tokens inside this if-case.
        self.cur_basic_block = Some(if_case_block);
        self.builder.position_at_end(if_case_block);
        for token in body {
            self.cur_block_id = id;
            self.compile(token)?;
        }

        if let Some(cur_basic_block) = self.cur_basic_block {
            self.builder.position_at_end(cur_basic_block);
            // Add a branch to the merge block if the current basic block
            // doesn't have a terminator yet.
            if cur_basic_block.get_terminator().is_none() {
                let merge_block = self.get_merge_block(id)?;
                self.builder.build_unconditional_branch(merge_block);
            }
            Ok(())
        } else {
            Err(self.err("Current basic block None".into()))
        }
    }

    pub(super) fn compile_struct(&mut self, struct_: &Struct) -> CustomResult<()> {
        // Go through all members of the struct and create a vector containing
        // all their types.
        let member_types = if let Some(members) = &struct_.members {
            let mut v = Vec::with_capacity(members.len());
            for member in members {
                if let Some(member_type_struct) = &member.ret_type {
                    let any_type = self.compile_type(&member_type_struct)?;
                    let basic_type = CodeGen::any_into_basic_type(any_type)?;
                    v.push(basic_type);
                } else {
                    return Err(self.err(format!(
                        "Bad type for struct \"{}\" member \"{}\".",
                        &struct_.name, &member.name
                    )));
                }
            }
            v
        } else {
            Vec::default()
        };

        let packed = false;
        let struct_type = self.context.opaque_struct_type(&struct_.name);
        struct_type.set_body(member_types.as_ref(), packed);

        Ok(())
    }

    /// All the "ParseToken" in the body should be "IfCase"s.
    fn compile_while(
        &mut self,
        expr_opt: &mut Option<Expr>,
        id: BlockId,
        body: &mut [AstToken],
    ) -> CustomResult<()> {
        let mut cur_block = self
            .cur_basic_block
            .ok_or_else(|| self.err("cur_block is None for \"While\".".into()))?;

        let while_branch_block = self
            .context
            .insert_basic_block_after(cur_block, "while.branch");
        let while_body_block = self
            .context
            .insert_basic_block_after(while_branch_block, "while.body");
        let merge_block = self
            .context
            .insert_basic_block_after(while_body_block, "while.merge");
        self.merge_blocks.insert(id, merge_block);

        self.builder.position_at_end(cur_block);
        self.builder.build_unconditional_branch(while_branch_block);

        // If expression is NOT set, treat this as a infinite while loop.
        self.builder.position_at_end(while_branch_block);
        if let Some(ref mut expr) = expr_opt {
            let value = self.compile_expr(expr, ExprTy::RValue)?;
            if value.is_int_value() {
                self.builder.build_conditional_branch(
                    value.into_int_value(),
                    while_body_block,
                    merge_block,
                );
            } else {
                return Err(self.err(format!(
                    "Expression in while loop didn't evaluate to int: {:?}",
                    value
                )));
            }
        } else {
            self.builder.build_unconditional_branch(while_body_block);
        }

        // Iterate through all "tokens" in this while-loop and compile them.
        self.cur_basic_block = Some(while_body_block);
        for token in body.iter_mut() {
            self.cur_block_id = id;
            self.cur_branch_block = Some(while_branch_block);
            cur_block = self
                .cur_basic_block
                .ok_or_else(|| self.err("cur_block is None for \"While\" body.".into()))?;

            self.builder.position_at_end(cur_block);
            self.compile(token)?;
        }

        // If the block does NOT contain a terminator instruction inside it
        // (return, yield etc.), add a unconditional branch back up to the
        // "while.branch" block.
        cur_block = self
            .cur_basic_block
            .ok_or_else(|| self.err("cur_block is None for \"While\" body.".into()))?;
        if cur_block.get_terminator().is_none() {
            self.builder.position_at_end(cur_block);
            self.builder.build_unconditional_branch(while_branch_block);
        }

        self.cur_basic_block = Some(merge_block);
        self.builder.position_at_end(merge_block);
        self.cur_branch_block = None;
        Ok(())
    }
}
