use crate::{expr::ExprTy, generator::CodeGen};
use common::{
    error::{
        CustomResult, LangError,
        LangErrorKind::{self, CodeGenError},
    },
    file::FilePosition,
    token::{
        ast::AstToken,
        block::{BlockHeader, Enum, Function, Struct},
        expr::{Expr, Var},
    },
    ty::{inner_ty::InnerTy, ty::Ty},
    BlockId,
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    module::Linkage,
    types::AnyTypeEnum,
    values::{AggregateValue, AnyValueEnum, FunctionValue, IntValue, PointerValue},
};
use log::{debug, warn};

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

    /// Returns the "if case" basic block at index `index` if one exists.
    pub fn get_if_case(
        &self,
        index: usize,
        file_pos: &FilePosition,
    ) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_cases.get(index) {
            Ok(*basic_block)
        } else {
            Err(LangError::new(
                format!("Unable to get if_case with index: {}", index),
                CodeGenError {
                    file_pos: file_pos.to_owned(),
                },
            ))
        }
    }

    /// Returns the "if branch" basic block at index `index` if one exists.
    pub fn get_if_branch(
        &self,
        index: usize,
        file_pos: &FilePosition,
    ) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_branches.get(index) {
            Ok(*basic_block)
        } else {
            Err(LangError::new(
                format!("Unable to get if_branch with index: {}", index),
                CodeGenError {
                    file_pos: file_pos.to_owned(),
                },
            ))
        }
    }
}

/// The type information for something that is being generated. This can for
/// example be used to ensure that "enum"s are handled correctly in match cases.
/// Since match cases needs to be ints, the enums needs to be converted to ints.
enum CodeGenTy {
    Int,
    Enum,
}

impl CodeGenTy {
    fn new(ty: &Ty) -> CustomResult<Self> {
        // TODO: Add more types.
        match ty {
            Ty::CompoundType(inner_ty, ..) => match inner_ty {
                InnerTy::Enum(_) => Ok(CodeGenTy::Enum),
                _ if inner_ty.is_int() => Ok(CodeGenTy::Int),
                _ => Err(LangError::new(
                    format!("Invalid type when creating CodeGenTy: {:#?}", ty),
                    LangErrorKind::GeneralError,
                )),
            },

            _ => Err(LangError::new(
                format!("Invalid type when creating CodeGenTy: {:#?}", ty),
                LangErrorKind::GeneralError,
            )),
        }
    }

    /// Helper function to get the given `value` as a integer const.
    fn as_int_const<'ctx>(&self, value: AnyValueEnum<'ctx>) -> CustomResult<IntValue<'ctx>> {
        let value = match self {
            CodeGenTy::Int => value,

            CodeGenTy::Enum => {
                assert!(value.is_struct_value());

                value
                    .into_struct_value()
                    .const_extract_value(&mut [0])
                    .into()
            }
        };

        if value.is_int_value() {
            Ok(value.into_int_value())
        } else {
            Err(LangError::new(
                format!(
                    "Given `value` expected to be const int, was not: {:#?}",
                    value
                ),
                LangErrorKind::CodeGenError {
                    file_pos: FilePosition::default(),
                },
            ))
        }
    }

    /// Helper function to get the given `value` as a integer.
    fn as_int<'ctx>(
        &self,
        builder: &Builder<'ctx>,
        value: AnyValueEnum<'ctx>,
    ) -> CustomResult<IntValue<'ctx>> {
        let value = match self {
            CodeGenTy::Int => value,

            CodeGenTy::Enum => {
                assert!(value.is_struct_value());
                let basic_value = CodeGen::any_into_basic_value(value)?;

                // TODO: Is there a better way to do this other than storing it
                //       on the stack temporarily to GEP?
                let enum_ptr = builder.build_alloca(basic_value.get_type(), "enum.as.int.alloc");
                builder.build_store(enum_ptr, basic_value);

                let value_ptr = builder
                    .build_struct_gep(enum_ptr, 0, "enum.as.int.gep")
                    .map_err(|_| {
                        LangError::new(
                            format!("Unable to GEP enum: {:#?}", &value),
                            LangErrorKind::GeneralError,
                        )
                    })?;
                builder.build_load(value_ptr, "enum.as.int.load").into()
            }
        };

        if value.is_int_value() {
            Ok(value.into_int_value())
        } else {
            Err(LangError::new(
                format!("Given `value` expected to be int, was not: {:#?}", value),
                LangErrorKind::CodeGenError {
                    file_pos: FilePosition::default(),
                },
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
                self.compile_func(&func.borrow(), id, body)?;
            }
            BlockHeader::Implement(_) => {
                for mut ast_token in body {
                    if let AstToken::Block(BlockHeader::Function(func), func_id, func_body) =
                        &mut ast_token
                    {
                        self.compile_func(&func.borrow(), *func_id, func_body)?;
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

            BlockHeader::Match(expr) => self.compile_match(expr, id, body)?,
            BlockHeader::MatchCase(_) => {
                return Err(self.err("Unexpected MatchCase in compile_block".into()));
            }

            BlockHeader::Struct(_) | BlockHeader::Enum(_) | BlockHeader::Interface(_) => {
                // All structs, enums and interfaces already compiled at this stage.
            }

            //BlockHeader::For(var, expr) => self.compile_for(var, expr),
            BlockHeader::While(expr_opt) => self.compile_while(expr_opt, id, body)?,

            //BlockHeader::With(expr) => self.compile_with(expr),
            //BlockHeader::Defer(expr) => self.compile_defer(expr),

            //BlockHeader::Test(test_func) => self.compile_test_func(expr),
            _ => panic!(format!("TODO: compile_block type: {:?}", header)),
        }
        Ok(())
    }

    fn alloc_param(&self, var: &Var) -> CustomResult<PointerValue<'ctx>> {
        if let Some(func) = self.cur_func {
            let entry = func
                .get_first_basic_block()
                .expect("Unable to unwrap first basic block in func.");

            // TODO: FIXME: Currently, a new alloca is added at the end of the
            //       entry block. Can this be a problem? Will this function ever
            //       be called when other instructions have been added?
            self.builder.position_at_end(entry);
            self.alloc_var(var)
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
        let fn_val = if let Some(fn_val) = self.module.get_function(&func.full_name()?) {
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

        if params.len() != fn_val.get_params().len() {
            return Err(self.err(format!(
                "Incorrect amount of parameters when generating function \"{:?}\". fn_val len: {}, params len: {}",
                &func.full_name(),
                fn_val.get_params().len(),
                params.len()
            )));
        }

        // TODO: How does this work with variadic parameters? Currently varargs
        //       aren't supported for functions written in the language itself,
        //       it is only allowed in external functions (for C interop).
        // Allocated space for the function parameters on the stack.
        for (param_value, param) in fn_val.get_param_iter().zip(params) {
            let ptr = self.alloc_param(&param.borrow())?;
            self.builder.build_store(ptr, param_value);

            let key = (param.borrow().full_name(), func_id);
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
                let param = param.borrow();

                if let Some(param_type_struct) = &param.ty {
                    let any_type = self.compile_type(&param_type_struct)?;
                    let basic_type = CodeGen::any_into_basic_type(any_type)?;
                    inner_types.push(basic_type);
                } else {
                    return Err(self.err(format!(
                        "Bad type for parameter with name\"{}\" in function \"{}\".",
                        &param.name,
                        &func.full_name()?
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

        Ok(self
            .module
            .add_function(&func.full_name()?, fn_type, linkage_opt))
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
            if let AstToken::Block(BlockHeader::IfCase(expr_opt), _, _) = &if_case {
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
        for (index, mut if_case) in body.iter_mut().enumerate() {
            self.cur_block_id = id;

            if let AstToken::Block(BlockHeader::IfCase(expr_opt), inner_id, inner_body) =
                &mut if_case
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
        let if_case_block = branch_info.get_if_case(index, &self.cur_file_pos)?;

        // If this is a if case with a expression, the branch condition should
        // be evaluated and branched from the branch block.
        if let Some(br_expr) = br_expr_opt {
            let branch_block = branch_info.get_if_branch(index, &self.cur_file_pos)?;

            // If there are no more branch blocks, set the next branch block to
            // the merge block if there are no more if_cases or set it to the
            // last if_case if there is still one left.
            let next_branch_block = if index + 1 >= branch_info.if_branches.len() {
                if index + 1 >= branch_info.if_cases.len() {
                    self.get_merge_block(id)?
                } else {
                    branch_info.get_if_case(index + 1, &self.cur_file_pos)?
                }
            } else {
                branch_info.get_if_branch(index + 1, &self.cur_file_pos)?
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

    /// All the "ParseToken"s in the body should be "MatchCase"s.
    fn compile_match(
        &mut self,
        expr: &mut Expr,
        id: BlockId,
        body: &mut [AstToken],
    ) -> CustomResult<()> {
        let start_block = self
            .cur_basic_block
            .ok_or_else(|| self.err("cur_block is None for \"If\".".into()))?;

        let codegen_ty = CodeGenTy::new(&expr.get_expr_type()?)?;

        let mut cases = Vec::default();
        let mut blocks_without_branch = Vec::default();

        // Iterate through all "match cases" in this match-statement and compile them.
        // This will NOT compile the default block. It is done in iteration after
        // this to ensure that the default block is generated after all other block
        // to keep the sequential flow.
        for mut match_case in body.iter_mut() {
            self.cur_block_id = id;

            if let AstToken::Block(BlockHeader::MatchCase(Some(case_expr)), _, inner_body) =
                &mut match_case
            {
                let cur_block = self.cur_basic_block.unwrap();
                let match_case_block = self
                    .context
                    .insert_basic_block_after(cur_block, "match.case");

                let value_expr = self.compile_expr(case_expr, ExprTy::RValue)?;
                let value = codegen_ty.as_int_const(value_expr)?;

                // The case expressions in a switch needs to be constant.
                if !value.is_constant_int() {
                    return Err(self.err(format!(
                        "Expression in match case not constant: {:#?}",
                        case_expr
                    )));
                }

                // Compile all tokens inside this match-case.
                self.cur_basic_block = Some(match_case_block);
                self.builder.position_at_end(match_case_block);
                for token in inner_body {
                    self.cur_block_id = id;
                    self.compile(token)?;
                }

                // If the body of the match case doesn't have a ending branch
                // instruction, a "ending" branch needs to be added. Store the
                // current block in `blocks_without_branch`. After this for-loop
                // is done, all the blocks in that vector will be given a branch
                // to the merge block. The merge block will be created after this
                // loop.
                let cur_block = self.cur_basic_block.unwrap();
                if cur_block.get_terminator().is_none() {
                    blocks_without_branch.push(cur_block);
                }

                cases.push((value, match_case_block));
            } else if let AstToken::Block(BlockHeader::MatchCase(None), ..) = &match_case {
                // Default block will be handled in logic below. Ignore for now.
            } else {
                return Err(self.err("Token in \"Match\" block wasn't a \"MatchCase\".".into()));
            }
        }

        // The default block that control flow will be branched to if no cases matches.
        let mut default_block_opt = None;

        // TODO: Fix the "unreachable" default block. Is not always unreachable.
        // Iterate through the match cases one more time to find the default block.
        // Also ensure that it only exists a single default block. If no default
        // block exists, create a "unreachable" instruction. This might not be
        // correct atm since there might be times when all values aren't covered
        // by the match cases.
        for mut match_case in body.iter_mut() {
            self.cur_block_id = id;

            if let AstToken::Block(BlockHeader::MatchCase(None), _, inner_body) = &mut match_case {
                if default_block_opt.is_some() {
                    return Err(self.err("More than one default block found in match.".into()));
                }

                let cur_block = self.cur_basic_block.unwrap();
                default_block_opt = Some(
                    self.context
                        .insert_basic_block_after(cur_block, "match.default"),
                );

                // Compile all tokens inside this default match-case.
                self.cur_basic_block = Some(default_block_opt.unwrap());
                self.builder.position_at_end(default_block_opt.unwrap());
                for token in inner_body {
                    self.cur_block_id = id;
                    self.compile(token)?;
                }

                let cur_block = self.cur_basic_block.unwrap();
                if cur_block.get_terminator().is_none() {
                    blocks_without_branch.push(cur_block);
                }
            }
        }

        // If None: No default block found, create a new default block that
        // contains a single unreachable instruction.
        let default_block = if let Some(default_block) = default_block_opt {
            default_block
        } else {
            let default_block = self
                .context
                .insert_basic_block_after(self.cur_basic_block.unwrap(), "match.default");

            self.builder.position_at_end(default_block);
            self.builder.build_unreachable();

            default_block
        };

        self.cur_basic_block = Some(default_block);

        // The merge block that all cases will branch to after the switch-statement
        // if they don't branch away themselves.
        // This will become the "current basic block" when this function returns.
        let merge_block = self
            .context
            .insert_basic_block_after(self.cur_basic_block.unwrap(), "switch.merge");

        for block_without_branch in blocks_without_branch {
            self.builder.position_at_end(block_without_branch);
            self.builder.build_unconditional_branch(merge_block);
        }

        self.builder.position_at_end(start_block);

        let value_expr = self.compile_expr(expr, ExprTy::RValue)?;
        let value = codegen_ty.as_int(self.builder, value_expr)?;

        self.builder.build_switch(value, default_block, &cases);

        self.cur_basic_block = Some(merge_block);
        self.builder.position_at_end(merge_block);

        Ok(())
    }

    pub(super) fn compile_struct(&mut self, struct_: &Struct) -> CustomResult<()> {
        debug!("Compiling struct -- {:#?}", struct_);

        // Go through all members of the struct and create a vector containing
        // all their types.
        let member_types = if let Some(members) = &struct_.members {
            let mut v = Vec::with_capacity(members.len());
            for member in members {
                let member = member.borrow();

                if let Some(member_type_struct) = &member.ty {
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
        let struct_ty = self.context.opaque_struct_type(&struct_.name);
        struct_ty.set_body(member_types.as_ref(), packed);

        Ok(())
    }

    pub(super) fn compile_enum(&mut self, enum_: &Enum) -> CustomResult<()> {
        debug!("Compiling enum -- {:#?}", enum_);

        // Create a new struct type containing a single member that has the type
        // of the "inner enum type". This will most likely be a integer type.
        //
        // The type of the whole `enum_` will be Enum(ident). This type will be
        // set for the members as well. Only the given literal values of the members
        // will be the inner type.
        let ty = if let Some(members) = &enum_.members {
            if let Some(member) = members.first() {
                if let Some(ty) = &member.borrow().value {
                    ty.get_expr_type()?
                } else {
                    return Err(self.err(format!(
                        "No default value set for first member in enum \"{}\".",
                        &enum_.name
                    )));
                }
            } else {
                return Err(self.err(format!(
                    "Unable to find first member in enum \"{}\".",
                    &enum_.name
                )));
            }
        } else {
            return Err(self.err(format!(
                "No members set for enum \"{}\". Unable to figure out member type.",
                &enum_.name
            )));
        };

        let any_ty = self.compile_type(&ty)?;
        let basic_ty = CodeGen::any_into_basic_type(any_ty)?;

        let packed = false;
        let enum_ty = self.context.opaque_struct_type(&enum_.name);
        enum_ty.set_body(&[basic_ty], packed);

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
