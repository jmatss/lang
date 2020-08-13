use super::codegen_state::CodeGenState;
use crate::analyze::analyzer::AnalyzeContext;
use crate::common::variable_type::Type;
use crate::error::CustomError;
use crate::error::CustomError::CodeGenError;
use crate::parse::token;
use crate::parse::token::{
    BinaryOperation, Expression, Function, FunctionCall, Operation, ParseToken, TypeStruct,
    UnaryOperation, Variable,
};
use crate::{lex::token::Literal, CustomResult};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::values::{
    AnyValueEnum, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue,
};
use inkwell::{
    basic_block::BasicBlock,
    types::{AnyTypeEnum, BasicTypeEnum, PointerType},
    AddressSpace, FloatPredicate, IntPredicate,
};
use std::collections::HashMap;
use std::convert::TryFrom;
use token::{AssignOperator, BlockHeader, BlockId, Modifier, ParseTokenKind, Path, Statement};

/// Contains information related to branches in either a if-statement or a
/// match-statement. This will then be sent around to all if-cases so that
/// they can see all information about where to branch etc.
struct BranchInfo<'ctx> {
    // Vectors are sorted, so the first if case/branch is at index 0 etc.
    pub if_cases: Vec<BasicBlock<'ctx>>,
    pub if_branches: Vec<BasicBlock<'ctx>>,
}

impl<'ctx> BranchInfo<'ctx> {
    pub fn new() -> Self {
        Self {
            if_cases: Vec::default(),
            if_branches: Vec::default(),
        }
    }

    pub fn get_if_case(&self, index: usize) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_cases.get(index) {
            Ok(*basic_block)
        } else {
            Err(CodeGenError(format!(
                "Unable to get if_case with index: {}",
                index
            )))
        }
    }

    pub fn get_if_branch(&self, index: usize) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_branches.get(index) {
            Ok(*basic_block)
        } else {
            Err(CodeGenError(format!(
                "Unable to get if_branch with index: {}",
                index
            )))
        }
    }
}

struct CodeGen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,

    pub analyze_context: &'ctx AnalyzeContext,
    pub state: CodeGenState<'ctx>,

    /// Contains pointers to variables that have been compiled.
    pub variables: HashMap<(String, BlockId), PointerValue<'ctx>>,

    /// Contains pointers to variables that have been compiled.
    pub functions: HashMap<(String, BlockId), &'ctx FunctionValue<'ctx>>,
}

pub fn generate<'a, 'ctx>(
    ast_root: &'ctx ParseToken,
    analyze_context: &'ctx AnalyzeContext,
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
) -> CustomResult<()> {
    let mut code_gen = CodeGen::new(context, analyze_context, builder, module);
    code_gen.compile_recursive(ast_root)?;

    // TODO: Temporary solution, loop through all merge blocks and look for all
    //       merge blocks with no instructions. If the merge block has a "wrapping"
    //       if-statement (a nested if-statement), the merge block should branch
    //       to the wrapping merge block.
    //       If there are no wrapping if-statement, just remove the empty merge
    //       block since it (probably) isn't used. This makes the assumption that
    //       the code has no logical flaw, which one shouldn't do.
    for (block_id, merge_block) in &code_gen.state.merge_blocks {
        if merge_block.get_first_instruction().is_none() {
            if let Some(wrapping_merge_block) = code_gen.get_parent_merge_block(*block_id)? {
                if let Some(block_info) = code_gen.analyze_context.block_info.get(block_id) {
                    if block_info.all_children_contains_return {
                        merge_block.remove_from_function().map_err(|_| {
                            CodeGenError(format!(
                                "1Unable to remove empty merge block with block ID: {}",
                                block_id
                            ))
                        })?;
                    } else {
                        code_gen.builder.position_at_end(*merge_block);
                        code_gen
                            .builder
                            .build_unconditional_branch(wrapping_merge_block);
                    }
                }
            } else {
                merge_block.remove_from_function().map_err(|_| {
                    CodeGenError(format!(
                        "2Unable to remove empty merge block with block ID: {}",
                        block_id
                    ))
                })?;
            }
        }
    }
    Ok(())
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        analyze_context: &'ctx AnalyzeContext,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Self {
        Self {
            context,
            analyze_context,
            state: CodeGenState::new(),

            builder,
            module,

            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn compile_recursive(&mut self, token: &'ctx ParseToken) -> CustomResult<()> {
        match &token.kind {
            ParseTokenKind::Block(header, id, body) => {
                self.compile_block(header, *id, body)?;
            }
            ParseTokenKind::Statement(stmt) => {
                self.compile_stmt(&stmt)?;
            }
            ParseTokenKind::Expression(expr) => {
                self.compile_expr(expr)?;
            }
            ParseTokenKind::EndOfFile => (),
        }
        Ok(())
    }

    fn create_entry_block_alloca(&self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        if let Some(func) = self.state.cur_func {
            let entry = func
                .get_first_basic_block()
                .expect("Unable to unwrap first basic block in func.");

            // TODO: FIXME: Currently, a new alloca is added at the end of the
            //       entry block. Can this be a problem? Will this function ever
            //       be called when other instructions have been added?
            self.builder.position_at_end(entry);
            self.compile_alloca(var)
        } else {
            Err(CodeGenError(format!(
                "No active cur func when creating var: {}",
                &var.name
            )))
        }
    }

    fn compile_alloca(&self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        if let Some(var_type) = &var.ret_type {
            Ok(match var_type.t.to_codegen(&self.context)? {
                AnyTypeEnum::ArrayType(ty) => {
                    // TODO: Alloca array, need to figure out constant size first.
                    //self.builder.build_array_alloca(ty, &var.name)
                    return Err(CodeGenError("TODO: Alloca array.".into()));
                }
                AnyTypeEnum::FloatType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::IntType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::PointerType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::StructType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::VectorType(ty) => self.builder.build_alloca(ty, &var.name),
                AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError("Tried to alloca function.".into()))
                }
                AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError("Tried to alloca void type.".into()))
                }
            })
        } else {
            Err(CodeGenError(format!(
                "type None when allocaing var: {:?}",
                &var.name
            )))
        }
    }

    fn compile_block(
        &mut self,
        header: &'ctx BlockHeader,
        id: BlockId,
        body: &'ctx [ParseToken],
    ) -> CustomResult<()> {
        match header {
            BlockHeader::Default => {
                for token in body {
                    self.compile_recursive(token)?
                }
            }
            BlockHeader::Function(func) => {
                self.compile_func(func, id, body)?;
            }
            //BlockHeader::Struct(struct_) => self.compile_struct(struct_),
            //BlockHeader::Enum(enum_) => self.compile_enum(enum_),
            //BlockHeader::Interface(interface) => self.compile_interface(interface),
            BlockHeader::If => {
                self.compile_if(id, body)?;
            }
            BlockHeader::IfCase(_) => {
                return Err(CodeGenError("Unexpected IfCase in compile_block".into()));
            }
            //BlockHeader::Match(expr) => self.compile_match(expr),
            //BlockHeader::MatchCase(expr) => self.compile_match_case(expr),

            //BlockHeader::For(var, expr) => self.compile_for(var, expr),
            //BlockHeader::While(expr_opt) => self.compile_while(expr_opt),

            //BlockHeader::With(expr) => self.compile_with(expr),
            //BlockHeader::Defer(expr) => self.compile_defer(expr),

            //BlockHeader::Test(test_func) => self.compile_test_func(expr),
            _ => panic!(format!("TODO: compile_block type: {:?}", header)),
        }
        Ok(())
    }

    fn compile_func(
        &mut self,
        func: &'ctx Function,
        id: BlockId,
        body: &'ctx [ParseToken],
    ) -> CustomResult<()> {
        self.state.cur_block_id = id;

        let linkage = Linkage::External;
        let fn_val = self.compile_func_proto(func, Some(linkage))?;
        let entry = self.context.append_basic_block(fn_val, "entry");

        self.state.cur_block = Some(entry);
        self.state.cur_func = Some(fn_val);
        self.builder.position_at_end(entry);

        // TODO: How does this work with variadic parameters?
        // Get names for the parameters and alloc space in the functions stack.
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let param = if let Some(params) = &func.parameters {
                params
                    .get(i)
                    .ok_or_else(|| CodeGenError(format!("Bad param at index: {}", i)))?
            } else {
                return Err(CodeGenError(format!(
                    "Got None param when compiling func: {:?}",
                    &func.name
                )));
            };

            let ptr = self.create_entry_block_alloca(param)?;
            self.builder.build_store(ptr, arg);

            let key = (param.name.clone(), self.state.cur_block_id);
            self.variables.insert(key, ptr);
        }

        // Compile the tokens in the body of the function.
        for token in body {
            self.compile_recursive(token)?;
        }

        // Add a "invisible" return at the end of the last block if this is a
        // function with no return type.
        if func.ret_type.is_none() {
            if let Some(last_block) = fn_val.get_last_basic_block() {
                self.builder.position_at_end(last_block);
                self.builder.build_return(None);
            } else {
                return Err(CodeGenError(format!(
                    "No basic block in func: {}",
                    &func.name
                )));
            }
        }
        Ok(())
    }

    fn compile_func_proto(
        &self,
        func: &Function,
        linkage_opt: Option<Linkage>,
    ) -> CustomResult<FunctionValue<'ctx>> {
        let par_types = if let Some(params) = &func.parameters {
            let mut v = Vec::with_capacity(params.len());
            for param in params {
                if let Some(param_type_struct) = &param.ret_type {
                    let any_type = param_type_struct.t.to_codegen(self.context)?;
                    let basic_type = CodeGen::any_into_basic_type(any_type)?;
                    v.push(basic_type);
                } else {
                    return Err(CodeGenError(format!(
                        "Bad type for fn \"{}\" param \"{}\".",
                        &func.name, &param.name
                    )));
                }
            }
            v
        } else {
            Vec::default()
        };

        let fn_type = if let Some(ret_type) = &func.ret_type {
            let any_type = ret_type.t.to_codegen(self.context)?;
            match any_type {
                AnyTypeEnum::ArrayType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::FloatType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::FunctionType(ty) => ty,
                AnyTypeEnum::IntType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::PointerType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::StructType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::VectorType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                AnyTypeEnum::VoidType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
            }
        /*
        match basic_type {
            BasicTypeEnum::IntType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
            BasicTypeEnum::FloatType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
            BasicTypeEnum::PointerType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
            BasicTypeEnum::ArrayType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
            BasicTypeEnum::StructType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
            BasicTypeEnum::VectorType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
        }
        */
        } else {
            self.context
                .void_type()
                .fn_type(par_types.as_slice(), func.is_var_arg)
        };

        let fn_val = self.module.add_function(&func.name, fn_type, linkage_opt);

        // TODO: Set names?
        /*
        // Set names for the parameters.
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let param = if let Some(params) = &func.parameters {
                params
                    .get(i)
                    .ok_or_else(|| GenerationError(format!("Bad param at index: {}", i)))?
            } else {
                return Err(GenerationError(format!(
                    "Expected no params for func: {:?}",
                    &func.name
                )));
            };

            arg.as_basic_value_enum().set_name(&param.name);
        }
        */

        Ok(fn_val)
    }

    /// All the "ParseToken" in the body should be "IfCase"s.
    fn compile_if(&mut self, id: BlockId, body: &'ctx [ParseToken]) -> CustomResult<()> {
        self.state.cur_block_id = id;

        let cur_func = self
            .state
            .cur_func
            .ok_or_else(|| CodeGenError("cur_func is None for \"If\".".into()))?;

        let cur_block = self
            .state
            .cur_block
            .ok_or_else(|| CodeGenError("cur_block is None for \"If\".".into()))?;

        // Create and store the "body" blocks of this if-statement.
        // For every if-case that has a expression (if/elif) a extra block
        // will be created which will contain the branching logic between the
        // cases.
        let mut branch_info = BranchInfo::new();
        branch_info.if_branches.push(cur_block);
        for (i, if_case) in body.iter().enumerate() {
            if let ParseTokenKind::Block(BlockHeader::IfCase(expr_opt), _, _) = &if_case.kind {
                // Skip adding a branch block if this is the first case (since it
                // has the branch block `cur_block`). Also only add a branch block
                // if this `if_case` contains a expression that can be "branched on".
                if i > 0 && expr_opt.is_some() {
                    let br_block = self.context.append_basic_block(cur_func, "if.branch");
                    branch_info.if_branches.push(br_block);
                }

                let if_block = self.context.append_basic_block(cur_func, "if.case");
                branch_info.if_cases.push(if_block);
            } else {
                return Err(CodeGenError(format!(
                    "Token in \"If\" block wasn't a \"IfCase\": {:?}",
                    if_case.kind
                )));
            }
        }

        let merge_block = self.context.append_basic_block(cur_func, "if.merge");
        self.state.merge_blocks.insert(id, merge_block);

        // Iterate through all "if cases" in this if-statement and compile them.
        for (index, if_case) in body.iter().enumerate() {
            if let ParseTokenKind::Block(BlockHeader::IfCase(expr_opt), inner_id, inner_body) =
                &if_case.kind
            {
                self.state.cur_block = Some(cur_block);
                self.compile_if_case(
                    &expr_opt,
                    *inner_id,
                    index,
                    inner_body.as_slice(),
                    &branch_info,
                )?;
            } else {
                return Err(CodeGenError(format!(
                    "Token in \"If\" block wasn't a \"IfCase\": {:?}",
                    if_case.kind
                )));
            }
        }

        self.state.cur_block = Some(merge_block);
        self.builder.position_at_end(merge_block);
        Ok(())
    }

    fn compile_if_case(
        &mut self,
        expr_opt: &Option<Expression>,
        id: BlockId,
        index: usize,
        body: &'ctx [ParseToken],
        branch_info: &BranchInfo<'ctx>,
    ) -> CustomResult<()> {
        let cur_block = branch_info.get_if_case(index)?;
        let merge_block = self.get_merge_block(id)?;

        self.state.cur_block = Some(cur_block);

        // If this is a if case with a expression, the branch condition should
        // be evaluated and branched from the branch block.
        if let Some(expr) = expr_opt {
            let branch_block = branch_info.get_if_branch(index)?;

            // If there are no more branch blocks, set the next branch block to
            // the merge block if there are no more if_cases or set it to the
            // last if_case if there is still one left.
            let next_branch_block = if index + 1 >= branch_info.if_branches.len() {
                if index + 1 >= branch_info.if_cases.len() {
                    merge_block
                } else {
                    branch_info.get_if_case(index + 1)?
                }
            } else {
                branch_info.get_if_branch(index + 1)?
            };

            // TODO: Return error instead of panicing inside the
            //       "into_int_value()" function.
            let expr = self.compile_expr(expr)?.into_int_value();
            self.builder.position_at_end(branch_block);
            self.builder
                .build_conditional_branch(expr, cur_block, next_branch_block);
        }

        // Compile all tokens inside this if-case.
        for token in body {
            // Need to reset `cur_block` at every iteration because of recursion.
            self.state.cur_block = Some(cur_block);
            self.builder.position_at_end(cur_block);
            self.compile_recursive(token)?;
        }

        self.state.cur_block = Some(cur_block);
        self.builder.position_at_end(cur_block);

        // Add a branch to the merge block if the current basic block
        // doesn't have a terminator yet.
        if cur_block.get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_block);
        }
        Ok(())
    }

    /// Returns the BasicBlock representing the merge block for the if-statement
    /// with the block id `id` or the parent scope of the if-case with
    /// block id `id`.
    fn get_merge_block(&self, id: BlockId) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(merge_block) = self.state.merge_blocks.get(&id) {
            Ok(*merge_block)
        } else {
            // Get from the parent scope if possible.
            let parent_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| CodeGenError(format!("Unable to find parent block with id {}", id)))?
                .parent_id;

            if let Some(merge_block) = self.state.merge_blocks.get(&parent_id) {
                Ok(*merge_block)
            } else {
                Err(CodeGenError(format!(
                    "Unable to find merge block in blocks with id {} and parent {}.",
                    id, parent_id
                )))
            }
        }
    }

    // TODO: Clean up.
    /// Returns the BasicBlock representing a "outer" if block if one exists.
    fn get_parent_merge_block(&self, id: BlockId) -> CustomResult<Option<BasicBlock<'ctx>>> {
        if self.state.merge_blocks.get(&id).is_some() {
            let parent_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| {
                    CodeGenError(format!("1Unable to find parent block with id {}", id))
                })?
                .parent_id;

            Ok(self.get_merge_block(parent_id).ok())
        } else {
            // The given `id` was the block ID of a if case. First get the ID
            // if the wrapping "If" block. Then get the parent ID of that block
            // to get the sought after merge block.
            let if_id = self
                .analyze_context
                .block_info
                .get(&id)
                .ok_or_else(|| {
                    CodeGenError(format!("2Unable to find parent block with id {}", id))
                })?
                .parent_id;

            let parent_id = self
                .analyze_context
                .block_info
                .get(&if_id)
                .ok_or_else(|| {
                    CodeGenError(format!("3Unable to find parent block with id {}", id))
                })?
                .parent_id;

            Ok(self.get_merge_block(parent_id).ok())
        }
    }

    fn compile_stmt(&mut self, stmt: &Statement) -> CustomResult<()> {
        match stmt {
            Statement::Return(expr_opt) => self.compile_return(expr_opt),
            Statement::Yield(expr) => self.compile_yield(expr),
            Statement::Break => self.compile_break(),
            Statement::Continue => self.compile_continue(),
            Statement::Use(path) => self.compile_use(path),
            Statement::Package(path) => self.compile_package(path),
            Statement::With(expr) => panic!("TODO: compile \"with\"."),
            Statement::Defer(expr) => panic!("TODO: compile \"defer\"."),
            Statement::VariableDecl(var, expr_opt) => {
                self.compile_var_decl(var)?;
                if let Some(expr) = expr_opt {
                    let any_value = self.compile_expr(expr)?;
                    let basic_value = CodeGen::any_into_basic_value(any_value)?;
                    self.compile_var_store(var, basic_value)?;
                } else if var.is_const {
                    return Err(CodeGenError(format!(
                        "const var decl of \"{}\" has no value set",
                        &var.name
                    )));
                }
                Ok(())
            }
            Statement::ExternalDecl(func) => {
                let linkage = Linkage::External;
                self.compile_func_proto(func, Some(linkage))?;
                Ok(())
            }
            Statement::Modifier(modifier) => self.compile_modifier(modifier),
            Statement::Assignment(assign_op, var, expr) => {
                self.compile_assign(assign_op, var, expr)
            }
        }
    }

    fn compile_return(&mut self, expr_opt: &Option<Expression>) -> CustomResult<()> {
        if let Some(expr) = expr_opt {
            let any_value = self.compile_expr(expr)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            self.builder.build_return(Some(&basic_value));
        } else {
            self.builder.build_return(None);
        }
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Expression) -> CustomResult<()> {
        Err(CodeGenError("TODO: Implement yield statement.".into()))
    }

    fn compile_break(&mut self) -> CustomResult<()> {
        // TODO: Is it always OK to use `self.state.cur_block_id` here?
        let id = self.state.cur_block_id;
        let merge_block = self.get_merge_block(id)?;
        self.builder.build_unconditional_branch(merge_block);
        Ok(())
    }

    fn compile_continue(&mut self) -> CustomResult<()> {
        Err(CodeGenError("TODO: Implement continue statement.".into()))
    }

    fn compile_use(&mut self, path: &Path) -> CustomResult<()> {
        Err(CodeGenError("TODO: Implement use statement.".into()))
    }

    fn compile_package(&mut self, path: &Path) -> CustomResult<()> {
        Err(CodeGenError("TODO: Implement package statement.".into()))
    }

    fn compile_modifier(&mut self, modifier: &Modifier) -> CustomResult<()> {
        Err(CodeGenError("TODO: Implement modifer statement.".into()))
    }

    // TODO: Only "int"s atm.
    fn compile_assign(
        &mut self,
        assign_op: &AssignOperator,
        var: &Variable,
        expr: &Expression,
    ) -> CustomResult<()> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = var.ret_type {
            ret_type.t.to_codegen(&self.context)?
        } else {
            return Err(CodeGenError(format!(
                "Type of var \"{}\" not know when compiling assignment.",
                &var.name
            )));
        };

        let right_any_value = self.compile_expr(expr)?;
        let right = CodeGen::any_into_basic_value(right_any_value)?;

        let left = self.compile_var_load(var)?;

        // TODO: Need to check the size(8,16,32...) and also signness for the
        //       left and right to choose the correct instruction.

        let value = match assign_op {
            AssignOperator::Assignment => right,

            AssignOperator::AssignAddition => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_add(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.add.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_add(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.add.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignAddition: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignSubtraction => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_sub(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.sub.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_add(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.sub.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignSubtraction: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignMultiplication => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_mul(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.mul.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_mul(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.mul.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignMultiplication: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignDivision => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_div(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.div.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_div(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.div.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignDivision: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignModulus => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_rem(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.mod.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_rem(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.mod.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignModulus: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignPower => {
                return Err(CodeGenError("TODO: AssignPower.".into()));
            }
            AssignOperator::AssignBitAnd => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_and(left.into_int_value(), right.into_int_value(), "assign.and")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignBitAnd: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignBitOr => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_or(left.into_int_value(), right.into_int_value(), "assign.or")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignBitOr: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignBitXor => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_xor(left.into_int_value(), right.into_int_value(), "assign.xor")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignBitXor: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignShiftLeft => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_left_shift(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.lshift",
                    )
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignShiftLeft: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignShiftRight => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = true;
                    self.builder
                        .build_right_shift(
                            left.into_int_value(),
                            right.into_int_value(),
                            sign_extend,
                            "assign.rshift",
                        )
                        .into()
                }
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for AssignShiftRight: {:?}",
                        ret_type
                    )))
                }
            },
        };

        self.compile_var_store(var, value)
    }

    fn compile_expr(&mut self, expr: &Expression) -> CustomResult<AnyValueEnum<'ctx>> {
        match expr {
            Expression::Literal(lit, ty_opt) => self.compile_lit(lit, ty_opt),
            Expression::Variable(var) => Ok(self.compile_var_load(var)?.into()),
            Expression::FunctionCall(func_call) => self.compile_func_call(func_call),
            Expression::Operation(op) => self.compile_op(op),
            Expression::Type(ty) => {
                // TODO: Does something need to be done here? Does a proper value
                //       need to be returned? For now just return a dummy value.
                Ok(match ty.t.to_codegen(&self.context)? {
                    AnyTypeEnum::ArrayType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::FloatType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::IntType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::PointerType(ty) => ty.const_null().into(),
                    AnyTypeEnum::StructType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::VectorType(ty) => ty.const_zero().into(),
                    AnyTypeEnum::FunctionType(ty) => panic!("TODO: compile_exr function type?"),
                    AnyTypeEnum::VoidType(ty) => panic!("TODO: compile_exr void type?"),
                })
            }
        }
    }

    fn compile_lit(
        &mut self,
        lit: &Literal,
        ty_opt: &Option<TypeStruct>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        match lit {
            Literal::StringLiteral(str_lit) => {
                // Returns a pointer to the newly created string literal.
                // The string literal will be an array of u8(/i8(?)) with a
                // null terminator.
                // TODO: Probably best to let string literals be a pointer to
                //       an array so one can get the size. But for now it is
                //       casted to a pointer to u8 to be compatible with C code.
                // See: https://github.com/TheDan64/inkwell/issues/32
                let lit_ptr = unsafe {
                    self.builder
                        .build_global_string(str_lit, "str.lit")
                        .as_pointer_value()
                };
                let address_space = AddressSpace::Global;
                let i8_ptr_type = self.context.i8_type().ptr_type(address_space);
                Ok(lit_ptr.const_cast(i8_ptr_type).into())
            }

            Literal::CharLiteral(char_lit) => {
                if char_lit.chars().count() == 1 {
                    if let Some(ch) = char_lit.chars().next() {
                        Ok(AnyValueEnum::IntValue(
                            self.context.i32_type().const_int(ch as u64, false),
                        ))
                    } else {
                        Err(CustomError::CodeGenError(
                            "Unable to get char literal.".to_string(),
                        ))
                    }
                } else {
                    Err(CustomError::CodeGenError(
                        "Char literal isn't a single character.".to_string(),
                    ))
                }
            }

            Literal::Bool(true) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_all_ones(),
            )),
            Literal::Bool(false) => Ok(AnyValueEnum::IntValue(
                self.context.bool_type().const_zero(),
            )),

            Literal::Integer(int_lit, radix) => Ok(AnyValueEnum::IntValue(
                self.compile_lit_int(int_lit, ty_opt, *radix)?,
            )),

            Literal::Float(float_lit) => Ok(AnyValueEnum::FloatValue(
                self.compile_lit_float(float_lit, ty_opt)?,
            )),
        }
    }

    // TODO: Better conversion of the integer literal.
    // TODO: i32 as default.
    fn compile_lit_int(
        &mut self,
        lit: &str,
        ty_opt: &Option<TypeStruct>,
        radix: u32,
    ) -> CustomResult<IntValue<'ctx>> {
        // TODO: Where should the integer literal conversion be made?

        Ok(match ty_opt {
            Some(type_struct) => match type_struct.t {
                Type::I8 => {
                    let val = i8::from_str_radix(lit, radix)? as u64;
                    self.context.i8_type().const_int(val, true)
                }
                Type::U8 => {
                    let val = u8::from_str_radix(lit, radix)? as u64;
                    self.context.i8_type().const_int(val, false)
                }
                Type::I16 => {
                    let val = i16::from_str_radix(lit, radix)? as u64;
                    self.context.i16_type().const_int(val, true)
                }
                Type::U16 => {
                    let val = u16::from_str_radix(lit, radix)? as u64;
                    self.context.i16_type().const_int(val, false)
                }
                Type::I32 => {
                    let val = i32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, true)
                }
                Type::U32 => {
                    let val = u32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, false)
                }
                Type::I64 => {
                    let val = i64::from_str_radix(lit, radix)? as u64;
                    self.context.i64_type().const_int(val, true)
                }
                Type::U64 => {
                    let val = u64::from_str_radix(lit, radix)? as u64;
                    self.context.i64_type().const_int(val, false)
                }
                Type::I128 => {
                    let val = i128::from_str_radix(lit, radix)? as u64;
                    self.context.i128_type().const_int(val, true)
                }
                Type::U128 => {
                    let val = u128::from_str_radix(lit, radix)? as u64;
                    self.context.i128_type().const_int(val, false)
                }
                // TODO: What should the default inte size be?
                Type::Int => {
                    let val = i32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, true)
                }
                Type::Uint => {
                    let val = u32::from_str_radix(lit, radix)? as u64;
                    self.context.i32_type().const_int(val, false)
                }
                _ => unreachable!("Invalid integer type: {:?}", type_struct.t),
            },
            // TODO: What should the default int size be? Signed 32 atm.
            None => {
                let val = i32::from_str_radix(lit, radix)? as u64;
                self.context.i32_type().const_int(val, true)
            }
        })
    }

    // TODO: Better conversion of the float literal.
    // TODO: f32 as default.
    fn compile_lit_float(
        &mut self,
        lit: &str,
        ty_opt: &Option<TypeStruct>,
    ) -> CustomResult<FloatValue<'ctx>> {
        Ok(match ty_opt {
            Some(type_struct) => match type_struct.t {
                Type::F32 => self.context.f32_type().const_float(lit.parse()?),
                Type::F64 => self.context.f64_type().const_float(lit.parse()?),
                // TODO: What should the default float size be?
                Type::Float => self.context.f32_type().const_float(lit.parse()?),
                _ => unreachable!("Invalid float type: {:?}", type_struct.t),
            },
            // TODO: What should the default float size be? F32 atm.
            None => self.context.f32_type().const_float(lit.parse()?),
        })
    }

    // TODO: How should a declaration of a "constant" be enforced?
    fn compile_var_decl(&mut self, var: &Variable) -> CustomResult<()> {
        let id = self.state.cur_block_id;
        let key = (var.name.clone(), id);

        if let Some(var_decl) = self.analyze_context.variables.get(&key) {
            debug!("Compiling var decl. Key: {:?}", &key);

            let ptr = self.compile_alloca(var_decl)?;
            self.variables.insert(key, ptr);
            Ok(())
        } else {
            Err(CodeGenError(format!(
                "No decl for var when compiling var decl: {}",
                &var.name
            )))
        }
    }

    fn compile_var_store(
        &mut self,
        var: &Variable,
        basic_value: BasicValueEnum,
    ) -> CustomResult<()> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Compile var_store, key: {:?}", &key);

        // Then get the pointer to the declared variable from the map
        // created during code generation.
        if let Some(ptr) = self.variables.get(&key) {
            self.builder.build_store(*ptr, basic_value);
            Ok(())
        } else {
            Err(CodeGenError(format!(
                "No decl for var `{}` when building store.",
                &var.name
            )))
        }
    }

    fn compile_var_load(&mut self, var: &Variable) -> CustomResult<BasicValueEnum<'ctx>> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);
        debug!("Compiling var load. Key: {:?}", &key);

        // Then get the pointer to the declared variable from the map
        // created during code generation.
        if let Some(ptr) = self.variables.get(&key) {
            Ok(self.builder.build_load(*ptr, "load"))
        } else {
            Err(CodeGenError(format!(
                "No decl for var `{}` when building load.",
                &var.name
            )))
        }
    }

    // TODO: Array access.

    // TODO: Temporarily treats functions return void as return i32 "0".
    //       Should make a custom value ex rusts "()" instead.
    /// Generates a function call. Returns the return value of the compiled
    /// function.
    fn compile_func_call(&mut self, func_call: &FunctionCall) -> CustomResult<AnyValueEnum<'ctx>> {
        if let Some(func_ptr) = self.module.get_function(&func_call.name) {
            // Checks to see if the arguments are fewer that parameters. The
            // arguments are allowed to be greater than parameters since variadic
            // functions are supported.
            if func_call.arguments.len() < func_ptr.count_params() as usize {
                return Err(CodeGenError(format!(
                    "Wrong amount of args given when calling func: {}. Expected: {}, got: {}",
                    &func_call.name,
                    func_ptr.count_params(),
                    func_call.arguments.len()
                )));
            }

            let mut args = Vec::with_capacity(func_call.arguments.len());
            for arg in &func_call.arguments {
                let any_value = self.compile_expr(&arg.value)?;
                let basic_value = CodeGen::any_into_basic_value(any_value)?;
                args.push(basic_value);
            }

            for (i, param) in func_ptr.get_param_iter().enumerate() {
                if let Some(arg) = args.get_mut(i) {
                    // Checks to see if the types of the parameter and the
                    // argument are the same. If they are different, see if the
                    // type of the argument can be casted to the same type.
                    self.infer_arg_type(i, &func_call.name, &param, arg)?;
                } else {
                    unreachable!("None when comparing arg and par in func call compile.");
                }
            }

            let call = self.builder.build_call(func_ptr, args.as_slice(), "call");

            // Left == BasicValueEnum, Right == InstructionValue.
            // Will be right if the function returns "void", left otherwise.
            Ok(if let Some(ret_val) = call.try_as_basic_value().left() {
                ret_val.into()
            } else {
                self.context.i32_type().const_zero().into()
            })
        } else {
            Err(CustomError::CodeGenError(format!(
                "Unable to find function with name {} to call.",
                &func_call.name
            )))
        }
    }

    fn infer_arg_type(
        &mut self,
        i: usize,
        func_name: &str,
        param: &BasicValueEnum,
        arg: &mut BasicValueEnum,
    ) -> CustomResult<()> {
        return Ok(());
        let arg_type = arg.get_type();
        let param_type = param.get_type();
        if arg_type != param_type {
            // TODO: Should be able to convert a {[u8: N]} to a {u8}. This is
            //       useful when working with for example string literals.
            //       Is there a way to see what type a PointerValue is poiting
            //       at through the inkwell API?
            // TODO: Add logic/edge cases where the type of the argument can
            //       be converted to the type of the parameter with no issues.
            Err(CodeGenError(format!(
                "Arg type at index {} wrong type when calling func: {}. Expected: {:?}, got: {:?}",
                i,
                func_name,
                param.get_type(),
                arg.get_type()
            )))
        } else {
            Ok(())
        }
    }

    fn compile_op(&mut self, op: &Operation) -> CustomResult<AnyValueEnum<'ctx>> {
        match op {
            Operation::BinaryOperation(bin_op) => self.compile_bin_op(bin_op),
            Operation::UnaryOperation(un_op) => self.compile_un_op(un_op),
        }
    }

    // TODO: Currently only ints, make for floats and other types.
    fn compile_bin_op(&mut self, bin_op: &BinaryOperation) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = bin_op.ret_type {
            ret_type.t.to_codegen(&self.context)?
        } else {
            return Err(CodeGenError(format!(
                "Type of bin_op \"{:?}\" not know when compiling assignment.",
                &bin_op
            )));
        };

        let left_any_value = self.compile_expr(&bin_op.left)?;
        let left = CodeGen::any_into_basic_value(left_any_value)?;

        let right_any_value = self.compile_expr(&bin_op.right)?;
        let right = CodeGen::any_into_basic_value(right_any_value)?;

        Ok(match bin_op.operator {
            token::BinaryOperator::In => panic!("TODO: In"),
            token::BinaryOperator::Is => panic!("TODO: Is"),
            token::BinaryOperator::As => {
                // TODO: Will probably need to check both sides before doing a
                //       cast. For example now, if the ret_type is float,
                //       the left will be casted into a float. But it assumes
                //       that the left side is some sort of float already.
                match ret_type {
                    AnyTypeEnum::ArrayType(ty) => {
                        self.builder.build_bitcast(left, ty, "cast.array").into()
                    }
                    AnyTypeEnum::FloatType(ty) => self
                        .builder
                        .build_float_cast(left.into_float_value(), ty, "cast.float")
                        .into(),
                    AnyTypeEnum::IntType(ty) => self
                        .builder
                        .build_int_cast(left.into_int_value(), ty, "cast.int")
                        .into(),
                    AnyTypeEnum::PointerType(ty) => self
                        .builder
                        .build_pointer_cast(left.into_pointer_value(), ty, "cast.ptr")
                        .into(),
                    AnyTypeEnum::StructType(ty) => {
                        self.builder.build_bitcast(left, ty, "cast.struct").into()
                    }
                    AnyTypeEnum::VectorType(ty) => {
                        self.builder.build_bitcast(left, ty, "cast.vector").into()
                    }
                    AnyTypeEnum::FunctionType(_) => panic!("TODO: compile_bin_op function type."),
                    AnyTypeEnum::VoidType(_) => panic!("TODO: compile_bin_op void type."),
                }
            }
            token::BinaryOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            token::BinaryOperator::Range => panic!("TODO: Range"),
            token::BinaryOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            token::BinaryOperator::Dot => panic!("TODO: Dot"),

            token::BinaryOperator::Equals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OEQ;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OEQ.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::EQ;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "EQ.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::Equals: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::NotEquals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::ONE;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "ONE.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::NE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "NE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::NotEquals: {:?}",
                        ret_type
                    )))
                }
            },

            // TODO: Signed/unsigned compares.
            token::BinaryOperator::LessThan => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OLT;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OLT.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SLE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SLE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::LessThan: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::GreaterThan => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OGT;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OGT.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SGT;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SGT.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::GreaterThan: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::LessThanOrEquals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OLE;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OLE.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SLE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SLE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::LessThanOrEquals: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::GreaterThanOrEquals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OGE;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OGE.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SGE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SGE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::GreaterThanOrEquals: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::Addition => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_add(
                        left.into_float_value(),
                        right.into_float_value(),
                        "add.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_add(left.into_int_value(), right.into_int_value(), "add.int")
                    .into(),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::Addition: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::Subtraction => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_sub(
                        left.into_float_value(),
                        right.into_float_value(),
                        "sub.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_sub(left.into_int_value(), right.into_int_value(), "sub.int")
                    .into(),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::Subtraction: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::Multiplication => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_mul(
                        left.into_float_value(),
                        right.into_float_value(),
                        "mul.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_mul(left.into_int_value(), right.into_int_value(), "mul.int")
                    .into(),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::Multiplication: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::Division => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_div(
                        left.into_float_value(),
                        right.into_float_value(),
                        "div.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_div(left.into_int_value(), right.into_int_value(), "div.int")
                    .into(),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::Division: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::Modulus => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_rem(
                        left.into_float_value(),
                        right.into_float_value(),
                        "mod.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_rem(left.into_int_value(), right.into_int_value(), "mod.int")
                    .into(),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::Modulus: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::Power => panic!("TODO: Power"),

            token::BinaryOperator::BitAnd => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_and(left.into_int_value(), right.into_int_value(), "bit.and")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::BitAnd: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::BitOr => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_or(left.into_int_value(), right.into_int_value(), "bit.or")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::BitOr: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::BitXor => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_xor(left.into_int_value(), right.into_int_value(), "bit.xor")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::BitXor: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::ShiftLeft => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_left_shift(left.into_int_value(), right.into_int_value(), "lshift")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::ShiftLeft: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::ShiftRight => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = true;
                    self.builder
                        .build_right_shift(
                            left.into_int_value(),
                            right.into_int_value(),
                            sign_extend,
                            "rshift",
                        )
                        .into()
                }
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for BinaryOperator::ShiftRight: {:?}",
                        ret_type
                    )))
                }
            },

            token::BinaryOperator::BoolAnd => panic!("TODO: BoolAnd"),
            token::BinaryOperator::BoolOr => panic!("TODO: BooldOr"),

            token::BinaryOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
        })
    }

    // TODO: Only ints atm.
    fn compile_un_op(&mut self, un_op: &UnaryOperation) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = un_op.ret_type {
            ret_type.t.to_codegen(&self.context)?
        } else {
            return Err(CodeGenError(format!(
                "Type of un_op \"{:?}\" not know when compiling assignment.",
                &un_op
            )));
        };

        let any_value = self.compile_expr(&un_op.value)?;
        let value = CodeGen::any_into_basic_value(any_value)?;

        Ok(match un_op.operator {
            token::UnaryOperator::Increment => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = false;
                    let one = ret_type.into_int_type().const_int(1, sign_extend);
                    self.builder
                        .build_int_add(value.into_int_value(), one, "inc")
                        .into()
                }
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for UnaryOperator::Increment: {:?}",
                        ret_type
                    )))
                }
            },

            token::UnaryOperator::Decrement => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = false;
                    let one = ret_type.into_int_type().const_int(1, sign_extend);
                    self.builder
                        .build_int_sub(value.into_int_value(), one, "dec")
                        .into()
                }
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for UnaryOperator::Decrement: {:?}",
                        ret_type
                    )))
                }
            },

            token::UnaryOperator::Deref => {
                panic!("TODO: Deref");
                //self.builder.build_load(ptr, name)
            }
            token::UnaryOperator::Address => {
                panic!("TODO: Address");
            }
            token::UnaryOperator::Positive => {
                // Do nothing.
                value.into()
            }

            token::UnaryOperator::Negative => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_neg(value.into_float_value(), "neg.float")
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_neg(value.into_int_value(), "neg.int")
                    .into(),
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for UnaryOperator::Negative: {:?}",
                        ret_type
                    )))
                }
            },

            token::UnaryOperator::BitComplement => panic!("TODO: Bit complement"),
            token::UnaryOperator::BoolNot => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    self.builder.build_not(value.into_int_value(), "not").into()
                }
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(CodeGenError(format!(
                        "Invalid type for UnaryOperator::BoolNot: {:?}",
                        ret_type
                    )))
                }
            },
        })
    }

    fn any_into_basic_value(any_value: AnyValueEnum) -> CustomResult<BasicValueEnum> {
        BasicValueEnum::try_from(any_value).map_err(|_| {
            CodeGenError(format!(
                "Unable to convert AnyValueEnum: {:#?} into BasicValueEnum.",
                any_value
            ))
        })
    }

    fn any_into_basic_type(any_type: AnyTypeEnum) -> CustomResult<BasicTypeEnum> {
        BasicTypeEnum::try_from(any_type).map_err(|_| {
            CodeGenError(format!(
                "Unable to convert AnyTypeEnum: {:#?} into BasicTypeEnum.",
                any_type
            ))
        })
    }
}
