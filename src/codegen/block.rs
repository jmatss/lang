use super::generator::CodeGen;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::{
    parse::token::{
        BlockHeader, BlockId, Expression, Function, ParseToken, ParseTokenKind, Struct, Variable,
    },
    CustomResult,
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

    pub fn get_if_case(&self, index: usize) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_cases.get(index) {
            Ok(*basic_block)
        } else {
            Err(LangError::new(
                format!("Unable to get if_case with index: {}", index),
                CodeGenError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    pub fn get_if_branch(&self, index: usize) -> CustomResult<BasicBlock<'ctx>> {
        if let Some(basic_block) = self.if_branches.get(index) {
            Ok(*basic_block)
        } else {
            Err(LangError::new(
                format!("Unable to get if_branch with index: {}", index),
                CodeGenError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_block(
        &mut self,
        header: &'ctx BlockHeader,
        id: BlockId,
        body: &'ctx mut [ParseToken],
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
                return Err(self.err("Unexpected IfCase in compile_block".into()));
            }
            BlockHeader::Struct(struct_) => {
                self.compile_struct(struct_)?;
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

    fn create_entry_block_alloca(&self, var: &Variable) -> CustomResult<PointerValue<'ctx>> {
        if let Some(func) = self.cur_func {
            let entry = func
                .get_first_basic_block()
                .expect("Unable to unwrap first basic block in func.");

            // TODO: FIXME: Currently, a new alloca is added at the end of the
            //       entry block. Can this be a problem? Will this function ever
            //       be called when other instructions have been added?
            self.builder.position_at_end(entry);
            self.compile_alloca(var)
        } else {
            Err(self.err(format!(
                "No active cur func when creating var: {}",
                &var.name
            )))
        }
    }

    fn compile_func(
        &mut self,
        func: &'ctx Function,
        id: BlockId,
        body: &'ctx mut [ParseToken],
    ) -> CustomResult<()> {
        self.cur_block_id = id;

        let linkage = Linkage::External;
        let fn_val = self.compile_func_proto(func, Some(linkage))?;
        let entry = self.context.append_basic_block(fn_val, "entry");

        self.cur_basic_block = Some(entry);
        self.cur_func = Some(fn_val);
        self.builder.position_at_end(entry);

        // TODO: How does this work with variadic parameters?
        // Get names for the parameters and alloc space in the functions stack.
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            let param = if let Some(params) = &func.parameters {
                params
                    .get(i)
                    .ok_or_else(|| self.err(format!("Bad param at index: {}", i)))?
            } else {
                return Err(self.err(format!(
                    "Got None param when compiling func: {:?}",
                    &func.name
                )));
            };

            let ptr = self.create_entry_block_alloca(param)?;
            self.builder.build_store(ptr, arg);

            let key = (param.name.clone(), self.cur_block_id);
            self.variables.insert(key, ptr);
        }

        // Compile the tokens in the body of the function.
        for token in body {
            self.compile_recursive(token)?;
        }

        // Add a "invisible" return at the end of the last block if this is a
        // function with no return type. Also check to see if this block
        // conntains a return stmt even though it should return anything.
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

    pub(super) fn compile_func_proto(
        &self,
        func: &Function,
        linkage_opt: Option<Linkage>,
    ) -> CustomResult<FunctionValue<'ctx>> {
        let par_types = if let Some(params) = &func.parameters {
            let mut v = Vec::with_capacity(params.len());
            for param in params {
                if let Some(param_type_struct) = &param.ret_type {
                    let any_type = self.compile_type(&param_type_struct)?;
                    let basic_type = CodeGen::any_into_basic_type(any_type)?;
                    v.push(basic_type);
                } else {
                    return Err(self.err(format!(
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
            let any_type = self.compile_type(&ret_type)?;
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
    fn compile_if(&mut self, id: BlockId, body: &'ctx mut [ParseToken]) -> CustomResult<()> {
        self.cur_block_id = id;

        let cur_func = self
            .cur_func
            .ok_or_else(|| self.err("cur_func is None for \"If\".".into()))?;

        let cur_block = self
            .cur_basic_block
            .ok_or_else(|| self.err("cur_block is None for \"If\".".into()))?;

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
                return Err(self.err(format!(
                    "Token in \"If\" block wasn't a \"IfCase\": {:?}",
                    if_case.kind
                )));
            }
        }

        let merge_block = self.context.append_basic_block(cur_func, "if.merge");
        self.merge_blocks.insert(id, merge_block);

        // Iterate through all "if cases" in this if-statement and compile them.
        for (index, if_case) in body.iter_mut().enumerate() {
            if let ParseTokenKind::Block(
                BlockHeader::IfCase(ref mut expr_opt),
                inner_id,
                ref mut inner_body,
            ) = &mut if_case.kind
            {
                self.cur_basic_block = Some(cur_block);
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

        self.cur_basic_block = Some(merge_block);
        self.builder.position_at_end(merge_block);
        Ok(())
    }

    fn compile_if_case(
        &mut self,
        expr_opt: &mut Option<Expression>,
        id: BlockId,
        index: usize,
        body: &'ctx mut [ParseToken],
        branch_info: &BranchInfo<'ctx>,
    ) -> CustomResult<()> {
        let cur_block = branch_info.get_if_case(index)?;
        let merge_block = self.get_merge_block(id)?;

        self.cur_basic_block = Some(cur_block);

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
            self.cur_basic_block = Some(cur_block);
            self.builder.position_at_end(cur_block);
            self.compile_recursive(token)?;
        }

        self.cur_basic_block = Some(cur_block);
        self.builder.position_at_end(cur_block);

        // Add a branch to the merge block if the current basic block
        // doesn't have a terminator yet.
        if cur_block.get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_block);
        }
        Ok(())
    }

    pub(super) fn compile_struct(&mut self, struct_: &Struct) -> CustomResult<()> {
        // Go trough all members of the struct and create a vector containing
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
}
