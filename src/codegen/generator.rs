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
use inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue};
use inkwell::{basic_block::BasicBlock, types::BasicTypeEnum, AddressSpace, IntPredicate};
use std::collections::HashMap;
use token::{AssignOperator, BlockHeader, BlockId, Modifier, ParseTokenKind, Path, Statement};

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
                // Set current ID and save the old state since the function might
                // make recursive calls that would change the state.
                self.state.cur_block_id = *id;
                let old_state = self.state.clone();
                self.compile_block(header, body)?;
                self.state = old_state;
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
            match var_type.t.to_codegen(&self.context)? {
                BasicTypeEnum::ArrayType(ty) => Ok(self.builder.build_alloca(ty, &var.name)),
                BasicTypeEnum::IntType(ty) => Ok(self.builder.build_alloca(ty, &var.name)),
                BasicTypeEnum::FloatType(ty) => Ok(self.builder.build_alloca(ty, &var.name)),
                BasicTypeEnum::PointerType(ty) => Ok(self.builder.build_alloca(ty, &var.name)),
                BasicTypeEnum::StructType(ty) => Ok(self.builder.build_alloca(ty, &var.name)),
                BasicTypeEnum::VectorType(ty) => Ok(self.builder.build_alloca(ty, &var.name)),
            }
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
        body: &'ctx [ParseToken],
    ) -> CustomResult<()> {
        match header {
            BlockHeader::Default => {
                for token in body {
                    self.compile_recursive(token)?
                }
                Ok(())
            }
            BlockHeader::Function(func) => self.compile_func(func, body),
            //BlockHeader::Struct(struct_) => self.compile_struct(struct_),
            //BlockHeader::Enum(enum_) => self.compile_enum(enum_),
            //BlockHeader::Interface(interface) => self.compile_interface(interface),
            BlockHeader::If => self.compile_if(body),
            BlockHeader::IfCase(_) => {
                Err(CodeGenError("Unexpected IfCase in compile_block".into()))
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
    }

    fn compile_func(&mut self, func: &'ctx Function, body: &'ctx [ParseToken]) -> CustomResult<()> {
        let old_state = self.state.clone();

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

        self.state = old_state;
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
                if let Some(param_type) = &param.ret_type {
                    v.push(param_type.t.to_codegen(self.context)?);
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
            let basic_type = ret_type.t.to_codegen(self.context)?;
            match basic_type {
                BasicTypeEnum::IntType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                BasicTypeEnum::FloatType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                BasicTypeEnum::PointerType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                BasicTypeEnum::ArrayType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                BasicTypeEnum::StructType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
                BasicTypeEnum::VectorType(ty) => ty.fn_type(par_types.as_slice(), func.is_var_arg),
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
    fn compile_if(&mut self, body: &'ctx [ParseToken]) -> CustomResult<()> {
        let old_state = self.state.clone();

        let cur_func = self
            .state
            .cur_func
            .ok_or_else(|| CodeGenError("cur_func is None for \"If\".".into()))?;

        let cur_block = self
            .state
            .cur_block
            .ok_or_else(|| CodeGenError("cur_block is None for \"If\".".into()))?;

        // Create and store the "body" blocks of this if-statement into the "gen_state".
        // For every if-case that has a expression (if/elif) a extra block
        // will be created which will contain the branching logic between the
        // cases.
        let mut if_cases = Vec::with_capacity(body.len());
        let mut if_branches = Vec::new();
        if_branches.push(cur_block);
        for (i, if_case) in body.iter().enumerate() {
            // Skip adding a branch block if this is the first case (since it
            // has the branch block `cur_block`).
            if i > 0 {
                if let ParseTokenKind::Block(BlockHeader::IfCase(Some(_)), _, _) = if_case.kind {
                    let br_block = self.context.append_basic_block(cur_func, "if.branch");
                    if_branches.push(br_block);
                }
            }

            let if_block = self.context.append_basic_block(cur_func, "if.case");
            if_cases.push(if_block);
        }
        self.state.cur_if_cases = Some(if_cases);
        self.state.cur_if_branches = Some(if_branches);

        let merge_block = self.context.append_basic_block(cur_func, "if.merge");
        self.state.cur_merge_block = Some(merge_block);

        for (i, if_case) in body.iter().enumerate() {
            if let ParseTokenKind::Block(BlockHeader::IfCase(expr_opt), _, body) = &if_case.kind {
                self.compile_if_case(&expr_opt, body.as_slice(), i)?;
            } else {
                return Err(CodeGenError(format!(
                    "Token in \"If\" block wasn't a \"IfCase\": {:?}",
                    if_case.kind
                )));
            }
        }

        self.state = old_state;
        self.builder.position_at_end(merge_block);
        Ok(())
    }

    // TODO: Clean up, remove duplicate code etc.
    fn compile_if_case(
        &mut self,
        expr_opt: &Option<Expression>,
        body: &'ctx [ParseToken],
        block_nr: usize,
    ) -> CustomResult<()> {
        let old_state = self.state.clone();

        let cur_block = self.get_if_case(block_nr)?;
        let merge_block = self
            .state
            .cur_merge_block
            .ok_or_else(|| CodeGenError("merge_block is None".into()))?;

        // If this is a if case with a expression, the branch condition should
        // be evaluated and branched from the "parent" branch block.
        if let Some(expr) = expr_opt {
            let branch_block = self.get_if_branch(block_nr)?;
            self.builder.position_at_end(branch_block);

            // If there are no more branch blocks, set the next branch block to
            // the merge block if there are no more if_cases or set it to the
            // last if_case if there is still one left.
            let next_branch_block = if block_nr + 1 >= self.get_if_branches_len()? {
                if block_nr + 1 >= self.get_if_cases_len()? {
                    merge_block
                } else {
                    self.get_if_case(block_nr + 1)?
                }
            } else {
                self.get_if_branch(block_nr + 1)?
            };

            // TODO: Return error instead of panicing inside the
            //       "into_int_value()" function.
            self.builder.build_conditional_branch(
                self.compile_expr(expr)?.into_int_value(),
                cur_block,
                next_branch_block,
            );
        }

        self.builder.position_at_end(cur_block);

        for token in body {
            self.state.cur_block = Some(cur_block);
            self.compile_recursive(token)?;
        }

        // Reset to point at the end of the current block since it might
        // have been changed in the for loop above.
        self.builder.position_at_end(cur_block);
        self.builder.build_unconditional_branch(merge_block);

        self.state = old_state;
        Ok(())
    }

    fn get_if_case(&self, block_nr: usize) -> CustomResult<BasicBlock<'ctx>> {
        self.state
            .cur_if_cases
            .as_ref()
            .and_then(|vec| vec.get(block_nr))
            .ok_or_else(|| CodeGenError("cur_if_cases was None".into()))
            .map(|block| *block)
    }

    fn get_if_branch(&self, block_nr: usize) -> CustomResult<BasicBlock<'ctx>> {
        self.state
            .cur_if_branches
            .as_ref()
            .and_then(|vec| vec.get(block_nr))
            .ok_or_else(|| CodeGenError("cur_if_branches was None".into()))
            .map(|block| *block)
    }

    fn get_if_cases_len(&self) -> CustomResult<usize> {
        self.state
            .cur_if_cases
            .as_ref()
            .map(|vec| vec.len())
            .ok_or_else(|| CodeGenError("cur_if_cases was None during len".into()))
    }

    fn get_if_branches_len(&self) -> CustomResult<usize> {
        self.state
            .cur_if_branches
            .as_ref()
            .map(|vec| vec.len())
            .ok_or_else(|| CodeGenError("cur_if_branches was None during len".into()))
    }

    fn compile_stmt(&mut self, stmt: &Statement) -> CustomResult<()> {
        match stmt {
            Statement::Return(expr_opt) => self.compile_return(expr_opt),
            Statement::Yield(expr) => self.compile_yield(expr),
            Statement::Break => self.compile_break(),
            Statement::Continue => self.compile_continue(),
            Statement::Use(path) => self.compile_use(path),
            Statement::Package(path) => self.compile_package(path),
            Statement::VariableDecl(var, expr_opt) => {
                self.compile_var_decl(var)?;
                if var.is_const {
                    if let Some(expr) = expr_opt {
                        self.compile_var_store(var, expr)?;
                    } else {
                        return Err(CodeGenError(format!(
                            "const var decl of \"{}\" has no value set",
                            &var.name
                        )));
                    }
                }
                if let Some(expr) = expr_opt {
                    self.compile_var_store(var, expr)?;
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
            let expr_comp = self.compile_expr(expr)?;
            self.builder.build_return(Some(&expr_comp));
        } else {
            self.builder.build_return(None);
        }
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Expression) -> CustomResult<()> {
        Err(CodeGenError("TODO: Implement yield statement.".into()))
    }

    fn compile_break(&mut self) -> CustomResult<()> {
        if let Some(merge_block) = self.state.cur_merge_block {
            self.builder.build_unconditional_branch(merge_block);
            Ok(())
        } else {
            Err(CodeGenError(
                "merge_block None when compiling break-stmt.".into(),
            ))
        }
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
        match assign_op {
            AssignOperator::Assignment => self.compile_var_store(var, expr),
            AssignOperator::AssignAddition => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_int_add(left, right, "assign.add");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignSubtraction => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_int_sub(left, right, "assign.sub");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignMultiplication => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_int_mul(left, right, "assign.mul");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignDivision => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_int_signed_div(left, right, "assign.div");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignModulus => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_int_signed_rem(left, right, "assign.mod");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignPower => panic!("TODO: Assign power."),

            AssignOperator::AssignBitAnd => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_and(left, right, "assign.and");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignBitOr => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_or(left, right, "assign.or");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignBitXor => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_xor(left, right, "assign.xor");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignShiftLeft => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let value = self.builder.build_left_shift(left, right, "assign.lshift");
                self.compile_var_store2(var, value.into())
            }
            AssignOperator::AssignShiftRight => {
                let left = self.compile_var_load(var)?.into_int_value();
                let right = self.compile_expr(expr)?.into_int_value();
                let sign_extend = true;
                let value =
                    self.builder
                        .build_right_shift(left, right, sign_extend, "assign.rshift");
                self.compile_var_store2(var, value.into())
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expression) -> CustomResult<BasicValueEnum<'ctx>> {
        match expr {
            Expression::Literal(lit, ty_opt) => self.compile_lit(lit, ty_opt),
            Expression::Variable(var) => self.compile_var_load(var),
            Expression::FunctionCall(func_call) => self.compile_func_call(func_call),
            Expression::Operation(op) => self.compile_op(op),
        }
    }

    fn compile_lit(
        &mut self,
        lit: &Literal,
        ty_opt: &Option<TypeStruct>,
    ) -> CustomResult<BasicValueEnum<'ctx>> {
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
                        Ok(BasicValueEnum::IntValue(
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

            Literal::Bool(true) => Ok(BasicValueEnum::IntValue(
                self.context.bool_type().const_all_ones(),
            )),
            Literal::Bool(false) => Ok(BasicValueEnum::IntValue(
                self.context.bool_type().const_zero(),
            )),

            Literal::Integer(int_lit, radix) => Ok(BasicValueEnum::IntValue(
                self.compile_lit_int(int_lit, ty_opt, *radix)?,
            )),

            Literal::Float(float_lit) => Ok(BasicValueEnum::FloatValue(
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

    fn compile_var_store(&mut self, var: &Variable, expr: &Expression) -> CustomResult<()> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        if let Some(decl_block_id) = self.analyze_context.get_var_decl_scope(&var.name, block_id) {
            let key = (var.name.clone(), decl_block_id);
            debug!("Compile var_store, key: {:?}", &key);

            // Then get the pointer to the declared variable from the map
            // created during code generation.
            if let Some(ptr) = self.variables.get(&key) {
                self.builder.build_store(*ptr, self.compile_expr(expr)?);
                Ok(())
            } else {
                Err(CodeGenError(format!(
                    "No decl for var `{}` when building store.",
                    &var.name
                )))
            }
        } else {
            Err(CodeGenError(format!(
                "Unable to find variable with name {} in block {} during store.",
                &var.name, self.state.cur_block_id
            )))
        }
    }

    fn compile_var_store2(
        &mut self,
        var: &Variable,
        basic_value: BasicValueEnum,
    ) -> CustomResult<()> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        if let Some(decl_block_id) = self.analyze_context.get_var_decl_scope(&var.name, block_id) {
            let key = (var.name.clone(), decl_block_id);
            debug!("Compile var_store2, key: {:?}", &key);

            // Then get the pointer to the declared variable from the map
            // created during code generation.
            if let Some(ptr) = self.variables.get(&key) {
                self.builder.build_store(*ptr, basic_value);
                Ok(())
            } else {
                Err(CodeGenError(format!(
                    "No decl for var `{}` when building store2.",
                    &var.name
                )))
            }
        } else {
            Err(CodeGenError(format!(
                "Unable to find variable with name {} in block {} during store2.",
                &var.name, self.state.cur_block_id
            )))
        }
    }

    fn compile_var_load(&mut self, var: &Variable) -> CustomResult<BasicValueEnum<'ctx>> {
        let block_id = self.state.cur_block_id;

        // Get the block ID of the block in which this variable was declared.
        if let Some(decl_block_id) = self.analyze_context.get_var_decl_scope(&var.name, block_id) {
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
        } else {
            Err(CodeGenError(format!(
                "Unable to find variable with name {} in block {} during load.",
                &var.name, self.state.cur_block_id
            )))
        }
    }

    // TODO: Array access.

    // TODO: Temporarily treats functions return void as return i32 "0".
    //       Should make a custom value ex rusts "()" instead.
    /// Generates a function call. Returns the return value of the compiled
    /// function.
    fn compile_func_call(
        &mut self,
        func_call: &FunctionCall,
    ) -> CustomResult<BasicValueEnum<'ctx>> {
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
                args.push(self.compile_expr(&arg.value)?);
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
                ret_val
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

    fn compile_op(&mut self, op: &Operation) -> CustomResult<BasicValueEnum<'ctx>> {
        match op {
            Operation::BinaryOperation(bin_op) => self.compile_bin_op(bin_op),
            Operation::UnaryOperation(un_op) => self.compile_un_op(un_op),
        }
    }

    // TODO: Currently only ints, make for floats and other types.
    fn compile_bin_op(&mut self, bin_op: &BinaryOperation) -> CustomResult<BasicValueEnum<'ctx>> {
        let left = self.compile_expr(&bin_op.left)?.into_int_value();
        let right = self.compile_expr(&bin_op.right)?.into_int_value();

        Ok(match bin_op.operator {
            token::BinaryOperator::In => panic!("TODO: In"),
            token::BinaryOperator::Is => panic!("TODO: Is"),
            token::BinaryOperator::As => panic!("TODO: As"),
            token::BinaryOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            token::BinaryOperator::Range => panic!("TODO: Range"),
            token::BinaryOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            token::BinaryOperator::Dot => panic!("TODO: Dot"),

            token::BinaryOperator::Equals => {
                // TODO: Just int compare atm.

                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "EQ.int")
                    .into()
            }
            token::BinaryOperator::NotEquals => self
                .builder
                .build_int_compare(IntPredicate::NE, left, right, "NE.int")
                .into(),
            // TODO: Signed/unsigned compares.
            token::BinaryOperator::LessThan => self
                .builder
                .build_int_compare(IntPredicate::SLT, left, right, "SLT.int")
                .into(),
            token::BinaryOperator::GreaterThan => self
                .builder
                .build_int_compare(IntPredicate::SGT, left, right, "SGT.int")
                .into(),
            token::BinaryOperator::LessThanOrEquals => self
                .builder
                .build_int_compare(IntPredicate::SLE, left, right, "SLE.int")
                .into(),
            token::BinaryOperator::GreaterThanOrEquals => self
                .builder
                .build_int_compare(IntPredicate::SGE, left, right, "SGE.int")
                .into(),

            token::BinaryOperator::Addition => {
                self.builder.build_int_add(left, right, "add.int").into()
            }
            token::BinaryOperator::Subtraction => {
                self.builder.build_int_sub(left, right, "sub.int").into()
            }
            token::BinaryOperator::Multiplication => {
                self.builder.build_int_mul(left, right, "mul.int").into()
            }
            token::BinaryOperator::Division => self
                .builder
                .build_int_signed_div(left, right, "div.int")
                .into(),
            token::BinaryOperator::Modulus => self
                .builder
                .build_int_signed_rem(left, right, "mod.int")
                .into(),
            token::BinaryOperator::Power => panic!("TODO: Power"),

            token::BinaryOperator::BitAnd => self.builder.build_and(left, right, "bit.and").into(),
            token::BinaryOperator::BitOr => self.builder.build_or(left, right, "bit.or").into(),
            token::BinaryOperator::BitXor => self.builder.build_xor(left, right, "bit.xor").into(),
            token::BinaryOperator::ShiftLeft => {
                self.builder.build_left_shift(left, right, "lshift").into()
            }
            token::BinaryOperator::ShiftRight => self
                .builder
                .build_right_shift(left, right, true, "rshift")
                .into(),
            token::BinaryOperator::BoolAnd => panic!("TODO: BoolAnd"),
            token::BinaryOperator::BoolOr => panic!("TODO: BooldOr"),

            token::BinaryOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
        })
    }

    // TODO: Only ints atm.
    fn compile_un_op(&mut self, un_op: &UnaryOperation) -> CustomResult<BasicValueEnum<'ctx>> {
        let value = self.compile_expr(&un_op.value)?.into_int_value();

        Ok(match un_op.operator {
            token::UnaryOperator::Increment => {
                let one = self.context.i32_type().const_int(1, false);
                self.builder.build_int_add(value, one, "inc").into()
            }
            token::UnaryOperator::Decrement => {
                // TODO: What type to use?
                let one = self.context.i32_type().const_int(1, false);
                self.builder.build_int_sub(value, one, "sub").into()
            }
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
            token::UnaryOperator::Negative => {
                let minus_one = self.context.i32_type().const_all_ones();
                self.builder.build_int_mul(value, minus_one, "pos").into()
            }
            token::UnaryOperator::BitComplement => panic!("TODO: Bit complement"),
            token::UnaryOperator::BoolNot => self.builder.build_not(value, "not").into(),
        })
    }
}
