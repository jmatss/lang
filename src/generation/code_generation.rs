use crate::analyzer::analyzer::{AnalyzeContext, VariableState};
use crate::common::variable_type::Type;
use crate::error::CustomError;
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::abstract_syntax_tree::{RCBlock, RCNode};
use crate::parser::token;
use crate::parser::token::{
    ArrayAccess, BinaryOperation, BinaryOperator, Expression, Function, FunctionCall, Literal,
    Operation, Token, TypeStruct, UnaryOperation, Variable,
};
use crate::CustomResult;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::BasicValue;
use inkwell::values::{FunctionValue, PointerValue};
use inkwell::IntPredicate;

struct CodeGen<'ctx> {
    pub gen_context: &'ctx Context,
    pub analyze_context: &'ctx AnalyzeContext<'ctx>,

    pub builder: &'ctx Builder<'ctx>,
    pub fpm: &'ctx PassManager<FunctionValue<'ctx>>,
    pub module: &'ctx Module<'ctx>,

    pub ast: &'ctx AST,
}

pub fn generate(ast: &AST, analyze_context: &AnalyzeContext) -> CustomResult<()> {
    let gen_context = Context::create();
    let builder = gen_context.create_builder();
    let module = gen_context.create_module(MODULE_NAME);
    let fpm = PassManager::create(module);

    let mut code_gen = CodeGen::new(&gen_context, analyze_context, &builder, &fpm, &module, ast);

    let root_block = code_gen.ast.blocks[0].clone();
    code_gen.generate_recursive(&root_block)?;

    Ok(())
}

const MODULE_NAME: &str = "MODULE_NAME";

impl<'ctx> CodeGen<'ctx> {
    fn new(
        gen_context: &'ctx Context,
        analyze_context: &'ctx AnalyzeContext,
        builder: &'ctx Builder<'ctx>,
        fpm: &'ctx PassManager<FunctionValue<'ctx>>,
        module: &'ctx Module<'ctx>,
        ast: &'ctx AST,
    ) -> Self {
        Self {
            gen_context,
            analyze_context,
            builder,
            fpm,
            module,
            ast,
        }
    }

    fn generate_recursive(&mut self, block: &RCBlock) -> CustomResult<()> {
        for child in &block.borrow().children {
            let scope_index;
            let token = match child {
                RCNode::Block(rc_block) => {
                    scope_index = rc_block.borrow().index;
                    rc_block.borrow().token.clone()
                }
                RCNode::Token(rc_token) => {
                    scope_index = rc_token.borrow().parent_index;
                    rc_token.borrow().token.clone()
                }
            };

            println!("TOKEN: {:?}", token);
            match token {
                Token::BlockHeader(ref block_header) => {
                    let mut result = self.compile_block_header(block_header)?;
                }
                Token::Expression(ref expr) => {
                    let mut result = self.compile_expression(expr, scope_index)?;
                }
                Token::Statement(x) => {
                    println!("{:?}", x);
                }
                _ => (),
            };
        }

        Ok(())
    }

    fn create_entry_block_alloca(
        &self,
        func: &FunctionValue<'ctx>,
        name: &Variable,
    ) -> PointerValue<'ctx> {
        let builder = self.gen_context.create_builder();

        let entry = func
            .get_first_basic_block()
            .expect("Unable to unwrap first basic block in func.");

        /*
        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => self.builder.position_at_end(entry),
        }

         */
    }

    pub fn compile_func(&self, func: &Function) {}

    pub fn compile_func_prototype(&self, func: &Function) {
        /*
        let return_type = if let Some(return_type) = &func.return_type {
            return_type.
        } else {

        };
        let ret_type = func.return_type
        */
    }

    fn compile_expression(
        &mut self,
        expr: &Expression,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        match expr {
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Integer(integer, t) => self.compile_integer(integer, t),
            Expression::Float(float, t) => self.compile_float(float, t),
            Expression::Variable(variable) => self.compile_variable(variable, scope_index),
            Expression::ArrayAccess(array_access_opt) => {
                self.compile_array_access(array_access_opt, scope_index)
            }
            Expression::FunctionCall(function_call_opt) => {
                self.compile_function_call(function_call_opt, scope_index)
            }
            Expression::MacroCall(macro_call) => Err(CustomError::GenerationError(
                "Macro call not implemented".to_string(),
            )),
            Expression::Case(case) => Err(CustomError::GenerationError(
                "Case not implemented".to_string(),
            )),
            Expression::Operation(operation) => self.compile_operation(operation, scope_index),
            _ => Err(CustomError::GenerationError("abc".to_string())),
        }
    }

    fn compile_literal(
        &mut self,
        literal: &Literal,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        match literal {
            Literal::StringLiteral(str_literal) => {
                let null_terminated = true;
                Ok(Box::new(
                    self.gen_context
                        .const_string(str_literal.as_bytes(), null_terminated),
                ))
            }
            Literal::CharLiteral(char_literal) => {
                if char_literal.chars().count() == 1 {
                    if let Some(char) = char_literal.chars().next() {
                        Ok(Box::new(
                            self.gen_context.i32_type().const_int(char as u64, false),
                        ))
                    } else {
                        Err(CustomError::GenerationError(
                            "Unable to get char literal.".to_string(),
                        ))
                    }
                } else {
                    Err(CustomError::GenerationError(
                        "Char literal isn't a single character.".to_string(),
                    ))
                }
            }
        }
    }

    fn compile_integer(
        &mut self,
        integer: &str,
        type_struct_opt: &Option<TypeStruct>,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        Ok(Box::new(match type_struct_opt {
            Some(type_struct) => match type_struct.t {
                Type::I8 => self.gen_context.i8_type().const_int(integer.parse()?, true),
                Type::U8 => self
                    .gen_context
                    .i8_type()
                    .const_int(integer.parse()?, false),
                Type::I16 => self
                    .gen_context
                    .i16_type()
                    .const_int(integer.parse()?, true),
                Type::U16 => self
                    .gen_context
                    .i16_type()
                    .const_int(integer.parse()?, false),
                Type::I32 => self
                    .gen_context
                    .i32_type()
                    .const_int(integer.parse()?, true),
                Type::U32 => self
                    .gen_context
                    .i32_type()
                    .const_int(integer.parse()?, false),
                Type::I64 => self
                    .gen_context
                    .i64_type()
                    .const_int(integer.parse()?, true),
                Type::U64 => self
                    .gen_context
                    .i64_type()
                    .const_int(integer.parse()?, false),
                Type::I128 => self
                    .gen_context
                    .i128_type()
                    .const_int(integer.parse()?, true),
                Type::U128 => self
                    .gen_context
                    .i128_type()
                    .const_int(integer.parse()?, false),
                // TODO: What should the default inte size be?
                Type::Int => self
                    .gen_context
                    .i32_type()
                    .const_int(integer.parse()?, true),
                Type::Uint => self
                    .gen_context
                    .i32_type()
                    .const_int(integer.parse()?, false),
                _ => unreachable!("Invalid integer type: {:?}", type_struct.t),
            },
            // TODO: What should the default int size be? Signed 32 atm.
            None => self
                .gen_context
                .i32_type()
                .const_int(integer.parse()?, true),
        }))
    }

    fn compile_float(
        &mut self,
        float: &str,
        type_struct_opt: &Option<TypeStruct>,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        Ok(Box::new(match type_struct_opt {
            Some(type_struct) => match type_struct.t {
                Type::F32 => self.gen_context.f32_type().const_float(float.parse()?),
                Type::F64 => self.gen_context.f64_type().const_float(float.parse()?),
                // TODO: What should the default float sise be?
                Type::Float => self.gen_context.f32_type().const_float(float.parse()?),
                _ => unreachable!("Invalid float type: {:?}", type_struct.t),
            },
            // TODO: What should the default float size be? F32 atm.
            None => self.gen_context.f32_type().const_float(float.parse()?),
        }))
    }

    fn compile_variable(
        &mut self,
        variable: &Variable,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if let Some(ref mut map_variables) = self.analyze_context.variables.get(&variable.name) {
            let mut mut_scope_index = scope_index;
            loop {
                if let Some(map_variable) = map_variables.get(&mut_scope_index) {
                    // If this is a declaration, the pointer should be created.
                    // Else: the pointer should already exists in the map so that it can be referenced.
                    return if variable.declaration {
                        // TODO: Only works for int atm.
                        let t = self.gen_context.i32_type();
                        Ok(Box::new(self.builder.build_alloca(t, &variable.name)))
                    } else if let Some(pointer) = map_variable.pointer {
                        Ok(Box::new(pointer))
                    } else {
                        Err(CustomError::GenerationError(format!(
                            "Invalid pointer for variable with name {} in scope {}.",
                            &variable.name, scope_index
                        )))
                    };
                }

                // If there are not more parents to look at, this function was unable to find
                // this variable in any of its ancestors. Return error, otherwise continue iterating
                // through the parents to try to find the variable.
                if let Some(parent_scope_index) =
                    self.analyze_context.parent_scopes.get(&mut_scope_index)
                {
                    mut_scope_index = *parent_scope_index;
                } else {
                    return Err(CustomError::GenerationError(format!(
                        "Unable to find variable with name {} in any of the parents of scope {}.",
                        &variable.name, scope_index
                    )));
                }
            }
        } else {
            Err(CustomError::GenerationError(format!(
                "Unable to find variable with name {} in map.",
                &variable.name
            )))
        }
    }

    // TODO: Array access.
    fn compile_array_access(
        &mut self,
        array_access_opt: &Option<ArrayAccess>,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if let Some(array_access) = array_access_opt {
            if let Some(pointer) = self.get_variable(&array_access.variable.name, scope_index) {
                Err(CustomError::GenerationError(
                    "Array access not implemented.".to_string(),
                ))
            } else {
                Err(CustomError::GenerationError(format!(
                    "Unable to find variable pointer for array access on variable {}.",
                    &array_access.variable.name
                )))
            }
        } else {
            Err(CustomError::GenerationError(
                "Array access is set to None.".to_string(),
            ))
        }
    }

    fn compile_function_call(
        &mut self,
        function_call_opt: &Option<FunctionCall>,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if let Some(function_call) = function_call_opt {
            if let Some(function_pointer) = self.module.get_function(&function_call.name) {
                let mut args = Vec::with_capacity(function_call.arguments.len());
                for arg in function_call.arguments {
                    let a = self.compile_expression(&arg.value, scope_index)?;
                    args.push(a);
                }

                Ok(Box::new(self.builder.build_call(
                    function_pointer,
                    args.as_slice(),
                    "function_call",
                )))
            } else {
                Err(CustomError::GenerationError(format!(
                    "Unable to find function with name {} to call.",
                    &function_call.name
                )))
            }
        } else {
            Err(CustomError::GenerationError(
                "Function call is set to None.".to_string(),
            ))
        }
    }

    fn compile_operation(
        &mut self,
        operation: &Operation,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        match operation {
            Operation::BinaryOperation(binary_operation_opt) => {
                self.compile_binary_operation(binary_operation_opt, scope_index)
            }
            Operation::UnaryOperation(unary_operation_opt) => {
                self.compile_unary_operation(unary_operation_opt, scope_index)
            }
        }
    }

    fn compile_binary_operation(
        &mut self,
        binary_operation_opt: &Option<BinaryOperation>,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if let Some(binary_operation) = binary_operation_opt {
            let left = self.compile_expression(&binary_operation.left, scope_index)?;
            let right = self.compile_expression(&binary_operation.right, scope_index)?;

            Ok(match binary_operation.operator {
                token::BinaryOperator::Assignment => panic!("TODO: Assignment"),
                token::BinaryOperator::In => panic!("TODO: In"),
                token::BinaryOperator::Is => panic!("TODO: Is"),
                token::BinaryOperator::As => panic!("TODO: As"),
                token::BinaryOperator::Of => panic!("TODO: Of"),

                // Create some sort of typedef that then can be used to iterate over.
                token::BinaryOperator::Range => panic!("TODO: Range"),
                token::BinaryOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
                token::BinaryOperator::MatchCase => panic!("TODO: matchCase, how to implement?"),
                token::BinaryOperator::Dot => panic!("TODO: Dot"),

                token::BinaryOperator::Equals => {
                    // TODO: Just int compare atm.
                    Box::new(self.builder.build_int_compare(
                        IntPredicate::EQ,
                        left,
                        right,
                        "tmpintEQ",
                    ))
                }
                token::BinaryOperator::NotEquals => Box::new(self.builder.build_int_compare(
                    IntPredicate::NE,
                    left,
                    right,
                    "tmpintNE",
                )),
                // TODO: Signed/unsigned compares.
                token::BinaryOperator::LessThan => Box::new(self.builder.build_int_compare(
                    IntPredicate::SLT,
                    left,
                    right,
                    "tmpintSLT",
                )),
                token::BinaryOperator::GreaterThan => Box::new(self.builder.build_int_compare(
                    IntPredicate::SGT,
                    left,
                    right,
                    "tmpintSGT",
                )),
                token::BinaryOperator::LessThanOrEquals => Box::new(
                    self.builder
                        .build_int_compare(IntPredicate::SLE, left, right, "tmpintSLE"),
                ),
                token::BinaryOperator::GreaterThanOrEquals => Box::new(
                    self.builder
                        .build_int_compare(IntPredicate::SGE, left, right, "tmpintSGE"),
                ),

                token::BinaryOperator::Addition => {
                    Box::new(self.builder.build_int_add(left, right, "tmpadd"))
                }
                token::BinaryOperator::Subtraction => {
                    Box::new(self.builder.build_int_sub(left, right, "tmpsub"))
                }
                token::BinaryOperator::Multiplication => {
                    Box::new(self.builder.build_int_mul(left, right, "tmpmul"))
                }
                token::BinaryOperator::Division => {
                    Box::new(self.builder.build_int_signed_div(left, right, "tmpdiv"))
                }
                token::BinaryOperator::Modulus => {
                    Box::new(self.builder.build_int_signed_rem(left, right, "tmpmod"))
                }
                token::BinaryOperator::Power => panic!("TODO: Power"),

                token::BinaryOperator::BitAnd => {
                    Box::new(self.builder.build_and(left, right, "tmpbitand"))
                }
                token::BinaryOperator::BitOr => {
                    Box::new(self.builder.build_or(left, right, "tmpbitor"))
                }
                token::BinaryOperator::BitXor => {
                    Box::new(self.builder.build_xor(left, right, "tmpbitxor"))
                }
                token::BinaryOperator::ShiftLeft => {
                    Box::new(self.builder.build_left_shift(left, right, "tmpshiftleft"))
                }
                token::BinaryOperator::ShiftRight => {
                    Box::new(
                        self.builder
                            .build_right_shift(left, right, true, "tmpshiftright"),
                    )
                }
                token::BinaryOperator::BoolAnd => panic!("TODO: BoolAnd"),
                token::BinaryOperator::BoolOr => panic!("TODO: BooldOr"),

                token::BinaryOperator::AssignAddition => panic!("TODO: AssignAddition"),
                token::BinaryOperator::AssignSubtraction => panic!("TODO: AssignSubtraction"),
                token::BinaryOperator::AssignMultiplication => panic!("TODO: AssignMultiplication"),
                token::BinaryOperator::AssignDivision => panic!("TODO: AssignDivision"),
                token::BinaryOperator::AssignModulus => panic!("TODO: AssignModulus"),
                token::BinaryOperator::AssignPower => panic!("TODO: AssignPower"),

                token::BinaryOperator::AssignBitAnd => panic!("TODO: AssignBitAnd"),
                token::BinaryOperator::AssignBitOr => panic!("TODO: AssignBitOr"),
                token::BinaryOperator::AssignBitXor => panic!("TODO: AssignBitXor"),
                token::BinaryOperator::AssignShiftLeft => panic!("TODO: AssignShiftLeft"),
                token::BinaryOperator::AssignShiftRight => panic!("TODO: AssignShiftRight"),

                token::BinaryOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
            })
        } else {
            Err(CustomError::GenerationError(
                "Binary operation is set to None.".to_string(),
            ))
        }
    }

    fn compile_unary_operation(
        &mut self,
        unary_operation_opt: &Option<UnaryOperation>,
        scope_index: usize,
    ) -> CustomResult<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if let Some(array_access) = array_access_opt {
            if let Some(pointer) = self.get_variable(&array_access.variable.name, scope_index) {
                Err(CustomError::GenerationError(
                    "Array access not implemented.".to_string(),
                ))
            } else {
                Err(CustomError::GenerationError(format!(
                    "Unable to find variable pointer for array access on variable {}.",
                    &array_access.variable.name
                )))
            }
        } else {
            Err(CustomError::GenerationError(
                "Array access is set to None.".to_string(),
            ))
        }
    }

    fn get_variable(
        &self,
        variable_name: &str,
        scope_index: usize,
    ) -> Option<Box<dyn BasicValue<'ctx> + 'ctx>> {
        if let Some(ref mut map_variables) = self.analyze_context.variables.get(variable_name) {
            let mut mut_scope_index = scope_index;
            loop {
                if let Some(map_variable) = map_variables.get(&mut_scope_index) {
                    if let Some(pointer) = map_variable.pointer {
                        return Some(Box::new(pointer));
                    }
                }

                // If there are not more parents to look at, this function was unable to find
                // this variable in any of its ancestors. Return error, otherwise continue iterating
                // through the parents to try to find the variable.
                if let Some(parent_scope_index) =
                    self.analyze_context.parent_scopes.get(&mut_scope_index)
                {
                    mut_scope_index = *parent_scope_index;
                } else {
                    return None;
                }
            }
        } else {
            None
        }
    }
}
