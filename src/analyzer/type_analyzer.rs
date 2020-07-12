use crate::analyzer::analyzer::{AnalyzeContext, VariableState};
use crate::common::variable_type::Type;
use crate::parser::abstract_syntax_tree::{ASTBlock, RCNode, AST};
use crate::parser::token::{
    ArrayAccess, BinaryOperation, Class, Function, FunctionCall, Literal, MacroCall, Operation,
    Token, TypeStruct, UnaryOperation, Variable,
};
use crate::parser::token::{BlockHeader, Expression, Statement};
use crate::CustomResult;
use std::cell::RefMut;
use std::collections::HashMap;

pub struct TypeAnalyzer<'a> {
    context: &'a mut AnalyzeContext<'a>,
}

impl<'a> TypeAnalyzer<'a> {
    /// Takes in a abstract syntax tree and tries to infer all the missing types.
    pub fn analyze(context: &'a mut AnalyzeContext, ast: &mut AST) -> CustomResult<()> {
        let mut type_analyzer = TypeAnalyzer::new(context);
        let root_block = ast.blocks[0].borrow_mut();
        type_analyzer.analyze_recursive(root_block)
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self { context }
    }

    fn analyze_recursive(&mut self, block: RefMut<ASTBlock>) -> CustomResult<()> {
        for child in &block.children {
            match child {
                RCNode::Block(ast_block) => {
                    let mut block = ast_block.borrow_mut();
                    self.context.current_scope = block.index;
                    self.context
                        .parent_scopes
                        .insert(block.index, block.parent_index);
                    self.parse_type(&mut block.token)?;
                    self.analyze_recursive(block)?;
                }
                RCNode::Token(ast_token) => {
                    let mut ast_token = ast_token.borrow_mut();
                    self.parse_type(&mut ast_token.token)?;
                }
            }
        }

        Ok(())
    }

    fn parse_type(&mut self, token: &mut Token) -> CustomResult<()> {
        match token {
            Token::Expression(expression) => self.parse_expression_type(expression),
            Token::Statement(statement) => self.parse_statement_type(statement),
            Token::BlockHeader(block_header) => self.parse_block_header_type(block_header),
        }
    }

    fn parse_expression_type(&mut self, expression: &mut Expression) -> CustomResult<()> {
        match expression {
            Expression::Literal(literal) => self.parse_literal_type(literal),
            Expression::Integer(integer, i_type) => self.parse_integer_type(integer, i_type),
            Expression::Float(float, f_type) => self.parse_float_type(float, f_type),
            Expression::Variable(variable) => self.parse_variable_type(variable),
            Expression::ArrayAccess(array_access) => self.parse_array_access_type(array_access),
            Expression::FunctionCall(function_call) => self.parse_function_call_type(function_call),
            Expression::MacroCall(macro_call) => self.parse_macro_call_type(macro_call),
            Expression::Operation(operation) => self.parse_operation_type(operation),
            _ => panic!("Bad expression: {:?}", expression),
        }
    }

    fn parse_expression_type_opt(
        &mut self,
        expression_opt: &mut Option<Expression>,
    ) -> CustomResult<()> {
        if let Some(expression) = expression_opt {
            self.parse_expression_type(expression)
        } else {
            panic!("None expression.");
        }
    }

    fn parse_literal_type(&mut self, literal: &Literal) -> CustomResult<()> {
        // Nothing to do here atm, the type can be seen as either String or Character
        // from the literal type.
        Ok(())
    }

    // TODO: The type might be specified postfix ex. 123u32
    fn parse_integer_type(
        &mut self,
        integer: &str,
        i_type: &Option<TypeStruct>,
    ) -> CustomResult<()> {
        // Nothing to do here atm, this is a integer value.
        // Might be needed in the future, see the comment above.
        Ok(())
    }

    // TODO: The type might be specified postfix ex. 1f32
    fn parse_float_type(&mut self, float: &str, f_type: &Option<TypeStruct>) -> CustomResult<()> {
        // Nothing to do here atm, this is a float value.
        // Might be needed in the future, see the comment above.
        Ok(())
    }

    fn parse_variable_type(&mut self, variable: &mut Variable) -> CustomResult<()> {
        // Add a new map of this variable in `self.variables` if this is the first time this
        // variable is seen. This is probably the declaration of the variable.
        self.context
            .variables
            .entry(variable.name.clone())
            .or_insert_with(HashMap::new);

        let scope_variables_opt = self.context.variables.get_mut(&variable.name);

        if let Some(scope_variables) = scope_variables_opt {
            // Add a new map of variable state if the current variable doesn't exist in the map yet.
            scope_variables
                .entry(self.context.current_scope)
                .or_insert_with(VariableState::new);

            if let Some(variable_state) = scope_variables.get_mut(&self.context.current_scope) {
                // Set the "one true" base_type if it isn't set yet.
                if variable_state.base_type.is_none() {
                    variable_state.base_type = variable.var_type.clone();
                }

                // Assume that the `base_type` is the correct type for this variable and set it
                // for this variable if it has no type yet.
                if variable.var_type.is_none() {
                    variable.var_type = variable_state.base_type.clone();
                }
            }
        }

        Ok(())
    }

    fn parse_array_access_type(
        &mut self,
        array_access_opt: &Option<ArrayAccess>,
    ) -> CustomResult<()> {
        Ok(())
    }

    // TODO: Look up function return type in self.functions and return that.
    fn parse_function_call_type(
        &mut self,
        function_call: &Option<FunctionCall>,
    ) -> CustomResult<()> {
        Ok(())
    }

    // TODO: Look up function return type in self.functions and return that.
    fn parse_macro_call_type(&mut self, macro_call: &Option<MacroCall>) -> CustomResult<()> {
        Ok(())
    }

    fn parse_operation_type(&mut self, operation: &mut Operation) -> CustomResult<()> {
        match operation {
            Operation::BinaryOperation(ref mut binary_operation) => {
                self.parse_binary_operation_type(binary_operation)
            }
            Operation::UnaryOperation(ref mut unary_operation) => {
                self.parse_unary_operation_type(unary_operation)
            }
        }
    }

    // TODO: This is completly broken. Need to check better. This will panic if the two expressions
    //       of the binary operation have different types, in most of those cases it should not panic.
    fn parse_binary_operation_type(
        &mut self,
        binary_operation_opt: &mut Option<BinaryOperation>,
    ) -> CustomResult<()> {
        // TODO: Fix this shit.

        if let Some(binary_operation) = binary_operation_opt {
            self.parse_expression_type(&mut binary_operation.left)?;
            self.parse_expression_type(&mut binary_operation.right)?;

            let left_type = match &*binary_operation.left {
                Expression::Literal(Literal::CharLiteral(_)) => {
                    Some(TypeStruct::new(Type::Character, None))
                }
                Expression::Literal(Literal::StringLiteral(_)) => {
                    Some(TypeStruct::new(Type::String, None))
                }
                Expression::Integer(_, type_struct_opt) => type_struct_opt.clone(),
                Expression::Float(_, type_struct_opt) => type_struct_opt.clone(),
                Expression::Variable(variable) => variable.var_type.clone(),
                Expression::ArrayAccess(Some(array_access)) => {
                    array_access.variable.var_type.clone()
                }
                Expression::ArrayAccess(None) => None,
                Expression::FunctionCall(Some(function_call)) => {
                    if let Some(function) = &function_call.function {
                        function.return_type.clone()
                    } else {
                        None
                    }
                }
                Expression::FunctionCall(None) => None,
                Expression::MacroCall(_) => None,
                // TODO: Where should case be, is it even an expression?
                Expression::Case(_) => None,
                Expression::Operation(Operation::BinaryOperation(binary_operation_opt)) => {
                    if let Some(binary_operation) = binary_operation_opt {
                        binary_operation.var_type.clone()
                    } else {
                        None
                    }
                }
                Expression::Operation(Operation::UnaryOperation(unary_operation_opt)) => {
                    if let Some(unary_operation) = unary_operation_opt {
                        unary_operation.var_type.clone()
                    } else {
                        None
                    }
                }
            };

            let right_type = match &*binary_operation.right {
                Expression::Literal(Literal::CharLiteral(_)) => {
                    Some(TypeStruct::new(Type::Character, None))
                }
                Expression::Literal(Literal::StringLiteral(_)) => {
                    Some(TypeStruct::new(Type::String, None))
                }
                Expression::Integer(_, type_struct_opt) => type_struct_opt.clone(),
                Expression::Float(_, type_struct_opt) => type_struct_opt.clone(),
                Expression::Variable(variable) => variable.var_type.clone(),
                Expression::ArrayAccess(Some(array_access)) => {
                    array_access.variable.var_type.clone()
                }
                Expression::ArrayAccess(None) => None,
                Expression::FunctionCall(Some(function_call)) => {
                    if let Some(function) = &function_call.function {
                        function.return_type.clone()
                    } else {
                        None
                    }
                }
                Expression::FunctionCall(None) => None,
                Expression::MacroCall(_) => None,
                // TODO: Where should case be, is it even an expression?
                Expression::Case(_) => None,
                Expression::Operation(Operation::BinaryOperation(binary_operation_opt)) => {
                    if let Some(binary_operation) = binary_operation_opt {
                        binary_operation.var_type.clone()
                    } else {
                        None
                    }
                }
                Expression::Operation(Operation::UnaryOperation(unary_operation_opt)) => {
                    if let Some(unary_operation) = unary_operation_opt {
                        unary_operation.var_type.clone()
                    } else {
                        None
                    }
                }
            };

            let inferred_type = self.infer_type(&left_type, &right_type);
            binary_operation.var_type = inferred_type;
            // TODO: How should the types be parsed from binary operations?
            Ok(())
        } else {
            panic!("Bad binary operation?");
        }
    }

    fn parse_unary_operation_type(
        &mut self,
        unary_operation_opt: &mut Option<UnaryOperation>,
    ) -> CustomResult<()> {
        if let Some(unary_operation) = unary_operation_opt {
            self.parse_expression_type(&mut unary_operation.value)?;

            // TODO: Fix this shit.
            let unary_type = match &*unary_operation.value {
                Expression::Literal(Literal::CharLiteral(_)) => {
                    Some(TypeStruct::new(Type::Character, None))
                }
                Expression::Literal(Literal::StringLiteral(_)) => {
                    Some(TypeStruct::new(Type::String, None))
                }
                Expression::Integer(_, type_struct_opt) => type_struct_opt.clone(),
                Expression::Float(_, type_struct_opt) => type_struct_opt.clone(),
                Expression::Variable(variable) => variable.var_type.clone(),
                Expression::ArrayAccess(Some(array_access)) => {
                    array_access.variable.var_type.clone()
                }
                Expression::ArrayAccess(None) => None,
                Expression::FunctionCall(Some(function_call)) => {
                    if let Some(function) = &function_call.function {
                        function.return_type.clone()
                    } else {
                        None
                    }
                }
                Expression::FunctionCall(None) => None,
                Expression::MacroCall(_) => None,
                // TODO: Where should case be, is it even an expression?
                Expression::Case(_) => None,
                Expression::Operation(Operation::BinaryOperation(binary_operation_opt)) => {
                    if let Some(binary_operation) = binary_operation_opt {
                        binary_operation.var_type.clone()
                    } else {
                        None
                    }
                }
                Expression::Operation(Operation::UnaryOperation(unary_operation_opt)) => {
                    if let Some(unary_operation) = unary_operation_opt {
                        unary_operation.var_type.clone()
                    } else {
                        None
                    }
                }
            };

            unary_operation.var_type = unary_type;

            Ok(())
        } else {
            panic!("Bad unary operation?");
        }
    }

    fn parse_statement_type(&mut self, statement: &mut Statement) -> CustomResult<()> {
        match statement {
            Statement::Return(return_opt) => self.parse_expression_type_opt(return_opt),
            Statement::Yield(yield_opt) => self.parse_expression_type_opt(yield_opt),
            Statement::Break
            | Statement::Continue
            | Statement::Use(_)
            | Statement::Package(_)
            | Statement::Modifier(_)
            | Statement::Init => Ok(()),
            Statement::Throw(throw) => self.parse_expression_type_opt(throw),
            _ => panic!("Bad statement: {:?}", statement),
        }
    }

    fn parse_block_header_type(&mut self, block_header: &mut BlockHeader) -> CustomResult<()> {
        match block_header {
            BlockHeader::Default => Ok(()),
            BlockHeader::Function(function) => self.parse_function_type(function),
            BlockHeader::Class(class) => self.parse_class_type(class),
            BlockHeader::Enum(_) => Ok(()),
            BlockHeader::Interface(_) => Ok(()),
            BlockHeader::Macro(_) => Ok(()),
            BlockHeader::Constructor(_) => Ok(()),
            BlockHeader::Destructor => Ok(()),
            BlockHeader::If(expression_opt) => self.parse_expression_type_opt(expression_opt),
            BlockHeader::Else(Some(ref mut expression)) => self.parse_expression_type(expression),
            BlockHeader::Else(None) => Ok(()),
            BlockHeader::Match(expression_opt) => self.parse_expression_type_opt(expression_opt),
            BlockHeader::Defer => Ok(()),
            BlockHeader::For(ref mut binary_operation_opt) => {
                self.parse_binary_operation_type(binary_operation_opt)
            }
            BlockHeader::While(expression_opt) => self.parse_expression_type_opt(expression_opt),
            BlockHeader::Loop => Ok(()),
            BlockHeader::With(expression_opt) => Ok(()),
            BlockHeader::Test(test) => Ok(()),
            _ => panic!("Bad block header: {:?}", block_header),
        }
    }

    fn parse_function_type(&mut self, function: &Option<Function>) -> CustomResult<()> {
        // Nothing to do here atm.
        Ok(())
    }

    fn parse_class_type(&mut self, class: &Option<Class>) -> CustomResult<()> {
        // Nothing to do here atm.
        Ok(())
    }

    // TODO: How should inheritance/implements etc. work?
    fn infer_type(
        &self,
        left_type_opt: &Option<TypeStruct>,
        right_type_opt: &Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        if left_type_opt.is_some() && right_type_opt.is_none() {
            left_type_opt.clone()
        } else if left_type_opt.is_none() && right_type_opt.is_some() {
            right_type_opt.clone()
        } else if left_type_opt.is_some() && right_type_opt.is_some() {
            let left_type = left_type_opt.as_ref().expect("left is none");
            let right_type = right_type_opt.as_ref().expect("right is none");

            if left_type == right_type {
                Some(left_type.clone())
            } else {
                None
            }
        } else {
            // Both none.
            None
        }
    }
}
