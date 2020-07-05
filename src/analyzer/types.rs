use crate::parser::abstract_syntax_tree::{ScopeIndex, AST, RCNode, RCBlock, ASTToken, ASTBlock};
use crate::parser::token::{Token, Literal, TypeStruct, Variable, ArrayAccess, FunctionCall, MacroCall, Operation, BinaryOperation, BinaryOperator, UnaryOperation, Function, Class, Macro};
use std::collections::HashMap;
use std::cell::RefMut;
use crate::parser::token::{Expression, Statement, BlockHeader};
use crate::CustomResult;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Character,
    String,
    Boolean,
    Int,
    Uint,
    Float,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    F32,
    I64,
    U64,
    F64,
    I128,
    U128,
    Unknown(String)
}

pub struct VariableState<'a> {
    tokens: Vec<&'a Token>,
    base_type: Option<TypeStruct>,
}

/// The `AnalyzeState` will contain information about the current know variables and blocks.
pub struct TypeAnalyzer<'a> {
    /// Contains all variables that have been seen traversing down to this part of the code.
    /// The outer map has a map that represents the scope that it is declared in and the
    /// inner map has the variable name as the key and the variable state as the value
    /// which contains all uses and the assumed actual type.
    variables: HashMap<ScopeIndex, HashMap<String, VariableState<'a>>>,

    /// List of types that are valid.
    types: HashMap<String, TypeStruct>,

    // TODO: Need to have a list of all functions so that one can figure out the type
    //       of it when used in a expression.
    /// Contains all function calls that have been seen traversing down to this part of the code.
    /// The outer map has a map that represents the scope that it is used in and the
    /// inner map has the variable name as the key and the actual token as value.
    //functions: HashMap<ScopeIndex, HashMap<String, Token>>,

    // TODO: To this in a better way
    /// The key is a child scope and the value is the parent of that scope.
    parent_scopes: HashMap<ScopeIndex, ScopeIndex>,

    /// The scope that the `analyzer` currently are in.
    current_scope: ScopeIndex,
}

impl TypeAnalyzer {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            types: HashMap::new(),
            parent_scopes: HashMap::new(),
            current_scope: 0,
        }
    }

    /// Takes in a abstract syntax tree and tries to infer all the missing types.
    pub fn analyze(&mut self, ast: &mut AST) {
        let root_block = ast.blocks[0].borrow_mut();
        self.analyze_recursive(root_block);
    }

    fn analyze_recursive(&mut self, block: RefMut<ASTBlock>) -> CustomResult<()> {
        for child in &block.children {
            match child {
                RCNode::Block(ast_block) => {
                    let block = ast_block.borrow_mut();
                    self.current_scope = block.index;
                    self.parent_scopes.insert(block.index, block.parent_index);
                    let token_type = self.parse_type(&mut block.token)?;
                    block.token
                }
                RCNode::Token(ast_token) =>
            }
            let a = analyzer.parse(child);
        }

        let root_block_data_opt = &ast.blocks[0].borrow().block_data;
        if let Some(block_data) = root_block_data_opt {
            for block in &block_data.children {
                for child in &block_data.children {
                    let child_token = &child.borrow().token;

                    println!("TOKEN: {:?}", child_token);

                    match child_token {
                        Token::BlockHeader(block_header) => {
                            block_header.
                            // TODO: FIXME: Temporary let else be caught in here
                            // since they are special case.
                            let mut result = transpile_block_header(block_header)?;
                            lines.push(result);
                        }
                        Token::Expression(expr) => {
                            let mut result = transpile_expression(expr)?;
                            result.push_str(symbols::EXPR_BRAKE);
                            lines.push(result);
                        }
                        Token::Statement(x) => {
                            println!("{:?}", x);
                        }
                        _ => {}
                    }
                }

                lines.push(symbols::BLOCK_END.to_string());
            }
        } else {
            panic!("NO CHILDREN IN ROOT_BLOCK.");
        }
    }

    fn parse_type(&mut self, token: &mut Token) -> CustomResult<()> {
        match token {
            Token::Expression(expression) => self.parse_expression_type(expression),
            Token::Statement(statement) => self.parse_statement_type(statement),
            Token::BlockHeader(block_Header) => self.parse_block_header_type(block_Header),
        }
    }

    fn parse_expression_type(&mut self, expression: &Expression) -> CustomResult<()> {
        match expression {
            Expression::Literal(literal) => self.parse_literal_type(literal),
            Expression::Integer(integer, i_type) => self.parse_integer_type(integer, i_type),
            Expression::Float(float, f_type) => self.parse_float_type(float, f_type),
            Expression::Variable(variable) => self.parse_variable_type(variable),
            Expression::ArrayAccess(array_access) => self.parse_array_access_type(array_access),
            Expression::FunctionCall(function_call) => self.parse_function_call_type(function_call),
            Expression::MacroCall(macro_call) => self.parse_macro_call_type(macro_call),
            Expression::Operation(operation) => self.parse_operation_type(operation),
            _ => panic!("Bad expression: {:?}", expression)
        }
    }

    fn parse_expression_type_opt(&mut self, expression_opt: &Option<Expression>) -> CustomResult<()> {
        if let Some(expression) = expression_opt {
            self.parse_expression_type(expression)
        } else {
            _ => panic!("Bad expression: {:?}", expression)
        }
    }

    fn parse_literal_type(&mut self, literal: &Literal) -> CustomResult<()> {
        // Nothing to do here atm, the type can be seen as either String or Character
        // from the literal type.
        Ok(())
    }

    // TODO: The type might be specified postfix ex. 123u32
    fn parse_integer_type(&mut self, integer: &str, i_type: &Option<TypeStruct>) -> CustomResult<()> {
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

    fn parse_variable_type(&mut self, variable: &Variable) -> CustomResult<()> {
        // Add a new map of this variable in `self.variables` if this is the first time this
        // variable is seen. This is probably the declaration of the variable.
        if !self.variables.contains_key(&self.current_scope) {
            self.variables.insert(self.current_scope, HashMap::new());
        }

        let scope_variables_opt = self.variables.get_mut(&self.current_scope);

        if let Some(var_type) = variable.var_type.clone() {
            if let Some(scope_variables) = scope_variables_opt {
                if let Some(variable_state) = scope_variables.get_mut(&variable.name) {
                    if let Some(base_type) = &variable_state.base_type {

                    } else {
                        variable_state.base_type = &mut variable.var_type
                    }
                }
            }
        }

        Ok(TypeStruct::new(None, Vec::new()))
    }

    fn parse_array_access_type(&mut self, array_access_opt: &Option<ArrayAccess>) -> CustomResult<()> {
        Ok(TypeStruct::new(None, Vec::new()))
    }

    // TODO: Look up function return type in self.functions and return that.
    fn parse_function_call_type(&mut self, function_call: &Option<FunctionCall>) -> CustomResult<()> {
        Ok(TypeStruct::new(None, Vec::new()))
    }

    // TODO: Look up function return type in self.functions and return that.
    fn parse_macro_call_type(&mut self, macro_call: &Option<MacroCall>) -> CustomResult<()> {
        Ok(TypeStruct::new(None, Vec::new()))
    }

    fn parse_operation_type(&mut self, operation: &Operation) -> CustomResult<()> {
        match operation {
            Operation::BinaryOperation(binary_operation) => self.parse_binary_operation_type(binary_operation),
            Operation::UnaryOperation(unary_operation) => self.parse_unary_operation_type(unary_operation),
        }
    }

    // TODO: This is completly broken. Need to check better. This will panic if the two expressions
    //       of the binary operation have different types, in most of those cases it should not panic.
    fn parse_binary_operation_type(&mut self, binary_operation_opt: &Option<BinaryOperation>) -> CustomResult<()> {
        if let Some(binary_operation) = binary_operation_opt {
            let left_type = self.parse_expression_type(&binary_operation.left)?;
            let right_type = self.parse_expression_type(&binary_operation.right)?;

            self.decide_type(&left_type, &right_type)
        } else {
            panic!("Bad binary operation?");
        }
    }

    fn parse_unary_operation_type(&mut self, unary_operation_opt: &Option<UnaryOperation>) -> CustomResult<()> {
        if let Some(unary_operation) = unary_operation_opt {
            self.parse_expression_type(&unary_operation.value)
        } else {
            panic!("Bad unary operation?");
        }
    }

    // TODO: Decide what type to use, for now just panic if types are different.
    //       Should allow promotion/implements etc.
    fn decide_type(&mut self, type_a: &TypeStruct, type_b: &TypeStruct) -> CustomResult<()> {
        if type_a.t.is_some() && type_b.t.is_some() {
            let a = type_a.t.expect("typeA");
            let b = type_b.t.expect("typeB");

            let string_a = match a {
                Type::Unknown(string_a) => string_a,
                _ => "".to_string()
            };
            let string_b = match b {
                Type::Unknown(string_b) => string_b,
                _ => "".to_string()
            };
            if string_a != string_b {
                panic!("string_a != string_b: {} != {}.", string_a, string_b);
            }

            if a != b {
                panic!("Diff types for a and b, a: {:?}, b: {:?}", type_a, type_b);
            }

            return Ok(type_a.clone());
        } else if type_a.t.is_none() && type_b.t.is_none() {
            return Ok(type_a.clone());
        } else {
            panic!("Diff types for a and b, a: {:?}, b: {:?}", type_a, type_b);
        }
    }

    fn parse_statement_type(&mut self, statement: &Statement) -> CustomResult<()> {
        match statement {
            Statement::Return(return_opt) => self.parse_expression_type_opt(&return_opt),
            Statement::Yield(yield_opt) => self.parse_expression_type_opt(&yield_opt),
            Statement::Break => Ok(TypeStruct::new(None, Vec::new())),
            Statement::Continue => Ok(TypeStruct::new(None, Vec::new())),
            Statement::Use(_) => Ok(TypeStruct::new(None, Vec::new())),
            Statement::Package(_) => Ok(TypeStruct::new(None, Vec::new())),
            Statement::Throw(throw) => self.parse_expression_type_opt(&throw),
            Statement::Modifier(_) => Ok(TypeStruct::new(None, Vec::new())),
            Statement::Init => Ok(TypeStruct::new(None, Vec::new())),
            _ => panic!("Bad statement: {:?}", statement)
        }
    }

    fn parse_block_header_type(&mut self, block_header: &BlockHeader) -> CustomResult<()> {
        match block_header {
            BlockHeader::Default => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::Function(function) => self.parse_function_type(function),
            BlockHeader::Class(class) => self.parse_class_type(class),
            BlockHeader::Enum(_) => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::Interface(_) => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::Macro(_) => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::Constructor(_) => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::Destructor => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::If(expression_opt) => self.parse_expression_type_opt(expression_opt),
            BlockHeader::Else(Some(expression)) => self.parse_expression_type(expression),
            BlockHeader::Else(None) => self.parse_expression_type_opt(&expression_opt),
            BlockHeader::Match(expression_opt) => self.parse_expression_type_opt(expression_opt),
            BlockHeader::Defer => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::For(binary_operation_opt) => self.parse_binary_operation_type(binary_operation_opt),
            BlockHeader::While(expression_opt) => self.parse_expression_type_opt(expression_opt),
            BlockHeader::Loop => Ok(TypeStruct::new(None, Vec::new())),
            BlockHeader::With(expression_opt) => k(TypeStruct::new(None, Vec::new())),
            BlockHeader::Test(test) => Ok(TypeStruct::new(None, Vec::new())),
            _ => panic!("Bad block header: {:?}", block_header),
        }
    }

    fn parse_function_type(&mut self, function_opt: &Option<Function>) -> CustomResult<()> {
        if let Some(function) = function_opt {
            Ok(function.return_type.clone())
        } else {
            panic!("Bad function header?");
        }
    }

    fn parse_class_type(&mut self, class_opt: &Option<Class>) -> CustomResult<()> {
        if let Some(class) = class_opt {
            Ok(TypeStruct::new(Some(Type::Unknown(class.name.clone())), class.generics.clone()))
        } else {
            panic!("Bad class header?");
        }
    }
}
