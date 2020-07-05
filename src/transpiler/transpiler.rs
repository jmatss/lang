/*
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::token;
use crate::parser::token::{Token, BlockHeader, Expression, Operation};
use crate::CustomResult;
use crate::transpiler::symbols;

pub fn transpile(ast: &AST) -> CustomResult<Vec<String>> {
    let mut lines: Vec<String> = Vec::new();

    let root_block_data_opt = &ast.blocks[0].borrow().block_data;
    if let Some(root_block_data) = root_block_data_opt {

        for block in &root_block_data.children {
            match &block.borrow().token {
                Token::BlockHeader(block_header) => {
                    let mut result = transpile_block_header(block_header)?;
                    result.push_str(&format!(" {}", symbols::BLOCK_START));
                    lines.push(result);
                }
                _ => panic!("BAD BLOCK_HEADER.")
            }

            let block_data_opt = &block.borrow().block_data;
            if let Some(block_data) = block_data_opt {

                for tmp_child in &block_data.children {
                    let child = tmp_child.borrow();
                    let token = &child.token;

                    println!("TOKEN: {:?}", token);

                    match token {
                        Token::BlockHeader(block_header) => {
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

            } else {
                panic!("NO BLOCK_DATA!");
            }

            lines.push(symbols::BLOCK_END.to_string());
        }

    } else {
        panic!("NO CHILDREN IN ROOT_BLOCK.");
    }

    Ok(lines)
}

fn transpile_block_header(block_header: &BlockHeader) -> CustomResult<String> {
    let mut result = String::new();

    match block_header {
        BlockHeader::If(expr_opt) => {
            if let Some(expr) = expr_opt {
                result.push_str("if ");
                result.push_str("(");
                result.push_str(&transpile_expression(expr)?);
                result.push_str(")");
            } else {
                panic!("BAD EXPRESSION");
            }
        }
        BlockHeader::Else(expr_opt) => {
            if let Some(expr) = expr_opt {
                result.push_str("else if ");
                result.push_str("(");
                result.push_str(&transpile_expression(expr)?);
                result.push_str(")");
            } else {
                result.push_str("else");
            }
        }
        _ =>  panic!("")
    }

    Ok(result)
}

fn transpile_expression(expr: &Expression) -> CustomResult<String> {
    let mut result = String::new();

    match expr {
        Expression::Operation(op) => {
            result.push_str(&transpile_operation(op)?);
        }
        Expression::FunctionCall(func_call_opt) => {
            if let Some(func_call) = func_call_opt {
                result.push_str(&transpile_function_call(func_call)?);
            } else {
                panic!("BAD FUNC CALL.");
            }
        }
        Expression::Variable(var_opt) => {
            if let Some(var) = var_opt {
                result.push_str(&var.name);
            } else {
                panic!("BAD VARIABLE.");
            }
        },
        Expression::Literal(lit_opt) => {
            if let Some(lit) = lit_opt {
                result.push_str(&transpile_literal(lit)?);
            } else {
                panic!("BAD LITERAL!");
            }
        }
        Expression::Integer(int_opt) => {
            if let Some(int) = int_opt {
                result.push_str(int);
            } else {
                panic!("BAD INT!");
            }
        }
        _ => panic!("BAD TYPE: {:?}", expr)
    }

    Ok(result)
}

fn transpile_operation(op: &Operation) -> CustomResult<String> {
    let mut result = String::new();

    match op {
        Operation::BinaryOperation(bin_opt) => {
            if let Some(bin) = bin_opt {
                result.push_str(&transpile_binary_operation(bin)?);
            } else {
                panic!("BAD OPT.");
            }
        }
        Operation::UnaryOperation(bin) => {
            // TODO:
        }
        _ => {}
    }

    Ok(result)
}

fn transpile_binary_operation(bin: &token::BinaryOperation) -> CustomResult<String> {
    let mut result = String::new();

    result.push_str(&transpile_expression(&bin.left)?);

    let operator = match bin.operator {
        token::BinaryOperator::Equals => "==",
        token::BinaryOperator::NotEquals => "!=",
        _ => panic!("BAD OPERATOR.")
    };
    result.push_str(&format!(" {} ", operator));

    result.push_str(&transpile_expression(&bin.right)?);

    Ok(result)
}

fn transpile_function_call(func_call: &token::FunctionCall) -> CustomResult<String> {
    let mut result = String::new();

    result.push_str(&func_call.name);
    result.push_str("(");

    for arg in &func_call.arguments {
        result.push_str(&transpile_expression(&arg.value)?);
        result.push_str(symbols::LIST_SEP);
    }

    result.push_str(")");

    Ok(result)
}

fn transpile_literal(lit: &token::Literal) -> CustomResult<String> {
    let mut result = String::new();

    match lit {
        token::Literal::StringLiteral(lit_str) => {
            result.push_str(lit_str);
        }
        token::Literal::CharLiteral(lit_char) => {
            result.push_str(lit_char);
        }
        _ => panic!("BAD STRING OR CHAR LITERAL.")
    }

    Ok(result)
}
 */
