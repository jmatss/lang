use crate::parser::abstract_syntax_tree::RCBlock;
use crate::parser::abstract_syntax_tree::{RCNode, AST};
use crate::parser::token;
use crate::parser::token::{BlockHeader, Expression, Operation, Token};
use crate::transpiler::symbols;
use crate::{analyzer::analyzer::AnalyzeContext, CustomResult};

pub struct Transpiler<'a> {
    /// Will contains the transpiled source code.
    lines: Vec<String>,

    /// Contains the AST parsed during parsing and updated during analyzing.
    ast: &'a AST,

    /// Contains the "context" parsed during analyzing.
    context: &'a AnalyzeContext,
}

pub fn transpile(ast: &AST, context: &AnalyzeContext) -> CustomResult<Vec<String>> {
    let mut transpiler = Transpiler::new(ast, context);

    let root_block = transpiler.ast.blocks[0].clone();
    transpiler.transpile_recursive(&root_block)?;

    Ok(std::mem::take(&mut transpiler.lines))
}

impl<'a> Transpiler<'a> {
    fn new(ast: &'a AST, context: &'a AnalyzeContext) -> Self {
        Self {
            lines: Vec::new(),
            ast,
            context,
        }
    }

    fn transpile_recursive(&mut self, block: &RCBlock) -> CustomResult<()> {
        let n = 4;

        for child in &block.borrow().children {
            let indent_level: usize;
            let token = match child {
                RCNode::Block(rc_block) => {
                    indent_level = rc_block.borrow().indent_level;
                    rc_block.borrow().token.clone()
                }
                RCNode::Token(rc_token) => {
                    indent_level = rc_token.borrow().indent_level;
                    rc_token.borrow().token.clone()
                }
            };

            println!("TOKEN: {:?}", token);

            let mut result_line = match token {
                Token::BlockHeader(ref block_header) => {
                    // TODO: FIXME: Temporary let else be caught in here
                    // since they are special case.
                    let mut result = self.transpile_block_header(block_header)?;
                    result.push_str(&format!(" {}", symbols::BLOCK_START));
                    result
                }
                Token::Expression(ref expr) => {
                    let mut result = self.transpile_expression(expr)?;
                    result.push_str(symbols::EXPR_BRAKE);
                    result
                }
                Token::Statement(x) => {
                    println!("{:?}", x);
                    String::new()
                }
                _ => String::new(),
            };

            result_line.insert_str(0, &" ".repeat(indent_level * n));
            self.lines.push(result_line);

            if let RCNode::Block(rc_block) = child {
                self.transpile_recursive(rc_block)?;
                let block_end = format!("{}{}", " ".repeat(indent_level * n), symbols::BLOCK_END);
                self.lines.push(block_end);
            }
        }

        Ok(())
    }

    fn transpile_block_header(&mut self, block_header: &BlockHeader) -> CustomResult<String> {
        let mut result = String::new();

        match block_header {
            BlockHeader::If(expr_opt) => {
                if let Some(expr) = expr_opt {
                    result.push_str("if ");
                    result.push_str("(");
                    result.push_str(&self.transpile_expression(expr)?);
                    result.push_str(")");
                } else {
                    panic!("BAD EXPRESSION");
                }
            }
            BlockHeader::Else(expr_opt) => {
                if let Some(expr) = expr_opt {
                    result.push_str("else if ");
                    result.push_str("(");
                    result.push_str(&self.transpile_expression(expr)?);
                    result.push_str(")");
                } else {
                    result.push_str("else");
                }
            }
            _ => panic!(""),
        }

        Ok(result)
    }

    fn transpile_expression(&self, expr: &Expression) -> CustomResult<String> {
        let mut result = String::new();

        match expr {
            Expression::Operation(op) => {
                result.push_str(&self.transpile_operation(op)?);
            }
            Expression::FunctionCall(func_call_opt) => {
                if let Some(func_call) = func_call_opt {
                    result.push_str(&self.transpile_function_call(func_call)?);
                } else {
                    panic!("BAD FUNC CALL.");
                }
            }
            Expression::MacroCall(macro_call_opt) => {
                if let Some(macro_call) = macro_call_opt {
                    result.push_str(&self.transpile_macro_call(macro_call)?);
                } else {
                    panic!("BAD MACRO CALL.");
                }
            }
            Expression::Variable(variable) => {
                result.push_str(&variable.name);
            }
            Expression::Literal(literal) => {
                result.push_str(&self.transpile_literal(literal)?);
            }
            Expression::Integer(int_literal, _int_literal_type) => {
                result.push_str(int_literal);
            }
            _ => panic!("BAD TYPE: {:?}", expr),
        }

        Ok(result)
    }

    fn transpile_operation(&self, op: &Operation) -> CustomResult<String> {
        let mut result = String::new();

        match op {
            Operation::BinaryOperation(binary_opt) => {
                if let Some(binary) = binary_opt {
                    result.push_str(&self.transpile_binary_operation(binary)?);
                } else {
                    panic!("BAD BINARY OPT.");
                }
            }
            Operation::UnaryOperation(unary_opt) => {
                if let Some(unary) = unary_opt {
                    result.push_str(&self.transpile_unary_operation(unary)?);
                } else {
                    panic!("BAD UNARY OPT.");
                }
            }
            _ => {}
        }

        Ok(result)
    }

    fn transpile_binary_operation(&self, binary: &token::BinaryOperation) -> CustomResult<String> {
        let left = &self.transpile_expression(&binary.left)?;
        let right = &self.transpile_expression(&binary.right)?;

        Ok(match binary.operator {
            token::BinaryOperator::Assignment => format!("{} = {}", left, right),
            token::BinaryOperator::In => panic!("TODO: In"),
            token::BinaryOperator::Is => panic!("TODO: Is"),
            token::BinaryOperator::As => format!("(({}){})", right, left),
            token::BinaryOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            token::BinaryOperator::Range => panic!("TODO: Range"),
            token::BinaryOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            token::BinaryOperator::MatchCase => panic!("TODO: matchCase, how to implement?"),
            token::BinaryOperator::Dot => format!("({}.{})", left, right),

            token::BinaryOperator::Equals => format!("({} == {})", left, right),
            token::BinaryOperator::NotEquals => format!("({} != {})", left, right),
            token::BinaryOperator::LessThan => format!("({} < {})", left, right),
            token::BinaryOperator::GreaterThan => format!("({} > {})", left, right),
            token::BinaryOperator::LessThanOrEquals => format!("({} <= {})", left, right),
            token::BinaryOperator::GreaterThanOrEquals => format!("({} >= {})", left, right),

            token::BinaryOperator::Addition => format!("({} + {})", left, right),
            token::BinaryOperator::Subtraction => format!("({} - {})", left, right),
            token::BinaryOperator::Multiplication => format!("({} * {})", left, right),
            token::BinaryOperator::Division => format!("({} / {})", left, right),
            token::BinaryOperator::Modulus => format!("({} % {})", left, right),
            token::BinaryOperator::Power => panic!("TODO: Power"),

            token::BinaryOperator::BitAnd => format!("({} & {})", left, right),
            token::BinaryOperator::BitOr => format!("({} | {})", left, right),
            token::BinaryOperator::BitXor => format!("({} ^ {})", left, right),
            token::BinaryOperator::ShiftLeft => format!("({} << {})", left, right),
            token::BinaryOperator::ShiftRight => format!("({} >> {})", left, right),

            token::BinaryOperator::BoolAnd => format!("({} && {})", left, right),
            token::BinaryOperator::BoolOr => format!("({} || {})", left, right),

            token::BinaryOperator::AssignAddition => format!("{} += {}", left, right),
            token::BinaryOperator::AssignSubtraction => format!("{} -= {}", left, right),
            token::BinaryOperator::AssignMultiplication => format!("{} *= {}", left, right),
            token::BinaryOperator::AssignDivision => format!("{} /= {}", left, right),
            token::BinaryOperator::AssignModulus => format!("{} %= {}", left, right),
            token::BinaryOperator::AssignPower => panic!("TODO: AssignPower"),

            token::BinaryOperator::AssignBitAnd => format!("{} &= {}", left, right),
            token::BinaryOperator::AssignBitOr => format!("{} |= {}", left, right),
            token::BinaryOperator::AssignBitXor => format!("{} ^= {}", left, right),
            token::BinaryOperator::AssignShiftLeft => format!("{} <<= {}", left, right),
            token::BinaryOperator::AssignShiftRight => format!("{} >>= {}", left, right),

            token::BinaryOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
        })
    }

    fn transpile_unary_operation(&self, unary: &token::UnaryOperation) -> CustomResult<String> {
        let expr = &self.transpile_expression(&unary.value)?;

        Ok(match unary.operator {
            token::UnaryOperator::IncrementPrefix => format!("(++{})", expr),
            token::UnaryOperator::IncrementPostfix => format!("({}++)", expr),
            token::UnaryOperator::DecrementPrefix => format!("(--{})", expr),
            token::UnaryOperator::DecrementPostfix => format!("({}--)", expr),

            token::UnaryOperator::Deref => format!("(*{})", expr),
            token::UnaryOperator::Address => format!("(&{})", expr),

            token::UnaryOperator::Positive => format!("(+{})", expr),
            token::UnaryOperator::Negative => format!("(-{})", expr),

            token::UnaryOperator::BitCompliment => format!("(~{})", expr),

            token::UnaryOperator::BoolNot => format!("(!{})", expr),
        })
    }

    fn transpile_function_call(&self, func_call: &token::FunctionCall) -> CustomResult<String> {
        let mut result = String::new();

        result.push_str(&func_call.name);
        result.push_str("(");

        let mut args = Vec::new();
        for arg in &func_call.arguments {
            args.push(self.transpile_expression(&arg.value)?);
        }
        result.push_str(&args.join(symbols::LIST_SEP));

        result.push_str(")");

        Ok(result)
    }

    fn transpile_macro_call(&self, macro_call: &token::MacroCall) -> CustomResult<String> {
        let mut result = String::new();

        result.push_str(&macro_call.name);
        result.push_str("(");

        let mut args = Vec::new();
        for arg in &macro_call.arguments {
            args.push(self.transpile_expression(&arg.value)?);
        }
        result.push_str(&args.join(symbols::LIST_SEP));

        result.push_str(")");

        Ok(result)
    }

    fn transpile_literal(&self, lit: &token::Literal) -> CustomResult<String> {
        let mut result = String::new();

        match lit {
            token::Literal::StringLiteral(lit_str) => {
                result.push_str(&format!("\"{}\"", lit_str));
            }
            token::Literal::CharLiteral(lit_char) => {
                result.push_str(&format!("\'{}\'", lit_char));
            }
            _ => panic!("BAD STRING OR CHAR LITERAL."),
        }

        Ok(result)
    }
}
