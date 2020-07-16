#![feature(let_chains)]

use super::{
    iter::ParseTokenIter,
    token::{
        BinaryOperation, BinaryOperator, Expression, Fix, FunctionCall, Operation, Operator,
        Output, ParseToken, UnaryOperation, UnaryOperator, Variable,
    },
};
use crate::CustomError::ParseError;
use crate::{
    lex::token::{LexTokenKind, Symbol},
    CustomResult,
};

pub struct ExprParser<'a> {
    outputs: Vec<Output>,
    operators: Vec<Operator>,

    iter: &'a ParseTokenIter,

    /// The expression is parsed one token at a time until a token is found
    /// matching a symbol in `stop_conds`.
    stop_conds: &'a [Symbol],

    /// This bool is used to ensure that all "operands" are separated by operators.
    prev_was_operand: bool,

    /// Keeps a count of the parenthesis seen in the expression.
    /// This number gets incremented when a "ParenthesisBegin" is found and gets
    /// decremented when a "ParenthesisEnd" is found.
    ///
    /// This is used to help see if the end of the expression have been reached
    /// for example during function calls when the expression is wrapped inside
    /// parenthesis and it can be hard to tell if the parenthesis belongs to the
    /// expression or the function call.
    ///
    /// If this is set to a negative number, a "ParenthesisEnd" was found that
    /// didn't belong to this expression, assume end of expression.
    parenthesis_count: isize,
}

impl<'a> ExprParser<'a> {
    pub fn parse(iter: &'a ParseTokenIter, stop_conds: &'a [Symbol]) -> CustomResult<Expression> {
        let expr_parser = Self {
            outputs: Vec::new(),
            operators: Vec::new(),
            iter,
            stop_conds,
            prev_was_operand: false,
            parenthesis_count: 0,
        };

        expr_parser.shunting_yard()?;
        println!("Outputs: {:#?}", expr_parser.outputs);

        expr_parser.rev_polish_to_expr()
    }

    fn parse_expr_ident(&mut self, ident: &str) -> CustomResult<Expression> {
        // The identifier will be either a function call or a reference to
        // a variable.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                // Function call.
                LexTokenKind::Symbol(Symbol::ParenthesisBegin) => {
                    let func_call = self.parse_expr_call(&ident)?;
                    Ok(Expression::FunctionCall(func_call))
                }

                // Variable (most likely, will add more stuff here later so will
                // not always just be variables that is caight here.)
                _ => {
                    let var = self.parse_expr_var(ident)?;
                    Ok(Expression::Variable(self.parse_expr_var(ident)?))
                }
            }
        } else {
            Err(ParseError("Received None when parsing identifier.".into()))
        }
    }

    /// Parses a variable and any type that it might have specified after which
    /// would indicate a declaration.
    fn parse_expr_var(&mut self, ident: &str) -> CustomResult<Variable> {
        let mut var = Variable::new(ident.into());

        // If the next token is a colon, a type is specified after this variable.
        // Parse it and set the `var_type` inside the variable.
        if let Some(next_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Symbol(Symbol::Colon) = next_token.kind {
                self.iter.next_skip_space(); // Skip the colon.
                var.set_type(self.iter.parse_type()?);
            }
        }

        Ok(var)
    }

    /// Parses a function call.
    fn parse_expr_call(&mut self, ident: &str) -> CustomResult<FunctionCall> {
        let arguments = self.iter.parse_arg_list()?;
        Ok(FunctionCall::new(ident.into(), arguments))
    }

    /// See https://www.andr.mu/logs/the-shunting-yard-algorithm/ for a good
    /// explanation of the algorithm.
    fn shunting_yard(&mut self) -> CustomResult<()> {
        while let Some(lex_token) = self.iter.next_skip_space() {
            // Break and stop parsing expression if a Symbol contained in
            // `stop_conds` are found or if EOF is reached.
            // Also stop parsing if a "ParenthesisEnd" has been found that isn't
            // part of the expression (`self.parenthesis_count < 0`).
            if let LexTokenKind::Symbol(ref symbol) = lex_token.kind {
                if self.stop_conds.contains(symbol) {
                    self.iter.put_back(lex_token);
                    break;
                }
            } else if let LexTokenKind::EndOfFile = lex_token.kind {
                self.iter.put_back(lex_token);
                break;
            } else if self.parenthesis_count < 0 {
                self.iter.put_back(lex_token);
                break;
            }

            match lex_token.kind {
                LexTokenKind::Identifier(ident) => {
                    let expr = self.parse_expr_ident(&ident)?;
                    self.shunt_operand(expr)?;
                }

                LexTokenKind::Literal(lit) => {
                    let expr = Expression::Literal(lit, None);
                    self.shunt_operand(expr)?;
                }

                LexTokenKind::Symbol(symbol) => {
                    if let Some(op) = ParseToken::get_if_operator(symbol) {
                        self.shunt_operator(op)?;
                    } else {
                        return Err(ParseError(format!(
                            "Parsed None operator during expression for symbol: {:?}",
                            symbol
                        )));
                    }
                }

                _ => {
                    return Err(ParseError(format!(
                        "Parsed invalid token during expression: {:?}",
                        lex_token
                    )))
                }
            }
        }

        // Move the remaining `operators` to `outputs` before parsing the expression.
        for op in self.operators.into_iter().rev() {
            self.outputs.push(Output::Operator(op));
        }

        Ok(())
    }

    /// Adds a operand into the "shunting".
    fn shunt_operand(&mut self, expr: Expression) -> CustomResult<()> {
        if !self.prev_was_operand {
            self.outputs.push(Output::Operand(expr));
            self.prev_was_operand = true;
            Ok(())
        } else {
            Err(ParseError("Received two operands in a row.".into()))
        }
    }

    /// Adds a orpeator into the "shunting".
    fn shunt_operator(&mut self, op: Operator) -> CustomResult<()> {
        match op {
            Operator::ParenthesisBegin => {
                self.operators.push(op);
                self.parenthesis_count += 1;
            }

            Operator::ParenthesisEnd => {
                self.parenthesis_count -= 1;

                // If `parenthesis_count` is < 0, assume that the current
                // end parenthesis isn't part of the expression.
                if self.parenthesis_count < 0 {
                    while let Some(op_pop) = self.operators.pop() {
                        if let Operator::ParenthesisBegin = op_pop {
                            break;
                        } else {
                            self.outputs.push(Output::Operator(op_pop));
                        }
                    }
                }
                // TODO: Error handling of operator stack pop is None (?).
            }

            // Need extra logic for minus and plus since the can be both
            // binary and unary operators (x + +y).
            Operator::Plus => {
                let plus_op = if self.prev_was_operand {
                    Operator::BinaryOperator(BinaryOperator::Addition)
                } else {
                    Operator::UnaryOperator(UnaryOperator::Positive)
                };
                self.add_operator(plus_op);
            }

            // Need extra logic for minus and plus since the can be both
            // binary and unary operators (x - -y).
            Operator::Minus => {
                let minus_op = if self.prev_was_operand {
                    Operator::BinaryOperator(BinaryOperator::Subtraction)
                } else {
                    Operator::UnaryOperator(UnaryOperator::Negative)
                };
                self.add_operator(minus_op);
            }

            _ => {
                self.add_operator(op);
            }
        }

        Ok(())
    }

    /// Run logic for adding a Operator into the "shunting".
    fn add_operator(&mut self, op: Operator) -> CustomResult<()> {
        let op_info = op.info()?;

        self.prev_was_operand = false;

        // Unary operators are treated differently than binary operators,
        // they just get added directly to one of the stacks depending on
        // if it prefix or postfix. Early return.
        if let Operator::UnaryOperator(_) = op {
            if let Fix::Prefix = op_info.fix {
                self.operators.push(op);
            } else {
                self.outputs.push(Output::Operator(op));
                // If the operator is postfix, it is assumed to be a part of
                // the previous operand.
                self.prev_was_operand = true;
            }
            return Ok(());
        }

        // OBS!. The precedence is in reverse i.e. the higher the value of the
        //       `precedence`, the lower the precedence is.
        while let Some(pop_op) = self.operators.pop() {
            let pop_op_info = pop_op.info()?;

            let cond_one = pop_op_info.prec < op_info.prec;
            let cond_two = (pop_op_info.prec == op_info.prec) && pop_op_info.eval_ltor;
            if cond_one || cond_two {
                // 1.5.1.1  &  1.5.2.1
                self.outputs.push(Output::Operator(pop_op));
            } else {
                // 1.5.1.2  &  1.5.2.2
                self.operators.push(pop_op); // Put back popped operator.
                self.operators.push(op);
                break;
            }
        }

        Ok(())
    }

    // TODO: Should empty expression be allowed?
    /// Converts the given "outputs" in reverse polsih notation to an expression.
    fn rev_polish_to_expr(&mut self) -> CustomResult<Expression> {
        let mut expr_stack = Vec::new();

        for output in self.outputs {
            match output {
                Output::Operand(expr) => {
                    expr_stack.push(expr);
                }

                Output::Operator(Operator::UnaryOperator(unary_op)) => {
                    if let Some(expr) = expr_stack.pop() {
                        let op = UnaryOperation::new(unary_op, Box::new(expr));
                        expr_stack.push(Expression::Operation(Operation::UnaryOperation(op)));
                    } else {
                        return Err(ParseError(
                            "Empty expr in expr_stack when popping (unary).".into(),
                        ));
                    }
                }

                Output::Operator(Operator::BinaryOperator(bin_op)) => {
                    if let Some(right) = expr_stack.pop() {
                        if let Some(left) = expr_stack.pop() {
                            let op = BinaryOperation::new(bin_op, Box::new(left), Box::new(right));
                            expr_stack.push(Expression::Operation(Operation::BinaryOperation(op)));
                        } else {
                            return Err(ParseError(
                                "Empty expr in expr_stack when popping (binary left).".into(),
                            ));
                        }
                    } else {
                        return Err(ParseError(
                            "Empty expr in expr_stack when popping (binary right).".into(),
                        ));
                    }
                }

                _ => {
                    return Err(ParseError(format!(
                        "Bad match during rev_polish_to_expr with Output: {:?}",
                        output
                    )));
                }
            }
        }

        // When the loop above have finished, the remaining expression in the
        // `expr_stack` should be the final expression to be returned.
        if expr_stack.len() != 1 {
            return Err(ParseError(format!(
                "Not one expression left at end of rev_polish_to_expr, amount: {}",
                expr_stack.len()
            )));
        }

        Ok(expr_stack.remove(0))
    }
}
