use crate::{
    parser::ParseTokenIter,
    token::{Fix, Operator, Output, ParseToken},
};
use common::{
    error::CustomResult,
    token::{
        expr::{Expression, FunctionCall, StructInit},
        op::{BinaryOperation, BinaryOperator, Operation, UnaryOperation, UnaryOperator},
    },
};
use lex::token::{LexTokenKind, Symbol};
use log::debug;

pub struct ExprParser<'a> {
    iter: &'a mut ParseTokenIter,

    /// Keeps a count of the amount of tokens seen. If no tokens have been seen
    /// before a "stop condition" is seen, something has gone wrong. This
    /// count is used to prevent a infinite loop if that error occures.
    token_count: usize,

    /// Containers used during the shunting yard algorithm to store operators
    /// and operand.
    outputs: Vec<Output>,
    operators: Vec<Operator>,

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
    /// didn't belong to this expression, assume end of expression. The extra
    /// "ParenthesisEnd" will be saved in the var `parenthesis_token` so that
    /// it can be put back into the iterator.
    parenthesis_count: isize,
}

impl<'a> ExprParser<'a> {
    pub fn parse(
        iter: &'a mut ParseTokenIter,
        stop_conds: &'a [Symbol],
    ) -> CustomResult<Expression> {
        let mut expr_parser = Self {
            iter,
            token_count: 0,
            outputs: Vec::new(),
            operators: Vec::new(),
            stop_conds,
            prev_was_operand: false,
            parenthesis_count: 0,
        };

        expr_parser.shunting_yard()?;
        debug!("Outputs: {:#?}", &expr_parser.outputs);

        expr_parser.rev_polish_to_expr()
    }

    /// See https://www.andr.mu/logs/the-shunting-yard-algorithm/ for a good
    /// explanation of the algorithm.
    fn shunting_yard(&mut self) -> CustomResult<()> {
        while let Some(lex_token) = self.iter.next_skip_space() {
            debug!("SHUNTING: {:?}", &lex_token);

            // Break and stop parsing expression if a Symbol contained in
            // `stop_conds` are found or if EOF is reached.
            // Also stop parsing if a "ParenthesisEnd" has been found that isn't
            // part of the expression (`self.parenthesis_count < 0`).
            if self.parenthesis_count < 0 {
                // Rewind to put back both the current `lex_token` and also to
                // put back the previous "ParenthesisEnd" that was removed.
                self.iter.rewind_skip_space()?;
                self.iter.rewind_skip_space()?;
                break;
            } else if let LexTokenKind::Symbol(ref symbol) = lex_token.kind {
                if self.stop_conds.contains(symbol) {
                    if self.token_count != 0 {
                        self.iter.rewind_skip_space()?;
                        break;
                    } else {
                        return Err(self
                            .iter
                            .err(format!("A `stop_cond` found before token: {:?}", symbol)));
                    }
                }
            } else if let LexTokenKind::EndOfFile = lex_token.kind {
                self.iter.rewind_skip_space()?;
                break;
            }

            self.token_count += 1;

            match lex_token.clone().kind {
                LexTokenKind::Identifier(ident) => {
                    let expr = self.parse_expr_ident(&ident)?;
                    self.shunt_operand(expr)?;
                }

                LexTokenKind::Literal(lit) => {
                    let expr = Expression::Literal(lit, None);
                    self.shunt_operand(expr)?;
                }

                // Array access.
                LexTokenKind::Symbol(Symbol::ArrayIndexBegin) => {
                    // TODO: Will only using the square bracket end as a stop
                    //       symbol break anything? Inifinite loop?
                    let stop_conds = [Symbol::SquareBracketEnd];
                    let expr = ExprParser::parse(self.iter, &stop_conds)?;

                    // Consume the "SquareBracketEnd".
                    self.iter.next_skip_space();

                    let op = Operator::UnaryOperator(UnaryOperator::ArrayAccess(Box::new(expr)));
                    self.shunt_operator(op)?;
                }

                // Array init. Example: "var x = [1, 2, 3]"
                LexTokenKind::Symbol(Symbol::SquareBracketBegin) => {
                    // The `parse_arg_list` function expects the start symbol
                    self.iter.rewind()?;

                    let start_symbol = Symbol::SquareBracketBegin;
                    let end_symbol = Symbol::SquareBracketEnd;
                    let args = self.iter.parse_arg_list(start_symbol, end_symbol)?;

                    let expr = Expression::ArrayInit(args, None);
                    self.shunt_operand(expr)?;
                }

                // Special case for operators that takes a "type" as rhs.
                LexTokenKind::Symbol(symbol @ Symbol::Is)
                | LexTokenKind::Symbol(symbol @ Symbol::As)
                | LexTokenKind::Symbol(symbol @ Symbol::Of) => {
                    if let Some(op) = ParseToken::get_if_expr_op(&symbol) {
                        self.shunt_operator(op)?;
                        let expr = Expression::Type(self.iter.parse_type()?);

                        self.shunt_operand(expr)?;
                    } else {
                        return Err(self.iter.err(format!(
                            "Parsed None operator during expression for symbol: {:?}",
                            symbol
                        )));
                    }
                }

                LexTokenKind::Symbol(symbol) => {
                    if let Some(op) = ParseToken::get_if_expr_op(&symbol) {
                        self.shunt_operator(op)?;
                    } else {
                        return Err(self.iter.err(format!(
                            "Parsed None operator during expression for symbol: {:?}",
                            symbol
                        )));
                    }
                }

                _ => {
                    return Err(self.iter.err(format!(
                        "Parsed invalid token during expression: {:?}",
                        lex_token
                    )));
                }
            }
        }

        // Move the remaining `operators` to `outputs` before parsing the expression.
        for op in std::mem::take(&mut self.operators).into_iter().rev() {
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
            Err(self
                .iter
                .err("Received two operands in a row (or a postfix operator).".into()))
        }
    }

    /// Adds a orpeator into the "shunting".
    fn shunt_operator(&mut self, op: Operator) -> CustomResult<()> {
        match op {
            Operator::ParenthesisBegin => {
                self.parenthesis_count += 1;
                self.operators.push(op);
            }

            Operator::ParenthesisEnd => {
                self.parenthesis_count -= 1;

                // If `parenthesis_count` is < 0, assume that the current
                // end parenthesis isn't part of the expression.
                // This condition will be checked at the start of the next
                // iteration.
                if self.parenthesis_count >= 0 {
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
                self.add_operator(plus_op)?;
            }

            // Need extra logic for minus and plus since the can be both
            // binary and unary operators (x - -y).
            Operator::Minus => {
                let minus_op = if self.prev_was_operand {
                    Operator::BinaryOperator(BinaryOperator::Subtraction)
                } else {
                    Operator::UnaryOperator(UnaryOperator::Negative)
                };
                self.add_operator(minus_op)?;
            }

            _ => {
                self.add_operator(op)?;
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
                //self.operators.push(op.clone());
            } else {
                // TODO: Is it ok to treat PostFix the same way as PreFix
                //       to try and get them to care about precedence?
                //self.outputs.push(Output::Operator(op));
                //self.operators.push(op.clone());

                // If the operator is postfix, it is assumed to be a part of
                // the previous operand.
                self.prev_was_operand = true;
            }
            //return Ok(());
        }

        // OBS!. The precedence is in reverse i.e. the higher the value of the
        //       `precedence`, the lower the precedence is.
        while let Some(pop_op) = self.operators.pop() {
            let pop_op_info = pop_op.info()?;

            // Edge case for "ParenthesisBegin" since it is special and doesn't
            // follow the regular rules. If it is found here, the "popping loop"
            // should end as in "1.5.1.2" & "1.5.2.2".
            if let Operator::ParenthesisBegin = pop_op {
                self.operators.push(pop_op); // Put back popped operator.
                break;
            }

            let cond_one = pop_op_info.prec < op_info.prec;
            let cond_two = (pop_op_info.prec == op_info.prec) && pop_op_info.eval_ltor;
            if cond_one || cond_two {
                // 1.5.1.1  &  1.5.2.1
                self.outputs.push(Output::Operator(pop_op));
            } else {
                // 1.5.1.2  &  1.5.2.2
                self.operators.push(pop_op); // Put back popped operator.
                break;
            }
        }

        self.operators.push(op);

        Ok(())
    }

    // TODO: Should empty expression be allowed?
    /// Converts the given "outputs" in reverse polsih notation to an expression.
    fn rev_polish_to_expr(&mut self) -> CustomResult<Expression> {
        let mut expr_stack = Vec::new();

        for output in std::mem::take(&mut self.outputs) {
            match output {
                Output::Operand(expr) => {
                    expr_stack.push(expr);
                }

                Output::Operator(Operator::UnaryOperator(un_op)) => {
                    if let Some(expr) = expr_stack.pop() {
                        let op = UnaryOperation::new(un_op, Box::new(expr));
                        expr_stack.push(Expression::Operation(Operation::UnaryOperation(op)));
                    } else {
                        return Err(self
                            .iter
                            .err("Empty expr in expr_stack when popping (unary).".into()));
                    }
                }

                Output::Operator(Operator::BinaryOperator(bin_op)) => {
                    if let Some(right) = expr_stack.pop() {
                        if let Some(left) = expr_stack.pop() {
                            let op = BinaryOperation::new(bin_op, Box::new(left), Box::new(right));
                            expr_stack.push(Expression::Operation(Operation::BinaryOperation(op)));
                        } else {
                            return Err(self.iter.err(
                                "Empty expr in expr_stack when popping (binary left).".into(),
                            ));
                        }
                    } else {
                        return Err(self
                            .iter
                            .err("Empty expr in expr_stack when popping (binary right).".into()));
                    }
                }

                _ => {
                    return Err(self.iter.err(format!(
                        "Bad match during rev_polish_to_expr with Output: {:?}",
                        output
                    )));
                }
            }
        }

        // When the loop above have finished, the remaining expression in the
        // `expr_stack` should be the final expression to be returned.
        if expr_stack.len() != 1 {
            return Err(self.iter.err(format!(
                "Not one expression left at end of rev_polish_to_expr, amount: {}",
                expr_stack.len()
            )));
        }

        Ok(expr_stack.remove(0))
    }

    // TODO: Seems like this gives incorrect column when parsed in some way.
    fn parse_expr_ident(&mut self, ident: &str) -> CustomResult<Expression> {
        // TODO: The peek doesn't skip line break, so can't ex. do a struct
        //       init with a line break at the start.
        // The identifier will be either a function call or a reference to
        // a variable.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                // Function call.
                LexTokenKind::Symbol(Symbol::ParenthesisBegin) => {
                    let start_symbol = Symbol::ParenthesisBegin;
                    let end_symbol = Symbol::ParenthesisEnd;
                    let arguments = self.iter.parse_arg_list(start_symbol, end_symbol)?;
                    let func_call = FunctionCall::new(ident.into(), arguments);
                    Ok(Expression::FunctionCall(func_call))
                }

                // Struct construction.
                LexTokenKind::Symbol(Symbol::CurlyBracketBegin) => {
                    let start_symbol = Symbol::CurlyBracketBegin;
                    let end_symbol = Symbol::CurlyBracketEnd;
                    let arguments = self.iter.parse_arg_list(start_symbol, end_symbol)?;
                    let struct_init = StructInit::new(ident.into(), arguments);
                    Ok(Expression::StructInit(struct_init))
                }

                _ => {
                    // See if this is a type. It is a type if the previous
                    // operator was a binary operator that takes a type
                    // as the right hand side.
                    if let Some(last_op) = self.operators.last() {
                        match last_op {
                            Operator::BinaryOperator(BinaryOperator::As)
                            | Operator::BinaryOperator(BinaryOperator::Is)
                            | Operator::BinaryOperator(BinaryOperator::Of) => {
                                // Put back the old `lex_token` contaning this
                                // identifier and parse as type.
                                self.iter.rewind()?;
                                let ty = self.iter.parse_type()?;
                                return Ok(Expression::Type(ty));
                            }
                            _ => (),
                        }
                    }

                    let var = self.iter.parse_var_type(ident)?;
                    Ok(Expression::Variable(var))
                }
            }
        } else {
            // TODO: Merge with logic above, same stuff.
            let var = self.iter.parse_var_type(ident)?;
            Ok(Expression::Variable(var))
        }
    }
}
