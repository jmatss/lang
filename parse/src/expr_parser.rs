use crate::{
    parser::ParseTokenIter,
    token::{get_if_expr_op, Fix, Operator, Output},
};
use common::{
    error::CustomResult,
    r#type::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
    },
    token::{
        expr::{ArrayInit, Expr, FuncCall, StructInit},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
};
use lex::token::{LexTokenKind, Sym};
use log::debug;

pub struct ExprParser<'a, 'b> {
    iter: &'a mut ParseTokenIter<'b>,

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
    stop_conds: &'a [Sym],

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

impl<'a, 'b> ExprParser<'a, 'b> {
    pub fn parse(iter: &'a mut ParseTokenIter<'b>, stop_conds: &'a [Sym]) -> CustomResult<Expr> {
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
            } else if let LexTokenKind::Sym(ref symbol) = lex_token.kind {
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
            } else if let LexTokenKind::EOF = lex_token.kind {
                self.iter.rewind_skip_space()?;
                break;
            }

            self.token_count += 1;

            match lex_token.clone().kind {
                LexTokenKind::Ident(ident) => {
                    let expr = self.parse_expr_ident(&ident)?;
                    self.shunt_operand(expr)?;
                }

                LexTokenKind::Lit(lit) => {
                    let expr = Expr::Lit(lit, None);
                    self.shunt_operand(expr)?;
                }

                // Array access.
                LexTokenKind::Sym(Sym::ArrayIndexBegin) => {
                    // TODO: Will only using the square bracket end as a stop
                    //       symbol break anything? Inifinite loop?
                    let stop_conds = [Sym::SquareBracketEnd];
                    let expr = ExprParser::parse(self.iter, &stop_conds)?;

                    // Consume the "SquareBracketEnd".
                    self.iter.next_skip_space();

                    let op = Operator::UnaryOperator(UnOperator::ArrayAccess(Box::new(expr)));
                    self.shunt_operator(op)?;
                }

                // Array init. Example: "var x = [1, 2, 3]"
                LexTokenKind::Sym(Sym::SquareBracketBegin) => {
                    // The `parse_arg_list` function expects the start symbol
                    self.iter.rewind()?;

                    let start_symbol = Sym::SquareBracketBegin;
                    let end_symbol = Sym::SquareBracketEnd;
                    let args = self.iter.parse_arg_list(start_symbol, end_symbol)?;

                    let expr = Expr::ArrayInit(ArrayInit::new(args));
                    self.shunt_operand(expr)?;
                }

                // Skip any linebreaks if the linbreaks aren't a part of the
                // stop conditions.
                LexTokenKind::Sym(Sym::LineBreak) => self.token_count -= 1,

                // Special case for operators that takes a "type" as rhs.
                LexTokenKind::Sym(symbol @ Sym::Is)
                | LexTokenKind::Sym(symbol @ Sym::As)
                | LexTokenKind::Sym(symbol @ Sym::Of) => {
                    if let Some(op) = get_if_expr_op(&symbol) {
                        self.shunt_operator(op)?;
                        let expr = Expr::Type(self.iter.parse_type(None)?);

                        self.shunt_operand(expr)?;
                    } else {
                        return Err(self.iter.err(format!(
                            "Parsed None operator during expression for symbol: {:?}",
                            symbol
                        )));
                    }
                }

                LexTokenKind::Sym(symbol) => {
                    if let Some(op) = get_if_expr_op(&symbol) {
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
    fn shunt_operand(&mut self, expr: Expr) -> CustomResult<()> {
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
                    Operator::BinaryOperator(BinOperator::Addition)
                } else {
                    Operator::UnaryOperator(UnOperator::Positive)
                };
                self.add_operator(plus_op)?;
            }

            // Need extra logic for minus and plus since the can be both
            // binary and unary operators (x - -y).
            Operator::Minus => {
                let minus_op = if self.prev_was_operand {
                    Operator::BinaryOperator(BinOperator::Subtraction)
                } else {
                    Operator::UnaryOperator(UnOperator::Negative)
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

        // If the unary operator is postfix, it is assumed to be a part of the
        // previous operand.
        if let Operator::UnaryOperator(_) = op {
            if let Fix::Postfix = op_info.fix {
                self.prev_was_operand = true;
            }
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
    fn rev_polish_to_expr(&mut self) -> CustomResult<Expr> {
        let mut expr_stack = Vec::new();
        let outputs = std::mem::take(&mut self.outputs);

        for output in &outputs {
            match output.clone() {
                Output::Operand(expr) => {
                    expr_stack.push(expr);
                }

                Output::Operator(Operator::UnaryOperator(un_op)) => {
                    if let Some(expr) = expr_stack.pop() {
                        let op = UnOp::new(un_op, Box::new(expr));
                        expr_stack.push(Expr::Op(Op::UnOp(op)));
                    } else {
                        return Err(self
                            .iter
                            .err("Empty expr in expr_stack when popping (unary).".into()));
                    }
                }

                Output::Operator(Operator::BinaryOperator(bin_op)) => {
                    if let Some(right) = expr_stack.pop() {
                        if let Some(left) = expr_stack.pop() {
                            let op = BinOp::new(bin_op, Box::new(left), Box::new(right));
                            expr_stack.push(Expr::Op(Op::BinOp(op)));
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
                        "Bad match during rev_polish_to_expr with Output: {:?}. Outputs: {:#?}",
                        output, outputs
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
    fn parse_expr_ident(&mut self, ident: &str) -> CustomResult<Expr> {
        // TODO: The peek doesn't skip line break, so can't ex. do a struct
        //       init with a line break at the start.
        // The identifier will be either a function call, a type or a reference
        // to a variable.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                // Function call.
                LexTokenKind::Sym(Sym::ParenthesisBegin) => {
                    let start_symbol = Sym::ParenthesisBegin;
                    let end_symbol = Sym::ParenthesisEnd;
                    let arguments = self.iter.parse_arg_list(start_symbol, end_symbol)?;
                    let func_call = FuncCall::new(ident.into(), arguments);
                    Ok(Expr::FuncCall(func_call))
                }

                // TODO: Add possiblity to specify generic types.
                // Struct construction.
                LexTokenKind::Sym(Sym::CurlyBracketBegin) => {
                    let start_symbol = Sym::CurlyBracketBegin;
                    let end_symbol = Sym::CurlyBracketEnd;
                    let arguments = self.iter.parse_arg_list(start_symbol, end_symbol)?;
                    let struct_init = StructInit::new(ident.into(), arguments);
                    Ok(Expr::StructInit(struct_init))
                }

                // Static method call, this is the lhs type.
                LexTokenKind::Sym(Sym::DoubleColon) => Ok(Expr::Type(Ty::CompoundType(
                    InnerTy::UnknownIdent(ident.into(), self.iter.current_block_id()),
                    Generics::new(GenericsKind::Impl),
                ))),

                _ => {
                    // See if this is a type. It is a type if the previous
                    // operator was a binary operator that takes a type
                    // as the right hand side.
                    if let Some(last_op) = self.operators.last() {
                        match last_op {
                            Operator::BinaryOperator(BinOperator::As)
                            | Operator::BinaryOperator(BinOperator::Is)
                            | Operator::BinaryOperator(BinOperator::Of) => {
                                // Put back the old `lex_token` contaning this
                                // identifier and parse as type.
                                self.iter.rewind()?;
                                let ty = self.iter.parse_type(None)?;
                                return Ok(Expr::Type(ty));
                            }
                            _ => (),
                        }
                    }

                    let var = self.iter.parse_var_type(ident)?;
                    Ok(Expr::Var(var))
                }
            }
        } else {
            // TODO: Merge with logic above, same stuff.
            let var = self.iter.parse_var_type(ident)?;
            Ok(Expr::Var(var))
        }
    }
}
