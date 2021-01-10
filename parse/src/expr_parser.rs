use crate::{
    parser::ParseTokenIter,
    token::{get_if_expr_op, Fix, Operator, Output},
    type_parser::TypeParser,
};
use common::{
    error::CustomResult,
    file::FilePosition,
    token::{
        expr::{ArrayInit, BuiltInCall, Expr, FuncCall, StructInit},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
    },
    type_info::TypeInfo,
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
    /// didn't belong to this expression, assume end of expression.
    parenthesis_count: isize,
}

impl<'a, 'b> ExprParser<'a, 'b> {
    /// If the expression was empty (was no expression), returns None.
    pub fn parse(
        iter: &'a mut ParseTokenIter<'b>,
        stop_conds: &'a [Sym],
    ) -> CustomResult<Option<Expr>> {
        // Take the file pos of the first symbol of the expression if possible.
        // This will be used if a more accurate/precise file_pos can't be used.
        let file_pos = iter.peek_file_pos()?;

        let mut expr_parser = Self {
            iter,
            token_count: 0,
            outputs: Vec::new(),
            operators: Vec::new(),
            stop_conds,
            prev_was_operand: false,
            parenthesis_count: 0,
        };

        match expr_parser.shunting_yard()? {
            // (was_empty, expr_file_pos_opt)
            (false, Some(file_pos)) => {
                debug!("Outputs: {:#?}", &expr_parser.outputs);
                expr_parser.rev_polish_to_expr(&file_pos).map(Some)
            }

            (true, _) => Ok(None),

            (false, None) => Err(expr_parser.iter.err(
                "Expr wasn't empty, but got back None expr_file_pos.".into(),
                Some(file_pos),
            )),
        }
    }

    /// See https://www.andr.mu/logs/the-shunting-yard-algorithm/ for a good
    /// explanation of the algorithm.
    /// Return `true` if the expression was empty.
    fn shunting_yard(&mut self) -> CustomResult<(bool, Option<FilePosition>)> {
        let mark = self.iter.mark();

        let mut file_pos = self.iter.peek_file_pos()?;

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

                file_pos.set_end(&self.iter.peek_file_pos()?)?;
                break;
            } else if let LexTokenKind::Sym(ref symbol) = lex_token.kind {
                if self.stop_conds.contains(symbol) {
                    // Rewind the iter so that the stop condition symbol is left.
                    self.iter.rewind_skip_space()?;

                    let was_empty = self.token_count == 0;
                    if was_empty {
                        // Edge case if this is an empty expression. Do not break
                        // and solve the parsed expressions, do a early return
                        // instead.
                        return Ok((was_empty, None));
                    } else {
                        file_pos.set_end(&self.iter.peek_file_pos()?)?;
                        break;
                    }
                }
            } else if let LexTokenKind::EOF = lex_token.kind {
                self.iter.rewind_skip_space()?;

                file_pos.set_end(&self.iter.peek_file_pos()?)?;
                break;
            }

            self.token_count += 1;

            match lex_token.clone().kind {
                LexTokenKind::Ident(ident) => {
                    let expr = self.parse_expr_ident(&ident, lex_token.file_pos)?;

                    if let Some(expr_file_pos) = expr.file_pos() {
                        file_pos.set_end(expr_file_pos)?;
                    } else {
                        unreachable!();
                    }

                    self.shunt_operand(expr)?;
                }

                LexTokenKind::Lit(lit) => {
                    let expr = Expr::Lit(lit, None, Some(lex_token.file_pos));

                    if let Some(expr_file_pos) = expr.file_pos() {
                        file_pos.set_end(expr_file_pos)?;
                    } else {
                        unreachable!();
                    }

                    self.shunt_operand(expr)?;
                }

                // "Built-in" function call.
                LexTokenKind::Sym(Sym::At) => {
                    // `next_token` should contain the ident.
                    let next_token = self.iter.next_skip_space();

                    // TODO: Most of the logic below is taken from `self.parse_expr_ident`.
                    //       Merge the logic.
                    let generics = if let Some(lex_token) = self.iter.peek_skip_space() {
                        if let LexTokenKind::Sym(Sym::PointyBracketBegin) = lex_token.kind {
                            match TypeParser::new(self.iter, None)
                                .parse_type_generics(GenericsKind::Impl)
                            {
                                Ok((generics, _)) => Some(generics),
                                Err(_) => {
                                    self.iter.rewind_to_mark(mark);
                                    None
                                }
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    if let Some(LexTokenKind::Ident(ident)) = next_token.clone().map(|t| t.kind) {
                        let start_symbol = Sym::ParenthesisBegin;
                        let end_symbol = Sym::ParenthesisEnd;
                        let (arguments, args_file_pos) =
                            self.iter.parse_arg_list(start_symbol, end_symbol)?;

                        let mut built_in_file_pos = lex_token.file_pos;
                        built_in_file_pos.set_end(&args_file_pos)?;
                        file_pos.set_end(&args_file_pos)?;

                        let built_in_call =
                            BuiltInCall::new(ident, arguments, generics, lex_token.file_pos);
                        self.shunt_operand(Expr::BuiltInCall(built_in_call))?;
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Expected ident after '@' (built-in call), got: {:?}",
                                next_token
                            ),
                            Some(lex_token.file_pos),
                        ));
                    }
                }

                // Array access.
                LexTokenKind::Sym(Sym::ArrayIndexBegin) => {
                    // TODO: Will only using the square bracket end as a stop
                    //       symbol break anything? Inifinite loop?
                    let stop_conds = [Sym::SquareBracketEnd];

                    let expr = if let Some(expr) = ExprParser::parse(self.iter, &stop_conds)? {
                        expr
                    } else {
                        return Err(self.iter.err(
                            "Found no expression in array indexing.".into(),
                            Some(lex_token.file_pos),
                        ));
                    };

                    // Consume the "SquareBracketEnd".
                    if let Some(end_token) = self.iter.next_skip_space() {
                        file_pos.set_end(&end_token.file_pos)?;
                    } else {
                        unreachable!();
                    }

                    let op = Operator::UnaryOperator(UnOperator::ArrayAccess(Box::new(expr)));

                    self.shunt_operator(op)?;
                }

                // Array init. Example: "var x = [1, 2, 3]"
                LexTokenKind::Sym(Sym::SquareBracketBegin) => {
                    // The `parse_arg_list` function expects the start symbol
                    self.iter.rewind_to_mark(mark);

                    let start_symbol = Sym::SquareBracketBegin;
                    let end_symbol = Sym::SquareBracketEnd;
                    let (args, args_file_pos) =
                        self.iter.parse_arg_list(start_symbol, end_symbol)?;

                    let mut array_init_file_pos = lex_token.file_pos;
                    array_init_file_pos.set_end(&args_file_pos)?;

                    let expr = Expr::ArrayInit(ArrayInit::new(args, array_init_file_pos));

                    if let Some(expr_file_pos) = expr.file_pos() {
                        file_pos.set_end(expr_file_pos)?;
                    } else {
                        unreachable!("Expr doesn't have a file_pos: {:#?}", expr);
                    }

                    self.shunt_operand(expr)?;
                }

                // Skip any linebreaks if the linbreaks aren't a part of the
                // stop conditions. Also skip comments.
                LexTokenKind::Sym(Sym::LineBreak) | LexTokenKind::Comment(..) => {
                    file_pos.set_end(&lex_token.file_pos)?;
                    self.token_count -= 1
                }

                // Special case for operators that takes a "type" as rhs.
                LexTokenKind::Sym(symbol @ Sym::Is)
                | LexTokenKind::Sym(symbol @ Sym::As)
                | LexTokenKind::Sym(symbol @ Sym::Of) => {
                    if let Some(op) = get_if_expr_op(&symbol) {
                        self.shunt_operator(op)?;
                        let ty_expr = self.iter.parse_type(None)?;
                        let ty_expr_file_pos = ty_expr.file_pos().cloned();
                        let expr = Expr::Type(ty_expr, ty_expr_file_pos);

                        if let Some(expr_file_pos) = expr.file_pos() {
                            file_pos.set_end(expr_file_pos)?;
                        } else {
                            unreachable!("Expr doesn't have a file_pos: {:#?}", expr);
                        }

                        self.shunt_operand(expr)?;
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Parsed None operator during expression for symbol: {:?}",
                                symbol
                            ),
                            Some(lex_token.file_pos),
                        ));
                    }
                }

                LexTokenKind::Sym(symbol) => {
                    if let Some(op) = get_if_expr_op(&symbol) {
                        file_pos.set_end(&lex_token.file_pos)?;
                        self.shunt_operator(op)?;
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Parsed None operator during expression for symbol: {:?}",
                                symbol
                            ),
                            Some(lex_token.file_pos),
                        ));
                    }
                }

                _ => {
                    return Err(self.iter.err(
                        format!("Parsed invalid token during expression: {:?}", lex_token),
                        Some(lex_token.file_pos),
                    ));
                }
            }
        }

        // Move the remaining `operators` to `outputs` before parsing the expression.
        for op in std::mem::take(&mut self.operators).into_iter().rev() {
            self.outputs.push(Output::Operator(op));
        }

        let was_empty = false;
        Ok((was_empty, Some(file_pos)))
    }

    /// Adds a operand into the "shunting".
    fn shunt_operand(&mut self, expr: Expr) -> CustomResult<()> {
        if !self.prev_was_operand {
            self.outputs.push(Output::Operand(expr));
            self.prev_was_operand = true;
            Ok(())
        } else {
            // Need to rewind twice to get to the previous expr. The first rewind
            // gets the current expr again. Also make sure to "forward" the iter
            // again to the end position.
            self.iter.rewind_skip_space()?;
            self.iter.rewind_skip_space()?;

            let prev_operand = self.iter.next_skip_space().unwrap();
            self.iter.next_skip_space(); // Skip cur expr.

            let mut err_file_pos = prev_operand.file_pos.to_owned();
            err_file_pos.set_end(expr.file_pos().unwrap())?;

            Err(self.iter.err(
                format!(
                    "Found two operands in a row when parsing expr. Prev: {:#?}, cur: {:#?}",
                    prev_operand, expr
                ),
                Some(err_file_pos),
            ))
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
    fn rev_polish_to_expr(&mut self, full_expr_file_pos: &FilePosition) -> CustomResult<Expr> {
        let mut expr_stack = Vec::new();
        let outputs = std::mem::take(&mut self.outputs);

        let mut prev_file_pos = Some(full_expr_file_pos.to_owned());

        for output in &outputs {
            match output.clone() {
                Output::Operand(expr) => {
                    prev_file_pos = expr.file_pos().cloned();
                    expr_stack.push(expr);
                }

                Output::Operator(Operator::UnaryOperator(un_op)) => {
                    if let Some(expr) = expr_stack.pop() {
                        let expr_file_pos = expr.file_pos().cloned();
                        prev_file_pos = expr_file_pos.to_owned();

                        let op = UnOp::new(un_op, Box::new(expr), expr_file_pos);
                        expr_stack.push(Expr::Op(Op::UnOp(op)));
                    } else {
                        return Err(self.iter.err(
                            "Empty expr in expr_stack when popping (unary).".into(),
                            prev_file_pos,
                        ));
                    }
                }

                Output::Operator(Operator::BinaryOperator(bin_op)) => {
                    if let Some(right) = expr_stack.pop() {
                        let right_file_pos = if let Some(right_file_pos) = right.file_pos() {
                            right_file_pos.to_owned()
                        } else {
                            unreachable!()
                        };

                        if let Some(left) = expr_stack.pop() {
                            let left_file_pos = if let Some(left_file_pos) = left.file_pos() {
                                left_file_pos.to_owned()
                            } else {
                                unreachable!()
                            };

                            let mut bin_op_file_pos = left_file_pos.to_owned();
                            bin_op_file_pos.set_end(&right_file_pos)?;

                            prev_file_pos = Some(bin_op_file_pos.to_owned());

                            let op = BinOp::new(
                                bin_op,
                                Box::new(left),
                                Box::new(right),
                                Some(bin_op_file_pos),
                            );
                            expr_stack.push(Expr::Op(Op::BinOp(op)));
                        } else {
                            return Err(self.iter.err(
                                "Empty expr in expr_stack when popping (binary left).".into(),
                                right.file_pos().cloned(),
                            ));
                        }
                    } else {
                        return Err(self.iter.err(
                            "Empty expr in expr_stack when popping (binary right).".into(),
                            prev_file_pos,
                        ));
                    }
                }

                _ => {
                    return Err(self.iter.err(
                        format!(
                            "Bad match during rev_polish_to_expr with Output: {:?}. Outputs: {:#?}",
                            output, outputs
                        ),
                        prev_file_pos,
                    ));
                }
            }
        }

        // When the loop above have finished, the remaining expression in the
        // `expr_stack` should be the final expression to be returned.
        if expr_stack.len() != 1 {
            return Err(self.iter.err(
                format!(
                    "Not one expression left at end of rev_polish_to_expr, amount: {}",
                    expr_stack.len()
                ),
                Some(full_expr_file_pos.to_owned()),
            ));
        }

        Ok(expr_stack.remove(0))
    }

    fn parse_expr_ident(&mut self, ident: &str, mut file_pos: FilePosition) -> CustomResult<Expr> {
        // If the identifier is followed by a "PointyBracketBegin" it can either
        // be a start of a generic list for structures/function, or it can also
        // be a "LessThan" compare operation. Try to parse it as a generic list,
        // if it fails assume that it is a lt compare.
        let mut mark = self.iter.mark();

        let generics = if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Sym(Sym::PointyBracketBegin) = lex_token.kind {
                match TypeParser::new(self.iter, None).parse_type_generics(GenericsKind::Impl) {
                    Ok((generics, Some(tmp_file_pos))) => {
                        file_pos.set_end(&tmp_file_pos)?;
                        Some(generics)
                    }
                    Ok((generics, None)) => Some(generics),
                    Err(_) => {
                        self.iter.rewind_to_mark(mark);
                        None
                    }
                }
            } else {
                None
            }
        } else {
            None
        };

        mark = self.iter.mark();

        // TODO: The peek doesn't skip line break, so can't ex. do a struct
        //       init with a line break at the start.
        // The identifier will be either a function call, a type or a reference
        // to a variable.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            match lex_token.kind {
                // Function call.
                LexTokenKind::Sym(Sym::ParenthesisBegin)
                    if !self.stop_conds.contains(&Sym::ParenthesisBegin) =>
                {
                    let start_symbol = Sym::ParenthesisBegin;
                    let end_symbol = Sym::ParenthesisEnd;
                    let (arguments, args_file_pos) =
                        self.iter.parse_arg_list(start_symbol, end_symbol)?;

                    file_pos.set_end(&args_file_pos)?;

                    Ok(Expr::FuncCall(FuncCall::new(
                        ident.into(),
                        arguments,
                        generics,
                        Some(file_pos),
                    )))
                }

                // Struct construction.
                LexTokenKind::Sym(Sym::CurlyBracketBegin)
                    if !self.stop_conds.contains(&Sym::CurlyBracketBegin) =>
                {
                    let start_symbol = Sym::CurlyBracketBegin;
                    let end_symbol = Sym::CurlyBracketEnd;
                    let (arguments, args_file_pos) =
                        self.iter.parse_arg_list(start_symbol, end_symbol)?;

                    file_pos.set_end(&args_file_pos)?;

                    Ok(Expr::StructInit(StructInit::new(
                        ident.into(),
                        arguments,
                        generics,
                        Some(file_pos),
                    )))
                }

                // Static method/variable access, this is the lhs type.
                LexTokenKind::Sym(Sym::DoubleColon)
                    if !self.stop_conds.contains(&Sym::DoubleColon) =>
                {
                    Ok(Expr::Type(
                        Ty::CompoundType(
                            InnerTy::UnknownIdent(ident.into(), self.iter.current_block_id()),
                            generics.unwrap_or_else(Generics::empty),
                            TypeInfo::Default(file_pos.to_owned()),
                        ),
                        Some(file_pos.to_owned()),
                    ))
                }

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
                                self.iter.rewind_to_mark(mark);

                                let ty = self.iter.parse_type(None)?;
                                let ty_file_pos = ty.file_pos().cloned();
                                return Ok(Expr::Type(ty, ty_file_pos));
                            }

                            _ => (),
                        }
                    }

                    // Otherwise this is just a regular variable name.
                    let parse_type = true;
                    let parse_value = false;
                    let is_const = false;
                    let var = self.iter.parse_var(
                        ident,
                        parse_type,
                        parse_value,
                        is_const,
                        generics.as_ref(),
                        file_pos,
                    )?;

                    Ok(Expr::Var(var))
                }
            }
        } else {
            // The ident was the last token if the current file, something must
            // have gone wrong.
            unreachable!("Got back None from iter when looking at ident: {}", ident);
        }
    }
}
