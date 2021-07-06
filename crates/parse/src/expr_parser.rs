use log::debug;

use common::{
    error::LangResult,
    file::FilePosition,
    path::LangPathBuilder,
    token::{
        ast::AstToken,
        block::{AdtKind, BlockHeader},
        expr::{AdtInit, ArrayInit, BuiltInCall, Expr, FnCall, FnPtr},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
    ty::{
        generics::{Generics, GenericsKind},
        inner_ty::InnerTy,
        ty::Ty,
        type_id::TypeId,
        type_info::TypeInfo,
    },
};
use lex::token::{Kw, LexToken, LexTokenKind, Sym};

use crate::{
    keyword_parser::KeyworkParser,
    parser::ParseTokenIter,
    token::{Fix, Operator, Output},
    type_parser::TypeParser,
};

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

    /// The previos lex token.
    prev: Option<LexToken>,

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
    ) -> LangResult<Option<Expr>> {
        // Take the file pos of the first symbol of the expression if possible.
        // This will be used if a more accurate/precise file_pos can't be used.
        let first_file_pos = iter.peek_file_pos()?;

        // True if this is a block that should be treated as a expression.
        // Else: For any other type of expr, parse it as a normal expression.
        if let Some((LexTokenKind::Kw(keyword @ Kw::If), kw_file_pos)) =
            iter.peek_skip_space_line().map(|t| (t.kind, t.file_pos))
        {
            let ast_token = KeyworkParser::parse(iter, keyword, kw_file_pos)?;

            let block = if let AstToken::Block(block) = ast_token {
                block
            } else {
                return Err(iter.err(
                    format!(
                        "Parsed if-block to AstToken that wasn't Block: {:#?}",
                        ast_token
                    ),
                    ast_token.file_pos().cloned(),
                ));
            };

            if !matches!(block.header, BlockHeader::If) {
                return Err(iter.err(
                    format!(
                        "Parsed if-block to AstToken that wasn't if-block: {:#?}",
                        block
                    ),
                    Some(block.file_pos),
                ));
            }

            Ok(Some(Expr::Block(Box::new(block), None)))
        } else {
            let mut expr_parser = Self {
                iter,
                token_count: 0,
                outputs: Vec::new(),
                operators: Vec::new(),
                stop_conds,
                prev_was_operand: false,
                prev: None,
                parenthesis_count: 0,
            };

            match expr_parser.shunting_yard()? {
                // (was_empty, expr_file_pos_opt)
                (false, Some(mut file_pos)) => {
                    debug!("Outputs: {:#?}", &expr_parser.outputs);
                    expr_parser.rev_polish_to_expr(&mut file_pos).map(Some)
                }

                (true, _) => Ok(None),

                (false, None) => Err(expr_parser.iter.err(
                    "Expr wasn't empty, but got back None expr_file_pos.".into(),
                    Some(first_file_pos),
                )),
            }
        }
    }

    /// See https://www.andr.mu/logs/the-shunting-yard-algorithm/ for a good
    /// explanation of the algorithm.
    /// Return `true` if the expression was empty.
    fn shunting_yard(&mut self) -> LangResult<(bool, Option<FilePosition>)> {
        let pos = self.iter.pos();

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
                    let next_token = if let Some(next_token) = self.iter.next_skip_space() {
                        next_token
                    } else {
                        return Err(self.iter.err(
                            "Got None after '@' (built-in call)".into(),
                            Some(lex_token.file_pos),
                        ));
                    };

                    let expr = if let LexTokenKind::Ident(ident) = next_token.kind {
                        self.parse_expr_ident(&ident, next_token.file_pos)?
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Expected ident after '@' (built-in call), got: {:?}",
                                next_token
                            ),
                            Some(next_token.file_pos),
                        ));
                    };

                    // "Convert" the parsed function call into a built in call.
                    let built_in_expr = if let Expr::FnCall(fn_call) = expr {
                        let mut built_in_file_pos = lex_token.file_pos;
                        built_in_file_pos.set_end(&fn_call.file_pos.unwrap())?;

                        let built_in_call = BuiltInCall::new(
                            fn_call.name,
                            fn_call.arguments,
                            fn_call.generics,
                            built_in_file_pos,
                        );
                        Expr::BuiltInCall(built_in_call)
                    } else {
                        return Err(self.iter.err(
                            format!(
                                "Expected to parse func call after '@' (built-in call), got: {:?}",
                                expr
                            ),
                            expr.file_pos().cloned(),
                        ));
                    };

                    self.shunt_operand(built_in_expr)?;
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
                    self.iter.rewind_to_pos(pos);

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

                // Function pointer.
                LexTokenKind::Kw(Kw::FunctionPointer) => {
                    let mut expr_file_pos = lex_token.file_pos.to_owned();

                    let mut module = self
                        .iter
                        .parse_path(&mut expr_file_pos, GenericsKind::Impl)?;

                    let last_part = module.pop().unwrap();
                    let name = last_part.0;
                    let generics = last_part.1;

                    file_pos.set_end(&expr_file_pos)?;

                    let expr = Expr::FnPtr(FnPtr::new(
                        name,
                        module,
                        generics,
                        None,
                        Some(expr_file_pos),
                    ));
                    self.shunt_operand(expr)?;
                }

                // Skip any linebreaks if the linebreaks aren't a part of the
                // stop conditions. Also skip comments.
                LexTokenKind::Sym(Sym::LineBreak) | LexTokenKind::Comment(..) => {
                    file_pos.set_end(&lex_token.file_pos)?;
                    self.token_count -= 1
                }

                // Special case for operators that takes a "type" as rhs.
                LexTokenKind::Sym(symbol @ Sym::As) | LexTokenKind::Sym(symbol @ Sym::Of) => {
                    if let Some(op) = crate::token::get_if_expr_op(&symbol) {
                        self.shunt_operator(op)?;
                        let (type_id, ty_file_pos) = self.iter.parse_type(None)?;
                        let expr = Expr::Type(type_id, Some(ty_file_pos));

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
                    if let Some(op) = crate::token::get_if_expr_op(&symbol) {
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

            self.prev = Some(lex_token)
        }

        // Move the remaining `operators` to `outputs` before parsing the expression.
        for op in std::mem::take(&mut self.operators).into_iter().rev() {
            self.outputs.push(Output::Operator(op));
        }

        let was_empty = false;
        Ok((was_empty, Some(file_pos)))
    }

    /// Adds a operand into the "shunting".
    fn shunt_operand(&mut self, expr: Expr) -> LangResult<()> {
        if !self.prev_was_operand {
            self.outputs.push(Output::Operand(expr));
            self.prev_was_operand = true;
            Ok(())
        } else {
            let prev_operand = self.prev.as_ref().unwrap();

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
    fn shunt_operator(&mut self, op: Operator) -> LangResult<()> {
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
                // TODO: Error handling if operator stack pop is None (?).
            }

            // Need extra logic for minus and plus since the can be both
            // binary and unary operators (x + +y).
            Operator::Plus => {
                let plus_op = if self.prev_was_operand {
                    Operator::BinaryOperator(BinOperator::Add)
                } else {
                    Operator::UnaryOperator(UnOperator::Positive)
                };
                self.add_operator(plus_op)?;
            }

            // Need extra logic for minus and plus since the can be both
            // binary and unary operators (x - -y).
            Operator::Minus => {
                let minus_op = if self.prev_was_operand {
                    Operator::BinaryOperator(BinOperator::Sub)
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
    fn add_operator(&mut self, op: Operator) -> LangResult<()> {
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
    /// Converts the given "outputs" in reverse polish notation to an expression.
    fn rev_polish_to_expr(&mut self, full_expr_file_pos: &mut FilePosition) -> LangResult<Expr> {
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

        if let Some(prev_file_pos) = prev_file_pos {
            full_expr_file_pos.set_end(&prev_file_pos)?;
        }

        let mut result_expr = expr_stack.remove(0);
        if let Some(file_pos) = result_expr.file_pos_mut() {
            *file_pos = *full_expr_file_pos;
        }
        Ok(result_expr)
    }

    fn parse_expr_ident(&mut self, ident: &str, file_pos: FilePosition) -> LangResult<Expr> {
        self.parse_expr_ident_with_path(ident, file_pos, LangPathBuilder::default())
    }

    /// The given ident expression might be the start of a path. In that case
    /// this function will be called recursively until the last identifier isn't
    /// followed by a double colon.
    fn parse_expr_ident_with_path(
        &mut self,
        ident: &str,
        mut file_pos: FilePosition,
        mut path_builder: LangPathBuilder,
    ) -> LangResult<Expr> {
        let gens_opt = self.parse_generic_impls(&mut file_pos);

        if let Some(gens) = &gens_opt {
            path_builder.add_path_gen(ident, gens);
        } else {
            path_builder.add_path(ident);
        }
        path_builder.file_pos(file_pos.to_owned());

        let pos = self.iter.pos();

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

                    let mut module = path_builder.build();
                    module.pop();

                    Ok(Expr::FnCall(FnCall::new(
                        ident.into(),
                        module,
                        arguments,
                        gens_opt,
                        Some(file_pos),
                    )))
                }

                // Struct or Union construction.
                LexTokenKind::Sym(Sym::CurlyBracketBegin)
                    if !self.stop_conds.contains(&Sym::CurlyBracketBegin) =>
                {
                    let start_symbol = Sym::CurlyBracketBegin;
                    let end_symbol = Sym::CurlyBracketEnd;
                    let (arguments, args_file_pos) =
                        self.iter.parse_arg_list(start_symbol, end_symbol)?;

                    file_pos.set_end(&args_file_pos)?;

                    let mut module = path_builder.build();
                    module.pop();

                    Ok(Expr::AdtInit(AdtInit::new(
                        ident.into(),
                        module,
                        arguments,
                        gens_opt,
                        Some(file_pos),
                        AdtKind::Unknown,
                    )))
                }

                // Either a static method/variable access or the lhs of a path.
                // Add this path to the `path_builder` and call this function
                // recursively until the end of the "path" is found.
                LexTokenKind::Sym(Sym::DoubleColon)
                    if !self.stop_conds.contains(&Sym::DoubleColon) =>
                {
                    // Consume the double colon
                    self.iter.next_skip_space().unwrap();

                    let next_lex_token = if let Some(lex_token) = self.iter.next_skip_space() {
                        lex_token
                    } else {
                        return Err(self
                            .iter
                            .err("Got None after double colon path".into(), Some(file_pos)));
                    };

                    if let LexTokenKind::Ident(new_ident) = next_lex_token.kind {
                        file_pos.set_end(&next_lex_token.file_pos)?;
                        self.parse_expr_ident_with_path(&new_ident, file_pos, path_builder)
                    } else {
                        Err(self.iter.err(
                            format!(
                                "Expected ident after double colon path, found: {:#?}",
                                next_lex_token
                            ),
                            Some(file_pos),
                        ))
                    }
                }

                _ => {
                    // See if this is a type. It is a type if the previous
                    // operator was a binary operator that takes a type
                    // as the right hand side.
                    if let Some(last_op) = self.operators.last() {
                        match last_op {
                            Operator::BinaryOperator(BinOperator::As)
                            | Operator::BinaryOperator(BinOperator::Of) => {
                                // Put back the old `lex_token` contaning this
                                // identifier and parse as type.
                                self.iter.rewind_to_pos(pos);

                                let (type_id, ty_file_pos) =
                                    self.iter.parse_type_with_path(None, path_builder)?;
                                return Ok(Expr::Type(type_id, Some(ty_file_pos)));
                            }

                            _ => (),
                        }
                    }

                    // If there are more than one part in the built up path, it
                    // means that this must be some kind of "static" access on
                    // a identifier. Currently the only valid static variable
                    // access is on enums, so this must be a enum usage.
                    //
                    // If there is no path built up(just a single identifier),
                    // this must be a regular variable.
                    if path_builder.count() > 1 {
                        let mut adt_path = path_builder.build();
                        adt_path.pop(); // Remove the name of the enum variant.

                        let mut ty_env_guard = self.iter.ty_env.lock().unwrap();
                        let enum_type_id = ty_env_guard.id(&Ty::CompoundType(
                            InnerTy::Enum(adt_path),
                            Generics::empty(),
                            TypeInfo::Default(file_pos.to_owned()),
                        ))?;
                        let enum_access =
                            UnOperator::EnumAccess(ident.into(), self.iter.current_block_id());
                        let un_op = UnOp::new(
                            enum_access,
                            Box::new(Expr::Type(enum_type_id, Some(file_pos.to_owned()))),
                            Some(file_pos.to_owned()),
                        );

                        Ok(Expr::Op(Op::UnOp(un_op)))
                    } else {
                        // Otherwise this is just a regular variable name.
                        let parse_type = true;
                        let parse_value = false;
                        let is_const = false;
                        let var = self.iter.parse_var(
                            ident,
                            parse_type,
                            parse_value,
                            is_const,
                            gens_opt.as_ref(),
                            file_pos,
                        )?;

                        Ok(Expr::Var(var))
                    }
                }
            }
        } else {
            // The ident was the last token if the current file, something must
            // have gone wrong.
            unreachable!("Got back None from iter when looking at ident: {}", ident);
        }
    }

    fn parse_generic_impls(&mut self, file_pos: &mut FilePosition) -> Option<Generics> {
        let pos = self.iter.pos();

        // Keep track of the current type id. Parsing the types below might create
        // new types. If it turns out that this isn't a generic list, revert and
        // remove the newly created types.
        let start_type_id = self.iter.ty_env.lock().unwrap().current_type_id();

        // If the next token is a  "PointyBracketBegin" it can either be a start
        // of a generic list for structures/function, or it can also be a "LessThan"
        // compare operation. Try to parse it as a generic list, if it fails assume
        // that it is a Lt compare and return None.
        if let Some(lex_token) = self.iter.peek_skip_space() {
            if let LexTokenKind::Sym(Sym::PointyBracketBegin) = lex_token.kind {
                match TypeParser::new(self.iter, None).parse_type_generics(GenericsKind::Impl) {
                    Ok((generics, Some(gens_file_pos))) => {
                        file_pos.set_end(&gens_file_pos).ok()?;
                        generics
                    }
                    Ok((generics, None)) => generics,
                    Err(_) => {
                        // Remove types that was created during the process of
                        // figuring out if they were types or not.
                        let end_type_id = self.iter.ty_env.lock().unwrap().current_type_id();
                        for type_id in start_type_id..end_type_id {
                            self.iter.ty_env.lock().unwrap().remove(TypeId(type_id))
                        }
                        self.iter.rewind_to_pos(pos);
                        None
                    }
                }
            } else {
                None
            }
        } else {
            None
        }
    }
}
