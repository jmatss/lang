use crate::error::CustomError::ParseError;
use crate::lexer::simple_token::{SimpleToken, Symbol, Literal as SimpleLiteral};
use crate::CustomResult;
use crate::parser::token::{Token, Statement, BlockHeader, Expression, FunctionCall, Variable, Type, Function, BinaryOperation, BinaryOperator, Class, Interface, Enum, Argument, Literal, Operator, Operation, ArrayAccess, Path, Output, UnaryOperation, UnaryOperator};
use crate::error::CustomError;
use crate::parser::abstract_syntax_tree::AST;

// TODO: Let line breaks be OK sometimes and treated as whitespace,
//  ex. when having a long parameter list or argument list.

// TODO: Fix correct parsing of block headers inside parse_expression().
//  Example:
//      a = if 1==1: 1 else: 2
//  In the example the if-expression needs to be added to the AST,
//  but also needs to be treated as an expression.

pub struct TokenIter<'a> {
    pub simple_tokens: &'a [SimpleToken],
    pub position: usize,

    pub ast: AST,

    // line_number and indent_level are updated in
    // update_indent_and_line_number() every time a LineBreak is found.
    // indent_level is the current indent size normalized by indent_fixed_size
    // so that it is represented as: 0, 1, 2 ...
    pub line_number: usize,
    pub indent_level: usize,

    pub indent_fixed_size: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(simple_tokens: &'a [SimpleToken], indent_fixed_size: usize) -> Self {
        TokenIter {
            simple_tokens,
            position: 0,

            ast: AST::new(),

            line_number: 1,
            indent_level: 0,

            indent_fixed_size,
        }
    }

    #[inline]
    fn next(&mut self) -> Option<SimpleToken> {
        let result = self.simple_tokens.get(self.position).cloned();
        self.position += 1;
        result
    }

    #[inline]
    fn next_skip_whitespace(&mut self) -> Option<SimpleToken> {
        self.skip_whitespace();
        self.next()
    }

    #[inline]
    fn next_skip_whitespace_and_line_break(&mut self, update_indent: bool) -> CustomResult<Option<SimpleToken>> {
        self.skip_whitespace_and_line_break(update_indent)?;
        Ok(self.next())
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        if let Some(SimpleToken::Symbol(Symbol::WhiteSpace(_))) = self.peek() {
            self.next();    // Remove whitespace.
        }
    }

    #[inline]
    fn skip_whitespace_and_line_break(&mut self, update_indent: bool) -> CustomResult<()> {
        // Need to do it in a loop if there are multiple whitespace/linebreaks in a row:
        //      whitespace|linebreak|whitespace|linebreak...
        loop {
            let result = self.next();
            match result {
                Some(SimpleToken::Symbol(Symbol::WhiteSpace(_))) =>
                    continue,
                Some(SimpleToken::Symbol(Symbol::LineBreak)) => {
                    self.update_indent_and_line_number(update_indent)?;
                    continue;
                }
                _ => {
                    self.rewind();
                    return Ok(());
                }
            }
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<SimpleToken> {
        self.peek_n(0)
    }

    #[inline]
    fn peek_n(&mut self, n: isize) -> Option<SimpleToken> {
        // n is isize to allow for negative peeks.
        let index = self.position as isize + n;
        if index < 0 {
            return None;
        }
        self.simple_tokens.get(index as usize).cloned()
    }

    #[inline]
    fn peek_skip_whitespace(&mut self) -> Option<SimpleToken> {
        self.skip_whitespace();
        self.peek()
    }

    #[inline]
    fn peek_skip_whitespace_and_line_break(&mut self) -> CustomResult<SimpleToken> {
        let mut peek_amount = 0;
        loop {
            let result = self.peek_n(peek_amount)
                .ok_or_else(|| ParseError("".to_string()))?;
            match result {
                | SimpleToken::Symbol(Symbol::WhiteSpace(_))
                | SimpleToken::Symbol(Symbol::LineBreak) => {
                    peek_amount += 1;
                    continue;
                }

                _ =>
                    return Ok(result)
            }
        }
    }

    #[inline]
    fn rewind(&mut self) {
        self.position -= 1;
    }

    #[inline]
    fn rewind_skip_whitespace(&mut self) {
        self.position -= 1;
        if let Some(SimpleToken::Symbol(Symbol::WhiteSpace(_))) = self.peek() {
            self.position -= 1;
        }
    }

    // Ensure that the next token is a block header end.
    fn assert_block_header_end(&mut self) -> CustomResult<()> {
        let next = self.next_skip_whitespace();
        if let Some(SimpleToken::Symbol(Symbol::Colon)) = next {
            Ok(())
        } else {
            Err(ParseError(
                format!("Incorrect block ending. Expected: Colon, got: {:?}.", next)
            ))
        }
    }

    // Can be called after a LineBreak have been consumed or right at the LineBreak.
    // Ensures that the new indent is valid and increases line_number by 1.
    // The "update_indent" arg is used to indicate if the indentation should be updated or not.
    // Examples when it should be set to false is LineBreaks before and after Commas.
    fn update_indent_and_line_number(&mut self, update_indent: bool) -> CustomResult<()> {
        if update_indent {
            if let Some(SimpleToken::Symbol(Symbol::LineBreak)) = self.peek() {
                self.next();    // Consume Linebreak.
            } else if let Some(SimpleToken::Symbol(Symbol::LineBreak)) = self.peek_n(-1) {
                // Everything alright, move along.
            } else {
                return Err(ParseError(
                    "Called update_indent_and_line_number with no line break in sight.".to_string()
                ));
            }

            let indent = self.next_whitespace();

            // Edge case if the row is empty, ignore indents.
            if let Some(SimpleToken::Symbol(Symbol::LineBreak)) = self.peek_skip_whitespace() {
                self.line_number += 1;
                return Ok(());
            } else if indent % self.indent_fixed_size != 0 {
                return Err(ParseError(
                    format!("Current indent not divisible by specified fixed indent. Fixed: {}, got: {}",
                            self.indent_fixed_size, indent
                    )
                ));
            }

            // If the indent size increases (i.e. a new block), ensure that it only increased
            // with at most 1 indent level.
            // It can be as small as it want if it decreases though.
            let current_indent_level = indent / self.indent_fixed_size;
            if current_indent_level - 1 > self.indent_level {
                return Err(ParseError(
                    format!("Current indent not not valid relative to header.\
                     \"BlockHeader\" indent level: {}, current indent level: {}",
                            self.indent_level, current_indent_level
                    )
                ));
            }

            self.indent_level = current_indent_level;
        }

        self.line_number += 1;

        Ok(())
    }

    fn next_whitespace(&mut self) -> usize {
        if let Some(SimpleToken::Symbol(Symbol::WhiteSpace(size))) = self.next() {
            size
        } else {
            self.rewind();
            0
        }
    }

    // Build up the AST.
    pub fn parse_abstract_syntax_tree(&'a mut self) -> CustomResult<AST> {
        loop {
            let current = self.next()
                .ok_or_else(|| ParseError("No more simple_tokens in start of parse_a_s_t".to_string()))?;

            /*
            println!(
                "Current SimpleToken: {:?}, line_number: {}, indent_level: {}",
                &current, self.line_number, self.indent_level
            );
            */

            match current {
                SimpleToken::Identifier(identifier) => {
                    let token = self.parse_identifier(&identifier)?;

                    // If BlockHeader: Add this as a new block to the AST.
                    // Else if Expression: Parse the rest of the expression.
                    // Else: Add this as a token to the AST.
                    match &token {
                        Token::BlockHeader(_) =>
                            self.ast.insert_block(token, self.line_number, self.indent_level)?,

                        Token::Expression(expression) => {
                            let expression_token = Token::Expression(
                                self.parse_expression_with_previous(
                                    expression.clone()
                                )?
                            );
                            self.ast.insert_token(expression_token, self.line_number, self.indent_level)?
                        }

                        _ =>
                            self.ast.insert_token(token, self.line_number, self.indent_level)?
                    }
                }

                SimpleToken::Symbol(Symbol::LineBreak) =>
                    self.update_indent_and_line_number(true)?,

                SimpleToken::Symbol(Symbol::SemiColon) =>
                    continue,

                // TODO: Make sure to update indent size when getting LineBreak.
                | SimpleToken::Symbol(_)
                | SimpleToken::Number(_)
                | SimpleToken::Literal(_) => {
                    self.rewind();
                    let expression = self.parse_expression_until_default()?;
                    let token = Token::Expression(expression);
                    self.ast.insert_token(token, self.line_number, self.indent_level)?;
                }

                SimpleToken::EndOfFile =>
                    return Ok(std::mem::replace(&mut self.ast, AST::new())),

                SimpleToken::Unknown(unknown) =>
                    return Err(ParseError(format!("Bad string (SimpleToken) during parse_next: {:?}", unknown)))
            }
        }
    }

    fn next_identifier(&mut self) -> CustomResult<String> {
        let simple_token = self.next_skip_whitespace();
        if let Some(SimpleToken::Identifier(identifier)) = simple_token {
            Ok(identifier)
        } else {
            Err(ParseError(
                format!("Did not get an identifier when in next_identifier: {:?}", simple_token)
            ))
        }
    }

    // Valid formats for variables:
    //      id
    //      id @ Type+ modifier*
    // (The assignment is handled separately, ex. id = expression)
    // TODO: Add "name, nameN" for declaring multiple variables on the same line.
    // TODO: Add support for tuples.
    fn parse_variable_with_identifier(&mut self, identifier: &str) -> CustomResult<Variable> {
        let mut variable = Variable::new(identifier.to_string());

        // Make sure that the variable doesn't have a name that clashes with a keyword.
        if let Some(keyword) = Token::lookup_identifier(&identifier) {
            return Err(CustomError::ParseError(
                format!("A keyword was used as variable name: {:?}", keyword)
            ));
        }

        // If true: this variable has specified type, parse the type and modifiers.
        if let Some(SimpleToken::Symbol(Symbol::At)) = self.peek_skip_whitespace() {
            self.next_skip_whitespace();    // Remove "At" symbol.
            self.parse_next_type_and_modifiers(&mut variable)?;
        }

        Ok(variable)
    }

    fn parse_variable(&mut self) -> CustomResult<Variable> {
        let peek = self.peek_skip_whitespace();
        if let Some(SimpleToken::Symbol(symbol)) = peek {
            return Err(CustomError::ParseError(
                format!("A symbol was used as variable name: {:?}", symbol)
            ));
        }

        let identifier = self.next_identifier()?;
        self.parse_variable_with_identifier(&identifier)
    }

    fn parse_next_type_and_modifiers(&mut self, variable: &mut Variable) -> CustomResult<()> {
        // Parse the type, which have to be the first "identifier".
        // If the first identifier is a modifier token, there is no type and it is set to None.
        let peek = self.peek_skip_whitespace();
        if let Some(SimpleToken::Identifier(identifier)) = peek {
            if let Some(Token::Statement(Statement::Modifier(modifier))) = Token::lookup_identifier(&identifier) {
                variable.var_type = None;
            } else {
                variable.var_type = Some(self.next_type()?);
            }
        } else {
            return Err(ParseError(
                format!("Expected identifier(s) in parse_next_type_and_modifiers, got: {:?}", peek)
            ));
        }

        while let Some(SimpleToken::Identifier(identifier)) = self.next_skip_whitespace() {
            // If true: this is a modifier, add to "variable" and continue parsing next token.
            if let Some(Token::Statement(Statement::Modifier(modifier))) = Token::lookup_identifier(&identifier) {
                variable.modifiers.push(modifier);
            } else {
                break;
            }
        }

        self.rewind_skip_whitespace();

        Ok(())
    }

    fn next_type(&mut self) -> CustomResult<Type> {
        let identifier = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        Ok(Type::new(Some(identifier), generics))
    }

    // Valid formats:
    //      Type
    //      Type1<T>, TypeN
    // Whitespace and line breaks are allowed between the types(before and after comma).
    fn next_type_list(&mut self) -> CustomResult<Vec<Type>> {
        let mut types = Vec::new();

        // Loop through and parse all items in the "type list".
        loop {
            self.skip_whitespace_and_line_break(false)?;
            types.push(self.next_type()?);

            let current = self.peek_skip_whitespace_and_line_break()?;
            if let SimpleToken::Symbol(Symbol::Comma) = current {
                self.next_skip_whitespace_and_line_break(false)?;    // Remove Comma symbol.
                continue;
            } else {
                break;
            }
        }

        Ok(types)
    }

    // PS: If there are no generic list i.e. the first symbol is NOT a PointBracketBegin,
    // returns an empty vector and does NOT return an error.
    fn next_generic_list(&mut self) -> CustomResult<Vec<Type>> {
        let mut generics = Vec::new();

        if let Some(SimpleToken::Symbol(Symbol::PointyBracketBegin)) = self.peek_skip_whitespace() {
            self.next_skip_whitespace();    // Remove PointBracketBegin symbol.

            generics = self.next_type_list()?;

            let current = self.next_skip_whitespace_and_line_break(false)?;
            if let Some(SimpleToken::Symbol(Symbol::PointyBracketEnd)) = current {
                // Everything alright.
            } else {
                return Err(ParseError(
                    format!("Expected a PointBracketEnd after parsing generic list, got: {:?}", current)
                ));
            }
        }

        Ok(generics)
    }

    // Path is used in "use" and "package".
    // Valid format:
    //      id1
    //      id1.iN      (A list of identifiers separated by dots)
    fn parse_path(&mut self) -> CustomResult<Path> {
        let mut path = Path::new();

        // Loop through the path one identifier at a time.
        loop {
            path.push(self.next_identifier()?);

            let peek = self.peek_skip_whitespace()
                .ok_or_else(|| ParseError("No more tokens in parse_path.".to_string()))?;
            if let SimpleToken::Symbol(Symbol::Dot) = peek {
                self.next_skip_whitespace();    // Remove Dot symbol.
                continue;
            } else if SimpleToken::is_break_symbol(&peek) {
                break;
            } else {
                return Err(ParseError(
                    format!("Incorrect symbol found during parsing of next_path: {:?}", peek)
                ));
            }
        }

        Ok(path)
    }

    /*
    fn next_tuple(&mut self) -> CustomResult {

    }
    */

    fn parse_identifier(&mut self, identifier: &str) -> CustomResult<Token> {
        // If true: A reserved keyword (either statement or header).
        // Else: a function call, variable, array access etc.
        if let Some(token) = Token::lookup_identifier(&identifier) {
            match token {
                Token::Statement(statement) =>
                    Ok(Token::Statement(self.parse_statement(statement)?)),

                Token::BlockHeader(block_header) =>
                    Ok(Token::BlockHeader(self.parse_block_header(block_header)?)),

                _ =>
                    Err(ParseError(
                        format!("Received incorrect token during parse_identifier: {:?}", token)
                    ))
            }
        } else {
            self.parse_identifier_as_expression(identifier)
        }
    }

    // Neither a statement nor a block.
    // Either a function call, variable, array access etc.
    fn parse_identifier_as_expression(&mut self, identifier: &str) -> CustomResult<Token> {
        let peek = self.peek_skip_whitespace()
            .ok_or_else(|| ParseError("No more tokens in parse_identifier_as_expression".to_string()))?;
        match peek {
            SimpleToken::Symbol(Symbol::ParenthesisBegin) => {
                // This is a function call.
                let function_call = self.parse_function_call(&identifier)?;
                Ok(Token::Expression(Expression::FunctionCall(Some(function_call))))
            }

            // TODO: Parse multiple dimension arrays
            //  Can only do one, ex. id[expr], need to do: id[expr1][expr2]...
            // TODO: Allow linebreak after SquareBracketBegin(easy) and before SquareBracketEnd(hard).
            // TODO: Add support for init of empty vector etc. (x = [])
            SimpleToken::Symbol(Symbol::SquareBracketBegin) => {
                // This is an array access (ex. id[expr])
                let array_access = self.parse_array_access(identifier)?;
                Ok(Token::Expression(Expression::ArrayAccess(Some(array_access))))
            }

            _ => {
                // Treat it as an variable.
                let variable = self.parse_variable_with_identifier(identifier)?;
                Ok(Token::Expression(Expression::Variable(Some(variable))))
            }
        }
    }

    fn parse_array_access(&mut self, identifier: &str) -> CustomResult<ArrayAccess> {
        let next = self.next_skip_whitespace();
        if let Some(SimpleToken::Symbol(Symbol::SquareBracketBegin)) = next {
            // Everything ok, the SquareBracketBegin have been removed.
        } else {
            return Err(ParseError(
                format!("First simple_token in parse_array_access isn't a SquareBracketBegin: {:?}", next)
            ));
        }

        // FIXME: Is it allowed for a variable that doesn't decalare itself to use modifiers/Type (?)
        //  Probably not, but currently the parsing allows it everywhere else, so keep it for now.
        let variable = self.parse_variable_with_identifier(identifier)?;
        let expression = self.parse_expression_until(&|simple_token| {
            if let SimpleToken::Symbol(Symbol::SquareBracketEnd) = simple_token {
                true
            } else {
                false
            }
        })?;

        self.next_skip_whitespace();    // Remove the SquareBracketEnd.
        let array_access = ArrayAccess::new(variable, Box::new(expression));

        Ok(array_access)
    }

    fn parse_statement(&mut self, statement: Statement) -> CustomResult<Statement> {
        match statement {
            Statement::Return(_) => {
                // If true: Next symbol is a break which means that this return-statement
                // doesn't return anything.
                // Else: It returns the next expression.
                let peek = self.peek_skip_whitespace()
                    .ok_or_else(|| ParseError("Out of tokens in parse_statement.".to_string()))?;
                if SimpleToken::is_break_symbol(&peek) {
                    Ok(Statement::Return(None))
                } else {
                    Ok(Statement::Return(Some(self.parse_expression_until_default()?)))
                }
            }

            Statement::Yield(_) =>
                Ok(Statement::Yield(Some(self.parse_expression_until_default()?))),

            Statement::Break | Statement::Continue =>
                Ok(statement),

            Statement::Use(_) =>
                Ok(Statement::Use(Some(self.parse_path()?))),

            Statement::Package(_) =>
                Ok(Statement::Package(Some(self.parse_path()?))),

            Statement::Throw(_) =>
                Ok(Statement::Throw(Some(self.parse_expression_until_default()?))),

            // Ignore modifiers.
            _ => Err(ParseError(
                format!("Received invalid statement in parse_statement: {:?}", statement)
            )),
        }
    }

    // Shunting-yard algorithm
    // Parses expression until stop_condition evaluates to true.
    // Does NOT consume the SimpleToken that returned true.
    // PS: Skips/ignores LineBreak if they aren't specified in the stop_condition().
    fn parse_expression_with_previous_until(
        &mut self,
        /*stop_condition: fn(&SimpleToken) -> bool,*/
        stop_condition: &dyn Fn(&SimpleToken) -> bool,
        previous_expression: Option<Expression>,
    ) -> CustomResult<Expression> {
        let mut output_stack: Vec<Output> = Vec::new();
        let mut operator_stack: Vec<Operator> = Vec::new();

        // This bool is used to ensure that all "values" are separated by operands.
        // "value" == not an operand, i.e. literals, numbers, variables or function calls.
        let mut previous_was_value =
            if let Some(expression) = previous_expression {
                // Add previously parsed expression if it exists.
                output_stack.push(Output::Value(expression));
                true
            } else {
                false
            };

        loop {
            let current = self.next_skip_whitespace()
                .ok_or_else(||
                    ParseError("No more tokens in parse_expression_with_previous_until".to_string())
                )?;

            /*
            println!("\"current\", \"output_stack\" and \"operator_stack\" in expression parsing:\n{:?}", &current);
            dbg!(&output_stack, &operator_stack);
            println!();
            */

            // TODO: Clean up LineBreak special case.
            // Break and stop parsing expression if stop_condition() evaluates to true.
            if stop_condition(&current) {
                // Put back the char
                self.rewind_skip_whitespace();
                break current;
            }

            // If Linebreak: Update indent/linebreak and skip.
            // This allows one to use LineBreak's freely in expressions that doesn't
            // have LineBreak as a stop_condition.
            if let SimpleToken::Symbol(Symbol::LineBreak) = current {
                self.update_indent_and_line_number(false)?;
                continue;
            }

            match current {
                SimpleToken::Identifier(identifier) => {
                    if previous_was_value {
                        return Err(ParseError(
                            "Received two \"values\" in a row in parse_expression.".to_string()
                        ));
                    }

                    // TODO: Add support for parsing if, match etc.
                    if let Token::Expression(expression) = self.parse_identifier_as_expression(&identifier)? {
                        // Function call, variable or array access.
                        output_stack.push(Output::Value(expression));
                    } else {
                        return Err(ParseError(
                            format!("Bad identifier parsed during parse_expression: {:?}", identifier)
                        ));
                    }

                    previous_was_value = true;
                }

                SimpleToken::Number(number_string) => {
                    if previous_was_value {
                        return Err(ParseError(
                            "Received two \"values\" in a row in parse_expression.".to_string()
                        ));
                    }

                    let number =
                        if number_string.contains('.') {
                            Expression::Float(Some(number_string))
                        } else {
                            Expression::Integer(Some(number_string))
                        };
                    output_stack.push(Output::Value(number));

                    previous_was_value = true;
                }

                SimpleToken::Literal(literal) => {
                    if previous_was_value {
                        return Err(ParseError(
                            "Received two \"values\" in a row in parse_expression.".to_string()
                        ));
                    }

                    let expression =
                        match literal {
                            SimpleLiteral::StringLiteral(string) =>
                                Expression::Literal(Some(Literal::StringLiteral(string))),
                            SimpleLiteral::CharLiteral(string) =>
                                Expression::Literal(Some(Literal::CharLiteral(string))),
                        };
                    output_stack.push(Output::Value(expression));

                    previous_was_value = true;
                }

                SimpleToken::Symbol(symbol) => {
                    let operator = Token::lookup_operator(symbol.clone())
                        .ok_or_else(||
                            ParseError(
                                format!("Parsed None operator during expression: {:?}", symbol)
                            )
                        )?;
                    match operator {
                        Operator::ParenthesisBegin =>
                            operator_stack.push(operator),

                        Operator::ParenthesisEnd => {
                            loop {
                                let popped_operator = operator_stack.pop()
                                    .ok_or_else(|| ParseError(
                                        "Exhausted operator stack while looking for parenthesisBegin.".to_string()
                                    ))?;

                                if let Operator::ParenthesisBegin = popped_operator {
                                    break;
                                } else {
                                    output_stack.push(Output::Operator(popped_operator));
                                }
                            }
                        }

                        Operator::Increment => {
                            let increment_operator =
                                if previous_was_value {
                                    Operator::UnaryOperator(UnaryOperator::IncrementPostfix)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::IncrementPrefix)
                                };

                            self.add_operator(
                                increment_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        Operator::Decrement => {
                            let decrement_operator =
                                if previous_was_value {
                                    Operator::UnaryOperator(UnaryOperator::DecrementPostfix)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::DecrementPrefix)
                                };

                            self.add_operator(
                                decrement_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        Operator::Plus => {
                            let plus_operator =
                                if previous_was_value {
                                    Operator::BinaryOperator(BinaryOperator::Addition)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::Positive)
                                };

                            self.add_operator(
                                plus_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        Operator::Minus => {
                            let minus_operator =
                                if previous_was_value {
                                    Operator::BinaryOperator(BinaryOperator::Subtraction)
                                } else {
                                    Operator::UnaryOperator(UnaryOperator::Negative)
                                };

                            self.add_operator(
                                minus_operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }

                        _ => {
                            // Rest of the operators that aren't parenthesis or inc/dec.
                            self.add_operator(
                                operator,
                                &mut output_stack,
                                &mut operator_stack,
                            )?;
                        }
                    }

                    // Previous was (/current is) Symbol.
                    previous_was_value = false;
                }

                // Ignore modifiers.
                _ => return Err(ParseError(
                    format!("Received invalid simple_token in parse_expression: {:?}", current.clone())
                )),
            }
        };

        while let Some(operator) = operator_stack.pop() {
            output_stack.push(Output::Operator(operator));
        }

        Ok(self.polish_to_expression(&output_stack)?)
    }

    fn add_operator(
        &self,
        operator: Operator,
        output_stack: &mut Vec<Output>,
        operator_stack: &mut Vec<Operator>,
    ) -> CustomResult<()> {
        let operator_precedence = operator.precedence()
            .ok_or_else(|| ParseError(
                format!("Unable to see precedence for operator: {:?}", operator)
            ))?;
        while let Some(popped_operator) = operator_stack.pop() {
            if let Operator::ParenthesisBegin = popped_operator {
                // put back the start parenthesis.
                operator_stack.push(popped_operator);
                break;
            }

            let popped_operator_prec = popped_operator.precedence()
                .ok_or_else(|| ParseError(
                    format!(
                        "Unable to see precedence for popped_operator: {:?}",
                        popped_operator
                    )
                ))?;

            // "popped operator" higher precedence than "current operator".
            // or same precedence with left-to-right evaluation.
            if operator_precedence > popped_operator_prec
                ||
                (operator_precedence == popped_operator_prec
                    && popped_operator.evaluate_left_to_right().ok_or_else(||
                    ParseError("Unable to see if operator eval left to right."
                        .to_string())
                )?)
            {
                output_stack.push(Output::Operator(popped_operator));
                continue;
            } else {
                operator_stack.push(popped_operator);    // Put back
                break;
            }
        }

        operator_stack.push(operator);
        Ok(())
    }

    fn parse_expression_until(&mut self, stop_condition: &dyn Fn(&SimpleToken) -> bool) -> CustomResult<Expression> {
        self.parse_expression_with_previous_until(stop_condition, None)
    }

    fn parse_expression_until_default(&mut self) -> CustomResult<Expression> {
        self.parse_expression_until(&TokenIter::default_stop_condition)
    }

    fn parse_expression_until_colon(&mut self) -> CustomResult<Expression> {
        self.parse_expression_until(
            &|stop|
                if let SimpleToken::Symbol(Symbol::Colon) = stop {
                    true
                } else {
                    false
                }
        )
    }

    fn parse_expression_until_comma_parenthesis_end(&mut self) -> CustomResult<Expression> {
        self.parse_expression_until(
            &|stop|
                match stop {
                    | SimpleToken::Symbol(Symbol::Comma)
                    | SimpleToken::Symbol(Symbol::ParenthesisEnd) => true,
                    _ => false
                }
        )
    }

    fn parse_expression_with_previous(&mut self, previous_expression: Expression) -> CustomResult<Expression> {
        self.parse_expression_with_previous_until(
            &TokenIter::default_stop_condition,
            Some(previous_expression),
        )
    }

    fn parse_expression_list_until(
        &mut self,
        stop_condition: &dyn Fn(&SimpleToken) -> bool,
    ) -> CustomResult<Vec<Expression>> {
        let mut expressions = Vec::new();

        // Parses a list of expressions separated with Commas.
        // Stops when "stop_condition" is met.
        loop {
            let expression = self.parse_expression_until(
                &|simple_token| {
                    if let SimpleToken::Symbol(Symbol::Comma) = simple_token {
                        true
                    } else if stop_condition(simple_token) {
                        true
                    } else {
                        false
                    }
                }
            )?;

            expressions.push(expression);

            if let Some(SimpleToken::Symbol(Symbol::Comma)) = self.peek() {
                self.next();    // Remove comma
                continue;
            } else {
                break;
            }
        }

        Ok(expressions)
    }

    // Returns true if any of these symbols are found:
    //      LineBreak, SemiColon, EndOfFile, Colon or Comma.
    fn default_stop_condition(simple_token: &SimpleToken) -> bool {
        if SimpleToken::is_break_symbol(simple_token) {
            return true;
        }
        match simple_token {
            | SimpleToken::Symbol(Symbol::Colon)
            | SimpleToken::Symbol(Symbol::Comma) => true,
            _ => false
        }
    }

    fn polish_to_expression(&self, output_stack: &[Output]) -> CustomResult<Expression> {
        let mut expression_stack = Vec::new();
        for i in 0..output_stack.len() {
            let current = output_stack.get(i)
                .ok_or_else(|| ParseError(format!("Empty item in output_stack at index: {}", i)))?;

            match current {
                Output::Operator(Operator::BinaryOperator(operator)) => {
                    let right = expression_stack.pop()
                        .ok_or_else(|| ParseError(
                            "Empty item in expression_stack when popping  (binary).".to_string()
                        ))?;
                    let left = expression_stack.pop()
                        .ok_or_else(|| ParseError(
                            "Empty item in expression_stack when popping left (binary).".to_string()
                        ))?;

                    let binary_operation = BinaryOperation::new(
                        operator.clone(),
                        Box::new(left),
                        Box::new(right),
                    );

                    expression_stack.push(
                        Expression::Operation(Operation::BinaryOperation(Some(binary_operation)))
                    );
                }

                Output::Operator(Operator::UnaryOperator(operator)) => {
                    let value = expression_stack.pop()
                        .ok_or_else(|| ParseError(
                            "Empty item in expression_stack when popping value (unary).".to_string()
                        ))?;

                    let unary_operation = UnaryOperation::new(
                        operator.clone(),
                        Box::new(value),
                    );

                    expression_stack.push(
                        Expression::Operation(Operation::UnaryOperation(Some(unary_operation)))
                    );
                }

                Output::Value(expression) =>
                    expression_stack.push(expression.clone()),

                _ => return Err(ParseError(
                    format!("Bad match during polish_to_expression with Output: {:?}", current)
                ))
            }
        }

        if expression_stack.len() != 1 {
            return Err(ParseError(
                format!("Not one expression left at end of polish_to_expression, amount: {}", expression_stack.len())
            ));
        }

        Ok(
            expression_stack.pop()
                .ok_or_else(|| ParseError("Unable to return last expression of expression_stack.".to_string()))?
        )
    }

    fn parse_function_call(&mut self, function_name: &str) -> CustomResult<FunctionCall> {
        let mut arguments = Vec::new();

        // The function name has already been parsed and is sent as argument.
        // Start by parsing the first ParenthesisBegin.
        let current = self.next_skip_whitespace();
        match current {
            Some(SimpleToken::Symbol(Symbol::ParenthesisBegin)) => {

                // Edge case if the call has no arguments.
                let peek = self.peek_skip_whitespace_and_line_break()?;
                if let SimpleToken::Symbol(Symbol::ParenthesisEnd) = peek {
                    self.next_skip_whitespace_and_line_break(false)?; // Remove end parenthesis.
                    return Ok(FunctionCall::new(function_name.to_string(), arguments));
                }

                // FIXME: Can figure out that it isn't a named argument at the first if-statement
                //  if it doesn't match a identifier, so can clean up code.
                loop {
                    // Start with the assumption that this is a named argument.
                    // Parse as identifier and peek on the next simple token, that is assumed to be Equals.
                    // If it is false, redo everything with the assumption that it is not a named argument.
                    self.skip_whitespace_and_line_break(false)?;  // Allow linebreak.
                    let next = self.next_skip_whitespace_and_line_break(false)?;
                    let name =
                        if let Some(SimpleToken::Identifier(identifier)) = next {
                            identifier
                        } else {
                            "".to_string()
                        };

                    // If true: This is a named argument,
                    // Else: This is a regular argument.
                    // FIXME: Maybe allow line break in front/behind equals sign.
                    if let Some(SimpleToken::Symbol(Symbol::Equals)) = self.peek_skip_whitespace() {
                        self.next_skip_whitespace();    // Remove Equals symbol.
                        let expr = self.parse_expression_until_comma_parenthesis_end()?;
                        arguments.push(
                            Argument::new(Some(name), expr)
                        );
                    } else {
                        self.rewind_skip_whitespace();
                        let expr = self.parse_expression_until_comma_parenthesis_end()?;
                        arguments.push(
                            Argument::new(None, expr)
                        );
                    }

                    let next = self.next_skip_whitespace_and_line_break(false)?;
                    match next {
                        Some(SimpleToken::Symbol(Symbol::Comma)) =>
                            continue,
                        Some(SimpleToken::Symbol(Symbol::ParenthesisEnd)) =>
                            break,
                        _ =>
                            return Err(ParseError(
                                format!("Received invalid symbol during next_function_call: {:?}", next)
                            ))
                    }
                }
            }

            _ => return Err(ParseError(
                format!("Expected start of parenthesis when parsing parse_function_call, got: {:?}", current)
            ))
        }

        Ok(FunctionCall::new(function_name.to_string(), arguments))
    }

    fn parse_block_header(&mut self, block_header: BlockHeader) -> CustomResult<BlockHeader> {
        match block_header {
            BlockHeader::Function(_) =>
                self.parse_function_header(),

            BlockHeader::Class(_) =>
                self.parse_class_header(),

            BlockHeader::Enum(_) =>
                self.parse_enum_header(),

            BlockHeader::Interface(_) =>
                self.parse_interface_header(),

            BlockHeader::If(_) =>
                Ok(BlockHeader::If(Some(self.parse_expression_header()?))),

            BlockHeader::Else(_) => {
                if self.peek_skip_whitespace() == Some(SimpleToken::Symbol(Symbol::Colon)) {
                    Ok(BlockHeader::Else(None))
                } else {
                    Ok(BlockHeader::Else(Some(self.parse_expression_header()?)))
                }
            }

            BlockHeader::Match(_) =>
                Ok(BlockHeader::Match(Some(self.parse_expression_header()?))),

            BlockHeader::For(_) => {
                // TODO: Allow for comma separated list of loops.
                let expr = self.parse_expression_header()?;
                if let Expression::Operation(Operation::BinaryOperation(Some(binary_operation))) = &expr {
                    if binary_operation.operator == BinaryOperator::In {
                        return Ok(BlockHeader::For(Some(binary_operation.clone())));
                    }
                }

                Err(ParseError(
                    format!(
                        "Unable to parse and match \"for\" expression. Expected: In expression, got: {:?}",
                        expr
                    )
                ))
            }

            BlockHeader::With(_) => {
                let expressions = self.parse_expression_list_until(
                    &|simple_token|
                        if let SimpleToken::Symbol(Symbol::Colon) = simple_token {
                            true
                        } else {
                            false
                        }
                )?;
                Ok(BlockHeader::With(Some(expressions)))
            }

            _ => Err(ParseError(
                format!("Received invalid block_header in parse_statement: {:?}", block_header)
            )),
        }
    }

    // Allows linebreak:
    //      before and after ParenthesisBegin
    //      before and after Comma
    //      before and after ParenthesisEnd
    fn parse_function_header(&mut self) -> CustomResult<BlockHeader> {
        let function_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;
        let mut parameters = Vec::new();
        let return_type;

        let next = self.next_skip_whitespace_and_line_break(false)?;
        if let Some(SimpleToken::Symbol(Symbol::ParenthesisBegin)) = next {

            // Loop through the parameters and parse one at a time until the end parenthesis.
            loop {

                // Edge case if the function has no arguments.
                let peek = self.peek_skip_whitespace_and_line_break()?;
                if let SimpleToken::Symbol(Symbol::ParenthesisEnd) = peek {
                    self.next_skip_whitespace_and_line_break(false)?; // Remove end parenthesis.
                    break;
                }

                self.skip_whitespace_and_line_break(false)?;
                let mut current_parameter = self.parse_variable()?;

                // If true: this parameter has a default value set.
                if let Some(SimpleToken::Symbol(Symbol::Equals)) = self.peek_skip_whitespace() {
                    // TODO: parse_expression breaks at end of parenthesis.
                    //  Fix so that parenthesis is allowed inside the expressions.
                    self.next_skip_whitespace();    // Remove Equals symbol.
                    let expression = self.parse_expression_until_comma_parenthesis_end()?;

                    current_parameter.value = Some(Box::new(expression));
                }

                parameters.push(current_parameter);

                // Removes the "current" token, either the Comma or the ParenthesisEnd.
                let current = self.next_skip_whitespace();
                if let Some(SimpleToken::Symbol(Symbol::Comma)) = current {
                    continue;
                } else if let Some(SimpleToken::Symbol(Symbol::ParenthesisEnd)) = current {
                    // The whole "parameter list" have been parsed, break.
                    break;
                } else {
                    return Err(ParseError(
                        "Didn't parse either a Comma or ParenthesisEnd \
                             when expecting next parameter item.".to_string()
                    ));
                }
            }

            // Parse return type if specified, else it will be set to None(void).
            return_type =
                if let Some(SimpleToken::Symbol(Symbol::At)) = self.peek_skip_whitespace() {
                    self.next_skip_whitespace();    // Remove At symbol.
                    self.next_type()?
                } else {
                    Type::new(None, Vec::new())
                };

            self.assert_block_header_end()?;

            Ok(BlockHeader::Function(Some(
                Function::new(function_name, generics, parameters, return_type)
            )))
        } else {
            Err(ParseError(
                "Expected ParenthesisBegin when parsing next_function_header.".to_string()
            ))
        }
    }

    fn parse_class_header(&mut self) -> CustomResult<BlockHeader> {
        let class_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;
        let mut implements = Vec::new();

        // See if this class implements any interfaces.
        let peek = self.peek_skip_whitespace();
        if let Some(SimpleToken::Symbol(Symbol::Is)) = peek {
            self.next_skip_whitespace();    // Remove Is symbol.
            implements = self.next_type_list()?;
        }

        self.assert_block_header_end()?;

        Ok(BlockHeader::Class(Some(
            Class::new(class_name, generics, implements)
        )))
    }

    fn parse_enum_header(&mut self) -> CustomResult<BlockHeader> {
        let interface_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        self.assert_block_header_end()?;

        Ok(BlockHeader::Enum(Some(
            Enum::new(interface_name, generics)
        )))
    }

    fn parse_interface_header(&mut self) -> CustomResult<BlockHeader> {
        let interface_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        self.assert_block_header_end()?;

        Ok(BlockHeader::Interface(Some(
            Interface::new(interface_name, generics)
        )))
    }

    fn parse_expression_header(&mut self) -> CustomResult<Expression> {
        let expression = self.parse_expression_until_colon()?;
        self.assert_block_header_end()?;
        Ok(expression)
    }
}