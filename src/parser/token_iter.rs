use crate::error::CustomError::ParseError;
use crate::lexer::simple_token::{SimpleToken, Symbol, Literal as SimpleLiteral};
use crate::CustomResult;
use crate::parser::token::{Token, Statement, BlockHeader, Expression, FunctionCall, Variable, Type, Function, BinaryOperation, BinaryOperator, Class, Interface, Enum, Argument, Block, Literal, Operator, Operation};
use crate::error::CustomError;
use crate::parser::action_tree::ActionTree;
use crate::parser::token::Expression::Literal;

// TODO: Let line breaks be OK sometimes and treated as whitespace,
//  ex. when having a long parameter list or argument list.

pub struct TokenIter<'a> {
    pub simple_tokens: &'a [SimpleToken],
    pub position: usize,
    pub line_number: usize,

    //pub tree: ActionTree,
    pub indent_fixed_size: usize,
}

#[derive(Debug)]
pub enum Output {
    Operator(Operator),
    Value(Expression),
}

impl<'a> TokenIter<'a> {
    pub fn new(simple_tokens: &'a [SimpleToken], indent_fixed_size: usize) -> Self {
        /*
        let mut current_block = Vec::new();
        current_block.push(
            Block {
                header: BlockHeader::Default,
                header_indent: 0,
                body_indent: 0,
                tokens: Vec::new(),
            }
        );
        */

        TokenIter {
            simple_tokens,
            position: 0,
            line_number: 0,

            indent_fixed_size,
        }
    }

    // FIXME: Possibility for out of bounds.
    #[inline]
    fn next(&mut self) -> SimpleToken {
        let result = self.simple_tokens[self.position].clone();
        self.position += 1;
        result
    }

    #[inline]
    fn next_skip_whitespace(&mut self) -> SimpleToken {
        let result = self.next();
        if let SimpleToken::Symbol(Symbol::WhiteSpace(_)) = result {
            self.next()
        } else {
            result
        }
    }

    #[inline]
    fn next_skip_whitespace_and_line_break(&mut self) -> SimpleToken {
        self.skip_whitespace_and_line_break();
        self.next()
    }

    #[inline]
    fn skip_whitespace_and_line_break(&mut self) {
        loop {
            let result = self.next();
            match result {
                | SimpleToken::Symbol(Symbol::WhiteSpace(_))
                | SimpleToken::Symbol(Symbol::LineBreak) => continue,
                _ =>
                    self.rewind(),
            }
        }
    }

    #[inline]
    fn peek(&mut self) -> SimpleToken {
        self.peek_n(1)
    }

    #[inline]
    fn peek_n(&mut self, n: usize) -> SimpleToken {
        self.simple_tokens[self.position + n].clone()
    }

    #[inline]
    fn peek_skip_whitespace(&mut self) -> SimpleToken {
        let result = self.peek();
        if let SimpleToken::Symbol(Symbol::WhiteSpace(_)) = result {
            self.simple_tokens[self.position + 2].clone()
        } else {
            result
        }
    }

    #[inline]
    fn peek_skip_whitespace_and_line_break(&mut self) -> SimpleToken {
        let mut peek_amount = 1;
        loop {
            let result = self.peek_n(peek_amount);
            match result {
                | SimpleToken::Symbol(Symbol::WhiteSpace(_))
                | SimpleToken::Symbol(Symbol::LineBreak) => {
                    peek_amount += 1;
                    continue;
                }
                _ => return result
            }
        }
    }

    #[inline]
    fn peek_prev(&mut self) -> SimpleToken {
        self.simple_tokens[self.position - 1].clone()
    }

    #[inline]
    fn peek_prev_skip_whitespace(&mut self) -> SimpleToken {
        let result = self.peek_prev();
        if let SimpleToken::Symbol(Symbol::WhiteSpace(_)) = result {
            self.simple_tokens[self.position - 2].clone()
        } else {
            result
        }
    }

    #[inline]
    fn rewind(&mut self) {
        self.position -= 1;
    }

    #[inline]
    fn rewind_skip_whitespace(&mut self) {
        self.position -= 1;
        if let SimpleToken::Symbol(Symbol::WhiteSpace(_)) = self.peek() {
            self.position -= 1;
        }
    }

    #[inline]
    pub fn valid_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    #[inline]
    pub fn valid_identifier(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    #[inline]
    fn valid_number(c: char, radix: u32) -> bool {
        c.is_digit(radix)
    }

    #[inline]
    fn valid_linebreak(c: char, c_next: Option<char>) -> bool {
        c == '\n' || (c == '\r' && c_next.map_or(false, |x| x == '\n'))
    }

    // char.is_whitespace() includes linebreaks, so need to check valid_linebreak first.
    #[inline]
    fn valid_whitespace(c: char) -> bool {
        c.is_whitespace()
    }

    fn current_block(&mut self) -> &Block {
        let last = self.blocks.len() - 1;
        &self.blocks[last]
    }

    // Ensure that the next token is a block header end.
    fn ensure_block_header_end(&mut self) -> CustomResult<()> {
        if let SimpleToken::Symbol(Symbol::Colon) = self.next_skip_whitespace() {
            Ok(())
        } else {
            Err(ParseError("Incorrect block ending.".to_string()))
        }
    }

    /*
    fn new_line_update_indents_and_line_number(&mut self) -> CustomResult<()> {
        // Ensure that indent size is valid.
        let indent = self.next_whitespace();
        if indent % self.indent_fixed_size != 0 {
            return Err(ParseError(
                format!("Current indent not divisible by specified fixed indent. Fixed: {}, got: {}",
                        self.indent_fixed_size, indent
                )
            ));
        }

        let current_block = self.current_block();


        // Ensure that indent size is not to large, i.e. bigger than header_indent + fixed_size.
        // It can be as small as it want though, since the previous "if-check" ensures
        // that the indent is a valid indentation relative to fixed_size.
        if indent - self.indent_fixed_size > current_block.header_indent
        {
            return Err(ParseError(
                format!("Current indent not not valid relative to header. Header: {}, current: {}",
                        current_block.header_indent, indent
                )
            ));
        }

        self.line_number += 1;
    }

    fn update_indents(&mut self, new_indent: usize) -> CustomResult<()> {
        self.
    }
        */

    // Get SimpleTokens up to (including) the next break symbol.
    fn next_group(&mut self) -> Vec<SimpleToken> {
        let mut group = Vec::new();

        loop {
            let current = self.next();

            if current == Symbol::LineStart {
                self.new_line_update_indents_and_line_number();
            }

            if Symbol::is_break_symbol(&current) {
                break;
            } else {
                group.push(current);
            }
        }

        group
    }

    pub fn next_token(&mut self) -> CustomResult<Token> {
        loop {
            let current = self.next();
            match current {
                SimpleToken::Identifier(identifier) => {
                    let token = self.parse_identifier(&identifier)?;

                    // If Block/BlockHeader: Add the new block to the iterator
                    //      so that new Tokens get put into this block.
                    // Else: Add the new token into this block.
                    if let Token::Block(_) = token {
                        let new_header_indent = self.;
                        self.blocks.push(
                            Block {
                                tokens: Vec::new(),
                                header_indent: new_header_indent,
                            }
                        );
                    } else {
                        let last = self.blocks.len() - 1;
                        self.blocks[last].tokens.push(token);
                    }
                }

                // TODO: Make sure to update indent size when getting LineBreak.
                | SimpleToken::Symbol(symbol)
                | SimpleToken::Number(number) => self.parse_expression(),
                SimpleToken::EndOfFile =>,
                SimpleToken::Unknown(unknown) => // Err,
            }
        }
    }

    fn next_identifier(&mut self) -> CustomResult<String> {
        let simple_token = self.next_skip_whitespace();
        if let SimpleToken::Identifier(identifier) = simple_token {
            Ok(identifier)
        } else {
            Err(ParseError(
                format!("Did not get an identifier when in next_identifier: {:?}", simple_token)
            ))
        }
    }

    // Valid formats:
    //      id
    //      id @ Type modifier*
    // (The assignment is handled separately, ex. id = expression)
    // TODO: Add "name, nameN" for declaring multiple variables on the same line.
    // TODO: Add support for tuples.
    fn next_variable(&mut self) -> CustomResult<Variable> {
        let mut identifier = self.next_identifier()?;
        let mut variable = Variable::new(identifier, None, None, Vec::new());

        // Make sure that the variable doesn't have a name that clashes with a keyword.
        if let Some(keyword) = Token::lookup_identifier(&identifier) {
            return Err(CustomError::ParseError(
                format!("A keyword was used as variable name: {:?}", keyword)
            ));
        }

        // If true: this variable has specified type, parse the type and modifiers.
        if let SimpleToken::Symbol(Symbol::At) = self.peek_skip_whitespace() {
            self.next_skip_whitespace();    // Remove "At" symbol.
            self.add_next_type_and_modifiers(&mut variable)?;
        }

        Ok(variable)
    }

    fn next_whitespace(&mut self) -> usize {
        if let Symbol::WhiteSpace(size) = self.next() {
            size
        } else {
            self.rewind();
            0
        }
    }

    fn add_next_type_and_modifiers(&mut self, variable: &mut Variable) -> CustomResult<()> {
        // Parse the type, which have to be the first "string".
        let var_type = self.next_type()?;
        variable.var_type = Some(var_type);

        loop {
            if let SimpleToken::Identifier(identifier) = self.next_skip_whitespace() {

                // If true: this is a modifier, add to "variable" and continue parsing next token.
                if let Some(Token::Statement(Statement::Modifier(modifier))) = Token::lookup_identifier(&identifier) {
                    variable.modifiers.push(modifier);
                } else {
                    break;
                }
            } else {
                break;
            }
        }

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
            self.skip_whitespace_and_line_break();
            types.push(self.next_type()?);

            // Removes the "current" token, either the Comma or the PointyBracketEnd.
            let current = self.peek_skip_whitespace_and_line_break();
            if let SimpleToken::Symbol(Symbol::Comma) = current {
                self.next_skip_whitespace_and_line_break();    // Remove Comma symbol.
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

        if let SimpleToken::Symbol(Symbol::PointyBracketBegin) = self.peek_skip_whitespace() {
            self.next_skip_whitespace();    // Remove PointBracketBegin symbol.

            generics = self.next_type_list()?;

            let current = self.next_skip_whitespace_and_line_break();
            if let SimpleToken::Symbol(Symbol::PointyBracketEnd) = current {
                // Everything alright.
            } else {
                return Err(ParseError(
                    format!("Expected a PointBracketEnd after parsing generic list, got: {:?}", current)
                ));
            }
        }

        Ok(generics)
    }

    // TODO: allow linebreaks between args/parameters.
    fn next_function_header(&mut self) -> CustomResult<BlockHeader> {
        let function_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;
        let mut parameters = Vec::new();
        let mut return_type;

        if let SimpleToken::Symbol(Symbol::ParenthesisBegin) = self.next_skip_whitespace() {

            // Loop through the parameters and parse one at a time until the end parenthesis.
            loop {
                let mut current_parameter = self.next_variable()?;

                // If true: this parameter has a default value set.
                if let SimpleToken::Symbol(Symbol::Equals) = self.peek_skip_whitespace() {
                    self.next_skip_whitespace();    // Remove Equals symbol.
                    current_parameter.value = Some(Box::new(self.next_expression()?));
                }

                // Removes the "current" token, either the Comma or the ParenthesisEnd.
                let current = self.next_skip_whitespace();
                if let SimpleToken::Symbol(Symbol::Comma) = current {
                    continue;
                } else if let SimpleToken::Symbol(Symbol::ParenthesisEnd) = current {
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
                if let SimpleToken::Symbol(Symbol::At) = self.peek_skip_whitespace() {
                    self.peek_skip_whitespace();    // Remove At symbol.
                    self.next_type()?
                } else {
                    Type::new(None, Vec::new())
                };

            self.ensure_block_header_end();

            Ok(BlockHeader::Function(Some(
                Function::new(function_name, generics, parameters, return_type)
            )))
        } else {
            Err(ParseError(
                "Expected ParenthesisBegin when parsing next_function_header.".to_string()
            ))
        }
    }

    fn next_class_header(&mut self) -> CustomResult<BlockHeader> {
        let class_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;
        let mut implements = Vec::new();

        // TODO: Fix hardcoded value "is" !!!
        // See if this class implements any interfaces.
        let peek = self.peek_skip_whitespace();
        if let SimpleToken::Identifier(id) = peek {
            if id == "is".to_string() {
                implements = self.next_type_list()?;
            }
        }

        self.ensure_block_header_end()?;

        Ok(BlockHeader::Class(Some(
            Class::new(class_name, generics, implements)
        )))
    }

    fn next_enum_header(&mut self) -> CustomResult<BlockHeader> {
        let interface_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        self.ensure_block_header_end()?;

        Ok(BlockHeader::Enum(Some(
            Enum::new(interface_name, generics)
        )))
    }

    fn next_interface_header(&mut self) -> CustomResult<BlockHeader> {
        let interface_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        self.ensure_block_header_end()?;

        Ok(BlockHeader::Interface(Some(
            Interface::new(interface_name, generics)
        )))
    }

    // Path is used in "use" and "package".
    // Valid format:
    //      id1
    //      id1.iN      (A list of identifiers separated by dots)
    fn parse_path(&mut self) -> CustomResult<String> {
        let mut path = Vec::new();

        // Loop through the path one identifier at a time.
        loop {
            path.push(self.next_identifier()?);

            let peek = self.peek_skip_whitespace();
            if let SimpleToken::Symbol(Symbol::Dot) = peek {
                self.next_skip_whitespace();    // Remove Dot symbol.
                continue;
            } else if Symbol::is_break_symbol(&peek) {
                break;
            } else {
                return Err(ParseError(
                    format!("Incorrect symbol found during parsing of next_path: {:?}", peek)
                ));
            }
        }

        Ok(path.join("."))
    }

    // Can be preceded with var, let or set.
    // Valid format:
    //      variable = expression
    //      variable = [LINEBREAK] expression
    // This function takes an already parsed variable as argument,
    // and starts parsing at the Equals sign.
    fn next_assignment_to_variable(&mut self, variable: Variable) -> CustomResult<BinaryOperation> {
        let current = self.next();
        if let SimpleToken::Symbol(Symbol::Equals) = current {} else {
            return Err(ParseError(
                format!("Expected assignment symbol in next_assignment: {:?}", current)
            ));
        }

        let left = Expression::Variable(Some(variable));
        let right = self.next_expression()?;
        Ok(
            BinaryOperation::new(BinaryOperator::Assignment, Box::new(left), Box::new(right))
        )
    }

    // Parses the variable before parsing the assignment.
    fn next_assignment(&mut self) -> CustomResult<BinaryOperation> {
        self.next_assignment_to_variable(self.next_variable()?)
    }

    fn next_binary_operation(&mut self) -> CustomResult<BinaryOperation> {}

    /*
    fn next_tuple(&mut self) -> CustomResult {

    }
    */

    fn parse_identifier(&mut self, identifier: &str) -> CustomResult<Token> {
        // If true: A reserved keyword (either statement or header).
        // Else: a function call, variable etc.
        if let Some(token) = Token::lookup_identifier(&identifier) {
            match token {
                Token::Statement(statement) =>
                    Ok(Token::Statement(self.parse_statement(statement)?)),

                Token::Block(block) => {
                    Ok(Token::Block(
                        Block::new_empty(self.parse_block_header(block.header)?)
                    ))
                }

                _ =>
                    Err(ParseError(
                        format!("Received incorrect token during parse_identifier: {:?}", token)
                    ))
            }
        } else {
            let next_simple_token = self.peek_skip_whitespace();
            match next_simple_token {
                SimpleToken::Symbol(Symbol::ParenthesisBegin) => {
                    // This is a function call.
                    self.parse_function_call(&identifier);
                }
                _ => {
                    self.next_variable()?;
                    // Treat it as an variable.
                }
            }
        }
    }

    fn parse_statement(&mut self, statement: Statement) -> CustomResult<Statement> {
        match statement {
            Statement::Return(_) => {
                // If true: Next symbol is a break which means that this return-statement
                // doesn't return anything.
                // Else: It returns the next expression.
                if Symbol::is_break_symbol(&self.peek_skip_whitespace()) {
                    Ok(Statement::Return(None))
                } else {
                    Ok(Statement::Return(Some(self.parse_expression()?)))
                }
            }

            Statement::Yield(_) =>
                Ok(Statement::Yield(Some(self.parse_expression()?))),

            Statement::Break | Statement::Continue =>
                Ok(statement),

            Statement::Use(_) =>
                Ok(Statement::Use(Some(self.parse_path()?))),

            Statement::Package(_) =>
                Ok(Statement::Package(Some(self.parse_path()?))),

            Statement::Throw(_) =>
                Ok(Statement::Throw(Some(self.parse_expression()?))),

            // Ignore modifiers.
            _ => Err(ParseError(
                format!("Received invalid statement in parse_statement: {:?}", statement)
            )),
        }
    }

    // Shunting-yard algorithm
    fn parse_expression(&mut self) -> CustomResult<Expression> {
        let mut output_stack: Vec<Output> = Vec::new();
        let mut operand_stack: Vec<Operator> = Vec::new();

        // This bool is used to ensure that all "values" are separated by operands.
        // "value" == not an operand, i.e. literals, numbers, variables or function calls.
        let mut previous_was_value = false;

        loop {
            let current = self.next_skip_whitespace();
            match current {
                SimpleToken::Identifier(identifier) => {
                    if previous_was_value {
                        return Err(ParseError("Received two \"values\" in a row in parse_expression.".to_string()));
                    }

                    if let SimpleToken::Symbol(Symbol::ParenthesisBegin) = self.peek_skip_whitespace() {
                        // This is a function call
                        let function_call = self.parse_function_call(&identifier)?;
                        output_stack.push(
                            Output::Value(Expression::FunctionCall(Some(function_call)))
                        );
                    } else {
                        // Not a function call, treat it as an variable
                        let variable = Variable::new(identifier, None, None, Vec::new());
                        output_stack.push(Output::Value(Expression::Variable(Some(variable))));
                    }

                    previous_was_value = true;
                }

                SimpleToken::Number(number_string) => {
                    if previous_was_value {
                        return Err(ParseError("Received two \"values\" in a row in parse_expression.".to_string()));
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
                        return Err(ParseError("Received two \"values\" in a row in parse_expression.".to_string()));
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
                    if Symbol::is_break_symbol(&current) {
                        break;
                    } else if let SimpleToken::Symbol(Symbol::Colon) = current {
                        // Break on header end.
                        break;
                    }

                    let operator = Token::lookup_operator(symbol)
                        .ok_or(ParseError("Parsed None operator during expression.".to_string()))?;
                    match operator {
                        Operator::ParenthesisBegin =>
                            operand_stack.push(operator),

                        Operator::ParenthesisEnd => {
                            loop {
                                let popped_operator = operand_stack.pop()
                                    .ok_or(ParseError(
                                        "Exhausted operator stack while looking for parenthesisBegin.".to_string()
                                    ))?;

                                if let Operator::ParenthesisBegin = popped_operator {
                                    break;
                                } else {
                                    output_stack.push(Output::Operator(popped_operator));
                                }
                            }
                        }

                        _ => {
                            // Rest of the operators that aren't parenthesis.
                            let operator_prec = operator.precedence()
                                .ok_or(ParseError(
                                    format!("Unable to see precedence for current: {:?}", current)
                                ))?;

                            loop {
                                if let Some(popped_operator) = operand_stack.pop() {
                                    let popped_operator_prec = popped_operator.precedence()
                                        .ok_or(ParseError(
                                            format!(
                                                "Unable to see precedence for popped_operator: {:?}",
                                                popped_operator
                                            )
                                        ))?;

                                    // "popped operator" higher precedence that "current operator".
                                    if operator_prec > popped_operator_prec
                                        ||
                                        (operator_prec == popped_operator_prec
                                            && popped_operator.evaluate_left_to_right().ok_or(
                                            ParseError("Unable to see if operator eval left to right."
                                                .to_string())
                                        )?)
                                    {
                                        output_stack.push(Output::Operator(popped_operator));
                                        continue;
                                    } else {
                                        operand_stack.push(popped_operator);    // Put back
                                        break;
                                    }
                                } else {
                                    break;
                                }
                            }

                            operand_stack.push(operator);
                        }
                    }

                    previous_was_value = false;
                }

                // Ignore modifiers.
                _ => return Err(ParseError(
                    format!("Received invalid simple_token in parse_expression: {:?}", current)
                )),
            }
        }

        while let Some(operator) = operand_stack.pop() {
            output_stack.push(Output::Operator(operator));
        }

        // The output stack now contains the expression in Reverse Polish Notation.
        println!("OutputStack: \n{:?}", output_stack);

        // TODO: Temporary place holder
        //self.polish_to_expression(output_stack)
        Ok(Expression::Integer(Some("123".to_string())))
    }

    /*
    fn polish_to_expression(&self, output_stack: Vec<Output>) -> CustomResult<Expression> {

    }

*/
    fn parse_function_call(&mut self, function_name: &str) -> CustomResult<FunctionCall> {
        let mut arguments = Vec::new();

        // The function name has already been parsed and is sent as argument.
        // Start by parsing the first ParenthesisBegin.
        let mut current = self.next_skip_whitespace();
        match current {
            SimpleToken::Symbol(Symbol::ParenthesisBegin) => {
                loop {
                    // Start with the assumption that this is a named argument.
                    // Parse as identifier and peek on the next simple token, that is assumed to be Equals.
                    // If it is false, redo everything with the assumption that it is not a named argument.
                    self.skip_whitespace_and_line_break();  // Allow linebreak.
                    let name = self.next_identifier()?;
                    let peek = self.peek_skip_whitespace();

                    // If true: This is a named argument,
                    // Else: This is a regular argument.
                    // FIXME: Maybe allow line break in front/behind equals sign.
                    if let SimpleToken::Symbol(Symbol::Equals) = self.peek_skip_whitespace() {
                        self.next_skip_whitespace();    // Remove Equals symbol.
                        arguments.push(
                            Argument::new(Some(name), self.parse_expression()?)
                        );
                    } else {
                        self.rewind_skip_whitespace();
                        arguments.push(
                            Argument::new(None, self.parse_expression()?)
                        );
                    }

                    let next = self.next_skip_whitespace_and_line_break();
                    match next {
                        SimpleToken::Symbol(Symbol::Comma) =>
                            continue,
                        SimpleToken::Symbol(Symbol::ParenthesisEnd) =>
                            break,
                        _ =>
                            return Err(ParseError(
                                format!("Received invalid symbol during next_function_call: {:?}", peek)
                            ))
                    }
                }
            }

            _ => return Err(ParseError(
                format!("Expected start of parenthesis when parsing parse_function_call, got: {:?}", current)
            ))
        }

        Ok(FunctionCall { name: function_name.to_string(), arguments })
    }

    fn parse_block_header(&mut self, block_header: BlockHeader) -> CustomResult<BlockHeader> {
        match block_header {
            BlockHeader::AssignBlockHeader(_) =>
                Ok(block_header),

            BlockHeader::Function(_) =>
                self.next_function_header(),

            BlockHeader::Class(_) =>
                self.next_class_header(),

            BlockHeader::Enum(_) =>
                self.next_enum_header(),

            BlockHeader::Interface(_) =>
                self.next_interface_header(),

            BlockHeader::If(_) =>
                Ok(BlockHeader::If(Some(self.parse_expression()?))),

            BlockHeader::Else(_) => {
                if self.peek_skip_whitespace() == SimpleToken::Symbol(Symbol::Colon) {
                    Ok(BlockHeader::Else(None))
                } else {
                    Ok(BlockHeader::Else(Some(self.parse_expression()?)))
                }
            }

            BlockHeader::Match(_) =>
                Ok(BlockHeader::Match(Some(self.parse_expression()?))),

            BlockHeader::For(_) => {
                // TODO: Allow for comma separated list of loops.
                let expr = self.parse_expression()?;
                if let Expression::Operation(Operation::BinaryOperation(Some(binary_operation))) = expr {
                    if binary_operation.operator == BinaryOperator::Assignment {
                        return Ok(BlockHeader::For(Some(binary_operation)));
                    }
                }

                Err(ParseError("Unable to parse and match for expression.".to_string()))
            }

            _ => Err(ParseError(
                format!("Received invalid block_header in parse_statement: {:?}", block_header)
            )),
        }
    }
}