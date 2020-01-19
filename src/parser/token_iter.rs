use crate::error::CustomError::ParseError;
use crate::lexer::simple_token::{SimpleToken, Symbol};
use crate::CustomResult;
use crate::parser::token::{Token, Statement, BlockHeader, Expression, FunctionCall, Variable, Type, Function, BinaryOperation, BinaryOperator, Class, Interface, Enum, Argument};
use crate::error::CustomError;


pub struct TokenIter<'a> {
    pub simple_tokens: &'a [SimpleToken],
    pub position: usize,
    pub line_number: usize,

    pub blocks: Vec<Block>,
    pub indent: usize,
    pub indent_fixed_size: usize,
}

pub struct Block {
    pub tokens: Vec<Token>,
    pub header_indent: usize,
}

impl<'a> TokenIter<'a> {
    pub fn new(simple_tokens: &'a [SimpleToken], indent_fixed_size: usize) -> Self {
        let mut current_block = Vec::new();
        current_block.push(
            Block {
                tokens: Vec::new(),
                header_indent: 0,
            }
        );

        TokenIter {
            simple_tokens,
            position: 0,
            line_number: 0,
            blocks: current_block,
            indent: 0,
            indent_fixed_size,
        }
    }

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
    fn peek(&mut self) -> SimpleToken {
        self.simple_tokens[self.position + 1].clone()
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

    // Ensure that the next token is a block end followed by a break symbol.
    fn ensure_block_end(&mut self) -> CustomResult<()> {
        if let SimpleToken::Symbol(Symbol::Colon) = self.next_skip_whitespace() {
            if Symbol::is_break_symbol(self.next_skip_whitespace()) {
                return Ok(());
            }
        }
        Err(ParseError("Incorrect block ending.".to_string()))
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
                    if let Token::Block(_, _) = token {
                        let new_header_indent = self.indent;
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
                SimpleToken::Symbol(symbol) =>,
                SimpleToken::Number(number) =>,
                SimpleToken::EndOfFile =>,
                SimpleToken::Unknown(unknown) => // Err,
            }
        }
    }

    // TODO: Parse expressions with parenthesis.
    fn next_expression(&mut self) -> CustomResult<Expression> {
        let token = self.next_token()?;
        if let Token::Expression(expr_result) = token {
            Ok(expr_result)
        } else {
            Err(ParseError(
                format!("Did not get expression when in next_expression: {:?}", token)
            ))
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
    //      id @ modifier* Type
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

        // If true: this variable has specified type, parse the modifiers and type.
        if let SimpleToken::Symbol(Symbol::At) = self.peek_skip_whitespace() {
            self.next_skip_whitespace();    // Remove "At" symbol.
            self.add_next_modifiers_and_type(&mut variable)?;
        }

        Ok(variable)
    }

    fn add_next_modifiers_and_type(&mut self, variable: &mut Variable) -> CustomResult<()> {
        loop {
            if let SimpleToken::Identifier(identifier) = self.next_skip_whitespace() {

                // This is a modifier, add to "variable" and continue parsing next token.
                // If true: this is a modifier, add to "variable" and continue parsing next token.
                // Else: this is the real "Type", parse and break to return.
                if let Some(Token::Statement(Statement::Modifier(modifier))) = Token::lookup_identifier(&identifier) {
                    variable.modifiers.push(modifier);
                } else {
                    self.rewind();
                    variable.var_type = Some(self.next_type()?);
                    break;
                }
            } else {
                return Err(CustomError::ParseError(
                    "Didn't receive identifier during next_modifiers_and_type".to_string()
                ));
            }
        }

        Ok(())
    }

    fn next_type(&mut self) -> CustomResult<Type> {
        let current = self.next_skip_whitespace();
        if let SimpleToken::Identifier(identifier) = current {
            let generics = self.next_generic_list()?;
            let mut var_type = Type::new(Some(identifier), generics);

            Ok(var_type)
        } else {
            Err(ParseError(
                "Didn't receive an identifier when parsing next_type.".to_string()
            ))
        }
    }

    // PS: If there are no generic list i.e. the first symbol is NOT a PointBracketBegin,
    // returns an empty vector and does NOT return an error.
    fn next_generic_list(&mut self) -> CustomResult<Vec<Type>> {
        let mut generics = Vec::new();

        if let SimpleToken::Symbol(Symbol::PointyBracketBegin) = self.peek_skip_whitespace() {
            self.next_skip_whitespace();    // Remove PointBracketBegin symbol.

            generics = self.next_type_list()?;

            let current = self.next_skip_whitespace();
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

    // Valid formats:
    //      Type
    //      Type1, TypeN
    fn next_type_list(&mut self) -> CustomResult<Vec<Type>> {
        let mut types = Vec::new();

        // Loop through and parse all items in the "type list".
        loop {
            types.push(self.next_type()?);

            // Removes the "current" token, either the Comma or the PointyBracketEnd.
            let current = self.peek_skip_whitespace();
            if let SimpleToken::Symbol(Symbol::Comma) = current {
                self.next_skip_whitespace();    // Remove Comma symbol.
                continue;
            } else {
                break;
            }
        }

        Ok(types)
    }

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

            self.ensure_block_end();

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

        self.ensure_block_end()?;

        Ok(BlockHeader::Class(Some(
            Class::new(class_name, generics, implements)
        )))
    }

    fn next_enum_header(&mut self) -> CustomResult<BlockHeader> {
        let interface_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        self.ensure_block_end()?;

        Ok(BlockHeader::Enum(Some(
            Enum::new(interface_name, generics)
        )))
    }

    fn next_interface_header(&mut self) -> CustomResult<BlockHeader> {
        let interface_name = self.next_identifier()?;
        let generics = self.next_generic_list()?;

        self.ensure_block_end()?;

        Ok(BlockHeader::Interface(Some(
            Interface::new(interface_name, generics)
        )))
    }

    // Path is used in "use" and "package".
    // Valid format:
    //      id1
    //      id1.iN      (A list of identifiers separated by dots)
    fn next_path(&mut self) -> CustomResult<String> {
        let mut path = Vec::new();

        // Loop through the path one identifier at a time.
        loop {
            path.push(self.next_identifier()?);

            let peek = self.peek_skip_whitespace();
            if let SimpleToken::Symbol(Symbol::Dot) = peek {
                self.next_skip_whitespace();    // Remove Dot symbol.
                continue;
            } else if Symbol::is_break_symbol(peek) {
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


    fn parse_function_call(&mut self, function_name: String) -> CustomResult<FunctionCall> {
        let mut arguments = Vec::new();

        // The function name has already been parsed and is sent as argument.
        // Start by parsing the first ParenthesisBegin.
        let mut current = self.next_skip_whitespace();
        match current {
            SimpleToken::Symbol(Symbol::ParenthesisBegin) => {
                loop {
                    // Start with the assumption that this is a named argument.
                    // Parse identifier and equals sign.
                    // If it is false, redo everything with the assumption that it is not a named argument.
                    let name = self.next_identifier()?;
                    let peek = self.peek_skip_whitespace();

                    // If true: This is a named argument,
                    // Else: This is a regular argument.
                    if let SimpleToken::Symbol(Symbol::Equals) = peek {
                        self.next_skip_whitespace();    // Remove Equals symbol.
                        let expression = self.next_expression()?;
                        arguments.push(
                            Argument::new(Some(name), expression)
                        );
                    } else {
                        self.rewind_skip_whitespace();
                        let expression = self.next_expression()?;
                        arguments.push(
                            Argument::new(None, expression)
                        );
                    }

                    let next = self.next_skip_whitespace();
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

        Ok(FunctionCall { name: function_name, arguments })
    }

    fn parse_identifier(&mut self, identifier: &str) -> CustomResult<Token> {
        // If true: A reserved keyword (either statement or header).
        // Else: a function call, variable etc.
        if let Some(token) = Token::lookup_identifier(&identifier) {
            match token {
                Token::Statement(statement) =>
                    Ok(Token::Statement(self.parse_statement(statement)?)),

                Token::Block(block_header, _) =>
                    Ok(Token::Block(self.parse_block_header(block_header)?, Vec::new())),

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
                if Symbol::is_break_symbol(self.peek_skip_whitespace()) {
                    Ok(Statement::Return(None))
                } else {
                    Ok(Statement::Return(Some(self.next_expression()?)))
                }
            }

            Statement::Yield(_) =>
                Ok(Statement::Yield(Some(self.next_expression()?))),

            Statement::Break | Statement::Continue | Statement::Next =>
                Ok(statement),

            Statement::Use(_) =>
                Ok(Statement::Use(Some(self.next_path()?))),

            Statement::Package(_) =>
                Ok(Statement::Package(Some(self.next_path()?))),

            Statement::Throw(_) =>
                Ok(Statement::Throw(Some(self.next_expression()?))),

            // Ignore modifiers.
            _ => Err(ParseError(
                format!("Received invalid statement in parse_statement: {:?}", statement)
            )),
        }
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
                Ok(BlockHeader::If(Some(self.next_expression()?))),

            BlockHeader::Else(_) => {
                if self.peek_skip_whitespace() == SimpleToken::Symbol(Symbol::Colon) {
                    Ok(BlockHeader::Else(None))
                } else {
                    Ok(BlockHeader::Else(Some(self.next_expression()?)))
                }
            }

            BlockHeader::Match(_) =>
                Ok(BlockHeader::Match(Some(self.next_expression()?))),

            BlockHeader::For(_) =>
                Ok(BlockHeader::For(Some(self.next_expression()?))),

            Statement::Return(_) => {
                let peek = self.peek_skip_whitespace();
                // If true: Next symbol is a break which means that this return-statement
                // doesn't return anything.
                // Else: It returns the next expression.
                if Symbol::is_break_symbol(peek) {
                    Ok(Statement::Return(None))
                } else {
                    Ok(Statement::Return(Some(self.next_expression()?)))
                }
            }

            Statement::Yield(_) =>
                Ok(Statement::Yield(Some(self.next_expression()?))),

            Statement::Break | Statement::Continue | Statement::Next =>
                Ok(statement),

            Statement::Use(_) =>
                Ok(Statement::Use(Some(self.next_path()?))),

            Statement::Package(_) =>
                Ok(Statement::Package(Some(self.next_path()?))),

            Statement::Throw(_) =>
                Ok(Statement::Throw(Some(self.next_expression()?))),

            // Ignore modifiers.
            _ => Err(ParseError(
                format!("Received invalid statement in parse_statement: {:?}", statement)
            )),
        }
    }
}