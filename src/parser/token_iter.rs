use crate::error::CustomError::ParseError;
use crate::lexer::simple_token::{SimpleToken, Symbol};
use crate::CustomResult;
use crate::parser::token::{Token, Statement, BlockHeader, Expression, FunctionCall, Variable};
use crate::parser::token::Statement::{BlockHeader as BlockHeaderEnum, NonBlockHeader as BlockHeaderEnum};
use crate::parser::token::BlockHeader::Function;
use crate::error::CustomError;
use crate::parser::token::Expression::FunctionCall;
use crate::lexer::simple_token_iter::SimpleTokenIter;

pub struct TokenIter<'a> {
    simple_tokens: &'a [SimpleToken],
    position: usize,
    // Symbol::WhiteSpace(usize)
    current_indent: Symbol,
}

impl<'a> TokenIter<'a> {
    pub fn new(simple_tokens: &'a [SimpleToken]) -> Self {
        TokenIter {
            simple_tokens,
            position: 0,
            current_indent: Symbol::WhiteSpace(0),
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
        match result {
            SimpleToken::Symbol(Symbol::WhiteSpace(_)) =>
                self.next(),
            _ =>
                result,
        }
    }

    #[inline]
    fn peek(&mut self) -> SimpleToken {
        self.simple_tokens[self.position + 1].clone()
    }

    #[inline]
    fn peek_skip_whitespace(&mut self) -> SimpleToken {
        // Peek next, but ignore and skip white space.
        let next = self.peek();
        match next {
            SimpleToken::Symbol(Symbol::WhiteSpace(_)) =>
                self.simple_tokens[self.position + 2].clone(),
            _ =>
                next,
        }
    }

    pub fn next_token(&mut self) -> CustomResult<Token> {
        let mut ast = Vec::new();

        loop {
            let current = self.next();
            match current {
                SimpleToken::Identifier(identifier) => {
                    // If true: A reserved keyword, else: a function call, variable etc.
                    if let Some(statement) = Token::lookup_identifier(&identifier) {
                        match statement {
                            BlockHeaderEnum(Function(_)) => {
                                let function_name = self.next_identifier();
                                ast.push(Function(Some()))
                            }
                            NonBlockHeaderEnum(non_block_header) =>,
                            _ => {}
                        }
                    } else {
                        let next_simple_token = self.peek_skip_whitespace();
                        match next_simple_token {
                            SimpleToken::Symbol(Symbol::ParenthesisBegin) => {
                                // This is a function call.
                                self.parse_function_call(&identifier);
                            }
                            _ => {
                                // Treat it as an variable.
                            }
                        }
                    }
                }
                SimpleToken::Symbol(symbol) =>,
                SimpleToken::Number(number) =>,
                SimpleToken::EndOfFile =>,
                SimpleToken::Unknown(unknown) => // Err,
            }
        }


        if let Some((c, c_next)) = self.next() {
            if TokenIter::valid_identifier_start(c) {
                let id: String = self.get_identifier_string()?;

                // Check if this a symbol that have a valid identifier name(and, or, not).
                if let Some(symbol_token) = Symbol::lookup_identifier(&id) {
                    Ok(symbol_token)
                } else {
                    Ok(SimpleToken::Identifier(id))
                }
            } else if TokenIter::valid_number(c, radix) {
                self.get_number(radix)
            } else if TokenIter::valid_linebreak(c, c_next) {
                self.get_linebreak()
            } else if TokenIter::valid_whitespace(c) {
                self.get_whitespaces()
            } else if let Some(symbol_token) = Symbol::lookup(c, c_next) {
                let (token, n) = symbol_token;
                self.skip(n);
                Ok(token)
            } else {
                Err(LexError(
                    format!("Didn't match a symbol token when c: {:?} and c_next: {:?}", c, c_next)
                ))
            }
        } else {
            Ok(SimpleToken::EndOfFile)
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

    fn next_expression(&mut self) -> CustomResult<Expression> {
        let token = self.next_token()?;
        if let Token::Expression(expr_result) = token {
            Ok(expr_result)
        } else {
            Err(CustomError::ParseError(
                format!("Did not get expression when in next_expression: {:?}", token)
            ))
        }
    }

    fn next_identifier(&mut self) -> CustomResult<String> {
        let simple_token = self.next_skip_whitespace();
        if let SimpleToken::Identifier(identifier) = simple_token {
            Ok(identifier)
        } else {
            Err(CustomError::ParseError(
                format!("Did not get an identifier when in next_identifier: {:?}", simple_token)
            ))
        }
    }

    // Valid formats:
    //      id        (Only if it is proceeded with the "var" keyword)
    //      id @ Type
    //      id = expression
    //      id @ Type = expression
    //      (id1, idN) = expression
    //      (id1, idN) @ Type = expression
    // TODO: Might add "name, nameN" for declaring multiple variables on the same line.
    fn next_variable(&mut self) -> CustomResult<Variable> {
        let mut identifier = self.next_identifier()?;
        // Make sure that the variable doesn't have a name that clashes with a keyword.
        if let Some(keyword) = Token::lookup_identifier(&identifier) {
            return Err(CustomError::ParseError(
                format!("A keyword was used as variable name: {:?}", keyword)
            ));
        }

        // TODO: Add tuple support.
        let mut variable = Variable::new(identifier, None, None);
        let mut current = self.next_skip_whitespace();
        match current {
            SimpleToken::Symbol(Symbol::At) => {
                let var_type = self.next_skip_whitespace();
                if let SimpleToken::Identifier(type_name) = var_type {

                } else {
                    return Err(CustomError::ParseError(
                        format!("Didn't parse identifier when expecting type in next_variable: {:?}", var_type)
                    ));
                }
            },
            SimpleToken::Symbol(Symbol::Equals) => ,
            _ => ,
        }
    }

    /*
    fn next_tuple(&mut self) -> CustomResult {

    }
    */

    fn parse_function_call(&mut self, function_name: &str) -> CustomResult<FunctionCall> {
        // The function name has already been parsed and is sent as argument.
        // Start by parsing the first ParenthesisBegin.
        let mut current = self.next_skip_whitespace();
        match current {
            SimpleToken::Symbol(Symbol::ParenthesisBegin) => ,
            _ => Err(CustomError::ParseError(
                format!("Expected start of parenthesis when parsing parse_function_call, got: {:?}", current)
            ))
        }



    }

    fn get_identifier_string(&mut self) -> CustomResult<String> {
        let mut result = String::new();
        while let Some(c) = self.next() {
            if !TokenIter::valid_identifier(c) {
                self.put_back(c);
                break;
            }
            result.push(c);
        }

        if !result.is_empty() {
            Ok(result)
        } else {
            Err(LexError("Empty result in next().".to_string()))
        }
    }

    // TODO: Hex/binary numbers with prefix 0x/0b
    //  Check cast (as) and suffix etc.
    pub fn get_number(&mut self, radix: u32) -> CustomResult<SimpleToken> {
        let mut number = self.get_integer(radix)?;
        if let Some('.') = self.peek() {    // True if float number.
            self.skip(1);                // Remove dot.
            number = [number, self.get_integer(radix)?].join(".");
        }

        Ok(SimpleToken::Number(number))
    }

    fn get_integer(&mut self, radix: u32) -> CustomResult<String> {
        let mut numbers = Vec::new();
        while let Some(c) = self.next() {
            if TokenIter::valid_number(c, radix) {
                numbers.push(c);
            } else {
                self.put_back(c);
                break;
            }
        }

        Ok(numbers.into_iter().collect())
    }

    fn get_linebreak(&mut self) -> CustomResult<SimpleToken> {
        let c = self.next()
            .ok_or(LexError("Reached EOF while parsing first char in get_linebreak.".to_string()))?;

        if c == '\n' {
            Ok(SimpleToken::Symbol(Symbol::LineBreak))
        } else if c == '\r' {
            let c_next = self.next()
                .ok_or(LexError("Reached EOF after a '\\r' had been parsed.".to_string()))?;

            if c_next == '\n' {
                Ok(SimpleToken::Symbol(Symbol::LineBreak))
            } else {
                Err(LexError("Didn't receive a '\\n' after a '\\r'.".to_string()))
            }
        } else {
            Err(LexError("No linebreak character received in get_linebreak.".to_string()))
        }
    }

    fn get_whitespaces(&mut self) -> CustomResult<SimpleToken> {
        let mut count = 0;

        while let Some(c) = self.next() {
            if c.is_whitespace() {
                count += 1;
            } else {
                self.put_back(c);
                break;
            }
        }

        Ok(SimpleToken::Symbol(Symbol::WhiteSpace(count)))
    }
}