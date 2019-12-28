/*

use std::str::Chars;
use itertools::{PutBack, put_back};

use crate::lexer::CustomResult;
use crate::error::CustomError::LexError;
use crate::token::{Variable, Token, Control, ShortHand, Header};
use std::error::Error;
use std::fs::File;
use std::io::{Read, BufReader};

const BUFFER_SIZE: usize = 1 << 16;

pub struct TokenIter<'a> {
    it: PutBack<Chars<'a>>,
}

impl<'a> TokenIter<'a> {
    pub fn new(filename: &str) -> std::io::Result<TokenIter> {
        // TODO: dont read whole file in one go.
        let mut file = File::open(filename)?;
        let mut string = String::new();
        file.read_to_string(&mut string)?;
        Ok(TokenIter { it: put_back(string.chars()) })
    }

    fn next(&mut self, func: &dyn Fn(char) -> bool, while_true: bool) -> CustomResult<String> {
        self.trim();

        let mut result = String::new();
        while let Some(c) = self.it.next() {
            if (while_true && !func(c)) || (!while_true && func(c)) {
                self.it.put_back(c);
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

    pub fn next_while_true(&mut self, func: &dyn Fn(char) -> bool) -> CustomResult<String> {
        self.next(func, true)
    }

    pub fn next_while_false(&mut self, func: &dyn Fn(char) -> bool) -> CustomResult<String> {
        self.next(func, false)
    }

    pub fn next_char(&mut self) -> Option<char> {
        self.trim();
        self.it.next()
    }
/*
    pub fn next_token(&mut self) -> CustomResult<Token> {
        if let Some(next_char) = self.trim_peek() {
            // TODO: hex/binary
            let radix = 10;
            if next_char.is_digit(radix) || next_char == '-' || next_char == '+' {
                return Ok(self.next_number(radix)?);
            } else if next_char.is_alphanumeric() || next_char == '_' {
                return Ok(self.next_identifier()?);
            }
        } else {
            Ok(Token::Control(Control::NewLine))
        }
    }
    */

    pub fn next_number(&mut self, radix: u32) -> CustomResult<Token> {
        self.trim();

        // Will be multiplied with the result to "decide" plus or minus sign.
        let sign = if let Some(first_char) = self.next_char() {
            if first_char == '-' {
                -1
            } else if first_char == '+' {
                1
            } else {    // a digit
                self.it.put_back(first_char);
                1
            }
        } else {
            return Err(LexError("EOF while reading start of number.".to_string()));
        };

        let number = self.next_integer(radix)?;
        // True if float number.
        if let Some('.') = self.trim_peek() {
            self.next_char();   // Remove dot.
            let decimal_part = self.next_integer(radix)?;

            let float = [number, decimal_part]
                .join(".")
                .parse::<f64>()
                .map_err(|e| LexError(e.description().to_string()))?;
            let float_signed = float * sign as f64;

            Ok(Token::ShortHand(ShortHand::Float(float_signed)))
        } else {
            let integer = number
                .parse::<i128>()
                .map_err(|e| LexError(e.description().to_string()))?;
            let integer_signed = integer * sign;

            Ok(Token::ShortHand(ShortHand::Integer(integer_signed)))
        }

        // TODO: check case (as) and suffix etc.
    }

    pub fn next_integer(&mut self, radix: u32) -> CustomResult<String> {
        let mut numbers = Vec::new();
        while let Some(next_char) = self.it.next() {
            if next_char.is_digit(radix) {
                numbers.push(next_char);
            } else {
                self.it.put_back(next_char);
                break;
            }
        }

        Ok(numbers.into_iter().collect())
    }

    pub fn next_word(&mut self) -> CustomResult<String> {
        self.next_while_false(&|c| -> bool { c.is_whitespace() })
    }

    pub fn next_identifier(&mut self) -> CustomResult<String> {
        self.next_while_true(&|c| -> bool { c.is_alphanumeric() || c == '_' })
    }

    pub fn next_type(&mut self) -> CustomResult<String> {
        let mut result = Vec::new();
        result.push(self.next_identifier()?);

        if let Some('<') = self.trim_peek() {
            result.push(self.next_generic()?);
        }

        Ok(result.join(""))
    }
/*
    pub fn next_expression(&mut self) -> CustomResult<Token>  {
        let name = self.next_identifier()?;
        let token = Token::lookup(name)?;
        match token {
            Token::Header(Header::If) =>
            _ => Err(LexError("Incorrect match of token in next_expression.".to_string()))
        }
    }
    */

    /*
    pub fn next_keyword(&mut self) -> CustomResult<Token> {
        let name = self.next_identifier()?;
        let token = Token::lookup(name);
    }
    */

    fn next_generic(&mut self) -> CustomResult<String> {
        let mut result: Vec<String> = Vec::new();

        if let Some('<') = self.next_char() {
            result.push('<'.to_string());
        } else {
            return Err(LexError("next_generic doesn't start with a '<'.".to_string()));
        }

        result.push(self.next_identifier()?);

        loop {
            if let Some(next_char) = self.next_char() {
                result.push(next_char.to_string());

                match next_char {
                    ',' | '<' => result.push(self.next_type()?),
                    '>' => break,
                    _ => return Err(LexError("Incorrect char in match of next_generic.".to_string()))
                };
            } else {
                return Err(LexError("Reached EOF while looping char for next_generic.".to_string()));
            }
        }

        Ok(result.join(""))
    }

    /*
    pub fn next_boolean_statement(&mut self) -> CustomResult<String> {
        self.next_while_true(&|c| -> bool { c.is_alphanumeric() || c.is_digit(10) || c == '_' || c == '<' || c == '>' })
    }
    */

    pub fn next_return_type(&mut self) -> CustomResult<Token> {
        if let Some(next_char) = self.next_char() {
            if next_char == '-' && self.next_char() == Some('>') {
                Ok(Token::ReturnType(self.next_type()?))
            } else {
                Err(LexError("Incorrect return type syntax.".to_string()))
            }
        } else {
            Ok(Token::ReturnType(String::from("")))
        }
    }

    pub fn next_arguments(&mut self) -> CustomResult<Vec<Token>> {
        self.trim();

        // TODO: inverse
        if let Some('(') = self.next_char() {} else {
            return Err(LexError("Expected start curly bracket while lexing arguments.".to_string()));
        }

        let mut arguments: Vec<Token> = Vec::new();
        loop {
            let name = self.next_identifier()?;

            if let Some(next_char) = self.next_char() {
                let var_type =
                    if next_char == ':' {
                        self.next_type()?
                    } else {
                        return Err(LexError("No type specifier char while parsing argument.".to_string()));
                    };
                arguments.push(Token::Variable(Variable { name, var_type }));
            }

            if let Some(next_char) = self.next_char() {
                if next_char == ')' {
                    break;
                } else if next_char == ',' {
                    continue;
                } else {
                    return Err(LexError("Incorrect separator between arguments or missing end parenthesis.".to_string()));
                }
            }
        }

        Ok(arguments)
    }

    pub fn trim(&mut self) -> usize {
        let mut count = 0;
        while let Some(c) = self.it.next() {
            if !c.is_whitespace() {
                self.it.put_back(c);
                break;
            }
            count += 1;
        }
        count
    }

    pub fn peek(&mut self) -> Option<char> {
        if let Some(c) = self.it.next() {
            self.it.put_back(c);
            Some(c)
        } else {
            None
        }
    }

    pub fn trim_peek(&mut self) -> Option<char> {
        self.trim();
        self.peek()
    }
}
*/