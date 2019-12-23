use std::mem::{drop, replace};
use std::str::Chars;
use itertools::{PutBack, put_back};

use crate::lexer::CustomResult;
use crate::error::CustomError::LexError;
use crate::token::Variable;

pub struct TokenIter<'a> {
    it: PutBack<Chars<'a>>,
}

impl<'a> TokenIter<'a> {
    pub fn new() -> TokenIter<'a> {
        TokenIter { it: put_back("".chars()) }
    }

    pub fn new_line(&mut self, line: &'a str) {
        drop(replace(&mut self.it, put_back(line.chars())));
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
            Err(LexError("Empty result in next()."))
        }
    }

    pub fn next_while_true(&mut self, func: &dyn Fn(char) -> bool) -> CustomResult<String> {
        self.next(func, true)
    }

    pub fn next_while_false(&mut self, func: &dyn Fn(char) -> bool) -> CustomResult<String> {
        self.next(func, false)
    }

    pub fn next_char(&mut self) -> CustomResult<char> {
        self.trim();

        if let Some(c) = self.it.next() {
            Ok(c)
        } else {
            Err(LexError("Reached EOF while getting next char."))
        }
    }

    pub fn next_word(&mut self) -> CustomResult<String> {
        self.next_while_false(&|c| -> bool { c.is_whitespace() })
    }

    pub fn next_identifier(&mut self) -> CustomResult<String> {
        self.next_while_true(&|c| -> bool { c.is_alphanumeric() || c == '_' })
    }

    pub fn next_arguments(&mut self) -> CustomResult<Vec<Variable>> {
        self.trim();

        // TODO: Not hardcoded par.
        if self.next_char()? != '(' {
            return Err(LexError("Expected start parenthesis while lexing arguments."));
        }

        let mut arguments: Vec<Variable> = Vec::new();
        loop {
            let id = self.next_identifier()?;

            if self.next_char()? != ':' {
                return Err(LexError("Incorrect separator between arguments."));
            }

            let var_type = self.next_while_false(
                &|c| -> bool {
                    c.is_whitespace() || c == ')'
                }
            )?;

            arguments.push(Variable { id, var_type });

            if self.trim_peek()? == ')' {
                self.next_char();
                break;
            }
        }

        Ok(arguments)
    }

    pub fn trim(&mut self) -> usize {
        let count = 0;
        while let Some(c) = self.it.next() {
            if !c.is_whitespace() {
                self.it.put_back(c);
                break;
            }
            count += 1;
        }
        count
    }

    pub fn peek(&mut self) -> CustomResult<char> {
        if let Some(c) = self.it.next() {
            self.it.put_back(c);
            Ok(c)
        } else {
            Err(LexError("Reached EOF while peeking."))
        }
    }

    pub fn trim_peek(&mut self) -> CustomResult<char> {
        self.trim();
        self.peek()
    }
}