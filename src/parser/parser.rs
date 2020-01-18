/*
use crate::lexer::simple_token::SimpleToken;
use crate::parser::token::Token;

pub fn parse(simple_tokens: Vec<SimpleToken>) -> Vec<Token> {

    let ast = Vec::new();

    for simple_token in simple_tokens.iter() {
        match simple_token {
            SimpleToken::Identifier(id) => {

                match  { }

            },
            SimpleToken::Number(number) => {},
            SimpleToken::Symbol(symbol) => {},
            SimpleToken::EndOfFile => {},
            SimpleToken::Unknown(string) => {},
            _ => panic!("Incorrect simple token.")
        }
    }

    ast
}

fn parse_identifier(identifier: &str) -> Token {

}

fn parse_identifier(id: &str) -> Token {

}

fn lookup(simple_token: SimpleToken) -> Option<Token> {

}
*/