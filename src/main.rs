mod error;
mod lexer;
mod parser;

use crate::error::CustomError;
use crate::lexer::lexer::lex;
use crate::parser::parser::parse;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() {
    // Loop through all files and lex simple tokens...
    //let simple_tokens = lex("examples/fib_iterative_4.ren").unwrap();
    let simple_tokens = lex("test_data/expr1.ren").unwrap();
    for simple_token in &simple_tokens {
        println!("{:?}", simple_token);
    }

    println!("Lexing complete.\n");

    let ast = parse(&simple_tokens).unwrap();
    println!("\nAST:");
    ast.debug_print();
}