mod analyzer;
mod error;
mod generation;
mod lexer;
mod parser;
mod transpiler;

use crate::error::CustomError;
use crate::lexer::lexer::lex;
use crate::parser::parser::parse;
//use crate::transpiler::transpiler::transpile;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() -> CustomResult<()> {
    let indent_size = 4;

    // Loop through all files and lex simple tokens...
    //let simple_tokens = lex("examples/fib_iterative_4.ren").unwrap();
    let simple_tokens = lex("test_data/if3.ren").unwrap();
    for simple_token in &simple_tokens {
        println!("{:?}", simple_token);
    }

    println!("Lexing complete.\n");

    let ast = parse(&simple_tokens, indent_size).unwrap();
    println!("\nAST:");
    ast.debug_print();

    println!("");

    /*
    let lines = transpile(&ast);
    println!("LINES:");
    for line in lines? {
        println!("{}", line);
    }
     */

    Ok(())
}
