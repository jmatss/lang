mod analyzer;
mod common;
mod error;
mod generation;
mod lexer;
mod parser;
mod transpiler;

use crate::analyzer::analyzer::analyze;
use crate::error::CustomError;
use crate::lexer::lexer::lex;
use crate::parser::parser::parse;
use crate::transpiler::transpiler::transpile;
use crate::generation::code_generation::generate;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() -> CustomResult<()> {
    let indent_size = 4;

    // Loop through all files and lex simple tokens...
    //let simple_tokens = lex("examples/fib_iterative_4.ren").unwrap();
    let simple_tokens = lex("test_data/expr9.ren").unwrap();
    for simple_token in &simple_tokens {
        println!("{:?}", simple_token);
    }

    println!("Lexing complete.\n");

    let mut ast = parse(&simple_tokens, indent_size).unwrap();
    println!("\nAST after parse:");
    ast.debug_print();

    println!();

    let analyze_context = analyze(&mut ast)?;
    println!("\nAST after analyze:");
    ast.debug_print();
    println!();
    println!("{:#?}", analyze_context);

    let ir = generate(&ast, &analyze_context);

    /*
    let lines = transpile(&ast, &analyze_context);
    println!("LINES:");
    for line in lines? {
        println!("{}", line);
    }
    */

    Ok(())
}
