mod analyze;
mod common;
mod error;
mod generate;
mod lex;
mod parse;
mod transpile;

use crate::lex::lexer;
use crate::parse::parser;
use crate::analyze::analyzer;

use crate::error::CustomError;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() -> CustomResult<()> {
    let indent_size = 4;

    // Loop through all files and lex simple tokens...
    //let simple_tokens = lex("examples/fib_iterative_4.ren").unwrap();
    let simple_tokens = lexer::lex("test_data/expr9.ren").unwrap();
    for simple_token in &simple_tokens {
        println!("{:?}", simple_token);
    }

    println!("Lexing complete.\n");

    let mut ast = parser::parse(simple_tokens).unwrap();
    println!("\nAST after parse:");
    ast.debug_print();

    println!();

    let analyze_context = analyzer::analyze(&mut ast)?;
    println!("\nAST after analyze:");
    ast.debug_print();
    println!();
    println!("{:#?}", analyze_context);

    //let ir = generate::generate(&ast, &analyze_context);

    /*
    let lines = transpile(&ast, &analyze_context);
    println!("LINES:");
    for line in lines? {
        println!("{}", line);
    }
    */

    Ok(())
}
