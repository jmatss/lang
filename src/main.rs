mod analyze;
mod common;
mod error;
mod generate;
mod lex;
mod parse;
mod transpile;

use crate::analyze::analyzer;
use crate::lex::lexer;
use crate::parse::parser;

use crate::error::CustomError;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() -> CustomResult<()> {
    // Loop through all files and lex simple tokens...
    //let simple_tokens = lex("examples/fib_iterative_4.ren").unwrap();
    let lex_tokens = lexer::lex("test_data/while1.ren").unwrap();
    for lex_token in &lex_tokens {
        println!("{:?}", lex_token);
    }
    println!("Lexing complete.\n");

    println!();

    let ast = parser::parse(lex_tokens).unwrap();
    println!("Parsing complete.\n");
    println!("\nAST after parse:\n{:#?}", ast);

    /*
    let analyze_context = analyzer::analyze(&mut ast)?;
    println!("\nAST after analyze:");
    ast.debug_print();
    println!();
    println!("{:#?}", analyze_context);
    */

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
