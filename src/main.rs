mod analyze;
mod codegen;
mod common;
mod error;
mod lex;
mod parse;
mod transpile;

use crate::analyze::analyzer;
use crate::error::CustomError;
use crate::lex::lexer;
use crate::parse::parser;
use parse::token::ParseToken;

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

    let mut ast_root: ParseToken = parser::parse(lex_tokens).unwrap();
    println!("Parsing complete.\n");
    println!("\nAST after parse:\n{:#?}", ast_root);

    let analyze_context = analyzer::analyze(&mut ast_root)?;
    println!("Analyzing complete.\n");
    println!("\nAST after analyze:\n{:#?}", ast_root);

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
