mod analyze;
mod codegen;
mod common;
mod error;
mod lex;
mod parse;
mod transpile;

#[macro_use]
extern crate log;

use crate::analyze::analyzer;
use crate::codegen::generator;
use crate::error::CustomError;
use crate::lex::lexer;
use crate::parse::parser;
use crate::parse::token::ParseToken;
use inkwell::context::Context;
use log::Level;

pub type CustomResult<T> = Result<T, CustomError>;

fn main() -> CustomResult<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        let msg = format!(
            "Invalid amount of args. Expected: {}, got: {}.\nUsage: {} <TEST_DATA>",
            2,
            args.len(),
            args[0]
        );
        return Err(CustomError::GeneralError(msg));
    }
    let input_file = &args[1];

    env_logger::init();

    let lex_tokens = lexer::lex(input_file).unwrap();
    if log_enabled!(Level::Debug) {
        for lex_token in &lex_tokens {
            debug!("{:?}", lex_token)
        }
    }
    println!("Lexing complete.\n");

    let mut ast_root: ParseToken = parser::parse(lex_tokens).unwrap();
    debug!("AST after parsing:\n{:#?}", ast_root);
    println!("Parsing complete.\n");

    let analyze_context = analyzer::analyze(&mut ast_root)?;
    debug!("\nAST after analyze:\n{:#?}", ast_root);
    println!("Analyzing complete.\n");

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("MODULE_NAME");
    generator::generate(&ast_root, &analyze_context, &context, &builder, &module)?;
    println!("Generating complete.\n");

    if log_enabled!(Level::Debug) {
        module.print_to_stderr();
    }
    module.verify()?;

    /*
    let lines = transpile(&ast, &analyze_context);
    println!("LINES:");
    for line in lines? {
        println!("{}", line);
    }
    */

    Ok(())
}
