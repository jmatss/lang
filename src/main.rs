mod analyze;
mod codegen;
mod common;
mod compile;
mod error;
mod lex;
mod parse;

#[macro_use]
extern crate log;

use crate::analyze::analyzer;
use crate::codegen::generator;
use crate::compile::compiler;
use crate::error::LangErrorKind::GeneralError;
use crate::lex::lexer;
use crate::parse::parser;
use error::LangError;
use inkwell::context::Context;
use log::Level;

pub type CustomResult<T> = Result<T, LangError>;

const OUTPUT_PATH: &str = "a.o";

fn main() -> CustomResult<()> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 2 {
        let msg = format!(
            "Invalid amount of args. Expected: {}, got: {}.\nUsage: {} <TEST_DATA>",
            2,
            args.len(),
            args[0]
        );
        return Err(LangError::new(msg, GeneralError));
    }
    let input_file = &args[1];
    let module_name = args[1].split('/').last().unwrap();

    env_logger::init();

    let lex_tokens = match lexer::lex(input_file) {
        Ok(lex_tokens) => lex_tokens,
        Err(errs) => {
            for e in errs {
                error!("{}", e);
            }
            std::process::exit(1);
        }
    };
    if log_enabled!(Level::Debug) {
        for lex_token in &lex_tokens {
            debug!("{:?}", lex_token)
        }
    }
    println!("Lexing complete.");

    let mut ast_root = match parser::parse(lex_tokens) {
        Ok(ast_root) => ast_root,
        Err(errs) => {
            for e in errs {
                error!("{}", e);
            }
            std::process::exit(1);
        }
    };
    debug!("\nAST after parsing:\n{:#?}", ast_root);
    println!("Parsing complete.");

    let analyze_context = match analyzer::analyze(&mut ast_root) {
        Ok(analyze_context) => analyze_context,
        Err(errs) => {
            for e in errs {
                error!("{}", e);
            }
            std::process::exit(1);
        }
    };
    debug!("\nAST after analyze:\n{:#?}", ast_root);
    debug!("Variables: {:#?}", &analyze_context.variables);
    debug!("Functions: {:#?}", &analyze_context.functions);
    println!("Analyzing complete.");

    let target_machine = compiler::setup_target()?;

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module(module_name);
    generator::generate(
        &mut ast_root,
        &analyze_context,
        &context,
        &builder,
        &module,
        &target_machine,
    )?;
    println!("Generating complete.");

    if log_enabled!(Level::Debug) {
        module.print_to_stderr();
    }
    module.verify()?;

    compiler::compile(target_machine, &module, OUTPUT_PATH)?;
    println!("Compiled to: {}", OUTPUT_PATH);

    Ok(())
}
