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
use crate::parse::parser::ParseTokenIter;
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
    let mut input_file = args[1].clone();
    let module_name = args[1].split('/').last().unwrap();

    env_logger::init();

    // Loop through files and lex+parse them until there or no more uses/includes
    // to process. ALl files will be incldued in the same module.
    let mut parser = ParseTokenIter::new();
    let mut i = 0;
    let mut ast_root = loop {
        let lex_tokens = match lexer::lex(&input_file) {
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
        info!("Lexing for file \"{}\" complete.", &input_file);

        parser.set_lex_tokens(lex_tokens);
        match parser.parse() {
            Ok(_) => (),
            Err(errs) => {
                for e in errs {
                    error!("{}", e);
                }
                std::process::exit(1);
            }
        }

        i += 1;

        if parser.uses.is_empty() {
            break parser.take_root_block();
        } else {
            input_file = if let Some(use_path) = parser.uses.pop() {
                if let Some(input_file) = use_path.to_file_path() {
                    input_file
                } else {
                    error!("Unable to convert path to string, path: {:?}", use_path);
                    std::process::exit(1);
                }
            } else {
                error!("Unable to get path from use stack");
                std::process::exit(1);
            };
        }
    };
    println!("Parsing complete.");
    debug!("\nAST after parsing:\n{:#?}", ast_root);

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
    debug!("Structs: {:#?}", &analyze_context.structs);
    println!("Analyzing complete.");

    let target_machine = compiler::setup_target()?;

    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module(module_name);
    match generator::generate(
        &mut ast_root,
        &analyze_context,
        &context,
        &builder,
        &module,
        &target_machine,
    ) {
        Ok(_) => (),
        Err(e) => {
            error!("{}", e);
            std::process::exit(1);
        }
    }
    println!("Generating complete.");

    if log_enabled!(Level::Debug) {
        module.print_to_stderr();
    }
    module.verify()?;

    compiler::compile(target_machine, &module, OUTPUT_PATH)?;
    println!("Compiled to: {}", OUTPUT_PATH);

    Ok(())
}
