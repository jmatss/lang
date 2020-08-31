mod compiler;

use analyze::analyze;
use clap::{App, Arg};
use codegen::generator;
use common::error::CustomResult;
use inkwell::context::Context;
use lex::lexer;
use log::Level;
use parse::parser::ParseTokenIter;

#[macro_use]
extern crate log;

fn main() -> CustomResult<()> {
    let matches = App::new("lang")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .help("The file to compile.")
                .takes_value(true)
                .required(true),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("NAME")
                .help("The output name of the produced object file.")
                .takes_value(true)
                .required(false),
        )
        .arg(
            Arg::with_name("optimize")
                .short("O")
                .long("optimize")
                .help("Set to run optimization of the LLVM IR.")
                .takes_value(false)
                .required(false),
        )
        .get_matches();

    let mut input_file = matches.value_of("input").unwrap().to_owned();
    if !input_file.ends_with(".ren") {
        error!("Expected input file to end with the extension \".ren\".");
        std::process::exit(1);
    }
    let input_file_clone = input_file.clone();
    let input_file_name = input_file_clone.split('/').last().unwrap();

    let default_output_file = input_file_name.replace(".ren", ".o");
    let output_file = matches.value_of("output").unwrap_or(&default_output_file);
    let optimize = matches.is_present("optimize");
    let module_name = input_file_name.split('.').next().unwrap();

    env_logger::init();

    // Loop through files and lex+parse them until there or no more uses/includes
    // to process. ALl files will be incldued in the same module.
    let mut parser = ParseTokenIter::new();
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

        if let Some(use_path) = parser.uses.pop() {
            input_file = match use_path.to_file_path() {
                Ok(input_file) => input_file,
                Err(e) => {
                    error!("{}", e);
                    std::process::exit(1);
                }
            };
        } else {
            // No more "use" statements that includes more files. Break, all
            // files parsed/lexed.
            break parser.take_root_block();
        }
    };
    println!("Parsing complete.");
    debug!("\nAST after parsing:\n{:#?}", ast_root);

    let analyze_context = match analyze(&mut ast_root) {
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
    module.verify()?;

    compiler::compile(target_machine, &module, output_file, optimize)?;

    if log_enabled!(Level::Debug) {
        module.print_to_stderr();
        println!("Module after optimization.");
    }

    println!("Compiled to: {}", output_file);

    Ok(())
}
