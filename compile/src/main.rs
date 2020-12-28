mod compiler;

use std::collections::HashMap;

use analyze::analyze;
use clap::{App, Arg};
use codegen::generator;
use common::{
    error::{CustomResult, LangError, LangErrorKind},
    file::{FileId, FileInfo},
};
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

    // Keep track of the files that are being parsed. This information will be
    // used when debugging and giving good error messages.
    let mut file_nr: FileId = 0;
    let mut file_info: HashMap<FileId, FileInfo> = HashMap::default();

    // Loop through files and lex+parse them until there or no more uses/includes
    // to process. ALl files will be incldued in the same module.
    let mut parser = ParseTokenIter::new();
    let mut ast_root = loop {
        let path = std::path::Path::new(&input_file);
        let filename = path
            .file_name()
            .map(|os| os.to_str())
            .flatten()
            .ok_or_else(|| {
                LangError::new(
                    format!("Unable to get filename: {}", input_file),
                    LangErrorKind::GeneralError,
                )
            })?
            .into();
        let directory = path
            .parent()
            .ok_or_else(|| {
                LangError::new(
                    format!("Unable to get directory: {}", input_file),
                    LangErrorKind::GeneralError,
                )
            })?
            .into();

        file_nr += 1;
        let file = FileInfo {
            directory,
            filename,
        };

        file_info.insert(file_nr, file.clone());

        let mut lex_tokens = match lexer::lex(file_nr, &file) {
            Ok(lex_tokens) => lex_tokens,
            Err(errs) => {
                for e in errs {
                    error!("{}", e);
                }
                std::process::exit(1);
            }
        };
        info!("Lexing for file (ID {}) {:#?} complete.", file_nr, &file);

        match parser.parse(&mut lex_tokens) {
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

    let analyze_context = match analyze(&mut ast_root, file_info) {
        Ok(analyze_context) => analyze_context,
        Err(errs) => {
            for e in errs {
                error!("{}", e);
            }
            std::process::exit(1);
        }
    };
    debug!("\nAST after analyze:\n{:#?}", ast_root);
    analyze_context.debug_print();
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
    compiler::compile(target_machine, &module, output_file, optimize)?;

    println!("Compiled to: {}", output_file);

    Ok(())
}
