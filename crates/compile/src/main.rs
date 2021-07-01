mod compiler;

use analyze::analyze;
use clap::{App, Arg};
use codegen_llvm::generator;
use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::{FileId, FileInfo},
    ty::ty_env::TY_ENV,
};
use inkwell::context::Context;
use lex::lexer;
use log::{log_enabled, Level};
use parse::parser::ParseTokenIter;
use std::{collections::HashMap, path::Path, time::Instant};

#[macro_use]
extern crate log;

fn main() -> LangResult<()> {
    env_logger::init();

    let mut opts = parse_opts();

    if let Some(input_files_list) = &opts.input_files_list {
        let content = match std::fs::read_to_string(input_files_list) {
            Ok(content) => content,
            Err(fs_err) => {
                return Err(LangError::new(
                    format!(
                        "Unable to read file specified in \"input\" option. Filename: {}, reason: {:#?}",
                        input_files_list,
                        fs_err.kind()
                    ),
                    LangErrorKind::GeneralError,
                    None,
                ));
            }
        };

        let parent_path = Path::new(input_files_list)
            .parent()
            .unwrap_or_else(|| Path::new(""));
        for input_file in content.split_whitespace() {
            let child_path = Path::new(input_file);
            let path = if child_path.is_absolute() {
                child_path.to_path_buf()
            } else {
                parent_path.join(child_path)
            };

            if let Some(path_str) = path.to_str() {
                opts.input_files.push(path_str.into())
            } else {
                return Err(LangError::new(
                    format!(
                        "Unable to convert path to string. Path: {}",
                        child_path.to_string_lossy()
                    ),
                    LangErrorKind::GeneralError,
                    None,
                ));
            }
        }
    }

    if opts.input_files.is_empty() {
        return Err(LangError::new(
            "No input files found.".into(),
            LangErrorKind::GeneralError,
            None,
        ));
    }

    // Keep track of the files that are being parsed. This information will be
    // used when debugging and giving good error messages.
    let mut file_nr: FileId = 0;
    let mut file_info: HashMap<FileId, FileInfo> = HashMap::default();

    // Loop through files and lex+parse them. All files will be incldued in the
    // same LLVM module.
    let mut parser = ParseTokenIter::new(&TY_ENV);
    for input_file in &opts.input_files {
        let path = std::path::Path::new(input_file);
        let filename = path
            .file_name()
            .map(|os| os.to_str())
            .flatten()
            .ok_or_else(|| {
                LangError::new(
                    format!("Unable to get filename: {}", input_file),
                    LangErrorKind::GeneralError,
                    None,
                )
            })?
            .into();
        let directory = path
            .parent()
            .ok_or_else(|| {
                LangError::new(
                    format!("Unable to get directory: {}", input_file),
                    LangErrorKind::GeneralError,
                    None,
                )
            })?
            .into();

        file_nr += 1;
        let file = FileInfo {
            directory,
            filename,
        };

        file_info.insert(file_nr, file.clone());

        let lex_timer = Instant::now();
        let mut lex_tokens = match lexer::lex(file_nr, &file) {
            Ok(lex_tokens) => lex_tokens,
            Err(errs) => {
                for e in errs {
                    eprintln!("[ERROR] {}", e);
                }
                std::process::exit(1);
            }
        };
        println!(
            "Lexing {} complete ({:?}).",
            &file.filename,
            lex_timer.elapsed()
        );

        let parse_timer = Instant::now();
        match parser.parse(&mut lex_tokens) {
            Ok(_) => (),
            Err(errs) => {
                for e in errs {
                    eprintln!("[ERROR] {}", e);
                }
                std::process::exit(1);
            }
        }
        println!(
            "Parsing {} complete ({:?}).",
            &file.filename,
            parse_timer.elapsed()
        );
    }

    let mut ast_root = parser.take_root_block();
    if log_enabled!(Level::Debug) {
        debug!("\nAST after parsing:\n{:#?}", ast_root);
    }

    let analyze_timer = Instant::now();
    let mut analyze_ctx = match analyze(&mut ast_root, &TY_ENV, file_info) {
        Ok(analyze_ctx) => analyze_ctx,
        Err(errs) => {
            for e in errs {
                eprintln!("[ERROR] {}", e);
            }
            std::process::exit(1);
        }
    };
    println!("Analyzing complete ({:?}).", analyze_timer.elapsed());
    if log_enabled!(Level::Debug) {
        debug!("\nAST after analyze:\n{:#?}", ast_root);
    } else if opts.ast {
        println!("\nAST after analyze:\n{:#?}", ast_root);
    }
    analyze_ctx.ast_ctx.debug_print();

    let generate_timer = Instant::now();
    let target_machine = compiler::setup_target()?;
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module(&opts.module_name);
    match generator::generate(
        &mut ast_root,
        &mut analyze_ctx,
        &context,
        &builder,
        &module,
        &target_machine,
    ) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("[ERROR] {}", e);
            std::process::exit(1);
        }
    }

    println!("Generating complete ({:?}).", generate_timer.elapsed());
    if opts.llvm {
        println!("\n## LLVM IR before optimization ##");
        module.print_to_stderr();
    }

    let compile_timer = Instant::now();
    module
        .verify()
        .map_err(|e| LangError::new(e.to_string(), LangErrorKind::CodeGenError, None))?;
    compiler::compile(target_machine, &module, &opts.output_file, opts.optimize)?;
    println!("Compiling complete ({:?}).", compile_timer.elapsed());

    if opts.llvm && opts.optimize {
        println!("\n## LLVM IR after optimization ##");
        module.print_to_stderr();
    }

    println!("Compiled to: {}", opts.output_file);

    Ok(())
}

struct Options {
    input_files: Vec<String>,
    input_files_list: Option<String>,
    output_file: String,
    module_name: String,
    optimize: bool,
    llvm: bool,
    ast: bool,
}

fn parse_opts() -> Options {
    let matches = App::new("lang")
        .arg(
            Arg::with_name("INPUTS")
                .help("List of input files.")
                .multiple(true)
                .required(false),
        )
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .value_name("FILE")
                .help(
                    "A file containing a list of input files. \
                    Relative paths inside the file will be resolved relative to this file path.",
                )
                .takes_value(true)
                .required(false),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .value_name("NAME")
                .help("The output name of the produced object file.")
                .default_value("a.o")
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
        .arg(
            Arg::with_name("llvm")
                .short("l")
                .long("llvm")
                .help("Set to dump/print the generated LLVM IR code.")
                .takes_value(false)
                .required(false),
        )
        .arg(
            Arg::with_name("ast")
                .short("a")
                .long("ast")
                .help("Set to print AST.")
                .takes_value(false)
                .required(false),
        )
        .arg(
            Arg::with_name("module")
                .short("m")
                .long("module")
                .help("Set the name of the LLVM module.")
                .default_value("lang_module")
                .takes_value(true)
                .required(false),
        )
        .get_matches();

    let mut input_files = Vec::default();
    if let Some(inputs) = matches.values_of("INPUTS") {
        inputs.for_each(|f| input_files.push(f.into()));
    }

    let input_files_list = if let Some(input) = matches.value_of("input") {
        Some(input.into())
    } else {
        None
    };

    Options {
        input_files,
        input_files_list,
        output_file: matches.value_of("output").unwrap().into(),
        module_name: matches.value_of("module").unwrap().into(),
        optimize: matches.is_present("optimize"),
        llvm: matches.is_present("llvm"),
        ast: matches.is_present("ast"),
    }
}
