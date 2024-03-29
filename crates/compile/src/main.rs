mod compiler;

use std::{collections::HashMap, path::Path, time::Instant};

use clap::{App, Arg};
use inkwell::context::Context;
use log::{log_enabled, Level};

use common::{
    error::{LangError, LangErrorKind, LangResult},
    file::{FileId, FileInfo},
    ty::ty_env::TY_ENV,
};
use lex::lexer;
use parse::parser::ParseTokenIter;

#[macro_use]
extern crate log;

// TODO: How to chose which `std` files to include? Ex. `String` that are currently
//       included into the compiler depends on libc functions, so it is not good
//       to have it as an requirement. Need to find a better way to pick which
//       types/functions to include in the compiler.
//       The paths to the files to include are also hardcoded in the code, this
//       needs to be changed.
// TODO: Currently the content of the `std` files needs to be mutable. This is
//       needed because something that is parsed as an "shift right" (>>) might
//       actually be two individual "greater than" signs inside a type.
//       This means that we have to copy the contents of the embedded `std` files
//       from static memory to mutable memory which isn't optimal. See if possible
//       in the future to not require the content to be mutable in `TokenIter`.

macro_rules! std_file {
    ($path:literal) => {
        ($path, Some(include_bytes!($path)))
    };
}

/// Array of files (and their content) that are included in the compiler. These
/// are `std` files containing ADTs/functions used extensively in the language.
const STD_FILES: [(&str, Option<&[u8]>); 25] = [
    std_file!("..\\..\\..\\std\\assert.ren"),
    std_file!("..\\..\\..\\std\\cmp.ren"),
    std_file!("..\\..\\..\\std\\either.ren"),
    std_file!("..\\..\\..\\std\\eq.ren"),
    std_file!("..\\..\\..\\std\\hash.ren"),
    std_file!("..\\..\\..\\std\\optional.ren"),
    std_file!("..\\..\\..\\std\\result.ren"),
    std_file!("..\\..\\..\\std\\collection\\list.ren"),
    std_file!("..\\..\\..\\std\\collection\\map.ren"),
    std_file!("..\\..\\..\\std\\io\\print.ren"),
    std_file!("..\\..\\..\\std\\mem\\allocator.ren"),
    std_file!("..\\..\\..\\std\\mem\\disposable.ren"),
    std_file!("..\\..\\..\\std\\primitive\\bool.ren"),
    std_file!("..\\..\\..\\std\\primitive\\common.ren"),
    std_file!("..\\..\\..\\std\\primitive\\u8.ren"),
    std_file!("..\\..\\..\\std\\primitive\\i8.ren"),
    std_file!("..\\..\\..\\std\\primitive\\u16.ren"),
    std_file!("..\\..\\..\\std\\primitive\\i16.ren"),
    std_file!("..\\..\\..\\std\\primitive\\u32.ren"),
    std_file!("..\\..\\..\\std\\primitive\\i32.ren"),
    std_file!("..\\..\\..\\std\\primitive\\u64.ren"),
    std_file!("..\\..\\..\\std\\primitive\\i64.ren"),
    std_file!("..\\..\\..\\std\\string\\as_view.ren"),
    std_file!("..\\..\\..\\std\\string\\string.ren"),
    std_file!("..\\..\\..\\std\\string\\string_view.ren"),
];

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
    let mut file_infos: HashMap<FileId, FileInfo> = HashMap::default();

    // All parsed files will be included in the same LLVM module.
    let mut parser = ParseTokenIter::new(&TY_ENV);

    let input_paths = &opts
        .input_files
        .iter()
        .map(|path| (&**path, None))
        .collect::<Vec<(_, Option<&[u8]>)>>();

    // Iterates through both the `std` included files and the files specified at
    // the command line.
    for (input_path, content) in STD_FILES.iter().chain(input_paths) {
        file_nr += 1;
        let file_info = parse_path(input_path)?;
        file_infos.insert(file_nr, file_info.clone());

        // TODO: Fix ugly mut hack, see comment at top of this file.
        let mut vec_content = content.map(|content| content.to_vec());

        lex_and_parse(
            &mut parser,
            file_nr,
            file_info,
            vec_content.as_deref_mut(),
            opts.quiet,
        );
    }

    let mut ast_root = parser.take_root_block();
    if log_enabled!(Level::Debug) {
        debug!("\nAST after parsing:\n{:#?}", ast_root);
    }

    let analyze_ast_timer = Instant::now();
    let mut analyze_ctx = match analyze_ast::analyze(&mut ast_root, &TY_ENV, file_infos, opts.quiet)
    {
        Ok(analyze_ctx) => analyze_ctx,
        Err(errs) => {
            eprintln!();
            for e in errs {
                eprintln!("[ERROR] {}", e);
            }
            std::process::exit(1);
        }
    };

    if !opts.quiet || opts.validate {
        println!(
            "Analyzing AST complete ({:?}).",
            analyze_ast_timer.elapsed()
        );
    }
    if log_enabled!(Level::Debug) {
        debug!("\nAST after analyze:\n{:#?}", ast_root);
    } else if opts.ast {
        println!("\nAST after analyze:\n{:#?}", ast_root);
    }
    analyze_ctx.ast_ctx.debug_print();

    // Only validate the correctness of the AST and skip code generation and
    // IR if `validate` is set.
    if opts.validate {
        return Ok(());
    }

    let mut ir_module = match ir_builder::build_module(
        "MODULE_NAME".into(),
        opts.ptr_size,
        &mut analyze_ctx,
        &mut ast_root,
    ) {
        Ok(ir_module) => ir_module,
        Err(errs) => {
            eprintln!();
            for e in errs {
                eprintln!("[ERROR] {}", e);
            }
            std::process::exit(1);
        }
    };

    if opts.ir {
        println!("\n## IR before optimization ##\n{:?}", ir_module);
    }

    let analyze_ir_timer = Instant::now();
    if let Err(errs) = analyze_ir::analyze(&mut ir_module, opts.optimize) {
        eprintln!();
        for e in errs {
            eprintln!("[ERROR] {}", e);
        }
        std::process::exit(1);
    };

    if opts.ir && opts.optimize {
        println!("\n## IR after optimization ##\n{:?}", ir_module);
    }

    if !opts.quiet {
        println!("Analyzing IR complete ({:?}).", analyze_ir_timer.elapsed());
    }

    let generate_timer = Instant::now();
    let target_machine = compiler::setup_target(&opts.target_triple)?;
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module(&opts.module_name);
    match codegen_llvm::generate(&context, &builder, &module, &target_machine, ir_module) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("[ERROR] {}", e);
            std::process::exit(1);
        }
    }

    if !opts.quiet {
        println!("Generating complete ({:?}).", generate_timer.elapsed());
    }
    if opts.llvm {
        println!("\n## LLVM IR before optimization ##");
        module.print_to_stderr();
    }

    let compile_timer = Instant::now();
    module
        .verify()
        .map_err(|e| LangError::new(e.to_string(), LangErrorKind::CodeGenError, None))?;
    compiler::compile(target_machine, &module, &opts.output_file, opts.optimize)?;
    if !opts.quiet {
        println!("Compiling complete ({:?}).", compile_timer.elapsed());
    }

    if opts.llvm && opts.optimize {
        println!("\n## LLVM IR after optimization ##");
        module.print_to_stderr();
    }

    println!("Output: {}", opts.output_file);

    Ok(())
}

fn lex_and_parse(
    parser: &mut ParseTokenIter,
    file_nr: FileId,
    file: FileInfo,
    content: Option<&mut [u8]>,
    quiet: bool,
) {
    let lex_timer = Instant::now();
    let lex_tokens_result = if let Some(content) = content {
        lexer::lex_with_content(file_nr, content)
    } else {
        lexer::lex(file_nr, &file)
    };

    let mut lex_tokens = match lex_tokens_result {
        Ok(lex_tokens) => lex_tokens,
        Err(errs) => {
            for e in errs {
                eprintln!("[ERROR] {}", e);
            }
            std::process::exit(1);
        }
    };
    if !quiet {
        println!(
            "Lexing {} complete ({:?}).",
            &file.filename,
            lex_timer.elapsed()
        );
    }

    let parse_timer = Instant::now();
    if let Err(errs) = parser.parse(&mut lex_tokens) {
        for e in errs {
            eprintln!("[ERROR] {}", e);
        }
        std::process::exit(1);
    }
    if !quiet {
        println!(
            "Parsing {} complete ({:?}).",
            &file.filename,
            parse_timer.elapsed()
        );
    }
}

/// Parses the given string `input_path` into a `FileInfo`.
/// The given path should contain a filename plus an optional absolute or
/// relative path.
fn parse_path(input_path: &str) -> LangResult<FileInfo> {
    let path = std::path::Path::new(input_path);
    let filename = path
        .file_name()
        .map(|os| os.to_str())
        .flatten()
        .ok_or_else(|| {
            LangError::new(
                format!("Unable to get filename: {}", input_path),
                LangErrorKind::GeneralError,
                None,
            )
        })?
        .into();
    let directory = path
        .parent()
        .ok_or_else(|| {
            LangError::new(
                format!("Unable to get directory: {}", input_path),
                LangErrorKind::GeneralError,
                None,
            )
        })?
        .into();
    Ok(FileInfo {
        directory,
        filename,
    })
}

struct Options {
    input_files: Vec<String>,
    input_files_list: Option<String>,
    output_file: String,
    module_name: String,
    ptr_size: usize,
    target_triple: Option<String>,
    optimize: bool,
    llvm: bool,
    ast: bool,
    ir: bool,
    quiet: bool,
    validate: bool,
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
            Arg::with_name("size")
                .short("s")
                .long("size")
                .help("Size in bytes that should be used for pointers.")
                .default_value("8")
                .takes_value(true)
                .required(false),
        )
        .arg(
            Arg::with_name("triple")
                .short("t")
                .long("triple")
                .help("Target triple.")
                .takes_value(true)
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
            Arg::with_name("ir")
                .short("I")
                .long("ir")
                .help("Set to dump/print the generated IR code.")
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
        .arg(
            Arg::with_name("quiet")
                .short("q")
                .long("quiet")
                .help("Set to not print step progress to stdout.")
                .takes_value(false)
                .required(false),
        )
        .arg(
            Arg::with_name("validate")
                .short("v")
                .long("validate")
                .help("Set to only validate the code and skip code generation.")
                .takes_value(false)
                .required(false),
        )
        .get_matches();

    let mut input_files = Vec::default();
    if let Some(inputs) = matches.values_of("INPUTS") {
        inputs.for_each(|f| input_files.push(f.into()));
    }

    let input_files_list = matches.value_of("input").map(|input| input.into());

    Options {
        input_files,
        input_files_list,
        output_file: matches.value_of("output").unwrap().into(),
        module_name: matches.value_of("module").unwrap().into(),
        ptr_size: matches.value_of("size").unwrap().parse().unwrap(),
        target_triple: matches.value_of("triple").map(|t| t.into()),
        optimize: matches.is_present("optimize"),
        llvm: matches.is_present("llvm"),
        ast: matches.is_present("ast"),
        ir: matches.is_present("ir"),
        quiet: matches.is_present("quiet"),
        validate: matches.is_present("validate"),
    }
}
