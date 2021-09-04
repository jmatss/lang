mod decl;
mod mid;
mod post;
mod pre;
mod ty;
pub mod util;

use std::{collections::HashMap, io::Write, time::Instant};

use parking_lot::Mutex;

use common::{
    ctx::{analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx},
    error::LangError,
    file::{FileId, FileInfo},
    token::ast::AstToken,
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse},
    ty::ty_env::TyEnv,
};
use decl::{
    adt::DeclTypeAnalyzer, block::BlockAnalyzer, built_in::init_built_ins, func::DeclFnAnalyzer,
    var::DeclVarAnalyzer,
};
use mid::{
    defer::DeferAnalyzer, generics::GenericsAnalyzer, method::MethodAnalyzer,
    path_resolver::PathResolver,
};
use post::{
    auto_deref::AutoDerefAnalyzer, call_args::CallArgs, clean_up::clean_up,
    match_exhaust::MatchExhaustAnalyzer, trait_generics::TraitGenericsAnalyzer,
    union_init_arg::UnionInitArg,
};
use pre::{indexing::IndexingAnalyzer, main_args::MainArgsAnalyzer};
use ty::{inferencer::TypeInferencer, traits_fn::TraitsFnAnalyzer};
use util::order::dependency_order_from_ctx;

use crate::{
    mid::trait_impl::TraitImplAnalyzer,
    post::{
        cmp::CmpAnalyzer, eq::EqAnalyzer, ext_struct_init::ExtStructInit,
        fn_return::FnReturnAnalyzer, format::FormatParser, trait_methods::TraitMethodsAnalyzer,
        void::VoidAnalyzer,
    },
    pre::signed_literals::SignedLiteralsAnalyzer,
    ty::{
        gen::{
            adt_collector::GenericAdtCollector, adt_creator::GenericAdtCreator,
            fn_collector::GenericFnCollector, fn_creator::GenericFnCreator,
            method_check::MethodGensCheck, method_gens::MethodGensInferencer,
            tys_solved::GenericTysSolvedChecker,
        },
        inf::primitive_to_adt::PrimitiveToAdtAnalyzer,
        solve::solve_type_system,
        updater::TypeUpdater,
    },
};

fn analyzer_name<T>(_: &T) -> &str {
    std::any::type_name::<T>().split("::").last().unwrap()
}

macro_rules! traverse {
    ($ctx:expr, $ast_root:expr, $quiet:expr, $analyzer_init:expr) => {{
        let start_timer = Instant::now();
        let mut analyzer = $analyzer_init;
        if !$quiet {
            print!("Running {} ", analyzer_name(&analyzer));
            std::io::stdout().flush().unwrap();
        }
        traverse($ctx, &mut analyzer, $ast_root)?;
        if !$quiet {
            println!("({:?}).", start_timer.elapsed());
        }
        analyzer
    }};
}

fn run_solve_type_system(ctx: &mut TraverseCtx, quiet: bool) -> Result<(), Vec<LangError>> {
    let start_timer = Instant::now();
    if !quiet {
        print!("Running TypeSolving ");
        std::io::stdout().flush().unwrap();
    }
    solve_type_system(ctx).map_err(|err| vec![err])?;
    if !quiet {
        println!("({:?}).", start_timer.elapsed());
    }
    Ok(())
}

// TODO: Error if a function that doesn't have a return type has a return in it.
// TODO: Make it so that one doesn't have to recreate a new AstVisitor for every
//       analyzing step. They all want to shared the same `analyze_context` but
//       moving the analyzer contianing the `analyze_context` into the traverser
//       will not work since rust assumes that the analyzer will live for the
//       whole life of the traverser. And since the analyzer contains a mut ref
//       to the `analyze_context`, rust will assume that the `analyze_context`
//       will live for the while "analyze" function inside the analyzer object.
// TODO: Since the "defer" copies expressions in the AST, is there a possiblity
//       that the corresponding "DeferExec" and its contents gets different types
//       during type analyzing? Would that be a problem if it is true?

/// Updates the AST with information about function prototypes and declarations
/// of structures.
/// The "defer analyzing" depends on being able to lookup block info, so it needs
/// to be ran after "block analyzing".
/// The "TypeInference/TypeSolve" depends on the "Decl...Analyzer"s to figure out
/// types for function calls and function parameters used in expressions, so the
/// "Decl...Analyzer"s needs to be ran before "Type...".
/// The "var & func decl analyzer" depends on the "type decl analyzer" since the
/// types might be used by variables/functions.
/// The "IndexingAnalyzer" should be ran before the "Type..." since the "Type..."
/// will fail if the indexing analyzing doesn't rewrite the AST before that.
/// The "MethodAnalyzer" should be ran before "Type..." since it rewrites the AST
/// so that the "type analyzing" can know that it is a method vs function.
/// The "MethodAnalyzer" should be ran after "decl analyzers" since it will
/// need struct/func information to figure out types for method calls.
/// "decl_fn_analyzer"/"decl_type_analyzer"/"type..." needs to be ran before
/// the "call_args" since it needs to access structs, functions and methods.
pub fn analyze<'a>(
    ast_root: &mut AstToken,
    ty_env: &'a Mutex<TyEnv>,
    file_info: HashMap<FileId, FileInfo>,
    quiet: bool,
) -> Result<AnalyzeCtx<'a>, Vec<LangError>> {
    let built_ins = init_built_ins(ty_env).map_err(|err| vec![err])?;

    let mut ast_ctx = AstCtx::new(built_ins, file_info).map_err(|err| vec![err])?;
    let mut ctx = TraverseCtx::new(&mut ast_ctx, ty_env);

    traverse!(&mut ctx, ast_root, quiet, MainArgsAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, SignedLiteralsAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, IndexingAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, BlockAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, DeclTypeAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, DeclFnAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, DeclVarAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, MethodAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, PathResolver::new());
    traverse!(&mut ctx, ast_root, quiet, GenericsAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, DeferAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, TraitImplAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, TypeInferencer::new());

    run_solve_type_system(&mut ctx, quiet)?;

    traverse!(&mut ctx, ast_root, quiet, MethodGensInferencer::new());
    traverse!(&mut ctx, ast_root, quiet, TypeUpdater::new());
    traverse!(&mut ctx, ast_root, quiet, PrimitiveToAdtAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, MethodGensCheck::new());
    traverse!(&mut ctx, ast_root, quiet, TraitsFnAnalyzer::new());

    let incl_impls = true;
    let full_paths = false;
    let mut dep_order_rev = dependency_order_from_ctx(&mut ctx, ast_root, incl_impls, full_paths)?;
    dep_order_rev.reverse();

    let fn_collector = traverse!(
        &mut ctx,
        ast_root,
        quiet,
        GenericFnCollector::new(&dep_order_rev)
    );
    traverse!(
        &mut ctx,
        ast_root,
        quiet,
        GenericFnCreator::new(fn_collector.generic_methods, fn_collector.generic_fns)
    );

    let adt_collector = traverse!(
        &mut ctx,
        ast_root,
        quiet,
        GenericAdtCollector::new(&dep_order_rev)
    );
    traverse!(
        &mut ctx,
        ast_root,
        quiet,
        GenericAdtCreator::new(adt_collector.generic_adts)
    );

    traverse!(&mut ctx, ast_root, quiet, GenericTysSolvedChecker::new());
    traverse!(&mut ctx, ast_root, quiet, FnReturnAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, VoidAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, ExtStructInit::new());
    traverse!(&mut ctx, ast_root, quiet, AutoDerefAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, UnionInitArg::new());
    traverse!(&mut ctx, ast_root, quiet, TraitMethodsAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, TraitGenericsAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, CmpAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, EqAnalyzer::new());
    traverse!(&mut ctx, ast_root, quiet, CallArgs::new());
    traverse!(&mut ctx, ast_root, quiet, FormatParser::new());
    traverse!(&mut ctx, ast_root, quiet, MatchExhaustAnalyzer::new());

    clean_up(&mut ast_ctx);

    Ok(AnalyzeCtx { ast_ctx, ty_env })
}
