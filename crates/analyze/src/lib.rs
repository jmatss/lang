mod decl;
mod mid;
mod post;
mod pre;
mod ty;
pub mod util;
//mod unitialized;

use std::collections::HashMap;

use log::debug;

use common::{
    ctx::{
        analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx, traverse_ctx::TraverseCtx, ty_ctx::TyCtx,
        ty_env::TyEnv,
    },
    error::LangError,
    file::{FileId, FileInfo},
    token::ast::AstToken,
    traverse::traverser::traverse,
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
    call_args::CallArgs, clean_up::clean_up, match_exhaust::MatchExhaustAnalyzer,
    method_this::MethodThisAnalyzer, traits_generic::TraitsGenericAnalyzer,
    union_init_arg::UnionInitArg,
};
use pre::{indexing::IndexingAnalyzer, main_args::MainArgsAnalyzer};
use ty::{
    fn_generics_check::FnGenericsCheck, generic_adt_collector::GenericAdtCollector,
    generic_adt_creator::GenericAdtCreator, generic_fn_collector::GenericFnCollector,
    generic_fn_creator::GenericFnCreator, generic_tys_solved::GenericTysSolvedChecker,
    inferencer::TypeInferencer, solver::TypeSolver, traits_fn::TraitsFnAnalyzer,
};
use util::order::dependency_order_from_ctx;

use crate::post::format::FormatParser;

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
pub fn analyze(
    ast_root: &mut AstToken,
    mut ty_env: TyEnv,
    file_info: HashMap<FileId, FileInfo>,
) -> Result<AnalyzeCtx, Vec<LangError>> {
    let built_ins = init_built_ins(&mut ty_env).map_err(|err| vec![err])?;

    let mut ast_ctx = match AstCtx::new(built_ins, file_info) {
        Ok(ast_ctx) => ast_ctx,
        Err(err) => return Err(vec![err]),
    };
    let mut ty_ctx = TyCtx::new(ty_env);
    let mut ctx = TraverseCtx::new(&mut ast_ctx, &mut ty_ctx);

    debug!("Running MainArgsAnalyzer");
    let mut main_analyzer = MainArgsAnalyzer::new();
    traverse(&mut ctx, &mut main_analyzer, ast_root)?;

    debug!("Running IndexingAnalyzer");
    let mut index_analyzer = IndexingAnalyzer::new();
    traverse(&mut ctx, &mut index_analyzer, ast_root)?;

    debug!("Running BlockAnalyzer");
    let mut block_analyzer = BlockAnalyzer::new();
    traverse(&mut ctx, &mut block_analyzer, ast_root)?;

    debug!("Running DeclTypeAnalyzer");
    let mut decl_type_analyzer = DeclTypeAnalyzer::new();
    traverse(&mut ctx, &mut decl_type_analyzer, ast_root)?;

    debug!("Running DeclFnAnalyzer");
    let mut decl_fn_analyzer = DeclFnAnalyzer::new();
    traverse(&mut ctx, &mut decl_fn_analyzer, ast_root)?;

    debug!("Running DeclVarAnalyzer");
    let mut decl_var_analyzer = DeclVarAnalyzer::new();
    traverse(&mut ctx, &mut decl_var_analyzer, ast_root)?;

    debug!("Lookup tables after decl step:");
    ctx.ast_ctx.debug_print();

    debug!("Running MethodAnalyzer");
    let mut method_analyzer = MethodAnalyzer::new();
    traverse(&mut ctx, &mut method_analyzer, ast_root)?;

    debug!("Running PathResolver");
    let mut path_resolver = PathResolver::new();
    traverse(&mut ctx, &mut path_resolver, ast_root)?;

    debug!("running GenericsAnalyzer");
    let mut generics_analyzer = GenericsAnalyzer::new();
    traverse(&mut ctx, &mut generics_analyzer, ast_root)?;

    debug!("Running DeferAnalyzer");
    let mut defer_analyzer = DeferAnalyzer::new();
    traverse(&mut ctx, &mut defer_analyzer, ast_root)?;

    debug!("Running TypeInferencer");
    let mut type_inference = TypeInferencer::new();
    traverse(&mut ctx, &mut type_inference, ast_root)?;

    debug!("Running TypeSolver");
    let mut type_solver = TypeSolver::new();
    traverse(&mut ctx, &mut type_solver, ast_root)?;

    debug!("Running FnGenericsCheck");
    let mut fn_generics_check = FnGenericsCheck::new();
    traverse(&mut ctx, &mut fn_generics_check, ast_root)?;

    debug!("Running TraitsFnAnalyzer");
    let mut traits_fn_analyzer = TraitsFnAnalyzer::new();
    traverse(&mut ctx, &mut traits_fn_analyzer, ast_root)?;

    let incl_impls = true;
    let full_paths = false;
    let mut dep_order_rev = dependency_order_from_ctx(&mut ctx, ast_root, incl_impls, full_paths)?;
    dep_order_rev.reverse();

    debug!("Running GenericFnCollector");
    let mut generic_fn_collector = GenericFnCollector::new(&dep_order_rev);
    traverse(&mut ctx, &mut generic_fn_collector, ast_root)?;

    debug!("Running GenericFnCreator");
    let mut generic_fn_creator = GenericFnCreator::new(generic_fn_collector.generic_methods);
    traverse(&mut ctx, &mut generic_fn_creator, ast_root)?;

    debug!("Running GenericAdtCollector");
    let mut generic_adt_collector = GenericAdtCollector::new(&dep_order_rev);
    traverse(&mut ctx, &mut generic_adt_collector, ast_root)?;

    debug!("Running GenericAdtCreator");
    let mut generic_adt_creator = GenericAdtCreator::new(generic_adt_collector.generic_adts);
    traverse(&mut ctx, &mut generic_adt_creator, ast_root)?;

    debug!("before generics check -- AST: {:#?}", &ast_root);

    debug!("Running GenericTysSolvedChecker");
    let mut generic_ty_solved_check = GenericTysSolvedChecker::new();
    traverse(&mut ctx, &mut generic_ty_solved_check, ast_root)?;

    debug!("Running MethodThisAnalyzer");
    let mut method_this_analyzer = MethodThisAnalyzer::new();
    traverse(&mut ctx, &mut method_this_analyzer, ast_root)?;

    debug!("Running UnionInitArg");
    let mut union_init_arg = UnionInitArg::new();
    traverse(&mut ctx, &mut union_init_arg, ast_root)?;

    debug!("Running CallArgs");
    let mut call_args = CallArgs::new();
    traverse(&mut ctx, &mut call_args, ast_root)?;

    debug!("Running FormatAnalyzer");
    let mut format_analyzer = FormatParser::new();
    traverse(&mut ctx, &mut format_analyzer, ast_root)?;

    debug!("Running MatchExhaustAnalyzer");
    let mut exhaust_analyzer = MatchExhaustAnalyzer::new();
    traverse(&mut ctx, &mut exhaust_analyzer, ast_root)?;

    debug!("Running TraitsGenericAnalyzer");
    let mut traits_generic_analyzer = TraitsGenericAnalyzer::new();
    traverse(&mut ctx, &mut traits_generic_analyzer, ast_root)?;

    clean_up(&mut ast_ctx);

    debug!("after analyzing -- AST: {:#?}", &ast_root);

    Ok(AnalyzeCtx { ast_ctx, ty_ctx })
}
