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
    ctx::{analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx, ty_ctx::TyCtx, ty_env::TyEnv},
    error::LangError,
    file::{FileId, FileInfo},
    token::ast::AstToken,
    traverse::traverser::AstTraverser,
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
    call_args::CallArgs, clean_up::clean_up, exhaust::ExhaustAnalyzer,
    traits_generic::TraitsGenericAnalyzer, union_init_arg::UnionInitArg,
};
use pre::indexing::IndexingAnalyzer;
use ty::{
    fn_generics_check::FnGenericsCheck, generic_adt_creator::GenericAdtCreator,
    generic_collector::GenericCollector, generic_fn_creator::GenericFnCreator,
    generic_tys_solved::GenericTysSolvedChecker, inferencer::TypeInferencer, solver::TypeSolver,
    traits_fn::TraitsFnAnalyzer,
};

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

    let mut traverser = AstTraverser::new(&mut ast_ctx, &mut ty_ctx);

    debug!("Running IndexingAnalyzer");
    let mut index_analyzer = IndexingAnalyzer::new();
    traverser.traverse_with_visitor(&mut index_analyzer, ast_root)?;

    debug!("Running BlockAnalyzer");
    let mut block_analyzer = BlockAnalyzer::new();
    traverser.traverse_with_visitor(&mut block_analyzer, ast_root)?;

    debug!("Running DeclTypeAnalyzer");
    let mut decl_type_analyzer = DeclTypeAnalyzer::new();
    traverser.traverse_with_visitor(&mut decl_type_analyzer, ast_root)?;

    debug!("Running DeclFnAnalyzer");
    let mut decl_fn_analyzer = DeclFnAnalyzer::new();
    traverser.traverse_with_visitor(&mut decl_fn_analyzer, ast_root)?;

    debug!("Running DeclVarAnalyzer");
    let mut decl_var_analyzer = DeclVarAnalyzer::new();
    traverser
        .add_visitor(&mut decl_var_analyzer)
        .traverse_token(ast_root)
        .clear_visitors()
        .take_errors_with_ctx()?;

    debug!("Lookup tables after decl step:");
    traverser.get_ctx().ast_ctx.debug_print();

    debug!("Running MethodAnalyzer");
    let mut method_analyzer = MethodAnalyzer::new();
    traverser.traverse_with_visitor(&mut method_analyzer, ast_root)?;

    debug!("Running PathResolver");
    let mut path_resolver = PathResolver::new();
    traverser.traverse_with_visitor(&mut path_resolver, ast_root)?;

    debug!("running GenericsAnalyzer");
    let mut generics_analyzer = GenericsAnalyzer::new();
    traverser.traverse_with_visitor(&mut generics_analyzer, ast_root)?;

    debug!("Running DeferAnalyzer");
    let mut defer_analyzer = DeferAnalyzer::new();
    traverser.traverse_with_visitor(&mut defer_analyzer, ast_root)?;

    debug!("Running TypeInferencer");
    let mut type_inference = TypeInferencer::new();
    traverser.traverse_with_visitor(&mut type_inference, ast_root)?;

    debug!("Running TypeSolver");
    let mut type_solver = TypeSolver::new();
    traverser.traverse_with_visitor(&mut type_solver, ast_root)?;

    debug!("Running FnGenericsCheck");
    let mut fn_generics_check = FnGenericsCheck::new();
    traverser.traverse_with_visitor(&mut fn_generics_check, ast_root)?;

    debug!("Running TraitsFnAnalyzer");
    let mut traits_fn_analyzer = TraitsFnAnalyzer::new();
    traverser.traverse_with_visitor(&mut traits_fn_analyzer, ast_root)?;

    debug!("Running GenericCollector");
    let mut generic_collector = GenericCollector::new();
    // TODO: Implement in a safe way.
    let generic_collector_copy = unsafe {
        (&mut generic_collector as *mut GenericCollector)
            .as_mut()
            .unwrap()
    };
    traverser.traverse_with_visitor(&mut generic_collector, ast_root)?;

    let generic_methods = std::mem::take(&mut generic_collector_copy.generic_methods);
    let generic_structs = std::mem::take(&mut generic_collector_copy.generic_adts);

    debug!("Running GenericFnCreator");
    let mut generic_fn_creator = GenericFnCreator::new(generic_methods);
    traverser.traverse_with_visitor(&mut generic_fn_creator, ast_root)?;

    debug!("Running GenericAdtCreator");
    let mut generic_adt_creator = GenericAdtCreator::new(generic_structs);
    traverser.traverse_with_visitor(&mut generic_adt_creator, ast_root)?;

    debug!("before generics check -- AST: {:#?}", &ast_root);

    debug!("Running GenericTysSolvedChecker");
    let mut generic_ty_solved_check = GenericTysSolvedChecker::new();
    traverser.traverse_with_visitor(&mut generic_ty_solved_check, ast_root)?;

    debug!("Running UnionInitArg");
    let mut union_init_arg = UnionInitArg::new();
    traverser.traverse_with_visitor(&mut union_init_arg, ast_root)?;

    debug!("Running CallArgs");
    let mut call_args = CallArgs::new();
    traverser.traverse_with_visitor(&mut call_args, ast_root)?;

    debug!("Running ExhaustAnalyzer");
    let mut exhaust_analyzer = ExhaustAnalyzer::new();
    traverser.traverse_with_visitor(&mut exhaust_analyzer, ast_root)?;

    debug!("Running TraitsGenericAnalyzer");
    let mut traits_generic_analyzer = TraitsGenericAnalyzer::new();
    traverser.traverse_with_visitor(&mut traits_generic_analyzer, ast_root)?;

    clean_up(&mut ast_ctx);

    debug!("after analyzing -- AST: {:#?}", &ast_root);

    Ok(AnalyzeCtx { ast_ctx, ty_ctx })
}
