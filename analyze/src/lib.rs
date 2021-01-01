pub mod context;
mod decl;
mod mid;
mod post;
mod pre;
mod ty;
//mod unitialized;

use common::{
    error::LangError,
    file::{FileId, FileInfo},
    token::ast::AstToken,
    traverser::AstTraverser,
    BlockId,
};
use context::AnalyzeContext;
use decl::{
    block::BlockAnalyzer, func::DeclFuncAnalyzer, ty::DeclTypeAnalyzer, var::DeclVarAnalyzer,
};
use log::debug;
use mid::{defer::DeferAnalyzer, generics::GenericsAnalyzer};
use post::{call_args::CallArgs, clean_up::clean_up, exhaust::ExhaustAnalyzer};
use pre::{indexing::IndexingAnalyzer, method::MethodAnalyzer};
use std::{cell::RefCell, collections::HashMap, fmt::Debug};
use ty::{
    context::TypeContext, converter::TypeConverter, inferencer::TypeInferencer, solver::TypeSolver,
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
/// "decl_func_analyzer"/"decl_type_analyzer"/"type..." needs to be ran before
/// the "call_args" since it needs to access structs, functions and methods.
pub fn analyze(
    ast_root: &mut AstToken,
    file_info: HashMap<FileId, FileInfo>,
) -> Result<AnalyzeContext, Vec<LangError>> {
    let analyze_context = RefCell::new(AnalyzeContext::new(file_info));

    debug!("Running IndexingAnalyzer");
    let mut indexing_analyzer = IndexingAnalyzer::new();
    AstTraverser::new()
        .add_visitor(&mut indexing_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running BlockAnalyzer");
    let mut block_analyzer = BlockAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut block_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running DeclTypeAnalyzer");
    let mut decl_type_analyzer = DeclTypeAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut decl_type_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running DeclVarAnalyzer, running DeclFuncAnalyzer");
    let mut decl_var_analyzer = DeclVarAnalyzer::new(&analyze_context);
    let mut decl_func_analyzer = DeclFuncAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut decl_func_analyzer)
        .add_visitor(&mut decl_var_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running MethodAnalyzer");
    let mut method_analyzer = MethodAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut method_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("running GenericsAnalyzer");
    let mut generics_analyzer = GenericsAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut generics_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running DeferAnalyzer");
    let mut defer_analyzer = DeferAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut defer_analyzer)
        .traverse_token(ast_root)
        .take_errors()?;

    let mut analyze_context = analyze_context.replace(AnalyzeContext::default());
    let mut type_context = TypeContext::new(&mut analyze_context);

    debug!("Running TypeInferencer");
    let mut type_inferencer = TypeInferencer::new(&mut type_context);
    AstTraverser::new()
        .add_visitor(&mut type_inferencer)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running TypeSolver");
    let mut type_solver = TypeSolver::new(&mut type_context);
    AstTraverser::new()
        .add_visitor(&mut type_solver)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running TypeConverter");
    let generic_structures = type_solver.generic_structures;
    let mut type_converter = TypeConverter::new(&mut analyze_context, generic_structures);
    AstTraverser::new()
        .add_visitor(&mut type_converter)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running ExhaustAnalyzer");
    let mut exhaust_analyze = ExhaustAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut exhaust_analyze)
        .traverse_token(ast_root)
        .take_errors()?;

    debug!("Running CallArgs");
    let mut call_args = CallArgs::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut call_args)
        .traverse_token(ast_root)
        .take_errors()?;

    clean_up(&mut analyze_context);

    Ok(analyze_context)
}

#[derive(Debug, Clone)]
pub struct BlockInfo {
    pub block_id: BlockId,
    pub parent_id: BlockId,

    /// Contains information about which control flow statements this block contains.
    /// This will be used during code generation to figure out which
    /// instructions needs to be generated and where branches should jump etc.
    pub contains_return: bool,
    pub contains_yield: bool,
    pub contains_break: bool,
    pub contains_continue: bool,
    pub contains_defer: bool,

    /// Contains information about the control flow of children. If all children
    /// contains a branch instruction, this block doesn't not need to add a
    /// implicit "terminator" at the end of the basic block, since there is no
    /// logical path that leads to the end of this block.
    pub all_children_contains_returns: bool,

    /// A "block root" is a block that starts a new scope that contains blocks that
    /// only have access to items inside this scope.
    /// For example:
    /// ```
    /// fn f() {            // <- BlockId 0
    ///   if (true) {       // <- BlockId 1
    ///     if (false) {}   // <- BlockId 2
    ///   }
    /// }
    /// ```
    /// In this example blocks 1 and 2 only lives inside block 0 (a function).
    /// So the "root block" for 1 & 2 would be 0. In turn the function would
    /// have a surrounding "root block" (which isn't shown in this example).
    pub is_root_block: bool,

    /// Indicates if this block is a "branchable" block that can contain "break"
    /// and "continue" statements. This is true for while-loops while it is not
    /// true for if-statements.
    pub is_branchable_block: bool,
}

impl BlockInfo {
    /// The block id given to the default block.
    const DEFAULT_BLOCK_ID: BlockId = 0;

    pub fn new(block_id: BlockId, is_root_block: bool, is_branchable_block: bool) -> Self {
        Self {
            block_id,
            parent_id: usize::MAX,
            contains_return: false,
            contains_yield: false,
            contains_break: false,
            contains_continue: false,
            contains_defer: false,
            all_children_contains_returns: false,
            is_root_block,
            is_branchable_block,
        }
    }
}
