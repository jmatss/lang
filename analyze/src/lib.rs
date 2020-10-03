mod arg_reorderer;
mod block;
mod decl_func;
mod decl_type;
mod decl_var;
mod defer;
mod indexing;
mod method;
mod type_context;
mod type_inferencer;
mod type_solver;
//mod unitialized;

use arg_reorderer::ArgReorderer;
use block::BlockAnalyzer;
use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    token::{
        ast::AstToken,
        block::{Enum, Function, Interface, Struct},
        expr::Var,
        stmt::Path,
    },
    traverser::AstTraverser,
    BlockId,
};
use decl_func::DeclFuncAnalyzer;
use decl_type::DeclTypeAnalyzer;
use decl_var::DeclVarAnalyzer;
use defer::DeferAnalyzer;
use indexing::IndexingAnalyzer;
use method::MethodAnalyzer;
use std::{cell::RefCell, collections::HashMap};
use type_context::TypeContext;
use type_inferencer::TypeInferencer;
use type_solver::TypeSolver;

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
pub fn analyze(ast_root: &mut AstToken) -> Result<AnalyzeContext, Vec<LangError>> {
    let analyze_context = RefCell::new(AnalyzeContext::new());

    let mut indexing_analyzer = IndexingAnalyzer::new();
    AstTraverser::new()
        .add_visitor(&mut indexing_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut block_analyzer = BlockAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut block_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut decl_type_analyzer = DeclTypeAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut decl_type_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut decl_var_analyzer = DeclVarAnalyzer::new(&analyze_context);
    let mut decl_func_analyzer = DeclFuncAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut decl_func_analyzer)
        .add_visitor(&mut decl_var_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut method_analyzer = MethodAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut method_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut defer_analyzer = DeferAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut defer_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut analyze_context = analyze_context.replace(AnalyzeContext::default());
    let mut type_context = TypeContext::new(&mut analyze_context);

    let mut type_inferencer = TypeInferencer::new(&mut type_context);
    AstTraverser::new()
        .add_visitor(&mut type_inferencer)
        .traverse(ast_root)
        .take_errors()?;

    let mut type_solver = TypeSolver::new(&mut type_context);
    AstTraverser::new()
        .add_visitor(&mut type_solver)
        .traverse(ast_root)
        .take_errors()?;

    let mut arg_reorderer = ArgReorderer::new(&mut analyze_context);
    AstTraverser::new()
        .add_visitor(&mut arg_reorderer)
        .traverse(ast_root)
        .take_errors()?;

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

#[derive(Debug)]
pub struct AnalyzeContext {
    /// Contains all ... that have been seen traversing down to this part of the code.
    /// The BlockId represent the outer scope for a item. For variables it will
    /// be the scope in which they are declared in and for the rest the BlockId
    /// will be their first root parent traversing upwards.
    pub variables: HashMap<(String, BlockId), Var>,
    pub functions: HashMap<(String, BlockId), Function>,
    pub structs: HashMap<(String, BlockId), Struct>,
    pub enums: HashMap<(String, BlockId), Enum>,
    pub interfaces: HashMap<(String, BlockId), Interface>,
    /// A `methods` entry should have a corresponding struct in `structs` with
    /// the same key. The string in the outer map key is the name of the struct
    /// and the string in the inner map is the name of the method.
    pub methods: HashMap<(String, BlockId), HashMap<String, Function>>,

    pub block_info: HashMap<BlockId, BlockInfo>,
    pub use_paths: Vec<Path>,

    /// The line/column where the `analyzer` currently is. When the analyzing is
    /// done, these variable will not be used and will be invalid.
    pub cur_line_nr: u64,
    pub cur_column_nr: u64,
}

impl Default for AnalyzeContext {
    fn default() -> Self {
        AnalyzeContext::new()
    }
}

impl AnalyzeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::default(),
            functions: HashMap::default(),
            structs: HashMap::default(),
            enums: HashMap::default(),
            interfaces: HashMap::default(),
            methods: HashMap::default(),

            block_info: HashMap::default(),
            use_paths: Vec::default(),

            cur_line_nr: 0,
            cur_column_nr: 0,
        }
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// the variable that this combination represents in the `analyze context`.
    pub fn get_var(&mut self, ident: &str, id: BlockId) -> CustomResult<&mut Var> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        let key = (ident.into(), decl_block_id);

        if let Some(context_var) = self.variables.get_mut(&key) {
            return Ok(context_var);
        }

        Err(LangError::new(
            format!(
                "Unable to find var with name \"{}\" in decl block ID {}.",
                ident, decl_block_id
            ),
            AnalyzeError {
                line_nr: 0,
                column_nr: 0,
            },
        ))
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// the block in which the sought after variable was declared.
    pub fn get_var_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        if self.variables.get(&(ident.into(), id)).is_some() {
            Ok(id)
        } else {
            // Unable to find variable in the current block scope. If this block
            // isn't a "root" block i.e. it has a parent scope, see recursively
            // if the variable exists in a parent scope.
            if let Some(block_info) = self.block_info.get(&id) {
                // TODO: How should this work for the default block?
                if id == block_info.parent_id {
                    Err(LangError::new(
                        format!("Block with id {} is its own parent in block info.", id),
                        AnalyzeError {
                            line_nr: self.cur_line_nr,
                            column_nr: self.cur_column_nr,
                        },
                    ))
                } else if !block_info.is_root_block {
                    self.get_var_decl_scope(ident, block_info.parent_id)
                } else {
                    Err(LangError::new(
                        format!(
                            "Unable to find var decl for \"{}\" in scope of root block {}.",
                            ident, id
                        ),
                        AnalyzeError {
                            line_nr: self.cur_line_nr,
                            column_nr: self.cur_column_nr,
                        },
                    ))
                }
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to get var decl block info for block with id: {}",
                        id
                    ),
                    AnalyzeError {
                        line_nr: self.cur_line_nr,
                        column_nr: self.cur_column_nr,
                    },
                ))
            }
        }
    }

    /// Finds root parent BlockId containing the struct with name `ident` in a
    /// scope containing the block with BlockId `id`.
    pub fn get_struct_parent_id(&self, ident: String, id: BlockId) -> CustomResult<BlockId> {
        let mut cur_id = id;

        while let Some(block_info) = self.block_info.get(&cur_id) {
            let key = (ident.clone(), cur_id);
            if self.structs.contains_key(&key) {
                return Ok(cur_id);
            }
            cur_id = block_info.parent_id;
        }

        Err(LangError::new(
            format!(
                "Unable to get struct \"{}\" block info for block with id: {}, ended at ID: {}.",
                &ident, &id, &cur_id
            ),
            AnalyzeError {
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            },
        ))
    }

    /// Given a block id `id`, returns the ID for the first root block
    /// "surounding" the `id` block. Example: For a if-statement, the root block
    /// will be the function containing the if-statement.
    pub fn get_root_parent(&self, id: BlockId) -> CustomResult<BlockId> {
        let mut cur_id = id;

        while let Some(block_info) = self.block_info.get(&cur_id) {
            // If this is a root block, return the ID; otherwise continue the loop.
            if block_info.is_root_block {
                return Ok(cur_id);
            } else {
                cur_id = block_info.parent_id;
            }
        }

        Err(LangError::new(
            format!(
                "Unable to get root block info for block with id {}, ended at block ID: {}.",
                &id, &cur_id
            ),
            AnalyzeError {
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            },
        ))
    }

    /// Given a block id `id`, returns the ID for the first root block
    /// "surounding" the `id` block. This function will NOT consider the
    /// current block with ID `id` a root block, it will look for its first
    /// parent instead.
    pub fn get_next_root_parent(&self, id: BlockId) -> CustomResult<BlockId> {
        // Get the parent id before the main loop so that the given `id` is
        // never tried to be a root block which it can be. But we are not
        // interested of this block, only its parents.
        let parent_id = if let Some(block_info) = self.block_info.get(&id) {
            block_info.parent_id
        } else {
            return Err(LangError::new(
                format!("The given block id doesn't have a parent: {}", id),
                AnalyzeError {
                    line_nr: self.cur_line_nr,
                    column_nr: self.cur_column_nr,
                },
            ));
        };

        self.get_root_parent(parent_id)
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id`. The struct can ex. be declared in parent block scope.
    pub fn get_struct(&self, struct_name: &str, id: BlockId) -> CustomResult<&Struct> {
        let parent_block_id = self.get_struct_parent_id(struct_name.into(), id)?;
        let key = (struct_name.into(), parent_block_id);

        if let Some(struct_) = self.structs.get(&key) {
            Ok(struct_)
        } else {
            Err(LangError::new(
                format!(
                    "Unable to find struct with name \"{}\" with child block ID {}.",
                    &struct_name, id
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the member with name `member_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_member(
        &self,
        struct_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<&Var> {
        let struct_ = self.get_struct(struct_name, id)?;
        if let Some(members) = &struct_.members {
            if let Some(var) = members.iter().find(|member| member.name == member_name) {
                Ok(var)
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to find member with name \"{}\" in struct \"{}\".",
                        &member_name, &struct_name
                    ),
                    AnalyzeError {
                        line_nr: 0,
                        column_nr: 0,
                    },
                ))
            }
        } else {
            Err(LangError::new(
                format!(
                    "Struct \"{}\" has no members but tried to access member \"{:?}\".",
                    &struct_name, &member_name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the index of the member with name `member_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_member_index(
        &self,
        struct_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        let struct_ = self.get_struct(struct_name, id)?;
        if let Some(members) = &struct_.members {
            let mut idx = 0;
            for member in members.iter() {
                if member_name == member.name {
                    return Ok(idx);
                }
                idx += 1;
            }

            Err(LangError::new(
                format!(
                    "Unable to find member with name \"{}\" in struct \"{}\".",
                    &member_name, &struct_name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        } else {
            Err(LangError::new(
                format!(
                    "Struct \"{}\" has no members but tried to access member \"{:?}\".",
                    &struct_name, &member_name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the method with name `method_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_method(
        &self,
        struct_name: &str,
        method_name: &str,
        id: BlockId,
    ) -> CustomResult<&Function> {
        let struct_parent_id = match self.get_struct_parent_id(struct_name.into(), id) {
            Ok(id) => id,
            Err(err) => return Err(err),
        };

        let key = (struct_name.into(), struct_parent_id);
        if let Some(struct_methods) = self.methods.get(&key) {
            if let Some(method) = struct_methods.get(method_name) {
                Ok(method)
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to find method \"{}\" in struct \"{}\" with parent ID {}.",
                        &method_name, &struct_name, &id
                    ),
                    AnalyzeError {
                        line_nr: 0,
                        column_nr: 0,
                    },
                ))
            }
        } else {
            Err(LangError::new(
                format!(
                    "Unable to find methods for struct \"{}\" with parent ID {}.",
                    &struct_name, &id
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`
    /// in the method with name `method_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_method_param_idx(
        &self,
        struct_name: &str,
        method_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        let method = self.get_struct_method(struct_name, method_name, id)?;

        if let Some(params) = &method.parameters {
            let mut idx: u64 = 0;
            for param in params {
                if param_name == param.name {
                    return Ok(idx);
                }
                idx += 1;
            }

            Err(LangError::new(
                format!(
                    "Unable to find param with name \"{}\" in method \"{}\" in struct \"{}\".",
                    &param_name, &method_name, &struct_name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        } else {
            Err(LangError::new(
                format!(
                    "Method \"{}\" in struct \"{}\" had no parameters, expected param with name: {}",
                    &method_name, &struct_name, &param_name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`.
    pub fn get_func_param_idx(
        &self,
        func_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        // TODO: Don't hardcode the default BlockId 0.
        let parent_id = 0;
        let key = (func_name.into(), parent_id);
        let func = if let Some(func) = self.functions.get(&key) {
            func
        } else {
            return Err(LangError::new(
                format!(
                    "Unable to find function \"{}\" in decl block ID \"{}\".",
                    &func_name, parent_id
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ));
        };

        if let Some(params) = &func.parameters {
            let mut idx: u64 = 0;
            for param in params {
                if param_name == param.name {
                    return Ok(idx);
                }
                idx += 1;
            }

            Err(LangError::new(
                format!(
                    "Unable to find param with name \"{}\" in function \"{}\".",
                    &param_name, &func_name,
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        } else {
            Err(LangError::new(
                format!(
                    "Function \"{}\" had no parameters, expected param with name: {}",
                    &func_name, &param_name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new_backtrace(
            msg,
            AnalyzeError {
                line_nr: self.cur_line_nr,
                column_nr: self.cur_column_nr,
            },
            true,
        )
    }
}
