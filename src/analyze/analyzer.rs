use super::{block_analyzer::BlockAnalyzer, indexing_analyzer::IndexingAnalyzer};
use crate::analyze::decl_analyzer::DeclAnalyzer;
use crate::analyze::type_analyzer::TypeAnalyzer;
use crate::error::{LangError, LangErrorKind::AnalyzeError};
use crate::parse::token::{BlockId, Enum, Function, Interface, ParseToken, Struct, Variable};
use crate::CustomResult;
use std::collections::HashMap;

// TODO: Error if a function that doesn't have a return type has a return in it.

/// Updates the AST with information about function prototypes and declarations
/// of structures.
/// The TypeAnalyzer depends on the DeclAnalyzer to figure out types for
/// function calls and function parameters used in expressions, so the DeclAnalyzer
/// needs to be ran TypeAnalyzer.
/// The TypeAnalyzer will also set the var types in "AnalyzeContext.variables".
/// The DeclAnalyzer depends on the running first IndexingAnalyzer.
pub fn analyze(ast_root: &mut ParseToken) -> CustomResult<AnalyzeContext> {
    let mut context = AnalyzeContext::new();

    BlockAnalyzer::analyze(&mut context, ast_root)?;
    IndexingAnalyzer::analyze(&mut context, ast_root);
    DeclAnalyzer::analyze(&mut context, ast_root)?;
    TypeAnalyzer::analyze(&mut context, ast_root)?;

    Ok(context)
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
    pub contains_with: bool,

    /// Contains information about the control flow of children. If all children
    /// contains a branch instruction, this block doesn't not need to add a
    /// implicit "terminator" at the end of the basic block, since there is no
    /// logical path that leads to the end of this block.
    pub all_children_contains_branches: bool,

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
}

impl BlockInfo {
    pub fn new(block_id: BlockId, is_root_block: bool) -> Self {
        Self {
            block_id,
            parent_id: usize::MAX,
            contains_return: false,
            contains_yield: false,
            contains_break: false,
            contains_continue: false,
            contains_defer: false,
            contains_with: false,
            all_children_contains_branches: false,
            is_root_block,
        }
    }
}

#[derive(Debug)]
pub struct AnalyzeContext {
    /// Contains all ... that have been seen traversing down to this part of the code.
    /// The BlockId represent the outer scope for a item. For variables it will
    /// be the scope in which they are declared in and for the rest the BlockId
    /// will be their first root parent traversing upwards.
    pub variables: HashMap<(String, BlockId), Variable>,
    pub functions: HashMap<(String, BlockId), Function>,
    pub structs: HashMap<(String, BlockId), Struct>,
    pub enums: HashMap<(String, BlockId), Enum>,
    pub interfaces: HashMap<(String, BlockId), Interface>,

    pub block_info: HashMap<BlockId, BlockInfo>,

    /// The block ID that the `analyzer` currently is in. When the analyzing is
    /// done, this variable will not be used and will be invalid.
    pub cur_block_id: BlockId,
}

impl AnalyzeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::default(),
            functions: HashMap::default(),
            structs: HashMap::default(),
            enums: HashMap::default(),
            interfaces: HashMap::default(),
            block_info: HashMap::default(),
            cur_block_id: 0,
        }
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
                        AnalyzeError,
                    ))
                } else if !block_info.is_root_block {
                    self.get_var_decl_scope(ident, block_info.parent_id)
                } else {
                    Err(LangError::new(
                        format!(
                            "Unable to find var decl for \"{}\" in scope of root block {}.",
                            ident, id
                        ),
                        AnalyzeError,
                    ))
                }
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to get var decl block info for block with id: {}",
                        id
                    ),
                    AnalyzeError,
                ))
            }
        }
    }

    /// Finds root parent BlockId containing the function with name `ident` in a
    /// scope containing the block with BlockId `id`.
    pub fn get_func_parent_id(&self, ident: String, id: BlockId) -> CustomResult<BlockId> {
        let mut cur_id = id;

        while let Some(block_info) = self.block_info.get(&cur_id) {
            let key = (ident.clone(), cur_id);
            if self.functions.contains_key(&key) {
                return Ok(cur_id);
            }
            cur_id = block_info.parent_id;
        }

        Err(LangError::new(
            format!(
                "Unable to get func \"{}\" block info for block with id: {}, ended at ID: {}.",
                &ident, &id, &cur_id
            ),
            AnalyzeError,
        ))
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
            AnalyzeError,
        ))
    }

    /// Given a block id `id`, returns the ID for the first root block
    /// "surounding" the `id` block. Example: For a if-statement, the root block
    /// will be the function containing the if-statement.
    pub fn get_root_parent(&self, id: BlockId) -> CustomResult<BlockId> {
        // Get the parent id before the main loop so that the given `id` is
        // never tried to be a root block which it can be. But we are not
        // interested of this block, only its parents.
        let mut cur_id = if let Some(block_info) = self.block_info.get(&id) {
            block_info.parent_id
        } else {
            return Err(LangError::new(
                format!("The given block id doesn't have a parent: {}", id),
                AnalyzeError,
            ));
        };

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
            AnalyzeError,
        ))
    }
}
