use super::scope_analyzer::ScopeAnalyzer;
use crate::analyze::decl_analyzer::DeclAnalyzer;
use crate::analyze::type_analyzer::TypeAnalyzer;
use crate::parse::token::{BlockId, Enum, Function, Interface, ParseToken, Struct, Variable};
use crate::CustomResult;
use std::collections::HashMap;

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

#[derive(Debug)]
pub struct AnalyzeContext {
    /// Contains all ... that have been seen traversing down to this part of the code.
    pub variables: HashMap<(String, BlockId), Variable>,
    pub functions: HashMap<(String, BlockId), Function>,
    pub structs: HashMap<(String, BlockId), Struct>,
    pub enums: HashMap<(String, BlockId), Enum>,
    pub interfaces: HashMap<(String, BlockId), Interface>,

    /// Maps child blocks to their parents. The key is a child ID and the value
    /// is the ID of the parent.
    pub child_to_parent: HashMap<BlockId, BlockId>,

    /// Stores a block ID as the key and the value is true if the specific
    /// block is a "root" block, false otherwise.
    pub is_root_block: HashMap<BlockId, bool>,

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
            child_to_parent: HashMap::default(),
            is_root_block: HashMap::default(),
            cur_block_id: 0,
        }
    }

    /// Returns a variable with name `ident` that exists either in the block
    /// with ID `blockId` or any of the blocks parents.
    pub fn get_variable(&self, ident: &str, id: BlockId) -> Option<&Variable> {
        if let Some(var) = self.variables.get(&(ident.into(), id)) {
            Some(var)
        } else {
            // Unable to find variable in the current block scope. If this block
            // isn't a "root" block i.e. it has a parent scope, see recursively
            // the variable exists in a parent scope.
            if let Some(false) = self.is_root_block.get(&id) {
                if let Some(parent_id) = self.child_to_parent.get(&id) {
                    self.get_variable(ident, *parent_id)
                } else {
                    unreachable!(format!("no parent found for block with id {}", id));
                }
            } else {
                None
            }
        }
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// the block in which the sought after variable was declared.
    /// If a declaration can't be found, None is returned.
    pub fn get_var_decl_scope(&self, ident: &str, id: BlockId) -> Option<BlockId> {
        if self.variables.get(&(ident.into(), id)).is_some() {
            Some(id)
        } else {
            // Unable to find variable in the current block scope. If this block
            // isn't a "root" block i.e. it has a parent scope, see recursively
            // the variable exists in a parent scope.
            if let Some(false) = self.is_root_block.get(&id) {
                if let Some(parent_id) = self.child_to_parent.get(&id) {
                    self.get_var_decl_scope(ident, *parent_id)
                } else {
                    unreachable!(format!("no parent found for block with id {}", id));
                }
            } else {
                None
            }
        }
    }
}

/// Updates the AST with information about function prototypes and declarations
/// of structures.
pub fn analyze<'a>(ast_root: &mut ParseToken) -> CustomResult<AnalyzeContext> {
    let mut context = AnalyzeContext::new();

    ScopeAnalyzer::analyze(&mut context, ast_root)?;
    TypeAnalyzer::analyze(&mut context, ast_root)?;
    DeclAnalyzer::analyze(&mut context, ast_root)?;

    Ok(context)
}
