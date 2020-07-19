use super::scope_analyzer::ScopeAnalyzer;
use crate::analyze::decl_analyzer::DeclAnalyzer;
use crate::analyze::type_analyzer::TypeAnalyzer;
use crate::parse::token::{BlockId, Enum, Function, Interface, ParseToken, Struct, Variable};
use crate::{common::decl_container::DeclContainer, CustomResult};
use std::collections::HashMap;

/// A tuple of a block and if it is a "block root" i.e. a block that starts a
/// new scope that contains blocks that only have access to items inside this scope.
///
/// For example:
///   fn f() {            // <- BlockId 0
///     if (true) {       // <- BlockId 1
///       if (false) {}   // <- BlockId 2
///     }
///   }
/// In this example blocks 1 and 2 only lives inside block 0 (a function).
/// So the "root block" for 1 & 2 would be 0. In turn the function would
/// have a surrounding "root block" (which isn't shown in this example).
#[derive(Debug, Clone)]
pub struct BlockNode {
    pub block_id: BlockId,
    pub is_root: bool,
}

impl BlockNode {
    pub fn new(block_id: BlockId, is_root: bool) -> Self {
        Self { block_id, is_root }
    }
}

#[derive(Debug)]
pub struct AnalyzeContext<'ctx> {
    /// Contains all ... that have been seen traversing down to this part of the code.
    pub variables: DeclContainer<'ctx, Variable>,
    pub functions: DeclContainer<'ctx, Function>,
    pub structs: DeclContainer<'ctx, Struct>,
    pub enums: DeclContainer<'ctx, Enum>,
    pub interfaces: DeclContainer<'ctx, Interface>,

    /// Maps child blocks to their parents. The key is a child ID and the value
    /// is the BlockNode of its parent.
    pub child_to_parent: HashMap<BlockId, BlockNode>,

    /// The block ID that the `analyzer` currently is in.
    pub cur_block_id: BlockId,
}

impl<'ctx> AnalyzeContext<'ctx> {
    pub fn new() -> Self {
        Self {
            variables: DeclContainer::new(),
            functions: DeclContainer::new(),
            structs: DeclContainer::new(),
            enums: DeclContainer::new(),
            interfaces: DeclContainer::new(),
            child_to_parent: HashMap::new(),
            cur_block_id: 0,
        }
    }
}

/// Updates the AST with information about function prototypes and declarations
/// of structures.
pub fn analyze<'a>(ast_root: &mut ParseToken) -> CustomResult<AnalyzeContext<'a>> {
    let mut context = AnalyzeContext::new();

    ScopeAnalyzer::analyze(&mut context, ast_root)?;
    TypeAnalyzer::analyze(&mut context, ast_root)?;
    DeclAnalyzer::analyze(&mut context, ast_root)?;

    Ok(context)
}
