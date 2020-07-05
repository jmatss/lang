use crate::analyzer::declaration_analyzer::DeclarationAnalyzer;
use crate::analyzer::type_analyzer::TypeAnalyzer;
use crate::parser::abstract_syntax_tree::{ScopeIndex, AST};
use crate::parser::token::{Class, Enum, Function, Interface, Macro, Token, TypeStruct};
use crate::CustomResult;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VariableState {
    pub tokens: Vec<Token>,
    pub base_type: Option<TypeStruct>,
}

impl VariableState {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            base_type: None,
        }
    }
}

#[derive(Debug)]
pub struct AnalyzeContext {
    /// Contains all variables that have been seen traversing down to this part of the code.
    /// The outer map has a map that represents the scope that it is declared in and the
    /// inner map has the variable name as the key and the variable state as the value
    /// which contains all uses and the assumed actual type.
    pub variables: HashMap<ScopeIndex, HashMap<String, VariableState>>,

    // TODO: Need to have a list of all functions so that one can figure out the type
    //       of it when used in a expression.
    /// Contains all function calls that have been seen traversing down to this part of the code.
    /// The outer map has a map that represents the scope that it is used in and the
    /// inner map has the variable name as the key and the actual token as value.
    pub functions: HashMap<ScopeIndex, HashMap<String, Function>>,

    pub classes: HashMap<ScopeIndex, HashMap<String, Class>>,
    pub enums: HashMap<ScopeIndex, HashMap<String, Enum>>,
    pub interfaces: HashMap<ScopeIndex, HashMap<String, Interface>>,
    pub macros: HashMap<ScopeIndex, HashMap<String, Macro>>,

    /// Temporary scope values.
    // TODO: To this in a better way
    /// The key is a child scope and the value is the parent of that scope.
    pub parent_scopes: HashMap<ScopeIndex, ScopeIndex>,

    /// The scope that the `analyzer` currently are in.
    pub current_scope: ScopeIndex,
}

impl AnalyzeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            enums: HashMap::new(),
            interfaces: HashMap::new(),
            macros: HashMap::new(),
            parent_scopes: HashMap::new(),
            current_scope: 0,
        }
    }
}

/// Updates the AST with information about function prototypes and declarations
/// of structures.
pub fn analyze(ast: &mut AST) -> CustomResult<AnalyzeContext> {
    let mut context = AnalyzeContext::new();

    DeclarationAnalyzer::analyze(&mut context, ast)?;
    TypeAnalyzer::analyze(&mut context, ast)?;

    Ok(context)
}
