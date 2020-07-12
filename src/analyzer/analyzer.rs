use crate::analyzer::declaration_analyzer::DeclarationAnalyzer;
use crate::analyzer::type_analyzer::TypeAnalyzer;
use crate::parser::abstract_syntax_tree::{ScopeIndex, AST};
use crate::parser::token::{Class, Enum, Function, Interface, Macro, Token, TypeStruct};
use crate::CustomResult;
use inkwell::values::PointerValue;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VariableState<'a> {
    pub tokens: Vec<Token>,
    pub base_type: Option<TypeStruct>,
    pub pointer: Option<PointerValue<'a>>,
}

impl<'a> VariableState<'a> {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            base_type: None,
            pointer: None,
        }
    }
}

#[derive(Debug)]
pub struct AnalyzeContext<'a> {
    /// Contains all variables that have been seen traversing down to this part of the code.
    /// The outer map has the variable name as key and the inner map has the scope index as key
    /// and the variable state as the value which contains information about the variable.
    pub variables: HashMap<String, HashMap<ScopeIndex, VariableState<'a>>>,

    /// Contains all functions that have been seen traversing down to this part of the code.
    /// The outer map has the variable name as key and the inner map has the scope as the key and
    /// information about the function as the value.
    pub functions: HashMap<String, HashMap<ScopeIndex, Function>>,

    pub classes: HashMap<String, HashMap<ScopeIndex, Class>>,
    pub enums: HashMap<String, HashMap<ScopeIndex, Enum>>,
    pub interfaces: HashMap<String, HashMap<ScopeIndex, Interface>>,
    pub macros: HashMap<String, HashMap<ScopeIndex, Macro>>,

    /// Temporary scope values.
    // TODO: To this in a better way
    /// The key is a child scope and the value is the parent of that scope.
    pub parent_scopes: HashMap<ScopeIndex, ScopeIndex>,

    /// The scope that the `analyzer` currently is in.
    pub current_scope: ScopeIndex,
}

impl<'a> AnalyzeContext<'a> {
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
