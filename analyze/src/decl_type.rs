use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::ast::Token,
    token::{ast::AstToken, block::BlockHeader},
    traverser::TraverseContext,
    visitor::Visitor,
};
use std::cell::RefCell;

/// Gathers information about all type declarations found in the AST and inserts
/// them into the `analyze_context`. This includes  structs, enums and interfaces.
pub struct DeclTypeAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a> DeclTypeAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for DeclTypeAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.analyze_context.borrow_mut().cur_line_nr = ast_token.line_nr;
        self.analyze_context.borrow_mut().cur_column_nr = ast_token.column_nr;
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Token::Block(BlockHeader::Struct(struct_), struct_id, ..) = &ast_token.token {
            // Add the struct in the scope of its root parent (`root_parent_id`).
            let root_parent_id = match analyze_context.get_next_root_parent(*struct_id) {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (struct_.name.clone(), root_parent_id);
            if let Some(prev_struct) = analyze_context.structs.get(&key) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = analyze_context.err(format!(
                    "A struct with name \"{}\" already defined.",
                    prev_struct.name
                ));
                self.errors.push(err);
            } else {
                analyze_context.structs.insert(key, struct_.clone());
            }
        }
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Token::Block(BlockHeader::Enum(enum_), enum_id, ..) = &ast_token.token {
            // Add the enum in the scope of its root parent (`root_parent_id`).
            let root_parent_id = match analyze_context.get_next_root_parent(*enum_id) {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (enum_.name.clone(), root_parent_id);
            if let Some(prev_enum) = analyze_context.enums.get(&key) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = analyze_context.err(format!(
                    "A enum with name \"{}\" already defined.",
                    prev_enum.name
                ));
                self.errors.push(err);
            } else {
                analyze_context.enums.insert(key, enum_.clone());
            }
        }
    }

    fn visit_interface(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Token::Block(BlockHeader::Interface(interface), interface_id, ..) = &ast_token.token
        {
            // Add the interface in the scope of its root parent (`root_parent_id`).
            let root_parent_id = match analyze_context.get_next_root_parent(*interface_id) {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (interface.name.clone(), root_parent_id);
            if let Some(prev_interface) = analyze_context.interfaces.get(&key) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = analyze_context.err(format!(
                    "A interface with name \"{}\" already defined.",
                    prev_interface.name
                ));
                self.errors.push(err);
            } else {
                analyze_context.interfaces.insert(key, interface.clone());
            }
        }
    }
}
