use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::ast::Token,
    token::{
        ast::AstToken,
        block::{BlockHeader, Enum, Interface, Struct},
    },
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
        self.analyze_context.borrow_mut().line_nr = ast_token.line_nr;
        self.analyze_context.borrow_mut().column_nr = ast_token.column_nr;
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Struct(struct_), struct_id, ..) = &mut ast_token.token {
            let mut analyze_context = self.analyze_context.borrow_mut();

            // The struct will be added in the scope of its parent, so fetch the
            // block id for the parent.
            let parent_id = match analyze_context.get_parent(*struct_id) {
                Ok(parent_id) => parent_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            if let Ok(prev_struct) = analyze_context.get_struct(&struct_.name, *struct_id) {
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
                // Add the struct into decl lookup maps.
                let key = (struct_.name.clone(), parent_id);
                let struct_ptr = struct_.as_mut() as *mut Struct;
                analyze_context.structs.insert(key, struct_ptr);
            }
        }
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Enum(enum_), enum_id, ..) = &mut ast_token.token {
            let mut analyze_context = self.analyze_context.borrow_mut();

            // The enum will be added in the scope of its parent, so fetch the
            // block id for the parent.
            let parent_id = match analyze_context.get_parent(*enum_id) {
                Ok(parent_id) => parent_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            if let Ok(prev_enum) = analyze_context.get_enum(&enum_.name, *enum_id) {
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
                // Add the enum into decl lookup maps.
                let key = (enum_.name.clone(), parent_id);
                let enum_ptr = enum_.as_mut() as *mut Enum;
                analyze_context.enums.insert(key, enum_ptr);
            }
        }
    }

    fn visit_interface(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Interface(interface), interface_id, ..) =
            &mut ast_token.token
        {
            let mut analyze_context = self.analyze_context.borrow_mut();

            // The interface will be added in the scope of its parent, so fetch
            // the block id for the parent.
            let parent_id = match analyze_context.get_parent(*interface_id) {
                Ok(parent_id) => parent_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            if let Ok(prev_enum) = analyze_context.get_interface(&interface.name, *interface_id) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = analyze_context.err(format!(
                    "A interface with name \"{}\" already defined.",
                    prev_enum.name
                ));
                self.errors.push(err);
            } else {
                // Add the interface into decl lookup maps.
                let key = (interface.name.clone(), parent_id);
                let interface_ptr = interface.as_mut() as *mut Interface;
                analyze_context.interfaces.insert(key, interface_ptr);
            }
        }
    }
}
