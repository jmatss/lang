use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader},
    },
    traverser::TraverseContext,
    visitor::Visitor,
    BlockId,
};
use std::{cell::RefCell, rc::Rc};

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

    fn decl_new_adt(&mut self, adt: &Rc<RefCell<Adt>>, id: BlockId) {
        let parent_id = match self.analyze_context.borrow().get_parent_id(id) {
            Ok(parent_id) => parent_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // TODO: Should this be done in the same way as function, that
        //       one just checks that the declarations are equals and doesn't
        //       throw a exception? This would allow for "extern" declarations
        //       but might be problematic if it two defines.
        if let Ok(prev_adt) = self
            .analyze_context
            .borrow()
            .get_adt(&adt.borrow().name, id)
        {
            let err = self.analyze_context.borrow().err(format!(
                "A ADT with name \"{}\" already defined.",
                prev_adt.borrow().name
            ));
            self.errors.push(err);
            return;
        }

        let key = (adt.borrow().name.clone(), parent_id);
        self.analyze_context
            .borrow_mut()
            .adts
            .insert(key, Rc::clone(adt));
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
        self.analyze_context.borrow_mut().file_pos =
            ast_token.file_pos().cloned().unwrap_or_default();
    }

    fn visit_struct(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(struct_), _, id, ..) = &mut ast_token {
            self.decl_new_adt(struct_, *id);
        }
    }

    fn visit_enum(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Enum(enum_), _, id, ..) = &mut ast_token {
            self.decl_new_adt(enum_, *id);
        }
    }

    fn visit_union(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Union(union), _, id, ..) = &mut ast_token {
            self.decl_new_adt(union, *id);
        }
    }

    // TODO: Merge logic with ADTs.
    fn visit_trait(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Trait(trait_), _, trait_id, ..) = &mut ast_token {
            let mut analyze_context = self.analyze_context.borrow_mut();

            // The trait will be added in the scope of its parent, so fetch
            // the block id for the parent.
            let parent_id = match analyze_context.get_parent_id(*trait_id) {
                Ok(parent_id) => parent_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            if let Ok(prev_trait) = analyze_context.get_trait(&trait_.borrow().name, *trait_id) {
                let err = analyze_context.err(format!(
                    "A trait with name \"{}\" already defined.",
                    prev_trait.borrow().name
                ));
                self.errors.push(err);
            } else {
                // Add the trait into decl lookup maps.
                let key = (trait_.borrow().name.clone(), parent_id);
                analyze_context.traits.insert(key, Rc::clone(trait_));
            }
        }
    }
}
