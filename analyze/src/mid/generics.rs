use crate::{block::BlockInfo, AnalyzeContext};
use common::{
    error::LangError,
    token::{ast::AstToken, block::BlockHeader},
    traverser::{AstTraverser, TraverseContext},
    ty::ty::Ty,
    visitor::Visitor,
};
use std::cell::RefCell;

/// Iterates through "generic" parameters tied to structures and functions (TODO)
/// and replaces the uses of the generics with "Generic" types instead if the
/// parsed "UnknownIdent". This is done so that generics can be handled differently
/// during the type inference stage.
pub struct GenericsAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,

    errors: Vec<LangError>,
}

impl<'a> GenericsAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for GenericsAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    // TODO: Implement for interfaces & enums.
    /// "Rewrites" the types of the generic member types to "Generic"s for
    /// the structure members.
    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(struct_), ..) = &ast_token {
            let struct_ = struct_.borrow();
            if let (Some(generics), Some(members)) = (&struct_.generics, &struct_.members) {
                for member in members {
                    if let Some(ty) = member.borrow_mut().ty.as_mut() {
                        ty.replace_generics(&generics)
                    }
                }
            }
        }
    }

    /// "Rewrites" generics parsed as "UnknownIdent"s to "Generic"s by matching
    /// the identifiers with known names for the generics defined on the structure.
    /// This will be done for both the method headers and everything in their bodies.
    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(ident, _), .., body) = ast_token {
            let analyze_context = self.analyze_context.borrow();

            // TODO: Will structures always be defined in the default block?
            let block_id = BlockInfo::DEFAULT_BLOCK_ID;

            // TODO: Implement generics for iterfaces and enums (?).

            let structure_generic_names =
                if let Ok(struct_) = analyze_context.get_struct(ident, block_id) {
                    struct_
                        .borrow()
                        .generics
                        .clone()
                        .unwrap_or_else(Vec::default)
                } else if analyze_context.get_enum(ident, block_id).is_ok()
                    || analyze_context.get_trait(ident, block_id).is_ok()
                {
                    Vec::default()
                } else {
                    let err = analyze_context.err(format!(
                        "Unable to find structure for impl block with name \"{}\" in block {}",
                        ident, block_id
                    ));
                    self.errors.push(err);
                    return;
                };

            // Iterate through the body of one method at a time and replace all
            // "UnknownIdent"s representing generics to "Generic"s.
            for method in body {
                let func_generic_names =
                    if let AstToken::Block(BlockHeader::Function(func), ..) = method {
                        func.borrow().generic_names.clone()
                    } else {
                        None
                    };

                // No generics declared on either sturcture or function, early skip.
                if structure_generic_names.is_empty() && func_generic_names.is_none() {
                    continue;
                }

                // Replaces any generics declared on the function.
                if let Some(func_generic_names) = func_generic_names {
                    let mut func_replacer = FuncGenericsReplacer::new(&func_generic_names);
                    if let Err(mut err) = AstTraverser::new()
                        .add_visitor(&mut func_replacer)
                        .traverse_token(method)
                        .take_errors()
                    {
                        self.errors.append(&mut err);
                        return;
                    }
                }

                // Replaces any generics declared on the structure.
                if !structure_generic_names.is_empty() {
                    let mut struct_replacer = FuncGenericsReplacer::new(&structure_generic_names);
                    if let Err(mut err) = AstTraverser::new()
                        .add_visitor(&mut struct_replacer)
                        .traverse_token(method)
                        .take_errors()
                    {
                        self.errors.append(&mut err);
                        return;
                    }
                }
            }
        }
    }
}

struct FuncGenericsReplacer<'a> {
    generic_names: &'a [String],
}

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
impl<'a> FuncGenericsReplacer<'a> {
    pub fn new(generic_names: &'a [String]) -> Self {
        Self { generic_names }
    }
}

impl<'a> Visitor for FuncGenericsReplacer<'a> {
    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        ty.replace_generics(self.generic_names);
    }
}
