use crate::{block::BlockInfo, AnalyzeContext};
use common::{
    error::LangError,
    path::LangPathPart,
    token::{ast::AstToken, block::BlockHeader},
    traverser::{AstTraverser, TraverseContext},
    ty::{environment::TypeEnvironment, generics::Generics},
    visitor::Visitor,
    TypeId,
};
use std::cell::RefCell;

/// Iterates through "generic" parameters tied to ADTs and functions, and replaces
/// the uses of the generics with "Generic" types instead of the parsed
/// "UnknownIdent". This is done so that generics can be handled differently
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

    /// "Rewrites" the types of the generic member types to "Generic"s for
    /// the structure members.
    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(struct_), ..) = &ast_token {
            let struct_ = struct_.borrow();
            if let (Some(generics), members) = (&struct_.generics, &struct_.members) {
                for member in members {
                    if let Some(type_id) = member.borrow_mut().ty.as_mut() {
                        if let Err(err) = self
                            .analyze_context
                            .borrow_mut()
                            .ty_env
                            .replace_generics(*type_id, generics)
                        {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    fn visit_union(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseContext) {
        if let AstToken::Block(BlockHeader::Union(union), ..) = &ast_token {
            let union = union.borrow();
            if let (Some(generics), members) = (&union.generics, &union.members) {
                for member in members {
                    if let Some(type_id) = member.borrow_mut().ty.as_mut() {
                        if let Err(err) = self
                            .analyze_context
                            .borrow_mut()
                            .ty_env
                            .replace_generics(*type_id, generics)
                        {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    /// "Rewrites" generics parsed as "UnknownIdent"s to "Generic"s by matching
    /// the identifiers with known names for the generics defined on the structure.
    /// This will be done for both the method headers and everything in their bodies.
    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(impl_path, _), .., body) = ast_token {
            let analyze_context = self.analyze_context.borrow();

            let full_impl_path = match analyze_context.get_module(ctx.block_id) {
                Ok(Some(mut full_impl_path)) => {
                    let impl_ident = impl_path.last().unwrap().0.clone();
                    full_impl_path.push(LangPathPart(impl_ident, None));
                    full_impl_path
                }
                Ok(None) => {
                    let mut full_impl_path = impl_path.clone();
                    let last_part = full_impl_path.pop().unwrap();
                    full_impl_path.push(LangPathPart(last_part.0, None));
                    full_impl_path
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // TODO: Will structures always be defined in the default block?
            let block_id = BlockInfo::DEFAULT_BLOCK_ID;

            // TODO: Implement generics for iterfaces and enums (?).

            let adt_generics = if let Ok(adt) = analyze_context.get_adt(&full_impl_path, block_id) {
                adt.borrow().generics.clone().unwrap_or_default()
            } else if analyze_context.get_trait(&full_impl_path, block_id).is_ok() {
                Generics::empty()
            } else {
                let err = analyze_context.err(format!(
                    "Unable to find ADT/Trait for impl block with name \"{}\" in block {}",
                    full_impl_path, block_id
                ));
                self.errors.push(err);
                return;
            };

            // Iterate through the body of one method at a time and replace all
            // "UnknownIdent"s representing generics to "Generic"s.
            for method in body {
                let func_generics = if let AstToken::Block(BlockHeader::Fn(func), ..) = method {
                    func.borrow().generics.clone()
                } else {
                    None
                };

                // No generics declared on either ADT/Trait or function, early skip.
                if adt_generics.is_empty() && func_generics.is_none() {
                    continue;
                }

                // Replaces any generics declared on the function.
                if let Some(func_generics) = func_generics {
                    let ty_env = &mut self.analyze_context.borrow_mut().ty_env;
                    let mut func_replacer = FuncGenericsReplacer::new(ty_env, &func_generics);

                    if let Err(mut err) = AstTraverser::new()
                        .add_visitor(&mut func_replacer)
                        .traverse_token(method)
                        .take_errors()
                    {
                        self.errors.append(&mut err);
                        return;
                    }
                }

                // Replaces any generics declared on the ADT/Trait.
                if !adt_generics.is_empty() {
                    let ty_env = &mut self.analyze_context.borrow_mut().ty_env;
                    let mut adt_replacer = FuncGenericsReplacer::new(ty_env, &adt_generics);

                    if let Err(mut err) = AstTraverser::new()
                        .add_visitor(&mut adt_replacer)
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
    ty_env: &'a mut TypeEnvironment,
    adt_generics: &'a Generics,
    errors: Vec<LangError>,
}

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
impl<'a> FuncGenericsReplacer<'a> {
    pub fn new(ty_env: &'a mut TypeEnvironment, adt_generics: &'a Generics) -> Self {
        Self {
            ty_env,
            adt_generics,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for FuncGenericsReplacer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, _ctx: &mut TraverseContext) {
        if let Err(err) = self.ty_env.replace_generics(*type_id, self.adt_generics) {
            self.errors.push(err);
        }
    }
}
