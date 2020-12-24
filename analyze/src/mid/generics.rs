use std::cell::RefCell;

use common::{
    error::LangError,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{BuiltInCall, Expr, StructInit},
        op::{UnOp, UnOperator},
    },
    traverser::{AstTraverser, TraverseContext},
    visitor::Visitor,
};

use crate::{AnalyzeContext, BlockInfo};

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
    /// "Rewrites" the types of the generic member types to "Generic"s. This
    /// includes the structure members, impl method parameters and the methods
    /// return types.
    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(struct_), ..) = &ast_token {
            let struct_ = struct_.borrow();

            if let Some(generics) = &struct_.generic_params {
                // Rewrite the generics for the members.
                if let Some(members) = &struct_.members {
                    for member in members {
                        if let Some(ty) = member.borrow_mut().ty.as_mut() {
                            ty.replace_generics(&generics)
                        }
                    }
                }

                // TODO: Rewrite the generics used inside the method bodies as well.

                // Rewrite the generics for the method parameters and return types.
                if let Some(methods) = &struct_.methods {
                    for method in methods.values() {
                        // Rewrite the generics for the parameters.
                        if let Some(params) = &method.borrow().parameters {
                            for param in params {
                                if let Some(ty) = param.borrow_mut().ty.as_mut() {
                                    ty.replace_generics(&generics);
                                }
                            }
                        }

                        // Rewrite the generics for the return type.
                        if let Some(ret_ty) = &mut method.borrow_mut().ret_type {
                            ret_ty.replace_generics(&generics);
                        }
                    }
                }
            }
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(ident), _, body) = ast_token {
            let analyze_context = self.analyze_context.borrow();

            // TODO: Will structures always be defined in the default block?
            let block_id = BlockInfo::DEFAULT_BLOCK_ID;

            // TODO: Implement generics for iterfaces and enums (?).

            let generic_names = if let Ok(struct_) = analyze_context.get_struct(ident, block_id) {
                struct_
                    .borrow()
                    .generic_params
                    .clone()
                    .unwrap_or_else(Vec::default)
            } else if let Ok(enum_) = analyze_context.get_enum(ident, block_id) {
                Vec::default()
            } else if let Ok(interface) = analyze_context.get_interface(ident, block_id) {
                Vec::default()
            } else {
                let err = analyze_context.err(format!(
                    "Unable to find structure for impl block with name \"{}\" in block {}",
                    ident, block_id
                ));
                self.errors.push(err);
                return;
            };

            // Early return if the structure doesn't have any generics, nothing
            // to replace in this function.
            if generic_names.is_empty() {
                return;
            }

            // Iterate through the body of one method at a time and replace all
            // "UnknownIdent"s representing generics to "Generic"s.
            for method in body {
                let mut func_replacer = FuncGenericsReplacer::new(&generic_names);
                if let Err(mut err) = AstTraverser::new()
                    .add_visitor(&mut func_replacer)
                    .traverse(method)
                    .take_errors()
                {
                    self.errors.append(&mut err);
                    return;
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
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Ok(ty) = expr.get_expr_type_mut() {
            ty.replace_generics(self.generic_names);
        }
    }

    // TODO: Need to replace some types that isn't expression. Should there be
    //       a `visit_type()` or something similar in the `Visitor` trait that
    //       lets one handle all cases in a single function?
    //       Would probably be best to implement, then it could be used for
    //       multiple Analyze structs.

    fn visit_un_op(&mut self, un_op: &mut UnOp, _ctx: &TraverseContext) {
        // Edge case for struct access. Need to replace thev operators type as well.
        if let UnOperator::StructAccess(.., Some(ty)) = &mut un_op.operator {
            ty.replace_generics(self.generic_names)
        }
    }

    fn visit_struct_init(&mut self, struct_init: &mut StructInit, _ctx: &TraverseContext) {
        if let Some(generics) = &mut struct_init.generics {
            for ty in generics.iter_types_mut() {
                ty.replace_generics(self.generic_names)
            }
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, _ctx: &TraverseContext) {
        if let Some(generics) = &mut built_in_call.generics {
            for ty in generics.iter_types_mut() {
                ty.replace_generics(self.generic_names)
            }
        }
    }
}
