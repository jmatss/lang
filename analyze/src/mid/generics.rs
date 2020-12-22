use common::{
    token::{ast::AstToken, block::BlockHeader},
    traverser::TraverseContext,
    visitor::Visitor,
};

/// Iterates through "generic" parameters tied to structures and functions (TODO)
/// and replaces the uses of the generics with "Generic" types instead if the
/// parsed "UnknownIdent". This is done so that generics can be handled differently
/// during the type inference stage.
pub struct GenericsAnalyzer {}

impl GenericsAnalyzer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Visitor for GenericsAnalyzer {
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
}
