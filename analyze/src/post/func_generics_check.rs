use crate::AnalyzeContext;
use common::{
    error::LangError, token::expr::FuncCall, traverser::TraverseContext, visitor::Visitor,
};

/// Checks so that all function calls calling functions with generics actually
/// specifies generics at the call site. This is needed because infering the
/// generics currently aren't supported for generics on functions, only on
/// generics for structs. This will need to be changed in the future.
pub struct FuncGenericsCheck<'a> {
    analyze_context: &'a AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> FuncGenericsCheck<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for FuncGenericsCheck<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// If this is a method call, makes sure that the amount of generics specified
    /// at the method call is the same amount as declared on the actual method.
    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        if let Some(structure_ty) = &func_call.method_structure {
            if structure_ty.is_generic() {
                return;
            }

            let structure_name = structure_ty.get_ident().unwrap();

            let method = match self.analyze_context.get_method(
                &structure_name,
                &func_call.name,
                ctx.block_id,
            ) {
                Ok(method) => method,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let method = method.borrow();

            if let Some(method_generics) = &method.generics {
                if method_generics.is_empty() {
                    return;
                }

                if let Some(func_call_generics) = &func_call.generics {
                    if method_generics.len() != func_call_generics.len_types() {
                        let err = self.analyze_context.err(format!(
                            "Func decl and func call generic count differ. \
                            Func decl #: {}, func call #: {}. Func_call: {:#?}",
                            method_generics.len(),
                            func_call_generics.len_types(),
                            func_call
                        ));
                        self.errors.push(err);
                        return;
                    }
                } else {
                    // TODO: Better error message.
                    let err = self.analyze_context.err(format!(
                        "Func decl contains generics, generics not specified for func call: {:#?}",
                        func_call
                    ));
                    self.errors.push(err);
                    return;
                }
            }
        }
    }
}
