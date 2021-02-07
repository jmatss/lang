use crate::AnalyzeContext;
use common::{error::LangError, token::expr::FnCall, traverser::TraverseContext, visitor::Visitor};

/// Checks so that all function calls calling functions with generics actually
/// specifies generics at the call site. This is needed because infering the
/// generics currently aren't supported for generics on functions, only on
/// generics for structs. This will need to be changed in the future.
pub struct FnGenericsCheck<'a> {
    analyze_context: &'a AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> FnGenericsCheck<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for FnGenericsCheck<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// If this is a method call, makes sure that the amount of generics specified
    /// at the method call is the same amount as declared on the actual method.
    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &TraverseContext) {
        // This is done for function pointers at a earlier stage(during type inference).
        if fn_call.is_fn_ptr_call {
            return;
        }

        if let Some(structure_ty) = &fn_call.method_adt {
            if structure_ty.is_generic() {
                return;
            }

            let structure_name = structure_ty.get_ident().unwrap();

            let method = match self.analyze_context.get_adt_method(
                &structure_name,
                &fn_call.name,
                ctx.block_id,
            ) {
                Ok(method) => method,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let method = method.borrow();

            if let Some(generic_impls) = &method.generic_impls {
                if generic_impls.is_empty() {
                    return;
                }

                if let Some(fn_call_generics) = &fn_call.generics {
                    if generic_impls.len() != fn_call_generics.len_types() {
                        let err = self.analyze_context.err(format!(
                            "Func decl and func call generic count differ. \
                            Func decl #: {}, func call #: {}. Fn_call: {:#?}",
                            generic_impls.len(),
                            fn_call_generics.len_types(),
                            fn_call
                        ));
                        self.errors.push(err);
                        return;
                    }
                } else {
                    // TODO: Better error message.
                    let err = self.analyze_context.err(format!(
                        "Func decl contains generics, generics not specified for func call: {:#?}",
                        fn_call
                    ));
                    self.errors.push(err);
                    return;
                }
            }
        }
    }
}
