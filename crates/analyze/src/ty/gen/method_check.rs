use common::{
    error::LangError,
    token::expr::FnCall,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        contains::contains_generic_shallow,
        get::get_ident,
        to_string::{to_string_path, to_string_type_id},
    },
};

/// Checks so that all instance method calls calling functions with generics
/// actually specifies generics at the call site. This is needed because
/// inferring the generics currently aren't supported for generics on instance
/// methods (is possible with static methods and free-standing functions).
/// This will need to be changed in the future.
pub struct FnGenericsCheck {
    errors: Vec<LangError>,
}

impl FnGenericsCheck {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for FnGenericsCheck {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// If this is a method call, makes sure that the amount of generics specified
    /// at the method call is the same amount as declared on the actual method.
    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        // This is done for function pointers at a earlier stage(during type inference).
        if fn_call.is_fn_ptr_call {
            return;
        }

        if let Some(adt_type_id) = &fn_call.method_adt {
            let ty_env_guard = ctx.ty_env.lock().unwrap();

            if contains_generic_shallow(&ty_env_guard, *adt_type_id).unwrap_or(false) {
                return;
            }

            let adt_path = match get_ident(&ty_env_guard, *adt_type_id) {
                Ok(Some(adt_path)) => adt_path,
                Ok(None) => {
                    let err = ctx.ast_ctx.err(format!(
                        "Unable to get path for ADT with type ID: {}, ty: {:?}",
                        adt_type_id,
                        to_string_type_id(&ty_env_guard, *adt_type_id),
                    ));
                    self.errors.push(err);
                    return;
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let method =
                match ctx
                    .ast_ctx
                    .get_method(&ty_env_guard, &adt_path.without_gens(), &fn_call.name)
                {
                    Ok(method) => method,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
            let method = method.as_ref().read().unwrap();

            if let Some(generic_impls) = &method.generics {
                if generic_impls.is_empty() {
                    return;
                }

                if let Some(fn_call_generics) = &fn_call.generics {
                    if generic_impls.len() != fn_call_generics.len_types() {
                        let err = ctx.ast_ctx.err(format!(
                            "Function declaration and function call generic count differs. \
                            Function declaration amount: {}, function call amount: {}. \
                            ADT: {}, method name: {}, file_pos: {:#?}",
                            generic_impls.len(),
                            fn_call_generics.len_types(),
                            to_string_path(&ty_env_guard, &adt_path),
                            fn_call.name,
                            fn_call.file_pos
                        ));
                        self.errors.push(err);
                        return;
                    }
                } else {
                    // TODO: Better error message.
                    let err = ctx.ast_ctx.err(format!(
                        "Method declaration specifies generics, but no generics were given when \
                        calling the method. ADT: {}, method name: {}, file_pos: {:#?}",
                        to_string_path(&ty_env_guard, &adt_path),
                        fn_call.name,
                        fn_call.file_pos
                    ));
                    self.errors.push(err);
                    return;
                }
            }
        }
    }
}
