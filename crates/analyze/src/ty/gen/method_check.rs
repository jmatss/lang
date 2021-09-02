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

/// Checks so that calls to method instances have the correct amount of generics
/// specified at the call site.
/// This is needed because inferring the generics for instance methods are done
/// at a later stage than the regular type inferrence. We therefore have to make
/// sure that the amount of the generics are correct after the solving of the
/// methods are solved.
///
/// For static method calls and free-standing functions, the amount of generics
/// are checked/enforced at the start of the type inference, so we don't need
/// to check them here.
pub struct MethodGensCheck {
    errors: Vec<LangError>,
}

impl MethodGensCheck {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for MethodGensCheck {
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
                            "Method declaration and method call generic count differs. \
                            Method declaration amount: {}, method call amount: {}. \
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
