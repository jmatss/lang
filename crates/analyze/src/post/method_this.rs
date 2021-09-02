use common::{
    error::{LangError, LangResult},
    token::expr::FnCall,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        get::get_ident,
        is::{is_adt, is_pointer, is_primitive},
        to_string::{to_string_path, to_string_type_id},
    },
};

// TODO: Make sure that only one of the `this` modifiers are defined on the
//       function. Currently multiple modifiers can be specified without a error.

/// Makes sure that all method calls are using the correct form of `this`.
/// A method can take `this` either by value (`this`) or by reference (`{this}`).
/// This analyzer ensures that the correct form of `this` is used so that the
/// form on the method declaration and the call site is the same.
pub struct MethodThisAnalyzer {
    errors: Vec<LangError>,
}

impl MethodThisAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn verify_this_modifiers(&mut self, fn_call: &FnCall, ctx: &mut TraverseCtx) -> LangResult<()> {
        let first_arg = if let Some(first_arg) = fn_call.arguments.first() {
            first_arg
        } else {
            return Ok(());
        };

        let arg_type_id = if first_arg
            .name
            .as_ref()
            .map(|name| name == "this")
            .unwrap_or(false)
        {
            first_arg.value.get_expr_type()?
        } else {
            return Ok(());
        };

        let ty_env_guard = ctx.ty_env.lock().unwrap();

        let adt_type_id = fn_call.method_adt.unwrap();
        let adt_path = get_ident(&ty_env_guard, adt_type_id)?.unwrap();

        let method =
            ctx.ast_ctx
                .get_method(&ty_env_guard, &adt_path, &fn_call.half_name(&ty_env_guard))?;
        let method = method.read().unwrap();

        if method.is_this_by_ref() && !is_pointer(&ty_env_guard, arg_type_id)? {
            Err(ctx.ast_ctx.err(format!(
                "Method with name \"{}\" on ADT \"{}\" expected to take `this` by reference. \
                `this` has unexpected type \"{}\" in method call at position:\n{:#?}",
                method.name,
                to_string_path(&ty_env_guard, &adt_path),
                to_string_type_id(&ty_env_guard, arg_type_id)?,
                first_arg.value.file_pos()
            )))
        } else if method.is_this_by_val()
            && !(is_adt(&ty_env_guard, arg_type_id)? || is_primitive(&ty_env_guard, arg_type_id)?)
        {
            Err(ctx.ast_ctx.err(format!(
                "Method with name \"{}\" on ADT \"{}\" expected to take `this` by value. \
                `this` has unexpected type \"{}\" in method call at position:\n{:#?}",
                method.name,
                to_string_path(&ty_env_guard, &adt_path),
                to_string_type_id(&ty_env_guard, arg_type_id)?,
                first_arg.value.file_pos()
            )))
        } else {
            Ok(())
        }
    }
}

impl Visitor for MethodThisAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if !fn_call.is_method {
            return;
        }

        if let Err(err) = self.verify_this_modifiers(fn_call, ctx) {
            self.errors.push(err);
            return;
        }
    }
}
