use common::{
    ctx::traverse_ctx::TraverseCtx,
    error::{LangError, LangResult},
    token::expr::FnCall,
    traverse::visitor::Visitor,
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

        let adt_type_id = fn_call.method_adt.unwrap();
        let adt_path = ctx.ty_ctx.ty_env.get_ident(adt_type_id)?.unwrap();

        let method = ctx
            .ast_ctx
            .get_method(&ctx.ty_ctx, &adt_path, &fn_call.name)?;
        let method = method.borrow();

        if method.is_this_by_ref() && !ctx.ty_ctx.ty_env.is_pointer(arg_type_id)? {
            Err(ctx.ast_ctx.err(format!(
                "Method with name \"{}\" on ADT \"{}\" expected to take `this` by reference. \
                `this` has unexpected type \"{}\" in method call at position:\n{:#?}",
                method.name,
                ctx.ty_ctx.to_string_path(&adt_path),
                ctx.ty_ctx.to_string_type_id(arg_type_id)?,
                first_arg.value.file_pos()
            )))
        } else if method.is_this_by_val()
            && ctx
                .ty_ctx
                .ty_env
                .get_ident(arg_type_id)
                .ok()
                .flatten()
                .is_none()
        {
            Err(ctx.ast_ctx.err(format!(
                "Method with name \"{}\" on ADT \"{}\" expected to take `this` by value. \
                `this` has unexpected type \"{}\" in method call at position:\n{:#?}",
                method.name,
                ctx.ty_ctx.to_string_path(&adt_path),
                ctx.ty_ctx.to_string_type_id(arg_type_id)?,
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
