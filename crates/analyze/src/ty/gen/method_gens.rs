use common::{
    error::{LangError, LangResult},
    token::expr::FnCall,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{generics::Generics, ty::Ty},
};

/// Iterates through all method calls in the AST and collects/replaces any
/// inferred generics for method calls that haven't specified any generics at
/// the call site.
///
/// During the solving of types, the `UnknownAdtMethod` will have been solved
/// and will contain any inferred generics for methods. In this analyzer we
/// iterate through all method calls and gets the generics from these
/// `UnknownAdtMethod` types and replaces the generics in the actual fn call.
pub struct MethodGensInferencer {
    errors: Vec<LangError>,
}

impl MethodGensInferencer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn get_inferred_gens(method_call: &FnCall, ctx: &TraverseCtx) -> LangResult<Option<Generics>> {
        let ret_type_id = if let Some(ret_type_id) = method_call.ret_type {
            ret_type_id
        } else {
            return Err(ctx.ast_ctx.err(format!(
                "Expected function call to have return type set: {:#?}",
                method_call
            )));
        };

        let ty_env_guard = ctx.ty_env.lock();
        Ok(
            if let Ty::UnknownAdtMethod(_, path_with_gens, ..) = ty_env_guard.ty(ret_type_id)? {
                match path_with_gens.gens() {
                    Some(gens) if !gens.is_empty_types() => Some(gens.clone()),
                    _ => None,
                }
            } else {
                None
            },
        )
    }
}

impl Visitor for MethodGensInferencer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if fn_call.method_adt.is_some() && fn_call.generics.is_none() {
            match Self::get_inferred_gens(fn_call, ctx) {
                Ok(new_gens) => fn_call.generics = new_gens,
                Err(err) => self.errors.push(err),
            }
        }
    }
}
