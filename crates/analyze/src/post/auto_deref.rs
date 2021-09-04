use common::{
    error::{LangError, LangResult},
    token::{
        expr::{Expr, FnCall},
        op::{Op, UnOp, UnOperator},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        get::{get_ident, get_type_info},
        is::{is_adt, is_pointer, is_primitive},
        to_string::{to_string_path, to_string_type_id},
        ty::Ty,
    },
};

// TODO: Make sure that only one of the `this` modifiers are defined on the
//       function. Currently multiple modifiers can be specified without a error.

/// Auto derefs ADT instances during access of members or methods.
///
/// If a member access is done on a ADT reference, it will be dereferenced.
/// Also makes sure that all method calls are using the correct form of `this`.
/// The `this` can either be dereferenced or "addressed" depending on the form
/// that is expected in the function declaration.
pub struct AutoDerefAnalyzer {
    errors: Vec<LangError>,
}

impl AutoDerefAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn auto_deref_method(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) -> LangResult<()> {
        let mut ty_env_guard = ctx.ty_env.lock();
        let fn_name_with_gens = fn_call.half_name(&ty_env_guard);

        let first_arg = if let Some(first_arg) = fn_call.arguments.first_mut() {
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
        let adt_path = get_ident(&ty_env_guard, adt_type_id)?.unwrap();

        let method = ctx
            .ast_ctx
            .get_method(&ty_env_guard, &adt_path, &fn_name_with_gens)?;
        let method = method.read();

        if method.is_this_by_ref() && !is_pointer(&ty_env_guard, arg_type_id)? {
            let mut un_op = UnOp::new(
                UnOperator::Address,
                Box::new(first_arg.value.clone()),
                first_arg.value.file_pos().cloned(),
            );
            let type_info = get_type_info(&ty_env_guard, adt_type_id).unwrap().clone();
            un_op.ret_type = Some(ty_env_guard.id(&Ty::Pointer(adt_type_id, type_info))?);

            first_arg.value = Expr::Op(Op::UnOp(un_op));
        } else if method.is_this_by_val()
            && !(is_adt(&ty_env_guard, arg_type_id)? || is_primitive(&ty_env_guard, arg_type_id)?)
        {
            if is_pointer(&ty_env_guard, arg_type_id)? {
                let mut un_op = UnOp::new(
                    UnOperator::Deref,
                    Box::new(first_arg.value.clone()),
                    first_arg.value.file_pos().cloned(),
                );
                un_op.ret_type = Some(adt_type_id);

                first_arg.value = Expr::Op(Op::UnOp(un_op));
            } else {
                return Err(ctx.ast_ctx.err(format!(
                    "Method with name \"{}\" on ADT \"{}\" expected to take `this` by value. \
                    `this` has unexpected type \"{}\" in method call at position:\n{:#?}",
                    method.name,
                    to_string_path(&ty_env_guard, &adt_path),
                    to_string_type_id(&ty_env_guard, arg_type_id)?,
                    first_arg.value.file_pos()
                )));
            }
        }

        Ok(())
    }
}

impl Visitor for AutoDerefAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if fn_call.is_method {
            if let Err(err) = self.auto_deref_method(fn_call, ctx) {
                self.errors.push(err);
                return;
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &mut TraverseCtx) {
        if let UnOperator::AdtAccess(..) = &un_op.operator {
            let ty_env_guard = ctx.ty_env.lock();

            let type_id = match un_op.value.get_expr_type() {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let inf_type_id = match ty_env_guard.inferred_type(type_id) {
                Ok(inf_type_id) => inf_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            match is_pointer(&ty_env_guard, inf_type_id) {
                Ok(true) => (),
                Ok(false) => return,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }

            let nested_type_id = match ty_env_guard.ty_clone(inf_type_id) {
                Ok(Ty::Pointer(nested_type_id, ..)) => nested_type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
                _ => unreachable!(),
            };

            let mut new_un_op = UnOp::new(
                UnOperator::Deref,
                un_op.value.clone(),
                un_op.value.file_pos().cloned(),
            );
            new_un_op.ret_type = Some(nested_type_id);

            un_op.value = Box::new(Expr::Op(Op::UnOp(new_un_op)));
        }
    }
}
