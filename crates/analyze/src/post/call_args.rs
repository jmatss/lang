use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use parking_lot::RwLock;

use common::{
    ctx::ast_ctx::AstCtx,
    error::LangError,
    error::LangResult,
    token::expr::Argument,
    token::expr::FnCall,
    token::{
        block::AdtKind,
        expr::{AdtInit, Var},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::ty::Ty,
};

/// Iterates through all function and method calls and re-orders all named
/// arguments so that they are put in the correct index position so that
/// the codegen works correctly. It also assigns any default values found in the
/// function parameters to the current function call (if expected).
pub struct CallArgs {
    errors: Vec<LangError>,
}

impl CallArgs {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    /// If any of the given `args` are named arguments, check so that every name
    /// is unique and is found only ones in the given argument list.
    /// Reports errors into `self.errors` if duplicates are found and returns
    /// false. Returns true if no duplicates are found.
    fn names_are_unique(&mut self, ast_ctx: &AstCtx, args: &[Argument]) -> bool {
        let mut name_to_arg: HashMap<_, Vec<&Argument>> = HashMap::new();
        let mut names_are_unique = true;

        for arg in args.iter().filter(|arg| arg.name.is_some()) {
            match name_to_arg.entry(arg.name.clone().unwrap()) {
                Entry::Occupied(mut o) => {
                    names_are_unique = false;
                    o.get_mut().push(arg);
                }
                Entry::Vacant(v) => {
                    v.insert(vec![arg]);
                }
            }
        }

        // Create a "nicely" formatted error message if a duplicate is found.
        if !names_are_unique {
            for (arg_name, dup_args) in name_to_arg.iter() {
                if dup_args.len() > 1 {
                    let mut err_pos_msg = String::new();

                    for (i, arg) in dup_args.iter().enumerate() {
                        err_pos_msg.push_str(&format!(
                            "\nuse {} at position:\n{:#?}",
                            i + 1,
                            arg.name_file_pos
                        ));
                    }

                    let err = ast_ctx.err(format!(
                        "Found multiple arguments with the name \"{}\" in argument list:{}",
                        arg_name, &err_pos_msg
                    ));
                    self.errors.push(err);
                }
            }
        }

        names_are_unique
    }

    fn reorder(&mut self, ast_ctx: &AstCtx, args: &mut Vec<Argument>, params: &[Arc<RwLock<Var>>]) {
        if params.is_empty() || !self.names_are_unique(ast_ctx, args) {
            return;
        }

        let mut param_name_to_idx = HashMap::new();
        for (idx, param) in params.iter().enumerate() {
            param_name_to_idx.insert(param.read().name.clone(), idx);
        }

        let mut arg_idx = 0;
        while arg_idx < args.len() {
            let arg = args.get(arg_idx).expect("Known to be in bounds.");

            let arg_name = if let Some(arg_name) = &arg.name {
                arg_name
            } else {
                arg_idx += 1;
                continue;
            };

            if let Some(param_idx) = param_name_to_idx.get(arg_name) {
                if arg_idx != *param_idx {
                    // The `arg_idx` is not increment if a swap was made. This is
                    // done since then you wan't to see if the new argument at this
                    // `arg_idx` needs to be swapped to somewhere else.
                    args.swap(arg_idx, *param_idx);
                    continue;
                }
            } else {
                let err = ast_ctx.err(format!(
                    "Unable to find parameter with name \"{}\" given in argument list at: {:#?}",
                    arg_name,
                    arg.value.file_pos()
                ));
                self.errors.push(err);
            }

            arg_idx += 1;
        }
    }

    fn default_args(
        &self,
        ast_ctx: &AstCtx,
        fn_call: &mut FnCall,
        params: &[Arc<RwLock<Var>>],
        is_variadic: bool,
    ) -> LangResult<()> {
        // If here are more parameters in the function that there are arguments
        // to the current function call, assume that "missing" arguments
        // are supposed to be filled in with the default parameters. If there are
        // no default value set, report that the function call contains to few
        // arguments.
        if !is_variadic && fn_call.arguments.len() < params.len() {
            let start_idx = fn_call.arguments.len();
            for param in params[start_idx..].iter() {
                let param = param.read();

                if let Some(default_value) = &param.value {
                    let default_arg =
                        Argument::new(Some(param.name.clone()), None, *default_value.clone());
                    fn_call.arguments.push(default_arg);
                } else {
                    return Err(ast_ctx.err(format!(
                        "Function call to \"{}\" missing argument for parameter \"{}\".",
                        &fn_call.name, &param.name
                    )));
                }
            }
        }

        Ok(())
    }
}

impl Visitor for CallArgs {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        // Function calls on variables containing fn pointers does currently not
        // support named arguments (since the names of the parameters isn't know
        // in those cases).
        if fn_call.is_fn_ptr_call {
            return;
        }

        // If this is a function contained in a ADT/trait (method), one needs to
        // make sure to fetch it as a method since they are stored differently
        // compared to a regular function.
        let func_res = {
            let ty_env_guard = ctx.ty_env.lock();

            if let Some(adt_type_id) = &fn_call.method_adt {
                let adt_ty = match ty_env_guard.ty(*adt_type_id) {
                    Ok(adt_ty) => adt_ty.clone(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let inner_ty = if let Ty::CompoundType(inner_ty, ..) = adt_ty {
                    inner_ty
                } else {
                    self.errors.push(ctx.ast_ctx.err(format!(
                        "method_adt not valid type for func call: {:#?}",
                        fn_call
                    )));
                    return;
                };

                let adt_path = if let Some(adt_path) = inner_ty.get_ident() {
                    adt_path
                } else {
                    self.errors.push(ctx.ast_ctx.err(format!(
                        "Bad inner type for func call method_adt: {:#?}",
                        fn_call
                    )));
                    return;
                };

                ctx.ast_ctx
                    .get_method(&ty_env_guard, &adt_path, &fn_call.half_name(&ty_env_guard))
            } else {
                let partial_path = fn_call.module.clone_push(
                    &fn_call.name,
                    fn_call.generics.as_ref(),
                    fn_call.file_pos,
                );

                let full_path = match ctx.ast_ctx.calculate_fn_full_path(
                    &ty_env_guard,
                    &partial_path,
                    ctx.block_id,
                ) {
                    Ok(full_path) => full_path,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                ctx.ast_ctx.get_fn(&ty_env_guard, &full_path)
            }
        };

        let func = match func_res {
            Ok(func) => func,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let func = func.read();

        // Get the parameters for the function. This will be used to reorder
        // the arguments in the function call correctly.
        let params = if let Some(params) = &func.parameters {
            params
        } else {
            // Early return if there are no parameters for the function, there
            // is nothing to do here.
            return;
        };

        // Reorder the arguments of the function call according to the parameter
        // names used for the arguments.
        self.reorder(ctx.ast_ctx, &mut fn_call.arguments, params);

        // Assign any default value for arguments that are missing a value in
        // the function call.
        if let Err(err) = self.default_args(ctx.ast_ctx, fn_call, params, func.is_var_arg) {
            self.errors.push(err);
            return;
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        match adt_init.kind {
            AdtKind::Struct => (),
            AdtKind::Union => return,
            AdtKind::Enum | AdtKind::Unknown => unreachable!("{:#?}", adt_init.kind),
        }

        let adt_path = adt_init.module.clone_push(
            &adt_init.name,
            adt_init.generics.as_ref(),
            adt_init.file_pos,
        );

        let adt = match ctx.ast_ctx.get_adt(&ctx.ty_env.lock(), &adt_path) {
            Ok(adt) => adt,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        self.reorder(ctx.ast_ctx, &mut adt_init.arguments, &adt.read().members);

        // TODO: Should there be default values for structs (?). Arrange that
        //       here in that case.
    }
}
