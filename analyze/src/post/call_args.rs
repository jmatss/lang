use crate::AnalyzeContext;
use common::{
    error::LangError,
    error::LangResult,
    token::expr::Argument,
    token::expr::FnCall,
    token::{
        block::AdtKind,
        expr::{AdtInit, Var},
    },
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

/// Iterates through all function and method calls and re-orders all named
/// arguments so that they are put in the correct index position so that
/// the codegen works correctly. It also assigns any default values found in the
/// function parameters to the current function call (if expected).
/// It also wraps "this" in a pointer if the modifier of the function that it is
/// calling expects this as a pointer.
pub struct CallArgs<'a> {
    analyze_context: &'a AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> CallArgs<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    /// If any of the given `args` are named arguments, check so that every name
    /// is unique and is found only ones in the given argument list.
    /// Reports errors into `self.errors` if duplicates are found and returns
    /// false. Returns true if no duplicates are found.
    fn names_are_unique(&mut self, args: &[Argument]) -> bool {
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

                    let err = self.analyze_context.err(format!(
                        "Found multiple arguments with the name \"{}\" in argument list:{}",
                        arg_name, &err_pos_msg
                    ));
                    self.errors.push(err);
                }
            }
        }

        names_are_unique
    }

    fn reorder(&mut self, args: &mut Vec<Argument>, params: &[Rc<RefCell<Var>>]) {
        if params.is_empty() || !self.names_are_unique(args) {
            return;
        }

        let mut param_name_to_idx = HashMap::new();
        for (idx, param) in params.iter().enumerate() {
            param_name_to_idx.insert(param.borrow().name.clone(), idx);
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
                let err = self.analyze_context.err(format!(
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
        fn_call: &mut FnCall,
        params: &[Rc<RefCell<Var>>],
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
                let param = param.borrow();

                if let Some(default_value) = &param.value {
                    let default_arg =
                        Argument::new(Some(param.name.clone()), None, *default_value.clone());
                    fn_call.arguments.push(default_arg);
                } else {
                    return Err(self.analyze_context.err(format!(
                        "Function call to \"{}\" missing argument for parameter \"{}\".",
                        &fn_call.name, &param.name
                    )));
                }
            }
        }

        Ok(())
    }
}

impl<'a> Visitor for CallArgs<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseContext) {
        // Function calls on variables containing fn pointers does currently not
        // support named arguments (since the names of the parameters isn't know
        // in those cases).
        if fn_call.is_fn_ptr_call {
            return;
        }

        // If this is a function contained in a ADT/trait (method), one needs to
        // make sure to fetch it as a method since they are stored differently
        // compared to a regular function.
        let func_res = if let Some(adt_type_id) = &fn_call.method_adt {
            let adt_ty = match self.analyze_context.ty_env.ty(*adt_type_id) {
                Ok(adt_ty) => adt_ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let full_path = match adt_ty {
                Ty::CompoundType(inner_ty, generics, ..) => match inner_ty {
                    InnerTy::Struct(mut path)
                    | InnerTy::Enum(mut path)
                    | InnerTy::Union(mut path)
                    | InnerTy::Trait(mut path) => {
                        // TODO: Is this needed? Just want to make sure that the
                        //       generics are as up-to-date as possible.
                        let mut last_part = path.pop().unwrap();
                        last_part.1 = Some(generics.clone());
                        path.push(last_part);

                        path
                    }
                    _ => {
                        let err = self.analyze_context.err(format!(
                            "Bad inner type for func call method_adt: {:#?}",
                            fn_call
                        ));
                        self.errors.push(err);
                        return;
                    }
                },
                _ => {
                    let err = self.analyze_context.err(format!(
                        "method_adt not valid type for func call: {:#?}",
                        fn_call
                    ));
                    self.errors.push(err);
                    return;
                }
            };

            self.analyze_context
                .get_method(&full_path, &fn_call.half_name(), ctx.block_id)
        } else {
            let partial_path = fn_call
                .module
                .clone_push(&fn_call.name, fn_call.generics.as_ref());

            let full_path = match self
                .analyze_context
                .calculate_fn_full_path(&partial_path, ctx.block_id)
            {
                Ok(full_path) => full_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.analyze_context.get_fn(&full_path, ctx.block_id)
        };

        let func = match func_res {
            Ok(func) => func,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let func = func.borrow();

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
        self.reorder(&mut fn_call.arguments, params);

        // Assign any default value for arguments that are missing a value in
        // the function call.
        if let Err(err) = self.default_args(fn_call, params, func.is_var_arg) {
            self.errors.push(err);
            return;
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseContext) {
        match adt_init.kind {
            AdtKind::Struct => (),
            AdtKind::Union => return,
            AdtKind::Enum | AdtKind::Unknown => unreachable!("{:#?}", adt_init.kind),
        }

        let ret_type_id = if let Some(ret_type_id) = &adt_init.ret_type {
            *ret_type_id
        } else {
            unreachable!("Adt init type not compound: {:#?}", adt_init);
        };

        let ret_ty = match self.analyze_context.ty_env.ty(ret_type_id) {
            Ok(ret_ty) => ret_ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let generics = if let Ty::CompoundType(_, generics, _) = ret_ty {
            if generics.len_types() > 0 {
                Some(generics)
            } else {
                None
            }
        } else {
            unreachable!("Adt init type not compound: {:#?}", adt_init);
        };

        let partial_path = adt_init
            .module
            .clone_push(&adt_init.name, generics.as_ref());

        let full_path = match self
            .analyze_context
            .calculate_adt_full_path(&partial_path, ctx.block_id)
        {
            Ok(full_path) => full_path,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let adt = match self.analyze_context.get_adt(&full_path, ctx.block_id) {
            Ok(adt) => adt,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        self.reorder(&mut adt_init.arguments, &adt.borrow().members);

        // TODO: Should there be default values for structs (?). Arrange that
        //       here in that case.
    }
}
