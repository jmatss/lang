use crate::AnalyzeContext;
use common::{
    error::CustomResult,
    error::LangError,
    token::expr::Argument,
    token::expr::FuncCall,
    token::expr::{StructInit, Var},
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    util,
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
        if !self.names_are_unique(args) {
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
        func_call: &mut FuncCall,
        params: &[Rc<RefCell<Var>>],
        is_variadic: bool,
    ) -> CustomResult<()> {
        // If here are more parameters in the function that there are arguments
        // to the current function call, assume that "missing" arguments
        // are supposed to be filled in with the default parameters. If there are
        // no default value set, report that the function call contains to few
        // arguments.
        if !is_variadic && func_call.arguments.len() < params.len() {
            let start_idx = func_call.arguments.len();
            for param in params[start_idx..].iter() {
                let param = param.borrow();

                if let Some(default_value) = &param.value {
                    let default_arg =
                        Argument::new(Some(param.name.clone()), None, *default_value.clone());
                    func_call.arguments.push(default_arg);
                } else {
                    return Err(self.analyze_context.err(format!(
                        "Function call to \"{}\" missing argument for parameter \"{}\".",
                        &func_call.name, &param.name
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

    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        // If this is a function contained in a structure (method), one needs to
        // make sure to fetch it as a method since they are stored differently
        // compared to a regular function.
        let func_res = if let Some(ty) = &func_call.method_structure {
            let full_struct_name = match ty {
                Ty::CompoundType(inner_ty, generics, ..) => match inner_ty {
                    InnerTy::Struct(ident) | InnerTy::Enum(ident) | InnerTy::Trait(ident) => {
                        util::to_generic_name(ident, generics)
                    }
                    _ => {
                        let err = self.analyze_context.err(format!(
                            "Bad inner type for func call method_structure: {:#?}",
                            func_call
                        ));
                        self.errors.push(err);
                        return;
                    }
                },
                _ => {
                    let err = self.analyze_context.err(format!(
                        "method structure not valid type for func call: {:#?}",
                        func_call
                    ));
                    self.errors.push(err);
                    return;
                }
            };

            self.analyze_context
                .get_method(&full_struct_name, &func_call.half_name(), ctx.block_id)
        } else {
            self.analyze_context
                .get_func(&func_call.half_name(), ctx.block_id)
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
        self.reorder(&mut func_call.arguments, params);

        // Assign any default value for arguments that are missing a value in
        // the function call.
        if let Err(err) = self.default_args(func_call, params, func.is_var_arg) {
            self.errors.push(err);
            return;
        }
    }

    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        // TODO: Something similar for enums and interfaces?
        let full_name = match struct_init.full_name() {
            Ok(full_name) => full_name,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let struct_ = match self.analyze_context.get_struct(&full_name, ctx.block_id) {
            Ok(struct_) => struct_,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // Get the members of the struct. This will be used to reorder the
        // arguments in the struct init call correctly.
        let struct_ = struct_.borrow();
        let members = if let Some(members) = &struct_.members {
            members
        } else {
            // Early return if there are no members of the struct, there
            // is nothing to do here.
            return;
        };

        // Reorder the arguments of the struct init call according to the member
        // names used for the arguments.
        self.reorder(&mut struct_init.arguments, members);

        // TODO: Should there be default values for structs (?). Arrange that
        //       here in that case.
    }
}
