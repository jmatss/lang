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
use std::{cell::RefCell, rc::Rc};

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

    fn reorder_func_call(
        &mut self,
        func_call: &mut FuncCall,
        params: &[Rc<RefCell<Var>>],
    ) -> CustomResult<()> {
        let mut idx: u64 = 0;
        while idx < func_call.arguments.len() as u64 {
            let arg = func_call
                .arguments
                .get(idx as usize)
                .expect("Known to be in bounds.");

            // TODO: Can this lead to a infinite loop where two named arguments
            //       with the same name will be swapped back over and over?
            if let Some(arg_name) = &arg.name {
                let mut new_idx = 0;
                let mut found = false;
                for param in params {
                    if arg_name == &param.borrow().name {
                        found = true;
                        break;
                    }
                    new_idx += 1;
                }

                if found && idx == new_idx {
                    idx += 1;
                } else if found {
                    // The `idx` is not increment if a swap was made. This is done
                    // since then you wan't to see if the new argument at this idx
                    // needs to be swapped to somewhere else.
                    func_call.arguments.swap(idx as usize, new_idx as usize);
                } else {
                    return Err(self.analyze_context.err(format!(
                        "Unable to find parameter \"{}\" in function \"{}\".",
                        arg_name, &func_call.name
                    )));
                }
            } else {
                idx += 1;
            }
        }

        Ok(())
    }

    fn reorder_struct_init(
        &mut self,
        struct_init: &mut StructInit,
        members: &[Rc<RefCell<Var>>],
    ) -> CustomResult<()> {
        let mut idx: u64 = 0;
        while idx < struct_init.arguments.len() as u64 {
            let arg = struct_init
                .arguments
                .get(idx as usize)
                .expect("Known to be in bounds.");

            // TODO: Can this lead to a infinite loop where two named arguments
            //       with the same name will be swapped back over and over?
            if let Some(arg_name) = &arg.name {
                let mut new_idx = 0;
                let mut found = false;
                for member in members {
                    if arg_name == &member.borrow().name {
                        found = true;
                        break;
                    }
                    new_idx += 1;
                }

                if found && idx == new_idx {
                    idx += 1;
                } else if found {
                    // The `idx` is not increment if a swap was made. This is done
                    // since then you wan't to see if the new argument at this idx
                    // needs to be swapped to somewhere else.
                    struct_init.arguments.swap(idx as usize, new_idx as usize);
                } else {
                    return Err(self.analyze_context.err(format!(
                        "Unable to find member \"{}\" in struct \"{}\".",
                        arg_name, &struct_init.name
                    )));
                }
            } else {
                idx += 1;
            }
        }

        Ok(())
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
                        Argument::new(Some(param.name.clone()), *default_value.clone());
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
                Ty::CompoundType(inner_ty, generics) => match inner_ty {
                    InnerTy::Struct(ident) | InnerTy::Enum(ident) | InnerTy::Interface(ident) => {
                        util::to_generic_struct_name(ident, generics)
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
                .get_method(&full_struct_name, &func_call.name, ctx.block_id)
        } else {
            self.analyze_context.get_func(&func_call.name, ctx.block_id)
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
        if let Err(err) = self.reorder_func_call(func_call, params) {
            self.errors.push(err);
            return;
        }

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
        if let Err(err) = self.reorder_struct_init(struct_init, members) {
            self.errors.push(err);
            return;
        }

        // TODO: Should there be default values for structs (?). Arrange that
        //       here in that case.
    }
}
