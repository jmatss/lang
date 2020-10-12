use std::cell::RefCell;

use common::{
    error::CustomResult, error::LangError, token::expr::Argument, token::expr::FuncCall,
    token::expr::Var, traverser::TraverseContext, visitor::Visitor,
};
use log::warn;

use crate::AnalyzeContext;

/// Iterates through all function and method calls and re-orders all named
/// arguments so that they are put in the correct index position so that
/// the codegen works correctly. It also assigns any default values found in the
/// function parameters to the current function call (if expected).
pub struct CallArgs<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a> CallArgs<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn reorder(&mut self, func_call: &mut FuncCall, params: &[Var]) -> CustomResult<()> {
        let analyze_context = self.analyze_context.borrow();

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
                    if arg_name == &param.name {
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
                    return Err(analyze_context.err(format!(
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

    fn default_args(
        &self,
        func_call: &mut FuncCall,
        params: &[Var],
        is_variadic: bool,
    ) -> CustomResult<()> {
        let analyze_context = self.analyze_context.borrow();

        // If here are more parameters in the function that there are arguments
        // to the current function call, assume that "missing" arguments
        // are supposed to be filled in with the default parameters. If there are
        // no default value set, report that the function call contains to few
        // arguments.
        if !is_variadic && func_call.arguments.len() < params.len() {
            let start_idx = func_call.arguments.len();
            for param in params[start_idx..].iter() {
                if let Some(default_value) = &param.default_value {
                    let default_arg =
                        Argument::new(Some(param.name.clone()), *default_value.clone());
                    func_call.arguments.push(default_arg);
                } else {
                    return Err(analyze_context.err(format!(
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
        let analyze_context = self.analyze_context.borrow();

        let func = if let Some(struct_name) = &func_call.method_struct {
            // Get function if this is a struct method.
            match analyze_context.get_struct_method(struct_name, &func_call.name, ctx.block_id) {
                Ok(func) => func,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        } else {
            // Get function if this is a regular stand-alone function.
            let parent_id = 0;
            let key = (func_call.name.clone(), parent_id);
            if let Some(func) = analyze_context.functions.get(&key) {
                func
            } else {
                let err = analyze_context.err(format!(
                    "Unable to find function \"{}\" in block id {}.",
                    &func_call.name, parent_id
                ));
                self.errors.push(err);
                return;
            }
        };

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
        if let Err(err) = self.reorder(func_call, params) {
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
}
