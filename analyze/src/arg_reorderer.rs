use common::{
    error::LangError, token::expr::FuncCall, traverser::TraverseContext, visitor::Visitor,
};

use crate::AnalyzeContext;

/// Iterates through all function and method calls and re-orders all named
/// arguments so that they are put in the correct index position so that
/// the codegen works correctly.
pub struct ArgReorderer<'a> {
    analyze_context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> ArgReorderer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for ArgReorderer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        // Early return if there are no arguments to reorder.
        if func_call.arguments.is_empty() {
            return;
        }

        let func = if let Some(struct_name) = &func_call.method_struct {
            // Get function if this is a struct method.
            match self
                .analyze_context
                .get_struct_method(struct_name, &func_call.name, ctx.block_id)
            {
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
            if let Some(func) = self.analyze_context.functions.get(&key) {
                func
            } else {
                let err = self.analyze_context.err(format!(
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
            let err = self.analyze_context.err(format!(
                "Unable to get parameters for function \"{}\", expected atleast one param.",
                &func_call.name
            ));
            self.errors.push(err);
            return;
        };

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
                    let err = self.analyze_context.err(format!(
                        "Unable to find parameter \"{}\" in function \"{}\".",
                        arg_name, &func_call.name
                    ));
                    self.errors.push(err);
                    return;
                }
            } else {
                idx += 1;
            }
        }
    }
}
