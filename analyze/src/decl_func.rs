use crate::{AnalyzeContext, BlockInfo};
use common::{
    error::LangError,
    token::ast::Token,
    token::expr::Var,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    types::Type,
    visitor::Visitor,
    BlockId,
};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, BTreeMap, HashMap},
};

/// Gathers information about all function/method declarations found in the AST
/// and inserts them into the `analyze_context`. This includes external function
/// declarations, functions and methods (in implement block).
pub struct DeclFuncAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a> DeclFuncAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn analyze_func_header(&mut self, func: &mut Function, func_id: BlockId) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        // The function will be added in the scope of its parent, so fetch the
        // block id for the parent.
        let parent_id = match analyze_context.get_parent(func_id) {
            Ok(parent_id) => parent_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // If true: Function already declared somewhere, make sure that the
        // current declaration and the previous one matches.
        if let Ok(prev_func) = analyze_context.get_func(&func.name, parent_id) {
            let empty_vec = Vec::new();
            let cur_func_params = if let Some(params) = &func.parameters {
                params
            } else {
                &empty_vec
            };
            let prev_func_params = if let Some(params) = &prev_func.parameters {
                params
            } else {
                &empty_vec
            };

            // Check that they have the same amount of parameters and
            // their types are equal.
            if cur_func_params.len() != prev_func_params.len() {
                let err_msg = format!(
                    "Two declarations of function \"{}\" have different amount of parameters. \
                    Prev amount: {}, current amount: {}",
                    &func.name,
                    cur_func_params.len(),
                    prev_func_params.len(),
                );
                let err = analyze_context.err(err_msg);
                self.errors.push(err);
            } else {
                for (i, (cur_param, prev_param)) in cur_func_params
                    .iter()
                    .zip(prev_func_params.iter())
                    .enumerate()
                {
                    if cur_param.name != prev_param.name {
                        let err_msg = format!(
                            "Two declarations of function \"{}\" have parameters with different names. \
                            Parameter at position {}. Prev name: {:?}, current name: {:?}.",
                            &func.name, i, &cur_param.name, &prev_param.name
                        );
                        let err = analyze_context.err(err_msg);
                        self.errors.push(err);
                    }
                    if cur_param.ret_type != prev_param.ret_type {
                        let param_name = if cur_param.name == prev_param.name {
                            cur_param.name.clone()
                        } else {
                            format!("{}/{}", &prev_param.name, &cur_param.name)
                        };
                        let err_msg = format!(
                            "Two declarations of function \"{}\" have parameters with different types. \
                            Parameter at position {} with name \"{}\". \
                            Prev type: {:?}, current type: {:?}",
                            &func.name, i, &param_name, cur_param.ret_type, prev_param.ret_type
                        );
                        let err = analyze_context.err(err_msg);
                        self.errors.push(err);
                    }
                }
            }
        } else {
            // Add the function into decl lookup maps.
            let key = (func.name.clone(), parent_id);
            let func_ptr = func as *mut Function;
            analyze_context.functions.insert(key, func_ptr);

            // Add the parameters as variables in the function scope decl lookup.
            if let Some(params) = &mut func.parameters {
                for param in params {
                    let param_key = (param.name.clone(), func_id);
                    let param_ptr = param as *mut Var;
                    analyze_context.variables.insert(param_key, param_ptr);
                }
            }
        }
    }

    fn analyze_method_header(&mut self, struct_name: &str, func: &mut Function, func_id: BlockId) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        // The method will be added in the scope of its wrapping struct, so
        // fetch the block id for the struct.
        let struct_decl_id = match analyze_context.get_struct_decl_scope(struct_name, func_id) {
            Ok(struct_decl_id) => struct_decl_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // TODO: Should probably be changed to something better.
        // If this is a non-static method, the first parameter should be a
        // reference(/pointer) to "this"/"self".
        if !func.is_static() {
            const THIS_VAR_NAME: &str = "this";
            let ty = Type::Pointer(Box::new(Type::CompoundType(
                struct_name.into(),
                BTreeMap::default(),
            )));
            let var = Var::new(THIS_VAR_NAME.into(), Some(ty), None, None, false);
            if let Some(ref mut params) = func.parameters {
                params.insert(0, var);
            } else {
                func.parameters = Some(vec![var]);
            }
        }

        // Insert this method into `methods` in the analyze context.
        let key = (struct_name.into(), struct_decl_id);
        match analyze_context.methods.entry(key) {
            Entry::Occupied(ref mut v) => {
                let func_ptr = func as *mut Function;
                v.get_mut().insert(func.name.clone(), func_ptr);
            }
            Entry::Vacant(_) => {
                let err = analyze_context.err(format!(
                    "Unable to find decl methods for struct \"{}\" in block with id {}.",
                    struct_name, struct_decl_id
                ));
                self.errors.push(err);
            }
        }

        // Add the parameters as variables in the method scope.
        if let Some(params) = &mut func.parameters {
            for param in params {
                let param_key = (param.name.clone(), func_id);
                let param_ptr = param as *mut Var;
                analyze_context.variables.insert(param_key, param_ptr);
            }
        }
    }
}

impl<'a> Visitor for DeclFuncAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.analyze_context.borrow_mut().line_nr = ast_token.line_nr;
        self.analyze_context.borrow_mut().column_nr = ast_token.column_nr;
    }

    /// Create a entry for this impl block in the analyze contexts `methods` map
    /// and marks the functions that it contain with the name of this impl block.
    /// This lets the one differentiate between functions and methods in the
    /// Function struct by checking the `method_struct` field.
    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Token::Block(BlockHeader::Implement(struct_name), impl_id, body) =
            &mut ast_token.token
        {
            let struct_decl_id = match analyze_context.get_struct_decl_scope(struct_name, *impl_id)
            {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (struct_name.clone(), struct_decl_id);
            analyze_context.methods.insert(key, HashMap::default());

            for body_token in body {
                if let Token::Block(BlockHeader::Function(func), ..) = &mut body_token.token {
                    func.method_struct = Some(struct_name.clone());
                } else {
                    let err = analyze_context.err(format!(
                        "AST token in impl block with name \"{}\" not a function: {:?}",
                        struct_name, body_token
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_func(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Function(func), func_id, ..) = &mut ast_token.token {
            if let Some(struct_name) = func.method_struct.clone() {
                self.analyze_method_header(&struct_name, func, *func_id);
            } else {
                self.analyze_func_header(func, *func_id);
            }
        }
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Stmt::ExternalDecl(func) = stmt {
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a function that they have the same
            //       parameters & return type.
            // External declarations should always be in the default block.
            let key = (func.name.clone(), BlockInfo::DEFAULT_BLOCK_ID);
            let func_ptr = func as *mut Function;
            analyze_context.functions.insert(key, func_ptr);
        }
    }
}
