use crate::AnalyzeContext;
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
    collections::{hash_map::Entry, HashMap},
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

    fn analyze_func_header(&mut self, func: &Function, func_id: BlockId) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        // Add the function in the scope of its root parent (`root_parent_id`).
        let root_parent_id = match analyze_context.get_next_root_parent(func_id) {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // If true: Function already declared somewhere, make sure that the
        // current declaration and the previous one matches.
        let key = (func.name.clone(), root_parent_id);
        if let Some(prev_func) = analyze_context.functions.get(&key) {
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
            analyze_context.functions.insert(key, func.clone());

            // Add the parameters as variables in the function scope.
            if let Some(ref params) = func.parameters {
                for param in params {
                    let param_key = (param.name.clone(), func_id);
                    analyze_context.variables.insert(param_key, param.clone());
                }
            }
        }
    }

    fn analyze_method_header(&mut self, struct_name: &str, func: &mut Function, func_id: BlockId) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        // Add the methods in the scope of the structs root parent.
        let struct_parent_id =
            match analyze_context.get_struct_parent_id(struct_name.into(), func_id) {
                Ok(id) => id,
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
            let ty = Type::Pointer(Box::new(Type::Custom(struct_name.into())));
            let var = Var::new(THIS_VAR_NAME.into(), Some(ty), None, None, false);
            if let Some(ref mut params) = func.parameters {
                params.insert(0, var);
            } else {
                func.parameters = Some(vec![var]);
            }
        }

        // Insert this method into `methods` in the analyze context.
        let key = (struct_name.into(), struct_parent_id);
        match analyze_context.methods.entry(key) {
            Entry::Occupied(ref mut v) => {
                v.get_mut().insert(func.name.clone(), func.clone());
            }
            Entry::Vacant(_) => {
                let err = analyze_context.err(format!(
                    "Unable to find decl methods for struct \"{}\" in block with id {}.",
                    struct_name, struct_parent_id
                ));
                self.errors.push(err);
            }
        }

        // Add the parameters as variables in the method scope.
        if let Some(ref params) = func.parameters {
            for param in params {
                let param_key = (param.name.clone(), func_id);
                analyze_context.variables.insert(param_key, param.clone());
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
        self.analyze_context.borrow_mut().cur_line_nr = ast_token.line_nr;
        self.analyze_context.borrow_mut().cur_column_nr = ast_token.column_nr;
    }

    /// Create a entry for this impl block in the analyze contexts `methods` map
    /// and marks the functions that it contain with the name of this impl block.
    /// This lets the functions when they are visited to know that they are
    /// methods.
    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Token::Block(BlockHeader::Implement(struct_name), impl_id, body) =
            &mut ast_token.token
        {
            let struct_parent_id =
                match analyze_context.get_struct_parent_id(struct_name.clone(), *impl_id) {
                    Ok(id) => id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
            let key = (struct_name.clone(), struct_parent_id);
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
            // TODO: Don't hardcode zeros for the default block everywhere.
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a function that they have the same
            //       parameters & return type.
            // External declarations should always be in the default block.
            let key = (func.name.clone(), 0);
            analyze_context.functions.insert(key, func.clone());
        }
    }
}
