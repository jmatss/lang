use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function},
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    types::Type,
    visitor::Visitor,
    BlockId,
};
use std::collections::{hash_map::Entry, HashMap};

/// Gathers information about all declarations found in the AST and inserts
/// them into the `analyze_context`. This includes variables, external declarations,
/// functions, structs, enums and interfaces.
pub struct DeclAnalyzer<'a> {
    analyze_context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> DeclAnalyzer<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn analyze_func_header(&mut self, func: &Function, func_id: BlockId) {
        // Add the function in the scope of its root parent (`root_parent_id`).
        let root_parent_id = match self.analyze_context.get_next_root_parent(func_id) {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // If true: Function already declared somewhere, make sure that the
        // current declaration and the previous one matches.
        let key = (func.name.clone(), root_parent_id);
        if let Some(prev_func) = self.analyze_context.functions.get(&key) {
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
                let err = self.analyze_context.err(err_msg);
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
                        let err = self.analyze_context.err(err_msg);
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
                        let err = self.analyze_context.err(err_msg);
                        self.errors.push(err);
                    }
                }
            }
        } else {
            self.analyze_context.functions.insert(key, func.clone());

            // Add the parameters as variables in the function scope.
            if let Some(ref params) = func.parameters {
                for param in params {
                    let param_key = (param.name.clone(), func_id);
                    self.analyze_context
                        .variables
                        .insert(param_key, param.clone());
                }
            }
        }
    }

    fn analyze_method_header(&mut self, struct_name: &str, func: &mut Function, func_id: BlockId) {
        // Add the methods in the scope of the structs root parent.
        let struct_parent_id = match self
            .analyze_context
            .get_struct_parent_id(struct_name.into(), func_id)
        {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // Since this is a method, the first parameters should be "this"/"self".
        // TODO: Where should the name of "this"/"self" be specified?
        const THIS_VAR_NAME: &str = "this";
        let ty = Type::Custom(struct_name.into());
        let var = Var::new(THIS_VAR_NAME.into(), Some(ty), None, false);
        if let Some(ref mut params) = func.parameters {
            params.insert(0, var);
        } else {
            func.parameters = Some(vec![var]);
        }

        // Insert this method into `methods` in the analyze context.
        let key = (struct_name.into(), struct_parent_id);
        match self.analyze_context.methods.entry(key) {
            Entry::Occupied(ref mut v) => {
                v.get_mut().insert(func.name.clone(), func.clone());
            }
            Entry::Vacant(_) => {
                let err = self.analyze_context.err(format!(
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
                self.analyze_context
                    .variables
                    .insert(param_key, param.clone());
            }
        }
    }
}

impl<'a> Visitor for DeclAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(_, id, _) = ast_token {
            self.analyze_context.cur_block_id = *id;
        }
    }

    /// Create a entry for this impl block in the analyze contexts `methods` map
    /// and marks the functions that it contain with the name of this impl block.
    /// This lets the functions when they are visited to know that they are
    /// methods.
    fn visit_impl(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(BlockHeader::Implement(struct_name), impl_id, body) = ast_token {
            let struct_parent_id = match self
                .analyze_context
                .get_struct_parent_id(struct_name.clone(), *impl_id)
            {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let key = (struct_name.clone(), struct_parent_id);
            self.analyze_context.methods.insert(key, HashMap::default());

            for body_token in body {
                if let AstToken::Block(BlockHeader::Function(func), ..) = body_token {
                    func.method_struct = Some(struct_name.clone());
                } else {
                    let err = self.analyze_context.err(format!(
                        "AST token in impl block with name \"{}\" not a function: {:?}",
                        struct_name, body_token
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_func(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(BlockHeader::Function(func), func_id, ..) = ast_token {
            if let Some(struct_name) = func.method_struct.clone() {
                self.analyze_method_header(&struct_name, func, *func_id);
            } else {
                self.analyze_func_header(func, *func_id);
            }
        }
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(BlockHeader::Struct(struct_), struct_id, ..) = ast_token {
            // Add the struct in the scope of its root parent (`root_parent_id`).
            let root_parent_id = match self.analyze_context.get_next_root_parent(*struct_id) {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (struct_.name.clone(), root_parent_id);
            if let Some(prev_struct) = self.analyze_context.structs.get(&key) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = self.analyze_context.err(format!(
                    "A struct with name \"{}\" already defined.",
                    prev_struct.name
                ));
                self.errors.push(err);
            } else {
                self.analyze_context.structs.insert(key, struct_.clone());
            }
        }
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(BlockHeader::Enum(enum_), enum_id, ..) = ast_token {
            // Add the enum in the scope of its root parent (`root_parent_id`).
            let root_parent_id = match self.analyze_context.get_next_root_parent(*enum_id) {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (enum_.name.clone(), root_parent_id);
            if let Some(prev_enum) = self.analyze_context.enums.get(&key) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = self.analyze_context.err(format!(
                    "A enum with name \"{}\" already defined.",
                    prev_enum.name
                ));
                self.errors.push(err);
            } else {
                self.analyze_context.enums.insert(key, enum_.clone());
            }
        }
    }

    fn visit_interface(&mut self, ast_token: &mut AstToken) {
        if let AstToken::Block(BlockHeader::Interface(interface), interface_id, ..) = ast_token {
            // Add the interface in the scope of its root parent (`root_parent_id`).
            let root_parent_id = match self.analyze_context.get_next_root_parent(*interface_id) {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (interface.name.clone(), root_parent_id);
            if let Some(prev_interface) = self.analyze_context.interfaces.get(&key) {
                // TODO: Should this be done in the same way as function, that
                //       one just checks that the declarations are equals and doesn't
                //       throw a exception? This would allow for "extern" declarations
                //       but might be problematic if it two defines.
                let err = self.analyze_context.err(format!(
                    "A interface with name \"{}\" already defined.",
                    prev_interface.name
                ));
                self.errors.push(err);
            } else {
                self.analyze_context
                    .interfaces
                    .insert(key, interface.clone());
            }
        }
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt) {
        if let Stmt::VariableDecl(var, _) = stmt {
            let key = (var.name.clone(), self.analyze_context.cur_block_id);

            if let Entry::Vacant(v) = self.analyze_context.variables.entry(key) {
                v.insert(var.clone());
            } else {
                let err_msg = format!(
                    "A variable with name \"{}\" already declared in this scope ({}).",
                    &var.name, self.analyze_context.cur_block_id
                );
                let err = self.analyze_context.err(err_msg);
                self.errors.push(err);
            }
        }
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt) {
        if let Stmt::ExternalDecl(func) = stmt {
            // TODO: Don't hardcode zeros for the default block everywhere.
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a function that they have the same
            //       parameters & return type.
            // External declarations should always be in the default block.
            let key = (func.name.clone(), 0);
            self.analyze_context.functions.insert(key, func.clone());
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt) {}

    fn visit_eof(&mut self, ast_token: &mut AstToken) {}

    fn visit_default_block(&mut self, ast_token: &mut AstToken) {}

    fn visit_anon(&mut self, ast_token: &mut AstToken) {}

    fn visit_if(&mut self, ast_token: &mut AstToken) {}

    fn visit_if_case(&mut self, ast_token: &mut AstToken) {}

    fn visit_match(&mut self, ast_token: &mut AstToken) {}

    fn visit_match_case(&mut self, ast_token: &mut AstToken) {}

    fn visit_for(&mut self, ast_token: &mut AstToken) {}

    fn visit_while(&mut self, ast_token: &mut AstToken) {}

    fn visit_test(&mut self, ast_token: &mut AstToken) {}

    fn visit_return(&mut self, stmt: &mut Stmt) {}

    fn visit_yield(&mut self, stmt: &mut Stmt) {}

    fn visit_break(&mut self, stmt: &mut Stmt) {}

    fn visit_continue(&mut self, stmt: &mut Stmt) {}

    fn visit_use(&mut self, stmt: &mut Stmt) {}

    fn visit_package(&mut self, stmt: &mut Stmt) {}

    fn visit_defer(&mut self, stmt: &mut Stmt) {}

    fn visit_defer_exec(&mut self, stmt: &mut Stmt) {}

    fn visit_assignment(&mut self, stmt: &mut Stmt) {}

    fn visit_modifier(&mut self, stmt: &mut Stmt) {}

    fn visit_lit(&mut self, expr: &mut Expr) {}

    fn visit_var(&mut self, var: &mut Var) {}

    fn visit_func_call(&mut self, func_call: &mut FuncCall) {}

    fn visit_struct_init(&mut self, struct_init: &mut StructInit) {}

    fn visit_array_init(&mut self, expr: &mut ArrayInit) {}

    fn visit_bin_op(&mut self, bin_op: &mut BinOp) {}

    fn visit_un_op(&mut self, un_op: &mut UnOp) {}
}