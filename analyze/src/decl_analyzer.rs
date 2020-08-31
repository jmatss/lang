use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        block::{BlockHeader, Enum, Function, Interface, Struct},
        expr::Variable,
        stmt::Statement,
    },
    variable_type::{Type, TypeStruct},
    BlockId,
};
use parse::token::{ParseToken, ParseTokenKind};
use std::collections::{hash_map::Entry, HashMap};

pub struct DeclAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,

    /// Contains the name of the struct for the current "implement" block (if any).
    /// This is needed during analyzing of functions since they might be
    /// declared in a impl block which means that they need to extra logic to
    /// handle the current "object".
    cur_impl: Option<String>,
}

impl<'a> DeclAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find all
    /// declarations/prototypes and adds them to the `AnalyzeContext` so that
    /// they can quickly be looked up during LLVM code generation.
    pub fn analyze(
        context: &'a mut AnalyzeContext,
        ast_root: &mut ParseToken,
    ) -> Result<(), Vec<LangError>> {
        let mut decl_analyzer = DeclAnalyzer::new(context);
        decl_analyzer.analyze_token(ast_root);
        if decl_analyzer.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut decl_analyzer.errors))
        }
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self {
            context,
            errors: Vec::default(),
            cur_impl: None,
        }
    }

    fn analyze_token(&mut self, token: &mut ParseToken) {
        self.context.cur_line_nr = token.line_nr;
        self.context.cur_column_nr = token.column_nr;

        match &mut token.kind {
            ParseTokenKind::Block(header, id, body) => {
                self.context.cur_block_id = *id;
                self.analyze_header(header);

                // Update the current impl block if needed for every iteration
                // of the tokens in the body.
                let cur_impl_opt = if let BlockHeader::Implement(ident) = header {
                    Some(ident.clone())
                } else {
                    None
                };
                for token in body {
                    self.context.cur_block_id = *id;
                    self.cur_impl = cur_impl_opt.clone();
                    self.analyze_token(token);
                }
                self.cur_impl = None;
            }
            ParseTokenKind::Statement(stmt) => self.analyze_stmt(&stmt),
            ParseTokenKind::Expression(_) | ParseTokenKind::EndOfFile => (),
        }
    }

    fn analyze_header(&mut self, header: &mut BlockHeader) {
        let cur_id = self.context.cur_block_id;
        match header {
            BlockHeader::Function(ref mut func) => {
                if let Some(cur_impl_struct_name) = self.cur_impl.clone() {
                    self.analyze_method_header(&cur_impl_struct_name, func, cur_id)
                } else {
                    self.analyze_func_header(func, cur_id)
                }
            }
            BlockHeader::Struct(struct_) => self.analyze_struct_header(struct_, cur_id),
            BlockHeader::Enum(enum_) => self.analyze_enum_header(enum_, cur_id),
            BlockHeader::Interface(interface) => self.analyze_interface_header(interface, cur_id),

            BlockHeader::Default
            | BlockHeader::Implement(..)
            | BlockHeader::If
            | BlockHeader::IfCase(_)
            | BlockHeader::Match(_)
            | BlockHeader::MatchCase(_)
            | BlockHeader::For(_, _)
            | BlockHeader::While(_)
            | BlockHeader::Test(_)
            | BlockHeader::Anonymous => (),
        }
    }

    fn analyze_func_header(&mut self, func: &Function, func_id: BlockId) {
        // Add the function in the scope of its root parent (`root_parent_id`).
        let root_parent_id = match self.context.get_root_parent(func_id) {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let key = (func.name.clone(), root_parent_id);
        if let Some(prev_func) = self.context.functions.get(&key) {
            // Function already declared somewhere, make sure that the
            // current declaration and the previous one matches.
            let empty_vec = Vec::new();
            let cur_func_params = if let Some(ref params) = func.parameters {
                params
            } else {
                &empty_vec
            };
            let prev_func_params = if let Some(ref params) = prev_func.parameters {
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
                let err = self.context.err(err_msg);
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
                        let err = self.context.err(err_msg);
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
                        let err = self.context.err(err_msg);
                        self.errors.push(err);
                    }
                }
            }
        } else {
            self.context.functions.insert(key, func.clone());

            // Add the parameters as variables in the function scope.
            if let Some(ref params) = func.parameters {
                for param in params {
                    let param_key = (param.name.clone(), func_id);
                    self.context.variables.insert(param_key, param.clone());
                }
            }
        }
    }

    fn analyze_method_header(&mut self, struct_name: &str, func: &mut Function, func_id: BlockId) {
        // Add the methods in the scope of the structs root parent.
        let struct_parent_id = match self
            .context
            .get_struct_parent_id(struct_name.into(), func_id)
        {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        func.method_struct = Some(struct_name.into());

        // Since this is a method, the first parameters should be "this"/"self".
        // TODO: Where should the name of "this"/"self" be specified?
        const THIS_VAR_NAME: &str = "this";
        let ty = Type::Custom(struct_name.into());
        let generics = None;
        let type_struct = TypeStruct::new(ty, generics);
        let var = Variable::new(THIS_VAR_NAME.into(), Some(type_struct), None, false);
        if let Some(ref mut params) = func.parameters {
            params.insert(0, var);
        } else {
            func.parameters = Some(vec![var]);
        }

        let key = (struct_name.into(), struct_parent_id);
        if let Some(inner_methods) = self.context.methods.get_mut(&key) {
            // TODO: Should probably check that it doesn't exists two functions
            //       with the same name. Or if overloading allowed, that they
            //       are different enough etc.
            inner_methods.insert(func.name.clone(), func.clone());
        } else {
            let mut inner_methods = HashMap::new();
            inner_methods.insert(func.name.clone(), func.clone());

            self.context.methods.insert(key, inner_methods);
        }

        // Add the parameters as variables in the method scope.
        if let Some(ref params) = func.parameters {
            for param in params {
                let param_key = (param.name.clone(), func_id);
                self.context.variables.insert(param_key, param.clone());
            }
        }
    }

    fn analyze_struct_header(&mut self, struct_: &Struct, struct_id: BlockId) {
        // Add the struct in the scope of its root parent (`root_parent_id`).
        let root_parent_id = match self.context.get_root_parent(struct_id) {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let key = (struct_.name.clone(), root_parent_id);
        if let Some(prev_struct) = self.context.structs.get(&key) {
            // TODO: Should this be done in the same way as function, that
            //       one just checks that the declarations are equals and doesn't
            //       throw a exception? This would allow for "extern" declarations
            //       but might be problematic if it two defines.
            panic!(
                "A struct with name \"{}\" already defined.",
                prev_struct.name,
            );
        } else {
            self.context.structs.insert(key, struct_.clone());
        }
    }

    fn analyze_enum_header(&mut self, enum_: &Enum, enum_id: BlockId) {
        // Add the enum in the scope of its root parent (`root_parent_id`).
        let root_parent_id = match self.context.get_root_parent(enum_id) {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let key = (enum_.name.clone(), root_parent_id);
        if let Some(prev_enum) = self.context.enums.get(&key) {
            // TODO: Should this be done in the same way as function, that
            //       one just checks that the declarations are equals and doesn't
            //       throw a exception? This would allow for "extern" declarations
            //       but might be problematic if it two defines.
            panic!("A enum with name \"{}\" already defined.", prev_enum.name);
        } else {
            self.context.enums.insert(key, enum_.clone());
        }
    }

    fn analyze_interface_header(&mut self, interface: &Interface, interface_id: BlockId) {
        // Add the interface in the scope of its root parent (`root_parent_id`).
        let root_parent_id = match self.context.get_root_parent(interface_id) {
            Ok(id) => id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let key = (interface.name.clone(), root_parent_id);
        if let Some(prev_interface) = self.context.interfaces.get(&key) {
            // TODO: Should this be done in the same way as function, that
            //       one just checks that the declarations are equals and doesn't
            //       throw a exception? This would allow for "extern" declarations
            //       but might be problematic if it two defines.
            panic!(
                "A interface with name \"{}\" already defined.",
                prev_interface.name
            );
        } else {
            self.context.interfaces.insert(key, interface.clone());
        }
    }

    /// Need to add declaration of variable if this stmt is a variable decl
    /// and a function if this stmt is a external declaration.
    fn analyze_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VariableDecl(var, _) => {
                let key = (var.name.clone(), self.context.cur_block_id);

                if let Entry::Vacant(v) = self.context.variables.entry(key) {
                    v.insert(var.clone());
                } else {
                    let err_msg = format!(
                        "A variable with name \"{}\" already declared in this scope ({}).",
                        &var.name, self.context.cur_block_id
                    );
                    let err = self.context.err(err_msg);
                    self.errors.push(err);
                }
            }
            Statement::ExternalDecl(func) => {
                // TODO: Don't hardcode zeros for the default block everywhere.
                // TODO: Should probably check that if there are multiple extern
                //       declarations of a function that they have the same
                //       parameters & return type.
                // External declarations should always be in the default block.
                let key = (func.name.clone(), 0);
                self.context.functions.insert(key, func.clone());
            }

            _ => (),
        }
    }
}
