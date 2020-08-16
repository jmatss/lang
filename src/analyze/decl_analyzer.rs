use crate::analyze::analyzer::AnalyzeContext;
use crate::error::{LangError, LangErrorKind::AnalyzeError};
use crate::parse::token::{
    BlockHeader, BlockId, Enum, Function, Interface, ParseToken, ParseTokenKind, Statement, Struct,
};
use crate::CustomResult;

pub struct DeclAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

impl<'a> DeclAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find all
    /// declarations/prototypes and adds them to the `AnalyzeContext` so that
    /// they can quickly be looked up during LLVM code generation.
    pub fn analyze(context: &'a mut AnalyzeContext, ast_root: &mut ParseToken) -> CustomResult<()> {
        let mut decl_analyzer = DeclAnalyzer::new(context);
        decl_analyzer.analyze_token(ast_root)
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self { context }
    }

    fn analyze_token(&mut self, token: &mut ParseToken) -> CustomResult<()> {
        match &mut token.kind {
            ParseTokenKind::Block(header, id, body) => {
                self.context.cur_block_id = *id;
                self.analyze_header(header)?;
                for token in body {
                    self.analyze_token(token)?;
                }
            }
            ParseTokenKind::Statement(stmt) => self.analyze_stmt(&stmt)?,
            ParseTokenKind::Expression(_) | ParseTokenKind::EndOfFile => (),
        }

        Ok(())
    }

    fn analyze_header(&mut self, header: &BlockHeader) -> CustomResult<()> {
        let cur_id = self.context.cur_block_id;
        match header {
            // TODO: Better error messages. For example print which params are
            //       different and line/column nr etc.
            BlockHeader::Function(func) => self.analyze_func_header(func, cur_id),
            BlockHeader::Struct(struct_) => self.analyze_struct_header(struct_, cur_id),
            BlockHeader::Enum(enum_) => self.analyze_enum_header(enum_, cur_id),
            BlockHeader::Interface(interface) => self.analyze_interface_header(interface, cur_id),

            BlockHeader::Default
            | BlockHeader::If
            | BlockHeader::IfCase(_)
            | BlockHeader::Match(_)
            | BlockHeader::MatchCase(_)
            | BlockHeader::For(_, _)
            | BlockHeader::While(_)
            | BlockHeader::Test(_) => Ok(()),
        }
    }

    fn analyze_func_header(&mut self, func: &Function, func_id: BlockId) -> CustomResult<()> {
        // Add the function in the scope of its root parent (`root_parent_id`).
        let root_parent_id = self.context.get_root_parent(func_id)?;
        let key = (func.name.clone(), root_parent_id);
        if let Some(prev_func) = self.context.functions.get_mut(&key) {
            // Function already declared somewhere, make sure that the
            // current declaration and the previous one matches.
            let empty_vec = Vec::new();
            let func_params = if let Some(ref params) = func.parameters {
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
            let mut err_msg = String::new();
            if func_params.len() != prev_func_params.len() {
                err_msg.push_str(&format!(
                    " Len of params differ: {} and {}.",
                    func_params.len(),
                    prev_func_params.len()
                ));
            } else {
                for cur_params in func_params.iter().zip(prev_func_params.iter()) {
                    if cur_params.0.ret_type != cur_params.1.ret_type {
                        err_msg.push_str(&format!(
                            " Param differ: {:?} and {:?}.",
                            cur_params.0.ret_type, cur_params.1.ret_type
                        ));
                    }
                }
            }

            if err_msg.is_empty() {
                Ok(())
            } else {
                err_msg.push_str(&format!(
                    "Function \"{}\" has two unequal declarations: ",
                    &func.name
                ));
                Err(LangError::new(err_msg, AnalyzeError))
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

            Ok(())
        }
    }

    fn analyze_struct_header(&mut self, struct_: &Struct, struct_id: BlockId) -> CustomResult<()> {
        // Add the struct in the scope of its root parent (`root_parent_id`).
        let root_parent_id = self.context.get_root_parent(struct_id)?;
        let key = (struct_.name.clone(), root_parent_id);
        if let Some(prev_struct) = self.context.structs.get(&key) {
            // TODO: Should this be done in the same way as function, that
            //       one just checks that the declarations are equals and doesn't
            //       throw a exception? This would allow for "extern" declarations
            //       but might be problematic if it two defines.
            Err(LangError::new(
                format!(
                    "A struct with name \"{}\" already defined.",
                    prev_struct.name
                ),
                AnalyzeError,
            ))
        } else {
            self.context.structs.insert(key, struct_.clone());
            Ok(())
        }
    }

    fn analyze_enum_header(&mut self, enum_: &Enum, enum_id: BlockId) -> CustomResult<()> {
        // Add the enum in the scope of its root parent (`root_parent_id`).
        let root_parent_id = self.context.get_root_parent(enum_id)?;
        let key = (enum_.name.clone(), root_parent_id);
        if let Some(prev_enum) = self.context.enums.get(&key) {
            Err(LangError::new(
                format!("A enum with name \"{}\" already defined.", prev_enum.name),
                AnalyzeError,
            ))
        } else {
            self.context.enums.insert(key, enum_.clone());
            Ok(())
        }
    }

    fn analyze_interface_header(
        &mut self,
        interface: &Interface,
        interface_id: BlockId,
    ) -> CustomResult<()> {
        // Add the interface in the scope of its root parent (`root_parent_id`).
        let root_parent_id = self.context.get_root_parent(interface_id)?;
        let key = (interface.name.clone(), root_parent_id);
        if let Some(prev_interface) = self.context.interfaces.get(&key) {
            Err(LangError::new(
                format!(
                    "A interface with name \"{}\" already defined.",
                    prev_interface.name
                ),
                AnalyzeError,
            ))
        } else {
            self.context.interfaces.insert(key, interface.clone());
            Ok(())
        }
    }

    /// Need to add declaration of variable if this stmt is a variable decl
    /// and a function if this stmt is a external declaration.
    fn analyze_stmt(&mut self, stmt: &Statement) -> CustomResult<()> {
        let id = self.context.cur_block_id;

        match stmt {
            Statement::VariableDecl(var, _) => {
                let key = (var.name.clone(), id);
                self.context.variables.insert(key, var.clone());
            }
            Statement::ExternalDecl(func) => {
                // TODO: Don't hardcode zeros for the default block everywhere.
                // External declarations should always be in the default block.
                let key = (func.name.clone(), 0);
                self.context.functions.insert(key, func.clone());
            }

            _ => (),
        }

        Ok(())
    }
}
