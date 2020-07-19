use crate::analyze::analyzer::AnalyzeContext;
use crate::error::CustomError::AnalyzeError;
use crate::parse::token::{
    BlockHeader, BlockId, Enum, Function, Interface, ParseToken, ParseTokenKind, Statement, Struct,
};
use crate::CustomResult;

pub struct DeclAnalyzer<'a, 'ctx> {
    context: &'a mut AnalyzeContext<'ctx>,
}

impl<'a, 'ctx> DeclAnalyzer<'a, 'ctx> {
    /// Takes in a the root of the AST and walks the whole tree to find all
    /// declarations/prototypes and adds them to the `AnalyzeContext` so that
    /// key can quickly be look up during generation of LLVM IR.
    pub fn analyze(
        context: &'a mut AnalyzeContext<'ctx>,
        ast_root: &mut ParseToken,
    ) -> CustomResult<()> {
        let mut decl_analyzer = DeclAnalyzer::new(context);
        decl_analyzer.analyze_token(ast_root)
    }

    fn new(context: &'a mut AnalyzeContext<'ctx>) -> Self {
        Self { context }
    }

    fn analyze_token(&mut self, token: &mut ParseToken) -> CustomResult<()> {
        match &mut token.kind {
            ParseTokenKind::Block(header, id, body) => {
                self.context.cur_block_id = id.clone();
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

            BlockHeader::Default => Ok(()),
            _ => Ok(()),
        }
    }

    fn analyze_func_header(&mut self, func: &Function, id: BlockId) -> CustomResult<()> {
        if let Some(prev_func) = self.context.functions.get_mut(&func.name, id) {
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
                Err(AnalyzeError(err_msg))
            }
        } else {
            self.context.functions.insert(&func.name, id, func.clone());

            // Add the parameters as variables for this scope.
            if let Some(ref params) = func.parameters {
                for param in params {
                    self.context
                        .variables
                        .insert(&param.name, id, param.clone());
                }
            }

            Ok(())
        }
    }

    fn analyze_struct_header(&mut self, struct_: &Struct, id: BlockId) -> CustomResult<()> {
        if let Some(prev_struct) = self.context.structs.get(&struct_.name, id) {
            Err(AnalyzeError(format!(
                "A struct with name \"{}\" already defined.",
                prev_struct.name
            )))
        } else {
            self.context
                .structs
                .insert(&struct_.name, id, struct_.clone());
            Ok(())
        }
    }

    fn analyze_enum_header(&mut self, enum_: &Enum, id: BlockId) -> CustomResult<()> {
        if let Some(prev_enum) = self.context.enums.get(&enum_.name, id) {
            Err(AnalyzeError(format!(
                "A enum with name \"{}\" already defined.",
                prev_enum.name
            )))
        } else {
            self.context.enums.insert(&enum_.name, id, enum_.clone());
            Ok(())
        }
    }

    fn analyze_interface_header(&mut self, interface: &Interface, id: BlockId) -> CustomResult<()> {
        if let Some(prev_interface) = self.context.interfaces.get(&interface.name, id) {
            Err(AnalyzeError(format!(
                "A interface with name \"{}\" already defined.",
                prev_interface.name
            )))
        } else {
            self.context
                .interfaces
                .insert(&interface.name, id, interface.clone());
            Ok(())
        }
    }

    /// Need to add declaration of variable if this stmt is a variable decl.
    fn analyze_stmt(&mut self, stmt: &Statement) -> CustomResult<()> {
        let cur_id = self.context.cur_block_id;
        if let Statement::VariableDecl(var, _) = stmt {
            self.context
                .variables
                .insert(&var.name, cur_id, var.clone());
        }
        Ok(())
    }
}
