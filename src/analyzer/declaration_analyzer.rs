use crate::analyzer::analyzer::AnalyzeContext;
use crate::parser::abstract_syntax_tree::{ASTBlock, RCNode, AST};
use crate::parser::token::{BlockHeader, Token};
use crate::CustomResult;
use std::cell::RefMut;
use std::collections::HashMap;

/// Parses all declarations/prototypes so that they are known and can be used when creating
/// the LLVM IR.
pub struct DeclarationAnalyzer<'a> {
    context: &'a mut AnalyzeContext<'a>,
}

impl<'a> DeclarationAnalyzer<'a> {
    /// Takes in a abstract syntax tree and tries to infer all the missing declarations.
    pub fn analyze(context: &'a mut AnalyzeContext, ast: &AST) -> CustomResult<()> {
        let mut declaration_analyzer = DeclarationAnalyzer::new(context);
        let root_block = ast.blocks[0].borrow_mut();
        declaration_analyzer.analyze_recursive(root_block)
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self { context }
    }

    fn analyze_recursive(&mut self, block: RefMut<ASTBlock>) -> CustomResult<()> {
        for child in &block.children {
            if let RCNode::Block(ast_block) = child {
                let mut block = ast_block.borrow_mut();
                self.context.current_scope = block.index;
                self.context
                    .parent_scopes
                    .insert(block.index, block.parent_index);
                self.parse_token(&mut block.token)?;
                self.analyze_recursive(block)?;
            }
        }

        Ok(())
    }

    fn parse_token(&mut self, token: &mut Token) -> CustomResult<()> {
        match token {
            Token::BlockHeader(block_header) => self.parse_block_header(&block_header),
            _ => Ok(()),
        }
    }

    fn parse_block_header(&mut self, block_header: &BlockHeader) -> CustomResult<()> {
        match block_header {
            BlockHeader::Default => Ok(()),
            BlockHeader::Function(Some(function)) => {
                if !self.context.functions.contains_key(&function.name) {
                    self.context
                        .functions
                        .insert(function.name.clone(), HashMap::new());
                }

                let scope_functions_opt = self.context.functions.get_mut(&function.name);
                if let Some(scope_functions) = scope_functions_opt {
                    scope_functions.insert(self.context.current_scope, function.clone());
                }

                Ok(())
            }
            BlockHeader::Function(None) => panic!("Bad function None."),
            BlockHeader::Class(Some(class)) => {
                if !self.context.classes.contains_key(&class.name) {
                    self.context
                        .classes
                        .insert(class.name.clone(), HashMap::new());
                }

                let scope_classes_opt = self.context.classes.get_mut(&class.name);
                if let Some(scope_classes) = scope_classes_opt {
                    scope_classes.insert(self.context.current_scope, class.clone());
                }

                Ok(())
            }
            BlockHeader::Class(None) => panic!("Bad class None."),
            BlockHeader::Enum(Some(enum_)) => {
                if !self.context.enums.contains_key(&enum_.name) {
                    self.context
                        .enums
                        .insert(enum_.name.clone(), HashMap::new());
                }

                let scope_enums_opt = self.context.enums.get_mut(&enum_.name.clone());
                if let Some(scope_enums) = scope_enums_opt {
                    scope_enums.insert(self.context.current_scope, enum_.clone());
                }

                Ok(())
            }
            BlockHeader::Enum(None) => panic!("Bad enum None."),
            BlockHeader::Interface(Some(interface)) => {
                if !self.context.interfaces.contains_key(&interface.name) {
                    self.context
                        .interfaces
                        .insert(interface.name.clone(), HashMap::new());
                }

                let scope_interfaces_opt = self.context.interfaces.get_mut(&interface.name);
                if let Some(scope_interfaces) = scope_interfaces_opt {
                    scope_interfaces.insert(self.context.current_scope, interface.clone());
                }

                Ok(())
            }
            BlockHeader::Interface(None) => panic!("Bad interface None."),
            BlockHeader::Macro(Some(macro_)) => {
                if !self.context.macros.contains_key(&macro_.name) {
                    self.context
                        .macros
                        .insert(macro_.name.clone(), HashMap::new());
                }

                let scope_macros_opt = self.context.macros.get_mut(&macro_.name);

                if let Some(scope_macros) = scope_macros_opt {
                    scope_macros.insert(self.context.current_scope, macro_.clone());
                }

                Ok(())
            }
            BlockHeader::Macro(None) => panic!("Bad macro None."),
            _ => Ok(()),
        }
    }
}
