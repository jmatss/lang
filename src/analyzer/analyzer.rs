use crate::parser::abstract_syntax_tree::{AST, ASTToken};
use crate::analyzer::action_tree::ActionTree;
use crate::CustomResult;
use crate::error::CustomError::AnalyzeError;
use std::cell::Ref;
use crate::parser::token::Token;

pub struct Analyzer {
    ast: AST,
    action_tree: ActionTree,
}

pub fn analyze(ast: AST) -> CustomResult<()> {
    let analyser = Analyzer::new(ast);

    let root_block = analyser.ast.blocks[0].borrow();
    if let Some(data) = &root_block.block_data {
        let children = &data.children;

        for child_ in children {
            let child = child_.borrow();
            let a = analyser.parse_variables(child);
        }
    } else {
        return Err(AnalyzeError(
            "The root block doesn't have any block_data.".to_string()
        ));
    }

    Ok(())
}

impl Analyzer {
    pub fn new(ast: AST) -> Self {
        let action_tree = ActionTree::new();
        Analyzer { ast, action_tree }
    }

    fn parse_variables(&self, ast_token: Ref<ASTToken>) {
        let token = &ast_token.token;
        match token {
            Token::Expression(expression) => {
                // Check operations for valid types.
                // variable declaration/assignment.
                // function/macro calls with variables (can move/borrow).
            }

            Token::Statement(statement) => {
                // return (need to check borrow/move)
                // yield (need to move the result (could borrow if lifetime is tied to ))
                // throw
            }

            Token::BlockHeader(block_header) => {
                // variable declaration/assignment.
            }
        }
    }
}
