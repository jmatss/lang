use crate::analyzer::action_tree::ActionTree;
use crate::error::CustomError::AnalyzeError;
use crate::parser::abstract_syntax_tree::{ASTToken, AST, RCNode};
use crate::parser::token::Token;
use crate::CustomResult;
use std::cell::Ref;

pub struct Analyzer {
    ast: AST,
    action_tree: ActionTree,
}

pub fn analyze(ast: AST) -> CustomResult<()> {
    let analyzer = Analyzer::new(ast);

    let root_block = analyzer.ast.blocks[0].borrow();

    for child in &root_block.children {
        match child {
            RCNode::Block(ast_block) => {
                let a = ast_block.borrow_mut();
            }
            RCNode::Token(ast_token) =>
        }
        let a = analyzer.parse(child);
    }

    let root_block_data_opt = &ast.blocks[0].borrow().block_data;
    if let Some(block_data) = root_block_data_opt {
        for block in &block_data.children {
            for child in &block_data.children {
                let child_token = &child.borrow().token;

                println!("TOKEN: {:?}", child_token);

                match child_token {
                    Token::BlockHeader(block_header) => {
                        block_header.
                        // TODO: FIXME: Temporary let else be caught in here
                        // since they are special case.
                        let mut result = transpile_block_header(block_header)?;
                        lines.push(result);
                    }
                    Token::Expression(expr) => {
                        let mut result = transpile_expression(expr)?;
                        result.push_str(symbols::EXPR_BRAKE);
                        lines.push(result);
                    }
                    Token::Statement(x) => {
                        println!("{:?}", x);
                    }
                    _ => {}
                }
            }

            lines.push(symbols::BLOCK_END.to_string());
        }
    } else {
        panic!("NO CHILDREN IN ROOT_BLOCK.");
    }

    Ok(())
}

impl Analyzer {
    pub fn new(ast: AST) -> Self {
        let action_tree = ActionTree::new();
        Analyzer { ast, action_tree }
    }

    fn parse(&self, ast_token: Ref<ASTToken>) {
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
