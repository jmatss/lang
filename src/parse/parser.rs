use super::token::BlockId;
use crate::lex::token::LexToken;
use crate::parse::ast::AST;
use crate::parse::iter::ParseTokenIter;
use crate::CustomResult;

pub fn parse(simple_tokens: Vec<LexToken>) -> CustomResult<AST> {
    iter.parse_abstract_syntax_tree()
}

pub fn parse_abstract_syntax_tree(lex_tokens: Vec<LexToken>) -> CustomResult<AST> {
    let mut block_id: BlockId;
    let mut ast = AST::new();
    let mut iter = ParseTokenIter::new(simple_tokens);

    loop {
        let current = self.next().ok_or_else(|| {
            ParseError("No more simple_tokens in start of parse_ast...".to_string())
        })?;

        if DEBUG {
            println!(
                "Current SimpleToken: {:?}, line_number: {}, indent_level: {}",
                &current, self.line_number, self.indent_level
            );
        }

        match current {
            LexToken::Identifier(identifier) => {
                if DEBUG {
                    println!("Identifier");
                }
                let token = self.parse_identifier(&identifier)?;

                // If BlockHeader: Add this as a new block to the AST.
                // Else if Expression: Parse the rest of the expression.
                // Else: Add this as a token to the AST.
                match &token {
                    ParseToken::Block(_) => {
                        self.ast
                            .insert_block(token, self.line_number, self.indent_level)?
                    }

                    ParseToken::Expression(expression) => {
                        let expression_token =
                            ParseToken::Expression(self.parse_expression_with_previous(
                                stop_on!(default),
                                Some(expression.clone()),
                            )?);
                        self.ast.insert_token(
                            expression_token,
                            self.line_number,
                            self.indent_level,
                        )?
                    }

                    _ => self
                        .ast
                        .insert_token(token, self.line_number, self.indent_level)?,
                }
            }

            LexToken::Symbol(Symbol::LineBreak) => {
                if DEBUG {
                    println!("Linebreak");
                }
                self.update_indent_and_line_number(true)?
            }

            LexToken::Symbol(Symbol::SemiColon) => {
                if DEBUG {
                    println!("Semocolon");
                }
                continue;
            }

            // TODO: Make sure to update indent size when getting LineBreak.
            LexToken::Symbol(_) | LexToken::Number(_) | LexToken::Literal(_) => {
                if DEBUG {
                    println!("Symbol, number or literal.");
                }
                self.rewind();
                let expression = self.parse_expression(stop_on!(default))?;
                let token = ParseToken::Expression(expression);
                self.ast
                    .insert_token(token, self.line_number, self.indent_level)?;
            }

            LexToken::EndOfFile => {
                if DEBUG {
                    println!("EndOfFile");
                }
                return Ok(std::mem::replace(&mut self.ast, AST::new()));
            }

            LexToken::Unknown(unknown) => {
                return Err(ParseError(format!(
                    "Bad string (SimpleToken) during parse_next: {:?}",
                    unknown
                )))
            }
        }
    }
}
