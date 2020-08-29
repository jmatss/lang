use crate::analyze::analyzer::AnalyzeContext;
use crate::error::LangError;
use crate::parse::token::{BlockId, Expression, ParseToken, ParseTokenKind, Statement};
use std::collections::HashMap;

pub struct DeferAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,

    /// Contains defer-statements for a specific block. Expressions will be added
    /// to this map continuously during codegen when the statement is seen. This
    /// means that all defer-statements for a specific block might not be in this
    /// map at a certain point during the codegen of this block.
    pub defer_statements: HashMap<BlockId, Vec<Expression>>,
}

impl<'a> DeferAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find defer
    /// statements and then inserts new blocks in the AST where the deferred
    /// expression(s) should be executed.
    pub fn analyze(
        context: &'a mut AnalyzeContext,
        ast_root: &mut ParseToken,
    ) -> Result<(), Vec<LangError>> {
        let mut defer_analyzer = DeferAnalyzer::new(context);
        defer_analyzer.analyze_block(ast_root);

        if defer_analyzer.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut defer_analyzer.errors))
        }
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self {
            context,
            errors: Vec::default(),
            defer_statements: HashMap::default(),
        }
    }

    fn analyze_block(&mut self, token: &mut ParseToken) {
        self.context.cur_line_nr = token.line_nr;
        self.context.cur_column_nr = token.column_nr;

        match &mut token.kind {
            ParseTokenKind::Block(_, id, body) => {
                let mut i = 0;
                while let Some(token) = body.get_mut(i) {
                    self.context.cur_block_id = *id;

                    match &mut token.kind {
                        ParseTokenKind::Block(..) => self.analyze_block(token),
                        ParseTokenKind::Statement(stmt) => match stmt {
                            // If a defer statement is found, inser it into the
                            // `defer_statement` map. Since this is update
                            // continuously during this analyzing, all defers
                            // might not exists in the map when a "DefereExecution"
                            // is added (which is the expected behaviour).
                            Statement::Defer(expr) => {
                                if let Some(defer_vec) = self.defer_statements.get_mut(&id) {
                                    defer_vec.push(expr.clone());
                                } else {
                                    let mut defer_vec = Vec::new();
                                    defer_vec.push(expr.clone());
                                    self.defer_statements.insert(*id, defer_vec);
                                }
                            }

                            // TODO: Only get all defers all the way up to the
                            //       function.
                            Statement::Return(_) => {
                                if let Some(defers) = self.get_all_defers(*id) {
                                    for expr in defers.iter() {
                                        warn!("Return - expr: {:#?}", expr);
                                        let kind = ParseTokenKind::Statement(
                                            Statement::DeferExecution(expr.clone()),
                                        );
                                        body.insert(i, ParseToken::new(kind, 0, 0));
                                        i += 1;
                                    }
                                }
                            }

                            Statement::Yield(_) | Statement::Break | Statement::Continue => {
                                if let Some(defers) = self.get_branchable_defers(*id) {
                                    for expr in defers.iter() {
                                        warn!("Yield/Break/Continue - expr: {:#?}", expr);
                                        let kind = ParseTokenKind::Statement(
                                            Statement::DeferExecution(expr.clone()),
                                        );
                                        body.insert(i, ParseToken::new(kind, 0, 0));
                                        i += 1;
                                    }
                                }
                            }

                            Statement::Use(_)
                            | Statement::Package(_)
                            | Statement::DeferExecution(_)
                            | Statement::Assignment(_, _, _)
                            | Statement::VariableDecl(_, _)
                            | Statement::ExternalDecl(_)
                            | Statement::Modifier(_) => (),
                        },
                        ParseTokenKind::Expression(_) | ParseTokenKind::EndOfFile => (),
                    }

                    i += 1;
                }

                // If there are some defers for this block and all paths leading
                // to the end of this block doesn't branch away, add
                // the defers to the end of this block.
                if let Some(block_info) = self.context.block_info.get(&id) {
                    if !(block_info.all_children_contains_returns
                        || block_info.contains_return
                        || block_info.contains_break
                        || block_info.contains_continue
                        || block_info.contains_yield)
                    {
                        if let Some(defers) = self.get_block_defers(*id) {
                            for expr in defers.iter() {
                                warn!("Block end - expr: {:#?}", expr);
                                let kind = ParseTokenKind::Statement(Statement::DeferExecution(
                                    expr.clone(),
                                ));
                                body.push(ParseToken::new(kind, 0, 0));
                            }
                        }
                    }
                }
            }

            ParseTokenKind::Statement(_)
            | ParseTokenKind::Expression(_)
            | ParseTokenKind::EndOfFile => {
                self.errors.push(self.context.err(format!(
                    "Got unexpected token when expecting block during defer analyzing: {:?}",
                    token
                )));
            }
        }
    }

    /// Given a block ID `id`, returns every deferred expression for this block.
    fn get_block_defers(&mut self, id: BlockId) -> Option<Vec<Expression>> {
        let mut defers = Vec::new();
        if let Some(cur_defers) = self.defer_statements.get(&id) {
            for defer in cur_defers.iter().rev() {
                defers.push(defer.clone());
            }
        }

        if !defers.is_empty() {
            Some(defers)
        } else {
            None
        }
    }

    /// Given a block ID `id`, returns every deferred expression for this block
    /// AND all its parent blocks.
    fn get_all_defers(&mut self, id: BlockId) -> Option<Vec<Expression>> {
        let mut defers = Vec::new();
        let mut cur_id = id;
        while let Some(cur_block_info) = self.context.block_info.get(&cur_id) {
            if let Some(cur_defers) = self.defer_statements.get(&cur_id) {
                for defer in cur_defers.iter().rev() {
                    defers.push(defer.clone());
                }
            }
            cur_id = cur_block_info.parent_id;
        }

        if !defers.is_empty() {
            Some(defers)
        } else {
            None
        }
    }

    /// Given a block ID `id`, returns every deferred expression for this block
    /// and all parent blocks up to the first "branchable" block (ex. "while"
    /// and "for" blocks).
    fn get_branchable_defers(&mut self, id: BlockId) -> Option<Vec<Expression>> {
        let mut defers = Vec::new();
        let mut cur_id = id;
        while let Some(cur_block_info) = self.context.block_info.get(&cur_id) {
            if let Some(cur_defers) = self.defer_statements.get(&cur_id) {
                for defer in cur_defers.iter().rev() {
                    defers.push(defer.clone());
                }
            }
            cur_id = cur_block_info.parent_id;

            if cur_block_info.is_branchable_block {
                break;
            }
        }

        if !defers.is_empty() {
            Some(defers)
        } else {
            None
        }
    }
}
