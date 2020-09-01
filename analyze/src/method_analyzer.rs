use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        expr::{AccessInstruction, Argument, Expression},
        op::{BinaryOperation, BinaryOperator, Operation},
        stmt::Statement,
    },
    variable_type::Type,
};
use parse::token::{ParseToken, ParseTokenKind};

pub struct MethodAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> MethodAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find method
    /// calls and inserts the correct "this"/"self" into the "func_call"s.
    pub fn analyze(
        context: &'a mut AnalyzeContext,
        ast_root: &mut ParseToken,
    ) -> Result<(), Vec<LangError>> {
        let mut method_analyzer = MethodAnalyzer::new(context);
        method_analyzer.analyze_token(ast_root);

        if method_analyzer.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut method_analyzer.errors))
        }
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self {
            context,
            errors: Vec::default(),
        }
    }

    fn analyze_token(&mut self, token: &mut ParseToken) {
        self.context.cur_line_nr = token.line_nr;
        self.context.cur_column_nr = token.column_nr;

        match &mut token.kind {
            ParseTokenKind::Block(_, id, body) => {
                for token in body {
                    self.context.cur_block_id = *id;
                    self.analyze_token(token);
                }
            }
            ParseTokenKind::Statement(stmt) => self.analyze_stmt(stmt),
            ParseTokenKind::Expression(expr) => self.analyze_expr(expr),
            ParseTokenKind::EndOfFile => (),
        }
    }

    fn analyze_stmt(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Yield(expr) => self.analyze_expr(expr),
            Statement::Defer(expr) => self.analyze_expr(expr),
            Statement::DeferExecution(expr) => self.analyze_expr(expr),
            Statement::Assignment(_, lhs, rhs) => {
                self.analyze_expr(lhs);
                self.analyze_expr(rhs);
            }
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expr(expr);
                }
            }
            Statement::VariableDecl(_, expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expr(expr);
                }
            }

            Statement::Break
            | Statement::Continue
            | Statement::Use(_)
            | Statement::Package(_)
            | Statement::ExternalDecl(_)
            | Statement::Modifier(_) => (),
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expression) {
        match expr {
            Expression::FunctionCall(func_call) => {
                for arg in &mut func_call.arguments {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::StructInit(struct_init) => {
                for arg in &mut struct_init.arguments {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::ArrayInit(args) => {
                for arg in args {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::Operation(op) => match op {
                Operation::BinaryOperation(bin_op) => self.analyze_bin_op(bin_op),
                Operation::UnaryOperation(un_op) => self.analyze_expr(&mut un_op.value),
            },

            Expression::Literal(..) | Expression::Type(_) | Expression::Variable(_) => (),
        }
    }

    /// If this binary operation is a method call (lhs == var && rhs == method),
    /// insert the lhs variable ("this"/"self") as a first argument in the
    /// function call.
    fn analyze_bin_op(&mut self, bin_op: &mut BinaryOperation) {
        self.analyze_expr(&mut bin_op.left);
        self.analyze_expr(&mut bin_op.right);

        if let BinaryOperator::Dot = bin_op.operator {
            if let Some(lhs_var) = bin_op.left.eval_to_var() {
                if let Some(rhs_method_call) = bin_op.right.eval_to_func_call() {
                    if let Some((_, access_instrs)) = &mut rhs_method_call.access_instrs {
                        if let Some(AccessInstruction::StructMethod(ref mut struct_name_opt, _)) =
                            access_instrs.last_mut()
                        {
                            if let Some(Type::Custom(struct_name)) =
                                lhs_var.ret_type.clone().map(|type_struct| type_struct.ty)
                            {
                                *struct_name_opt = Some(struct_name)
                            } else {
                                let err_msg = format!(
                                    "Last access instruction of rhs in method call not \"StructMethod\". \
                                    Left: {:?}, right: {:?}",
                                    bin_op.left, bin_op.right
                                );
                                let err = self.context.err(err_msg);
                                self.errors.push(err);
                                return;
                            }
                        } else {
                            let err_msg = format!(
                                "Last access instruction of rhs in method call not \"StructMethod\". \
                                Left: {:?}, right: {:?}",
                                bin_op.left, bin_op.right
                            );
                            let err = self.context.err(err_msg);
                            self.errors.push(err);
                            return;
                        }

                        // TODO: Where should the name of "this"/"self" be specified?
                        const THIS_VAR_NAME: &str = "this";
                        let arg = Argument::new(Some(THIS_VAR_NAME.into()), *bin_op.left.clone());
                        rhs_method_call.arguments.insert(0, arg);
                    } else {
                        let err_msg = format!(
                            "Rhs method call during Dot has None access instructions set. \
                            Left: {:?}, right: {:?}",
                            bin_op.left, bin_op.right
                        );
                        let err = self.context.err(err_msg);
                        self.errors.push(err);
                        return;
                    }
                }
            } else {
                let err_msg = format!(
                    "Left hand side of Dot symbol didn't eval to variable. \
                    Left: {:?}, right: {:?}",
                    bin_op.left, bin_op.right
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return;
            }
        }
    }
}
