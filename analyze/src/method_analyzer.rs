use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        expr::{Argument, Expression, FuncCall},
        op::{BinOp, BinOperator, Op},
        stmt::Statement,
    },
    util,
    variable_type::{Type, TypeStruct},
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
            Expression::FuncCall(func_call) => {
                for arg in &mut func_call.arguments {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::StructInit(struct_init) => {
                for arg in &mut struct_init.arguments {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::ArrayInit(args, _) => {
                for arg in args {
                    self.analyze_expr(&mut arg.value);
                }
            }
            Expression::Op(op) => match op {
                Op::BinOp(bin_op) => self.analyze_bin_op(bin_op),
                Op::UnOp(un_op) => self.analyze_expr(&mut un_op.value),
            },

            Expression::Lit(..) | Expression::Type(_) | Expression::Var(_) => (),
        }
    }

    /// If this binary operation is a method call (rhs == func_call),
    /// insert the lhs expression ("this"/"self") as a first argument in the
    /// function call. It also saves the name/struct ... of the lhs that this
    /// method "belongs" to.
    fn analyze_bin_op(&mut self, bin_op: &mut BinOp) {
        self.analyze_expr(&mut bin_op.left);
        self.analyze_expr(&mut bin_op.right);

        // TODO: FIXME: For now, it doesn't matter what the lhs is. It will always
        //              be used as the first argument to the function call.
        if let BinOperator::Dot = bin_op.operator {
            if let Some(rhs_method_call) = bin_op.right.eval_to_func_call() {
                if let Some(lhs_type) = self.get_ret_type(&bin_op.left) {
                    let struct_name = match lhs_type.ty {
                        Type::Custom(ref struct_name) => struct_name,
                        _ => panic!("TODO: More lhs type in method call"),
                    };

                    rhs_method_call.name = util::to_method_name(struct_name, &rhs_method_call.name);
                } else {
                    let err_msg = format!(
                        "Unable to get type of lhs in Dot operation. Lhs: {:?}",
                        &bin_op.left,
                    );
                    let err = self.context.err(err_msg);
                    self.errors.push(err);
                    return;
                }

                // TODO: Where should the name of "this"/"self" be specified?
                const THIS_VAR_NAME: &str = "this";
                let arg = Argument::new(Some(THIS_VAR_NAME.into()), *bin_op.left.clone());
                rhs_method_call.arguments.insert(0, arg);
            }
        }
    }

    fn get_ret_type(&self, expr: &Expression) -> Option<TypeStruct> {
        match expr {
            Expression::Lit(_, type_struct_opt) => type_struct_opt.clone(),
            Expression::Type(type_struct) => Some(type_struct.clone()),
            Expression::Var(var) => var.ret_type.clone(),
            Expression::FuncCall(func_call) => panic!("TODO: Get return type from func call."),
            Expression::StructInit(struct_init) => {
                let ty = Type::Custom(struct_init.name.clone());
                Some(TypeStruct::new(ty, None, false))
            }
            Expression::ArrayInit(_, type_struct_opt) => type_struct_opt.clone(),
            Expression::Op(op) => match op {
                Op::BinOp(bin_op) => bin_op.ret_type.clone(),
                Op::UnOp(un_op) => un_op.ret_type.clone(),
            },
        }
    }
}
