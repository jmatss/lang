use crate::analyze::analyzer::AnalyzeContext;
use crate::parse::token::{
    BinaryOperation, BinaryOperator, Expression, FunctionCall, Operation, ParseToken,
    ParseTokenKind, Statement, StructInit,
};

// TODO: Add array indexing here.
// TODO: Is it possible to convert the indexing with a ident to a number?
//       Ex. if "x" is the second member of a struct, it would be converted
//       to "1", or at least get it as a attribute. This is currently done in
//       in "TypeAnalyzer" but should probably be done here if possible.

pub struct IndexingAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

impl<'a> IndexingAnalyzer<'a> {
    /// Takes in a the root of the AST and walks the whole tree to find information
    /// related to indexing of variables. This includes array indexing and also
    /// struct indexing.
    pub fn analyze(context: &'a mut AnalyzeContext, ast_root: &mut ParseToken) {
        let mut block_analyzer = IndexingAnalyzer::new(context);
        block_analyzer.analyze_indexing(ast_root);
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        // Reset the `cur_block_id` to the default block (== 0).
        context.cur_block_id = 0;
        Self { context }
    }

    fn analyze_indexing(&mut self, token: &mut ParseToken) {
        match token.kind {
            ParseTokenKind::Block(_, _, ref mut body) => {
                for token in body {
                    self.analyze_indexing(token);
                }
            }
            ParseTokenKind::Statement(ref mut stmt) => self.analyze_stmt(stmt),
            ParseTokenKind::Expression(ref mut expr) => self.analyze_expr(expr),
            ParseTokenKind::EndOfFile => (),
        }
    }

    fn analyze_stmt(&mut self, stmt: &mut Statement) {
        match stmt {
            Statement::Yield(expr) => self.analyze_expr(expr),
            Statement::With(expr) => self.analyze_expr(expr),
            Statement::Defer(expr) => self.analyze_expr(expr),
            Statement::Assignment(_, _, expr) => self.analyze_expr(expr),
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expr(expr)
                }
            }
            Statement::VariableDecl(_, expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.analyze_expr(expr)
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
            Expression::FunctionCall(func_call) => self.analyze_func_call(func_call),
            Expression::StructInit(struct_init) => self.analyze_struct_init(struct_init),
            Expression::Operation(op) => self.analyze_op(op),

            Expression::Literal(_, _) | Expression::Type(_) | Expression::Variable(_) => (),
        }
    }

    fn analyze_func_call(&mut self, func_call: &mut FunctionCall) {
        for arg in func_call.arguments.iter_mut() {
            self.analyze_expr(&mut arg.value);
        }
    }

    fn analyze_struct_init(&mut self, struct_init: &mut StructInit) {
        for arg in struct_init.arguments.iter_mut() {
            self.analyze_expr(&mut arg.value);
        }
    }

    fn analyze_op(&mut self, op: &mut Operation) {
        match op {
            Operation::BinaryOperation(bin_op) => self.analyze_bin_op(bin_op),
            Operation::UnaryOperation(un_op) => self.analyze_expr(&mut un_op.value),
        }
    }

    /// Set any variables that are used to index as struct members.
    fn analyze_bin_op(&mut self, bin_op: &mut BinaryOperation) {
        self.analyze_expr(&mut bin_op.left);
        self.analyze_expr(&mut bin_op.right);
        match bin_op.operator {
            BinaryOperator::Dot => {
                if let Expression::Variable(ref mut var) = *bin_op.right {
                    var.is_struct_member = true;
                }
            }
            _ => (),
        }
    }
}
