use crate::analyze::analyzer::AnalyzeContext;
use crate::common::variable_type::Type;
use crate::parse::token::{
    BinaryOperation, Operation, ParseToken, TypeStruct, UnaryOperation, Variable,
};
use crate::parse::token::{BlockHeader, Expression, ParseTokenKind, Statement};
use crate::{lex::token::Literal, CustomResult};

pub struct TypeAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

impl<'a> TypeAnalyzer<'a> {
    /// Takes in the root of the AST and tries to infer all the missing types
    /// for variables and expressions (what type the expressions evaluates to).
    pub fn analyze(context: &'a mut AnalyzeContext, ast_root: &mut ParseToken) -> CustomResult<()> {
        let mut type_analyzer = TypeAnalyzer::new(context);
        type_analyzer.analyze_type(ast_root)
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self { context }
    }

    fn analyze_type(&mut self, token: &mut ParseToken) -> CustomResult<()> {
        match token.kind {
            ParseTokenKind::Block(ref mut header, id, ref mut body) => {
                self.context.cur_block_id = id;
                self.analyze_header_type(header)?;
                for token in body {
                    self.analyze_type(token)?;
                }
            }
            ParseTokenKind::Statement(ref mut stmt) => self.analyze_stmt_type(stmt)?,
            ParseTokenKind::Expression(ref mut expr) => self.analyze_expr_type(expr)?,
            ParseTokenKind::EndOfFile => (),
        }

        Ok(())
    }

    fn analyze_expr_type(&mut self, expression: &mut Expression) -> CustomResult<()> {
        match expression {
            Expression::Literal(lit, type_opt) => self.analyze_literal_type(lit, type_opt),
            Expression::Variable(var) => self.parse_variable_type(var),
            Expression::Operation(op) => self.analyze_op_type(op),
            _ => Ok(()),
        }
    }

    fn analyze_expr_type_opt(
        &mut self,
        expression_opt: &mut Option<Expression>,
    ) -> CustomResult<()> {
        if let Some(expression) = expression_opt {
            self.analyze_expr_type(expression)
        } else {
            Ok(())
        }
    }

    // TODO: Placeholder function if this functionallity is ever needed.
    #[allow(unused_variables)]
    fn analyze_literal_type(
        &mut self,
        literal: &Literal,
        l_type: &Option<TypeStruct>,
    ) -> CustomResult<()> {
        // Nothing to do here atm. For bool, char and string literals there are
        // currently no need to look at the type since they are implied.
        // Nothing implemented for the number literals yet, might not be needed
        // at all.
        Ok(())
    }

    // TODO: Placeholder function if this functionallity is ever needed.
    #[allow(unused_variables)]
    fn parse_variable_type(&mut self, variable: &mut Variable) -> CustomResult<()> {
        // This is a variable inside a expression, there is no way to specify
        // the type. The type will be stored in the surrounding "As" expression
        // if a cast is done.
        Ok(())
    }

    fn analyze_op_type(&mut self, operation: &mut Operation) -> CustomResult<()> {
        match operation {
            Operation::BinaryOperation(ref mut bin_op) => self.analyze_bin_op_type(bin_op),
            Operation::UnaryOperation(ref mut un_op) => self.parse_un_op_type(un_op),
        }
    }

    // TODO: This is completly broken. Need to check better. This will panic if the two expressions
    //       of the binary operation have different types, in most of those cases it should not panic.
    fn analyze_bin_op_type(&mut self, bin_op: &mut BinaryOperation) -> CustomResult<()> {
        self.analyze_expr_type(&mut bin_op.left)?;
        self.analyze_expr_type(&mut bin_op.right)?;

        // TODO: If this is a assignment, the type of the right hand side should
        //       be compared to the type of the variable. If the variable doesn't
        //       have a type set but the right has, that type must be cascaded
        //       to the declaration of the variable so that it can be seen during
        //       codegen (since codegen will only look at the declaration of
        //       a variable).
        let left_type = self.get_ret_type(&bin_op.left);
        let right_type = self.get_ret_type(&bin_op.right);

        let inferred_type = self.infer_type(&left_type, &right_type);
        bin_op.ret_type = inferred_type;
        Ok(())
    }

    fn parse_un_op_type(&mut self, un_op: &mut UnaryOperation) -> CustomResult<()> {
        self.analyze_expr_type(&mut un_op.value)?;

        un_op.ret_type = self.get_ret_type(&un_op.value);
        Ok(())
    }

    fn get_ret_type(&self, expr: &Expression) -> Option<TypeStruct> {
        match expr {
            Expression::Literal(Literal::CharLiteral(_), _) => {
                Some(TypeStruct::new(Type::Character, None))
            }
            Expression::Literal(Literal::StringLiteral(_), _) => {
                Some(TypeStruct::new(Type::String, None))
            }
            Expression::Literal(Literal::Bool(_), _) => Some(TypeStruct::new(Type::Boolean, None)),
            Expression::Literal(Literal::Integer(_, _), type_opt) => type_opt.clone(),
            Expression::Literal(Literal::Float(_), type_opt) => type_opt.clone(),
            Expression::Variable(var) => var.ret_type.clone(),
            Expression::FunctionCall(func_call) => {
                let id = self.context.cur_block_id;
                let key = (func_call.name.clone(), id);
                if let Some(func) = self.context.functions.get(&key) {
                    func.ret_type.clone()
                } else {
                    None
                }
            }
            Expression::Operation(Operation::BinaryOperation(bin_op)) => bin_op.ret_type.clone(),
            Expression::Operation(Operation::UnaryOperation(un_op)) => un_op.ret_type.clone(),
        }
    }

    fn analyze_stmt_type(&mut self, statement: &mut Statement) -> CustomResult<()> {
        match statement {
            Statement::Break
            | Statement::Continue
            | Statement::Use(_)
            | Statement::Package(_)
            | Statement::Modifier(_) => Ok(()),

            Statement::Return(return_opt) => self.analyze_expr_type_opt(return_opt),
            Statement::Yield(yield_opt) => self.analyze_expr_type(yield_opt),
            Statement::Assignment(_, _var, expr) => {
                self.analyze_expr_type(expr)?;
                // TODO: Analyes `var` as well (?).
                Ok(())
            }
            Statement::VariableDecl(_var, expr_opt) => {
                self.analyze_expr_type_opt(expr_opt)?;
                // TODO: Analyes `var` as well (?).
                Ok(())
            }
        }
    }

    fn analyze_header_type(&mut self, block_header: &mut BlockHeader) -> CustomResult<()> {
        match block_header {
            BlockHeader::IfCase(expr_opt) => self.analyze_expr_type_opt(expr_opt),
            BlockHeader::Match(expr) => self.analyze_expr_type(expr),
            BlockHeader::MatchCase(expr) => self.analyze_expr_type(expr),
            BlockHeader::While(expr_opt) => self.analyze_expr_type_opt(expr_opt),
            BlockHeader::With(expr) => self.analyze_expr_type(expr),

            BlockHeader::For(var, expr) => {
                // The type of the variable `var` will be infered to the same
                // as the expression if no type is specified in the var itself.
                self.analyze_expr_type(expr)?;
                if var.ret_type.is_none() {
                    var.ret_type = self.get_ret_type(expr);
                }
                Ok(())
            }

            _ => Ok(()),
        }
    }

    // TODO: How should inheritance/implements etc. work?
    fn infer_type(
        &self,
        left_type_opt: &Option<TypeStruct>,
        right_type_opt: &Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        if left_type_opt.is_some() && right_type_opt.is_none() {
            left_type_opt.clone()
        } else if left_type_opt.is_none() && right_type_opt.is_some() {
            right_type_opt.clone()
        } else if left_type_opt.is_some() && right_type_opt.is_some() {
            if left_type_opt == right_type_opt {
                left_type_opt.clone()
            } else {
                None
            }
        } else {
            // Both none.
            None
        }
    }
}
