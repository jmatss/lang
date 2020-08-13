use crate::analyze::analyzer::AnalyzeContext;
use crate::common::variable_type::Type;
use crate::parse::token::{
    BinaryOperation, Operation, ParseToken, TypeStruct, UnaryOperation, Variable,
};
use crate::parse::token::{BinaryOperator, BlockHeader, Expression, ParseTokenKind, Statement};
use crate::{lex::token::Literal, CustomResult};

pub struct TypeAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
}

// TODO: Infer the types if it can be figured out.
//       Should probably use the first type found as the type for the whole
//       variable.

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
            ParseTokenKind::Statement(ref mut stmt) => {
                self.analyze_stmt_type(stmt)?;
            }
            ParseTokenKind::Expression(ref mut expr) => {
                self.analyze_expr_type(expr, None)?;
            }
            ParseTokenKind::EndOfFile => (),
        }

        Ok(())
    }

    /// The `prev_type` is a type that might have been found from a parent and
    /// should be cascaded down the expression and be set for them as well.
    fn analyze_expr_type(
        &mut self,
        expr: &mut Expression,
        prev_type_opt: Option<TypeStruct>,
    ) -> CustomResult<Option<TypeStruct>> {
        match expr {
            Expression::Literal(lit, old_type_opt) => {
                // If a type is already set and has been given as a argument to
                // this function, use that instead of the default for literals.
                Ok(if let Some(prev_type) = prev_type_opt {
                    *old_type_opt = Some(prev_type);
                    old_type_opt.clone()
                } else {
                    let new_type_opt = Some(self.analyze_literal_type(lit)?);
                    *old_type_opt = new_type_opt;
                    old_type_opt.clone()
                })
            }
            Expression::Type(type_struct) => Ok(Some(type_struct.clone())),
            Expression::Variable(var) => {
                // If a previous type is set, replace it with the one in `var`.
                // The "analyze_var_type" will just go one function call deep,
                // so there is no need to update the "prev_type_opt", it will
                // stop being used after this call.
                if let Some(prev_type) = prev_type_opt {
                    var.ret_type = Some(prev_type);
                }
                self.analyze_var_type(var)
            }
            Expression::Operation(op) => self.analyze_op_type(op, prev_type_opt),
            Expression::FunctionCall(func_call) => {
                let cur_block_id = self.context.cur_block_id;
                let func_block_id = self
                    .context
                    .get_func_parent_id(func_call.name.clone(), cur_block_id)?;

                // Analyze all arguments of the function call.
                for arg in &mut func_call.arguments {
                    self.analyze_expr_type(&mut arg.value, None)?;
                }

                // Always use the type from the function, it doesn't matter
                // if it is None since there is no way to get a type from a
                // function call.
                let key = (func_call.name.clone(), func_block_id);
                if let Some(a) = self.context.functions.get(&key) {
                    Ok(a.ret_type.clone())
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn analyze_expr_type_opt(
        &mut self,
        expr_opt: &mut Option<Expression>,
        prev_type_opt: Option<TypeStruct>,
    ) -> CustomResult<Option<TypeStruct>> {
        if let Some(expr) = expr_opt {
            self.analyze_expr_type(expr, prev_type_opt)
        } else {
            Ok(None)
        }
    }

    fn analyze_literal_type(&mut self, lit: &Literal) -> CustomResult<TypeStruct> {
        // TODO: Implement generics.
        let generics = None;

        Ok(match lit {
            Literal::StringLiteral(_) => TypeStruct::new(Type::String, generics),
            Literal::CharLiteral(_) => TypeStruct::new(Type::Character, generics),
            Literal::Bool(_) => TypeStruct::new(Type::Boolean, generics),
            // Default type for int are Int.
            Literal::Integer(_, _) => TypeStruct::new(Type::Int, generics),
            // Default type for float are F32.
            Literal::Float(_) => TypeStruct::new(Type::F32, generics),
        })
    }

    fn analyze_var_type(&mut self, var: &mut Variable) -> CustomResult<Option<TypeStruct>> {
        let cur_block_id = self.context.cur_block_id;
        let var_decl_id = self.context.get_var_decl_scope(&var.name, cur_block_id)?;

        // TODO: Compare types from the current `var` and the var in context?
        // If the variable in `context` has a type, use that type. Otherwise
        // use the current `var` type and update the type in `context` with it.
        let key = (var.name.clone(), var_decl_id);
        self.context.variables.entry(key.clone()).and_modify(|e| {
            if e.ret_type.is_some() {
                var.ret_type = e.ret_type.clone();
            } else {
                e.ret_type = var.ret_type.clone();
            }
        });

        if var.ret_type.is_none() {
            if let Some(context_var) = self.context.variables.get(&key) {
                Ok(context_var.ret_type.clone())
            } else {
                Ok(None)
            }
        } else {
            Ok(var.ret_type.clone())
        }
    }

    fn analyze_op_type(
        &mut self,
        op: &mut Operation,
        prev_type_opt: Option<TypeStruct>,
    ) -> CustomResult<Option<TypeStruct>> {
        match op {
            Operation::BinaryOperation(ref mut bin_op) => {
                self.analyze_bin_op_type(bin_op, prev_type_opt)
            }
            Operation::UnaryOperation(ref mut un_op) => {
                self.analyze_un_op_type(un_op, prev_type_opt)
            }
        }
    }

    // TODO: This is completly broken. Need to check better. This will panic if the two expressions
    //       of the binary operation have different types, in most of those cases it should not panic.
    fn analyze_bin_op_type(
        &mut self,
        bin_op: &mut BinaryOperation,
        prev_type_opt: Option<TypeStruct>,
    ) -> CustomResult<Option<TypeStruct>> {
        // TODO: If this is a assignment, the type of the right hand side should
        //       be compared to the type of the variable. If the variable doesn't
        //       have a type set but the right has, that type must be cascaded
        //       to the declaration of the variable so that it can be seen during
        //       codegen (since codegen will only look at the declaration of
        //       a variable).
        let left_type = self.analyze_expr_type(&mut bin_op.left, prev_type_opt.clone())?;
        let right_type = self.analyze_expr_type(&mut bin_op.right, prev_type_opt)?;

        let inferred_type = self.infer_type(bin_op, &left_type, &right_type);
        bin_op.ret_type = inferred_type.clone();
        Ok(inferred_type)
    }

    fn analyze_un_op_type(
        &mut self,
        un_op: &mut UnaryOperation,
        prev_type_opt: Option<TypeStruct>,
    ) -> CustomResult<Option<TypeStruct>> {
        let ret_type = self.analyze_expr_type(&mut un_op.value, prev_type_opt)?;
        un_op.ret_type = ret_type.clone();
        Ok(ret_type)
    }

    fn analyze_stmt_type(&mut self, statement: &mut Statement) -> CustomResult<()> {
        match statement {
            Statement::Break
            | Statement::Continue
            | Statement::Use(_)
            | Statement::Package(_)
            | Statement::Modifier(_)
            | Statement::ExternalDecl(_) => Ok(()),

            Statement::With(expr) => {
                self.analyze_expr_type(expr, None)?;
                Ok(())
            }
            Statement::Defer(expr) => {
                self.analyze_expr_type(expr, None)?;
                Ok(())
            }
            Statement::Return(expr_opt) => {
                self.analyze_expr_type_opt(expr_opt, None)?;
                Ok(())
            }
            Statement::Yield(expr) => {
                self.analyze_expr_type(expr, None)?;
                Ok(())
            }
            Statement::Assignment(_, var, expr) => {
                // TODO: Should this check so that the left and right hand side
                //       have the same type (or just are compatible?).
                let expr_type = self.analyze_expr_type(expr, var.ret_type.clone())?;

                // Update the variable type if it is None.
                if var.ret_type.is_none() {
                    var.ret_type = expr_type.clone();
                }

                // Update the type of the variable in the "AnalyzeContext"
                // if the type is None.
                let cur_block_id = self.context.cur_block_id;
                let var_decl_id = self.context.get_var_decl_scope(&var.name, cur_block_id)?;
                let key = (var.name.clone(), var_decl_id);
                self.context.variables.entry(key).and_modify(|e| {
                    if e.ret_type.is_none() {
                        e.ret_type = expr_type.clone()
                    }
                });

                Ok(())
            }
            Statement::VariableDecl(var, expr_opt) => {
                // TODO: Should this check so that the left and right hand side
                //       have the same type (or just are compatible?).
                // If a type can be found for the right hand size, set the left
                // side variable  to have the same type (if the variable doesn't
                // already have a pre-defined type).
                let expr_type = self.analyze_expr_type_opt(expr_opt, var.ret_type.clone())?;

                // Update the variable type if it is None.
                if var.ret_type.is_none() {
                    var.ret_type = expr_type.clone();
                }

                // Update the type of the variable in the "AnalyzeContext"
                // if the type is None.
                let cur_block_id = self.context.cur_block_id;
                let var_decl_id = self.context.get_var_decl_scope(&var.name, cur_block_id)?;
                let key = (var.name.clone(), var_decl_id);
                self.context.variables.entry(key).and_modify(|e| {
                    if e.ret_type.is_none() {
                        e.ret_type = expr_type.clone()
                    }
                });

                Ok(())
            }
        }
    }

    fn analyze_header_type(&mut self, block_header: &mut BlockHeader) -> CustomResult<()> {
        match block_header {
            BlockHeader::IfCase(expr_opt) => {
                self.analyze_expr_type_opt(expr_opt, None)?;
            }
            BlockHeader::Match(expr) => {
                self.analyze_expr_type(expr, None)?;
            }
            BlockHeader::MatchCase(expr) => {
                self.analyze_expr_type(expr, None)?;
            }
            BlockHeader::While(expr_opt) => {
                self.analyze_expr_type_opt(expr_opt, None)?;
            }

            BlockHeader::For(var, expr) => {
                // The type of the variable `var` will be infered to the same
                // as the expression if no type is specified in the var itself.
                let ret_type = self.analyze_expr_type(expr, var.ret_type.clone())?;
                if var.ret_type.is_none() {
                    var.ret_type = ret_type;
                }
            }

            BlockHeader::Default
            | BlockHeader::Function(_)
            | BlockHeader::Struct(_)
            | BlockHeader::Enum(_)
            | BlockHeader::Interface(_)
            | BlockHeader::If
            | BlockHeader::Test(_) => (),
        }

        Ok(())
    }

    // TODO: How should inheritance/implements etc. work?
    fn infer_type(
        &self,
        bin_op: &mut BinaryOperation,
        left_type_opt: &Option<TypeStruct>,
        right_type_opt: &Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        if left_type_opt.is_some() && right_type_opt.is_none() {
            self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
            left_type_opt.clone()
        } else if left_type_opt.is_none() && right_type_opt.is_some() {
            self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
            right_type_opt.clone()
        } else if left_type_opt.is_some() && right_type_opt.is_some() {
            let left_type = left_type_opt.clone().expect("left type None");
            let right_type = right_type_opt.clone().expect("right type None");

            // TODO: Generics.
            // Match and see if this is a binary operation where one of the
            // sides should be prefered. Otherwise have a look at the both
            // types and try to figure out which one to prefer.
            Some(match bin_op.operator {
                BinaryOperator::In => {
                    self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
                    left_type
                }
                BinaryOperator::Is => {
                    self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
                    right_type
                }
                BinaryOperator::As => {
                    self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
                    right_type
                }
                BinaryOperator::Of => {
                    self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
                    right_type
                }
                BinaryOperator::Dot => {
                    self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
                    right_type
                }
                BinaryOperator::ShiftLeft => {
                    self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
                    left_type
                }
                BinaryOperator::ShiftRight => {
                    self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
                    left_type
                }
                _ => {
                    // The "compare" function will try and promote values and take the
                    // one with the "higesht priority". Returns None if unable to compare
                    // the types.
                    if let Some(type_choice) = left_type.compare(&right_type) {
                        match type_choice {
                            crate::parse::token::TypeChoice::This => {
                                self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
                                left_type
                            }
                            crate::parse::token::TypeChoice::Other => {
                                self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
                                right_type
                            }
                        }
                    } else {
                        // TODO: Arbitrary choice of left_type, just take one.
                        left_type
                    }
                }
            })
        } else {
            // Both none.
            None
        }
    }

    /// Set the type of a expression after it has ben infered.
    fn set_type(&self, expr: &mut Expression, new_ty: Option<TypeStruct>) {
        match expr {
            Expression::Literal(_, old_ty) => *old_ty = new_ty,
            Expression::Variable(var) => var.ret_type = new_ty,
            Expression::Operation(op) => match op {
                Operation::BinaryOperation(bin_op) => bin_op.ret_type = new_ty,
                Operation::UnaryOperation(un_op) => un_op.ret_type = new_ty,
            },

            // Can't set type for function call.
            Expression::FunctionCall(_) => (),
            Expression::Type(_) => panic!("set_type for Type: {:?}", new_ty),
        }
    }
}
