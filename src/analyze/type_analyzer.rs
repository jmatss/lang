use super::type_inference::TypeChoice;
use crate::analyze::analyzer::AnalyzeContext;
use crate::common::variable_type::Type;
use crate::error::LangError;
use crate::error::LangErrorKind::AnalyzeError;
use crate::parse::token::{
    BinaryOperation, Operation, ParseToken, TypeStruct, UnaryOperation, Variable,
};
use crate::parse::token::{
    BinaryOperator, BlockHeader, Expression, FunctionCall, ParseTokenKind, Statement, StructInit,
    UnaryOperator,
};
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
                // If this is a struct member, it will be at the right hand side
                // of a "Dot" binary operation. It that case the `prev_type_opt`
                // will come from the left hand side and it will be the type of
                // the struct. Look up the struct and see what type this struct
                // member has.
                if var.is_struct_member {
                    if let Some(struct_type) = prev_type_opt.clone() {
                        self.analyze_struct_member(var, struct_type)
                    } else {
                        Err(LangError::new(
                            format!(
                                "prev_type was None when looking at struct member: {}",
                                &var.name
                            ),
                            AnalyzeError,
                        ))
                    }
                } else {
                    self.analyze_var_type(var)
                }
            }
            Expression::Operation(op) => self.analyze_op_type(op, prev_type_opt),
            Expression::FunctionCall(func_call) => self.analyze_func_call(func_call),
            Expression::StructInit(struct_init) => self.analyze_struct_init(struct_init),
        }
    }

    fn analyze_func_call(
        &mut self,
        func_call: &mut FunctionCall,
    ) -> CustomResult<Option<TypeStruct>> {
        let cur_block_id = self.context.cur_block_id;
        let func_block_id = self
            .context
            .get_func_parent_id(func_call.name.clone(), cur_block_id)?;

        // Get the function from the map parsed during "decl analyzing".
        let key = (func_call.name.clone(), func_block_id);
        let func = if let Some(func) = self.context.functions.get(&key) {
            func.clone()
        } else {
            return Err(LangError::new(
                format!("Unable to find func decl for: {}", &func_call.name),
                AnalyzeError,
            ));
        };

        // Analyze all arguments of the function call and make sure that
        // the amount of types of the args/params are the same. They can
        // differ if this function is variadic.
        if let Some(func_params) = func.parameters {
            if (func_call.arguments.len() == func_params.len() && !func.is_var_arg)
                || (func_call.arguments.len() >= func_params.len() && func.is_var_arg)
            {
                for (i, arg) in func_call.arguments.iter_mut().enumerate() {
                    let prev_type_opt = if i < func_params.len() {
                        func_params
                            .get(i)
                            .expect("More args than params.")
                            .ret_type
                            .clone()
                    } else {
                        None
                    };
                    self.analyze_expr_type(&mut arg.value, prev_type_opt)?;
                }
            } else {
                return Err(LangError::new(
                            format!(
                                "Func/call to {}, incorrect amount of param/arg (vararg={}). Actual func #: {}, got: {}.",
                                &func_call.name,
                                func.is_var_arg,
                                func_params.len(),
                                func_call.arguments.len()
                            ),
                            AnalyzeError,
                        ));
            }
        } else if !func_call.arguments.is_empty() {
            return Err(LangError::new(
                format!(
                    "Func {} has no params, but func_call had {} args.",
                    &func_call.name,
                    func_call.arguments.len()
                ),
                AnalyzeError,
            ));
        }

        // Always return the type of the actual function.
        Ok(func.ret_type.clone())
    }

    fn analyze_struct_init(
        &mut self,
        struct_init: &mut StructInit,
    ) -> CustomResult<Option<TypeStruct>> {
        let cur_block_id = self.context.cur_block_id;
        let struct_block_id = self
            .context
            .get_struct_parent_id(struct_init.name.clone(), cur_block_id)?;

        // Get the struct from the map parsed during "decl analyzing".
        let key = (struct_init.name.clone(), struct_block_id);
        let struct_ = if let Some(struct_) = self.context.structs.get(&key) {
            struct_.clone()
        } else {
            return Err(LangError::new(
                format!("Unable to find struct decl for: {}", &struct_init.name),
                AnalyzeError,
            ));
        };

        // Analyze all arguments of the struct init and make sure that
        // the amount of types of the args/members are the same.
        // This is done by using the members type as the `prev_type_opt`.
        if let Some(struct_members) = struct_.members {
            if struct_init.arguments.len() == struct_members.len() {
                for (i, arg) in struct_init.arguments.iter_mut().enumerate() {
                    let member = struct_members.get(i).expect("member/arg len diff.");
                    self.analyze_expr_type(&mut arg.value, member.ret_type.clone())?;
                }
            } else {
                return Err(LangError::new(
                    format!(
                        "Struct/init to {}, members/args amount differs. Expected: {}, got: {}.",
                        &struct_init.name,
                        struct_members.len(),
                        struct_init.arguments.len()
                    ),
                    AnalyzeError,
                ));
            }
        } else if !struct_init.arguments.is_empty() {
            return Err(LangError::new(
                format!(
                    "Struct {} has no params, but struct_init had {} args.",
                    &struct_init.name,
                    struct_init.arguments.len()
                ),
                AnalyzeError,
            ));
        }

        // Return the type of the  struct.
        let ty = Type::Custom(struct_init.name.clone());
        let generics = None;
        Ok(Some(TypeStruct::new(ty, generics)))
    }

    // TODO: This function also sets the `member_index` and `parent_key `for the
    //       member. This should be idealy be moved to "indexing_analyzer" if
    //       possible, it is unrelated to "type analyzing" but this was the
    //       easiest place to implement it in.
    /// This function is called when a "Dot" binary operator has been found
    /// with a "Variable" at the right hand side. This means that this is a
    /// indexing of a struct. Try to figure out the type of the member
    /// by looking up the struct, and then finding the type of the member
    /// in the struct definition.
    fn analyze_struct_member(
        &mut self,
        var: &mut Variable,
        prev_type: TypeStruct,
    ) -> CustomResult<Option<TypeStruct>> {
        if let Type::Custom(ref ident) = prev_type.t {
            let struct_block_id = self.context.cur_block_id;
            let parent_block_id = self
                .context
                .get_struct_parent_id(ident.clone(), struct_block_id)?;

            let key = (ident.clone(), parent_block_id);
            let struct_ = self.context.structs.get(&key).ok_or_else(|| {
                LangError::new(
                    format!(
                        "Unable to find struct with name {} with block ID {}.",
                        &ident, struct_block_id
                    ),
                    AnalyzeError,
                )
            })?;

            // Loop through the struct members and see if a member with the same
            // name can be found. If a member with the same name is found, this
            // is a instance of that member. Set to the same type and set the
            // member index.
            if let Some(members) = &struct_.members {
                let mut found = false;
                for (i, member) in members.iter().enumerate() {
                    if member.name == var.name {
                        var.member_index = i as u32;
                        var.ret_type = member.ret_type.clone();
                        var.struct_name = Some(ident.clone());
                        var.modifiers = member.modifiers.clone();
                        var.is_const = member.is_const;
                        found = true;
                        break;
                    }
                }

                if found {
                    Ok(var.ret_type.clone())
                } else {
                    Err(LangError::new(
                        format!(
                            "Unable to find member with name {} in struct with name {} with block ID {}.",
                            &var.name, &ident, struct_block_id
                        ),
                        AnalyzeError,
                    ))
                }
            } else {
                Err(LangError::new(
                    format!(
                        "Unable to find struct with name {} with block ID {}.",
                        &ident, struct_block_id
                    ),
                    AnalyzeError,
                ))
            }
        } else {
            Err(LangError::new(
                format!(
                    "prev_type was not custom when looking at struct member: {}",
                    &var.name
                ),
                AnalyzeError,
            ))
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
            } else if var.ret_type.is_some() {
                e.ret_type = var.ret_type.clone();
            }

            // Update the fields that are set during declaration.
            var.modifiers = e.modifiers.clone();
            var.is_const = e.is_const;
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
        // If the left has a type set, use that as the "prev_type" when looking
        // at the type for the right side.
        let left_type = self.analyze_expr_type(&mut bin_op.left, prev_type_opt.clone())?;
        let left_prev_type_opt = if left_type.is_some() {
            left_type.clone()
        } else {
            prev_type_opt
        };
        let right_type = self.analyze_expr_type(&mut bin_op.right, left_prev_type_opt)?;

        let inferred_type = self.infer_type(bin_op, &left_type, &right_type);
        bin_op.ret_type = inferred_type.clone();
        Ok(inferred_type)
    }

    fn analyze_un_op_type(
        &mut self,
        un_op: &mut UnaryOperation,
        prev_type_opt: Option<TypeStruct>,
    ) -> CustomResult<Option<TypeStruct>> {
        let ret_type = match un_op.operator {
            UnaryOperator::Deref => {
                // Analyze the inner value. Then dereference the result to get
                // the value that is inside the pointer.
                if let Some(type_struct) =
                    self.analyze_expr_type(&mut un_op.value, prev_type_opt)?
                {
                    if let Type::Pointer(inner) = type_struct.t {
                        Some(*inner)
                    } else {
                        return Err(LangError::new(
                            format!("Trying to dereference non pointer type: {:?}", type_struct),
                            AnalyzeError,
                        ));
                    }
                } else {
                    return Err(LangError::new(
                        "Type set to None when dereferencing.".into(),
                        AnalyzeError,
                    ));
                }
            }
            // TODO: Address
            UnaryOperator::Address => self.analyze_expr_type(&mut un_op.value, prev_type_opt)?,

            UnaryOperator::Increment
            | UnaryOperator::Decrement
            | UnaryOperator::Positive
            | UnaryOperator::Negative
            | UnaryOperator::BitComplement
            | UnaryOperator::BoolNot => self.analyze_expr_type(&mut un_op.value, prev_type_opt)?,
        };
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
                BinaryOperator::In => left_type,
                BinaryOperator::Is => right_type,
                BinaryOperator::As => right_type,
                BinaryOperator::Of => right_type,
                BinaryOperator::Dot => right_type,
                BinaryOperator::ShiftLeft => left_type,
                BinaryOperator::ShiftRight => left_type,
                _ => {
                    // The "compare" function will try and promote values and take the
                    // one with the "higesht priority". Returns None if unable to compare
                    // the types.
                    if let Some(type_choice) = self.compare_type(&left_type, &right_type) {
                        match type_choice {
                            TypeChoice::First => {
                                self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
                                left_type
                            }
                            TypeChoice::Second => {
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

            // Can't set type for function call or struct init.
            Expression::FunctionCall(_) | Expression::StructInit(_) => (),
            Expression::Type(_) => panic!("set_type for Type: {:?}", new_ty),
        }
    }
}
