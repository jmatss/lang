use super::generator::CodeGen;
use crate::{
    common::variable_type::Type,
    parse::token::{
        AccessType, AssignOperator, Expression, Modifier, Operation, Path, Statement, TypeStruct,
    },
    CustomResult,
};
use inkwell::{module::Linkage, types::AnyTypeEnum, values::InstructionValue};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_stmt(&mut self, stmt: &mut Statement) -> CustomResult<()> {
        match stmt {
            Statement::Return(expr_opt) => self.compile_return(expr_opt),
            Statement::Yield(expr) => self.compile_yield(expr),
            Statement::Break => self.compile_break(),
            Statement::Continue => self.compile_continue(),
            Statement::Use(path) => self.compile_use(path),
            Statement::Package(path) => self.compile_package(path),
            Statement::Modifier(modifier) => self.compile_modifier(modifier),
            Statement::With(expr) => self.compile_with(expr),
            Statement::Defer(expr) => self.compile_defer(expr),
            Statement::VariableDecl(var, expr_opt) => {
                self.compile_var_decl(var)?;
                if let Some(expr) = expr_opt {
                    let any_value = self.compile_expr(expr)?;
                    let basic_value = CodeGen::any_into_basic_value(any_value)?;
                    // TODO: Will this always be regular?
                    self.compile_var_store(var, basic_value)?;
                } else if var.is_const {
                    return Err(self.err(format!(
                        "const var decl of \"{}\" has no value set",
                        &var.name
                    )));
                }
                Ok(())
            }
            // TODO: Add other external declares other than func (var, struct etc.)
            Statement::ExternalDecl(func) => {
                let linkage = Linkage::External;
                self.compile_func_proto(func, Some(linkage))?;
                Ok(())
            }
            Statement::Assignment(assign_op, lhs, rhs) => {
                self.compile_assign(assign_op, lhs, rhs)?;
                Ok(())
            }
        }
    }

    fn compile_return(&mut self, expr_opt: &mut Option<Expression>) -> CustomResult<()> {
        if let Some(expr) = expr_opt {
            let any_value = self.compile_expr(expr)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            self.builder.build_return(Some(&basic_value));
        } else {
            self.builder.build_return(None);
        }
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Expression) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"yield\" statement.".into()))
    }

    fn compile_break(&mut self) -> CustomResult<()> {
        // TODO: Is it always OK to use `self.state.cur_block_id` here?
        let id = self.cur_block_id;
        let merge_block = self.get_merge_block(id)?;
        self.builder.build_unconditional_branch(merge_block);
        Ok(())
    }

    fn compile_continue(&mut self) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"continue\" statement.".into()))
    }

    fn compile_use(&mut self, path: &Path) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"use\" statement.".into()))
    }

    fn compile_package(&mut self, path: &Path) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"package\" statement.".into()))
    }

    fn compile_modifier(&mut self, modifier: &Modifier) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"modifier\" statement.".into()))
    }

    fn compile_with(&mut self, expr: &Expression) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"with\" statement.".into()))
    }

    fn compile_defer(&mut self, expr: &Expression) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"defer\" statement.".into()))
    }

    fn compile_assign(
        &mut self,
        assign_op: &AssignOperator,
        lhs: &mut Expression,
        rhs: &mut Expression,
    ) -> CustomResult<InstructionValue<'ctx>> {
        let access_type = if let Some(access_type) = lhs.get_access_type() {
            access_type
        } else {
            return Err(self.err(format!(
                "Left hand side in assignment not a valid type lhs: {:?}.",
                lhs
            )));
        };

        let var = if let Some(var) = lhs.eval_to_var() {
            var
        } else {
            return Err(self.err(format!(
                "Left hand side in assignment doesn't expand to a variable: {:?}",
                lhs
            )));
        };

        // The return type of the evaluated variable. This might not be the actual
        // return type since it might ex. be dereferenced or indexed. Use this
        // `ret_type` and then figure out the actual correct return type by
        // looking at the AccessType.
        let var_ret_type = if let Some(ref ret_type) = var.ret_type {
            ret_type
        } else {
            return Err(self.err(format!(
                "Type of variable \"{}\" not know when compiling assignment.",
                &var.name
            )));
        };

        // TODO: Probably move this logic into "Expression" together with the
        //       other functions like "is_var", "eval_to_var" etc.
        // Figure out the actual return type.
        let ret_type = match access_type {
            AccessType::Regular => self.compile_type(var_ret_type)?,
            AccessType::Deref => match &var_ret_type.t {
                Type::Pointer(inner) => self.compile_type(&inner)?,
                _ => {
                    return Err(self.err(format!(
                        "Tried to dereference variable \"{}\" that isn't a pointer. Is: {:?}.",
                        &var.name, var_ret_type.t
                    )))
                }
            },
            AccessType::Address => self.compile_type(&TypeStruct::new(
                Type::Pointer(Box::new(var_ret_type.clone())),
                None,
            ))?,
            AccessType::ArrayAccess => match &var_ret_type.t {
                Type::Array(inner, _) => self.compile_type(&inner)?,
                _ => {
                    return Err(self.err(format!(
                        "Tried to array index variable \"{}\" that isn't a array. Is: {:?}.",
                        &var.name, var_ret_type.t
                    )))
                }
            },
        };

        let right_any_value = self.compile_expr(rhs)?;
        let right = CodeGen::any_into_basic_value(right_any_value)?;

        let left = self.compile_var_load(var)?;

        // TODO: Need to check the size(8,16,32...) and also signness for the
        //       left and right to choose the correct instruction.

        let value = match assign_op {
            AssignOperator::Assignment => right,

            AssignOperator::AssignAddition => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_add(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.add.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_add(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.add.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!("Invalid type for AssignAddition: {:?}", ret_type)))
                }
            },

            AssignOperator::AssignSubtraction => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_sub(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.sub.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_add(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.sub.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!(
                        "Invalid type for AssignSubtraction: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignMultiplication => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_mul(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.mul.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_mul(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.mul.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!(
                        "Invalid type for AssignMultiplication: {:?}",
                        ret_type
                    )))
                }
            },

            AssignOperator::AssignDivision => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_div(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.div.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_div(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.div.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!("Invalid type for AssignDivision: {:?}", ret_type)))
                }
            },

            AssignOperator::AssignModulus => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_rem(
                        left.into_float_value(),
                        right.into_float_value(),
                        "assign.mod.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_rem(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.mod.int",
                    )
                    .into(),
                AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!("Invalid type for AssignModulus: {:?}", ret_type)))
                }
            },

            AssignOperator::AssignPower => return Err(self.err("TODO: AssignPower.".into())),
            AssignOperator::AssignBitAnd => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_and(left.into_int_value(), right.into_int_value(), "assign.and")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!("Invalid type for AssignBitAnd: {:?}", ret_type)))
                }
            },

            AssignOperator::AssignBitOr => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_or(left.into_int_value(), right.into_int_value(), "assign.or")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!("Invalid type for AssignBitOr: {:?}", ret_type)))
                }
            },

            AssignOperator::AssignBitXor => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_xor(left.into_int_value(), right.into_int_value(), "assign.xor")
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(self.err(format!("Invalid type for AssignBitXor: {:?}", ret_type)))
                }
            },

            AssignOperator::AssignShiftLeft => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_left_shift(
                        left.into_int_value(),
                        right.into_int_value(),
                        "assign.lshift",
                    )
                    .into(),
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(
                        self.err(format!("Invalid type for AssignShiftLeft: {:?}", ret_type))
                    )
                }
            },

            AssignOperator::AssignShiftRight => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = true;
                    self.builder
                        .build_right_shift(
                            left.into_int_value(),
                            right.into_int_value(),
                            sign_extend,
                            "assign.rshift",
                        )
                        .into()
                }
                AnyTypeEnum::FloatType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_)
                | AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_) => {
                    return Err(
                        self.err(format!("Invalid type for AssignShiftRight: {:?}", ret_type))
                    )
                }
            },
        };

        self.compile_var_store(var, value)
    }
}
