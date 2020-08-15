use super::generator::CodeGen;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::{
    parse::token::{AssignOperator, Expression, Modifier, Path, Statement, Variable},
    CustomResult,
};
use inkwell::{module::Linkage, types::AnyTypeEnum};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_stmt(&mut self, stmt: &Statement) -> CustomResult<()> {
        match stmt {
            Statement::Return(expr_opt) => self.compile_return(expr_opt),
            Statement::Yield(expr) => self.compile_yield(expr),
            Statement::Break => self.compile_break(),
            Statement::Continue => self.compile_continue(),
            Statement::Use(path) => self.compile_use(path),
            Statement::Package(path) => self.compile_package(path),
            Statement::With(expr) => panic!("TODO: compile \"with\"."),
            Statement::Defer(expr) => panic!("TODO: compile \"defer\"."),
            Statement::VariableDecl(var, expr_opt) => {
                self.compile_var_decl(var)?;
                if let Some(expr) = expr_opt {
                    let any_value = self.compile_expr(expr)?;
                    let basic_value = CodeGen::any_into_basic_value(any_value)?;
                    self.compile_var_store(var, basic_value)?;
                } else if var.is_const {
                    return Err(LangError::new(
                        format!("const var decl of \"{}\" has no value set", &var.name),
                        CodeGenError,
                    ));
                }
                Ok(())
            }
            Statement::ExternalDecl(func) => {
                let linkage = Linkage::External;
                self.compile_func_proto(func, Some(linkage))?;
                Ok(())
            }
            Statement::Modifier(modifier) => self.compile_modifier(modifier),
            Statement::Assignment(assign_op, var, expr) => {
                self.compile_assign(assign_op, var, expr)
            }
        }
    }

    fn compile_return(&mut self, expr_opt: &Option<Expression>) -> CustomResult<()> {
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
        Err(LangError::new(
            "TODO: Implement yield statement.".into(),
            CodeGenError,
        ))
    }

    fn compile_break(&mut self) -> CustomResult<()> {
        // TODO: Is it always OK to use `self.state.cur_block_id` here?
        let id = self.state.cur_block_id;
        let merge_block = self.get_merge_block(id)?;
        self.builder.build_unconditional_branch(merge_block);
        Ok(())
    }

    fn compile_continue(&mut self) -> CustomResult<()> {
        Err(LangError::new(
            "TODO: Implement continue statement.".into(),
            CodeGenError,
        ))
    }

    fn compile_use(&mut self, path: &Path) -> CustomResult<()> {
        Err(LangError::new(
            "TODO: Implement use statement.".into(),
            CodeGenError,
        ))
    }

    fn compile_package(&mut self, path: &Path) -> CustomResult<()> {
        Err(LangError::new(
            "TODO: Implement package statement.".into(),
            CodeGenError,
        ))
    }

    fn compile_modifier(&mut self, modifier: &Modifier) -> CustomResult<()> {
        Err(LangError::new(
            "TODO: Implement modifier statement.".into(),
            CodeGenError,
        ))
    }

    // TODO: Only "int"s atm.
    fn compile_assign(
        &mut self,
        assign_op: &AssignOperator,
        var: &Variable,
        expr: &Expression,
    ) -> CustomResult<()> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = var.ret_type {
            ret_type.t.to_codegen(&self.context)?
        } else {
            return Err(LangError::new(
                format!(
                    "Type of var \"{}\" not know when compiling assignment.",
                    &var.name
                ),
                CodeGenError,
            ));
        };

        let right_any_value = self.compile_expr(expr)?;
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignAddition: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignSubtraction: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignMultiplication: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignDivision: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignModulus: {:?}", ret_type),
                        CodeGenError,
                    ))
                }
            },

            AssignOperator::AssignPower => {
                return Err(LangError::new("TODO: AssignPower.".into(), CodeGenError))
            }
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignBitAnd: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignBitOr: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignBitXor: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignShiftLeft: {:?}", ret_type),
                        CodeGenError,
                    ))
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
                    return Err(LangError::new(
                        format!("Invalid type for AssignShiftRight: {:?}", ret_type),
                        CodeGenError,
                    ))
                }
            },
        };

        self.compile_var_store(var, value)
    }
}
