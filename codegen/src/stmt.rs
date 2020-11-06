use crate::{expr::ExprTy, generator::CodeGen};
use common::{
    error::CustomResult,
    token::{
        expr::Expr,
        op::AssignOperator,
        stmt::{Modifier, Path, Stmt},
    },
};
use inkwell::{
    module::Linkage, types::AnyTypeEnum, values::AnyValueEnum, values::BasicValueEnum,
    values::InstructionValue,
};
use log::debug;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_stmt(&mut self, stmt: &mut Stmt) -> CustomResult<()> {
        match stmt {
            Stmt::Return(expr_opt) => self.compile_return(expr_opt),
            Stmt::Yield(expr) => self.compile_yield(expr),
            Stmt::Break => self.compile_break(),
            Stmt::Continue => self.compile_continue(),
            Stmt::Use(path) => self.compile_use(path),
            Stmt::Package(path) => self.compile_package(path),
            Stmt::Increment(expr) => self.compile_inc(expr),
            Stmt::Decrement(expr) => self.compile_dec(expr),
            Stmt::Modifier(modifier) => self.compile_modifier(modifier),

            // Only the "DeferExecution" are compiled into code, the "Defer" is
            // only used during analyzing.
            Stmt::Defer(_) => Ok(()),
            Stmt::DeferExec(expr) => {
                self.compile_expr(expr, ExprTy::RValue)?;
                Ok(())
            }

            Stmt::VariableDecl(var, expr_opt) => {
                let var = var.borrow();

                self.compile_var_decl(&var)?;
                if let Some(expr) = expr_opt {
                    let any_value = self.compile_expr(expr, ExprTy::RValue)?;
                    let basic_value = CodeGen::any_into_basic_value(any_value)?;
                    self.compile_var_store(&var, basic_value)?;
                } else if var.is_const {
                    return Err(self.err(format!(
                        "const var decl of \"{}\" has no value set",
                        &var.name
                    )));
                }
                Ok(())
            }
            // TODO: Add other external declares other than func (var, struct etc.)
            Stmt::ExternalDecl(func) => {
                let linkage = Linkage::External;
                self.compile_func_proto(&func.borrow(), Some(linkage))?;
                Ok(())
            }
            Stmt::Assignment(assign_op, lhs, rhs) => {
                self.compile_assign(assign_op, lhs, rhs)?;
                Ok(())
            }
        }
    }

    fn compile_return(&mut self, expr_opt: &mut Option<Expr>) -> CustomResult<()> {
        if let Some(expr) = expr_opt {
            let any_value = self.compile_expr(expr, ExprTy::RValue)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            self.builder.build_return(Some(&basic_value));
        } else {
            self.builder.build_return(None);
        }
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Expr) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"yield\" statement.".into()))
    }

    fn compile_break(&mut self) -> CustomResult<()> {
        let id = self.cur_block_id;
        let merge_block = self.get_branchable_merge_block(id)?;
        self.builder.build_unconditional_branch(merge_block);
        Ok(())
    }

    fn compile_continue(&mut self) -> CustomResult<()> {
        if let Some(branch_block) = self.cur_branch_block {
            self.builder.build_unconditional_branch(branch_block);
            Ok(())
        } else {
            Err(self.err("Branch block None when compiling \"continue\".".into()))
        }
    }

    fn compile_use(&mut self, path: &Path) -> CustomResult<()> {
        // Do nothing, "use"s are done during lexing/parsing for now.
        Ok(())
    }

    fn compile_package(&mut self, path: &Path) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"package\" statement.".into()))
    }

    fn compile_inc(&mut self, expr: &mut Expr) -> CustomResult<()> {
        let any_value = self.compile_expr(expr, ExprTy::LValue)?;

        let ptr = if let AnyTypeEnum::PointerType(_) = any_value.get_type() {
            any_value.into_pointer_value()
        } else {
            return Err(self.err(format!(
                "Increment expr didn't eval to pointer.\nExpr: {:#?}\nany_value: {:#?}",
                expr, any_value
            )));
        };

        let val = self.builder.build_load(ptr, "inc.load");
        let new_value = if let BasicValueEnum::IntValue(value) = val {
            let sign_extend = false;
            let one = value.get_type().const_int(1, sign_extend);
            if value.is_const() {
                value.const_add(one)
            } else {
                self.builder.build_int_add(value, one, "inc")
            }
        } else {
            return Err(self.err(format!(
                "Increment value in pointer didn't eval to int.\nExpr: {:#?}\nany_value: {:#?}",
                expr, any_value
            )));
        };

        self.builder.build_store(ptr, new_value);
        Ok(())
    }

    fn compile_dec(&mut self, expr: &mut Expr) -> CustomResult<()> {
        let any_value = self.compile_expr(expr, ExprTy::LValue)?;

        let ptr = if let AnyTypeEnum::PointerType(_) = any_value.get_type() {
            any_value.into_pointer_value()
        } else {
            return Err(self.err(format!(
                "Decrement expr didn't eval to pointer.\nExpr: {:#?}\nany_value: {:#?}",
                expr, any_value
            )));
        };

        let val = self.builder.build_load(ptr, "dec.load");
        let new_value = if let BasicValueEnum::IntValue(value) = val {
            let sign_extend = false;
            let one = value.get_type().const_int(1, sign_extend);
            if value.is_const() {
                value.const_sub(one)
            } else {
                self.builder.build_int_sub(value, one, "dec")
            }
        } else {
            return Err(self.err(format!(
                "Decrement value in pointer didn't eval to int.\nExpr: {:#?}\nany_value: {:#?}",
                expr, any_value
            )));
        };

        self.builder.build_store(ptr, new_value);
        Ok(())
    }

    fn compile_modifier(&mut self, modifier: &Modifier) -> CustomResult<()> {
        Err(self.err("TODO: Implement \"modifier\" statement.".into()))
    }

    fn compile_assign(
        &mut self,
        assign_op: &AssignOperator,
        lhs: &mut Expr,
        rhs: &mut Expr,
    ) -> CustomResult<InstructionValue<'ctx>> {
        let lhs_ptr = {
            let lhs_any = self.compile_expr(lhs, ExprTy::LValue)?;
            if lhs_any.is_pointer_value() {
                lhs_any.into_pointer_value()
            } else {
                return Err(self.err(format!(
                    "Lhs not a pointer when compiling assignment\nlhs: {:#?}\nlhs_any: {:#?}, rhs: {:#?}",
                    lhs, lhs_any, rhs
                )));
            }
        };
        let lhs_val = self.builder.build_load(lhs_ptr, "assign.lhs.val");

        let rhs_val = self.compile_expr(rhs, ExprTy::RValue)?;
        let value = match assign_op {
            AssignOperator::Assignment => rhs_val,
            AssignOperator::AssignAdd => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_add(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.add.int",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_add(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "assign.add.float",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignAddition: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignSub => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_sub(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.sub.int",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_sub(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "assign.sub.float",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignSubtraction: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignMul => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_mul(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.mul.int",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_mul(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "assign.mul.float",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignMultiplication: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignDiv => match lhs_val {
                // TODO: Check signess.
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_signed_div(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.div.int",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_div(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "assign.div.float",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignDivision: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignMod => match lhs_val {
                // TODO: Check signess.
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_int_signed_rem(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.mod.int",
                    )
                    .into(),
                BasicValueEnum::FloatValue(_) => self
                    .builder
                    .build_float_rem(
                        lhs_val.into_float_value(),
                        rhs_val.into_float_value(),
                        "assign.rem.float",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignModulus: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignBitAnd => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_and(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.bit.and",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignBitAnd: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignBitOr => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_or(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.bit.or",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignBitOr: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignBitXor => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_xor(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.bit.xor",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignBitXor: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignShl => match lhs_val {
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_left_shift(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        "assign.lshift",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignLeftShift: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
            AssignOperator::AssignShr => match lhs_val {
                // TODO: Signess.
                BasicValueEnum::IntValue(_) => self
                    .builder
                    .build_right_shift(
                        lhs_val.into_int_value(),
                        rhs_val.into_int_value(),
                        true,
                        "assign.lshift",
                    )
                    .into(),
                _ => {
                    return Err(self.err(format!(
                        "Invalid type for AssignRightShift: {:?}",
                        lhs_val.get_type()
                    )))
                }
            },
        };

        debug!(
            "Assigning, op: {:?}, lhs_ptr: {:?}, val: {:?}",
            assign_op, lhs_ptr, value
        );

        Ok(self
            .builder
            .build_store(lhs_ptr, CodeGen::any_into_basic_value(value)?))
    }
}
