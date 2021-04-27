use crate::{expr::ExprTy, generator::CodeGen};
use common::{
    error::LangResult,
    file::FilePosition,
    path::LangPath,
    token::{expr::Expr, op::AssignOperator, stmt::Stmt},
};
use inkwell::{
    module::Linkage, types::AnyTypeEnum, values::BasicValueEnum, values::InstructionValue,
};
use log::debug;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_stmt(&mut self, stmt: &mut Stmt) -> LangResult<()> {
        match stmt {
            Stmt::Return(expr_opt, ..) => self.compile_return(expr_opt),
            Stmt::Yield(expr, file_pos) => self.compile_yield(expr, file_pos),
            Stmt::Break(file_pos) => self.compile_break(file_pos),
            Stmt::Continue(file_pos) => self.compile_continue(file_pos),
            Stmt::Use(path) => self.compile_use(path),
            Stmt::Module(path) => self.compile_module(path),

            // Only the "DeferExecution" are compiled into code, the "Defer" is
            // only used during analyzing.
            Stmt::Defer(..) => Ok(()),
            Stmt::DeferExec(expr) => {
                self.compile_expr(expr, ExprTy::RValue)?;
                Ok(())
            }

            Stmt::VariableDecl(var, ..) => {
                let mut var = var.borrow_mut();

                self.compile_var_decl(&var)?;
                if var.is_global {
                    // TODO: Fix this. Currently doesn't allow assigns in globals
                    //       since they can be created outside functions and the
                    //       assigns in LLVM would then be invalid since they
                    //       aren't inside a function/block.
                } else if let Some(expr) = &mut var.value {
                    let any_value = self.compile_expr(expr, ExprTy::RValue)?;
                    let basic_value = CodeGen::any_into_basic_value(any_value)?;
                    self.compile_var_store(&var, basic_value)?;
                } else if var.is_const {
                    return Err(self.err(
                        format!("const var decl of \"{}\" has no value set", &var.name),
                        var.file_pos.to_owned(),
                    ));
                }
                Ok(())
            }
            // TODO: Add other external declares other than func (var, struct etc.)
            Stmt::ExternalDecl(func, file_pos) => {
                let linkage = Linkage::External;
                self.compile_fn_proto(&func.borrow(), file_pos.to_owned(), Some(linkage))?;
                Ok(())
            }
            Stmt::Assignment(assign_op, lhs, rhs, file_pos) => {
                self.compile_assign(assign_op, lhs, rhs, file_pos)?;
                Ok(())
            }
        }
    }

    fn compile_return(&mut self, expr_opt: &mut Option<Expr>) -> LangResult<()> {
        if let Some(expr) = expr_opt {
            let any_value = self.compile_expr(expr, ExprTy::RValue)?;
            let basic_value = CodeGen::any_into_basic_value(any_value)?;
            self.builder.build_return(Some(&basic_value));
        } else {
            self.builder.build_return(None);
        }
        Ok(())
    }

    fn compile_yield(&mut self, expr: &Expr, file_pos: &Option<FilePosition>) -> LangResult<()> {
        Err(self.err(
            "TODO: Implement \"yield\" statement.".into(),
            file_pos.to_owned(),
        ))
    }

    fn compile_break(&mut self, file_pos: &Option<FilePosition>) -> LangResult<()> {
        let id = self.cur_block_id;
        let merge_block = self.get_branchable_merge_block(id)?;
        self.builder.build_unconditional_branch(merge_block);
        Ok(())
    }

    fn compile_continue(&mut self, file_pos: &Option<FilePosition>) -> LangResult<()> {
        if let Some(branch_block) = self.cur_branch_block {
            self.builder.build_unconditional_branch(branch_block);
            Ok(())
        } else {
            Err(self.err(
                "Branch block None when compiling \"continue\".".into(),
                file_pos.to_owned(),
            ))
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn compile_use(&mut self, _path: &LangPath) -> LangResult<()> {
        // Do nothing, "use"s are done during lexing/parsing for now.
        Ok(())
    }

    #[allow(clippy::unnecessary_wraps)]
    fn compile_module(&mut self, _path: &LangPath) -> LangResult<()> {
        // Do nothing, "module"s are done during lexing/parsing for now.
        Ok(())
    }

    // TODO: Check type to see that the assignment is valid.
    fn compile_assign(
        &mut self,
        assign_op: &AssignOperator,
        lhs: &mut Expr,
        rhs: &mut Expr,
        file_pos: &Option<FilePosition>,
    ) -> LangResult<InstructionValue<'ctx>> {
        let lhs_ptr = {
            let lhs_any = self.compile_expr(lhs, ExprTy::LValue)?;
            if lhs_any.is_pointer_value() {
                lhs_any.into_pointer_value()
            } else {
                return Err(self.err(
                    format!(
                        "Lhs not a pointer when compiling assignment\nlhs: {:#?}\nlhs_any: {:#?}, rhs: {:#?}",
                        lhs, lhs_any, rhs
                    ),
                    file_pos.to_owned()
                ));
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
                    return Err(self.err(
                        format!("Invalid type for AssignAddition: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!(
                            "Invalid type for AssignSubtraction: {:?}",
                            lhs_val.get_type()
                        ),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!(
                            "Invalid type for AssignMultiplication: {:?}",
                            lhs_val.get_type()
                        ),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!("Invalid type for AssignDivision: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!("Invalid type for AssignModulus: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!("Invalid type for AssignBitAnd: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!("Invalid type for AssignBitOr: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!("Invalid type for AssignBitXor: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!("Invalid type for AssignLeftShift: {:?}", lhs_val.get_type()),
                        file_pos.to_owned(),
                    ))
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
                    return Err(self.err(
                        format!(
                            "Invalid type for AssignRightShift: {:?}",
                            lhs_val.get_type()
                        ),
                        file_pos.to_owned(),
                    ))
                }
            },
        };

        debug!(
            "Assigning, op: {:#?}, lhs_ptr: {:#?}, val: {:#?}",
            assign_op, lhs_ptr, value
        );

        Ok(self
            .builder
            .build_store(lhs_ptr, CodeGen::any_into_basic_value(value)?))
    }
}
