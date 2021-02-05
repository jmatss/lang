use crate::{expr::ExprTy, generator::CodeGen};
use analyze::block::BlockInfo;
use common::{
    error::LangResult,
    file::FilePosition,
    token::{
        block::Adt,
        expr::Expr,
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Stmt,
    },
    ty::ty::Ty,
    util,
};
use inkwell::{
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{AggregateValue, AnyValueEnum, BasicValueEnum, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};
use log::debug;
use std::{cell::RefCell, rc::Rc};

// TODO: Check constness for operators. Ex. adding two consts should use a
//       const add instruction so that the result also is const.

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_op(
        &mut self,
        op: &mut Op,
        expr_ty: ExprTy,
        file_pos: Option<FilePosition>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        match op {
            Op::BinOp(ref mut bin_op) => match expr_ty {
                ExprTy::LValue => {
                    Err(self.err(format!("Bin op not allowed in lvalue: {:?}", op), file_pos))
                }
                ExprTy::RValue => self.compile_bin_op(bin_op, file_pos),
            },
            Op::UnOp(ref mut un_op) => self.compile_un_op(un_op, expr_ty, file_pos),
        }
    }

    fn compile_bin_op(
        &mut self,
        bin_op: &mut BinOp,
        file_pos: Option<FilePosition>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let ret_type = if let Some(ref ret_type) = bin_op.ret_type {
            self.compile_type(&ret_type, file_pos)?
        } else {
            return Err(self.err(
                format!(
                    "Type of bin_op \"{:?}\" not know when compiling assignment.",
                    &bin_op
                ),
                file_pos,
            ));
        };

        let left_any = self.compile_expr(&mut bin_op.lhs, ExprTy::RValue)?;
        let left = CodeGen::any_into_basic_value(left_any)?;

        // Bool "and" and "or" should be short circuit, so they need to be
        // compiled before the right side is compiled. This is a temporary fix
        // to ensure that they are evaluated before the right hand side is
        // compiled. This logic should be merged better with the other operators.
        match bin_op.operator {
            BinOperator::BoolAnd => {
                return self.compile_bin_op_bool_and(ret_type, left, &mut bin_op.rhs)
            }
            BinOperator::BoolOr => {
                return self.compile_bin_op_bool_or(ret_type, left, &mut bin_op.rhs)
            }
            _ => (),
        }

        let right_any = self.compile_expr(&mut bin_op.rhs, ExprTy::RValue)?;
        let right = CodeGen::any_into_basic_value(right_any)?;

        bin_op.is_const = CodeGen::is_const(&[left_any, right_any]);

        let left_type = left.get_type();
        let right_type = right.get_type();

        // Check to see if left and right have the same base type so that they
        // can interact correctly in expressions. For some operations this is
        // not required and they are therefore excluded from the base type check.
        match bin_op.operator {
            BinOperator::In
            | BinOperator::Is
            | BinOperator::As
            | BinOperator::Of
            | BinOperator::Dot
            | BinOperator::DoubleColon => {}

            _ => {
                if !self.is_same_base_type(left_type, right_type) {
                    return Err(self.err(
                        format!(
                            "Left & right type different base types during bin op. Op: {:?}\nleft_type: {:?}\nright_type: {:?}",
                            bin_op.operator,
                            left_type,
                            right_type,
                        ),
                        file_pos,
                    ));
                }
            }
        }

        // TODO: Implement is_signed for the function `compile_bin_op_as()`.

        Ok(match bin_op.operator {
            BinOperator::In => panic!("TODO: In"),
            BinOperator::Is => panic!("TODO: Is"),
            BinOperator::As => self.compile_bin_op_as(ret_type, bin_op.is_const, left, right)?,
            BinOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            BinOperator::Range => panic!("TODO: Range"),
            BinOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            BinOperator::Dot => unreachable!("Compile bin op Dot"),
            BinOperator::DoubleColon => unreachable!("Compile bin op DoubleColon"),

            BinOperator::Eq
            | BinOperator::Neq
            | BinOperator::Lt
            | BinOperator::Gt
            | BinOperator::Lte
            | BinOperator::Gte => self.compile_bin_op_compare(
                bin_op.ret_type.as_ref().map_or(false, |ty| ty.is_signed()),
                bin_op.is_const,
                &bin_op.operator,
                left,
                right,
            )?,

            BinOperator::Add => {
                self.compile_bin_op_addition(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Sub => {
                self.compile_bin_op_subtraction(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Mul => {
                self.compile_bin_op_multiplication(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Div => self.compile_bin_op_division(
                ret_type,
                bin_op.ret_type.as_ref().map_or(false, |ty| ty.is_signed()),
                bin_op.is_const,
                left,
                right,
            )?,
            BinOperator::Mod => self.compile_bin_op_modulus(
                ret_type,
                bin_op.ret_type.as_ref().map_or(false, |ty| ty.is_signed()),
                bin_op.is_const,
                left,
                right,
            )?,
            BinOperator::BitAnd => {
                self.compile_bin_op_bit_and(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::BitOr => {
                self.compile_bin_op_bit_or(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::BitXor => {
                self.compile_bin_op_bit_xor(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::ShiftLeft => {
                self.compile_bin_op_shift_left(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::ShiftRight => self.compile_bin_op_shift_right(
                ret_type,
                bin_op.ret_type.as_ref().map_or(false, |ty| ty.is_signed()),
                bin_op.is_const,
                left,
                right,
            )?,
            BinOperator::BoolAnd => {
                panic!("Unexpected BoolAnd to late in func.");
            }
            BinOperator::BoolOr => {
                panic!("Unexpected BoolOr to late in func.");
            }
        })
    }

    fn compile_un_op(
        &mut self,
        un_op: &mut UnOp,
        expr_ty: ExprTy,
        file_pos: Option<FilePosition>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let ret_type = if let Some(ref ret_type) = un_op.ret_type {
            self.compile_type(&ret_type, file_pos)?
        } else {
            return Err(self.err(
                format!(
                    "Type of un_op \"{:?}\" not know when compiling assignment.",
                    &un_op
                ),
                file_pos,
            ));
        };

        if let ExprTy::LValue = expr_ty {
            match &un_op.operator {
                UnOperator::Address
                | UnOperator::Positive
                | UnOperator::Negative
                | UnOperator::BitComplement
                | UnOperator::BoolNot => {
                    return Err(self.err(format!("Invalid un op in lvalue: {:?}", un_op), file_pos))
                }
                _ => (),
            }
        }

        match &mut un_op.operator {
            UnOperator::Deref => {
                let any_value = self.compile_un_op_deref(un_op)?;
                match expr_ty {
                    ExprTy::LValue => Ok(any_value),
                    ExprTy::RValue => {
                        if any_value.is_pointer_value() {
                            let ptr = any_value.into_pointer_value();
                            Ok(self.builder.build_load(ptr, "deref.rval").into())
                        } else {
                            Err(self.err(
                                format!("Tried to deref non pointer in rvalue {:?}", any_value),
                                file_pos,
                            ))
                        }
                    }
                }
            }
            UnOperator::Address => {
                // TODO: Does address need lval/rval logic?
                self.compile_un_op_address(un_op)
            }
            UnOperator::ArrayAccess(_) => {
                let ptr = self.compile_un_op_array_access(un_op)?;
                match expr_ty {
                    ExprTy::LValue => Ok(ptr.into()),
                    ExprTy::RValue => Ok(self.builder.build_load(ptr, "array.gep.rval").into()),
                }
            }
            UnOperator::AdtAccess(..) => {
                let val = self.compile_un_op_adt_access(un_op, expr_ty)?;
                match expr_ty {
                    ExprTy::LValue => Ok(val),
                    ExprTy::RValue => Ok(if val.is_pointer_value() {
                        self.builder
                            .build_load(val.into_pointer_value(), "struct.gep.rval")
                            .into()
                    } else {
                        val
                    }),
                }
            }
            UnOperator::EnumAccess(..) => match expr_ty {
                ExprTy::LValue => Err(self.err(
                    format!("Enum access is not allowed as a LValue: {:?}", un_op),
                    file_pos,
                )),
                ExprTy::RValue => self.compile_un_op_enum_access(un_op),
            },
            UnOperator::UnionIs(member_name, var_decl) => {
                let var = if let Stmt::VariableDecl(var, ..) = var_decl.as_ref() {
                    var
                } else {
                    unreachable!("{:#?}", un_op);
                };

                // TODO: The value will be stored into the new var decl here.
                //       This is compiled in the if-case expr which means that the
                //       variable is declared and then stored even though the expr
                //       might not evaluate to true. Change this so that the var
                //       is declared and initialized ONLY if the expr evals to true.
                let union_access = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
                self.compile_var_decl(&var.borrow())?;
                self.compile_var_store(
                    &var.borrow(),
                    CodeGen::any_into_basic_value(union_access)?,
                )?;

                // TODO: Do in cleaner way.
                // Get the type of the union and get the tag idx for the member
                // that is being accessed. This will be compared to the actual
                // tag idx of the variable in the code to see if this is a match
                // or not.
                let union_var = if let Expr::Op(Op::UnOp(inner_un_op)) = un_op.value.as_ref() {
                    if let Expr::Var(union_var) = inner_un_op.value.as_ref() {
                        union_var
                    } else {
                        unreachable!("inner_un_op.value not a var: {:#?}", un_op);
                    }
                } else {
                    unreachable!("un_op.value not union access: {:#?}", un_op);
                };

                let tag_cmp = if let Ty::CompoundType(inner_ty, generics, ..) =
                    union_var.ty.as_ref().unwrap()
                {
                    let adt_name = util::to_generic_name(&inner_ty.get_ident().unwrap(), generics);

                    let expected_tag = self.analyze_context.get_adt_member_index(
                        &adt_name,
                        member_name,
                        self.cur_block_id,
                    )?;
                    let expected_tag = self.context.i8_type().const_int(expected_tag, false);

                    let union_var_ptr = self.get_var_ptr(union_var)?;
                    let tag_ptr = self
                        .builder
                        .build_struct_gep(union_var_ptr, 0, "union.is.tag.gep")
                        .map_err(|_| {
                            self.err(
                                format!("Unable to GEP union \"{}\" tag.", &adt_name),
                                union_var.file_pos,
                            )
                        })?;

                    let actual_tag = self.builder.build_load(tag_ptr, "union.is.tag.load");
                    if !actual_tag.is_int_value() {
                        unreachable!(
                            "Union tag isn't IntValue. actual_tag: {:#?}, un_op: {:#?}",
                            actual_tag, un_op
                        );
                    }

                    self.builder.build_int_compare(
                        IntPredicate::EQ,
                        expected_tag,
                        actual_tag.into_int_value(),
                        "union.is.tag.cmp",
                    )
                } else {
                    unreachable!("inner_un_op.value type not compound: {:#?}", un_op);
                };

                Ok(tag_cmp.into())
            }
            UnOperator::Positive => {
                let any_value = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
                un_op.is_const = CodeGen::is_const(&[any_value]);

                // Do nothing.
                Ok(any_value)
            }
            UnOperator::Negative => self.compile_un_op_negative(ret_type, un_op),
            UnOperator::BitComplement => panic!("TODO: Bit complement"),
            UnOperator::BoolNot => self.compile_un_op_bool_not(ret_type, un_op),
        }
    }

    fn compile_bin_op_as(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // TODO: Will probably need to check both sides before doing a
        //       cast. For example now, if the ret_type is float,
        //       the left will be casted into a float. But it assumes
        //       that the left side is some sort of float already.
        let left_type = left.get_type();
        let right_type = right.get_type();

        // TODO: Figure out sign extensions.

        Ok(if is_const {
            match right_type {
                BasicTypeEnum::ArrayType(ty) => panic!("TODO: Const array \"as\"."),
                BasicTypeEnum::FloatType(ty) => {
                    if left_type.is_float_type() {
                        left.into_float_value().const_cast(ty).into()
                    } else if left_type.is_int_type() {
                        left.into_int_value().const_signed_to_float(ty).into()
                    } else {
                        return Err(self.err(
                            format!("Invalid type when casting \"as\" float: {:?}", left_type),
                            None,
                        ));
                    }
                }
                BasicTypeEnum::IntType(ty) => {
                    if left_type.is_float_type() {
                        left.into_float_value().const_to_signed_int(ty).into()
                    } else if left_type.is_int_type() {
                        let is_signed = true;
                        left.into_int_value().const_cast(ty, is_signed).into()
                    } else {
                        return Err(self.err(
                            format!("Invalid type when casting \"as\" int: {:?}", left_type),
                            None,
                        ));
                    }
                }
                BasicTypeEnum::PointerType(ty) => left.into_pointer_value().const_cast(ty).into(),
                BasicTypeEnum::StructType(ty) => panic!("TODO: Const struct \"as\"."),
                BasicTypeEnum::VectorType(ty) => panic!("TODO: Const vector \"as\"."),
            }
        } else {
            match right_type {
                BasicTypeEnum::ArrayType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.array").into()
                }
                BasicTypeEnum::FloatType(ty) => {
                    if left_type.is_float_type() {
                        self.builder
                            .build_float_cast(left.into_float_value(), ty, "cast.float")
                            .into()
                    } else if left_type.is_int_type() {
                        self.builder
                            .build_signed_int_to_float(left.into_int_value(), ty, "cast.float")
                            .into()
                    } else {
                        self.builder.build_bitcast(left, ty, "cast.float").into()
                    }
                }
                BasicTypeEnum::IntType(ty) => {
                    if left_type.is_float_type() {
                        self.builder
                            .build_float_to_signed_int(left.into_float_value(), ty, "cast.int")
                            .into()
                    } else if left_type.is_int_type() {
                        self.builder
                            .build_int_cast(left.into_int_value(), ty, "cast.int")
                            .into()
                    } else {
                        self.builder.build_bitcast(left, ty, "cast.int").into()
                    }
                }
                BasicTypeEnum::PointerType(ty) => self
                    .builder
                    .build_pointer_cast(left.into_pointer_value(), ty, "cast.ptr")
                    .into(),
                BasicTypeEnum::StructType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.struct").into()
                }
                BasicTypeEnum::VectorType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.vector").into()
                }
            }
        })
    }

    fn compile_bin_op_compare(
        &mut self,
        is_signed: bool,
        is_const: bool,
        op: &BinOperator,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // Ensure that lhs and rhs has the same type.
        match (lhs.get_type(), rhs.get_type()) {
            (BasicTypeEnum::ArrayType(_), BasicTypeEnum::ArrayType(_))
            | (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(_))
            | (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_))
            | (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_))
            | (BasicTypeEnum::StructType(_), BasicTypeEnum::StructType(_))
            | (BasicTypeEnum::VectorType(_), BasicTypeEnum::VectorType(_)) => {}

            _ => {
                return Err(self.err(
                    format!(
                        "Lhs and rhs of bin op compare not same type. Lhs: {:?}, rhs: {:?}",
                        lhs, rhs
                    ),
                    None,
                ))
            }
        }

        // TODO: Add compares for more types.
        Ok(match lhs.get_type() {
            BasicTypeEnum::FloatType(_) => {
                let predicate = match op {
                    BinOperator::Eq => FloatPredicate::OEQ,
                    BinOperator::Neq => FloatPredicate::ONE,
                    BinOperator::Lt => FloatPredicate::OLT,
                    BinOperator::Gt => FloatPredicate::OGT,
                    BinOperator::Lte => FloatPredicate::OLE,
                    BinOperator::Gte => FloatPredicate::OGE,
                    _ => {
                        return Err(
                            self.err(format!("Invalid operator in float compare: {:?}", op), None)
                        )
                    }
                };

                if is_const {
                    lhs.into_float_value()
                        .const_compare(predicate, rhs.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_compare(
                            predicate,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "compare.float",
                        )
                        .into()
                }
            }

            // TODO: Signness
            BasicTypeEnum::IntType(_) => {
                let predicate = match op {
                    BinOperator::Eq => IntPredicate::EQ,
                    BinOperator::Neq => IntPredicate::NE,
                    BinOperator::Lt if is_signed => IntPredicate::SLT,
                    BinOperator::Lt if !is_signed => IntPredicate::ULT,
                    BinOperator::Gt if is_signed => IntPredicate::SGT,
                    BinOperator::Gt if !is_signed => IntPredicate::UGT,
                    BinOperator::Lte if is_signed => IntPredicate::SLE,
                    BinOperator::Lte if !is_signed => IntPredicate::ULE,
                    BinOperator::Gte if is_signed => IntPredicate::SGE,
                    BinOperator::Gte if !is_signed => IntPredicate::UGE,
                    _ => {
                        return Err(
                            self.err(format!("Invalid operator in int compare: {:?}", op), None)
                        )
                    }
                };

                if is_const {
                    lhs.into_int_value()
                        .const_int_compare(predicate, rhs.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_compare(
                            predicate,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "compare.int",
                        )
                        .into()
                }
            }

            BasicTypeEnum::PointerType(_) => {
                let predicate = match op {
                    BinOperator::Eq => IntPredicate::EQ,
                    BinOperator::Neq => IntPredicate::NE,
                    BinOperator::Lt if is_signed => IntPredicate::SLT,
                    BinOperator::Lt if !is_signed => IntPredicate::ULT,
                    BinOperator::Gt if is_signed => IntPredicate::SGT,
                    BinOperator::Gt if !is_signed => IntPredicate::UGT,
                    BinOperator::Lte if is_signed => IntPredicate::SLE,
                    BinOperator::Lte if !is_signed => IntPredicate::ULE,
                    BinOperator::Gte if is_signed => IntPredicate::SGE,
                    BinOperator::Gte if !is_signed => IntPredicate::UGE,
                    _ => {
                        return Err(
                            self.err(format!("Invalid operator in ptr compare: {:?}", op), None)
                        )
                    }
                };

                let ptr_int_ty = self
                    .context
                    .ptr_sized_int_type(&self.target_machine.get_target_data(), None);

                if is_const {
                    let lhs_ptr = lhs.into_pointer_value().const_to_int(ptr_int_ty);
                    let rhs_ptr = rhs.into_pointer_value().const_to_int(ptr_int_ty);

                    lhs_ptr.const_int_compare(predicate, rhs_ptr).into()
                } else {
                    let lhs_ptr = self.builder.build_ptr_to_int(
                        lhs.into_pointer_value(),
                        ptr_int_ty,
                        "lhs.ptr.to.int",
                    );
                    let rhs_ptr = self.builder.build_ptr_to_int(
                        rhs.into_pointer_value(),
                        ptr_int_ty,
                        "rhs.ptr.to.int",
                    );

                    self.builder
                        .build_int_compare(predicate, lhs_ptr, rhs_ptr, "compare.ptr")
                        .into()
                }
            }

            _ => {
                return Err(self.err(
                    format!("Invalid type used in bin op compare: {:?}", lhs.get_type()),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_addition(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if is_const {
                    left.into_float_value()
                        .const_add(right.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_add(
                            left.into_float_value(),
                            right.into_float_value(),
                            "add.float",
                        )
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_add(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_add(left.into_int_value(), right.into_int_value(), "add.int")
                        .into()
                }
            }
            AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::Addition: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_subtraction(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if is_const {
                    left.into_float_value()
                        .const_sub(right.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_sub(
                            left.into_float_value(),
                            right.into_float_value(),
                            "sub.float",
                        )
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_sub(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_sub(left.into_int_value(), right.into_int_value(), "sub.int")
                        .into()
                }
            }
            AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!(
                        "Invalid type for BinaryOperator::Subtraction: {:?}",
                        ret_type
                    ),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_multiplication(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if is_const {
                    left.into_float_value()
                        .const_mul(right.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_mul(
                            left.into_float_value(),
                            right.into_float_value(),
                            "mul.float",
                        )
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_mul(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_mul(left.into_int_value(), right.into_int_value(), "mul.int")
                        .into()
                }
            }
            AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!(
                        "Invalid type for BinaryOperator::Multiplication: {:?}",
                        ret_type
                    ),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_division(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if is_const {
                    left.into_float_value()
                        .const_div(right.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_div(
                            left.into_float_value(),
                            right.into_float_value(),
                            "div.float",
                        )
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if is_const && is_signed {
                    left.into_int_value()
                        .const_signed_div(right.into_int_value())
                        .into()
                } else if is_const {
                    left.into_int_value()
                        .const_unsigned_div(right.into_int_value())
                        .into()
                } else if is_signed {
                    self.builder
                        .build_int_signed_div(
                            left.into_int_value(),
                            right.into_int_value(),
                            "div.int",
                        )
                        .into()
                } else {
                    self.builder
                        .build_int_unsigned_div(
                            left.into_int_value(),
                            right.into_int_value(),
                            "div.int",
                        )
                        .into()
                }
            }
            AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::Division: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_modulus(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if is_const {
                    left.into_float_value()
                        .const_remainder(right.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_rem(
                            left.into_float_value(),
                            right.into_float_value(),
                            "mod.float",
                        )
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if is_const && is_signed {
                    left.into_int_value()
                        .const_signed_remainder(right.into_int_value())
                        .into()
                } else if is_const {
                    left.into_int_value()
                        .const_unsigned_remainder(right.into_int_value())
                        .into()
                } else if is_signed {
                    self.builder
                        .build_int_signed_rem(
                            left.into_int_value(),
                            right.into_int_value(),
                            "mod.int",
                        )
                        .into()
                } else {
                    self.builder
                        .build_int_unsigned_rem(
                            left.into_int_value(),
                            right.into_int_value(),
                            "mod.int",
                        )
                        .into()
                }
            }
            AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::Modulus: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_bit_and(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_and(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_and(left.into_int_value(), right.into_int_value(), "bit.and")
                        .into()
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::BitAnd: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_bit_or(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_or(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_or(left.into_int_value(), right.into_int_value(), "bit.or")
                        .into()
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::BitOr: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_bit_xor(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_xor(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_xor(left.into_int_value(), right.into_int_value(), "bit.xor")
                        .into()
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::BitXor: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_shift_left(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    left.into_int_value()
                        .const_shl(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_left_shift(left.into_int_value(), right.into_int_value(), "lshift")
                        .into()
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::ShiftLeft: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_shift_right(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                // TODO: Sign extension.
                // TODO: What is the difference between "const_ashr" and "const_rshr".
                if is_const {
                    left.into_int_value()
                        .const_ashr(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_right_shift(
                            left.into_int_value(),
                            right.into_int_value(),
                            is_signed, // == sign_extend
                            "rshift",
                        )
                        .into()
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!(
                        "Invalid type for BinaryOperator::ShiftRight: {:?}",
                        ret_type
                    ),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_bool_and(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        left: BasicValueEnum<'ctx>,
        right_expr: &mut Expr,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // TODO: Const.
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if let Some(cur_basic_block) = self.cur_basic_block {
                    // The eval block is to evaluate the rhs only if the lhs
                    // evaluates to true to allow for short circuit.
                    let eval_block = self
                        .context
                        .insert_basic_block_after(cur_basic_block, "bool.and.rhs");
                    let phi_block = self
                        .context
                        .insert_basic_block_after(eval_block, "bool.and.merge");

                    // The old `cur_basic_block` will be used as a branch block.
                    self.builder.position_at_end(cur_basic_block);
                    self.builder.build_conditional_branch(
                        left.into_int_value(),
                        eval_block,
                        phi_block,
                    );

                    // TODO: Must be a better way to do this branch. Currently
                    //       this eval block only exists to compile the expr and
                    //       then jump into a phi-block.
                    self.builder.position_at_end(eval_block);
                    let right = self.compile_expr(right_expr, ExprTy::RValue)?;
                    self.builder.build_unconditional_branch(phi_block);

                    let bool_type = self.context.bool_type();
                    let false_val = bool_type.const_zero();

                    self.builder.position_at_end(phi_block);
                    let phi_val = self.builder.build_phi(bool_type, "bool.and.phi");
                    phi_val.add_incoming(&[
                        (&false_val, cur_basic_block),
                        (&right.into_int_value(), eval_block),
                    ]);

                    self.cur_basic_block = Some(phi_block);
                    phi_val.as_basic_value().into()
                } else {
                    return Err(self.err(
                        "No current basic block set when compiling bool and.".into(),
                        None,
                    ));
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::BoolAnd: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_bin_op_bool_or(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        left: BasicValueEnum<'ctx>,
        right_expr: &mut Expr,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // TODO: Const.
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if let Some(cur_basic_block) = self.cur_basic_block {
                    // The eval block is to evaluate the rhs only if the lhs
                    // evaluates to false to allow for short circuit.
                    let eval_block = self
                        .context
                        .insert_basic_block_after(cur_basic_block, "bool.or.rhs");
                    let phi_block = self
                        .context
                        .insert_basic_block_after(eval_block, "bool.or.merge");

                    // The old "if.case" will be used as a branch block.
                    self.builder.position_at_end(cur_basic_block);
                    self.builder.build_conditional_branch(
                        left.into_int_value(),
                        phi_block,
                        eval_block,
                    );

                    // TODO: Must be a better way to do this branch. Currently
                    //       this eval block only exists to compile the expr and
                    //       then jump into a phi-block.
                    self.builder.position_at_end(eval_block);
                    let right = self.compile_expr(right_expr, ExprTy::RValue)?;
                    self.builder.build_unconditional_branch(phi_block);

                    let bool_type = self.context.bool_type();
                    let true_val = bool_type.const_all_ones();

                    self.builder.position_at_end(phi_block);
                    let phi_val = self.builder.build_phi(bool_type, "bool.or.phi");
                    phi_val.add_incoming(&[
                        (&true_val, cur_basic_block),
                        (&right.into_int_value(), eval_block),
                    ]);

                    self.cur_basic_block = Some(phi_block);
                    phi_val.as_basic_value().into()
                } else {
                    return Err(self.err(
                        "No current basic block set when compiling bool or.".into(),
                        None,
                    ));
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for BinaryOperator::BoolOr: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    /// Dereferences the given expression. This function will return a pointer
    /// to a allocation containing the actual value. If one wants the pointer
    /// to the value or the value itself is up to the caller.
    fn compile_un_op_deref(&mut self, un_op: &mut UnOp) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        if any_value.is_pointer_value() {
            // TODO: Find better way to do this.
            // Edge case if this is a pointer value containing a struct value.
            // Struct values should ALWAYS be wrapped in a pointer, otherwise
            // the members can't be accessed. Prevent deref of the pointer if
            // this is the case.
            let ptr = any_value.into_pointer_value();
            if let AnyTypeEnum::StructType(_) = ptr.get_type().get_element_type() {
                Ok(any_value)
            } else {
                Ok(self.builder.build_load(ptr, "deref").into())
            }
        } else {
            Err(self.err(
                format!("Tried to deref non pointer type expr: {:?}", un_op),
                None,
            ))
        }
    }

    fn compile_un_op_address(&mut self, un_op: &mut UnOp) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        if any_value.is_pointer_value() {
            Ok(any_value)
        } else {
            match any_value {
                // TODO: Move this kind of logic to into a new analyzing stage.
                AnyValueEnum::StructValue(_) => {
                    let ptr = self.builder.build_alloca(
                        any_value.get_type().into_struct_type(),
                        "struct.func.ret.alloc",
                    );
                    self.builder.build_store(ptr, any_value.into_struct_value());
                    Ok(ptr.into())
                }
                _ => Err(self.err(
                    format!(
                        "Expr in \"Address\" not a pointer. Un op: {:#?}\nany value: {:#?}",
                        un_op, any_value
                    ),
                    None,
                )),
            }
        }
    }

    /// Access a member of an array. This function will return a pointer
    /// to a allocation containing the actual value. If one wants the pointer
    /// to the value or the value itself is up to the caller.
    fn compile_un_op_array_access(&mut self, un_op: &mut UnOp) -> LangResult<PointerValue<'ctx>> {
        let dim = if let UnOperator::ArrayAccess(dim) = &mut un_op.operator {
            dim
        } else {
            return Err(self.err(
                format!(
                    "Un op not array access when compilig array access: {:?}",
                    un_op
                ),
                None,
            ));
        };

        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        let ptr = if any_value.is_pointer_value() {
            any_value.into_pointer_value()
        } else if let AnyTypeEnum::ArrayType(_) = any_value.get_type() {
            // Need to wrap any array types with a temporary alloc to
            // access its members.
            let ptr = self
                .builder
                .build_alloca(any_value.get_type().into_array_type(), "array.type.access");
            self.builder.build_store(ptr, any_value.into_array_value());
            ptr
        } else {
            return Err(self.err(
                format!("Expr in array access not a pointer: {:?}", un_op),
                None,
            ));
        };

        debug!(
            "Compiling array access -- expr: {:#?}\nptr: {:#?}\ndim: {:?}",
            un_op.value, ptr, dim
        );

        let compiled_dim = self.compile_expr(dim, ExprTy::RValue)?;
        if !compiled_dim.is_int_value() {
            return Err(self.err(
                format!("Dim in array access didn't eval to int: {:?}", un_op),
                None,
            ));
        }

        debug!("Compiled dim: {:?}", compiled_dim);

        // Need to index the pointer to the array first, so add a extra zero
        // before the actual indexing (2d index).
        let sign_extend = false;
        let zero = self.context.i32_type().const_int(0, sign_extend);

        Ok(unsafe {
            self.builder
                .build_gep(ptr, &[zero, compiled_dim.into_int_value()], "array.gep")
        })
    }

    /// This function accessed the member at index `idx_opt` for the ADT in
    /// expression `expr`. The returned value will be a pointer to the allocated
    /// member, so the caller would have to do a load if they want the actual value.
    fn compile_un_op_adt_access(
        &mut self,
        un_op: &mut UnOp,
        expr_ty: ExprTy,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let idx = if let UnOperator::AdtAccess(_, idx) = &un_op.operator {
            idx.unwrap()
        } else {
            unreachable!();
        };

        let adt_ty = un_op.value.get_expr_type()?;
        let adt_name = if let Ty::CompoundType(inner_ty, generics, ..) = &adt_ty {
            if inner_ty.is_adt() {
                util::to_generic_name(&inner_ty.get_ident().unwrap(), generics)
            } else {
                return Err(self.analyze_context.err(format!(
                    "Expression that was ADT accessed wasn't ADT, was: {:#?}",
                    un_op.value
                )));
            }
        } else {
            return Err(self.analyze_context.err(format!(
                "Expression that was ADT accessed wasn't Compound, was: {:#?}",
                un_op.value
            )));
        };

        // TODO: Don't hardcode default id.
        let id = BlockInfo::DEFAULT_BLOCK_ID;

        if self.analyze_context.is_struct(&adt_name, id) {
            self.compile_un_op_struct_access(un_op, expr_ty, idx as u32)
        } else if self.analyze_context.is_union(&adt_name, id) {
            let union_ = self.analyze_context.get_adt(&adt_name, id)?;
            self.compile_un_op_union_access(un_op, expr_ty, idx as u32, Rc::clone(&union_))
        } else {
            unreachable!("{:#?}", un_op);
        }
    }

    fn compile_un_op_struct_access(
        &mut self,
        un_op: &mut UnOp,
        expr_ty: ExprTy,
        idx: u32,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let mut any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        debug!(
            "Compiling struct access, idx: {} -- un_op: {:#?}\nany_value: {:#?}",
            idx, un_op, any_value
        );

        // TODO: Better way to do this? Can one skip this stack allocation
        //       in some way and GEP the value without a pointer?
        // If the value is given as a struct value and it isn't a const, it needs
        // to be stored temporarily on the stack to get a pointer which can then
        // be non-const GEPd.
        if any_value.is_struct_value() && !un_op.is_const {
            let ptr = self
                .builder
                .build_alloca(any_value.into_struct_value().get_type(), "struct.tmp.alloc");
            self.builder
                .build_store(ptr, CodeGen::any_into_basic_value(any_value)?);
            any_value = ptr.into();
        }

        if any_value.is_pointer_value() {
            let ptr = any_value.into_pointer_value();
            self.builder
                .build_struct_gep(ptr, idx, "struct.gep")
                .map(|val| val.into())
                .map_err(|_| {
                    self.err(
                        format!("Unable to gep for struct member index: {:?}.", un_op),
                        None,
                    )
                })
        } else if any_value.is_struct_value() {
            // Known to be const at this point, so safe to "const extract".
            if let ExprTy::RValue = expr_ty {
                Ok(any_value
                    .into_struct_value()
                    .const_extract_value(&mut [idx])
                    .into())
            } else {
                Err(self.err(
                    format!("StructValue not allowed in lvalue: {:#?}", un_op),
                    None,
                ))
            }
        } else {
            Err(self.err(
                format!(
                    "Expr in struct access not a pointer or struct. Un up: {:#?}\ncompiled expr: {:#?}",
                    un_op, any_value
                ),
                None,
            ))
        }
    }

    // TODO: const logic.
    fn compile_un_op_union_access(
        &mut self,
        un_op: &mut UnOp,
        expr_ty: ExprTy,
        idx: u32,
        union_: Rc<RefCell<Adt>>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let address_space = AddressSpace::Generic;

        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        debug!(
            "Compiling union access, idx: {} -- un_op: {:#?}\nany_value: {:#?}",
            idx, un_op, any_value
        );

        let ty = union_
            .borrow()
            .members
            .get(idx as usize)
            .unwrap()
            .borrow()
            .ty
            .clone()
            .unwrap();
        let member_ty = self.compile_type(&ty, ty.file_pos().cloned())?;
        let basic_member_ty = CodeGen::any_into_basic_type(member_ty)?;

        let data_idx = 1;
        if any_value.is_pointer_value() {
            let ptr = any_value.into_pointer_value();
            let member_ptr = self
                .builder
                .build_struct_gep(ptr, data_idx, "union.gep")
                .map_err(|_| {
                    self.err(
                        format!("Unable to gep for union member index: {:?}.", un_op),
                        None,
                    )
                })?;

            Ok(self
                .builder
                .build_pointer_cast(
                    member_ptr,
                    basic_member_ty.ptr_type(address_space),
                    "union.ptr.cast",
                )
                .into())
        } else if any_value.is_struct_value() {
            // Known to be const at this point, so safe to "const extract".
            if let ExprTy::RValue = expr_ty {
                let member_value = any_value
                    .into_struct_value()
                    .const_extract_value(&mut [idx]);

                // TODO: Does this work?
                Ok(self
                    .builder
                    .build_bitcast(member_value, basic_member_ty, "union.bitcast")
                    .into())
            } else {
                Err(self.err(
                    format!("StructValue not allowed in lvalue: {:#?}", un_op),
                    None,
                ))
            }
        } else {
            Err(self.err(
                format!(
                    "Expr in union access not a pointer or struct. Un up: {:#?}\ncompiled expr: {:#?}",
                    un_op, any_value
                ),
                None,
            ))
        }
    }

    /// This function "creates" a instance of a member of a specific enum.
    /// This instance will be a const value.
    fn compile_un_op_enum_access(&mut self, un_op: &mut UnOp) -> LangResult<AnyValueEnum<'ctx>> {
        let (member_name, block_id) =
            if let UnOperator::EnumAccess(member_name, block_id) = &un_op.operator {
                (member_name.clone(), *block_id)
            } else {
                return Err(self.err(
                    format!(
                        "Un op not struct access when compilig enum access: {:?}",
                        un_op
                    ),
                    None,
                ));
            };

        // TODO: Does the enum name need to be fetched in some other way in the
        //       future? Will it be ex. prepended with package/module info?
        let enum_name = if let Expr::Type(Ty::CompoundType(inner_ty, ..), ..) = un_op.value.as_ref()
        {
            inner_ty.to_string()
        } else {
            return Err(self.err(
                format!("Unop value in enum access not a enum type: {:#?}", un_op,),
                None,
            ));
        };

        let member = self.analyze_context.get_adt_member(
            &enum_name,
            &member_name,
            block_id,
            un_op.file_pos,
        )?;

        let basic_value = if let Some(mut value) = member.borrow().value.clone() {
            let any_value = self.compile_expr(value.as_mut(), ExprTy::RValue)?;
            CodeGen::any_into_basic_value(any_value)?
        } else {
            return Err(self.err(
                format!(
                    "Member of enum \"{}\" has no value set, member: {:#?}",
                    enum_name, member
                ),
                None,
            ));
        };

        let enum_ty = if let Some(enum_ty) = self.module.get_struct_type(&enum_name) {
            enum_ty
        } else {
            return Err(self.err(
                format!(
                    "Unable to find enum with name \"{}\" in codegen module.",
                    enum_name
                ),
                None,
            ));
        };

        debug!(
            "Compiling enum access -- enum_name: {}, member_name: {}, basic_value: {:#?}",
            enum_name, member_name, basic_value
        );

        Ok(enum_ty.const_named_struct(&[basic_value]).into())
    }

    fn compile_un_op_negative(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        un_op: &mut UnOp,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if un_op.is_const {
                    any_value.into_float_value().const_neg().into()
                } else {
                    self.builder
                        .build_float_neg(any_value.into_float_value(), "neg.float")
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if un_op.is_const {
                    any_value.into_int_value().const_neg().into()
                } else {
                    self.builder
                        .build_int_neg(any_value.into_int_value(), "neg.int")
                        .into()
                }
            }
            AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for UnaryOperator::Negative: {:?}", ret_type),
                    None,
                ))
            }
        })
    }

    fn compile_un_op_bool_not(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        un_op: &mut UnOp,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
        un_op.is_const = CodeGen::is_const(&[any_value]);

        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if un_op.is_const {
                    any_value.into_int_value().const_not().into()
                } else {
                    self.builder
                        .build_not(any_value.into_int_value(), "not")
                        .into()
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(
                    format!("Invalid type for UnaryOperator::BoolNot: {:?}", ret_type),
                    None,
                ))
            }
        })
    }
}
