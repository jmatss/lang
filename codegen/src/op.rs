// TODO: Check constness for operators. Ex. adding two consts should use a
//       const add instruction so that the result also is const.

use crate::{expr::ExprTy, generator::CodeGen};
use common::{
    error::CustomResult,
    token::{
        expr::Expr,
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
};
use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::PointerValue,
    values::{AnyValueEnum, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use log::debug;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_op(
        &mut self,
        op: &mut Op,
        expr_ty: ExprTy,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        match op {
            Op::BinOp(ref mut bin_op) => match expr_ty {
                ExprTy::LValue => Err(self.err(format!("Bin op not allowed in lvalue: {:?}", op))),
                ExprTy::RValue => self.compile_bin_op(bin_op),
            },
            Op::UnOp(ref mut un_op) => self.compile_un_op(un_op, expr_ty),
        }
    }

    fn compile_bin_op(&mut self, bin_op: &mut BinOp) -> CustomResult<AnyValueEnum<'ctx>> {
        let ret_type = if let Some(ref ret_type) = bin_op.ret_type {
            self.compile_type(&ret_type)?
        } else {
            return Err(self.err(format!(
                "Type of bin_op \"{:?}\" not know when compiling assignment.",
                &bin_op
            )));
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

        bin_op.is_const = self.is_const(&[&left_any, &right_any]);

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
                ));
                }
            }
        }

        Ok(match bin_op.operator {
            BinOperator::In => panic!("TODO: In"),
            BinOperator::Is => panic!("TODO: Is"),
            BinOperator::As => self.compile_bin_op_as(ret_type, bin_op.is_const, left, right)?,
            BinOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            BinOperator::Range => panic!("TODO: Range"),
            BinOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            BinOperator::Dot => self.compile_bin_op_dot(ret_type, left, right)?,
            BinOperator::DoubleColon => self.compile_bin_op_double_colon(ret_type, left, right)?,

            BinOperator::Equals
            | BinOperator::NotEquals
            | BinOperator::LessThan
            | BinOperator::GreaterThan
            | BinOperator::LessThanOrEquals
            | BinOperator::GreaterThanOrEquals => self.compile_bin_op_compare(
                ret_type,
                bin_op.is_const,
                &bin_op.operator,
                left,
                right,
            )?,

            BinOperator::Addition => {
                self.compile_bin_op_addition(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Subtraction => {
                self.compile_bin_op_subtraction(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Multiplication => {
                self.compile_bin_op_multiplication(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Division => {
                self.compile_bin_op_division(ret_type, bin_op.is_const, left, right)?
            }
            BinOperator::Modulus => {
                self.compile_bin_op_modulus(ret_type, bin_op.is_const, left, right)?
            }
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
            BinOperator::ShiftRight => {
                self.compile_bin_op_shift_right(ret_type, bin_op.is_const, left, right)?
            }
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
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let ret_type = if let Some(ref ret_type) = un_op.ret_type {
            self.compile_type(&ret_type)?
        } else {
            return Err(self.err(format!(
                "Type of un_op \"{:?}\" not know when compiling assignment.",
                &un_op
            )));
        };

        if let ExprTy::LValue = expr_ty {
            match &un_op.operator {
                UnOperator::Address
                | UnOperator::Positive
                | UnOperator::Negative
                | UnOperator::BitComplement
                | UnOperator::BoolNot => {
                    return Err(self.err(format!("Invalid un op in lvalue: {:?}", un_op)))
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
                            // TODO: Find better way to do this.
                            // Edge case if this is a pointer value containing a struct value.
                            // Struct values should ALWAYS be wrapped in a pointer, otherwise
                            // the members can't be accessed. Prevent deref of the pointer if
                            // this is the case.
                            let ptr = any_value.into_pointer_value();
                            if let AnyTypeEnum::StructType(_) = ptr.get_type().get_element_type() {
                                Ok(any_value)
                            } else {
                                Ok(self.builder.build_load(ptr, "deref.rval").into())
                            }
                        } else {
                            Err(self.err(format!(
                                "Tried to deref non pointer in rvalue {:?}",
                                any_value
                            )))
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
            UnOperator::StructAccess(..) => {
                let ptr = self.compile_un_op_struct_access(un_op)?;
                match expr_ty {
                    ExprTy::LValue => Ok(ptr.into()),
                    ExprTy::RValue => Ok(self.builder.build_load(ptr, "struct.gep.rval").into()),
                }
            }
            UnOperator::Positive => {
                let any_value = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
                un_op.is_const = self.is_const(&[&any_value]);

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
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                        return Err(self.err(format!(
                            "Invalid type when casting \"as\" float: {:?}",
                            left_type
                        )));
                    }
                }
                BasicTypeEnum::IntType(ty) => {
                    if left_type.is_float_type() {
                        left.into_float_value().const_to_signed_int(ty).into()
                    } else if left_type.is_int_type() {
                        let is_signed = true;
                        left.into_int_value().const_cast(ty, is_signed).into()
                    } else {
                        return Err(self.err(format!(
                            "Invalid type when casting \"as\" int: {:?}",
                            left_type
                        )));
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

    fn compile_bin_op_dot(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let left_type = left.get_type();
        Ok(match left_type {
            BasicTypeEnum::StructType(_) => {
                // TODO: FIXME: The right side will have been a struct gep
                //              and should contain the content value that we
                //              want to return. Is this a valid assumption?
                right.into()
            }

            BasicTypeEnum::ArrayType(_)
            | BasicTypeEnum::PointerType(_)
            | BasicTypeEnum::VectorType(_)
            | BasicTypeEnum::FloatType(_)
            | BasicTypeEnum::IntType(_) => {
                return Err(self.err(format!("Bad left type in Dot bin op: {:?}", left_type)))
            }
        })
    }

    fn compile_bin_op_double_colon(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        panic!("TODO: Implement static variables.")
    }

    fn compile_bin_op_compare(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        op: &BinOperator,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        // Ensure that lhs and rhs has the same type.
        match (lhs.get_type(), rhs.get_type()) {
            (BasicTypeEnum::ArrayType(_), BasicTypeEnum::ArrayType(_))
            | (BasicTypeEnum::FloatType(_), BasicTypeEnum::FloatType(_))
            | (BasicTypeEnum::IntType(_), BasicTypeEnum::IntType(_))
            | (BasicTypeEnum::PointerType(_), BasicTypeEnum::PointerType(_))
            | (BasicTypeEnum::StructType(_), BasicTypeEnum::StructType(_))
            | (BasicTypeEnum::VectorType(_), BasicTypeEnum::VectorType(_)) => {}

            _ => {
                return Err(self.err(format!(
                    "Lhs and rhs of bin op compare not same type. Lhs: {:?}, rhs: {:?}",
                    lhs, rhs
                )))
            }
        }

        // TODO: Add compares for more types.
        Ok(match lhs.get_type() {
            BasicTypeEnum::FloatType(_) => {
                let predicate = match op {
                    BinOperator::Equals => FloatPredicate::OEQ,
                    BinOperator::NotEquals => FloatPredicate::ONE,
                    BinOperator::LessThan => FloatPredicate::OLT,
                    BinOperator::GreaterThan => FloatPredicate::OGT,
                    BinOperator::LessThanOrEquals => FloatPredicate::OLE,
                    BinOperator::GreaterThanOrEquals => FloatPredicate::OGE,
                    _ => {
                        return Err(
                            self.err(format!("Invalid operator in bin_op compare: {:?}", op))
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
                    BinOperator::Equals => IntPredicate::EQ,
                    BinOperator::NotEquals => IntPredicate::NE,
                    BinOperator::LessThan => IntPredicate::SLT,
                    BinOperator::GreaterThan => IntPredicate::SGT,
                    BinOperator::LessThanOrEquals => IntPredicate::SLE,
                    BinOperator::GreaterThanOrEquals => IntPredicate::SGE,
                    _ => {
                        return Err(
                            self.err(format!("Invalid operator in bin_op compare: {:?}", op))
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

            _ => {
                return Err(self.err(format!(
                    "Invalid type used in bin op compare: {:?}",
                    lhs.get_type()
                )))
            }
        })
    }

    fn compile_bin_op_addition(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::Addition: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_subtraction(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::Subtraction: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_multiplication(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::Multiplication: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_division(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                if is_const {
                    left.into_int_value()
                        .const_signed_div(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_signed_div(
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::Division: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_modulus(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                if is_const {
                    left.into_int_value()
                        .const_signed_remainder(right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_signed_rem(
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::Modulus: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_bit_and(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::BitAnd: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_bit_or(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::BitOr: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_bit_xor(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::BitXor: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_shift_left(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::ShiftLeft: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_shift_right(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                // TODO: Sign extension.
                // TODO: What is the difference between "const_ashr" and "const_rshr".
                if is_const {
                    left.into_int_value()
                        .const_ashr(right.into_int_value())
                        .into()
                } else {
                    let sign_extend = true;
                    self.builder
                        .build_right_shift(
                            left.into_int_value(),
                            right.into_int_value(),
                            sign_extend,
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
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::ShiftRight: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_bool_and(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        left: BasicValueEnum<'ctx>,
        right_expr: &mut Expr,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                    return Err(
                        self.err("No current basic block set when compiling bool and.".into())
                    );
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::BoolAnd: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_bin_op_bool_or(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        left: BasicValueEnum<'ctx>,
        right_expr: &mut Expr,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
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
                    return Err(
                        self.err("No current basic block set when compiling bool or.".into())
                    );
                }
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(format!(
                    "Invalid type for BinaryOperator::BoolOr: {:?}",
                    ret_type
                )))
            }
        })
    }

    /// Dereferences the given expression. This function will return a pointer
    /// to a allocation containing the actual value. If one wants the pointer
    /// to the value or the value itself is up to the caller.
    fn compile_un_op_deref(&mut self, un_op: &mut UnOp) -> CustomResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = self.is_const(&[&any_value]);

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
            Err(self.err(format!("Tried to deref non pointer type expr: {:?}", un_op)))
        }
    }

    fn compile_un_op_address(&mut self, un_op: &mut UnOp) -> CustomResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = self.is_const(&[&any_value]);

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
                _ => Err(self.err(format!(
                    "Expr in \"Address\" not a pointer. Un op: {:#?}\nany value: {:#?}",
                    un_op, any_value
                ))),
            }
        }
    }

    /// Access a member of an array. This function will return a pointer
    /// to a allocation containing the actual value. If one wants the pointer
    /// to the value or the value itself is up to the caller.
    fn compile_un_op_array_access(&mut self, un_op: &mut UnOp) -> CustomResult<PointerValue<'ctx>> {
        let dim = if let UnOperator::ArrayAccess(dim) = &mut un_op.operator {
            dim
        } else {
            return Err(self.err(format!(
                "Un op not array access when compilig array access: {:?}",
                un_op
            )));
        };

        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = self.is_const(&[&any_value]);

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
            return Err(self.err(format!("Expr in array access not a pointer: {:?}", un_op)));
        };

        debug!(
            "Compiling array access -- expr: {:#?}\nptr: {:#?}\ndim: {:?}",
            un_op.value, ptr, dim
        );

        let compiled_dim = self.compile_expr(dim, ExprTy::RValue)?;
        if !compiled_dim.is_int_value() {
            return Err(self.err(format!(
                "Dim in array access didn't eval to int: {:?}",
                un_op
            )));
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

    /// This function accessed the member at index `idx_opt` for the struct in
    /// expression `expr`. The returned value will be a pointer to the allocated
    /// member, so the caller would have to do a load if they want the actual value.
    fn compile_un_op_struct_access(
        &mut self,
        un_op: &mut UnOp,
    ) -> CustomResult<PointerValue<'ctx>> {
        let idx = if let UnOperator::StructAccess(_, idx_opt) = un_op.operator {
            if let Some(idx) = idx_opt {
                idx
            } else {
                return Err(self.err(format!(
                    "No index set when compiling struct access: {:?}",
                    un_op
                )));
            }
        } else {
            return Err(self.err(format!(
                "Un op not struct access when compilig struct access: {:?}",
                un_op
            )));
        };

        let any_value = self.compile_expr(&mut un_op.value, ExprTy::LValue)?;
        un_op.is_const = self.is_const(&[&any_value]);

        let ptr = if any_value.is_pointer_value() {
            any_value.into_pointer_value()
        } else if let AnyTypeEnum::StructType(_) = any_value.get_type() {
            // Need to wrap any struct types with a temporary alloc to
            // access its members.
            let ptr = self.builder.build_alloca(
                any_value.get_type().into_struct_type(),
                "struct.type.access",
            );
            self.builder.build_store(ptr, any_value.into_struct_value());
            ptr
        } else {
            return Err(self.err(format!(
                "Expr in struct access not a pointer. Un up: {:#?}\ncompiled expr: {:#?}",
                un_op, any_value
            )));
        };

        debug!(
            "Compilng struct access -- expr: {:#?}\nptr: {:#?}\nidx: {:?}",
            un_op.value, ptr, idx
        );

        self.builder
            .build_struct_gep(ptr, idx as u32, "struct.gep")
            .map_err(|_| {
                self.err(format!(
                    "Unable to gep for struct member index: {:?}.",
                    un_op
                ))
            })
    }

    fn compile_un_op_negative(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        un_op: &mut UnOp,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
        un_op.is_const = self.is_const(&[&any_value]);

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
                return Err(self.err(format!(
                    "Invalid type for UnaryOperator::Negative: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_un_op_bool_not(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        un_op: &mut UnOp,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        let any_value = self.compile_expr(&mut un_op.value, ExprTy::RValue)?;
        un_op.is_const = self.is_const(&[&any_value]);

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
                return Err(self.err(format!(
                    "Invalid type for UnaryOperator::BoolNot: {:?}",
                    ret_type
                )))
            }
        })
    }

    /// Returns true if all basic values in `values` are const.
    fn is_const(&self, values: &[&AnyValueEnum<'ctx>]) -> bool {
        for value in values.iter() {
            let is_const = match *value {
                AnyValueEnum::ArrayValue(val) => val.is_const(),
                AnyValueEnum::IntValue(val) => val.is_const(),
                AnyValueEnum::FloatValue(val) => val.is_const(),
                AnyValueEnum::PointerValue(val) => val.is_const(),
                AnyValueEnum::StructValue(val) => {
                    // TODO: Should probably be some way to iterate through all its member
                    //       recursively and figure out if all fields of the struct is
                    //       const. If that is the case, one can assume that the struct
                    //       is also const.
                    // If the struct has no name, this is a constant struct
                    // according to inkwell documentation.
                    val.get_name().to_bytes().is_empty()
                }
                AnyValueEnum::VectorValue(val) => val.is_const(),

                AnyValueEnum::PhiValue(_)
                | AnyValueEnum::FunctionValue(_)
                | AnyValueEnum::InstructionValue(_) => false,
            };
            if !is_const {
                return false;
            }
        }
        true
    }
}
