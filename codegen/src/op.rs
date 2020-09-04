// TODO: Check constness for operators. Ex. adding two consts should use a
//       const add instruction so that the result also is const.

use crate::generator::CodeGen;
use common::{
    error::CustomResult,
    token::{
        expr::{Expression, Var},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
};
use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValueEnum},
    FloatPredicate, IntPredicate,
};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_op(&mut self, op: &mut Op) -> CustomResult<AnyValueEnum<'ctx>> {
        match op {
            Op::BinOp(ref mut bin_op) => self.compile_bin_op(bin_op),
            Op::UnOp(ref mut un_op) => self.compile_un_op(un_op),
        }
    }

    fn compile_bin_op(&mut self, bin_op: &mut BinOp) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = bin_op.ret_type {
            self.compile_type(&ret_type)?
        } else {
            return Err(self.err(format!(
                "Type of bin_op \"{:?}\" not know when compiling assignment.",
                &bin_op
            )));
        };

        let left_any_value = self.compile_expr(&mut bin_op.left)?;
        let left = CodeGen::any_into_basic_value(left_any_value)?;

        // Bool "and" and "or" should be short circuit, so they need to be
        // compiled before the right side is compiled. This is a temporary fix
        // to ensure that they are evaluated before the right hand side is
        // compiled. This logic should be merged better with the other operators.
        match bin_op.operator {
            BinOperator::BoolAnd => {
                return self.compile_bin_op_bool_and(ret_type, left, &mut bin_op.right)
            }
            BinOperator::BoolOr => {
                return self.compile_bin_op_bool_or(ret_type, left, &mut bin_op.right)
            }
            _ => (),
        }

        let right_any_value = self.compile_expr(&mut bin_op.right)?;
        let right = CodeGen::any_into_basic_value(right_any_value)?;

        bin_op.is_const = self.is_const(&[&left, &right]);

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
            | BinOperator::Dot => {}

            _ => {
                if !self.is_same_base_type(left_type, right_type) {
                    return Err(self.err(
                    format!(
                        "Left & right type different base types during bin op. left_type: {:?}, right_type: {:?}",
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
            BinOperator::Power => panic!("TODO: Power"),
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
            BinOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
        })
    }

    fn compile_un_op(&mut self, un_op: &mut UnOp) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = un_op.ret_type {
            self.compile_type(&ret_type)?
        } else {
            return Err(self.err(format!(
                "Type of un_op \"{:?}\" not know when compiling assignment.",
                &un_op
            )));
        };

        let any_value = self.compile_expr(&mut un_op.value)?;
        let basic_value = CodeGen::any_into_basic_value(any_value)?;

        un_op.is_const = self.is_const(&[&basic_value]);

        Ok(match un_op.operator {
            UnOperator::Increment => {
                if let Some(var) = un_op.value.eval_to_var() {
                    self.compile_un_op_increment(ret_type, un_op.is_const, basic_value, var)?
                } else {
                    return Err(self.err(format!(
                        "Trying to increment something that is a variable: {:?}",
                        &un_op.value
                    )));
                }
            }
            UnOperator::Decrement => {
                if let Some(var) = un_op.value.eval_to_var() {
                    self.compile_un_op_decrement(ret_type, un_op.is_const, basic_value, var)?
                } else {
                    return Err(self.err(format!(
                        "Trying to decrement something that is a variable: {:?}",
                        &un_op.value
                    )));
                }
            }
            UnOperator::Deref => self.compile_un_op_deref(&mut un_op.value)?,
            UnOperator::Address => {
                if let Some(var) = un_op.value.eval_to_var() {
                    self.compile_un_op_address(var)?
                } else {
                    return Err(self.err(format!(
                        "Trying to dereference invalid type: {:?}",
                        &un_op.value
                    )));
                }
            }
            UnOperator::ArrayAccess(_) => {
                if let Some(var) = un_op.value.eval_to_var() {
                    self.compile_un_op_array_access(var)?
                } else {
                    return Err(
                        self.err(format!("Trying to index invalid type: {:?}", &un_op.value))
                    );
                }
            }
            UnOperator::Positive => {
                // Do nothing.
                basic_value.into()
            }
            UnOperator::Negative => {
                self.compile_un_op_negative(ret_type, un_op.is_const, basic_value)?
            }
            UnOperator::BitComplement => panic!("TODO: Bit complement"),
            UnOperator::BoolNot => {
                self.compile_un_op_bool_not(ret_type, un_op.is_const, basic_value)?
            }
        })
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

    /*
    token::BinaryOperator::GreaterThanOrEquals
    */

    fn compile_bin_op_compare(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        op: &BinOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Add compares for more types.
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
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
                    left.into_float_value()
                        .const_compare(predicate, right.into_float_value())
                        .into()
                } else {
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "compare.float",
                        )
                        .into()
                }
            }

            // TODO: Signness
            AnyTypeEnum::IntType(_) => {
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
                    left.into_int_value()
                        .const_int_compare(predicate, right.into_int_value())
                        .into()
                } else {
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "compare.int",
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
                    "Invalid type for BinaryOperator::Equals: {:?}",
                    ret_type
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
        right_expr: &mut Expression,
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
                    let right = self.compile_expr(right_expr)?;
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
        right_expr: &mut Expression,
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
                    let right = self.compile_expr(right_expr)?;
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

    fn compile_un_op_increment(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        value: BasicValueEnum<'ctx>,
        var: &mut Var,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                let sign_extend = false;
                let one = ret_type.into_int_type().const_int(1, sign_extend);
                let new_value = if is_const {
                    value.into_int_value().const_add(one).into()
                } else {
                    self.builder
                        .build_int_add(value.into_int_value(), one, "inc")
                        .into()
                };

                self.compile_var_store(var, new_value)?;
                new_value.into()
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(format!(
                    "Invalid type for UnaryOperator::Increment: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_un_op_decrement(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        value: BasicValueEnum<'ctx>,
        var: &mut Var,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                let sign_extend = false;
                let one = ret_type.into_int_type().const_int(1, sign_extend);
                let new_value = if is_const {
                    value.into_int_value().const_sub(one).into()
                } else {
                    self.builder
                        .build_int_sub(value.into_int_value(), one, "dec")
                        .into()
                };

                self.compile_var_store(var, new_value)?;
                new_value.into()
            }
            AnyTypeEnum::FloatType(_)
            | AnyTypeEnum::ArrayType(_)
            | AnyTypeEnum::FunctionType(_)
            | AnyTypeEnum::PointerType(_)
            | AnyTypeEnum::StructType(_)
            | AnyTypeEnum::VectorType(_)
            | AnyTypeEnum::VoidType(_) => {
                return Err(self.err(format!(
                    "Invalid type for UnaryOperator::Decrement: {:?}",
                    ret_type
                )))
            }
        })
    }

    fn compile_un_op_deref(&mut self, value: &mut Expression) -> CustomResult<AnyValueEnum<'ctx>> {
        if let Some(var) = value.eval_to_var() {
            Ok(self.compile_var_load(var)?.into())
        } else {
            Err(self.err(format!("Value didn't eval to var in deref: {:?}", value)))
        }
    }

    fn compile_un_op_address(&mut self, var: &mut Var) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(self.compile_var_load(var)?.into())
    }

    fn compile_un_op_array_access(
        &mut self,
        var: &mut Var,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(self.compile_var_load(var)?.into())
        /*
        let block_id = self.cur_block_id;
        let decl_block_id = self
            .analyze_context
            .get_var_decl_scope(&var.name, block_id)?;
        let key = (var.name.clone(), decl_block_id);

        let compiled_dim = self.compile_expr(dim)?;
        if !compiled_dim.is_int_value() {
            return Err(self.err(
                format!("Dim in array didn't compile to int: {:?}", &dim),
            ));
        }

        if let Some(var_ptr) = self.variables.get(&key) {
            unsafe {
                Ok(self
                    .builder
                    .build_gep(*var_ptr, &[compiled_dim.into_int_value()], "array.gep")
                    .into())
            }
        } else {
            Err(self.err(
                format!(
                    "Unable to find var \"{}\" in decl block id {}.",
                    &var.name, decl_block_id
                ),

            ))
        }
        */
    }

    fn compile_un_op_negative(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        value: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                if is_const {
                    value.into_float_value().const_neg().into()
                } else {
                    self.builder
                        .build_float_neg(value.into_float_value(), "neg.float")
                        .into()
                }
            }
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    value.into_int_value().const_neg().into()
                } else {
                    self.builder
                        .build_int_neg(value.into_int_value(), "neg.int")
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
        is_const: bool,
        value: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                if is_const {
                    value.into_int_value().const_not().into()
                } else {
                    self.builder.build_not(value.into_int_value(), "not").into()
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
    fn is_const(&self, values: &[&BasicValueEnum<'ctx>]) -> bool {
        for value in values.iter() {
            let is_const = match *value {
                BasicValueEnum::ArrayValue(val) => val.is_const(),
                BasicValueEnum::IntValue(val) => val.is_const(),
                BasicValueEnum::FloatValue(val) => val.is_const(),
                BasicValueEnum::PointerValue(val) => val.is_const(),
                BasicValueEnum::StructValue(val) => {
                    // TODO: Should probably be some way to iterate through all its member
                    //       recursively and figure out if all fields of the struct is
                    //       const. If that is the case, one can assume that the struct
                    //       is also const.
                    // If the struct has no name, this is a constant struct
                    // according to inkwell documentation.
                    val.get_name().to_bytes().is_empty()
                }
                BasicValueEnum::VectorValue(val) => val.is_const(),
            };
            if !is_const {
                return false;
            }
        }
        true
    }
}
