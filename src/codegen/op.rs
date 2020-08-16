use super::generator::CodeGen;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::{
    parse::token::{self, BinaryOperation, Operation},
    CustomResult,
};
use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use token::{BinaryOperator, UnaryOperation, UnaryOperator};

// TODO: Check constness for operators. Ex. adding two consts should use a
//       const add instruction so that the result also is const.

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_op(&mut self, op: &mut Operation) -> CustomResult<AnyValueEnum<'ctx>> {
        match op {
            Operation::BinaryOperation(ref mut bin_op) => self.compile_bin_op(bin_op),
            Operation::UnaryOperation(ref mut un_op) => self.compile_un_op(un_op),
        }
    }

    // TODO: Currently only ints, make for floats and other types.
    fn compile_bin_op(&mut self, bin_op: &mut BinaryOperation) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = bin_op.ret_type {
            self.compile_type(&ret_type.t)?
        } else {
            return Err(LangError::new(
                format!(
                    "Type of bin_op \"{:?}\" not know when compiling assignment.",
                    &bin_op
                ),
                CodeGenError,
            ));
        };

        let left_any_value = self.compile_expr(&mut bin_op.left)?;
        let left = CodeGen::any_into_basic_value(left_any_value)?;

        let right_any_value = self.compile_expr(&mut bin_op.right)?;
        let right = CodeGen::any_into_basic_value(right_any_value)?;

        bin_op.is_const = self.is_const(&[&left, &right]);

        let left_type = left.get_type();
        let right_type = right.get_type();

        // Check to see if left and right have the same base type so that they
        // can interact correctly in expressions. For some operations this is
        // not required and they are therefore excluded from the base type check.
        match bin_op.operator {
            BinaryOperator::In
            | BinaryOperator::Is
            | BinaryOperator::As
            | BinaryOperator::Of
            | BinaryOperator::Dot => {}

            _ => {
                if !self.is_same_base_type(left_type, right_type) {
                    return Err(LangError::new(
                    format!(
                        "Left & right type different base types during as. left_type: {:?}, right_type: {:?}",
                        left_type,
                        right_type,
                    ),
                    CodeGenError,
                ));
                }
            }
        }

        Ok(match bin_op.operator {
            BinaryOperator::In => panic!("TODO: In"),
            BinaryOperator::Is => panic!("TODO: Is"),
            BinaryOperator::As => self.compile_bin_op_as(ret_type, bin_op.is_const, left, right)?,
            BinaryOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            BinaryOperator::Range => panic!("TODO: Range"),
            BinaryOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            BinaryOperator::Dot => self.compile_bin_op_dot(ret_type, left, right)?,

            BinaryOperator::Equals
            | BinaryOperator::NotEquals
            | BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessThanOrEquals
            | BinaryOperator::GreaterThanOrEquals => self.compile_bin_op_compare(
                ret_type,
                bin_op.is_const,
                &bin_op.operator,
                left,
                right,
            )?,

            BinaryOperator::Addition => {
                self.compile_bin_op_addition(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::Subtraction => {
                self.compile_bin_op_subtraction(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::Multiplication => {
                self.compile_bin_op_multiplication(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::Division => {
                self.compile_bin_op_division(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::Modulus => {
                self.compile_bin_op_modulus(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::Power => panic!("TODO: Power"),
            BinaryOperator::BitAnd => {
                self.compile_bin_op_bit_and(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::BitOr => {
                self.compile_bin_op_bit_or(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::BitXor => {
                self.compile_bin_op_bit_xor(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::ShiftLeft => {
                self.compile_bin_op_shift_left(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::ShiftRight => {
                self.compile_bin_op_shift_right(ret_type, bin_op.is_const, left, right)?
            }
            BinaryOperator::BoolAnd => panic!("TODO: BoolAnd"),
            BinaryOperator::BoolOr => panic!("TODO: BooldOr"),
            BinaryOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
        })
    }

    fn compile_un_op(&mut self, un_op: &mut UnaryOperation) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = un_op.ret_type {
            self.compile_type(&ret_type.t)?
        } else {
            return Err(LangError::new(
                format!(
                    "Type of un_op \"{:?}\" not know when compiling assignment.",
                    &un_op
                ),
                CodeGenError,
            ));
        };

        let any_value = self.compile_expr(&mut un_op.value)?;
        let value = CodeGen::any_into_basic_value(any_value)?;

        un_op.is_const = self.is_const(&[&value]);

        Ok(match un_op.operator {
            UnaryOperator::Increment => {
                self.compile_un_op_increment(ret_type, un_op.is_const, value)?
            }
            UnaryOperator::Decrement => {
                self.compile_un_op_decrement(ret_type, un_op.is_const, value)?
            }
            UnaryOperator::Deref => {
                panic!("TODO: Deref");
                //self.builder.build_load(ptr, name)
            }
            UnaryOperator::Address => {
                panic!("TODO: Address");
            }
            UnaryOperator::Positive => {
                // Do nothing.
                value.into()
            }
            UnaryOperator::Negative => {
                self.compile_un_op_negative(ret_type, un_op.is_const, value)?
            }
            UnaryOperator::BitComplement => panic!("TODO: Bit complement"),
            UnaryOperator::BoolNot => {
                self.compile_un_op_bool_not(ret_type, un_op.is_const, value)?
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
        let right_type = right.get_type();

        // TODO: Figure out sign extensions.

        Ok(if is_const {
            match right_type {
                BasicTypeEnum::ArrayType(ty) => panic!("TODO: Const array \"as\"."),
                BasicTypeEnum::FloatType(ty) => left.into_float_value().const_cast(ty).into(),
                BasicTypeEnum::IntType(ty) => {
                    let is_signed = true;
                    left.into_int_value().const_cast(ty, is_signed).into()
                }
                BasicTypeEnum::PointerType(ty) => left.into_pointer_value().const_cast(ty).into(),
                BasicTypeEnum::StructType(ty) => panic!("TODO: Const struct \"as\"."),
                BasicTypeEnum::VectorType(ty) => panic!("TODO: Const vector \"as\"."),
            }
        } else {
            match ret_type {
                AnyTypeEnum::ArrayType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.array").into()
                }
                AnyTypeEnum::FloatType(ty) => self
                    .builder
                    .build_float_cast(left.into_float_value(), ty, "cast.float")
                    .into(),
                AnyTypeEnum::IntType(ty) => self
                    .builder
                    .build_int_cast(left.into_int_value(), ty, "cast.int")
                    .into(),
                AnyTypeEnum::PointerType(ty) => self
                    .builder
                    .build_pointer_cast(left.into_pointer_value(), ty, "cast.ptr")
                    .into(),
                AnyTypeEnum::StructType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.struct").into()
                }
                AnyTypeEnum::VectorType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.vector").into()
                }
                AnyTypeEnum::FunctionType(_) => panic!("TODO: compile_bin_op function type."),
                AnyTypeEnum::VoidType(_) => panic!("TODO: compile_bin_op void type."),
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
                return Err(LangError::new(
                    format!("Bad left type in Dot bin op: {:?}", left_type),
                    CodeGenError,
                ))
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
        op: &BinaryOperator,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Add compares for more types.
        Ok(match ret_type {
            AnyTypeEnum::FloatType(_) => {
                let predicate = match op {
                    BinaryOperator::Equals => FloatPredicate::OEQ,
                    BinaryOperator::NotEquals => FloatPredicate::ONE,
                    BinaryOperator::LessThan => FloatPredicate::OLT,
                    BinaryOperator::GreaterThan => FloatPredicate::OGT,
                    BinaryOperator::LessThanOrEquals => FloatPredicate::OLE,
                    BinaryOperator::GreaterThanOrEquals => FloatPredicate::OGE,
                    _ => {
                        return Err(LangError::new(
                            format!("Invalid operator in bin_op compare: {:?}", op),
                            CodeGenError,
                        ))
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
                    BinaryOperator::Equals => IntPredicate::EQ,
                    BinaryOperator::NotEquals => IntPredicate::NE,
                    BinaryOperator::LessThan => IntPredicate::SLT,
                    BinaryOperator::GreaterThan => IntPredicate::SGT,
                    BinaryOperator::LessThanOrEquals => IntPredicate::SLE,
                    BinaryOperator::GreaterThanOrEquals => IntPredicate::SGE,
                    _ => {
                        return Err(LangError::new(
                            format!("Invalid operator in bin_op compare: {:?}", op),
                            CodeGenError,
                        ))
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::Equals: {:?}", ret_type),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::Addition: {:?}", ret_type),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!(
                        "Invalid type for BinaryOperator::Subtraction: {:?}",
                        ret_type
                    ),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!(
                        "Invalid type for BinaryOperator::Multiplication: {:?}",
                        ret_type
                    ),
                    CodeGenError,
                ))
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::Division: {:?}", ret_type),
                    CodeGenError,
                ))
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::Modulus: {:?}", ret_type),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::BitAnd: {:?}", ret_type),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::BitOr: {:?}", ret_type),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::BitXor: {:?}", ret_type),
                    CodeGenError,
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
                return Err(LangError::new(
                    format!("Invalid type for BinaryOperator::ShiftLeft: {:?}", ret_type),
                    CodeGenError,
                ))
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
                return Err(LangError::new(
                    format!(
                        "Invalid type for BinaryOperator::ShiftRight: {:?}",
                        ret_type
                    ),
                    CodeGenError,
                ))
            }
        })
    }

    fn compile_un_op_increment(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        value: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                let sign_extend = false;
                let one = ret_type.into_int_type().const_int(1, sign_extend);
                if is_const {
                    value.into_int_value().const_add(one).into()
                } else {
                    self.builder
                        .build_int_add(value.into_int_value(), one, "inc")
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
                return Err(LangError::new(
                    format!("Invalid type for UnaryOperator::Increment: {:?}", ret_type),
                    CodeGenError,
                ))
            }
        })
    }

    fn compile_un_op_decrement(
        &mut self,
        ret_type: AnyTypeEnum<'ctx>,
        is_const: bool,
        value: BasicValueEnum<'ctx>,
    ) -> CustomResult<AnyValueEnum<'ctx>> {
        Ok(match ret_type {
            AnyTypeEnum::IntType(_) => {
                let sign_extend = false;
                let one = ret_type.into_int_type().const_int(1, sign_extend);
                if is_const {
                    value.into_int_value().const_sub(one).into()
                } else {
                    self.builder
                        .build_int_sub(value.into_int_value(), one, "dec")
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
                return Err(LangError::new(
                    format!("Invalid type for UnaryOperator::Decrement: {:?}", ret_type),
                    CodeGenError,
                ))
            }
        })
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
                return Err(LangError::new(
                    format!("Invalid type for UnaryOperator::Negative: {:?}", ret_type),
                    CodeGenError,
                ))
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
                return Err(LangError::new(
                    format!("Invalid type for UnaryOperator::BoolNot: {:?}", ret_type),
                    CodeGenError,
                ))
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
            if is_const {
                return true;
            }
        }
        false
    }
}
