use super::generator::CodeGen;
use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::{
    parse::token::{self, BinaryOperation, Operation},
    CustomResult,
};
use inkwell::{types::AnyTypeEnum, values::AnyValueEnum, FloatPredicate, IntPredicate};
use token::UnaryOperation;

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_op(&mut self, op: &Operation) -> CustomResult<AnyValueEnum<'ctx>> {
        match op {
            Operation::BinaryOperation(bin_op) => self.compile_bin_op(bin_op),
            Operation::UnaryOperation(un_op) => self.compile_un_op(un_op),
        }
    }

    // TODO: Currently only ints, make for floats and other types.
    fn compile_bin_op(&mut self, bin_op: &BinaryOperation) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = bin_op.ret_type {
            ret_type.t.to_codegen(&self.context)?
        } else {
            return Err(LangError::new(
                format!(
                    "Type of bin_op \"{:?}\" not know when compiling assignment.",
                    &bin_op
                ),
                CodeGenError,
            ));
        };

        let left_any_value = self.compile_expr(&bin_op.left)?;
        let left = CodeGen::any_into_basic_value(left_any_value)?;

        let right_any_value = self.compile_expr(&bin_op.right)?;
        let right = CodeGen::any_into_basic_value(right_any_value)?;

        Ok(match bin_op.operator {
            token::BinaryOperator::In => panic!("TODO: In"),
            token::BinaryOperator::Is => panic!("TODO: Is"),
            token::BinaryOperator::As => {
                // TODO: Will probably need to check both sides before doing a
                //       cast. For example now, if the ret_type is float,
                //       the left will be casted into a float. But it assumes
                //       that the left side is some sort of float already.
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
            }
            token::BinaryOperator::Of => panic!("TODO: Of"),

            // Create some sort of typedef that then can be used to iterate over.
            token::BinaryOperator::Range => panic!("TODO: Range"),
            token::BinaryOperator::RangeInclusive => panic!("TODO: RangeInclusive"),
            token::BinaryOperator::Dot => panic!("TODO: Dot"),

            token::BinaryOperator::Equals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OEQ;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OEQ.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::EQ;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "EQ.int",
                        )
                        .into()
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
            },

            token::BinaryOperator::NotEquals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::ONE;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "ONE.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::NE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "NE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(LangError::new(
                        format!("Invalid type for BinaryOperator::NotEquals: {:?}", ret_type),
                        CodeGenError,
                    ))
                }
            },

            // TODO: Signed/unsigned compares.
            token::BinaryOperator::LessThan => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OLT;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OLT.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SLE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SLE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(LangError::new(
                        format!("Invalid type for BinaryOperator::LessThan: {:?}", ret_type),
                        CodeGenError,
                    ))
                }
            },

            token::BinaryOperator::GreaterThan => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OGT;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OGT.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SGT;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SGT.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(LangError::new(
                        format!(
                            "Invalid type for BinaryOperator::GreaterThan: {:?}",
                            ret_type
                        ),
                        CodeGenError,
                    ))
                }
            },

            token::BinaryOperator::LessThanOrEquals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OLE;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OLE.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SLE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SLE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(LangError::new(
                        format!(
                            "Invalid type for BinaryOperator::LessThanOrEquals: {:?}",
                            ret_type
                        ),
                        CodeGenError,
                    ))
                }
            },

            token::BinaryOperator::GreaterThanOrEquals => match ret_type {
                AnyTypeEnum::FloatType(_) => {
                    let predicate = FloatPredicate::OGE;
                    self.builder
                        .build_float_compare(
                            predicate,
                            left.into_float_value(),
                            right.into_float_value(),
                            "OGE.float",
                        )
                        .into()
                }
                AnyTypeEnum::IntType(_) => {
                    let predicate = IntPredicate::SGE;
                    self.builder
                        .build_int_compare(
                            predicate,
                            left.into_int_value(),
                            right.into_int_value(),
                            "SGE.int",
                        )
                        .into()
                }
                AnyTypeEnum::ArrayType(_)
                | AnyTypeEnum::FunctionType(_)
                | AnyTypeEnum::PointerType(_)
                | AnyTypeEnum::StructType(_)
                | AnyTypeEnum::VectorType(_)
                | AnyTypeEnum::VoidType(_) => {
                    return Err(LangError::new(
                        format!(
                            "Invalid type for BinaryOperator::GreaterThanOrEquals: {:?}",
                            ret_type
                        ),
                        CodeGenError,
                    ))
                }
            },

            token::BinaryOperator::Addition => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_add(
                        left.into_float_value(),
                        right.into_float_value(),
                        "add.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_add(left.into_int_value(), right.into_int_value(), "add.int")
                    .into(),
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
            },

            token::BinaryOperator::Subtraction => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_sub(
                        left.into_float_value(),
                        right.into_float_value(),
                        "sub.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_sub(left.into_int_value(), right.into_int_value(), "sub.int")
                    .into(),
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
            },

            token::BinaryOperator::Multiplication => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_mul(
                        left.into_float_value(),
                        right.into_float_value(),
                        "mul.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_mul(left.into_int_value(), right.into_int_value(), "mul.int")
                    .into(),
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
            },

            token::BinaryOperator::Division => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_div(
                        left.into_float_value(),
                        right.into_float_value(),
                        "div.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_div(left.into_int_value(), right.into_int_value(), "div.int")
                    .into(),
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
            },

            token::BinaryOperator::Modulus => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_rem(
                        left.into_float_value(),
                        right.into_float_value(),
                        "mod.float",
                    )
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_signed_rem(left.into_int_value(), right.into_int_value(), "mod.int")
                    .into(),
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
            },

            token::BinaryOperator::Power => panic!("TODO: Power"),

            token::BinaryOperator::BitAnd => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_and(left.into_int_value(), right.into_int_value(), "bit.and")
                    .into(),
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
            },

            token::BinaryOperator::BitOr => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_or(left.into_int_value(), right.into_int_value(), "bit.or")
                    .into(),
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
            },

            token::BinaryOperator::BitXor => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_xor(left.into_int_value(), right.into_int_value(), "bit.xor")
                    .into(),
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
            },

            token::BinaryOperator::ShiftLeft => match ret_type {
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_left_shift(left.into_int_value(), right.into_int_value(), "lshift")
                    .into(),
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
            },

            token::BinaryOperator::ShiftRight => match ret_type {
                AnyTypeEnum::IntType(_) => {
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
            },

            token::BinaryOperator::BoolAnd => panic!("TODO: BoolAnd"),
            token::BinaryOperator::BoolOr => panic!("TODO: BooldOr"),

            token::BinaryOperator::ExpressionAnd => panic!("TODO: ExpressionAnd"),
        })
    }

    fn compile_un_op(&mut self, un_op: &UnaryOperation) -> CustomResult<AnyValueEnum<'ctx>> {
        // TODO: Can one always assume that the `ret_type` will be set at this point?
        let ret_type = if let Some(ref ret_type) = un_op.ret_type {
            ret_type.t.to_codegen(&self.context)?
        } else {
            return Err(LangError::new(
                format!(
                    "Type of un_op \"{:?}\" not know when compiling assignment.",
                    &un_op
                ),
                CodeGenError,
            ));
        };

        let any_value = self.compile_expr(&un_op.value)?;
        let value = CodeGen::any_into_basic_value(any_value)?;

        Ok(match un_op.operator {
            token::UnaryOperator::Increment => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = false;
                    let one = ret_type.into_int_type().const_int(1, sign_extend);
                    self.builder
                        .build_int_add(value.into_int_value(), one, "inc")
                        .into()
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
            },

            token::UnaryOperator::Decrement => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    let sign_extend = false;
                    let one = ret_type.into_int_type().const_int(1, sign_extend);
                    self.builder
                        .build_int_sub(value.into_int_value(), one, "dec")
                        .into()
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
            },

            token::UnaryOperator::Deref => {
                panic!("TODO: Deref");
                //self.builder.build_load(ptr, name)
            }
            token::UnaryOperator::Address => {
                panic!("TODO: Address");
            }
            token::UnaryOperator::Positive => {
                // Do nothing.
                value.into()
            }

            token::UnaryOperator::Negative => match ret_type {
                AnyTypeEnum::FloatType(_) => self
                    .builder
                    .build_float_neg(value.into_float_value(), "neg.float")
                    .into(),
                AnyTypeEnum::IntType(_) => self
                    .builder
                    .build_int_neg(value.into_int_value(), "neg.int")
                    .into(),
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
            },

            token::UnaryOperator::BitComplement => panic!("TODO: Bit complement"),
            token::UnaryOperator::BoolNot => match ret_type {
                AnyTypeEnum::IntType(_) => {
                    self.builder.build_not(value.into_int_value(), "not").into()
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
            },
        })
    }
}
