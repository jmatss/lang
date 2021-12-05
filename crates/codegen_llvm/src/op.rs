use inkwell::{
    types::{AnyTypeEnum, BasicType, BasicTypeEnum},
    values::{AggregateValue, AnyValueEnum, BasicValueEnum},
    AddressSpace, FloatPredicate, IntPredicate,
};
use log::debug;

use common::error::LangResult;
use ir::{BinOper, Signed, Type, UnOper, Val};

use crate::{
    generator::CodeGen,
    util::{any_into_basic_type, any_into_basic_value, is_const},
};

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub(super) fn compile_bin_op(
        &self,
        oper: &BinOper,
        lhs: &Val,
        rhs: &Val,
        ret_val: &Val,
        signed: Signed,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let lhs_any = self.get_compiled_val(lhs)?;
        let lhs_basic = any_into_basic_value(lhs_any)?;
        let lhs_type = lhs_basic.get_type();

        let rhs_any = self.get_compiled_val(rhs)?;
        let rhs_basic = any_into_basic_value(rhs_any)?;
        let rhs_type = rhs_basic.get_type();

        let ret_type = &ret_val.1;
        let is_signed = matches!(signed, Signed::True);
        let is_const = is_const(&[lhs_any, rhs_any]);

        // Check to see if left and right have the same base type so that they
        // can interact correctly in expressions. For some operations this is
        // not required and they are therefore excluded from the base type check.
        if !matches!(oper, BinOper::As) && !self.is_same_base_type(lhs_type, rhs_type) {
            return Err(self.err(
                format!(
                    "Left & right type different base types during bin op. Op: {:?}\
                        \nleft_type: {:?}\nright_type: {:?}",
                    oper, lhs_type, rhs_type,
                ),
                None,
            ));
        }

        match oper {
            BinOper::As => self.compile_bin_op_cast(ret_type, is_signed, is_const, lhs_basic),
            BinOper::Eq
            | BinOper::Neq
            | BinOper::Lt
            | BinOper::Gt
            | BinOper::Lte
            | BinOper::Gte => {
                self.compile_bin_op_cmp(is_signed, is_const, oper, lhs_basic, rhs_basic)
            }
            BinOper::Add => self.compile_bin_op_add(ret_type, is_const, lhs_basic, rhs_basic),
            BinOper::Sub => self.compile_bin_op_sub(ret_type, is_const, lhs_basic, rhs_basic),
            BinOper::Mul => self.compile_bin_op_mul(ret_type, is_const, lhs_basic, rhs_basic),
            BinOper::Div => {
                self.compile_bin_op_div(ret_type, is_signed, is_const, lhs_basic, rhs_basic)
            }
            BinOper::Mod => {
                self.compile_bin_op_mod(ret_type, is_signed, is_const, lhs_basic, rhs_basic)
            }
            BinOper::BitAnd => {
                self.compile_bin_op_bit_and(ret_type, is_const, lhs_basic, rhs_basic)
            }
            BinOper::BitOr => self.compile_bin_op_bit_or(ret_type, is_const, lhs_basic, rhs_basic),
            BinOper::BitXor => {
                self.compile_bin_op_bit_xor(ret_type, is_const, lhs_basic, rhs_basic)
            }
            BinOper::ShiftLeft => {
                self.compile_bin_op_shift_left(ret_type, is_const, lhs_basic, rhs_basic)
            }
            BinOper::ShiftRight => {
                self.compile_bin_op_shift_right(ret_type, is_signed, is_const, lhs_basic, rhs_basic)
            }
        }
    }

    pub(super) fn compile_un_op(&self, oper: &UnOper, val: &Val) -> LangResult<AnyValueEnum<'ctx>> {
        match oper {
            UnOper::AdtAccess(idx) => self.compile_un_op_adt_access(val, *idx as u32),
            UnOper::ArrayAccess(idx_val) => self.compile_un_op_array_access(val, idx_val),
            UnOper::IsNull => self.compile_un_op_is_null(val),
            UnOper::BitComplement => panic!("TODO: Bit complement"),
            UnOper::BoolNot => self.compile_un_op_bool_not(val),
        }
    }

    fn compile_bin_op_cast(
        &self,
        ret_type: &Type,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // TODO: Will probably need to check both sides before doing a
        //       cast. For example now, if the ret_type is float,
        //       the left will be casted into a float. But it assumes
        //       that the left side is some sort of float already.
        let left_type = left.get_type();

        let any_type = self.compile_type(ret_type)?;
        let basic_type = any_into_basic_type(any_type)?;

        Ok(if is_const {
            match basic_type {
                BasicTypeEnum::ArrayType(_) => self.compile_alloc_cast(left, basic_type),
                BasicTypeEnum::FloatType(ty) => {
                    if left_type.is_float_type() {
                        left.into_float_value().const_cast(ty).into()
                    } else if left_type.is_int_type() {
                        if is_signed {
                            left.into_int_value().const_signed_to_float(ty).into()
                        } else {
                            left.into_int_value().const_unsigned_to_float(ty).into()
                        }
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
                        left.into_int_value().const_cast(ty, is_signed).into()
                    } else {
                        return Err(self.err(
                            format!("Invalid type when casting \"as\" int: {:?}", left_type),
                            None,
                        ));
                    }
                }
                BasicTypeEnum::PointerType(ty) => {
                    if left_type.is_int_type() {
                        left.into_int_value().const_to_pointer(ty).into()
                    } else {
                        left.into_pointer_value().const_cast(ty).into()
                    }
                }
                BasicTypeEnum::StructType(ty) => panic!("TODO: Const struct \"as\"."),
                BasicTypeEnum::VectorType(ty) => panic!("TODO: Const vector \"as\"."),
            }
        } else {
            match basic_type {
                BasicTypeEnum::ArrayType(_) => self.compile_alloc_cast(left, basic_type),
                BasicTypeEnum::FloatType(ty) => {
                    if left_type.is_float_type() {
                        self.builder
                            .build_float_cast(left.into_float_value(), ty, "cast.float")
                            .into()
                    } else if left_type.is_int_type() {
                        if is_signed {
                            self.builder
                                .build_signed_int_to_float(
                                    left.into_int_value(),
                                    ty,
                                    "cast.float.signed",
                                )
                                .into()
                        } else {
                            self.builder
                                .build_unsigned_int_to_float(
                                    left.into_int_value(),
                                    ty,
                                    "cast.float.unsigned",
                                )
                                .into()
                        }
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
                    } else if left_type.is_pointer_type() {
                        self.builder
                            .build_ptr_to_int(left.into_pointer_value(), ty, "cast.int")
                            .into()
                    } else {
                        self.builder.build_bitcast(left, ty, "cast.int").into()
                    }
                }
                BasicTypeEnum::PointerType(ty) => {
                    if left_type.is_int_type() {
                        self.builder
                            .build_int_to_ptr(left.into_int_value(), ty, "cast.ptr")
                            .into()
                    } else if left_type.is_pointer_type() {
                        self.builder
                            .build_pointer_cast(left.into_pointer_value(), ty, "cast.ptr")
                            .into()
                    } else {
                        self.builder.build_bitcast(left, ty, "cast.ptr").into()
                    }
                }
                BasicTypeEnum::StructType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.struct").into()
                }
                BasicTypeEnum::VectorType(ty) => {
                    self.builder.build_bitcast(left, ty, "cast.vector").into()
                }
            }
        })
    }

    /// If we have a cast that can't be done directly because the value and the
    /// type that it is to be cast to isn't compatible, store the value temporarily
    /// on the stack and cast it as a pointer instead.
    fn compile_alloc_cast(
        &self,
        value: BasicValueEnum<'ctx>,
        ty: BasicTypeEnum<'ctx>,
    ) -> AnyValueEnum<'ctx> {
        let ptr = self
            .builder
            .build_alloca(value.get_type(), "alloc.cast.alloc");
        self.builder.build_store(ptr, value);

        let casted_ptr = self.builder.build_pointer_cast(
            ptr,
            ty.ptr_type(AddressSpace::Generic),
            "alloc.cast.ptr",
        );
        self.builder
            .build_load(casted_ptr, "alloc.cast.load")
            .into()
    }

    fn compile_bin_op_cmp(
        &self,
        is_signed: bool,
        is_const: bool,
        bin_oper: &BinOper,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        // When this function is called, we know that `lhs` & `rhs` has the same
        // type, so we can pick any one of them.
        // TODO: Add compares for more types.
        Ok(match lhs.get_type() {
            BasicTypeEnum::FloatType(_) => {
                let predicate = match bin_oper {
                    BinOper::Eq => FloatPredicate::OEQ,
                    BinOper::Neq => FloatPredicate::ONE,
                    BinOper::Lt => FloatPredicate::OLT,
                    BinOper::Gt => FloatPredicate::OGT,
                    BinOper::Lte => FloatPredicate::OLE,
                    BinOper::Gte => FloatPredicate::OGE,
                    _ => {
                        return Err(self.err(
                            format!("Invalid operator in float compare: {:?}", bin_oper),
                            None,
                        ))
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
                let predicate = match bin_oper {
                    BinOper::Eq => IntPredicate::EQ,
                    BinOper::Neq => IntPredicate::NE,
                    BinOper::Lt if is_signed => IntPredicate::SLT,
                    BinOper::Lt if !is_signed => IntPredicate::ULT,
                    BinOper::Gt if is_signed => IntPredicate::SGT,
                    BinOper::Gt if !is_signed => IntPredicate::UGT,
                    BinOper::Lte if is_signed => IntPredicate::SLE,
                    BinOper::Lte if !is_signed => IntPredicate::ULE,
                    BinOper::Gte if is_signed => IntPredicate::SGE,
                    BinOper::Gte if !is_signed => IntPredicate::UGE,
                    _ => {
                        return Err(self.err(
                            format!("Invalid operator in int compare: {:?}", bin_oper),
                            None,
                        ))
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
                let predicate = match bin_oper {
                    BinOper::Eq => IntPredicate::EQ,
                    BinOper::Neq => IntPredicate::NE,
                    BinOper::Lt if is_signed => IntPredicate::SLT,
                    BinOper::Lt if !is_signed => IntPredicate::ULT,
                    BinOper::Gt if is_signed => IntPredicate::SGT,
                    BinOper::Gt if !is_signed => IntPredicate::UGT,
                    BinOper::Lte if is_signed => IntPredicate::SLE,
                    BinOper::Lte if !is_signed => IntPredicate::ULE,
                    BinOper::Gte if is_signed => IntPredicate::SGE,
                    BinOper::Gte if !is_signed => IntPredicate::UGE,
                    _ => {
                        return Err(self.err(
                            format!("Invalid operator in ptr compare: {:?}", bin_oper),
                            None,
                        ))
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

            // Allow struct type if the struct type only has a single member and
            // that member is a type that can be compared (float, int or pointer).
            // This will for example allow compares of enums (since they are
            // compiled down to a struct with a single i8 member).
            BasicTypeEnum::StructType(struct_type) if struct_type.count_fields() == 1 => {
                let field_type = struct_type.get_field_type_at_index(0).unwrap();
                if field_type.is_int_type()
                    || field_type.is_float_type()
                    || field_type.is_pointer_type()
                {
                    let lhs_member = self.compile_struct_access(lhs.into(), 0, is_const)?;
                    let rhs_member = self.compile_struct_access(rhs.into(), 0, is_const)?;
                    self.compile_bin_op_cmp(
                        is_signed,
                        is_const,
                        bin_oper,
                        any_into_basic_value(lhs_member)?,
                        any_into_basic_value(rhs_member)?,
                    )?
                } else {
                    return Err(self.err(
                        format!(
                            "Invalid struct type used in bin op compare: {:?}",
                            lhs.get_type()
                        ),
                        None,
                    ));
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

    fn compile_bin_op_add(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_float() {
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
        } else if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_add(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_int_add(left.into_int_value(), right.into_int_value(), "add.int")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::Add: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_sub(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_float() {
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
        } else if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_sub(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_int_sub(left.into_int_value(), right.into_int_value(), "sub.int")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::Sub: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_mul(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_float() {
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
        } else if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_mul(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_int_mul(left.into_int_value(), right.into_int_value(), "mul.int")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::Mul: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_div(
        &self,
        ret_type: &Type,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_float() {
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
        } else if ret_type.is_int() {
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
                    .build_int_signed_div(left.into_int_value(), right.into_int_value(), "div.int")
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
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::Div: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_mod(
        &self,
        ret_type: &Type,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_float() {
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
        } else if ret_type.is_int() {
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
                    .build_int_signed_rem(left.into_int_value(), right.into_int_value(), "mod.int")
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
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::Mod: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_bit_and(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_and(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_and(left.into_int_value(), right.into_int_value(), "bit.and")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::BitAnd: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_bit_or(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_or(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_or(left.into_int_value(), right.into_int_value(), "bit.or")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::BitOr: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_bit_xor(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_xor(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_xor(left.into_int_value(), right.into_int_value(), "bit.xor")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::BitXor: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_shift_left(
        &self,
        ret_type: &Type,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_int() {
            if is_const {
                left.into_int_value()
                    .const_shl(right.into_int_value())
                    .into()
            } else {
                self.builder
                    .build_left_shift(left.into_int_value(), right.into_int_value(), "lshift")
                    .into()
            }
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::ShiftLeft: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_bin_op_shift_right(
        &self,
        ret_type: &Type,
        is_signed: bool,
        is_const: bool,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        Ok(if ret_type.is_int() {
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
        } else {
            return Err(self.err(
                format!("Invalid type for BinOper::ShiftRight: {:?}", ret_type),
                None,
            ));
        })
    }

    fn compile_un_op_array_access(
        &self,
        arr_val: &Val,
        idx_val: &Val,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        let arr_value = self.get_compiled_val(arr_val)?;
        let idx_value = self.get_compiled_val(idx_val)?;

        let arr_ptr_value = if arr_value.is_pointer_value() {
            arr_value.into_pointer_value()
        } else if let AnyTypeEnum::ArrayType(_) = arr_value.get_type() {
            // Need to wrap any array types with a temporary alloc to
            // access its members.
            let ptr = self
                .builder
                .build_alloca(arr_value.get_type().into_array_type(), "array.type.access");
            self.builder.build_store(ptr, arr_value.into_array_value());
            ptr
        } else {
            return Err(self.err(
                format!(
                    "Expr in array access not a array or pointer: {:?}",
                    arr_value
                ),
                None,
            ));
        };

        if !idx_value.is_int_value() {
            return Err(self.err(
                format!("Dim in array access didn't eval to int: {:?}", idx_value),
                None,
            ));
        }

        // Need to index the pointer to the array first, so add a extra zero
        // before the actual indexing (2d index).
        let sign_extend = false;
        let zero = self.context.i32_type().const_int(0, sign_extend);

        let member_ptr = unsafe {
            self.builder.build_gep(
                arr_ptr_value,
                &[zero, idx_value.into_int_value()],
                "array.gep",
            )
        };

        Ok(member_ptr.into())
    }

    fn compile_un_op_adt_access(&self, adt_val: &Val, idx: u32) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.get_compiled_val(adt_val)?;
        let is_const = is_const(&[any_value]);
        self.compile_struct_access(any_value, idx, is_const)
    }

    pub fn compile_struct_access(
        &self,
        mut any_value: AnyValueEnum<'ctx>,
        idx: u32,
        is_const: bool,
    ) -> LangResult<AnyValueEnum<'ctx>> {
        debug!(
            "Compiling struct access, idx: {}, any_value: {:#?}",
            idx, any_value
        );

        // TODO: Better way to do this? Can one skip this stack allocation
        //       in some way and GEP the value without a pointer?
        // If the value is given as a struct value and it isn't a const, it needs
        // to be stored temporarily on the stack to get a pointer which can then
        // be non-const GEPd.
        if any_value.is_struct_value() && !is_const {
            let ptr = self
                .builder
                .build_alloca(any_value.into_struct_value().get_type(), "struct.tmp.alloc");
            self.builder
                .build_store(ptr, any_into_basic_value(any_value)?);
            any_value = ptr.into();
        }

        if any_value.is_pointer_value() {
            let ptr = any_value.into_pointer_value();
            let member_ptr = self
                .builder
                .build_struct_gep(ptr, idx, "struct.gep")
                .map_err(|_| {
                    self.err(
                        format!(
                            "Unable to gep struct member at index {}: {:?}.",
                            idx, any_value
                        ),
                        None,
                    )
                })?;
            Ok(member_ptr.into())
        } else if any_value.is_struct_value() {
            // Known to be const at this point, so safe to "const extract".
            Ok(any_value
                .into_struct_value()
                .const_extract_value(&mut [idx])
                .into())
        } else {
            Err(self.err(
                format!(
                    "Expr in struct access not a pointer or struct. Compiled expr: {:#?}",
                    any_value
                ),
                None,
            ))
        }
    }

    fn compile_un_op_is_null(&self, val: &Val) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.get_compiled_val(val)?;
        // TODO: Which types should be allowed?
        if any_value.is_pointer_value() {
            let ptr = any_value.into_pointer_value();
            Ok(self.builder.build_is_null(ptr, "is.null.ptr").into())
        } else if any_value.is_int_value() {
            let int_value = any_value.into_int_value();
            let zero = int_value.get_type().const_zero();
            Ok(self
                .builder
                .build_int_compare(IntPredicate::EQ, int_value, zero, "is.null.int")
                .into())
        } else if any_value.is_float_value() {
            let float_value = any_value.into_float_value();
            let zero = float_value.get_type().const_zero();
            Ok(self
                .builder
                .build_float_compare(FloatPredicate::OEQ, float_value, zero, "is.null.float")
                .into())
        } else {
            Err(self.err(
                format!("Invalid type of value given to is_null: {:?}", val),
                None,
            ))
        }
    }

    fn compile_un_op_bool_not(&self, val: &Val) -> LangResult<AnyValueEnum<'ctx>> {
        let any_value = self.get_compiled_val(val)?;
        let is_const = is_const(&[any_value]);

        Ok(if is_const {
            any_value.into_int_value().const_not().into()
        } else {
            self.builder
                .build_not(any_value.into_int_value(), "not")
                .into()
        })
    }
}
