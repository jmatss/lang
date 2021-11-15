use common::error::{LangError, LangErrorKind, LangResult};
use ir::{
    BinOper, Data, DataIdx, EndInstr, ExprInstr, ExprInstrKind, IrError, IrResult, Lit, Module, Op,
    Signed, Type, UnOper, Val, VarIdx, VAL_EMPTY, VAL_START_NR,
};

use crate::into_err;

pub struct InstrBuilder {
    /// The unique ID used when creating new `Val`s. This will be incremented
    /// for every new `Val` created.
    val_id: usize,
}

impl InstrBuilder {
    const STD_STRING_VIEW: &'static str = "std::string::StringView";

    pub fn new() -> Self {
        Self {
            val_id: VAL_START_NR,
        }
    }

    pub fn new_val(&mut self, ir_type: Type) -> Val {
        Val(self.new_val_id(), ir_type)
    }

    fn new_val_id(&mut self) -> usize {
        let val_id = self.val_id;
        self.val_id += 1;
        val_id
    }

    /// Creates a new string literal. The type of the returned string literal
    /// will be `std::string::StringView`.
    pub fn string_lit(&mut self, module: &mut Module, str_lit: &str) -> ExprInstr {
        let val_type = Type::Adt(Self::STD_STRING_VIEW.into());
        ExprInstr {
            val: self.new_val(val_type),
            kind: ExprInstrKind::Lit(Lit::String(
                module.add_data(Data::StringLit(str_lit.into())),
            )),
        }
    }

    pub fn char_lit(&mut self, char_lit: char) -> ExprInstr {
        ExprInstr {
            val: self.new_val(Type::Char),
            kind: ExprInstrKind::Lit(Lit::Char(char_lit)),
        }
    }

    pub fn bool_true(&mut self) -> ExprInstr {
        self.bool_lit(true)
    }

    pub fn bool_false(&mut self) -> ExprInstr {
        self.bool_lit(false)
    }

    fn bool_lit(&mut self, bool_lit: bool) -> ExprInstr {
        ExprInstr {
            val: self.new_val(Type::Bool),
            kind: ExprInstrKind::Lit(Lit::Bool(bool_lit)),
        }
    }

    pub fn u8(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::U8)
    }

    pub fn i8(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::I8)
    }

    pub fn u16(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::U16)
    }

    pub fn i16(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::I16)
    }

    pub fn u32(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::U32)
    }

    pub fn i32(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::I32)
    }

    pub fn u64(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::U64)
    }

    pub fn i64(&mut self, int_lit: &str) -> ExprInstr {
        self.int(int_lit, Type::I64)
    }

    pub fn int(&mut self, int_lit: &str, int_type: Type) -> ExprInstr {
        ExprInstr {
            val: self.new_val(int_type),
            kind: ExprInstrKind::Lit(Lit::Integer(int_lit.into())),
        }
    }

    pub fn f32(&mut self, float_lit: &str) -> ExprInstr {
        self.float(float_lit, Type::F32)
    }

    pub fn f64(&mut self, float_lit: &str) -> ExprInstr {
        self.float(float_lit, Type::F64)
    }

    fn float(&mut self, float_lit: &str, float_type: Type) -> ExprInstr {
        ExprInstr {
            val: self.new_val(float_type),
            kind: ExprInstrKind::Lit(Lit::Float(float_lit.into())),
        }
    }

    pub fn fn_call(
        &mut self,
        module: &mut Module,
        fn_name: &str,
        args: &[Val],
    ) -> IrResult<ExprInstr> {
        let fn_decl = if let Some(fn_decl) = module.get_func(fn_name) {
            fn_decl
        } else {
            return Err(IrError::new(format!(
                "Unable to find function with name \"{}\" in `build_fn_call`.",
                fn_name
            )));
        };

        if !fn_decl.is_var_arg && args.len() != fn_decl.params.len() {
            return Err(IrError::new(format!(
                "Tried to call function \"{}\" with incorrect amount of arguments. \
                Parameter count: {}, argument count: {}",
                fn_name,
                fn_decl.params.len(),
                args.len()
            )));
        } else if fn_decl.is_var_arg && args.len() < fn_decl.params.len() {
            return Err(IrError::new(format!(
                "Tried to call variadic function \"{}\" with incorrect amount of arguments. \
                Parameter count (minimum): {}, argument count: {}",
                fn_name,
                fn_decl.params.len(),
                args.len()
            )));
        }

        for (idx, (arg, param_type)) in args.iter().zip(&fn_decl.params).enumerate() {
            let arg_type = &arg.1;
            if Self::assert_type_eq(arg_type, param_type).is_err() {
                return Err(IrError::new(format!(
                    "Invalid type of argument at index {} when calling function \"{}\". \
                    Parameter type: {:#?}, argument type: {:#?}",
                    idx, fn_name, param_type, arg_type
                )));
            }
        }

        Ok(ExprInstr {
            val: self.new_val(fn_decl.ret_type.clone()),
            kind: ExprInstrKind::FnCall(fn_name.into(), args.to_vec()),
        })
    }

    pub fn fn_ptr_call(
        &mut self,
        module: &mut Module,
        fn_ptr_var_idx: VarIdx,
        args: &[Val],
    ) -> IrResult<ExprInstr> {
        todo!("build_fn_ptr_call");
        // TODO: How to check that arg/param count matches?
        /*
        Ok(ExprInstr {
            ret_val: Val(self.new_val_id(), fn_decl.ret_ty.clone()),
            kind: ExprInstrKind::FnPtrCall(fn_name.into(), args.to_vec()),
        })
        */
    }

    pub fn fn_ptr(&mut self, module: &mut Module, fn_name: &str) -> IrResult<ExprInstr> {
        let fn_decl = if let Some(fn_decl) = module.get_func(fn_name) {
            fn_decl
        } else {
            return Err(IrError::new(format!(
                "Unable to find function with name \"{}\" in `build_fn_ptr`.",
                fn_name
            )));
        };

        let param_types = fn_decl.params.clone();
        let ret_type = fn_decl.ret_type.clone();
        Ok(ExprInstr {
            val: self.new_val(Type::FuncPointer(param_types, Box::new(ret_type))),
            kind: ExprInstrKind::FnPtr(fn_name.into()),
        })
    }

    // TODO: How should this be done? Where should the variable be created?
    //       Should we be able to find the type of the local in this function,
    //       how in that case?
    pub fn store(&mut self, var_ptr: Val, val: Val) -> ExprInstr {
        ExprInstr {
            val: self.new_val(Type::Void),
            kind: ExprInstrKind::Store(var_ptr, val),
        }
    }

    // TODO: How should this be done? Where should the variable be created?
    //       Should we be able to find the type of the local in this function,
    //       how in that case?
    pub fn load(&mut self, var_ptr: Val) -> IrResult<ExprInstr> {
        if let Type::Pointer(ir_type) = &var_ptr.1 {
            Ok(ExprInstr {
                val: self.new_val(*ir_type.clone()),
                kind: ExprInstrKind::Load(var_ptr),
            })
        } else {
            Err(IrError::new(format!(
                "Tried to take `load` non pointer value: {:#?}",
                var_ptr
            )))
        }
    }

    pub fn cast(&mut self, lhs: Val, ty: Type) -> ExprInstr {
        self.bin_op(BinOper::As, lhs, VAL_EMPTY, ty)
    }

    pub fn eq(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = Type::Bool;
        Self::assert_number(&lhs.1)?;
        self.bin_op_with_type_eq(BinOper::Eq, lhs, rhs, ret_type)
    }

    pub fn neq(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = Type::Bool;
        Self::assert_number(&lhs.1)?;
        self.bin_op_with_type_eq(BinOper::Neq, lhs, rhs, ret_type)
    }

    pub fn lt(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = Type::Bool;
        Self::assert_number(&lhs.1)?;
        self.bin_op_with_type_eq(BinOper::Lt, lhs, rhs, ret_type)
    }

    pub fn gt(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = Type::Bool;
        Self::assert_number(&lhs.1)?;
        self.bin_op_with_type_eq(BinOper::Gt, lhs, rhs, ret_type)
    }

    pub fn lte(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = Type::Bool;
        Self::assert_number(&lhs.1)?;
        self.bin_op_with_type_eq(BinOper::Lte, lhs, rhs, ret_type)
    }

    pub fn gte(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = Type::Bool;
        Self::assert_number(&lhs.1)?;
        self.bin_op_with_type_eq(BinOper::Gte, lhs, rhs, ret_type)
    }

    pub fn add(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_number(&ret_type)?;
        self.bin_op_with_type_eq(BinOper::Add, lhs, rhs, ret_type)
    }

    pub fn sub(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_number(&ret_type)?;
        self.bin_op_with_type_eq(BinOper::Sub, lhs, rhs, ret_type)
    }

    pub fn mul(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_number(&ret_type)?;
        self.bin_op_with_type_eq(BinOper::Mul, lhs, rhs, ret_type)
    }

    pub fn div(&mut self, lhs: Val, rhs: Val, signed: Signed) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_number(&ret_type)?;
        Self::assert_type_eq(&lhs.1, &rhs.1)?;
        Ok(self.bin_op_signed(BinOper::Div, lhs, rhs, ret_type, signed))
    }

    pub fn modu(&mut self, lhs: Val, rhs: Val, signed: Signed) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_int(&ret_type)?;
        Self::assert_type_eq(&lhs.1, &rhs.1)?;
        Ok(self.bin_op_signed(BinOper::Mod, lhs, rhs, ret_type, signed))
    }

    pub fn bit_and(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_int(&ret_type)?;
        self.bin_op_with_type_eq(BinOper::BitAnd, lhs, rhs, ret_type)
    }

    pub fn bit_or(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_int(&ret_type)?;
        self.bin_op_with_type_eq(BinOper::BitOr, lhs, rhs, ret_type)
    }

    pub fn bit_xor(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_int(&ret_type)?;
        self.bin_op_with_type_eq(BinOper::BitXor, lhs, rhs, ret_type)
    }

    pub fn shift_left(&mut self, lhs: Val, rhs: Val) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_int(&ret_type)?;
        Ok(self.bin_op(BinOper::ShiftLeft, lhs, rhs, ret_type))
    }

    pub fn shift_right(&mut self, lhs: Val, rhs: Val, signed: Signed) -> IrResult<ExprInstr> {
        let ret_type = lhs.1.clone();
        Self::assert_int(&ret_type)?;
        Ok(self.bin_op_signed(BinOper::ShiftRight, lhs, rhs, ret_type, signed))
    }

    /// Builds an binary operator. This function also makes sure that the types
    /// of `lhs` & `rhs` are the same/compatible.
    fn bin_op_with_type_eq(
        &mut self,
        oper: BinOper,
        lhs: Val,
        rhs: Val,
        ret_type: Type,
    ) -> IrResult<ExprInstr> {
        Self::assert_type_eq(&lhs.1, &rhs.1)?;
        Ok(self.bin_op(oper, lhs, rhs, ret_type))
    }

    fn bin_op(&mut self, oper: BinOper, lhs: Val, rhs: Val, ret_type: Type) -> ExprInstr {
        self.bin_op_signed(oper, lhs, rhs, ret_type, Signed::False)
    }

    fn bin_op_signed(
        &mut self,
        oper: BinOper,
        lhs: Val,
        rhs: Val,
        ret_type: Type,
        signed: Signed,
    ) -> ExprInstr {
        ExprInstr {
            val: self.new_val(ret_type),
            kind: ExprInstrKind::Op(Op::BinOp {
                oper,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                signed,
            }),
        }
    }

    /// Returns the address of the given var.
    pub fn var_address(&mut self, var_idx: VarIdx, var_type: Type) -> ExprInstr {
        let var_ptr_type = Type::Pointer(Box::new(var_type));
        ExprInstr {
            val: self.new_val(var_ptr_type),
            kind: ExprInstrKind::VarAddress(var_idx),
        }
    }

    /// Returns the address of the given data.
    pub fn data_address(&mut self, data_idx: DataIdx, var_type: Type) -> ExprInstr {
        let data_ptr_type = Type::Pointer(Box::new(var_type));
        ExprInstr {
            val: self.new_val(data_ptr_type),
            kind: ExprInstrKind::DataAddress(data_idx),
        }
    }

    /// Build an ADT/enum access. The `value` contains the thing that is to be
    /// accessed (should be an "indexable" type).
    ///
    /// The returned ExprInstr will contain a pointer to the given member. If
    /// one wants to access the value, it will have to be `load`ed by the caller.
    pub fn adt_access(
        &mut self,
        module: &mut Module,
        value: Val,
        idx: usize,
    ) -> IrResult<ExprInstr> {
        let member_type = match &value.1 {
            Type::Pointer(inner_type) if matches!(**inner_type, Type::Adt(_)) => {
                if let Type::Adt(name) = &**inner_type {
                    Self::adt_member_type(module, name, idx)?
                } else {
                    unreachable!()
                }
            }
            Type::Adt(name) => Self::adt_member_type(module, name, idx)?,
            _ => {
                return Err(IrError::new(format!(
                    "Tried to ADT access/index invalid type: {:#?}",
                    value
                )))
            }
        };

        let ret_type = Type::Pointer(Box::new(member_type));
        Ok(self.un_op(UnOper::AdtAccess(idx), value, ret_type))
    }

    /// Build an array access. The `value` contains the thing that is to be
    /// accessed (should be an array type).
    ///
    /// The returned ExprInstr will contain a pointer to the given member. If
    /// one wants to access the value, it will have to be `load`ed by the caller.
    pub fn array_access(&mut self, value: Val, idx_val: Val) -> IrResult<ExprInstr> {
        let member_type = match &value.1 {
            Type::Pointer(inner_type) if matches!(**inner_type, Type::Array(..)) => {
                if let Type::Array(ret_type, _) = &**inner_type {
                    *ret_type.clone()
                } else {
                    unreachable!()
                }
            }
            Type::Array(ret_type, _) => *ret_type.clone(),
            _ => {
                return Err(IrError::new(format!(
                    "Tried to array access/index invalid type: {:#?}",
                    value
                )))
            }
        };

        let ret_type = Type::Pointer(Box::new(member_type));
        Ok(self.un_op(UnOper::ArrayAccess(idx_val), value, ret_type))
    }

    fn adt_member_type(module: &Module, name: &str, idx: usize) -> IrResult<Type> {
        let members = if let Some(members) = module.get_struct(name) {
            members
        } else {
            return Err(IrError::new(format!(
                "Unable to find ADT with name \"{}\" in `build_access`.",
                name
            )));
        };

        if let Some(member_type) = members.get(idx) {
            Ok(member_type.clone())
        } else {
            return Err(IrError::new(format!(
                "Tried to index outside of bounds of type in `build_access`. \
                Type name: {}, index: {}",
                name, idx
            )));
        }
    }

    pub fn bit_compliment(&mut self, value: Val) -> ExprInstr {
        let ret_type = value.1.clone();
        self.un_op(UnOper::BitComplement, value, ret_type)
    }

    pub fn bool_not(&mut self, value: Val) -> ExprInstr {
        self.un_op(UnOper::BoolNot, value, Type::Bool)
    }

    fn un_op(&mut self, oper: UnOper, value: Val, ret_type: Type) -> ExprInstr {
        ExprInstr {
            val: self.new_val(ret_type),
            kind: ExprInstrKind::Op(Op::UnOp {
                oper,
                value: Box::new(value),
            }),
        }
    }

    pub fn phi(&mut self, values: &[(String, Val)]) -> IrResult<ExprInstr> {
        if values.is_empty() {
            return Err(IrError::new(
                "Tried to create phi-block with no values.".into(),
            ));
        }

        let first_type = (values.iter().next().unwrap().1).1.clone();
        for (idx, (_, val)) in values.iter().enumerate() {
            let val_type = &val.1;
            if Self::assert_type_eq(&first_type, val_type).is_err() {
                return Err(IrError::new(format!(
                    "Values in phi-block must have same type, but found different types.\n\
                    First type: {:#?}, type at index {}: {:#?}",
                    first_type, idx, val_type
                )));
            }
        }

        Ok(ExprInstr {
            val: self.new_val(first_type),
            kind: ExprInstrKind::Phi(values.to_vec()),
        })
    }

    pub fn struct_init(
        &mut self,
        module: &mut Module,
        name: &str,
        args: &[Val],
    ) -> IrResult<ExprInstr> {
        let members = if let Some(members) = module.get_struct(name) {
            members
        } else {
            return Err(IrError::new(format!(
                "Unable to find string with name \"{}\" in `build_struct_init`.",
                name
            )));
        };

        for (idx, (arg, param_type)) in args.iter().zip(members).enumerate() {
            let arg_type = &arg.1;
            if Self::assert_type_eq(arg_type, param_type).is_err() {
                return Err(IrError::new(format!(
                    "Invalid type of struct init argument at index {} for struct \"{}\". \
                    Struct decl type: {:#?}, argument type: {:#?}",
                    idx, name, param_type, arg_type
                )));
            }
        }

        Ok(ExprInstr {
            val: self.new_val(Type::Adt(name.into())),
            kind: ExprInstrKind::StructInit(name.into(), args.to_vec()),
        })
    }

    /// `idx` is the index of the member to initialize. This is the value that
    /// will be set in the union tag.
    /// OBS! The `value` should have been cast to the general u8 array before
    ///      this function is called.
    pub fn union_init(
        &mut self,
        module: &mut Module,
        name: &str,
        value: Val,
        idx: Val,
    ) -> IrResult<ExprInstr> {
        if module.get_struct(name).is_none() {
            return Err(IrError::new(format!(
                "Unable to find union(struct) with name \"{}\" in `build_union_init`.",
                name
            )));
        }

        Ok(ExprInstr {
            val: self.new_val(Type::Adt(name.into())),
            kind: ExprInstrKind::StructInit(name.into(), vec![value, idx]),
        })
    }

    /// Creates an array with length `args.len()` and with the inner type with
    /// the same type as the given values in `args`.
    pub fn array_init(&mut self, args: &[Val]) -> IrResult<ExprInstr> {
        if args.is_empty() {
            return Err(IrError::new(
                "Tried to create array init with no values.".into(),
            ));
        }

        let first_type = args.iter().next().unwrap().1.clone();
        for (idx, val) in args.iter().enumerate() {
            let val_type = &val.1;
            if Self::assert_type_eq(&first_type, val_type).is_err() {
                return Err(IrError::new(format!(
                    "Values in array init must have same type, but found different types.\n\
                    First type: {:#?}, type at index {}: {:#?}",
                    first_type, idx, val_type
                )));
            }
        }

        Ok(ExprInstr {
            val: self.new_val(first_type),
            kind: ExprInstrKind::ArrayInit(args.to_vec()),
        })
    }

    pub fn ret(&mut self, val: Option<Val>) -> EndInstr {
        EndInstr::Return(val)
    }

    pub fn exit(&mut self) -> EndInstr {
        EndInstr::Exit
    }

    pub fn branch(&mut self, label: &str) -> EndInstr {
        EndInstr::Branch(label.into())
    }

    pub fn branch_if(&mut self, expr: Val, label_true: &str, label_false: &str) -> EndInstr {
        EndInstr::BranchIf(expr, label_true.into(), label_false.into())
    }

    pub fn branch_switch(
        &mut self,
        expr: Val,
        label_default: &str,
        cases: &[(Val, String)],
    ) -> IrResult<EndInstr> {
        if let Some(first_type) = cases.iter().next().map(|(val, _)| val.1.clone()) {
            for (idx, (val, _)) in cases.iter().enumerate() {
                let val_type = &val.1;
                if Self::assert_type_eq(&first_type, val_type).is_err() {
                    return Err(IrError::new(format!(
                        "Values in branch switch must have same type, but found different types.\n\
                        First type: {:#?}, type at index {}: {:#?}",
                        first_type, idx, val_type
                    )));
                }
            }
        }
        Ok(EndInstr::BranchSwitch(
            expr,
            label_default.into(),
            cases.to_vec(),
        ))
    }

    pub fn unreachable(&mut self) -> EndInstr {
        EndInstr::Unreachable
    }

    pub fn null(&mut self, module: &mut Module, ir_type: Type) -> LangResult<ExprInstr> {
        Ok(match &ir_type {
            Type::Adt(adt_name) => {
                if let Some(members) = module.get_struct(adt_name).cloned() {
                    let mut arg_vals = Vec::with_capacity(members.len());
                    for member_type in members {
                        let member_instr = self.null(module, member_type)?;
                        arg_vals.push(member_instr.val);
                    }

                    self.struct_init(module, adt_name, &arg_vals)
                        .map_err(into_err)?
                } else {
                    return Err(LangError::new(
                        format!(
                            "Unable to find ADT with name \"{}\" in build_null()",
                            adt_name
                        ),
                        LangErrorKind::IrError,
                        None,
                    ));
                }
            }

            // TODO: int/uint instead of u64.
            Type::Pointer(_) => {
                let lhs_instr = self.u64("0");
                self.cast(lhs_instr.val, ir_type.clone())
            }

            Type::Char => self.char_lit('\0'),
            Type::Bool => self.bool_false(),
            Type::I8 => self.i8("0"),
            Type::U8 => self.u8("0"),
            Type::I16 => self.i16("0"),
            Type::U16 => self.u16("0"),
            Type::I32 => self.i32("0"),
            Type::U32 => self.u32("0"),
            Type::F32 => self.f32("0"),
            Type::I64 => self.i64("0"),
            Type::U64 => self.u64("0"),
            Type::F64 => self.f64("0"),

            Type::Array(ir_type_i, Some(dim)) => {
                let mut args = Vec::with_capacity(*dim as usize);
                for _ in 0..(*dim as usize) {
                    let arg_val = self.null(module, *ir_type_i.clone())?;
                    args.push(arg_val.val)
                }
                self.array_init(&args).map_err(into_err)?
            }

            Type::Array(_, None) | Type::Void | Type::Func(_) | Type::FuncPointer(..) => {
                return Err(LangError::new(
                    format!("Tried to create null of unsized type: {:#?}", ir_type),
                    LangErrorKind::IrError,
                    None,
                ))
            }

            Type::I128 => todo!("build_null i128"),
            Type::U128 => todo!("build_null u128"),
        })
    }

    pub fn is_null(&mut self, value: Val) -> ExprInstr {
        self.un_op(UnOper::IsNull, value, Type::Bool)
    }

    fn assert_type_eq(lhs_type: &Type, rhs_type: &Type) -> IrResult<()> {
        match (lhs_type, rhs_type) {
            (Type::Pointer(inner_type_a), Type::Pointer(inner_type_b)) => {
                Self::assert_type_eq(inner_type_a, inner_type_b)?;
            }
            (Type::Array(inner_type_a, dim_a), Type::Array(inner_type_b, dim_b))
                if dim_a == dim_b =>
            {
                Self::assert_type_eq(inner_type_a, inner_type_b)?;
            }

            (Type::Adt(name_a), Type::Adt(name_b)) | (Type::Func(name_a), Type::Func(name_b))
                if name_a == name_b => {}

            (Type::Void, Type::Void)
            | (Type::Char, Type::Char)
            | (Type::Bool, Type::Bool)
            | (Type::I8, Type::I8)
            | (Type::U8, Type::U8)
            | (Type::I16, Type::I16)
            | (Type::U16, Type::U16)
            | (Type::I32, Type::I32)
            | (Type::U32, Type::U32)
            | (Type::F32, Type::F32)
            | (Type::I64, Type::I64)
            | (Type::U64, Type::U64)
            | (Type::F64, Type::F64)
            | (Type::I128, Type::I128)
            | (Type::U128, Type::U128) => (),

            // TODO: Add line/column nr to error.
            _ => {
                return Err(IrError::new(format!(
                    "Expected lhs and rhs types to match but they didn't.\n\
                    Lhs type: {:#?}\nRhs type: {:#?}",
                    lhs_type, rhs_type,
                )));
            }
        }
        Ok(())
    }

    fn assert_number(ir_type: &Type) -> IrResult<()> {
        match ir_type {
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::F32
            | Type::I64
            | Type::U64
            | Type::F64
            | Type::I128
            | Type::U128 => Ok(()),
            _ => Err(IrError::new(format!(
                "Expected type to be number, got: {:?}.",
                ir_type,
            ))),
        }
    }

    fn assert_int(ir_type: &Type) -> IrResult<()> {
        match ir_type {
            Type::I8
            | Type::U8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::I128
            | Type::U128 => Ok(()),
            _ => Err(IrError::new(format!(
                "Expected type to be integer, got: {:?}.",
                ir_type,
            ))),
        }
    }
}

impl Default for InstrBuilder {
    fn default() -> Self {
        Self::new()
    }
}
