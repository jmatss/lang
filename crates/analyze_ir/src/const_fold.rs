use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use common::error::LangResult;
use ir::{
    BinOper, DataIdx, EndInstr, ExprInstr, ExprInstrKind, FuncDecl, Lit, Module, Op, Type, UnOper,
    Val, VarIdx,
};
use log::warn;

use crate::into_err;

/// This file contains logic to rewrite calculate const values at compile-time.
/// Ex. operations on const values will be pre-calculated here and the results
/// will be used directly in the IR instead of having to calculate them at
/// run-time.
///
/// This file also contains logic to rewrite `BranchIf` & `BranchSwitch` statements
/// into regular `Branches` if all their values are const.

#[derive(Debug, Clone)]
enum ConstValue {
    Lit(Lit),
    FnPtr(String),
    VarAddress(VarIdx),
    DataAddress(DataIdx),
}

impl ConstValue {
    fn to_const_value(instr_kind: &ExprInstrKind) -> Option<ConstValue> {
        Some(match instr_kind {
            ExprInstrKind::Lit(lit) => ConstValue::Lit(lit.clone()),
            ExprInstrKind::FnPtr(name) => ConstValue::FnPtr(name.into()),
            ExprInstrKind::VarAddress(var_idx) => ConstValue::VarAddress(*var_idx),
            ExprInstrKind::DataAddress(data_idx) => ConstValue::DataAddress(*data_idx),
            _ => return None,
        })
    }

    fn to_instr_kind(&self) -> ExprInstrKind {
        match self {
            ConstValue::Lit(lit) => ExprInstrKind::Lit(lit.clone()),
            ConstValue::FnPtr(name) => ExprInstrKind::FnPtr(name.into()),
            ConstValue::VarAddress(var_idx) => ExprInstrKind::VarAddress(*var_idx),
            ConstValue::DataAddress(data_idx) => ExprInstrKind::DataAddress(*data_idx),
        }
    }

    fn eq_with_type(&self, other: &ConstValue, ir_type: &Type) -> LangResult<bool> {
        Ok(match (self, other) {
            (Self::Lit(lit_a), Self::Lit(lit_b)) => match (lit_a, lit_b) {
                (Lit::String(a), Lit::String(b)) => a == b,
                (Lit::Char(a), Lit::Char(b)) => a == b,
                (Lit::Bool(a), Lit::Bool(b)) => a == b,
                (Lit::Integer(lit_a), Lit::Integer(lit_b))
                | (Lit::Float(lit_a), Lit::Float(lit_b)) => {
                    bin_op_cmp(&BinOper::Eq, lit_a, lit_b, ir_type)?
                }
                _ => false,
            },
            (Self::FnPtr(a), Self::FnPtr(b)) => a == b,
            (Self::VarAddress(a), Self::VarAddress(b)) => a == b,
            (Self::DataAddress(a), Self::DataAddress(b)) => a == b,
            _ => false,
        })
    }
}

#[derive(Debug, Clone)]
enum ConstResult {
    /// Used to indicate a value that should replace the current value to make
    /// it const.
    Replace(ExprInstrKind),
    /// Indicates that the current value should be removed. This is true for ex.
    /// `store` operations. The store/load will be removed (since they aren't
    /// const) and the value will be used instead without any stores/loads.
    Remove,
    /// Indicates that nothing special should be done. This will be used ex. for
    /// struct & array init. The members have already been made const, so nothing
    /// will be done for the struct/array init itself.
    Nothing,
}

pub(super) fn const_fold_new(ir_module: &mut Module) -> LangResult<()> {
    // TODO:
    // Key: val idx, value: const Val.
    //let global_const_vals = HashMap::default();
    for fn_decl in ir_module.funcs.values_mut() {
        // Key: val idx, value: const Val.
        let mut const_vals = HashMap::default();
        // Key: VarIdx, value: the const value stored in the variable at the
        // "current time". This will be changed while we iterate through all
        // instructions in order. The variable will be updated with the latest
        // stored value. If the latest stored value isn't const, the map entry
        // will be empty.
        let mut const_var_vals = HashMap::default();

        for basic_block in &mut fn_decl.basic_blocks {
            let mut instr_idx = 0;
            for instr in basic_block.instrs.to_vec() {
                if !is_const_new(&mut const_vals, &mut const_var_vals, &instr) {
                    // TODO: Match on this fn call (return ConstResult?)?
                    //       The remove/replace etc. in here.
                    // try_make_const
                }
            }
        }
    }
}

pub(super) fn const_fold(ir_module: &mut Module) -> LangResult<()> {
    for fn_decl in ir_module.funcs.values_mut() {
        let dependencies = collect_dependencies(fn_decl);
        let is_const = collect_is_const(fn_decl);
        let constable = collect_constable(fn_decl);
        let const_var_load = collect_const_var_load(fn_decl, &is_const, &ir_module.global_vars);

        let mut const_lookup = HashMap::default();

        for basic_block in &mut fn_decl.basic_blocks {
            let mut instr_idx = 0;
            for instr in basic_block.instrs.to_vec() {
                let mut instr_was_removed = false;
                let val = instr.val.clone();

                if *is_const.get(&val.0).unwrap() {
                    println!("IS CONST : {:?}", instr);
                    if let Some(const_value) = ConstValue::to_const_value(&instr.kind) {
                        const_lookup.insert(val.0, const_value);
                    }
                } else if is_constable(&dependencies, &constable, val.0) {
                    warn!("");
                    println!("fn_name: {}", fn_decl.name);
                    println!("IS CONSTABLE : {:?}", instr);
                    match make_const(&mut const_lookup, &const_var_load, &instr)? {
                        ConstResult::Replace(kind) => {
                            basic_block
                                .replace(instr_idx, ExprInstr { val, kind })
                                .map_err(into_err)?;
                        }
                        ConstResult::Remove => {
                            basic_block.remove(instr_idx).map_err(into_err)?;
                            instr_was_removed = true;
                        }
                        ConstResult::Nothing => (),
                    }
                }

                if !instr_was_removed {
                    instr_idx += 1;
                }
            }

            let end_instr = basic_block.end_instr.as_mut().unwrap();
            const_fold_branch(&const_lookup, end_instr)?;
        }
        println!(
            "DEPS: {:#?}, IS_CONST: {:#?}, CONSTABLE: {:#?}",
            dependencies, is_const, constable
        );
    }

    Ok(())
}

/// Returns true if the given `instr` is const or "consists" of const values.
/// If the given `instr` is const, it will be added to the `const_vals` map.
/// `const_vals` will contain all values that are const and have previosuly been
/// traversed in this function.
fn is_const_new(
    const_vals: &mut HashMap<usize, Val>,
    //TODO: This is newly added. Add const values of variables in here.
    const_var_vals: &mut HashMap<VarIdx, Val>,
    instr: &ExprInstr,
) -> bool {
    let cur_is_const = match &instr.kind {
        ExprInstrKind::Lit(_) | ExprInstrKind::FnPtr(_) => true,
        // TODO: Is it correct to consider locals const? Taking the address
        //       should be const, but the content isn't.
        ExprInstrKind::VarAddress(_) | ExprInstrKind::DataAddress(_) => true,

        ExprInstrKind::StructInit(_, args) | ExprInstrKind::ArrayInit(args) => {
            let mut adt_is_const = true;
            for arg in args {
                if !const_vals.contains_key(&arg.0) {
                    adt_is_const = false;
                    break;
                }
            }
            adt_is_const
        }

        ExprInstrKind::Op(Op::BinOp { oper, lhs, rhs, .. }) => {
            !matches!(oper, BinOper::As)
                && const_vals.contains_key(&lhs.0)
                && const_vals.contains_key(&rhs.0)
        }

        ExprInstrKind::Op(Op::UnOp { oper, value }) => {
            let mut oper_is_const = true;
            if let UnOper::ArrayAccess(dim_val) = oper {
                oper_is_const = const_vals.contains_key(&dim_val.0);
            }
            const_vals.contains_key(&value.0) && oper_is_const
        }

        ExprInstrKind::FnPtrCall(_, _) | ExprInstrKind::FnCall(_, _) => false,

        ExprInstrKind::Store(var_val, val) => {
            match var_val.1 {
                Type::Adt(_) => todo!(),
                Type::Func(_) => todo!(),
                Type::Pointer(_) => todo!(),
                Type::Array(_, _) => todo!(),
                Type::FuncPointer(_, _) => todo!(),
                Type::Void => todo!(),
                Type::Char => todo!(),
                Type::Bool => todo!(),
                Type::I8 => todo!(),
                Type::U8 => todo!(),
                Type::I16 => todo!(),
                Type::U16 => todo!(),
                Type::I32 => todo!(),
                Type::U32 => todo!(),
                Type::F32 => todo!(),
                Type::I64 => todo!(),
                Type::U64 => todo!(),
                Type::F64 => todo!(),
                Type::I128 => todo!(),
                Type::U128 => todo!(),
            }
        }
        ExprInstrKind::Load(var_val) => false,

        // TODO: Can this be const in any cases?
        ExprInstrKind::Phi(_) => false,
    };

    let cur_val_idx = instr.val.0;
    if cur_is_const {
        const_vals.insert(cur_val_idx, instr.val.clone());
    }
    cur_is_const
}

fn try_make_const(
    const_vals: &mut HashMap<usize, Val>,
    instr: &ExprInstr,
) -> LangResult<ConstResult> {
    let cur_val_idx = instr.val.0;
    if const_vals.contains_key(&cur_val_idx) {
        return ConstResult::Nothing;
    }

    Ok(match &instr.kind {
        // Already const.
        ExprInstrKind::Lit(_)
        | ExprInstrKind::FnPtr(_)
        | ExprInstrKind::DataAddress(_)
        | ExprInstrKind::VarAddress(_) => {
            unreachable!("try_make_const -- Already const: {:#?}", instr);
        }

        ExprInstrKind::Store(..) => ConstResult::Remove,

        ExprInstrKind::Load(_) => {
            let load_val = if let Some(load_val) = const_var_load.get(&cur_val_idx) {
                load_val
            } else {
                // TODO: Will end up here when there are no direct init value for
                //       a variable. This can happen ex. for a array value which
                //       is initialized one member at a time.
                return Ok(ConstResult::Nothing);
            };
            let const_val = if let Some(const_val) = const_lookup.get(&load_val.0) {
                const_val.clone()
            } else {
                return Ok(ConstResult::Nothing);
            };
            const_lookup.insert(cur_val_idx, const_val.clone());
            ConstResult::Replace(const_val.to_instr_kind())
        }

        // For these types, the values that they depend on should already have
        // been consted, so nothing to do here.
        ExprInstrKind::StructInit(_, _) | ExprInstrKind::ArrayInit(_) => ConstResult::Nothing,

        ExprInstrKind::Op(Op::BinOp { oper, lhs, rhs, .. }) => {
            let lhs_const_val = if let Some(const_val) = const_lookup.get(&lhs.0) {
                const_val
            } else {
                return Ok(ConstResult::Nothing);
            };
            let rhs_const_val = if let Some(const_val) = const_lookup.get(&rhs.0) {
                const_val
            } else {
                return Ok(ConstResult::Nothing);
            };
            let const_value = make_const_bin_op(oper, lhs_const_val, rhs_const_val, &lhs.1)?;
            const_lookup.insert(cur_val_idx, const_value.clone());
            ConstResult::Replace(const_value.to_instr_kind())
        }

        ExprInstrKind::Op(Op::UnOp { oper, value }) => {
            let const_val = if let Some(const_val) = const_lookup.get(&value.0) {
                const_val
            } else {
                return Ok(ConstResult::Nothing);
            };
            let const_value = make_const_un_op(oper, const_val, &value.1)?;
            const_lookup.insert(cur_val_idx, const_value.clone());
            ConstResult::Replace(const_value.to_instr_kind())
        }

        ExprInstrKind::Phi(_) | ExprInstrKind::FnPtrCall(_, _) | ExprInstrKind::FnCall(_, _) => {
            unreachable!("make_const -- Not constable: {:#?}", instr);
        }
    })
}

/// Tries to const calculate the values in `BranchIf` & `BranchSwitch` to
/// rewrite them as regular `Branch`es.
fn const_fold_branch(
    const_lookup: &HashMap<usize, ConstValue>,
    end_instr: &mut EndInstr,
) -> LangResult<()> {
    match &end_instr {
        EndInstr::BranchIf(val, label_true, label_false) => {
            if let Some(const_value) = const_lookup.get(&val.0) {
                let branch_label = match const_value {
                    ConstValue::Lit(Lit::Bool(true)) => label_true,
                    ConstValue::Lit(Lit::Bool(false)) => label_false,
                    _ => unreachable!("const_fold_branch -- end_instr: {:#?}", end_instr),
                };
                *end_instr = EndInstr::Branch(branch_label.clone());
            }
        }

        EndInstr::BranchSwitch(val, default_label, cases) => {
            let mut all_is_const = true;
            if !const_lookup.contains_key(&val.0) {
                all_is_const = false;
            }
            for (case_val, _) in cases {
                if !const_lookup.contains_key(&case_val.0) {
                    all_is_const = false;
                }
            }

            // If the match expression and all case expressions are const, we
            // can't calculate the correct match here.
            if all_is_const {
                let mut branch_label = default_label.clone();
                let ir_type = val.1.clone();
                let const_value = const_lookup.get(&val.0).unwrap();

                for (case_val, case_label) in cases {
                    let case_const_value = const_lookup.get(&case_val.0).unwrap();
                    if const_value.eq_with_type(case_const_value, &ir_type)? {
                        branch_label = case_label.clone();
                        break;
                    }
                }

                *end_instr = EndInstr::Branch(branch_label);
            }
        }

        _ => (),
    }
    Ok(())
}

fn is_constable(
    dependencies: &HashMap<usize, HashSet<usize>>,
    constable: &HashMap<usize, bool>,
    val_idx: usize,
) -> bool {
    if !*constable.get(&val_idx).unwrap_or(&true) {
        // This val isn't constable.
        return false;
    }

    let mut cur_is_constable = true;
    if let Some(cur_dependencies) = dependencies.get(&val_idx) {
        for child_val_idx in cur_dependencies {
            if !is_constable(dependencies, constable, *child_val_idx) {
                cur_is_constable = false;
                break;
            }
        }
    }
    cur_is_constable
}

/// Returns a map indicating all dependincies for a given val idx.
/// For example an `add` instruction as follows:
///   $3 = add $1 $2
/// would be represented as a entry in the map with the key $3 and the values
/// $1 & $2.
///
/// A val with no depenencies are represented by not existing in the map.
fn collect_dependencies(fn_decl: &FuncDecl) -> HashMap<usize, HashSet<usize>> {
    let mut val_deps: HashMap<usize, HashSet<usize>> = HashMap::default();
    for basic_block in &fn_decl.basic_blocks {
        for instr in &basic_block.instrs {
            let cur_val_idx = instr.val.0;
            match &instr.kind {
                ExprInstrKind::FnCall(_, args)
                | ExprInstrKind::StructInit(_, args)
                | ExprInstrKind::ArrayInit(args) => {
                    for arg in args {
                        val_deps.entry(cur_val_idx).or_default().insert(arg.0);
                    }
                }

                ExprInstrKind::FnPtrCall(fn_ptr, args) => {
                    val_deps.entry(cur_val_idx).or_default().insert(fn_ptr.0);
                    for arg in args {
                        val_deps.entry(cur_val_idx).or_default().insert(arg.0);
                    }
                }

                ExprInstrKind::Store(ptr, val) => {
                    val_deps.entry(cur_val_idx).or_default().insert(ptr.0);
                    val_deps.entry(cur_val_idx).or_default().insert(val.0);
                }

                ExprInstrKind::Load(ptr) => {
                    val_deps.entry(cur_val_idx).or_default().insert(ptr.0);
                }

                ExprInstrKind::Op(Op::BinOp { lhs, rhs, .. }) => {
                    val_deps.entry(cur_val_idx).or_default().insert(lhs.0);
                    val_deps.entry(cur_val_idx).or_default().insert(rhs.0);
                }

                ExprInstrKind::Op(Op::UnOp { value, .. }) => {
                    val_deps.entry(cur_val_idx).or_default().insert(value.0);
                }

                ExprInstrKind::Phi(cases) => {
                    for (_, case_val) in cases {
                        val_deps.entry(cur_val_idx).or_default().insert(case_val.0);
                    }
                }

                ExprInstrKind::Lit(_)
                | ExprInstrKind::FnPtr(_)
                | ExprInstrKind::VarAddress(_)
                | ExprInstrKind::DataAddress(_) => (),
            }
        }
    }
    val_deps
}

/// The `usize` key is the `val` idx.
fn collect_is_const(fn_decl: &FuncDecl) -> HashMap<usize, bool> {
    let mut is_const: HashMap<usize, bool> = HashMap::default();
    for basic_block in &fn_decl.basic_blocks {
        for instr in &basic_block.instrs {
            let cur_val_idx = instr.val.0;
            let cur_is_const = match &instr.kind {
                ExprInstrKind::Lit(_) | ExprInstrKind::FnPtr(_) => true,
                // TODO: Is it correct to consider locals const? Taking the address
                //       should be const, but the content isn't.
                ExprInstrKind::VarAddress(_) | ExprInstrKind::DataAddress(_) => true,

                ExprInstrKind::StructInit(_, args) | ExprInstrKind::ArrayInit(args) => {
                    let mut adt_is_const = true;
                    for arg in args {
                        if !is_const.get(&arg.0).unwrap() {
                            adt_is_const = false;
                            break;
                        }
                    }
                    adt_is_const
                }

                ExprInstrKind::Op(Op::BinOp { oper, lhs, rhs, .. }) => {
                    !matches!(oper, BinOper::As)
                        && *is_const.get(&lhs.0).unwrap()
                        && *is_const.get(&rhs.0).unwrap()
                }

                ExprInstrKind::Op(Op::UnOp { oper, value }) => {
                    let mut oper_is_const = true;
                    if let UnOper::ArrayAccess(dim_val) = oper {
                        oper_is_const = *is_const.get(&dim_val.0).unwrap();
                    }
                    *is_const.get(&value.0).unwrap() && oper_is_const
                }

                ExprInstrKind::FnPtrCall(_, _)
                | ExprInstrKind::FnCall(_, _)
                | ExprInstrKind::Store(_, _)
                | ExprInstrKind::Load(_) => false,

                // TODO: Can this be const in any cases?
                ExprInstrKind::Phi(_) => false,
            };
            is_const.insert(cur_val_idx, cur_is_const);
        }
    }
    is_const
}

/// The `usize` key is the `val` idx.
fn collect_constable(fn_decl: &FuncDecl) -> HashMap<usize, bool> {
    let mut constable: HashMap<usize, bool> = HashMap::default();
    for basic_block in &fn_decl.basic_blocks {
        for instr in &basic_block.instrs {
            let cur_val_idx = instr.val.0;
            let is_constable = match &instr.kind {
                ExprInstrKind::Lit(_)
                | ExprInstrKind::FnPtr(_)
                | ExprInstrKind::DataAddress(_)
                | ExprInstrKind::VarAddress(VarIdx::Local(_)) => true,

                ExprInstrKind::VarAddress(VarIdx::Global(_) | VarIdx::Param(_)) => false,

                ExprInstrKind::Store(ptr, val) => {
                    *constable.get(&ptr.0).unwrap() && *constable.get(&val.0).unwrap()
                }

                ExprInstrKind::Load(ptr) => *constable.get(&ptr.0).unwrap(),

                ExprInstrKind::StructInit(_, args) | ExprInstrKind::ArrayInit(args) => {
                    let mut adt_is_constable = true;
                    for arg in args {
                        if !constable.get(&arg.0).unwrap() {
                            adt_is_constable = false;
                            break;
                        }
                    }
                    adt_is_constable
                }

                // TODO: How to handle `As` operations. Should be possible to
                //       make const, but need to consider ex. non-bitwise casts.
                ExprInstrKind::Op(Op::BinOp { oper, lhs, rhs, .. }) => {
                    !matches!(oper, BinOper::As)
                        && *constable.get(&lhs.0).unwrap()
                        && *constable.get(&rhs.0).unwrap()
                }

                // TODO: Currently we can't convert a non-const ADT/array access
                //       to a const. TO be implemented in the future.
                ExprInstrKind::Op(Op::UnOp { oper, value, .. }) => {
                    let mut oper_is_constable = true;
                    if matches!(oper, UnOper::ArrayAccess(_) | UnOper::AdtAccess(_)) {
                        // TODO:
                        oper_is_constable = false;
                    }
                    *constable.get(&value.0).unwrap() && oper_is_constable
                }

                // TODO: Can this be constable in any cases?
                ExprInstrKind::Phi(_) => false,

                // TODO: Implement const functions.
                ExprInstrKind::FnPtrCall(_, _) | ExprInstrKind::FnCall(_, _) => false,
            };
            constable.insert(cur_val_idx, is_constable);
        }
    }
    constable
}

// TODO: This can't work since a const variable might be assigned multiple times
//       in the same function. In those cases we need to get the const value that
//       is stored in the variable at a specific "section" of the function.
/// Returns a map mapping val indices represent variable with their corresponding
/// const Val.
fn collect_const_var_load(
    fn_decl: &FuncDecl,
    is_const: HashMap<usize, bool>,
    globals: &[(Type, Option<Lit>)],
) -> HashMap<usize, Val> {
    let mut val_idx_to_var_idx = HashMap::new();
    let mut var_idx_to_val_idx = HashMap::new();
    let mut val_idx_to_val = HashMap::default();

    for basic_block in &fn_decl.basic_blocks {
        for instr in &basic_block.instrs {
            let cur_val_idx = instr.val.0;
            match &instr.kind {
                ExprInstrKind::VarAddress(var_idx) => {
                    val_idx_to_var_idx.insert(cur_val_idx, *var_idx);
                }

                ExprInstrKind::Store(ptr, val) => {
                    if val_idx_to_var_idx.contains_key(&ptr.0) {
                        let var_idx = val_idx_to_var_idx.get(&ptr.0).unwrap();
                        var_idx_to_val_idx.insert(*var_idx, val.clone());
                    }
                }

                ExprInstrKind::Load(ptr) => {
                    let val = if let Some(VarIdx::Global(_)) = val_idx_to_var_idx.get(&ptr.0) {
                        // TODO: Handle globals.
                        let todo_handle_globals = globals;
                        continue;
                    } else if let Some(VarIdx::Param(_)) = val_idx_to_var_idx.get(&ptr.0) {
                        // TODO: Handle params.
                        let todo_handle_params = ();
                        continue;
                    } else if let Some(var_idx) = val_idx_to_var_idx.get(&ptr.0) {
                        if let Some(val) = var_idx_to_val_idx.get(var_idx) {
                            val.clone()
                        } else {
                            // TODO: How to handle this? Happens ex. if this is
                            //       an array access.
                            continue;
                        }
                    } else {
                        continue;
                    };
                    val_idx_to_val.insert(cur_val_idx, val);
                }

                _ => (),
            }
        }
    }
    val_idx_to_val
}

/// This function should ONLY be called with a `instr` that can be converted a
/// const according to the `constable` map.
fn make_const(
    const_lookup: &mut HashMap<usize, ConstValue>,
    const_var_load: &HashMap<usize, Val>,
    instr: &ExprInstr,
) -> LangResult<ConstResult> {
    let cur_val_idx = instr.val.0;
    Ok(match &instr.kind {
        // Already const.
        ExprInstrKind::Lit(_)
        | ExprInstrKind::FnPtr(_)
        | ExprInstrKind::DataAddress(_)
        | ExprInstrKind::VarAddress(_) => {
            unreachable!("make_const -- Already const: {:#?}", instr);
        }

        ExprInstrKind::Store(..) => ConstResult::Remove,

        ExprInstrKind::Load(_) => {
            let load_val = if let Some(load_val) = const_var_load.get(&cur_val_idx) {
                load_val
            } else {
                // TODO: Will end up here when there are no direct init value for
                //       a variable. This can happen ex. for a array value which
                //       is initialized one member at a time.
                return Ok(ConstResult::Nothing);
            };
            let const_val = if let Some(const_val) = const_lookup.get(&load_val.0) {
                const_val.clone()
            } else {
                return Ok(ConstResult::Nothing);
            };
            const_lookup.insert(cur_val_idx, const_val.clone());
            ConstResult::Replace(const_val.to_instr_kind())
        }

        // For these types, the values that they depend on should already have
        // been consted, so nothing to do here.
        ExprInstrKind::StructInit(_, _) | ExprInstrKind::ArrayInit(_) => ConstResult::Nothing,

        ExprInstrKind::Op(Op::BinOp { oper, lhs, rhs, .. }) => {
            let lhs_const_val = if let Some(const_val) = const_lookup.get(&lhs.0) {
                const_val
            } else {
                return Ok(ConstResult::Nothing);
            };
            let rhs_const_val = if let Some(const_val) = const_lookup.get(&rhs.0) {
                const_val
            } else {
                return Ok(ConstResult::Nothing);
            };
            let const_value = make_const_bin_op(oper, lhs_const_val, rhs_const_val, &lhs.1)?;
            const_lookup.insert(cur_val_idx, const_value.clone());
            ConstResult::Replace(const_value.to_instr_kind())
        }

        ExprInstrKind::Op(Op::UnOp { oper, value }) => {
            let const_val = if let Some(const_val) = const_lookup.get(&value.0) {
                const_val
            } else {
                return Ok(ConstResult::Nothing);
            };
            let const_value = make_const_un_op(oper, const_val, &value.1)?;
            const_lookup.insert(cur_val_idx, const_value.clone());
            ConstResult::Replace(const_value.to_instr_kind())
        }

        ExprInstrKind::Phi(_) | ExprInstrKind::FnPtrCall(_, _) | ExprInstrKind::FnCall(_, _) => {
            unreachable!("make_const -- Not constable: {:#?}", instr);
        }
    })
}

// TODO: Make sure that `Data` merging is done before. In that case this will
//       work correctly.
fn make_const_bin_op(
    oper: &BinOper,
    lhs_const_val: &ConstValue,
    rhs_const_val: &ConstValue,
    ir_type: &Type,
) -> LangResult<ConstValue> {
    let lhs_lit = if let ConstValue::Lit(lit) = lhs_const_val {
        lit
    } else {
        unreachable!(
            "oper: {:?}, lhs: {:?}, rhs: {:?}",
            oper, lhs_const_val, rhs_const_val
        );
    };
    let rhs_lit = if let ConstValue::Lit(lit) = rhs_const_val {
        lit
    } else {
        unreachable!(
            "oper: {:?}, lhs: {:?}, rhs: {:?}",
            oper, lhs_const_val, rhs_const_val
        );
    };

    if let BinOper::As = oper {
        todo!("Handle `As` as const in the future.");
    }

    Ok(ConstValue::Lit(match (lhs_lit, rhs_lit) {
        (Lit::String(lhs), Lit::String(rhs)) => bin_op_string(oper, lhs, rhs),
        (Lit::Char(lhs), Lit::Char(rhs)) => bin_op_char(oper, *lhs, *rhs),
        (Lit::Bool(lhs), Lit::Bool(rhs)) => bin_op_bool(oper, *lhs, *rhs),
        (Lit::Integer(lhs), Lit::Integer(rhs)) | (Lit::Float(lhs), Lit::Float(rhs)) => {
            bin_op_number(oper, lhs, rhs, ir_type)?
        }
        _ => unreachable!(
            "make_const_bin_op -- oper: {:?}, lhs_lit: {:?}, rhs_lit: {:?}",
            oper, lhs_lit, rhs_lit
        ),
    }))
}

fn bin_op_string(oper: &BinOper, lhs: &DataIdx, rhs: &DataIdx) -> Lit {
    Lit::Bool(match oper {
        BinOper::Eq => lhs.0 == rhs.0,
        BinOper::Neq => lhs.0 != rhs.0,
        _ => unreachable!(
            "bin_op_string -- oper: {:?}, lhs: {:?}, rhs: {:?}",
            oper, lhs, rhs
        ),
    })
}

fn bin_op_char(oper: &BinOper, lhs: char, rhs: char) -> Lit {
    Lit::Bool(match oper {
        BinOper::Eq => lhs == rhs,
        BinOper::Neq => lhs != rhs,
        BinOper::Lt => lhs < rhs,
        BinOper::Gt => lhs > rhs,
        BinOper::Lte => lhs <= rhs,
        BinOper::Gte => lhs >= rhs,
        _ => unreachable!(
            "bin_op_char -- oper: {:?}, lhs: {:?}, rhs: {:?}",
            oper, lhs, rhs
        ),
    })
}

fn bin_op_bool(oper: &BinOper, lhs: bool, rhs: bool) -> Lit {
    Lit::Bool(match oper {
        BinOper::Eq => lhs == rhs,
        BinOper::Neq => lhs != rhs,
        _ => unreachable!(
            "bin_op_bool -- oper: {:?}, lhs: {:?}, rhs: {:?}",
            oper, lhs, rhs
        ),
    })
}

fn bin_op_number(oper: &BinOper, lhs: &str, rhs: &str, ir_type: &Type) -> LangResult<Lit> {
    Ok(match oper {
        BinOper::As => unreachable!(),

        BinOper::Eq | BinOper::Neq | BinOper::Lt | BinOper::Gt | BinOper::Lte | BinOper::Gte => {
            Lit::Bool(bin_op_cmp(oper, lhs, rhs, ir_type)?)
        }

        BinOper::Add | BinOper::Sub | BinOper::Mul | BinOper::Div | BinOper::Mod
            if ir_type.is_float() =>
        {
            Lit::Float(bin_op_math(oper, lhs, rhs, ir_type)?)
        }

        BinOper::Add
        | BinOper::Sub
        | BinOper::Mul
        | BinOper::Div
        | BinOper::Mod
        | BinOper::BitAnd
        | BinOper::BitOr
        | BinOper::BitXor
        | BinOper::ShiftLeft
        | BinOper::ShiftRight
            if ir_type.is_int() =>
        {
            Lit::Integer(bin_op_math(oper, lhs, rhs, ir_type)?)
        }

        _ => unreachable!(
            "bin_op_number -- oper: {:?}, lhs: {:?}, rhs: {:?}",
            oper, lhs, rhs
        ),
    })
}

macro_rules! bin_op_math {
    ("add", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? + <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("sub", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? - <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("mul", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? * <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("div", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? / <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("mod", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? % <$ty>::from_str($rhs_lit)?).to_string()
    };
}

macro_rules! bin_op_math_bit {
    ("and", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? & <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("or", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? | <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("xor", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? ^ <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("shl", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? << <$ty>::from_str($rhs_lit)?).to_string()
    };

    ("shr", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? >> <$ty>::from_str($rhs_lit)?).to_string()
    };
}

macro_rules! bin_op_cmp {
    ("eq_i", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        <$ty>::from_str($lhs_lit)? == <$ty>::from_str($rhs_lit)?
    };

    ("eq_f", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? - <$ty>::from_str($rhs_lit)?).abs() < <$ty>::EPSILON
    };

    ("neq_i", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        <$ty>::from_str($lhs_lit)? != <$ty>::from_str($rhs_lit)?
    };

    ("neq_f", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        (<$ty>::from_str($lhs_lit)? - <$ty>::from_str($rhs_lit)?).abs() >= <$ty>::EPSILON
    };

    ("lt", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        <$ty>::from_str($lhs_lit)? < <$ty>::from_str($rhs_lit)?
    };

    ("gt", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        <$ty>::from_str($lhs_lit)? > <$ty>::from_str($rhs_lit)?
    };

    ("lte", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        <$ty>::from_str($lhs_lit)? <= <$ty>::from_str($rhs_lit)?
    };

    ("gte", $lhs_lit:expr, $rhs_lit:expr, $ty:ty) => {
        <$ty>::from_str($lhs_lit)? >= <$ty>::from_str($rhs_lit)?
    };
}

macro_rules! un_op_is_null {
    ("is_null", $lit:expr, $null_value:expr, $ty:ty) => {
        <$ty>::from_str($lit)? == $null_value
    };
}

macro_rules! un_op_bit_complement {
    ("bit_complement", $lit:expr, $dummy_value:expr, $ty:ty) => {
        (!<$ty>::from_str($lit)?).to_string()
    };
}

/// Used for both unop and binop operations.
macro_rules! op_on_num {
    ($op_type:ident, $op:tt, $lit_a:expr, $lit_b:expr, $ir_type:expr) => {
        match $ir_type {
            Type::I8 => $op_type!($op, $lit_a, $lit_b, i8),
            Type::U8 => $op_type!($op, $lit_a, $lit_b, u8),
            Type::I16 => $op_type!($op, $lit_a, $lit_b, i16),
            Type::U16 => $op_type!($op, $lit_a, $lit_b, u16),
            Type::I32 => $op_type!($op, $lit_a, $lit_b, i32),
            Type::U32 => $op_type!($op, $lit_a, $lit_b, u32),
            Type::F32 => $op_type!($op, $lit_a, $lit_b, f32),
            Type::I64 => $op_type!($op, $lit_a, $lit_b, i64),
            Type::U64 => $op_type!($op, $lit_a, $lit_b, u64),
            Type::F64 => $op_type!($op, $lit_a, $lit_b, f64),
            Type::I128 => $op_type!($op, $lit_a, $lit_b, i128),
            Type::U128 => $op_type!($op, $lit_a, $lit_b, u128),
            _ => unreachable!(
                "op_on_num! -- oper: {:?}, lit_a: {}, lit_b: {} type: {:?}",
                $op, $lit_a, $lit_b, $ir_type
            ),
        }
    };
}

/// Used for both unop and binop operations.
macro_rules! op_on_int {
    ($op_type:ident, $op:tt, $lit_a:expr, $lit_b:expr, $ir_type:expr) => {
        match $ir_type {
            Type::I8 => $op_type!($op, $lit_a, $lit_b, i8),
            Type::U8 => $op_type!($op, $lit_a, $lit_b, u8),
            Type::I16 => $op_type!($op, $lit_a, $lit_b, i16),
            Type::U16 => $op_type!($op, $lit_a, $lit_b, u16),
            Type::I32 => $op_type!($op, $lit_a, $lit_b, i32),
            Type::U32 => $op_type!($op, $lit_a, $lit_b, u32),
            Type::I64 => $op_type!($op, $lit_a, $lit_b, i64),
            Type::U64 => $op_type!($op, $lit_a, $lit_b, u64),
            Type::I128 => $op_type!($op, $lit_a, $lit_b, i128),
            Type::U128 => $op_type!($op, $lit_a, $lit_b, u128),
            _ => unreachable!(
                "op_on_int! -- oper: {:?}, lit_a: {}, lit_b: {} type: {:?}",
                $op, $lit_a, $lit_b, $ir_type
            ),
        }
    };
}

/// Used for both unop and binop operations.
macro_rules! op_on_float {
    ($op_type:ident, $op:tt, $lit_a:expr, $lit_b:expr, $ir_type:expr) => {
        match $ir_type {
            Type::F32 => $op_type!($op, $lit_a, $lit_b, f32),
            Type::F64 => $op_type!($op, $lit_a, $lit_b, f64),
            _ => unreachable!(
                "op_on_float! -- oper: {:?}, lit_a: {}, lit_b: {} type: {:?}",
                $op, $lit_a, $lit_b, $ir_type
            ),
        }
    };
}

fn bin_op_cmp(oper: &BinOper, lhs_lit: &str, rhs_lit: &str, ir_type: &Type) -> LangResult<bool> {
    Ok(match oper {
        BinOper::Eq if ir_type.is_int() => {
            op_on_int!(bin_op_cmp, "eq_i", lhs_lit, rhs_lit, ir_type)
        }
        BinOper::Neq if ir_type.is_int() => {
            op_on_int!(bin_op_cmp, "neq_i", lhs_lit, rhs_lit, ir_type)
        }
        BinOper::Eq if ir_type.is_float() => {
            op_on_float!(bin_op_cmp, "eq_f", lhs_lit, rhs_lit, ir_type)
        }
        BinOper::Neq if ir_type.is_float() => {
            op_on_float!(bin_op_cmp, "neq_f", lhs_lit, rhs_lit, ir_type)
        }
        BinOper::Lt => op_on_num!(bin_op_cmp, "lt", lhs_lit, rhs_lit, ir_type),
        BinOper::Gt => op_on_num!(bin_op_cmp, "gt", lhs_lit, rhs_lit, ir_type),
        BinOper::Lte => op_on_num!(bin_op_cmp, "lte", lhs_lit, rhs_lit, ir_type),
        BinOper::Gte => op_on_num!(bin_op_cmp, "gte", lhs_lit, rhs_lit, ir_type),
        _ => unreachable!(
            "bin_op_cmp -- oper: {:?}, lhs_lit: {}, rhs_lit: {} type: {:?}",
            oper, lhs_lit, rhs_lit, ir_type
        ),
    })
}

fn bin_op_math(oper: &BinOper, lhs_lit: &str, rhs_lit: &str, ir_type: &Type) -> LangResult<String> {
    Ok(match oper {
        BinOper::Add => op_on_num!(bin_op_math, "add", lhs_lit, rhs_lit, ir_type),
        BinOper::Sub => op_on_num!(bin_op_math, "sub", lhs_lit, rhs_lit, ir_type),
        BinOper::Mul => op_on_num!(bin_op_math, "mul", lhs_lit, rhs_lit, ir_type),
        BinOper::Div => op_on_num!(bin_op_math, "div", lhs_lit, rhs_lit, ir_type),
        BinOper::Mod => op_on_num!(bin_op_math, "mod", lhs_lit, rhs_lit, ir_type),
        BinOper::BitAnd => op_on_int!(bin_op_math_bit, "and", lhs_lit, rhs_lit, ir_type),
        BinOper::BitOr => op_on_int!(bin_op_math_bit, "or", lhs_lit, rhs_lit, ir_type),
        BinOper::BitXor => op_on_int!(bin_op_math_bit, "xor", lhs_lit, rhs_lit, ir_type),
        BinOper::ShiftLeft => op_on_int!(bin_op_math_bit, "shl", lhs_lit, rhs_lit, ir_type),
        BinOper::ShiftRight => op_on_int!(bin_op_math_bit, "shr", lhs_lit, rhs_lit, ir_type),
        _ => unreachable!(
            "bin_op_math -- oper: {:?}, lhs_lit: {}, rhs_lit: {}, type: {:?}",
            oper, lhs_lit, rhs_lit, ir_type
        ),
    })
}

fn make_const_un_op(
    oper: &UnOper,
    const_val: &ConstValue,
    ir_type: &Type,
) -> LangResult<ConstValue> {
    let lit = if let ConstValue::Lit(lit) = const_val {
        lit
    } else {
        unreachable!("");
    };

    Ok(ConstValue::Lit(match oper {
        UnOper::AdtAccess(_) => unreachable!("oper: {:?}", oper),
        UnOper::ArrayAccess(_) => unreachable!("oper: {:?}", oper),
        UnOper::IsNull => Lit::Bool(un_op_is_null(lit, ir_type)?),
        UnOper::BitComplement => Lit::Integer(un_op_bit_complement(lit, ir_type)?),
        UnOper::BoolNot => Lit::Bool(un_op_bool_not(lit)?),
    }))
}

fn un_op_is_null(const_lit: &Lit, ir_type: &Type) -> LangResult<bool> {
    Ok(match const_lit {
        Lit::String(_) => false,
        Lit::Char(char) => *char as i32 == 0,
        Lit::Bool(_) => unreachable!(),
        Lit::Integer(lit) => op_on_int!(un_op_is_null, "is_null", lit, 0, ir_type),
        Lit::Float(lit) => op_on_float!(un_op_is_null, "is_null", lit, 0.0, ir_type),
    })
}

fn un_op_bit_complement(const_lit: &Lit, ir_type: &Type) -> LangResult<String> {
    Ok(match const_lit {
        Lit::Integer(lit) => op_on_int!(
            un_op_bit_complement,
            "bit_complement",
            lit,
            "dummy",
            ir_type
        ),
        _ => unreachable!(
            "un_op_bit_complement -- const_lit: {:?}, type: {:?}",
            const_lit, ir_type
        ),
    })
}

fn un_op_bool_not(const_lit: &Lit) -> LangResult<bool> {
    Ok(match const_lit {
        Lit::Bool(lit) => !lit,
        _ => unreachable!("un_op_bool_not -- const_lit: {:?}", const_lit),
    })
}
