use std::collections::{HashMap, HashSet};

use ir::{EndInstr, ExprInstrKind, FuncDecl, Module, Op, UnOper};

/// Used when specifying which vals are used by which. This dummy value can be
/// set to indicate that a specific val should never be removed (since they are
/// "used by" this dummy value which will never be removed).
const UNREMOVABLE_VAL_IDX: usize = usize::MAX;

/// Used when specifying which basic block are used by which. This dummy value
/// can be set to indicate that a specific block should never be removed (since
/// they are "used by" this dummy value which will never be removed).
const UNREMOVABLE_LABEL: &str = "UNREMOVABLE_LABEL";

pub fn remove_unused(ir_module: &mut Module) {
    remove_unused_instrs(ir_module);
    remove_unused_blocks(ir_module);
}

fn remove_unused_instrs(ir_module: &mut Module) {
    let fn_names = ir_module.funcs.keys().cloned().collect::<Vec<_>>();
    for fn_name in fn_names {
        let fn_decl = ir_module.get_func(&fn_name).unwrap();
        let used_by = collect_vals_used(fn_decl);

        let fn_decl = ir_module.get_func_mut(&fn_name).unwrap();
        for basic_block in &mut fn_decl.basic_blocks {
            let mut idx = 0;
            while idx < basic_block.instrs.len() {
                let instr = basic_block.instrs.get(idx).unwrap();
                if is_removable_instr(&used_by, instr.val.0) {
                    basic_block.instrs.remove(idx);
                } else {
                    idx += 1;
                }
            }
        }
    }
}

fn remove_unused_blocks(ir_module: &mut Module) {
    for fn_decl in ir_module.funcs.values_mut() {
        let used_by = collect_block_labels_used(fn_decl);
        let mut idx = 0;
        while idx < fn_decl.basic_blocks.len() {
            let label = &fn_decl.basic_blocks.get(idx).unwrap().label;
            if is_removable_block(&used_by, label) {
                fn_decl.basic_blocks.remove(idx);
            } else {
                idx += 1;
            }
        }
    }
}

/// Given the val idx `val_idx`, returns true if the corresponding val is
/// removable (i.e. it self and all its parents are removeable). This function
/// is called recursively for every val that uses this specific `val_idx`.
fn is_removable_instr(used_by: &HashMap<usize, HashSet<usize>>, val_idx: usize) -> bool {
    if val_idx == UNREMOVABLE_VAL_IDX {
        return false;
    }

    let mut cur_is_removable = true;
    if let Some(parent_val_indices) = used_by.get(&val_idx) {
        for parent_val_idx in parent_val_indices {
            if !is_removable_instr(used_by, *parent_val_idx) {
                cur_is_removable = false;
                break;
            }
        }
    }
    cur_is_removable
}

/// Given the block label `label`, returns true if the corresponding block is
/// removable (i.e. it self and all its parents are removeable). This function
/// is called recursively for every label that uses this specific `label`.
fn is_removable_block(used_by: &HashMap<String, HashSet<String>>, label: &str) -> bool {
    if label == UNREMOVABLE_LABEL {
        return false;
    }

    let mut cur_is_removable = true;
    if let Some(parent_labels) = used_by.get(label) {
        for parent_label in parent_labels {
            if !is_removable_block(used_by, parent_label) {
                cur_is_removable = false;
                break;
            }
        }
    }
    cur_is_removable
}

/// Returns a map indicating all dependincies for a given val idx.
/// For example an `add` instruction as follows:
///   $3 = add $1 $2
/// would be represented as two entries in the map. The keys would be $1 & $2
/// and the value in both entries would be $3.
///
/// A val with no depenencies are represented by not existing in the map.
///
/// The instructions/vals that can't be removed are:
///  - fn/fnPtr calls (might have side-effects)
///  - vals used in end instructions
fn collect_vals_used(fn_decl: &FuncDecl) -> HashMap<usize, HashSet<usize>> {
    let mut used_by: HashMap<usize, HashSet<usize>> = HashMap::default();
    for basic_block in &fn_decl.basic_blocks {
        for instr in &basic_block.instrs {
            let cur_val_idx = instr.val.0;
            match &instr.kind {
                ExprInstrKind::FnCall(_, args) => {
                    // Function calls will always be seen as used even though
                    // their returned value might not be used (since the call
                    // might have side effects).
                    used_by
                        .entry(cur_val_idx)
                        .or_default()
                        .insert(UNREMOVABLE_VAL_IDX);
                    for arg in args {
                        used_by.entry(arg.0).or_default().insert(cur_val_idx);
                    }
                }

                ExprInstrKind::StructInit(_, args) | ExprInstrKind::ArrayInit(args) => {
                    for arg in args {
                        used_by.entry(arg.0).or_default().insert(cur_val_idx);
                    }
                }

                ExprInstrKind::FnPtrCall(fn_ptr, args) => {
                    used_by
                        .entry(cur_val_idx)
                        .or_default()
                        .insert(UNREMOVABLE_VAL_IDX);
                    used_by.entry(fn_ptr.0).or_default().insert(cur_val_idx);
                    for arg in args {
                        used_by.entry(arg.0).or_default().insert(cur_val_idx);
                    }
                }

                ExprInstrKind::Store(ptr, val) => {
                    used_by.entry(ptr.0).or_default().insert(cur_val_idx);
                    used_by.entry(val.0).or_default().insert(cur_val_idx);
                }

                ExprInstrKind::Load(ptr) => {
                    used_by.entry(ptr.0).or_default().insert(cur_val_idx);
                }

                ExprInstrKind::Op(Op::BinOp { lhs, rhs, .. }) => {
                    used_by.entry(lhs.0).or_default().insert(cur_val_idx);
                    used_by.entry(rhs.0).or_default().insert(cur_val_idx);
                }

                ExprInstrKind::Op(Op::UnOp { oper, value }) => {
                    if let UnOper::ArrayAccess(dim_val) = oper {
                        used_by.entry(dim_val.0).or_default().insert(cur_val_idx);
                    }
                    used_by.entry(value.0).or_default().insert(cur_val_idx);
                }

                ExprInstrKind::Phi(cases) => {
                    for (_, case_val) in cases {
                        used_by.entry(case_val.0).or_default().insert(cur_val_idx);
                    }
                }

                ExprInstrKind::Lit(_)
                | ExprInstrKind::FnPtr(_)
                | ExprInstrKind::VarAddress(_)
                | ExprInstrKind::DataAddress(_) => (),
            }
        }

        match basic_block.end_instr.as_ref().unwrap() {
            EndInstr::Return(Some(val)) | EndInstr::BranchIf(val, _, _) => {
                used_by
                    .entry(val.0)
                    .or_default()
                    .insert(UNREMOVABLE_VAL_IDX);
            }
            EndInstr::BranchSwitch(val, _, cases) => {
                used_by
                    .entry(val.0)
                    .or_default()
                    .insert(UNREMOVABLE_VAL_IDX);
                for (case_val, _) in cases {
                    used_by
                        .entry(case_val.0)
                        .or_default()
                        .insert(UNREMOVABLE_VAL_IDX);
                }
            }
            _ => (),
        }
    }
    used_by
}

/// Returns a map indicating all dependincies for a given basic block label.
/// For example an `BranchIf` instruction as follows in a block with name
/// "label.cur":
///   branch_if $0 "label.one" "label.two"
/// would be represented as two entries in the map. The keys would be "label.one"
/// & "label.two" and the value in both entries would be "label.cur".
///
/// A block with no depenencies are represented by not existing in the map.
///
/// The blocks that can't be removed (at this point) are:
///  - the first block in a function (entry block)
fn collect_block_labels_used(fn_decl: &FuncDecl) -> HashMap<String, HashSet<String>> {
    let mut used_by: HashMap<String, HashSet<String>> = HashMap::default();
    for (idx, basic_block) in fn_decl.basic_blocks.iter().enumerate() {
        let cur_block_label = basic_block.label.clone();

        // Entry block should NOT be removable (at this point at least).
        if idx == 0 {
            used_by
                .entry(cur_block_label.clone())
                .or_default()
                .insert(UNREMOVABLE_LABEL.into());
        }

        for instr in &basic_block.instrs {
            if let ExprInstrKind::Phi(cases) = &instr.kind {
                for (case_label, _) in cases {
                    used_by
                        .entry(case_label.into())
                        .or_default()
                        .insert(cur_block_label.clone());
                }
            }
        }

        match basic_block.end_instr.as_ref().unwrap() {
            EndInstr::Branch(label) => {
                used_by
                    .entry(label.into())
                    .or_default()
                    .insert(cur_block_label.clone());
            }

            EndInstr::BranchIf(_, label_true, label_false) => {
                used_by
                    .entry(label_true.into())
                    .or_default()
                    .insert(cur_block_label.clone());
                used_by
                    .entry(label_false.into())
                    .or_default()
                    .insert(cur_block_label.clone());
            }

            EndInstr::BranchSwitch(_, label_default, cases) => {
                used_by
                    .entry(label_default.into())
                    .or_default()
                    .insert(cur_block_label.clone());
                for (_, label_case) in cases {
                    used_by
                        .entry(label_case.into())
                        .or_default()
                        .insert(cur_block_label.clone());
                }
            }

            _ => (),
        }
    }
    used_by
}
