use std::{fmt::Debug, ops::Deref};

use ty::Type;

pub mod basic_block;
pub mod error;
pub mod func;
pub mod instr;
pub mod module;
pub mod ty;

/// Used to indicate if an expression is a L- or Rvalue.
#[derive(Debug, Copy, Clone)]
pub enum ExprTy {
    LValue,
    RValue,
}

/// Will store temporary values that are evaluated from `ExprInstr`s. These
/// values can then be referenced from other instructions to use the result of
/// the previous expr instruction.
/// The `usize` is a unique identifier and the type is the type of the value.
#[derive(Debug, Clone)]
pub struct Val(pub usize, pub Type);

impl Deref for Val {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Used to indicate the Val of a expression that doesn't return anything.
/// This will be used for ex. `EndExpr`s.
pub const VAL_EMPTY: Val = Val(0, Type::Void);
pub const VAL_BOOL_TRUE: Val = Val(1, Type::Bool);
pub const VAL_BOOL_FALSE: Val = Val(2, Type::Bool);
pub const VAL_START_NR: usize = 3;

// TODO: More different data types.
pub enum Data {
    StringLit(String),
}

impl Debug for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringLit(data) => write!(f, "{}", data),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DataIdx(pub usize);

impl Deref for DataIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Copy)]
pub enum VarIdx {
    Global(GlobalVarIdx),
    Local(LocalVarIdx),
    Param(ParamVarIdx),
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalVarIdx(pub usize);

impl Deref for GlobalVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LocalVarIdx(pub usize);

impl Deref for LocalVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParamVarIdx(pub usize);

impl Deref for ParamVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Debug for VarIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Global(var_idx) => write!(f, "global({})", var_idx.0),
            Self::Local(var_idx) => write!(f, "local({})", var_idx.0),
            Self::Param(var_idx) => write!(f, "param({})", var_idx.0),
        }
    }
}
