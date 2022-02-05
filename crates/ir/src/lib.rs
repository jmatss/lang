use std::{fmt::Debug, ops::Deref};

mod basic_block;
mod error;
mod func;
mod instr;
mod module;
mod size;
mod ty;

pub use basic_block::BasicBlock;
pub use error::{IrError, IrResult};
pub use func::{FuncDecl, FuncVisibility};
pub use instr::*;
pub use module::Module;
pub use size::size_with_padding;
pub use ty::Type;

/// Used to indicate if an expression is a L- or Rvalue.
#[derive(Debug, Copy, Clone)]
pub enum ExprTy {
    LValue,
    RValue,
}

/// Can be used as a dummy value when a `Val` needs to be provided but that
/// never will be used.
pub const DUMMY_VAL: Val = Val(usize::MAX, Type::Void);

/// Will store temporary values that are evaluated from `ExprInstr`s. These
/// values can then be referenced from other instructions to use the result of
/// the previous expr instruction.
/// The `usize` is a unique identifier and the type is the type of the value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Val(pub usize, pub Type);

impl Deref for Val {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// TODO: More different data types.
pub enum Data {
    StringLit(String),
}

impl Debug for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StringLit(data) => write!(f, "\"{}\"", data),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DataIdx(pub usize);

impl Deref for DataIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum VarIdx {
    Global(GlobalVarIdx),
    Local(LocalVarIdx),
    Param(ParamVarIdx),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalVarIdx(pub usize);

impl Deref for GlobalVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalVarIdx(pub usize);

impl Deref for LocalVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
