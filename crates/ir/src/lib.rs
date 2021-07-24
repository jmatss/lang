use std::ops::Deref;

pub mod basic_block;
pub mod decl;
pub mod error;
pub mod instruction;
pub mod module;

/// Used to indicate if an expression is a L- or Rvalue.
#[derive(Debug, Copy, Clone)]
pub enum ExprTy {
    LValue,
    RValue,
}

/// Will store temporary values that are evaluated from `ExprInstr`s. These
/// values can then be referenced from other instructions to use the result of
/// the previous expr instruction.
///
/// The wrapped `usize` should be unique for every unique temporary value.
#[derive(Debug, Clone, Copy)]
pub struct Val(pub usize);

impl Deref for Val {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// TODO: More different data types.
#[derive(Debug)]
pub enum Data {
    StringLit(String),
}

#[derive(Debug, Clone, Copy)]
pub struct DataIdx(pub usize);

impl Deref for DataIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalVarIdx(pub usize);

impl Deref for GlobalVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// This is used for both parameters and localy declared variables.
/// Their indices are independant, this means that they will contain the same
/// inner usize and it is up to the "user" to make sure to distinguish between
/// local and parameter indices when needed.
#[derive(Debug, Clone, Copy)]
pub struct LocalVarIdx(pub usize);

impl Deref for LocalVarIdx {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
