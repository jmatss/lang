use std::{
    hash::Hash,
    sync::{Arc, RwLock},
};

use crate::{file::FilePosition, path::LangPath};

use super::{
    block::{Adt, Fn},
    expr::{Expr, Var},
    op::AssignOperator,
};

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Option<Expr>, Option<FilePosition>),
    /// Yield ~= Break with a value
    Yield(Expr, Option<FilePosition>),
    Break(Option<FilePosition>),
    Continue(Option<FilePosition>),

    Use(LangPath),
    Module(LangPath), // Module ~= Package

    /// Defer -> Run this expression at the end of the current block scope.
    /// The "Defer" is the place in the code where the "defer <expr>" was written
    /// in the actual source code.
    /// "DeferExec" statements will be added during the analyzing stage
    /// at places in the AST where the deferred expression should be executed,
    /// i.e. when the block ends or when it branches away.
    Defer(Expr, Option<FilePosition>),
    DeferExec(Expr),

    /// The lhs can't be a "Variable" directly since it needs to support
    /// ex. array indexing and dereferencing. But evaluationg the lhs expressions
    /// MUST evaluate to a variable.
    /// The valid lhs expressions are (Variable or wrapping a Variable):
    ///   Variable
    ///   bin op:
    ///     Dot (both lhs and rhs as Variables)
    ///   un op:
    ///     Deref
    ///     Address
    /// The "middle expr" is the lhs and the "right expr" is the rhs of the assignment.
    Assignment(AssignOperator, Expr, Expr, Option<FilePosition>),

    /// Used both for "var" and "const" variables.
    VariableDecl(Arc<RwLock<Var>>, Option<FilePosition>),

    // TODO: Implement extern for variables as well.
    /// Declaration of extern functions.
    ExternalDecl(ExternalDecl, Option<FilePosition>),
}

#[derive(Debug, Clone)]
pub enum ExternalDecl {
    Fn(Arc<RwLock<Fn>>),
    Struct(Arc<RwLock<Adt>>),
}

impl Stmt {
    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self {
            Stmt::Return(_, file_pos)
            | Stmt::Yield(_, file_pos)
            | Stmt::Break(file_pos)
            | Stmt::Continue(file_pos)
            | Stmt::Defer(_, file_pos)
            | Stmt::Assignment(.., file_pos)
            | Stmt::VariableDecl(.., file_pos)
            | Stmt::ExternalDecl(_, file_pos) => file_pos.as_ref(),

            Stmt::Use(path) | Stmt::Module(path) => path.file_pos(),

            Stmt::DeferExec(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Modifier {
    Const,
    External,
    Static,
    Private,
    Public,
    Hidden,
    This,
    ThisPointer,
}
