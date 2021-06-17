use std::{
    hash::Hash,
    sync::{Arc, RwLock},
};

use crate::{file::FilePosition, path::LangPath};

use super::{
    block::Fn,
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
    ExternalDecl(Arc<RwLock<Fn>>, Option<FilePosition>),
}

/*
impl Hash for Stmt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Stmt::Return(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            Stmt::Break(a) | Stmt::Continue(a) => {
                a.hash(state);
            }
            Stmt::Use(a) | Stmt::Module(a) => {
                a.hash(state);
            }
            Stmt::Yield(a, b) | Stmt::Defer(a, b) => {
                a.hash(state);
                b.hash(state);
            }
            Stmt::DeferExec(a) => {
                a.hash(state);
            }
            Stmt::Assignment(a, b, c, d) => {
                a.hash(state);
                b.hash(state);
                c.hash(state);
                d.hash(state);
            }
            Stmt::VariableDecl(a, b) => {
                a.as_ref().read().unwrap().hash(state);
                b.hash(state);
            }
            Stmt::ExternalDecl(a, b) => {
                let func = a.as_ref().read().unwrap();
                func.name.hash(state);
                func.generics.hash(state);
                func.ret_type.hash(state);
                func.modifiers.hash(state);
                func.is_var_arg.hash(state);
                func.method_adt.hash(state);
                b.hash(state);
            }
        }
    }
}
*/

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
