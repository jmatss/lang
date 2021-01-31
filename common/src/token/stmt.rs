use super::{
    block::Function,
    expr::{Expr, Var},
    op::AssignOperator,
};
use crate::{
    error::{LangError, LangErrorKind::GeneralError, LangResult},
    file::FilePosition,
    ENV_VAR,
};
use std::{cell::RefCell, hash::Hash, path::PathBuf, rc::Rc};

#[derive(Debug, Clone, Eq)]
pub enum Stmt {
    Return(Option<Expr>, Option<FilePosition>),
    /// Yield ~= Break with a value
    Yield(Expr, Option<FilePosition>),
    Break(Option<FilePosition>),
    Continue(Option<FilePosition>),

    Use(Path),
    Package(Path),

    /// Inc/dec are statements so that they can't be used in other expressions.
    Increment(Expr, Option<FilePosition>),
    Decrement(Expr, Option<FilePosition>),

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
    VariableDecl(Rc<RefCell<Var>>, Option<FilePosition>),

    // TODO: Implement extern for variables as well.
    /// Declaration of extern functions.
    ExternalDecl(Rc<RefCell<Function>>, Option<FilePosition>),
}

impl PartialEq for Stmt {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Stmt::Return(a, b), Stmt::Return(c, d)) => a == c && b == d,
            (Stmt::Break(a), Stmt::Break(b)) | (Stmt::Continue(a), Stmt::Continue(b)) => a == b,
            (Stmt::Use(a), Stmt::Use(b)) | (Stmt::Package(a), Stmt::Package(b)) => a == b,
            (Stmt::Yield(a, b), Stmt::Yield(c, d))
            | (Stmt::Increment(a, b), Stmt::Increment(c, d))
            | (Stmt::Decrement(a, b), Stmt::Decrement(c, d))
            | (Stmt::Defer(a, b), Stmt::Defer(c, d)) => a == c && b == d,
            (Stmt::DeferExec(a), Stmt::DeferExec(b)) => a == b,
            (Stmt::Assignment(a1, b1, c1, d1), Stmt::Assignment(e2, f2, g2, h2)) => {
                a1 == e2 && b1 == f2 && c1 == g2 && d1 == h2
            }
            (Stmt::VariableDecl(a, b), Stmt::VariableDecl(c, d)) => a == c && b == d,
            (Stmt::ExternalDecl(a, b), Stmt::ExternalDecl(c, d)) => a == c && b == d,
            _ => false,
        }
    }
}

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
            Stmt::Use(a) | Stmt::Package(a) => {
                a.hash(state);
            }
            Stmt::Yield(a, b)
            | Stmt::Increment(a, b)
            | Stmt::Decrement(a, b)
            | Stmt::Defer(a, b) => {
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
                a.borrow().hash(state);
                b.hash(state);
            }
            Stmt::ExternalDecl(a, b) => {
                let func = a.borrow();
                func.name.hash(state);
                func.generic_names.hash(state);
                func.generic_impls.hash(state);
                func.ret_type.hash(state);
                func.modifiers.hash(state);
                func.is_var_arg.hash(state);
                func.method_adt.hash(state);
                b.hash(state);
            }
        }
    }
}

impl Stmt {
    pub fn file_pos(&self) -> Option<&FilePosition> {
        match self {
            Stmt::Return(_, file_pos)
            | Stmt::Yield(_, file_pos)
            | Stmt::Break(file_pos)
            | Stmt::Continue(file_pos)
            | Stmt::Increment(_, file_pos)
            | Stmt::Decrement(_, file_pos)
            | Stmt::Defer(_, file_pos)
            | Stmt::Assignment(.., file_pos)
            | Stmt::VariableDecl(.., file_pos)
            | Stmt::ExternalDecl(_, file_pos) => file_pos.as_ref(),

            Stmt::Use(path) | Stmt::Package(path) => Some(&path.file_pos),

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub idents: Vec<String>,
    pub file_pos: FilePosition,
}

impl Path {
    const EXTENSION: &'static str = ".ren";

    pub fn new(idents: Vec<String>, file_pos: FilePosition) -> Self {
        Path { idents, file_pos }
    }

    /// Returns the path to a file. First looks for the file in the env var
    /// "LANG_HOME". If it isn't found, assume the file is located relative
    /// to the current working directory.
    pub fn to_file_path(&self) -> LangResult<String> {
        let lang_path = std::env::var(ENV_VAR)?;
        let relative_path = self.idents.join("/");

        let lang_file_path = PathBuf::from(format!(
            "{}/{}{}",
            lang_path,
            relative_path,
            Path::EXTENSION
        ));
        let relative_file_path = PathBuf::from(format!("{}{}", relative_path, Path::EXTENSION));

        if lang_file_path.exists() {
            Ok(lang_file_path
                .to_str()
                .ok_or_else(|| {
                    LangError::new(
                        format!("Unable to convert path \"{:?}\" to str.", lang_file_path),
                        GeneralError,
                        Some(self.file_pos.to_owned()),
                    )
                })?
                .into())
        } else if relative_file_path.exists() {
            Ok(relative_file_path
                .to_str()
                .ok_or_else(|| {
                    LangError::new(
                        format!(
                            "Unable to convert path \"{:?}\" to str.",
                            relative_file_path
                        ),
                        GeneralError,
                        Some(self.file_pos.to_owned()),
                    )
                })?
                .into())
        } else {
            Err(LangError::new(
                format!("Unable to find file: {}{}", relative_path, Path::EXTENSION),
                GeneralError,
                Some(self.file_pos.to_owned()),
            ))
        }
    }
}
