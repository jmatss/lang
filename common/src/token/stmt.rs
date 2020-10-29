use super::{
    block::Function,
    expr::{Expr, Var},
    op::AssignOperator,
};
use crate::{
    error::{CustomResult, LangError, LangErrorKind::GeneralError},
    ENV_VAR,
};
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Return(Option<Expr>),
    // Yield ~= Break with a value
    Yield(Expr),
    Break,
    // Continue == Next
    Continue,

    Use(Path),
    Package(Path),

    /// Inc/dec are statements so that they can't be used in other expressions.
    Increment(Expr),
    Decrement(Expr),

    // Defer -> Run this expression at the end of the current block scope.
    /// The "Defer" is the place in the code where the "defer <expr>" was written
    /// in the actual source code.
    /// "DeferExec" statements will be added during the analyzing stage
    /// at places in the AST where the deferred expression should be executed,
    /// i.e. when the block ends or when it branches away.
    Defer(Expr),
    DeferExec(Expr),

    // The lhs can't be a "Variable" directly since it needs to support
    // ex. array indexing and dereferencing. But evaluationg the lhs expressions
    // MUST evaluate to a variable.
    // The valid lhs expressions are (Variable or wrapping a Variable):
    //   Variable
    //   bin op:
    //     Dot (both lhs and rhs as Variables)
    //   un op:
    //     Deref
    //     Address
    // The "middle expr" is the lhs and the "right expr" is the rhs of the assignment.
    Assignment(AssignOperator, Expr, Expr),

    // Used both for "var" and "const" variables. The expr options will be Some
    // if this var decl also has han initializer.
    VariableDecl(Var, Option<Expr>),

    // TODO: Implement extern for variables as well.
    // Declaration of extern functions.
    ExternalDecl(Function),

    // static, private etc.
    Modifier(Modifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Modifier {
    Const,
    External,
    Static,
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub idents: Vec<String>,
}

impl Path {
    const EXTENSION: &'static str = ".ren";

    pub fn new(idents: Vec<String>) -> Self {
        Path { idents }
    }

    /// Returns the path to a file. First looks for the file in the env var
    /// "LANG_HOME". If it isn't found, assume the file is located relative
    /// to the current working directory.
    pub fn to_file_path(&self) -> CustomResult<String> {
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
                    )
                })?
                .into())
        } else {
            Err(LangError::new(
                format!("Unable to find file: {}{}", relative_path, Path::EXTENSION),
                GeneralError,
            ))
        }
    }
}
