use crate::{file::FilePosition, ENV_VAR};
use backtrace::Backtrace;
use inkwell::support::LLVMString;
use log::Level;
use std::error::Error;
use std::fmt;
use std::{
    env::VarError,
    fmt::{Display, Formatter},
};

pub type LangResult<T> = Result<T, LangError>;

/// Returned from function that checks cyclic dependencies. The two Strings will
/// be the names/idents of the "things" that causes the cyclic dependency.
#[derive(Debug, Clone)]
pub struct CyclicDependencyError(pub String, pub String);

#[derive(Debug, Clone)]
pub struct LangError {
    pub msg: String,
    pub kind: LangErrorKind,
    pub file_pos: Option<FilePosition>,
    pub backtrace: Option<Backtrace>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LangErrorKind {
    GeneralError,
    LexError,
    ParseError,
    AnalyzeError,
    CodeGenError,
    CompileError,
}

impl PartialEq for LangError {
    fn eq(&self, other: &Self) -> bool {
        self.msg == other.msg && self.kind == other.kind && self.file_pos == other.file_pos
    }
}

impl LangError {
    pub fn new(msg: String, kind: LangErrorKind, file_pos: Option<FilePosition>) -> Self {
        LangError::new_backtrace(msg, kind, file_pos, true)
    }

    pub fn new_backtrace(
        msg: String,
        kind: LangErrorKind,
        file_pos: Option<FilePosition>,
        contain_backtrace: bool,
    ) -> Self {
        let backtrace = if log_enabled!(Level::Debug) && contain_backtrace {
            Some(Backtrace::new())
        } else {
            None
        };

        Self {
            msg,
            kind,
            file_pos,
            backtrace,
        }
    }
}

impl Error for LangError {}

impl Display for LangError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        if let Some(file_pos) = self.file_pos {
            write!(f, "[{:?} - {:?}]", self.kind, file_pos)?;
        } else {
            write!(f, "[{:?}]", self.kind)?;
        }

        write!(f, " {}", self.msg)?;

        if log_enabled!(Level::Debug) {
            if let Some(backtrace) = &self.backtrace {
                write!(f, "\n{:#?}", backtrace)?;
            }
        }

        Ok(())
    }
}

impl From<std::num::ParseIntError> for LangError {
    fn from(e: std::num::ParseIntError) -> Self {
        LangError::new(e.to_string(), LangErrorKind::CodeGenError, None)
    }
}

impl From<std::num::ParseFloatError> for LangError {
    fn from(e: std::num::ParseFloatError) -> Self {
        LangError::new(e.to_string(), LangErrorKind::CodeGenError, None)
    }
}

impl From<std::io::Error> for LangError {
    fn from(e: std::io::Error) -> Self {
        LangError::new(e.to_string(), LangErrorKind::LexError, None)
    }
}

impl From<LLVMString> for LangError {
    fn from(e: LLVMString) -> Self {
        LangError::new(e.to_string(), LangErrorKind::CodeGenError, None)
    }
}

impl From<VarError> for LangError {
    fn from(e: VarError) -> Self {
        LangError::new(
            format!("{}: {}", e, ENV_VAR),
            LangErrorKind::GeneralError,
            None,
        )
    }
}
