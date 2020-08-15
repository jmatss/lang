use backtrace::Backtrace;
use inkwell::support::LLVMString;
use log::Level;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct LangError {
    msg: String,
    kind: LangErrorKind,
    backtrace: Option<Backtrace>,
}

#[derive(Debug)]
pub enum LangErrorKind {
    GeneralError,
    LexError { line_nr: u64, column_nr: u64 },
    ParseError { line_nr: u64, column_nr: u64 },
    AnalyzeError,
    CodeGenError,
    CompileError,
}

impl LangError {
    pub fn new(msg: String, kind: LangErrorKind) -> Self {
        LangError::new_backtrace(msg, kind, true)
    }

    pub fn new_backtrace(msg: String, kind: LangErrorKind, contain_backtrace: bool) -> Self {
        let backtrace = if log_enabled!(Level::Debug) && contain_backtrace {
            Some(Backtrace::new())
        } else {
            None
        };

        Self {
            msg,
            kind,
            backtrace,
        }
    }
}

impl Error for LangError {}

impl Display for LangError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "[{:?}] {}\n{:#?}", self.kind, self.msg, self.backtrace)
    }
}

impl From<std::num::ParseIntError> for LangError {
    fn from(e: std::num::ParseIntError) -> Self {
        LangError::new(e.to_string(), LangErrorKind::CodeGenError)
    }
}

impl From<std::num::ParseFloatError> for LangError {
    fn from(e: std::num::ParseFloatError) -> Self {
        LangError::new(e.to_string(), LangErrorKind::CodeGenError)
    }
}

impl From<std::io::Error> for LangError {
    fn from(e: std::io::Error) -> Self {
        LangError::new(
            e.to_string(),
            LangErrorKind::LexError {
                column_nr: 0,
                line_nr: 0,
            },
        )
    }
}

impl From<LLVMString> for LangError {
    fn from(e: LLVMString) -> Self {
        LangError::new(e.to_string(), LangErrorKind::CodeGenError)
    }
}
