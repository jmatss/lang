use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

use backtrace::Backtrace;
use log::{log_enabled, Level};

pub type IrResult<T> = Result<T, IrError>;

#[derive(Debug, Clone)]
pub struct IrError {
    pub msg: String,
    pub backtrace: Option<Backtrace>,
}

impl IrError {
    pub fn new(msg: String) -> Self {
        IrError::new_backtrace(msg, true)
    }

    pub fn new_backtrace(msg: String, contain_backtrace: bool) -> Self {
        let backtrace = if log_enabled!(Level::Debug) && contain_backtrace {
            Some(Backtrace::new())
        } else {
            None
        };

        Self { msg, backtrace }
    }
}

impl Error for IrError {}

impl Display for IrError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.msg)?;

        if log_enabled!(Level::Debug) {
            if let Some(backtrace) = &self.backtrace {
                write!(f, "\n{:#?}", backtrace)?;
            }
        }

        Ok(())
    }
}
