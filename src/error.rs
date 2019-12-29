use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug)]
pub enum CustomError {
    Pass1Error(String),
    Pass2Error(String),
}

impl Error for CustomError {}

impl Display for CustomError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            CustomError::Pass1Error(x) => write!(f, "{}", x),
            CustomError::Pass2Error(x) => write!(f, "{}", x)
        }
    }
}