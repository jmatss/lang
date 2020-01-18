use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug)]
pub enum CustomError {
    LexError(String),
    ParseError(String),
}

impl Error for CustomError {}

impl Display for CustomError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            CustomError::LexError(x) => write!(f, "{}", x),
            CustomError::ParseError(x) => write!(f, "{}", x)
        }
    }
}