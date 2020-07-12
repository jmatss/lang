use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum CustomError {
    LexError(String),
    ParseError(String),
    AnalyzeError(String),
    GenerationError(String),
}

impl Error for CustomError {}

impl Display for CustomError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            CustomError::LexError(x) => write!(f, "{}", x),
            CustomError::ParseError(x) => write!(f, "{}", x),
            CustomError::AnalyzeError(x) => write!(f, "{}", x),
            CustomError::GenerationError(x) => write!(f, "{}", x),
        }
    }
}

impl From<std::num::ParseIntError> for CustomError {
    fn from(e: std::num::ParseIntError) -> Self {
        CustomError::GenerationError(e.to_string())
    }
}

impl From<std::num::ParseFloatError> for CustomError {
    fn from(e: std::num::ParseFloatError) -> Self {
        CustomError::GenerationError(e.to_string())
    }
}
