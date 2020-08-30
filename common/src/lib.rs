#[macro_use]
extern crate log;

/// A unique number given to every block.
pub type BlockId = usize;

pub mod error;
pub mod iter;
pub mod token;
pub mod variable_type;
