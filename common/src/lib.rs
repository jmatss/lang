#[macro_use]
extern crate log;

/// A unique number given to every block.
pub type BlockId = usize;

/// A unique identifier given to every unknown type.
pub type TypeId = String;

/// The name of the environment variable pointing to the path of "lang".
/// This will be used to resolve the std lib.
pub const ENV_VAR: &str = "LANG_HOME";

//pub mod cf_traverser;
//pub mod cf_visitor;
pub mod empty_visitor;
pub mod error;
pub mod file;
pub mod iter;
pub mod path;
pub mod token;
pub mod traverser;
pub mod ty;
pub mod type_info;
pub mod util;
pub mod visitor;
