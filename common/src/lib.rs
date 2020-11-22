#[macro_use]
extern crate log;

/// A unique number given to every block.
pub type BlockId = usize;

/// The name of the environment variable pointing to the path of "lang".
/// This will be used to resolve the std lib.
pub const ENV_VAR: &str = "LANG_HOME";

//pub mod cf_traverser;
//pub mod cf_visitor;
pub mod empty_visitor;
pub mod error;
pub mod iter;
pub mod token;
pub mod traverser;
pub mod r#type;
pub mod util;
pub mod visitor;
