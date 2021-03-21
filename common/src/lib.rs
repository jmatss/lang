#[macro_use]
extern crate log;

/// A unique number given to every block.
pub type BlockId = usize;

/// A unique ID used to reference specific types.
#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Hash, Clone, Copy)]
pub struct TypeId(pub u64);

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
