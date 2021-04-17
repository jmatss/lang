#[macro_use]
extern crate log;

/// A unique number given to every block.
pub type BlockId = usize;

/// A arbitrary unique ID.
pub type UniqueId = u64;

/// A unique ID used to reference specific types. All cmp/eq function only looks
/// at the unique ID, the scope information is ignored.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct TypeId(pub u64);

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub const ARGC_GLOBAL_VAR_NAME: &str = "_lang_argc_global";
pub const ARGV_GLOBAL_VAR_NAME: &str = "_lang_argv_global";
pub const ARGC_PARAM_VAR_NAME: &str = "_lang_argc_param";
pub const ARGV_PARAM_VAR_NAME: &str = "_lang_argv_param";

/// The name of the environment variable pointing to the path of "lang".
/// This will be used to resolve the std lib.
pub const ENV_VAR: &str = "LANG_HOME";

//pub mod cf_traverser;
//pub mod cf_visitor;
pub mod ctx;
pub mod error;
pub mod file;
pub mod iter;
pub mod path;
pub mod token;
pub mod traverse;
pub mod ty;
pub mod util;
