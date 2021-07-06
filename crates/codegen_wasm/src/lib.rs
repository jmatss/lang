use std::collections::HashMap;

use spec::types::GlobalIdx;

use common::{ctx::ast_ctx::AstCtx, BlockId};

mod fn_collector;
mod module;
mod spec;

/// Returns a map of all global variables.
///
/// The String in the key tuple is the full_name of the variable (including a
/// potential copynr) and the BlockID is the block in which the variable was
/// declared (should always be in the default block 0 for now, might this change
/// in the future?). The values in the returned map are the wasm global index
/// assigned to the corresponding variables.
///
/// One can see it as the key of the map being the identifier for the variable
/// in the AST and the value of the map being the identifier in wasm.
pub fn globals(ast_ctx: &AstCtx) -> HashMap<(String, BlockId), GlobalIdx> {
    let mut globals = HashMap::default();
    let mut global_idx = 0;
    for (key, var) in &ast_ctx.variables {
        if var.read().unwrap().is_global {
            globals.insert(key.clone(), GlobalIdx(global_idx));
            global_idx += 1;
        }
    }
    globals
}
