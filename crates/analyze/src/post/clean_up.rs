use std::{collections::HashMap, hash::Hash, rc::Rc};

use common::ctx::ast_ctx::AstCtx;

/// The analyzing stage is done. Remove all references to items in the look-up
/// tables that has been removed from the AST.
pub fn clean_up(ast_ctx: &mut AstCtx) {
    remove_unused(&mut ast_ctx.variables);
    remove_unused(&mut ast_ctx.fns);
    remove_unused(&mut ast_ctx.adts);
    remove_unused(&mut ast_ctx.traits);
}

fn remove_unused<K, V>(map: &mut HashMap<K, Rc<V>>)
where
    K: Eq + Hash,
{
    map.retain(|_, v| Rc::strong_count(v) >= 2);
}
