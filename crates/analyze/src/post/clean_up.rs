use std::{collections::HashMap, hash::Hash, sync::Arc};

use common::{ctx::ast_ctx::AstCtx, hash::TyEnvHash, hash_map::TyEnvHashMap};

/// The analyzing stage is done. Remove all references to items in the look-up
/// tables that has been removed from the AST.
pub fn clean_up(ast_ctx: &mut AstCtx) {
    remove_unused(&mut ast_ctx.variables);
    remove_unused_ty_env(&mut ast_ctx.fns);
    remove_unused_ty_env(&mut ast_ctx.adts);
    remove_unused_ty_env(&mut ast_ctx.traits);
}

fn remove_unused<K, V>(map: &mut HashMap<K, Arc<V>>)
where
    K: Eq + Hash,
{
    map.retain(|_, v| Arc::strong_count(v) >= 2);
}

fn remove_unused_ty_env<K: TyEnvHash, V>(map: &mut TyEnvHashMap<K, Arc<V>>) {
    map.retain(|_, v| Arc::strong_count(v) >= 2);
}
