use crate::AnalyzeContext;
use std::{collections::HashMap, hash::Hash, rc::Rc};

/// The analyzing stage is done. Remove all references to items in the look-up
/// tables that has been removed from the AST.
pub fn clean_up(analyze_context: &mut AnalyzeContext) {
    remove_unused(&mut analyze_context.variables);
    remove_unused(&mut analyze_context.fns);
    remove_unused(&mut analyze_context.adts);
    remove_unused(&mut analyze_context.traits);
}

fn remove_unused<K, V>(map: &mut HashMap<K, Rc<V>>)
where
    K: Eq + Hash,
{
    map.retain(|_, v| Rc::strong_count(v) > 1);
}
