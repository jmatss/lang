use crate::parser::abstract_syntax_tree::AST;
use crate::analyzer::action_tree::ActionTree;

pub struct Analyzer<'a> {
    ast: &'a AST,
    action_tree: ActionTree,
}
/*
impl<'a> Analyzer<'a> {
    pub fn new(ast: &AST) -> Self {
        let action_tree = ActionTree::new();
        Analyzer { ast, action_tree:  }
    }
}

pub fn analyze(ast: &AST) {
    let analyser = Analyzer::new(ast);
}
*/