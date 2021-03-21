use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{block::AdtKind, expr::AdtInit},
    traverser::TraverseContext,
    visitor::Visitor,
};

/// Makes sure that all union initializations are correct. A union construction
/// must have exactly one argument, and that argument must be named. The union
/// will then be initialized for the type of the specified named member.
pub struct UnionInitArg<'a> {
    analyze_context: &'a AnalyzeContext,
    errors: Vec<LangError>,
}

impl<'a> UnionInitArg<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for UnionInitArg<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, _ctx: &mut TraverseContext) {
        if let AdtKind::Union = adt_init.kind {
        } else {
            return;
        }

        if adt_init.arguments.len() != 1 {
            let err = self.analyze_context.err(format!(
                "Expected one argument for union init: {:#?}",
                adt_init
            ));
            self.errors.push(err);
            return;
        }

        // TODO: Does one need to check the name/type here? Is already check
        //       in the type inference step.
    }
}
