use common::{
    error::LangError,
    token::{block::AdtKind, expr::AdtInit},
};

use crate::{traverse_ctx::TraverseCtx, visitor::Visitor};

/// Makes sure that all union initializations are correct. A union construction
/// must have exactly one argument, and that argument must be named. The union
/// will then be initialized for the type of the specified named member.
pub struct UnionInitArg {
    errors: Vec<LangError>,
}

impl UnionInitArg {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for UnionInitArg {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &mut TraverseCtx) {
        if !matches!(adt_init.kind, AdtKind::Union) {
            return;
        }

        if adt_init.arguments.len() != 1 {
            let err = ctx.ast_ctx.err(format!(
                "Expected one argument for union init: {:#?}",
                adt_init
            ));
            self.errors.push(err);
        } else if adt_init.arguments.get(0).unwrap().name.is_none() {
            let err = ctx.ast_ctx.err(format!(
                "Expected named argument in union init but was unnamed: {:#?}",
                adt_init
            ));
            self.errors.push(err);
        }
    }
}
