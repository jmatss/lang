use std::collections::HashSet;

use common::{
    error::LangError,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{inner_ty::InnerTy, ty::Ty, type_id::TypeId},
};

/// Iterates through all types in the AST and collects all uses of possible tuple
/// types.
pub struct TupleCollector {
    pub tuple_types: HashSet<TypeId>,
    errors: Vec<LangError>,
}

impl TupleCollector {
    pub fn new() -> Self {
        Self {
            tuple_types: HashSet::default(),
            errors: Vec::default(),
        }
    }
}

impl Visitor for TupleCollector {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        match ctx.ty_env.lock().ty(*type_id) {
            Ok(Ty::CompoundType(InnerTy::Tuple(..), ..)) => {
                self.tuple_types.insert(*type_id);
            }
            Ok(_) => (),
            Err(err) => self.errors.push(err),
        }
    }
}
