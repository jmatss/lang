use common::{
    error::LangError,
    hash::DerefType,
    token::block::{Block, BlockHeader},
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
};

/// Iterates through all impl-block and stores the path of the implemented traits
/// in the corresponding ADTs that implements them.
///
/// This should be called after the `PathResolver` stage so that the trait paths
/// are fully qualified including module and generics.
pub struct TraitImplAnalyzer {
    errors: Vec<LangError>,
}

impl TraitImplAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for TraitImplAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_impl(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Implement(adt_path, trait_path),
            ..
        } = block
        {
            let ty_env_guard = ctx.ty_env.lock().unwrap();

            let adt = match ctx.ast_ctx.get_adt(&ty_env_guard, &adt_path.without_gens()) {
                Ok(adt) => adt,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let mut adt = adt.write().unwrap();

            if let Err(err) =
                adt.implemented_traits
                    .insert(&ty_env_guard, DerefType::Deep, trait_path.clone())
            {
                self.errors.push(err);
            }
        }
    }
}
