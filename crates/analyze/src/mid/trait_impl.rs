use common::{
    eq::path_eq,
    error::LangError,
    hash::DerefType,
    token::block::{Block, BlockHeader},
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::to_string::to_string_path,
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

            // TODO: Better way to do this, do in constant time instead of
            //       linear. Since `adt.implemented_traits` contains the paths
            //       including generics, it is no easy way to lookup trait impls
            //       without generics or with different generics.
            let mut contains_duplicate_trait = false;

            let new_trait_path_without_gens = trait_path.without_gens();
            for old_trait_path in adt.implemented_traits.values() {
                let is_eq = match path_eq(
                    &ty_env_guard,
                    &new_trait_path_without_gens,
                    &old_trait_path.without_gens(),
                    DerefType::None,
                ) {
                    Ok(is_eq) => is_eq,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };
                if is_eq {
                    contains_duplicate_trait = true;
                    break;
                }
            }

            if contains_duplicate_trait {
                self.errors.push(ctx.ast_ctx.err(format!(
                    "ADT \"{}\" contains multiple impl's of the trait \"{}\". \
                    Only one instance of a impl is allowed per ADT. \
                    This restriction is in place to prevent problems with multiple \
                    implementations of a trait with different generics.",
                    to_string_path(&ty_env_guard, adt_path),
                    to_string_path(&ty_env_guard, &trait_path.without_gens()),
                )));
                return;
            }

            if let Err(err) =
                adt.implemented_traits
                    .insert(&ty_env_guard, DerefType::Deep, trait_path.clone())
            {
                self.errors.push(err);
            }
        }
    }
}
