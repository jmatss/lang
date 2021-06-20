use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::FromIterator,
};

use common::{
    error::{LangError, LangResult},
    path::LangPathPart,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader},
        expr::FnCall,
    },
    ty::{get::get_generic_ident, inner_ty::InnerTy, is::is_generic, ty::Ty},
};

use crate::{traverse_ctx::TraverseCtx, visitor::Visitor};

/// Goes through all function calls done on variables with a generic type and
/// makes sure that the function names can be found in traits required on the
/// generic type in a "where" clause.
pub struct TraitsFnAnalyzer {
    /// Contains a map mapping names of generics to names of methods that the
    /// specific generic implements (according to the "while" clause).
    /// This map will be set everytime a `implement` block is seen. It will
    /// then be accessed when the containing methods are traversed.
    ///
    /// This variable will be set/re-set for every new "implement" block visited.
    /// This will only contain generics related to ADT for the impl block.
    generic_trait_method_names: Option<HashMap<String, HashSet<String>>>,

    errors: Vec<LangError>,
}

impl TraitsFnAnalyzer {
    pub fn new() -> Self {
        Self {
            generic_trait_method_names: None,
            errors: Vec::default(),
        }
    }

    /// Given a ADT `adt`, fetches and stores mappings from the ADTs generic names
    /// to the trait method names that they implement. This information will be used
    /// to verify that any function call on the generic type is valid according to
    /// the "where" clause.
    fn store_generic_trait_method_names(&mut self, ctx: &TraverseCtx, adt: &Adt) -> LangResult<()> {
        let implements = if let Some(implements) = &adt.implements {
            implements
        } else {
            self.generic_trait_method_names = None;
            return Ok(());
        };

        let mut generic_trait_method_names: HashMap<String, HashSet<String>> = HashMap::default();

        // Go through one generic at a time and fetch all trait method names
        // for that specific generic and insert into the `generic_trait_method_names`
        // map.
        // The name will be the name of the generic (`gen_name`) and the
        // values are the names of methods for the traits that the generic
        // implements.
        for (gen_name, trait_tys) in implements {
            for trait_type_id in trait_tys {
                let trait_ty = ctx.ty_env.lock().unwrap().ty_clone(*trait_type_id)?;
                let trait_path = if let Ty::CompoundType(InnerTy::Trait(trait_name), ..) = trait_ty
                {
                    trait_name
                } else {
                    let err = ctx.ast_ctx.err(format!(
                        "Non trait type used in \"where\" clause for struct \"{}\", \
                                enforced on the generic type \"{}\". Found type: {:#?}",
                        &adt.name, gen_name, trait_ty
                    ));
                    self.errors.push(err);
                    continue;
                };

                let trait_method_names = match ctx
                    .ast_ctx
                    .get_trait_method_names(&ctx.ty_env.lock().unwrap(), &trait_path.without_gens())
                {
                    Ok(trait_method_names) => trait_method_names,
                    Err(err) => {
                        self.errors.push(err);
                        continue;
                    }
                };

                match generic_trait_method_names.entry(gen_name.clone()) {
                    Entry::Occupied(mut o) => {
                        o.get_mut().extend(trait_method_names);
                    }
                    Entry::Vacant(v) => {
                        v.insert(HashSet::from_iter(trait_method_names));
                    }
                }
            }
        }

        self.generic_trait_method_names = Some(generic_trait_method_names);
        Ok(())
    }

    /// Checks that the generic with name `generic_name` implements a trait
    /// containing a function named `method_name`.
    fn is_valid_method(&mut self, generic_name: &str, method_name: &str) -> bool {
        self.generic_trait_method_names
            .as_ref()
            .map(|map| map.get(generic_name))
            .flatten()
            .map(|method_names| method_names.contains(method_name))
            .unwrap_or_else(|| false)
    }
}

impl Visitor for TraitsFnAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Implement(impl_path, ..), ..) = ast_token {
            let full_impl_path = match ctx.ast_ctx.get_module(ctx.block_id) {
                Ok(Some(mut full_impl_path)) => {
                    let impl_ident = impl_path.last().unwrap().0.clone();
                    full_impl_path.push(LangPathPart(impl_ident, None));
                    full_impl_path
                }
                Ok(None) => {
                    let mut full_impl_path = impl_path.clone();
                    let last_part = full_impl_path.pop().unwrap();
                    full_impl_path.push(LangPathPart(last_part.0, None));
                    full_impl_path
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let adt = match ctx
                .ast_ctx
                .get_adt(&ctx.ty_env.lock().unwrap(), &full_impl_path)
            {
                Ok(adt) => adt,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let adt = adt.as_ref().borrow().read().unwrap();

            if let Err(err) = self.store_generic_trait_method_names(&ctx, &adt) {
                self.errors.push(err);
            }
        }
    }

    /// Check any function call in which the `method_adt` is a generic.
    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if let Some(method_adt_type_id) = &fn_call.method_adt {
            match is_generic(&ctx.ty_env.lock().unwrap(), *method_adt_type_id) {
                Ok(true) => (),
                Ok(false) => return,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let ty_env_lock = ctx.ty_env.lock().unwrap();
            let generic_name = match get_generic_ident(&ty_env_lock, *method_adt_type_id) {
                Ok(name) => name,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let method_name = &fn_call.name;

            if !self.is_valid_method(generic_name, method_name) {
                let err = ctx.ast_ctx.err(format!(
                    "Used method named \"{0}\" on value with the generic type \"{1}\". \
                    No trait enforced on \"{1}\" contains a method with that name.\n\
                    At position: {2:#?}",
                    method_name, &generic_name, fn_call.file_pos
                ));
                self.errors.push(err);
            }
        }
    }
}
