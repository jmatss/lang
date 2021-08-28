use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    sync::Arc,
};

use common::{
    error::{LangError, LangResult},
    path::LangPath,
    token::{
        block::{Adt, Block, BlockHeader, Fn},
        expr::FnCall,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{get::get_generic_ident, is::is_generic},
};

/// Goes through all function calls done on variables with a generic type and
/// makes sure that the function names can be found in traits required on the
/// generic type in a "where" clause.
pub struct TraitsFnAnalyzer {
    /// Contains a map mapping names of generics to names of methods that the
    /// specific generic implements (according to the "while" clause).
    /// It will then be accessed when the containing methods are traversed.
    ///
    /// This variable will be set/re-set for every new ADT or impl block visited.
    /// This will only contain generics related to ADT for the impl block.
    adt_trait_method_names: Option<HashMap<String, HashSet<String>>>,

    /// Contains a map mapping names of generics to names of methods that the
    /// specific generic implements (according to the "while" clause).
    /// It will then be accessed when the body of the function is traversed.
    ///
    /// This variable will be set/re-set for every new function block visited.
    /// This will only contain generics related to function itself, any ADT
    /// related generics will be stored in `self.adt_trait_method_names`.
    fn_trait_method_names: Option<HashMap<String, HashSet<String>>>,

    errors: Vec<LangError>,
}

impl TraitsFnAnalyzer {
    pub fn new() -> Self {
        Self {
            adt_trait_method_names: None,
            fn_trait_method_names: None,
            errors: Vec::default(),
        }
    }

    /// Given a map of `implements` (representing a "where" clause), mapping
    /// generic names to traits that they must implemented, fetches all
    /// methods for those traits and returns a map mapping the generic names
    /// to methods (instead of the original gen_name -> trait).
    ///
    /// The given `implements` can be either from a ADT or function. This
    /// information will be used to verify that any function call on the generic
    /// type is valid according to the "where" clause.
    fn fetch_trait_method_names(
        &mut self,
        ctx: &TraverseCtx,
        implements: &HashMap<String, Vec<LangPath>>,
    ) -> LangResult<Option<HashMap<String, HashSet<String>>>> {
        let mut trait_method_names: HashMap<String, HashSet<String>> = HashMap::default();

        // Iterateos through one generic at a time and fetch all trait method
        // names for that specific generic and insert into the
        // `generic_trait_method_names` map. The name will be the name of the
        // generic (`gen_name`) and the values are the names of methods for the
        // traits that the generic implements.
        for (gen_name, trait_paths) in implements {
            for trait_path in trait_paths {
                let cur_trait_method_names = ctx.ast_ctx.get_trait_method_names(
                    &ctx.ty_env.lock().unwrap(),
                    &trait_path.without_gens(),
                )?;

                match trait_method_names.entry(gen_name.clone()) {
                    Entry::Occupied(mut o) => {
                        o.get_mut().extend(cur_trait_method_names);
                    }
                    Entry::Vacant(v) => {
                        v.insert(cur_trait_method_names);
                    }
                }
            }
        }

        if !trait_method_names.is_empty() {
            Ok(Some(trait_method_names))
        } else {
            Ok(None)
        }
    }

    fn store_adt_trait_method_names(&mut self, ctx: &TraverseCtx, adt: &Adt) -> LangResult<()> {
        self.adt_trait_method_names = if let Some(implements) = &adt.implements {
            self.fetch_trait_method_names(ctx, implements)?
        } else {
            None
        };
        Ok(())
    }

    fn store_fn_trait_method_names(&mut self, ctx: &TraverseCtx, func: &Fn) -> LangResult<()> {
        self.fn_trait_method_names = if let Some(implements) = &func.implements {
            self.fetch_trait_method_names(ctx, implements)?
        } else {
            None
        };
        Ok(())
    }

    /// Checks that the generic with name `generic_name` implements a trait
    /// containing a function named `method_name`.
    fn is_valid_fn(&mut self, generic_name: &str, method_name: &str) -> bool {
        let in_adt_traits = self
            .adt_trait_method_names
            .as_ref()
            .map(|map| map.get(generic_name))
            .flatten()
            .map(|method_names| method_names.contains(method_name))
            .unwrap_or_else(|| false);
        let in_fn_traits = self
            .fn_trait_method_names
            .as_ref()
            .map(|map| map.get(generic_name))
            .flatten()
            .map(|method_names| method_names.contains(method_name))
            .unwrap_or_else(|| false);
        in_adt_traits || in_fn_traits
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

    fn visit_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { header, .. } = block;

        let adt = match header {
            BlockHeader::Implement(impl_path, ..) => {
                let path_without_gens = match ctx.ast_ctx.get_module(ctx.block_id) {
                    Ok(Some(module)) => {
                        let impl_name = impl_path.last().unwrap().0.clone();
                        module.clone_push(&impl_name, None, None)
                    }
                    Ok(None) => impl_path.without_gens(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                match ctx
                    .ast_ctx
                    .get_adt(&ctx.ty_env.lock().unwrap(), &path_without_gens)
                {
                    Ok(adt) => adt,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            }

            BlockHeader::Struct(adt) | BlockHeader::Union(adt) => Arc::clone(adt),

            _ => return,
        };

        let adt = adt.read().unwrap();
        if let Err(err) = self.store_adt_trait_method_names(&ctx, &adt) {
            self.errors.push(err);
        }
    }

    fn visit_fn(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { header, .. } = block;
        if let BlockHeader::Fn(func) = header {
            let func = func.read().unwrap();
            if let Err(err) = self.store_fn_trait_method_names(&ctx, &func) {
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

            let ty_env_guard = ctx.ty_env.lock().unwrap();
            let generic_name = match get_generic_ident(&ty_env_guard, *method_adt_type_id) {
                Ok(name) => name,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            let method_name = &fn_call.name;

            if !self.is_valid_fn(generic_name, method_name) {
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
