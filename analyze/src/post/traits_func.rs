use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader},
        expr::FuncCall,
    },
    traverser::TraverseContext,
    ty::{inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
    BlockId,
};
use std::{
    cell::Ref,
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::FromIterator,
};

/// Goes through all function calls done on variables with a generic type and
/// makes sure that the function names can be found in traits required on the
/// generic type in a "where" clause.
pub struct TraitsFuncAnalyzer<'a> {
    analyze_context: &'a AnalyzeContext,

    /// Contains a map mapping names of generics to names of methods that the
    /// specific generic implements (according to the "wwhile" clause).
    /// This map will be set everytime a `implement` block is seen. It will
    /// then be accessed when the containing methods are traversed.
    generic_trait_method_names: Option<HashMap<String, HashSet<String>>>,

    errors: Vec<LangError>,
}

impl<'a> TraitsFuncAnalyzer<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            generic_trait_method_names: None,
            errors: Vec::default(),
        }
    }

    /// Given a ADT `adt`, fetches and stores mappings from the ADTs generic names
    /// to the trait method names that they implement. This information will be used
    /// to verify that any function call on the generic type is valid according to
    /// the "where" clause.
    fn store_generic_trait_method_names(&mut self, adt: Ref<Adt>, block_id: BlockId) {
        let implements = if let Some(implements) = &adt.implements {
            implements
        } else {
            self.generic_trait_method_names = None;
            return;
        };

        let mut generic_trait_method_names: HashMap<String, HashSet<String>> = HashMap::default();

        // Go through one generic at a time and fetch all trait method names
        // for that specific generic and insert into the `generic_trait_method_names`
        // map.
        // The name will be the name of the generic (`gen_name`) and the
        // values are the names of methods for the traits that the generic
        // implements.
        for (gen_name, trait_tys) in implements {
            for trait_ty in trait_tys {
                let trait_name = if let Ty::CompoundType(InnerTy::Trait(trait_name), ..) = trait_ty
                {
                    trait_name
                } else {
                    let err = self.analyze_context.err(format!(
                        "Non trait type used in \"where\" clause for struct \"{}\", \
                                enforced on the generic type \"{}\". Found type: {:#?}",
                        &adt.name, gen_name, trait_ty
                    ));
                    self.errors.push(err);
                    continue;
                };

                let trait_method_names = match self
                    .analyze_context
                    .get_trait_method_names(trait_name, block_id)
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

impl<'a> Visitor for TraitsFuncAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(ident, ..), ..) = ast_token {
            let adt = match self.analyze_context.get_adt(ident, ctx.block_id) {
                Ok(adt) => adt,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.store_generic_trait_method_names(adt.borrow(), ctx.block_id);
        }
    }

    /// Check any function call in which the `method_structure` is a generic.
    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        if let Some(method_structure) = &func_call.method_adt {
            if !method_structure.is_generic() {
                return;
            }

            let generic_name = method_structure.get_ident().unwrap();
            let method_name = &func_call.name;

            if !self.is_valid_method(&generic_name, method_name) {
                let err = self.analyze_context.err(format!(
                    "Used method named \"{0}\" on value with the generic type \"{1}\". \
                    No trait enforced on \"{1}\" contains a method with that name.\n\
                    At position: {2:#?}",
                    method_name, &generic_name, func_call.file_pos
                ));
                self.errors.push(err);
            }
        }
    }
}
