use std::{
    cell::{Ref, RefCell},
    collections::{hash_map::Entry, HashMap, HashSet},
    rc::Rc,
};

use common::{
    error::LangError,
    token::{
        ast::{AstToken, Token},
        block::Function,
        block::{BlockHeader, Struct},
    },
    traverser::AstTraverser,
    traverser::TraverseContext,
    ty::ty::Ty,
    visitor::Visitor,
    BlockId,
};
use log::debug;

use crate::AnalyzeContext;

use super::generic_replace::GenericsReplacer;

pub struct TypeConverter<'a> {
    /// Needed to look up structs.
    analyze_context: &'a mut AnalyzeContext,

    /// The key is the name of the original struct (WITHOUT generics in the name).
    generic_structs: HashMap<String, Vec<(Rc<RefCell<Struct>>, Ty)>>,
    /// The outer key is the name of the modified struct (WITH generics in the name).
    /// The inner key is the name of the method.
    generic_struct_methods: HashMap<String, HashMap<String, Rc<RefCell<Function>>>>,

    /// A set of the "implement" blocks that is to be removed from the AST.
    /// These are blocks that belongs to structs that have been removed because
    /// they contained generics and have been replaced. This means that the
    /// "implement" block also will be copied and replaced.
    impls_to_be_modified: HashSet<String>,

    errors: Vec<LangError>,
}

/// Remove any structs/interfaces that contain generics. Add new struct/interfaces
/// that contain replaced generics (static dispatch). The old blocks are set to
/// "Empty". This will done for the "implement" block and all its contained
/// methods as well.
impl<'a> TypeConverter<'a> {
    pub fn new(
        analyze_context: &'a mut AnalyzeContext,
        generic_structs: HashMap<String, Vec<(Rc<RefCell<Struct>>, Ty)>>,
        generic_struct_methods: HashMap<String, HashMap<String, Rc<RefCell<Function>>>>,
    ) -> Self {
        Self {
            analyze_context,
            generic_structs,
            generic_struct_methods,
            impls_to_be_modified: HashSet::default(),
            errors: Vec::default(),
        }
    }

    fn modify_struct(
        &mut self,
        struct_: Ref<Struct>,
        body_token: &AstToken,
        body: &mut Vec<AstToken>,
        i: usize,
        old_id: BlockId,
        parent_id: BlockId,
    ) {
        let mut to_be_removed = false;

        if struct_.generic_params.is_some() {
            // Flag that the old struct containg unsolved generics to be removed
            // (/set to empty). Structs that already have had their generics
            // replaced will have a colon in their name and shouldn't be removed,
            // do early return in that case.
            if !struct_.name.contains(':') {
                to_be_removed = true;
                self.impls_to_be_modified.insert(struct_.name.clone());
            } else {
                return;
            }

            // Insert the new struct variants that have replaced the generic
            // parameters with an actual real type. These new struct will be
            // inserted both into the AST and into the struct lookup table
            // (in AnalyzeContext).
            match self.generic_structs.entry(struct_.name.clone()) {
                Entry::Occupied(ref mut o) => {
                    for (generic_struct, _) in o.get_mut() {
                        debug!(
                            "Creating new generic struct in block id {}: {:#?}",
                            old_id, generic_struct
                        );

                        // Insert the new struct into the lookup table.
                        let key = (generic_struct.borrow().name.clone(), parent_id);
                        self.analyze_context
                            .structs
                            .insert(key, Rc::clone(generic_struct));

                        // Create a new AST token that will be inserted
                        // into the AST.
                        let header = BlockHeader::Struct(Rc::clone(generic_struct));
                        let struct_body = Vec::with_capacity(0);

                        let token = Token::Block(header, old_id, struct_body);
                        let new_token = AstToken {
                            token,
                            line_nr: body_token.line_nr,
                            column_nr: body_token.column_nr,
                        };

                        // Slower to shift all the ast tokens to the
                        // right, but ensure that the tokens are
                        // inserted next to the old struct and
                        // doesn't ex. get added after the EOF token.
                        body.insert(i + 1, new_token);
                    }
                }
                Entry::Vacant(_) => {
                    // Do not add any new structs if there are no
                    // implemententations of the generic struct.
                    // The newly added struct implementations into
                    // `body` will also end up in here and should
                    // be ignored.
                }
            }
        }

        // Set the old struct containing generics to empty in the AST
        // and remove the struct from the lookup table.
        if to_be_removed {
            let key = (struct_.name.clone(), parent_id);
            self.analyze_context.structs.remove(&key);

            *body.get_mut(i).expect("Known to be in bounds.") = AstToken {
                token: Token::Empty,
                line_nr: body_token.line_nr,
                column_nr: body_token.column_nr,
            };
        }
    }
}

// TODO: Refactor the ugly stuff in here.

impl<'a> Visitor for TypeConverter<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    // TODO: Currently the assumption is that all structs are stored in the
    //       default block. For later, when this might not be the case, need
    //       to find another better way to do this. Will probably have to
    //       implement some helper functions to modify the AST, remove/add etc.
    /// Create new structs for generic implementations.
    ///
    /// OBS! This needs to be ran first, before
    fn visit_default_block(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Default, parent_id, body) = &mut ast_token.token {
            let mut i = 0;

            // Iterate through all the structs in the AST.
            while i < body.len() {
                let body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // Modify and create the new structs. The old struct will also
                // be removed.
                if let Token::Block(BlockHeader::Struct(struct_), old_id, ..) = &body_token.token {
                    self.modify_struct(struct_.borrow(), &body_token, body, i, *old_id, *parent_id)
                }

                i += 1;
            }

            i = 0;

            // Iterate through all the implement blocks in the AST.
            while i < body.len() {
                let body_token = body.get(i).cloned().expect("Known to be in bounds.");

                // Modify and create the new implement blocks and their containing
                // methods. The old implement blocks and the methods will be removed.
                if let Token::Block(BlockHeader::Implement(struct_name), old_id, _) =
                    &body_token.token
                {
                    if self.impls_to_be_modified.contains(struct_name) {
                        match self.generic_structs.entry(struct_name.into()) {
                            Entry::Occupied(ref mut o) => {
                                for (generic_struct, gen_struct_ty) in o.get_mut() {
                                    // Get the actual struct instance that have had
                                    // the generics implemented.
                                    let struct_ = match self
                                        .analyze_context
                                        .get_struct(&generic_struct.borrow().name, *old_id)
                                    {
                                        Ok(struct_) => struct_,
                                        Err(err) => {
                                            self.errors.push(err);
                                            return;
                                        }
                                    };

                                    struct_.borrow_mut().methods = Some(HashMap::default());

                                    // Make a clone of the old implement block.
                                    // Change the name of this new impl block
                                    // and also replace the old methods with
                                    // the new methods for the specific struct
                                    // instances from `generic_struct_methods`.
                                    let mut new_impl_token = body_token.clone();
                                    let new_impl_body = if let Token::Block(
                                        BlockHeader::Implement(ident),
                                        _,
                                        new_impl_body,
                                    ) = &mut new_impl_token.token
                                    {
                                        *ident = generic_struct.borrow().name.clone();
                                        new_impl_body
                                    } else {
                                        unreachable!()
                                    };

                                    // Replace the "Function" struct in the
                                    // new method block. This new method block
                                    // will be inserted into a new impl block.
                                    for method in new_impl_body {
                                        if let Token::Block(
                                            BlockHeader::Function(inner_func),
                                            _,
                                            body,
                                        ) = &mut method.token
                                        {
                                            let new_func = if let Some(new_func) = self
                                                .generic_struct_methods
                                                .get_mut(&generic_struct.borrow().name)
                                                .map(|m| m.remove(&inner_func.borrow().name))
                                                .flatten()
                                            {
                                                new_func
                                            } else {
                                                let err = self.analyze_context.err(format!(
                                                    "Unable to get func \"{}\" when creating new impl block for struct_init \"{}\".",
                                                    &inner_func.borrow().name,
                                                    &generic_struct.borrow().name
                                                ));
                                                self.errors.push(err);
                                                return;
                                            };

                                            let new_func_name = new_func.borrow().name.clone();
                                            *inner_func = Rc::clone(&new_func);

                                            // Replace all found generic types
                                            // in the function bodies with the actual
                                            // generics implementation.
                                            if let Some(Ty::CompoundType(_, generics_impl)) =
                                                &inner_func.borrow().method_structure
                                            {
                                                let mut generics_replacer = GenericsReplacer::new(
                                                    generics_impl,
                                                    &struct_name,
                                                    gen_struct_ty,
                                                );

                                                for body_token in body {
                                                    AstTraverser::new()
                                                        .add_visitor(&mut generics_replacer)
                                                        .traverse(body_token);
                                                }
                                            };

                                            // Make the field "methods" in the
                                            // "Struct" struct point to the newly
                                            // created method.
                                            if let Some(methods) = &mut struct_.borrow_mut().methods
                                            {
                                                methods
                                                    .insert(new_func_name, Rc::clone(&inner_func));
                                            }
                                        }
                                    }

                                    // Slower to shift all the ast tokens to the
                                    // right, but ensure that the tokens are
                                    // inserted next to the old "implement" and
                                    // doesn't ex. get added after the EOF token.
                                    body.insert(i + 1, new_impl_token);
                                }
                            }
                            Entry::Vacant(_) => {}
                        }

                        // Remove/empty the old implement block.
                        *body.get_mut(i).expect("Known to be in bounds.") = AstToken {
                            token: Token::Empty,
                            line_nr: body_token.line_nr,
                            column_nr: body_token.column_nr,
                        };
                    }
                }

                i += 1;
            }
        }
    }

    // TODO: Implement similar generic logic as for structs.
    fn visit_interface(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {}
    fn visit_enum(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {}
}
