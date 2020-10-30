use std::collections::{hash_map::Entry, HashMap};

use common::{
    error::LangError,
    token::{
        ast::{AstToken, Token},
        block::{BlockHeader, Struct},
    },
    traverser::TraverseContext,
    visitor::Visitor,
};

pub struct TypeConverter {
    generic_structs: HashMap<String, Vec<Struct>>,

    errors: Vec<LangError>,
}

/// Remove any structs/interfaces that contain generics. Add new struct/interfaces
/// that contain replaced generics (static dispatch). The old blocks are set to
/// "Empty".
impl TypeConverter {
    pub fn new(generic_structs: HashMap<String, Vec<Struct>>) -> Self {
        Self {
            generic_structs,
            errors: Vec::default(),
        }
    }
}

impl Visitor for TypeConverter {
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
        if let Token::Block(BlockHeader::Default, _, body) = &mut ast_token.token {
            let mut i = 0;
            while i < body.len() {
                let body_token = body.get(i).cloned().expect("Known to be in bounds.");
                let mut to_be_removed = false;

                if let Token::Block(BlockHeader::Struct(struct_), old_id, ..) = &body_token.token {
                    if struct_.generic_params.is_some() {
                        let old_token = body_token.clone();

                        // Flag that the old struct containg unsolved generics
                        // to be removed (/set to empty). Structs that already
                        // have had their generics replaced will have a colon
                        // in their name and shouldn't be removed.
                        if !struct_.name.contains(':') {
                            to_be_removed = true;
                        }

                        // Insert the new struct variants that have replaced the
                        // generic parameters with an actual real type.
                        match self.generic_structs.entry(struct_.name.clone()) {
                            Entry::Occupied(o) => {
                                for generic_struct in o.get() {
                                    let header = BlockHeader::Struct(generic_struct.clone());
                                    let struct_body = Vec::with_capacity(0);

                                    let token = Token::Block(header, *old_id, struct_body);
                                    let new_token = AstToken {
                                        token,
                                        line_nr: old_token.line_nr,
                                        column_nr: old_token.column_nr,
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
                }

                // Set the old struct containing generics to empty
                if to_be_removed {
                    *body.get_mut(i).expect("Known to be in bounds.") = AstToken {
                        token: Token::Empty,
                        line_nr: ast_token.line_nr,
                        column_nr: ast_token.column_nr,
                    };
                }

                i += 1;
            }
        }
    }

    // TODO: Implement similar generic logic as for structs.
    fn visit_interface(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {}
    fn visit_enum(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {}
}
