use common::{
    error::{CyclicDependencyError, LangError, LangErrorKind},
    token::{ast::AstToken, block::BlockHeader, expr::Var},
    traverser::{AstTraverser, TraverseContext},
    ty::ty::Ty,
    visitor::Visitor,
};
use log::debug;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    rc::Rc,
};

/// Can be used to figure out the order in which different structs/enums depends
/// on each other. This is needed to make sure that the "inner" structures are
/// compiled/handled etc. first before the "outer" ones since they depend on them.
///
/// The given `include_impls` specified if a reference in a impl block of a structure
/// should count as a reference or not. If this is set to `true`, references to
/// other structures in impl blocks are considered reference. If it is set to
/// `false`, only the members of the structures are considered references,
/// everything inside the impl blocks are ignored.
pub fn dependency_order(
    ast_token: &mut AstToken,
    include_impls: bool,
    full_names: bool,
) -> Result<Vec<String>, Vec<LangError>> {
    // Step 1: Find all structures and references between them.
    // Step 2: Figure out the correct order to compile them in.

    // The key is the name of a structure and the value set contains names of
    // structures that is referenced from the given "key" structure. This does
    // NOT include recursive dependencies, only direct "top level" references.
    let references = order_step1(ast_token, include_impls, full_names)?;
    match order_step2(&references) {
        Ok(order) => {
            debug!(
                "structures -- references: {:#?}, order: {:#?}",
                references, order
            );
            Ok(order)
        }
        Err(cyc_err) => Err(vec![LangError::new(
            format!(
                "Cyclic dependency between structures \"{}\" and \"{}\". All refs: {:#?}",
                cyc_err.0, cyc_err.1, references
            ),
            LangErrorKind::GeneralError,
            None,
        )]),
    }
}

/// Iterate through all types in the structure and see if it uses/references
/// other structures. In those cases, the referenced structures would need to
/// be handled/compiled before the referencing structures.
///
/// This function will go through all structures in the given `ast_token`
/// root and populate `references` with the references between the structures.
fn order_step1(
    ast_token: &mut AstToken,
    include_impls: bool,
    full_names: bool,
) -> Result<HashMap<String, HashSet<String>>, Vec<LangError>> {
    let mut ref_collector = ReferenceCollector::new(include_impls, full_names);
    AstTraverser::new()
        .add_visitor(&mut ref_collector)
        .traverse_token(ast_token)
        .take_errors()?;

    Ok(ref_collector.references)
}

/// Given the references between "idents" in `references`, figures out the order
/// in which the idents depends/references each other. Returns a error if a cyclic
/// dependency is found.
pub fn order_step2(
    references: &HashMap<String, HashSet<String>>,
) -> Result<Vec<String>, CyclicDependencyError> {
    let mut order: Vec<String> = Vec::with_capacity(references.len());

    for cur_ident in references.keys() {
        let mut idx = 0;

        for prev_ident in &order {
            let prev_references_cur = contains(cur_ident, prev_ident, &references);
            let cur_references_prev = contains(prev_ident, cur_ident, &references);

            if prev_references_cur && cur_references_prev {
                return Err(CyclicDependencyError(cur_ident.into(), prev_ident.into()));
            } else if prev_references_cur {
                // Can't insert the "current" structure before the "previous"
                // since it is being referenced from it. Insert into `order`
                // at this index so that "current" gets compiled before "previous".
                break;
            } else {
                // else if cur_references_prev = >
                //   "Current" references "previous", so "previous" needs to
                //   be compiled first. Keep iterating and find a spot after
                //   "previous" to insert it.
                // else
                //   The two structures has no references between each other.
                //   Keep iterating to find a spot to insert it as "late"
                //   as possible.
            }

            idx += 1;
        }

        if idx > order.len() {
            order.push(cur_ident.into());
        } else {
            order.insert(idx, cur_ident.into());
        }
    }

    debug!("order: {:#?}", order);

    Ok(order)
}

// TODO: This is not effective. Will this be a problem for programs with
//       a lot of structures?
/// Checks if the ident with name `cur_ident` are referenced from the ident with
/// name `ref_ident` recursively. This will find cyclic dependencies.
fn contains(
    cur_ident: &str,
    ref_ident: &str,
    references: &HashMap<String, HashSet<String>>,
) -> bool {
    // HashSet used to detect cyclic dependencies.
    let mut seen_idents = HashSet::default();
    contains_rec(cur_ident, ref_ident, references, &mut seen_idents)
}

fn contains_rec(
    cur_ident: &str,
    ref_ident: &str,
    references: &HashMap<String, HashSet<String>>,
    seen_idents: &mut HashSet<String>,
) -> bool {
    let ref_references = if let Some(ref_references) = references.get(ref_ident) {
        ref_references
    } else {
        return false;
    };

    if ref_references.contains(cur_ident) {
        return true;
    } else {
        for nested_ident in ref_references {
            let is_cyclic_dependency = seen_idents.contains(nested_ident);
            seen_idents.insert(nested_ident.into());

            if is_cyclic_dependency
                || contains_rec(cur_ident, nested_ident, references, seen_idents)
            {
                return true;
            }
        }
    }

    false
}

pub struct ReferenceCollector {
    pub references: HashMap<String, HashSet<String>>,
    cur_structure_name: String,
    include_impls: bool,
    full_names: bool,
    errors: Vec<LangError>,
}

impl ReferenceCollector {
    pub fn new(include_impls: bool, full_names: bool) -> Self {
        Self {
            references: HashMap::default(),
            cur_structure_name: String::with_capacity(0),
            include_impls,
            full_names,
            errors: Vec::default(),
        }
    }

    fn collect_member_references(&mut self, structure_name: &str, members: &[Rc<RefCell<Var>>]) {
        let mut local_references = HashSet::default();

        for member in members {
            if let Some(ty) = &member.borrow().ty {
                if let Some(names) = ty.get_structure_names(self.full_names) {
                    for name in names {
                        if name != structure_name {
                            local_references.insert(name);
                        }
                    }
                }
            }
        }

        self.references
            .insert(structure_name.into(), local_references);
    }
}

impl Visitor for ReferenceCollector {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(struct_), ..) = ast_token {
            let structure_name = struct_.borrow().name.clone();
            if let Some(members) = &struct_.borrow().members {
                self.collect_member_references(&structure_name, members);
            } else {
                self.references
                    .insert(structure_name, HashSet::with_capacity(0));
            }
        }
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Enum(enum_), ..) = ast_token {
            let structure_name = enum_.borrow().name.clone();
            if let Some(members) = &enum_.borrow().members {
                self.collect_member_references(&structure_name, members);
            } else {
                self.references
                    .insert(structure_name, HashSet::with_capacity(0));
            }
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(ident, ..), ..) = ast_token {
            self.cur_structure_name = ident.clone();
        }
    }

    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        if self.include_impls {
            if let Some(mut references) = ty.get_structure_names(self.full_names) {
                // Do not add references to itself.
                references.remove(&self.cur_structure_name);

                match self.references.entry(self.cur_structure_name.clone()) {
                    Entry::Occupied(mut o) => {
                        o.get_mut().extend(references);
                    }
                    Entry::Vacant(v) => {
                        v.insert(references);
                    }
                }
            }
        }
    }
}
