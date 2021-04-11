use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap, HashSet},
    rc::Rc,
};

use log::debug;

use common::{
    ctx::{
        analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx, traverse_ctx::TraverseCtx, ty_ctx::TyCtx,
        ty_env::TyEnv,
    },
    error::{CyclicDependencyError, LangError, LangErrorKind, LangResult},
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Adt, BlockHeader},
        expr::Var,
    },
    traverse::{traverser::AstTraverser, visitor::Visitor},
    BlockId, TypeId,
};

// TODO: Merge function taking TraverCtx or AnalyzeCtx.

/// Can be used to figure out the order in which different ADTs depends on each
/// other. This is needed to make sure that the "inner" ADTs are compiled/handled
/// first before the "outer" ones since they depend on them.
///
/// The given `include_impls` specified if a reference in a impl block of a ADT
/// should count as a reference or not. If this is set to `true`, references to
/// other ADTs in impl blocks are considered reference. If it is set to `false`,
/// only the members of the ADT are considered references, everything inside the
/// impl blocks are ignored.
pub fn dependency_order(
    analyze_ctx: &mut AnalyzeCtx,
    ast_token: &mut AstToken,
    include_impls: bool,
    full_paths: bool,
) -> Result<Vec<LangPath>, Vec<LangError>> {
    // Step 1: Find all ADTs and references between them.
    // Step 2: Figure out the correct order to compile them in.

    // The key is the name of a ADT and the value set contains names of ADTs that
    // is referenced from the given "key" ADT. This does NOT include recursive
    // dependencies, only direct "top level" references.
    let references = order_step1(analyze_ctx, ast_token, include_impls, full_paths)?;
    match order_step2(&analyze_ctx.ty_ctx, &references) {
        Ok(order) => {
            debug!("ADTs -- references: {:#?}, order: {:#?}", references, order);
            Ok(order)
        }
        Err(cyc_err) => Err(vec![LangError::new(
            format!(
                "Cyclic dependency between ADTs \"{}\" and \"{}\". All refs: {:#?}",
                cyc_err.0, cyc_err.1, references
            ),
            LangErrorKind::GeneralError,
            None,
        )]),
    }
}

pub fn dependency_order_from_ctx(
    traverse_ctx: &mut TraverseCtx,
    ast_token: &mut AstToken,
    include_impls: bool,
    full_paths: bool,
) -> Result<Vec<LangPath>, Vec<LangError>> {
    // Step 1: Find all ADTs and references between them.
    // Step 2: Figure out the correct order to compile them in.

    // The key is the name of a ADT and the value set contains names of ADTs that
    // is referenced from the given "key" ADT. This does NOT include recursive
    // dependencies, only direct "top level" references.
    let references = order_step1_from_ctx(traverse_ctx, ast_token, include_impls, full_paths)?;
    match order_step2(&traverse_ctx.ty_ctx, &references) {
        Ok(order) => {
            debug!("ADTs -- references: {:#?}, order: {:#?}", references, order);
            Ok(order)
        }
        Err(cyc_err) => Err(vec![LangError::new(
            format!(
                "Cyclic dependency between ADTs \"{}\" and \"{}\". All refs: {:#?}",
                cyc_err.0, cyc_err.1, references
            ),
            LangErrorKind::GeneralError,
            None,
        )]),
    }
}

/// Iterate through all types in the ADT and see if it uses/references other
/// ADTs. In those cases, the referenced ADTs would need to be handled/compiled
/// before the referencing ADT.
fn order_step1(
    analyze_ctx: &mut AnalyzeCtx,
    ast_token: &mut AstToken,
    include_impls: bool,
    full_paths: bool,
) -> Result<HashMap<LangPath, HashSet<LangPath>>, Vec<LangError>> {
    let mut ref_collector = ReferenceCollector::new(include_impls, full_paths);
    AstTraverser::new(&mut analyze_ctx.ast_ctx, &mut analyze_ctx.ty_ctx)
        .traverse_with_visitor(&mut ref_collector, ast_token)?;

    Ok(ref_collector.references)
}

fn order_step1_from_ctx(
    traverse_ctx: &mut TraverseCtx,
    ast_token: &mut AstToken,
    include_impls: bool,
    full_paths: bool,
) -> Result<HashMap<LangPath, HashSet<LangPath>>, Vec<LangError>> {
    let mut ref_collector = ReferenceCollector::new(include_impls, full_paths);
    AstTraverser::from_ctx(traverse_ctx).traverse_with_visitor(&mut ref_collector, ast_token)?;

    Ok(ref_collector.references)
}

/// Given the references between the "items" in `references`, figures out the order
/// in which the items depends/references each other. Returns a error if a cyclic
/// dependency is found.
pub fn order_step2(
    ty_ctx: &TyCtx,
    references: &HashMap<LangPath, HashSet<LangPath>>,
) -> Result<Vec<LangPath>, CyclicDependencyError> {
    let mut order: Vec<LangPath> = Vec::with_capacity(references.len());

    for cur_ident in references.keys() {
        let mut idx = 0;

        for prev_ident in &order {
            let prev_references_cur = contains(cur_ident, prev_ident, &references);
            let cur_references_prev = contains(prev_ident, cur_ident, &references);

            if prev_references_cur && cur_references_prev {
                return Err(CyclicDependencyError(
                    ty_ctx.ty_env.to_string_path(ty_ctx, cur_ident),
                    ty_ctx.ty_env.to_string_path(ty_ctx, prev_ident),
                ));
            } else if prev_references_cur {
                // Can't insert the "current" ident before the "previous" since
                // it is being referenced from it. Insert into `order` at this
                // index so that "current" gets compiled before "previous".
                break;
            } else {
                // else if cur_references_prev = >
                //   "Current" references "previous", so "previous" needs to
                //   be compiled first. Keep iterating and find a spot after
                //   "previous" to insert it.
                // else
                //   The two idents has no references between each other.
                //   Keep iterating to find a spot to insert it as "late"
                //   as possible.
            }

            idx += 1;
        }

        if idx > order.len() {
            order.push(cur_ident.clone());
        } else {
            order.insert(idx, cur_ident.clone());
        }
    }

    debug!("order: {:#?}", order);

    Ok(order)
}

// TODO: Merge with `order_step2()`.
pub fn order_step2_strings(
    references: &HashMap<String, HashSet<String>>,
) -> Result<Vec<String>, CyclicDependencyError> {
    let mut order: Vec<String> = Vec::with_capacity(references.len());

    for cur_ident in references.keys() {
        let mut idx = 0;

        for prev_ident in &order {
            let prev_references_cur = contains_strings(cur_ident, prev_ident, &references);
            let cur_references_prev = contains_strings(prev_ident, cur_ident, &references);

            if prev_references_cur && cur_references_prev {
                return Err(CyclicDependencyError(
                    cur_ident.to_string(),
                    prev_ident.to_string(),
                ));
            } else if prev_references_cur {
                // Can't insert the "current" ident before the "previous" since
                // it is being referenced from it. Insert into `order` at this
                // index so that "current" gets compiled before "previous".
                break;
            } else {
                // else if cur_references_prev = >
                //   "Current" references "previous", so "previous" needs to
                //   be compiled first. Keep iterating and find a spot after
                //   "previous" to insert it.
                // else
                //   The two idents has no references between each other.
                //   Keep iterating to find a spot to insert it as "late"
                //   as possible.
            }

            idx += 1;
        }

        if idx > order.len() {
            order.push(cur_ident.clone());
        } else {
            order.insert(idx, cur_ident.clone());
        }
    }

    debug!("order: {:#?}", order);

    Ok(order)
}

// TODO: This is not effective. Will this be a problem for programs with
//       a lot of structures?
/// Checks if the item with name/path `cur_ident` are referenced from the ident
/// with name/path `ref_ident` recursively. This will find cyclic dependencies.
fn contains(
    cur_ident: &LangPath,
    ref_ident: &LangPath,
    references: &HashMap<LangPath, HashSet<LangPath>>,
) -> bool {
    // HashSet used to detect cyclic dependencies.
    let mut seen_idents = HashSet::default();
    contains_rec(cur_ident, ref_ident, references, &mut seen_idents)
}

fn contains_rec(
    cur_ident: &LangPath,
    ref_ident: &LangPath,
    references: &HashMap<LangPath, HashSet<LangPath>>,
    seen_idents: &mut HashSet<LangPath>,
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
            seen_idents.insert(nested_ident.clone());

            if is_cyclic_dependency
                || contains_rec(cur_ident, nested_ident, references, seen_idents)
            {
                return true;
            }
        }
    }

    false
}

// TODO: Merge with non string contains functions.
fn contains_strings(
    cur_ident: &str,
    ref_ident: &str,
    references: &HashMap<String, HashSet<String>>,
) -> bool {
    // HashSet used to detect cyclic dependencies.
    let mut seen_idents = HashSet::default();
    contains_strings_rec(cur_ident, ref_ident, references, &mut seen_idents)
}

fn contains_strings_rec(
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
            seen_idents.insert(nested_ident.clone());

            if is_cyclic_dependency
                || contains_strings_rec(cur_ident, nested_ident, references, seen_idents)
            {
                return true;
            }
        }
    }

    false
}

pub struct ReferenceCollector {
    pub references: HashMap<LangPath, HashSet<LangPath>>,
    cur_adt_name: LangPath,
    include_impls: bool,
    full_paths: bool,
    errors: Vec<LangError>,
}

impl ReferenceCollector {
    pub fn new(include_impls: bool, full_paths: bool) -> Self {
        Self {
            references: HashMap::default(),
            cur_adt_name: LangPath::new(Vec::with_capacity(0), None),
            include_impls,
            full_paths,
            errors: Vec::default(),
        }
    }

    fn collect_member_references(
        &mut self,
        ty_env: &TyEnv,
        adt_path: &LangPath,
        members: &[Rc<RefCell<Var>>],
    ) -> LangResult<()> {
        let mut local_references = HashSet::default();

        for member in members {
            if let Some(type_id) = &member.borrow().ty {
                let adt_and_trait_paths =
                    ty_env.get_adt_and_trait_paths(*type_id, self.full_paths)?;

                for path in adt_and_trait_paths {
                    if &path != adt_path {
                        local_references.insert(path);
                    }
                }
            }
        }

        self.references.insert(adt_path.clone(), local_references);
        Ok(())
    }

    fn adt_path(
        &self,
        ast_ctx: &AstCtx,
        adt: &Rc<RefCell<Adt>>,
        id: BlockId,
    ) -> LangResult<LangPath> {
        let adt_name = adt.borrow().name.clone();

        let module = if let Some(module) = ast_ctx.get_module(id)? {
            module
        } else {
            LangPath::default()
        };

        Ok(if self.full_paths {
            let generics = if let Some(generics) = &adt.borrow().generics {
                Some(generics.clone())
            } else {
                None
            };
            module.clone_push(&adt_name, generics.as_ref())
        } else {
            module.clone_push(&adt_name, None)
        })
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

    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Struct(adt), ..) = ast_token {
            let adt_path = match self.adt_path(ctx.ast_ctx, adt, ctx.block_id) {
                Ok(adt_path) => adt_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            if let Err(err) =
                self.collect_member_references(&ctx.ty_ctx.ty_env, &adt_path, &adt.borrow().members)
            {
                self.errors.push(err);
            }
        }
    }

    fn visit_enum(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Enum(adt), ..) = ast_token {
            let adt_path = match self.adt_path(ctx.ast_ctx, adt, ctx.block_id) {
                Ok(adt_path) => adt_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            if let Err(err) =
                self.collect_member_references(&ctx.ty_ctx.ty_env, &adt_path, &adt.borrow().members)
            {
                self.errors.push(err);
            }
        }
    }

    fn visit_union(&mut self, ast_token: &mut AstToken, ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Union(adt), ..) = ast_token {
            let adt_path = match self.adt_path(ctx.ast_ctx, adt, ctx.block_id) {
                Ok(adt_path) => adt_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            if let Err(err) =
                self.collect_member_references(&ctx.ty_ctx.ty_env, &adt_path, &adt.borrow().members)
            {
                self.errors.push(err);
            }
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseCtx) {
        if let AstToken::Block(BlockHeader::Implement(ident, ..), ..) = ast_token {
            self.cur_adt_name = ident.clone();
        }
    }

    // TODO: Does this logic make sense? This is called every time a type is
    //       visited. The first if check is impls should be included, but how
    //       is that enforces? If `self.include_impls` is true, from what I can
    //       tell types outside of impls might be used??
    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if self.include_impls {
            let mut references = match ctx
                .ty_ctx
                .ty_env
                .get_adt_and_trait_paths(*type_id, self.full_paths)
            {
                Ok(paths) => paths,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Do not add references to itself.
            references.remove(&self.cur_adt_name);

            match self.references.entry(self.cur_adt_name.clone()) {
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
