use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    sync::{Arc, RwLock},
};

use either::Either;
use log::debug;

use common::{
    ctx::{analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx},
    eq::path_eq,
    error::{CyclicDependencyError, LangError, LangErrorKind, LangResult},
    hash::DerefType,
    hash_map::TyEnvHashMap,
    hash_set::TyEnvHashSet,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Adt, Block, BlockHeader},
        expr::Var,
    },
    traverse::{traverse_ctx::TraverseCtx, traverser::traverse, visitor::Visitor},
    ty::{get::get_adt_and_trait_paths, to_string::to_string_path, ty_env::TyEnv, type_id::TypeId},
    BlockId,
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
    let mut traverse_ctx = TraverseCtx::new(&mut analyze_ctx.ast_ctx, analyze_ctx.ty_env);
    dependency_order_from_ctx(&mut traverse_ctx, ast_token, include_impls, full_paths)
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
    let references = order_step1(traverse_ctx, ast_token, include_impls, full_paths)?;
    match order_step2(&traverse_ctx.ty_env.lock().unwrap(), &references) {
        Ok(order) => {
            debug!("ADTs -- references: {:#?}, order: {:#?}", references, order);
            Ok(order)
        }
        Err(Either::Left(err)) => Err(vec![err]),
        Err(Either::Right(cyc_err)) => Err(vec![LangError::new(
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
    traverse_ctx: &mut TraverseCtx,
    ast_token: &mut AstToken,
    include_impls: bool,
    full_paths: bool,
) -> Result<TyEnvHashMap<LangPath, TyEnvHashSet<LangPath>>, Vec<LangError>> {
    let mut ref_collector = ReferenceCollector::new(include_impls, full_paths);
    traverse(traverse_ctx, &mut ref_collector, ast_token)?;
    Ok(ref_collector.references)
}

/// Given the references between the "items" in `references`, figures out the order
/// in which the items depends/references each other. Returns a error if a cyclic
/// dependency is found.
pub fn order_step2(
    ty_env: &TyEnv,
    references: &TyEnvHashMap<LangPath, TyEnvHashSet<LangPath>>,
) -> Result<Vec<LangPath>, Either<LangError, CyclicDependencyError>> {
    let mut order: Vec<LangPath> = Vec::with_capacity(references.len());

    for cur_ident in references.keys() {
        let mut idx = 0;

        for prev_ident in &order {
            let prev_references_cur =
                contains(ty_env, cur_ident, prev_ident, &references).map_err(Either::Left)?;
            let cur_references_prev =
                contains(ty_env, prev_ident, cur_ident, &references).map_err(Either::Left)?;

            if prev_references_cur && cur_references_prev {
                return Err(Either::Right(CyclicDependencyError(
                    to_string_path(ty_env, cur_ident),
                    to_string_path(ty_env, prev_ident),
                )));
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
    ty_env: &TyEnv,
    cur_ident: &LangPath,
    ref_ident: &LangPath,
    references: &TyEnvHashMap<LangPath, TyEnvHashSet<LangPath>>,
) -> LangResult<bool> {
    // HashSet used to detect cyclic dependencies.
    let mut seen_idents = TyEnvHashSet::default();
    contains_rec(ty_env, cur_ident, ref_ident, references, &mut seen_idents)
}

fn contains_rec(
    ty_env: &TyEnv,
    cur_ident: &LangPath,
    ref_ident: &LangPath,
    references: &TyEnvHashMap<LangPath, TyEnvHashSet<LangPath>>,
    seen_idents: &mut TyEnvHashSet<LangPath>,
) -> LangResult<bool> {
    let ref_references =
        if let Some(ref_references) = references.get(ty_env, DerefType::Deep, ref_ident)? {
            ref_references
        } else {
            return Ok(false);
        };

    if ref_references.contains(ty_env, DerefType::Deep, cur_ident)? {
        return Ok(true);
    } else {
        for nested_ident in ref_references.values() {
            seen_idents.insert(ty_env, DerefType::Deep, nested_ident.clone())?;
            if contains_rec(ty_env, cur_ident, nested_ident, references, seen_idents)? {
                return Ok(true);
            }
        }
    }

    Ok(false)
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
            seen_idents.insert(nested_ident.clone());
            if contains_strings_rec(cur_ident, nested_ident, references, seen_idents) {
                return true;
            }
        }
    }

    false
}

pub struct ReferenceCollector {
    pub references: TyEnvHashMap<LangPath, TyEnvHashSet<LangPath>>,
    cur_adt_path: Option<LangPath>,
    include_impls: bool,
    full_paths: bool,
    errors: Vec<LangError>,
}

impl ReferenceCollector {
    pub fn new(include_impls: bool, full_paths: bool) -> Self {
        Self {
            references: TyEnvHashMap::default(),
            cur_adt_path: None,
            include_impls,
            full_paths,
            errors: Vec::default(),
        }
    }

    fn collect_member_references(
        &mut self,
        ty_env: &TyEnv,
        adt_path: &LangPath,
        members: &[Arc<RwLock<Var>>],
    ) -> LangResult<()> {
        let mut local_references = TyEnvHashSet::default();

        for member in members {
            if let Some(type_id) = &member.as_ref().borrow().read().unwrap().ty {
                let adt_and_trait_paths =
                    get_adt_and_trait_paths(&ty_env, *type_id, self.full_paths)?;

                for path in adt_and_trait_paths.values() {
                    if !path_eq(ty_env, path, adt_path, DerefType::Deep)? {
                        local_references.insert(ty_env, DerefType::Deep, path.clone())?;
                    }
                }
            }
        }

        self.references
            .insert(ty_env, DerefType::Deep, adt_path.clone(), local_references)?;
        Ok(())
    }

    fn adt_path(
        &self,
        ast_ctx: &AstCtx,
        adt: &Arc<RwLock<Adt>>,
        id: BlockId,
    ) -> LangResult<LangPath> {
        let adt_name = adt.as_ref().borrow().read().unwrap().name.clone();
        let adt_file_pos = Some(adt.as_ref().borrow().read().unwrap().file_pos);

        let module = if let Some(module) = ast_ctx.get_module(id)? {
            module
        } else {
            LangPath::default()
        };

        Ok(if self.full_paths {
            let generics = adt.read().unwrap().generics.as_ref().cloned();
            module.clone_push(&adt_name, generics.as_ref(), adt_file_pos)
        } else {
            module.clone_push(&adt_name, None, adt_file_pos)
        })
    }
}

impl Visitor for ReferenceCollector {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_struct(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Struct(adt),
            ..
        } = block
        {
            let adt_path = match self.adt_path(ctx.ast_ctx, adt, ctx.block_id) {
                Ok(adt_path) => adt_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            if let Err(err) = self.collect_member_references(
                &ctx.ty_env.lock().unwrap(),
                &adt_path,
                &adt.as_ref().borrow().read().unwrap().members,
            ) {
                self.errors.push(err);
            }
        }
    }

    fn visit_enum(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Enum(adt),
            ..
        } = block
        {
            let adt_path = match self.adt_path(ctx.ast_ctx, adt, ctx.block_id) {
                Ok(adt_path) => adt_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            if let Err(err) = self.collect_member_references(
                &ctx.ty_env.lock().unwrap(),
                &adt_path,
                &adt.as_ref().borrow().read().unwrap().members,
            ) {
                self.errors.push(err);
            }
        }
    }

    fn visit_union(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Union(adt),
            ..
        } = block
        {
            let adt_path = match self.adt_path(ctx.ast_ctx, adt, ctx.block_id) {
                Ok(adt_path) => adt_path,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };
            if let Err(err) = self.collect_member_references(
                &ctx.ty_env.lock().unwrap(),
                &adt_path,
                &adt.as_ref().borrow().read().unwrap().members,
            ) {
                self.errors.push(err);
            }
        }
    }

    fn visit_impl(&mut self, block: &mut Block, _ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Implement(impl_path, ..),
            ..
        } = block
        {
            self.cur_adt_path = Some(impl_path.clone());
        }
    }

    fn visit_block(&mut self, block: &mut Block, _ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = block
        {
            // Need to do this in `visit_block` instead of `visit_fn` since the types
            // in the function declaration/prototype is traversed before the
            // `visit_fn` is.
            //
            // This is a function, not a method. It is not located inside a impl
            // block, so reset the ADT name since it doesn't belong to the ADT.
            if func.as_ref().borrow().read().unwrap().method_adt.is_none() {
                self.cur_adt_path = None;
            }
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if self.include_impls {
            if let Some(cur_adt_path) = &self.cur_adt_path {
                let ty_env_guard = &ctx.ty_env.lock().unwrap();

                let mut references =
                    match get_adt_and_trait_paths(&ty_env_guard, *type_id, self.full_paths) {
                        Ok(paths) => paths,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                // Do not add references to itself.
                if let Err(err) = references.remove(&ty_env_guard, DerefType::Deep, cur_adt_path) {
                    self.errors.push(err);
                    return;
                }

                let contains_key =
                    match self
                        .references
                        .contains_key(&ty_env_guard, DerefType::Deep, cur_adt_path)
                    {
                        Ok(contains_key) => contains_key,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                if contains_key {
                    let self_references = self
                        .references
                        .get_mut(&ty_env_guard, DerefType::Deep, cur_adt_path)
                        .unwrap()
                        .unwrap();
                    if let Err(err) =
                        self_references.extend(&ty_env_guard, DerefType::Deep, &references)
                    {
                        self.errors.push(err);
                    }
                } else if let Err(err) = self.references.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    cur_adt_path.clone(),
                    references,
                ) {
                    self.errors.push(err);
                }
            }
        }
    }
}
