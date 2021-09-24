use std::hash::Hash;

use either::Either;

use common::{
    eq::generics_eq,
    error::{LangError, LangResult},
    hash::{DerefType, TyEnvHash},
    hash_map::TyEnvHashMap,
    path::LangPath,
    token::{
        block::{Block, BlockHeader},
        expr::FnCall,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        contains::contains_generic_shallow, generics::Generics, get::get_ident, ty::Ty,
        ty_env::TyEnv, type_id::TypeId,
    },
};

/// Collects "nested generic" ADTs, functions and methods. These are uses of
/// the "declared" generic type.
///
/// ```no_run
///
/// ```
///
/// # Example
///
/// If there are two structs `A` and `B`, both containing generic parameters.
/// If `A` contains a function that creates a `B`, where the generic type
/// for `B` is the generic type for `A`, that use will be considered a nested
/// generic struct.
///
/// ```no_run
/// struct B<T2>;
/// struct A<T1> {
///     fn test_func() {
///         var b = B<T1> { }
///     }
/// }
/// ```
///
/// These cases are special since the generic `T1` in this case isn't solved and
/// replaced during the type inference stage, it is solved/replace at a later
/// stage that handles generics types. This means that the use of `B<T1>` is not
/// solved and thus structs of `B` will not be created for every `T1`.
///
/// The first LangPath key is the path of the containing ADT (`A` in the example)
/// and the second String key is the name of the method in which the vector
/// of types was found in.
pub struct GenericNestedCollector {
    // TODO: Make inner `Vec` a `HashSet` for quicker look-ups.
    /// Contains all nested ADTs. The key of the map contains information about
    /// where the nested ADT was found and its path.
    pub nested_generic_adts: TyEnvHashMap<NestedAdtInfo, Vec<TypeId>>,

    // TODO: Make inner `Vec` a `HashSet` for quicker look-ups.
    /// Contains all nested functions/methods. The key of the map contains
    /// information about where the nested fn/method was found and its path.
    pub nested_generic_fns: TyEnvHashMap<NestedFnInfo, Vec<Generics>>,

    /// Contains the name of the current function that is being visited.
    cur_fn_name: Option<String>,

    /// Will be set to the ADT of the current function (with name `cur_fn_name`).
    /// If the function isn't located in a ADT, this will be set to None.
    /// This path will NOT contain any generics.
    cur_adt_path: Option<LangPath>,

    errors: Vec<LangError>,
}

impl GenericNestedCollector {
    pub fn new() -> Self {
        Self {
            nested_generic_adts: TyEnvHashMap::default(),
            nested_generic_fns: TyEnvHashMap::default(),
            cur_fn_name: None,
            cur_adt_path: None,
            errors: Vec::default(),
        }
    }

    fn collect_nested_generic_adts(
        &mut self,
        ctx: &mut TraverseCtx,
        type_id: TypeId,
    ) -> LangResult<()> {
        if !contains_generic_shallow(&ctx.ty_env.lock(), type_id)? {
            return Ok(());
        }

        let ty = ctx.ty_env.lock().ty_clone(type_id)?;
        match ty {
            Ty::CompoundType(inner_ty, ..) => {
                let ty_env_guard = ctx.ty_env.lock();

                if inner_ty.is_adt() || inner_ty.is_trait() || inner_ty.is_unknown_ident() {
                    let fn_decl_adt_or_module = if let Some(adt_path) = &self.cur_adt_path {
                        Either::Left(adt_path.clone())
                    } else {
                        Either::Right(
                            ctx.ast_ctx
                                .get_module(ctx.block_id)?
                                .unwrap_or_else(LangPath::empty),
                        )
                    };

                    let adt_path = get_ident(&ty_env_guard, type_id)?
                        .map(|path| path.without_gens())
                        .unwrap();

                    let nested_adt_info = NestedAdtInfo {
                        fn_decl_name: self.cur_fn_name.clone(),
                        fn_decl_adt_or_module,
                        adt_path,
                    };

                    let contains_key = self.nested_generic_adts.contains_key(
                        &ty_env_guard,
                        DerefType::Deep,
                        &nested_adt_info,
                    )?;

                    if contains_key {
                        let vec_inner = self
                            .nested_generic_adts
                            .get_mut(&ty_env_guard, DerefType::Deep, &nested_adt_info)?
                            .unwrap();

                        if !vec_inner.contains(&type_id) {
                            vec_inner.push(type_id);
                        }
                    } else {
                        self.nested_generic_adts.insert(
                            &ty_env_guard,
                            DerefType::Deep,
                            nested_adt_info,
                            vec![type_id],
                        )?;
                    }
                }
            }

            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                self.collect_nested_generic_adts(ctx, type_id_i)?;
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    self.collect_nested_generic_adts(ctx, type_id_i)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    fn collect_nested_generic_fns(
        &mut self,
        ctx: &mut TraverseCtx,
        fn_call: &FnCall,
    ) -> LangResult<()> {
        let fn_call_gens = match &fn_call.generics {
            Some(gens) if !gens.is_empty() => gens,
            _ => return Ok(()),
        };

        let ty_env_guard = ctx.ty_env.lock();

        let mut contains_nested_gens = false;
        for gen_type_id in fn_call_gens.iter_types() {
            if contains_generic_shallow(&ty_env_guard, *gen_type_id)? {
                contains_nested_gens = true;
            }
        }

        if contains_nested_gens {
            let fn_decl_adt_or_module = if let Some(adt_path) = &self.cur_adt_path {
                Either::Left(adt_path.clone())
            } else {
                Either::Right(ctx.ast_ctx.get_module(ctx.block_id)?.unwrap())
            };

            let fn_call_adt_or_module = if let Some(adt_type_id) = fn_call.method_adt {
                Either::Left(
                    get_ident(&ty_env_guard, adt_type_id)?
                        .map(|path| path.without_gens())
                        .unwrap(),
                )
            } else {
                Either::Right(fn_call.module.clone())
            };

            let nested_fn_info = NestedFnInfo {
                fn_decl_name: self.cur_fn_name.clone().unwrap(),
                fn_decl_adt_or_module,
                fn_call_name: fn_call.name.clone(),
                fn_call_adt_or_module,
            };

            let contains_key = self.nested_generic_fns.contains_key(
                &ty_env_guard,
                DerefType::Deep,
                &nested_fn_info,
            )?;

            if contains_key {
                let vec_inner = self
                    .nested_generic_fns
                    .get_mut(&ty_env_guard, DerefType::Deep, &nested_fn_info)?
                    .unwrap();

                let mut contains_gens = false;
                for gen_inner in vec_inner.iter() {
                    if generics_eq(&ty_env_guard, gen_inner, fn_call_gens, DerefType::Deep)? {
                        contains_gens = true;
                    }
                }

                if !contains_gens {
                    vec_inner.push(fn_call_gens.clone());
                }
            } else {
                self.nested_generic_fns.insert(
                    &ty_env_guard,
                    DerefType::Deep,
                    nested_fn_info,
                    vec![fn_call_gens.clone()],
                )?;
            }
        }

        Ok(())
    }
}

impl Visitor for GenericNestedCollector {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        // Need to do this in `visit_block` instead of `visit_fn` since the types
        // in the function declaration/prototype is traversed before the
        // `visit_fn` is.
        let Block { header, .. } = block;

        match header {
            BlockHeader::Fn(func) => {
                self.cur_fn_name = Some(func.read().name.clone());
                self.cur_adt_path = if let Some(adt_type_id) = func.read().method_adt {
                    match get_ident(&ctx.ty_env.lock(), adt_type_id) {
                        Ok(adt_path) => adt_path.map(|path| path.without_gens()),
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    None
                };
            }

            BlockHeader::Struct(adt) | BlockHeader::Union(adt) | BlockHeader::Enum(adt) => {
                self.cur_fn_name = None;
                let adt = adt.read();
                self.cur_adt_path =
                    Some(adt.module.clone_push(&adt.name, None, Some(adt.file_pos)));
            }

            BlockHeader::Trait(trait_) => {
                self.cur_fn_name = None;
                let trait_ = trait_.read();
                self.cur_adt_path = Some(trait_.module.clone_push(
                    &trait_.name,
                    None,
                    Some(trait_.file_pos),
                ));
            }

            _ => (),
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if fn_call.is_fn_ptr_call {
            return;
        }

        if let Err(err) = self.collect_nested_generic_fns(ctx, fn_call) {
            self.errors.push(err);
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.collect_nested_generic_adts(ctx, *type_id) {
            self.errors.push(err);
        }
    }
}

/// Used to store information about a nested method/functions containing generics.
#[derive(Debug, Clone)]
pub struct NestedFnInfo {
    /// The name of the function where this specific nested generic was found in.
    pub fn_decl_name: String,

    /// If this is the left variant: Path of the ADT containing the
    /// `fn_decl_name` function.
    /// If this is the right variant: the `fn_decl_name` is a free-standing
    /// function, this value is then the module of the function.
    pub fn_decl_adt_or_module: Either<LangPath, LangPath>,

    /// The name of the function that contains the nested generics. I.e. this is
    /// a function call containing generics that is called inside the `func_name`
    /// function.
    pub fn_call_name: String,

    /// Path of the ADT that `fn_call_name` was called on. If this is a free-
    /// standing function, this will be None.
    /// If this is the left variant: Path of the ADT tha `fn_call_name` was
    /// called on.
    /// If this is the right variant: the `fn_call_name` is a free-standing
    /// function, this value is then the module of the function.
    pub fn_call_adt_or_module: Either<LangPath, LangPath>,
}

/// Used to store information about a nested ADT containing generics.
#[derive(Debug, Clone)]
pub struct NestedAdtInfo {
    /// The name of the function where this specific nested ADT was found in.
    pub fn_decl_name: Option<String>,

    /// If this is the left variant: Path of the ADT containing the
    /// `fn_decl_name` function.
    /// If this is the right variant: the `fn_decl_name` is a free-standing
    /// function, this value is then the module of the function.
    pub fn_decl_adt_or_module: Either<LangPath, LangPath>,

    /// The path of the nested ADT (excluding generics).
    pub adt_path: LangPath,
}

impl TyEnvHash for NestedFnInfo {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        nested_fn_info_hash(self, ty_env, deref_type, state)
    }
}

impl TyEnvHash for &NestedFnInfo {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        nested_fn_info_hash(self, ty_env, deref_type, state)
    }
}

fn nested_fn_info_hash<H: std::hash::Hasher>(
    fn_info: &NestedFnInfo,
    ty_env: &TyEnv,
    deref_type: DerefType,
    state: &mut H,
) -> LangResult<()> {
    fn_info.fn_decl_name.hash(state);
    match &fn_info.fn_decl_adt_or_module {
        Either::Left(adt_path) => {
            0.hash(state);
            adt_path.hash_with_state(ty_env, deref_type, state)?;
        }
        Either::Right(module_path) => {
            1.hash(state);
            module_path.hash_with_state(ty_env, deref_type, state)?;
        }
    }
    fn_info.fn_call_name.hash(state);
    match &fn_info.fn_call_adt_or_module {
        Either::Left(adt_path) => {
            2.hash(state);
            adt_path.hash_with_state(ty_env, deref_type, state)?;
        }
        Either::Right(module_path) => {
            3.hash(state);
            module_path.hash_with_state(ty_env, deref_type, state)?;
        }
    }
    Ok(())
}

impl TyEnvHash for NestedAdtInfo {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        self.fn_decl_name.hash(state);
        match &self.fn_decl_adt_or_module {
            Either::Left(adt_path) => {
                0.hash(state);
                adt_path.hash_with_state(ty_env, deref_type, state)?;
            }
            Either::Right(module_path) => {
                1.hash(state);
                module_path.hash_with_state(ty_env, deref_type, state)?;
            }
        }
        self.adt_path.hash_with_state(ty_env, deref_type, state)
    }
}
