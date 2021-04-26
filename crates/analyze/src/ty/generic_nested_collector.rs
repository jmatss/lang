use std::collections::{hash_map::Entry, HashMap};

use common::{
    ctx::{traverse_ctx::TraverseCtx, ty_env::TyEnv},
    error::{LangError, LangResult},
    path::LangPath,
    token::{ast::AstToken, block::BlockHeader, expr::FnCall},
    traverse::visitor::Visitor,
    ty::{generics::Generics, ty::Ty},
    TypeId,
};

/// Used to store information about a nested method containing generics..
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(super) struct NestedMethodInfo {
    /// The name of the function where this specific nested generic was found in.
    pub func_name: String,

    /// The name of the method that contains the nested generics.
    pub method_call_name: String,

    /// Name of the ADT containing the `method_call_name`.
    pub method_adt_path: LangPath,
}

pub(super) struct GenericNestedCollector {
    /// The key String is the name of the function that this type/generic was
    /// found in.
    pub nested_generic_adts: HashMap<String, Vec<TypeId>>,

    /// The key contains information about where the types/generics was found.
    pub nested_generic_methods: HashMap<NestedMethodInfo, Vec<Generics>>,

    /// Contains the name of the current function that is being visited.
    cur_func_name: String,

    errors: Vec<LangError>,
}

impl GenericNestedCollector {
    pub(super) fn new() -> Self {
        Self {
            nested_generic_adts: HashMap::default(),
            nested_generic_methods: HashMap::default(),
            cur_func_name: "".into(),
            errors: Vec::default(),
        }
    }

    fn collect_nested_generic_adts(
        &mut self,
        ty_env: &mut TyEnv,
        type_id: TypeId,
    ) -> LangResult<()> {
        if !ty_env.contains_generic_shallow(type_id)? {
            return Ok(());
        }

        match ty_env.ty(type_id)?.clone() {
            Ty::CompoundType(inner_ty, generics, _) => {
                let mut contains_generic = false;
                for gen_type_id in generics.iter_types() {
                    if ty_env.contains_generic_shallow(*gen_type_id)? {
                        contains_generic = true;
                    }
                }

                if contains_generic && inner_ty.is_adt() {
                    match self.nested_generic_adts.entry(self.cur_func_name.clone()) {
                        Entry::Occupied(mut o) => {
                            if !o.get().contains(&type_id) {
                                o.get_mut().push(type_id);
                            }
                        }
                        Entry::Vacant(v) => {
                            v.insert(vec![type_id]);
                        }
                    }
                }
            }
            Ty::Pointer(type_id_i, ..) | Ty::Array(type_id_i, ..) => {
                self.collect_nested_generic_adts(ty_env, type_id_i)?;
            }
            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    self.collect_nested_generic_adts(ty_env, type_id_i)?;
                }
            }
            _ => (),
        }

        Ok(())
    }

    fn collect_nested_generic_methods(
        &mut self,
        ty_env: &mut TyEnv,
        method_call: &FnCall,
    ) -> LangResult<()> {
        if method_call.method_adt.is_none() {
            return Ok(());
        }

        let method_call_gens = match &method_call.generics {
            Some(gens) if !gens.is_empty() => gens,
            _ => return Ok(()),
        };

        let mut contains_nested_generic = false;
        for gen_type_id in method_call_gens.iter_types() {
            if ty_env.contains_generic_shallow(*gen_type_id)? {
                contains_nested_generic = true;
            }
        }

        if contains_nested_generic {
            let method_adt_path = ty_env
                .get_ident(*method_call.method_adt.as_ref().unwrap())?
                .unwrap();
            let key = NestedMethodInfo {
                func_name: self.cur_func_name.clone(),
                method_call_name: method_call.name.clone(),
                method_adt_path,
            };

            match self.nested_generic_methods.entry(key) {
                Entry::Occupied(mut o) => {
                    if !o.get().contains(method_call_gens) {
                        o.get_mut().push(method_call_gens.clone());
                    }
                }
                Entry::Vacant(v) => {
                    v.insert(vec![method_call_gens.clone()]);
                }
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

    fn visit_block(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseCtx) {
        // Need to do this in `visit_block` instead of `visit_fn` since the types
        // in the function declaration/prototype is traversed before the
        // `visit_fn` is.
        if let AstToken::Block(BlockHeader::Fn(func), ..) = ast_token {
            self.cur_func_name = func.borrow().name.clone();
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if fn_call.method_adt.is_none() || fn_call.is_fn_ptr_call {
            return;
        }

        if let Err(err) = self.collect_nested_generic_methods(&mut ctx.ty_ctx.ty_env, fn_call) {
            self.errors.push(err);
        }
    }

    fn visit_type(&mut self, type_id: &mut TypeId, ctx: &mut TraverseCtx) {
        if let Err(err) = self.collect_nested_generic_adts(&mut ctx.ty_ctx.ty_env, *type_id) {
            self.errors.push(err);
        }
    }
}
