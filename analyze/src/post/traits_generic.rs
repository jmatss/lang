use crate::AnalyzeContext;
use common::{
    error::LangError,
    token::block::TraitCompareError,
    traverser::TraverseContext,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    util,
    visitor::Visitor,
    BlockId,
};
use std::collections::HashSet;

/// Checks that all generics that have specified "where implements" clauses
/// actual are instances of types that implements the specified trait and all
/// its methods.
pub struct TraitsGenericAnalyzer<'a> {
    analyze_context: &'a AnalyzeContext,

    /// Contains a set keeping track of all seen types. This is done to prevent
    /// checking the same type multiple times.
    seen_types: HashSet<Ty>,

    errors: Vec<LangError>,
}

impl<'a> TraitsGenericAnalyzer<'a> {
    pub fn new(analyze_context: &'a AnalyzeContext) -> Self {
        Self {
            analyze_context,
            seen_types: HashSet::default(),
            errors: Vec::default(),
        }
    }

    fn verify_adt_traits(&mut self, old_adt_name: &str, generics: &Generics, block_id: BlockId) {
        let adt_name = util::to_generic_name(old_adt_name, generics);

        let adt = match self.analyze_context.get_adt(&adt_name, block_id) {
            Ok(adt) => adt,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let adt = adt.borrow();

        // Iterate through all generics for this ADT and make sure that the
        // generics implements the traits specified in the "where" clause.
        for (generic_name, generic_ty) in generics.iter_names().zip(generics.iter_types()) {
            let trait_tys = if let Some(trait_tys) = adt
                .implements
                .as_ref()
                .map(|impls| impls.get(generic_name))
                .flatten()
            {
                trait_tys
            } else {
                // If there are no "implements" clause for the specific generic,
                // nothing to do here, continue looking at the next generic.
                continue;
            };

            // The `generic_ty` will be the instance type that has replaced the
            // generic with name `generic_name`. This is the type that should
            // implement the traits in `trait_tys`.
            let (generic_adt_name, generic_adt) = if let Some(ident) = generic_ty.get_ident() {
                match self.analyze_context.get_adt(&ident, block_id) {
                    Ok(generic_adt) => (ident, generic_adt),
                    Err(err) => {
                        self.errors.push(err);
                        continue;
                    }
                }
            } else {
                let trait_names = trait_tys
                    .iter()
                    .map(|ty| format!("\n{}", ty.to_string()))
                    .collect::<String>();
                let err = self.analyze_context.err(format!(
                    "ADT \"{0}\" has \"where\" clause for type \"{1}\" which isn't a ADT. \
                    The type \"{1}\" can therefore not implement the required traits:{2}.",
                    adt_name,
                    generic_ty.to_string(),
                    trait_names,
                ));
                self.errors.push(err);
                continue;
            };
            let generic_adt = generic_adt.borrow();

            let gen_struct_methods = &generic_adt.methods;

            for trait_ty in trait_tys {
                let trait_name = if let Ty::CompoundType(InnerTy::Trait(trait_name), ..) = trait_ty
                {
                    trait_name
                } else {
                    let err = self.analyze_context.err(format!(
                        "Generic with name \"{}\" on ADT \"{}\" implements non trait type: {:#?}",
                        generic_name, adt_name, trait_ty
                    ));
                    self.errors.push(err);
                    continue;
                };

                let trait_ = match self.analyze_context.get_trait(trait_name, block_id) {
                    Ok(trait_) => trait_,
                    Err(err) => {
                        self.errors.push(err);
                        continue;
                    }
                };

                for trait_method in &trait_.borrow().methods {
                    let method_name = trait_method.name.clone();

                    let gen_struct_method = if let Some(struct_method) =
                        gen_struct_methods.get(&method_name)
                    {
                        struct_method
                    } else {
                        let err = self.analyze_context.err(format!(
                            "Struct \"{0}\" requires that its generic type \"{1}\" implements \
                            the trait \"{2}\". The type \"{3}\" is used as generic \"{1}\", \
                            but it does NOT implement the function \"{4}\" from the trait \"{2}\".",
                            old_adt_name, generic_name, trait_name, generic_adt_name, method_name
                        ));
                        self.errors.push(err);
                        return;
                    };

                    // TODO: Make safe. Gets a borrow error if done as usual,
                    //       there is a mutable borrow already. Where is that?
                    let struct_method_borrow =
                        unsafe { gen_struct_method.as_ptr().as_ref().unwrap() };

                    // Make the check to ensure that the trait method are correctly implemented.
                    if let Err(cmp_errors) = struct_method_borrow.trait_cmp(trait_method) {
                        let err_msg_start = format!(
                            "Struct \"{}\"s impl of trait \"{}\"s method \"{}\" is incorrect.\n",
                            old_adt_name, trait_name, method_name,
                        );
                        let err_msg_end = format!(
                            "\nstruct_method: {:#?}\ntrait_method: {:#?}",
                            struct_method_borrow, trait_method
                        );

                        for cmp_err in cmp_errors {
                            let err_msg = match cmp_err {
                                TraitCompareError::ParamLenDiff(s_len, t_len, contains_this) => {
                                    let (s_len, t_len) = if contains_this {
                                        (s_len, t_len + 1)
                                    } else {
                                        (s_len, t_len)
                                    };
                                    format!(
                                        "Parameter list length differs. Struct len: {}, trait len: {}",
                                        s_len,
                                        t_len,
                                    )
                                }
                                TraitCompareError::ParamTypeDiff(t_idx, contains_this) => {
                                    let s_idx = if contains_this { t_idx + 1 } else { t_idx };
                                    format!(
                                        "Parameter types at idx {} differs. Struct param type: {:#?}, trait param type: {:#?}",
                                        s_idx,
                                        struct_method_borrow.parameters.as_ref().unwrap().get(s_idx).unwrap().borrow().ty,
                                        trait_method.parameters.as_ref().unwrap().get(t_idx).unwrap().borrow().ty,
                                    )
                                }
                                TraitCompareError::ReturnTypeDiff => {
                                    format!(
                                        "Return types differ. Struct return type: {:#?}, trait return type: {:#?}",
                                        struct_method_borrow.ret_type,
                                        trait_method.ret_type,
                                    )
                                }
                                TraitCompareError::GenericsLenDiff(s_len, t_len) => {
                                    format!(
                                        "Generic list length differs. Struct len: {}, trait len: {}",
                                        s_len,
                                        t_len,
                                    )
                                }
                                TraitCompareError::GenericsNameDiff(idx) => {
                                    format!(
                                        "Generic at idx {} differs. Struct generic name: {:#?}, trait generic name: {:#?}",
                                        idx,
                                        struct_method_borrow.generic_names.as_ref().unwrap().get(idx).unwrap(),
                                        trait_method.generic_names.as_ref().unwrap().get(idx).unwrap(),
                                    )
                                }
                                TraitCompareError::ImplsLenDiff(s_len, t_len) => {
                                    format!(
                                        "Implements list length differs. Struct len: {}, trait len: {}",
                                        s_len,
                                        t_len,
                                    )
                                }
                                TraitCompareError::ImplsNameDiff(Some(s_name), None) => {
                                    format!(
                                        "Found impls for generic with name \"{}\" in struct, not found trait.",
                                        s_name,
                                    )
                                }
                                TraitCompareError::ImplsNameDiff(None, Some(t_name)) => {
                                    format!(
                                        "Found impls for generic with name \"{}\" in trait, not found struct.",
                                        t_name,
                                    )
                                }
                                TraitCompareError::ImplsNameDiff(..) => {
                                    unreachable!()
                                }
                                TraitCompareError::ImplsTypeDiff(gen_name) => {
                                    format!(
                                        "Impls list diff for generic with name \"{}\".",
                                        gen_name,
                                    )
                                }
                            };

                            let err = self
                                .analyze_context
                                .err(format!("{}{}{}", err_msg_start, err_msg, err_msg_end));
                            self.errors.push(err);
                        }
                    }
                }
            }
        }
    }

    /// Recursively gets the types for the given `ty`. If any of the types are
    /// a ADT with a generic, checks that the generics of that ADT actually
    /// implements the specified traits in the "where" clause (if any).
    fn check_adt_traits(&mut self, ty: &Ty, block_id: BlockId) {
        match ty {
            Ty::CompoundType(inner_ty, generics, ..) => {
                for ty_i in generics.iter_types() {
                    self.check_adt_traits(ty_i, block_id);
                }

                // TODO: Implement for unions as well.
                match inner_ty {
                    InnerTy::Struct(ident) | InnerTy::Union(ident) => {
                        if !generics.is_empty() {
                            self.verify_adt_traits(ident, generics, block_id);
                        }
                    }
                    _ => (),
                };
            }

            Ty::Array(ty_i, expr_opt, ..) => {
                if let Some(expr_ty) = expr_opt
                    .as_ref()
                    .map(|expr| expr.get_expr_type().ok())
                    .flatten()
                {
                    self.check_adt_traits(&expr_ty, block_id);
                }
                self.check_adt_traits(ty_i, block_id);
            }
            Ty::Expr(expr, ..) => {
                if let Ok(expr_ty) = expr.get_expr_type() {
                    self.check_adt_traits(&expr_ty, block_id);
                }
            }

            Ty::Pointer(ty_i, _)
            | Ty::UnknownAdtMember(ty_i, ..)
            | Ty::UnknownAdtMethod(ty_i, ..)
            | Ty::UnknownMethodArgument(ty_i, ..)
            | Ty::UnknownMethodGeneric(ty_i, ..)
            | Ty::UnknownArrayMember(ty_i, ..) => {
                self.check_adt_traits(ty_i, block_id);
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }
    }
}

impl<'a> Visitor for TraitsGenericAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    // TODO: More effective way to check this.
    fn visit_type(&mut self, ty: &mut Ty, ctx: &TraverseContext) {
        if !self.seen_types.contains(ty) {
            self.seen_types.insert(ty.clone());
            self.check_adt_traits(ty, ctx.block_id);
        }
    }
}