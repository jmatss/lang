use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    token::expr::Expr,
    types::Type,
};
use log::{debug, warn};
use std::collections::{hash_map, HashMap};

use crate::AnalyzeContext;

// TODO: Remove `analyze_context` from here, makes no sense to have it nested
//       in this TypeContext. The TypeCOntexct currently needs to look up
//       struct declaration when solving substitutions.

/// "Stand-alone" struct that will contain all the important state/context
/// after the type inference have been ran. This struct will then be used by
/// the "TypeSolver" to make sure that everything has gone as expected and
/// it will also replace all temporary "unknown" types with real ones.
pub struct TypeContext<'a> {
    /// Needed to look up ex. struct members.
    pub analyze_context: &'a mut AnalyzeContext,

    /// The key is a "unknown" type that is to be infered and the value
    /// is the type that the "unknown" type infers to. This map will be updated
    /// continuously during this step but should contain all substitutions after
    /// this analyzer is done.
    ///
    /// If a "unknown" type doesn't have a substitution in this map at the end
    /// of this step, the program didn't have enough information to infer the type
    /// and the error should be reported.
    pub substitutions: HashMap<Type, Type>,

    /// This contains constraints/equalities that, from looking at the actual source
    /// code, needs to be true. Ex. "var x = y" would create a constraint that
    /// x and y needs to be equal. These constraints will continuous added and
    /// also tried to be solved during this analyzing step.
    ///
    /// If a constraint can be solved, it will "converted" to a substitution that
    /// will be moved to into `substitutions`. Ex. "var x: i32 = y" is a solvable
    /// constraint that would lead to a substitution "typeof(y) => i32".
    ///
    /// If a constraint is added that can never be true ex. "i32 == u32",
    /// that should be reported as a error.
    pub constraints: Vec<(Type, Type)>,
}

#[derive(Debug, Clone)]
pub enum SubResult {
    Solved(Type),
    UnSolved(Type),
    Err(LangError),
}

impl<'a> TypeContext<'a> {
    pub fn new(analyze_context: &'a mut AnalyzeContext) -> Self {
        Self {
            analyze_context,
            substitutions: HashMap::default(),
            constraints: Vec::default(),
        }
    }

    /// Inserts a constraint. This function sorts the lhs and rhs in order of:
    ///   primitive - aggregated - unknown - generic
    /// This will make it easier to match the types in other parts of the code.
    pub fn insert_constraint(&mut self, lhs: Type, rhs: Type) {
        debug!("Insert constraint -- lhs: {:?}, rhs: {:?}", &lhs, &rhs);
        if lhs.is_primitive() {
            self.constraints.push((lhs, rhs));
        } else if rhs.is_primitive() {
            self.constraints.push((rhs, lhs));
        } else if lhs.is_aggregated() {
            self.constraints.push((lhs, rhs));
        } else if rhs.is_aggregated() {
            self.constraints.push((rhs, lhs));
        } else if lhs.is_unknown_any() {
            self.constraints.push((lhs, rhs));
        } else if rhs.is_unknown_any() {
            self.constraints.push((rhs, lhs));
        } else {
            self.constraints.push((lhs, rhs));
        }
    }

    /// Takes in two types and sorts them in the correct "mapping order" i.e.
    /// which type should map to which in the substitution map.
    /// The left item in the returned tuple should be mapped to the right.
    /// Returns Err if something goes wrong and returns None if the types are
    /// equal and shouldn't be mapped.
    pub fn get_mapping_direction(&mut self, lhs: Type, rhs: Type) -> CustomResult<(Type, Type)> {
        // TODO: Can mapping unknowns (any/int/float) to each other cause infinite
        //       recursion. Probably; how can it be prevented?

        debug!(
            "Getting mapping direction -- lhs: {:?}, rhs: {:?}",
            lhs, rhs
        );

        let lhs_sub = match self.get_substitution(&lhs, false) {
            SubResult::Solved(replaced_ty) | SubResult::UnSolved(replaced_ty) => replaced_ty,
            SubResult::Err(err) => return Err(err),
        };
        let rhs_sub = match self.get_substitution(&rhs, false) {
            SubResult::Solved(replaced_ty) | SubResult::UnSolved(replaced_ty) => replaced_ty,
            SubResult::Err(err) => return Err(err),
        };

        if !lhs_sub.is_compatible(&rhs_sub) {
            return Err(LangError::new(
                format!(
                    "Tried to map incompatible types. Lhs_sub: {:?}, rhs_sub: {:?}",
                    lhs_sub, rhs_sub
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ));
        }

        Ok(
            if lhs_sub == rhs_sub || lhs_sub.is_unknown() || lhs_sub.is_generic() {
                (lhs, rhs)
            } else if rhs_sub.is_unknown() || rhs_sub.is_generic() {
                (rhs, lhs)
            } else if !lhs_sub.is_unknown_any() && !rhs_sub.is_unknown_any() {
                // True if both are known, but they aren't equal and they are still
                // compatible. This means that both are aggregates of the same type.
                // Structs will have been filtered earlier in this function.
                match (lhs_sub.clone(), rhs_sub.clone()) {
                    (Type::Pointer(inner_lhs), Type::Pointer(inner_rhs)) => {
                        self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                    }
                    (Type::Array(inner_lhs, _), Type::Array(inner_rhs, _)) => {
                        self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                    }
                    _ => unreachable!(format!("lhs_sub: {:?}, rhs_sub: {:?}", lhs_sub, rhs_sub)),
                }
            } else if !lhs_sub.is_unknown_any() {
                (rhs, lhs) // Only lhs known.
            } else if !rhs_sub.is_unknown_any() {
                (lhs, rhs) // Only rhs known.
            } else if lhs_sub.is_unknown_struct_member() {
                // Prefer struct member unknowns over int/float/array unknowns since
                // it will always have its type set in the struct definition.
                (rhs, lhs)
            } else if rhs.is_unknown_struct_member() {
                (lhs, rhs)
            } else if lhs_sub.is_unknown_int() || lhs_sub.is_unknown_float() {
                // Prefer int/float unknowns over array member unknowns.
                (rhs, lhs)
            } else if rhs.is_unknown_int() || rhs_sub.is_unknown_float() {
                (lhs, rhs)
            } else {
                // Both are array member unknowns, direction doesn't matter.
                (rhs, lhs)
            },
        )
    }

    /// Checks if adding a substitution from `from` to `type` creates a loop.
    /// This will be called recursivly to check if mapping `from` to a type
    /// that `to` maps to causes a loop... and so on.
    fn causes_loop(&self, from: &Type, to: &Type) -> bool {
        if let Some(new_to) = self.substitutions.get(to) {
            if from == new_to {
                true
            } else {
                self.causes_loop(from, new_to)
            }
        } else {
            false
        }
    }

    /// Inserts a new substitution, mapping `from` to `to`.
    pub fn insert_substitution(&mut self, from: Type, to: Type) -> CustomResult<()> {
        debug!("Insert substitution -- from: {:?}, to: {:?}", &from, &to);

        // Can't map to itself (infinite recursion) and shouldn't cause any kind
        // of loop.
        if from == to || self.causes_loop(&from, &to) {
            debug!(
                "Substitution to self or causes loop -- from unsolved: {:?}, to solved: {:?}",
                &from, &to
            );
            return Ok(());
        }

        // If the `from` doesn't already have a mapping, just insert the
        // new mapping to `to` and return. Otherwise add a new substitution
        // between the old and the new `to`.
        let old_to = match self.substitutions.entry(from) {
            hash_map::Entry::Occupied(ref mut o) => o.get().clone(),
            hash_map::Entry::Vacant(v) => {
                v.insert(to);
                return Ok(());
            }
        };

        match self.get_mapping_direction(to, old_to) {
            Ok((new_from, new_to)) => {
                if !self.causes_loop(&new_from, &new_to) {
                    self.insert_substitution(new_from, new_to)
                } else {
                    Ok(())
                }
            }
            Err(err) => Err(err),
        }
    }

    /// Substitutes the given type recursively until no more substitution can
    /// be found. If the type is already known, this function will make no
    /// substitution and will not report any errors.
    /// If `finalize` is set to true, any "UnknownInt" or "UnknownFloat" will be
    /// converted to its default types (i32/f32). At that point the type is assumed
    /// to be set to the correct type and will never change again.
    /// If `finalize` is set to false, the UnknownInt" or "UnknownFloat" will be
    /// kept since the type inference isn't finished yet and they might be set
    /// to something different than the default i32/f32.
    pub fn get_substitution(&mut self, ty: &Type, finalize: bool) -> SubResult {
        let mut cur_ty = ty.clone();

        let mut i = 0;
        loop {
            if i > 50 {
                return SubResult::Err(LangError::new(
                    format!("Recursion error, #iterations: {}", i),
                    AnalyzeError {
                        line_nr: 0,
                        column_nr: 0,
                    },
                ));
            }
            debug!("Substituting, i: {} -- cur_ty: {:?}", i, cur_ty);
            i += 1;

            if self.substitutions.contains_key(&cur_ty) {
                if let Some(sub_ty) = self.substitutions.get(&cur_ty) {
                    cur_ty = sub_ty.clone();
                } else {
                    unreachable!()
                }
            } else if cur_ty.is_primitive() {
                return SubResult::Solved(cur_ty);
            } else if cur_ty.is_aggregated() {
                match &mut cur_ty {
                    Type::Pointer(inner_ty) | Type::Array(inner_ty, _) => {
                        match self.get_substitution(&inner_ty, finalize) {
                            SubResult::Solved(sub_ty) => {
                                // Add a new substitution for the inner types
                                // if they are solvable.
                                if let Err(err) =
                                    self.insert_substitution(*inner_ty.clone(), sub_ty.clone())
                                {
                                    return SubResult::Err(err);
                                }
                                warn!(
                                    "aggregate solved -- inner: {:#?}\nsub_ty: {:#?}",
                                    inner_ty, sub_ty
                                );

                                *inner_ty = Box::new(sub_ty);
                                return SubResult::Solved(cur_ty);
                            }
                            SubResult::UnSolved(un_sub_ty) => {
                                // Add a new constraint for the inner types
                                // if they are unsolvable.
                                self.insert_constraint(*inner_ty.clone(), un_sub_ty.clone());
                                warn!(
                                    "aggregate unsolved -- inner: {:#?}\nun_sub_ty: {:#?}",
                                    inner_ty, un_sub_ty
                                );

                                *inner_ty = Box::new(un_sub_ty);
                                return SubResult::UnSolved(cur_ty);
                            }
                            err => return err,
                        }
                    }
                    Type::CompoundType(_, gen_map) => {
                        let mut is_solved = true;

                        // All generic types needs to be solved. If any of them
                        // can't be solved, this whould compound type will count
                        // as unsolvable.
                        for gen_ty in gen_map.values_mut() {
                            match self.get_substitution(gen_ty, finalize) {
                                SubResult::Solved(sub_ty) => {
                                    // Add a new substitution for the gen types
                                    // if they are solvable.
                                    if let Err(err) =
                                        self.insert_substitution(gen_ty.clone(), sub_ty.clone())
                                    {
                                        return SubResult::Err(err);
                                    }
                                    warn!(
                                        "compound solved -- gen_ty: {:#?}\nsub_ty: {:#?}",
                                        gen_ty, sub_ty
                                    );

                                    *gen_ty = sub_ty;
                                }
                                SubResult::UnSolved(un_sub_ty) => {
                                    // Add a new constraint for the gen types
                                    // if they are unsolvable.
                                    self.insert_constraint(gen_ty.clone(), un_sub_ty.clone());
                                    warn!(
                                        "compound unsolved -- gen_ty: {:#?}\nun_sub_ty: {:#?}",
                                        gen_ty, un_sub_ty
                                    );

                                    *gen_ty = un_sub_ty;
                                    is_solved = false;
                                }
                                err => return err,
                            }
                        }

                        if is_solved {
                            return SubResult::Solved(cur_ty);
                        } else {
                            return SubResult::UnSolved(cur_ty);
                        }
                    }
                    Type::Custom(_) => return SubResult::Solved(cur_ty),
                    _ => unreachable!("Unmatched aggregate type: {:#?}", cur_ty),
                }
            } else {
                match cur_ty.clone() {
                    // True if finalize is either true or false.
                    Type::Unknown(_) => return SubResult::UnSolved(cur_ty),

                    // Set default type for int/float if this is to be finalized.
                    // Don't set default int/float if this isn't to be finalized.
                    Type::UnknownInt(..) if finalize => {
                        return SubResult::Solved(Type::I32);
                    }
                    Type::UnknownFloat(_) if finalize => {
                        return SubResult::Solved(Type::F32);
                    }
                    Type::UnknownInt(..) | Type::UnknownFloat(_) if !finalize => {
                        return SubResult::UnSolved(cur_ty)
                    }

                    // If a generic matches a substition, it will be done in the
                    // else-if above. Ending up here is makes it "unsolvable".
                    Type::Generic(_) => return SubResult::UnSolved(cur_ty),

                    Type::UnknownStructMember(ref struct_ty, ref member_name) => {
                        let struct_ty = match self.get_substitution(struct_ty, finalize) {
                            SubResult::Solved(struct_ty) => {
                                cur_ty = Type::UnknownStructMember(
                                    Box::new(struct_ty.clone()),
                                    member_name.clone(),
                                );
                                struct_ty
                            }
                            SubResult::UnSolved(un_sub_ty) => {
                                cur_ty = Type::UnknownStructMember(
                                    Box::new(un_sub_ty.clone()),
                                    member_name.clone(),
                                );
                                un_sub_ty
                            }
                            err => return err,
                        };

                        match &struct_ty {
                            Type::Custom(struct_name) => {
                                // TODO: Fix this. Move `analyze_context` out
                                //       of `type_context` and don't hardcode
                                //       the default block ID.
                                let var = match self.analyze_context.get_struct_member(
                                    struct_name,
                                    member_name,
                                    0,
                                ) {
                                    Ok(var) => var.clone(),
                                    Err(err) => {
                                        return SubResult::Err(err);
                                    }
                                };

                                if let Some(var_type) = &var.ret_type {
                                    match self.get_substitution(var_type, finalize) {
                                        SubResult::Solved(sub_ty) => {
                                            return SubResult::Solved(sub_ty)
                                        }
                                        SubResult::UnSolved(un_sub_ty) => {
                                            return SubResult::UnSolved(un_sub_ty)
                                        }
                                        err => return err,
                                    }
                                } else {
                                    return SubResult::Err(LangError::new(
                                        format!(
                                            "Type not set for member \"{}\" in struct \"{}\"",
                                            member_name, struct_name
                                        ),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                }
                            }

                            Type::CompoundType(struct_name, gens) => {
                                let var = match self.analyze_context.get_struct_member(
                                    struct_name,
                                    member_name,
                                    0,
                                ) {
                                    Ok(var) => var.clone(),
                                    Err(err) => {
                                        return SubResult::Err(err);
                                    }
                                };

                                if let Some(ty) = &var.ret_type {
                                    // Since this fetched the actual struct "template"
                                    // that is used by all, the generics will still
                                    // be the "Generics". Need to replace them with
                                    // the actual type for this specific use of the
                                    // struct.
                                    let mut ty = ty.clone();
                                    ty.replace_generics_impl(gens);

                                    match self.get_substitution(&ty, finalize) {
                                        SubResult::Solved(sub_ty) => {
                                            return SubResult::Solved(sub_ty)
                                        }
                                        SubResult::UnSolved(un_sub_ty) => {
                                            return SubResult::UnSolved(un_sub_ty)
                                        }
                                        err => return err,
                                    }
                                } else {
                                    return SubResult::Err(LangError::new(
                                        format!(
                                            "Type not set for member \"{}\" in struct \"{}\"",
                                            member_name, struct_name
                                        ),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                }
                            }

                            _ => {
                                if finalize {
                                    return SubResult::Err(LangError::new(
                                    format!(
                                        "Struct access (member name \"{}\") on non struct type: {:?}",
                                        member_name, cur_ty
                                    ),
                                    AnalyzeError {
                                        line_nr: 0,
                                        column_nr: 0,
                                    },
                                ));
                                } else {
                                    return SubResult::UnSolved(cur_ty);
                                }
                            }
                        }
                    }

                    Type::UnknownStructMethod(ref struct_ty, ref method_name) => {
                        let struct_ty = match self.get_substitution(struct_ty, finalize) {
                            SubResult::Solved(struct_ty) => {
                                cur_ty = Type::UnknownStructMethod(
                                    Box::new(struct_ty.clone()),
                                    method_name.clone(),
                                );
                                struct_ty
                            }
                            SubResult::UnSolved(un_sub_ty) => {
                                cur_ty = Type::UnknownStructMethod(
                                    Box::new(un_sub_ty.clone()),
                                    method_name.clone(),
                                );
                                un_sub_ty
                            }
                            err => return err,
                        };

                        match &struct_ty {
                            Type::Custom(struct_name) => {
                                // TODO: Fix this. Move `analyze_context` out
                                //       of `type_context` and don't hardcode
                                //       the default block ID.
                                let method = match self.analyze_context.get_struct_method(
                                    struct_name,
                                    method_name,
                                    0,
                                ) {
                                    Ok(method) => method.clone(),
                                    Err(err) => {
                                        return SubResult::Err(err);
                                    }
                                };

                                if let Some(ret_type) = &method.ret_type {
                                    match self.get_substitution(ret_type, finalize) {
                                        SubResult::Solved(sub_ty) => {
                                            return SubResult::Solved(sub_ty)
                                        }
                                        SubResult::UnSolved(un_sub_ty) => {
                                            return SubResult::UnSolved(un_sub_ty)
                                        }
                                        err => return err,
                                    }
                                } else {
                                    return SubResult::Solved(Type::Void);
                                }
                            }
                            _ => {
                                if finalize {
                                    return SubResult::Err(LangError::new(
                                        format!(
                                            "Method call (method name \"{}\") on non struct type: {:?}",
                                            method_name, cur_ty
                                        ),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                } else {
                                    return SubResult::UnSolved(cur_ty);
                                }
                            }
                        }
                    }

                    Type::UnknownMethodArgument(
                        ref struct_ty,
                        ref method_name,
                        ref name_opt,
                        ref idx,
                    ) => {
                        let struct_sub_ty = match self.get_substitution(&struct_ty, finalize) {
                            SubResult::Solved(sub_ty) => {
                                cur_ty = Type::UnknownMethodArgument(
                                    Box::new(sub_ty.clone()),
                                    method_name.clone(),
                                    name_opt.clone(),
                                    *idx,
                                );
                                sub_ty
                            }
                            SubResult::UnSolved(un_sub_ty) => {
                                cur_ty = Type::UnknownMethodArgument(
                                    Box::new(un_sub_ty.clone()),
                                    method_name.clone(),
                                    name_opt.clone(),
                                    *idx,
                                );
                                un_sub_ty
                            }
                            err => return err,
                        };

                        match &struct_sub_ty {
                            Type::Custom(struct_name) => {
                                // TODO: Fix this. Move `analyze_context` out
                                //       of `type_context` and don't hardcode
                                //       the default block ID.
                                let method = match self.analyze_context.get_struct_method(
                                    struct_name,
                                    method_name,
                                    0,
                                ) {
                                    Ok(method) => method,
                                    Err(err) => {
                                        return SubResult::Err(err);
                                    }
                                };

                                // If this is a named argument, use that name to
                                // identify the parameter type and then get the
                                // index for the parameter. Otherwise use the
                                // index of the argument directly.
                                let actual_idx = if let Some(arg_name) = name_opt {
                                    let params = if let Some(params) = &method.parameters {
                                        params
                                    } else {
                                        return SubResult::Err(LangError::new(
                                            format!(
                                                "Method \"{}\" contains no parameters",
                                                method_name
                                            ),
                                            AnalyzeError {
                                                line_nr: 0,
                                                column_nr: 0,
                                            },
                                        ));
                                    };

                                    if let Some((idx, _)) = params
                                        .iter()
                                        .enumerate()
                                        .find(|(_, param)| &param.name == arg_name)
                                    {
                                        idx
                                    } else {
                                        return SubResult::Err(LangError::new(
                                            format!(
                                                "Unable to find parameter \"{}\" in method \"{}\".",
                                                arg_name, method_name
                                            ),
                                            AnalyzeError {
                                                line_nr: 0,
                                                column_nr: 0,
                                            },
                                        ));
                                    }
                                } else {
                                    *idx as usize
                                };

                                let arg_ty = if let Some(param_ty) = &method
                                    .parameters
                                    .as_ref()
                                    .map(|params| {
                                        params
                                            .get(actual_idx)
                                            .map(|param| param.ret_type.clone())
                                            .flatten()
                                    })
                                    .flatten()
                                {
                                    param_ty.clone()
                                } else {
                                    return SubResult::Err(LangError::new(
                                        format!(
                                            "Unable to get type for method \"{}\" parameter at index \"{}\".",
                                            method_name, actual_idx
                                        ),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                };

                                return SubResult::Solved(arg_ty);
                            }
                            _ => {
                                if finalize {
                                    return SubResult::Err(LangError::new(
                                        format!(
                                            "Method call (method name \"{}\") argument used on non struct type(???): {:?}",
                                            method_name, cur_ty
                                        ),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                } else {
                                    return SubResult::UnSolved(cur_ty);
                                }
                            }
                        }
                    }

                    Type::UnknownArrayMember(array_ty) => {
                        let array_sub_ty = match self.get_substitution(&array_ty, finalize) {
                            SubResult::Solved(sub_ty) => {
                                cur_ty = Type::UnknownArrayMember(Box::new(sub_ty.clone()));
                                sub_ty
                            }
                            SubResult::UnSolved(un_sub_ty) => {
                                // If it is known that this is an array but the
                                // inner type is unknown, add a constrain between
                                // the inner type and the return type of the
                                // current "UnknownArrayMember".
                                // This constraint is impossible to figure
                                // out before this point, so this is a ugly fix
                                // to ensure that nested arrays are solvable.
                                if let Type::Array(inner_ty, _) = &un_sub_ty {
                                    self.insert_constraint(cur_ty, *inner_ty.clone());
                                }

                                cur_ty = Type::UnknownArrayMember(Box::new(un_sub_ty.clone()));
                                un_sub_ty
                            }
                            err => return err,
                        };

                        match &array_sub_ty {
                            Type::Array(inner_ty, _) => {
                                match self.get_substitution(inner_ty, finalize) {
                                    SubResult::Solved(sub_ty) => return SubResult::Solved(sub_ty),
                                    SubResult::UnSolved(un_sub_ty) => {
                                        return SubResult::UnSolved(un_sub_ty)
                                    }
                                    err => return err,
                                }
                            }
                            _ => {
                                if finalize {
                                    return SubResult::Err(LangError::new(
                                        format!("Array access on non Array type: {:?}", cur_ty),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                } else {
                                    return SubResult::UnSolved(cur_ty);
                                }
                            }
                        }
                    }

                    _ => unreachable!(),
                }
            }
        }
    }

    pub fn get_expr_type_opt<'b>(
        &self,
        expr_opt: Option<&'b mut Expr>,
    ) -> CustomResult<&'b mut Type> {
        if let Some(expr) = expr_opt {
            expr.get_expr_type_mut()
        } else {
            Err(LangError::new(
                "expr opt set to None.".into(),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }

    pub fn get_ret_type<'b>(
        &self,
        ret_type_opt: Option<&'b mut Type>,
    ) -> CustomResult<&'b mut Type> {
        if let Some(ty) = ret_type_opt {
            Ok(ty)
        } else {
            Err(LangError::new(
                format!("Unable to unwrap ret type: {:?}", ret_type_opt),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ))
        }
    }
}
