use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    token::expr::Expr,
    types::Type,
};
use log::debug;
use std::collections::HashMap;

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

    /// Substitutes the given type recursively until no more substitution can
    /// be found. If the type is already known, this function will make no
    /// substitution and will not report any errors.
    /// If `finalize` is set to true, any "UnknownInt" or "UnknownFloat" will be
    /// converted to its default types (i32/f32). At that point the type is assumed
    /// to be set to the correct type and will never change again.
    /// If `finalize` is set to false, the UnknownInt" or "UnknownFloat" will be
    /// kept since the type inference isn't finished yet and they might be set
    /// to something different than the default i32/f32.
    pub fn get_substitution(&self, ty: &Type, finalize: bool) -> SubResult {
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

            if cur_ty.is_primitive() {
                return SubResult::Solved(cur_ty);
            } else if cur_ty.is_aggregated() {
                match &mut cur_ty {
                    Type::Pointer(inner_ty) | Type::Array(inner_ty, _) => {
                        match self.get_substitution(&inner_ty, finalize) {
                            SubResult::Solved(sub_ty) => {
                                *inner_ty = Box::new(sub_ty);
                                return SubResult::Solved(cur_ty);
                            }
                            SubResult::UnSolved(_) => return SubResult::UnSolved(cur_ty),
                            err => return err,
                        }
                    }
                    _ => {
                        // Only possibility is Struct type, nothing to unwrap.
                        return SubResult::Solved(cur_ty);
                    }
                }
            } else if self.substitutions.contains_key(&cur_ty) {
                if let Some(sub_ty) = self.substitutions.get(&cur_ty) {
                    cur_ty = sub_ty.clone();
                } else {
                    unreachable!()
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
                                return SubResult::UnSolved(Type::UnknownStructMember(
                                    Box::new(un_sub_ty),
                                    member_name.clone(),
                                ))
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
                                    Ok(var) => var,
                                    Err(err) => {
                                        return SubResult::Err(err);
                                    }
                                };

                                if let Some(ret_type) = &var.ret_type {
                                    match self.get_substitution(ret_type, finalize) {
                                        SubResult::Solved(sub_ty) => {
                                            return SubResult::Solved(sub_ty)
                                        }
                                        SubResult::UnSolved(_) => {
                                            return SubResult::UnSolved(cur_ty)
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
                                return SubResult::UnSolved(Type::UnknownStructMethod(
                                    Box::new(un_sub_ty),
                                    method_name.clone(),
                                ))
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
                                    Ok(method) => method,
                                    Err(err) => {
                                        return SubResult::Err(err);
                                    }
                                };

                                if let Some(ret_type) = &method.ret_type {
                                    match self.get_substitution(ret_type, finalize) {
                                        SubResult::Solved(sub_ty) => {
                                            return SubResult::Solved(sub_ty)
                                        }
                                        SubResult::UnSolved(_) => {
                                            return SubResult::UnSolved(cur_ty)
                                        }
                                        err => return err,
                                    }
                                } else {
                                    return SubResult::Err(LangError::new(
                                        format!(
                                            "Return type not set for method \"{}\" in struct \"{}\"",
                                            method_name, struct_name
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
                                return SubResult::UnSolved(Type::UnknownMethodArgument(
                                    Box::new(un_sub_ty),
                                    method_name.clone(),
                                    name_opt.clone(),
                                    *idx,
                                ))
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
                        let sub_ty = match self.get_substitution(&array_ty, finalize) {
                            SubResult::Solved(sub_ty) => {
                                cur_ty = Type::UnknownArrayMember(Box::new(sub_ty.clone()));
                                sub_ty
                            }
                            SubResult::UnSolved(un_sub_ty) => {
                                return SubResult::UnSolved(Type::UnknownArrayMember(Box::new(
                                    un_sub_ty,
                                )))
                            }
                            err => return err,
                        };

                        match &sub_ty {
                            Type::Array(inner_ty, _) => {
                                match self.get_substitution(inner_ty, finalize) {
                                    SubResult::Solved(sub_ty) => return SubResult::Solved(sub_ty),
                                    SubResult::UnSolved(_) => return SubResult::UnSolved(cur_ty),
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
