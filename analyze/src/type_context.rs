use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    token::expr::Expr,
    types::Type,
};
use log::debug;
use std::collections::HashMap;

use crate::AnalyzeContext;

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
    pub fn get_substitution(&self, ty: &Type, finalize: bool) -> CustomResult<Type> {
        let mut cur_ty = ty.clone();

        let mut i = 0;
        loop {
            if i > 50 {
                return Err(LangError::new(
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
                break;
            } else if cur_ty.is_aggregated() {
                match &mut cur_ty {
                    Type::Pointer(inner_ty) | Type::Array(inner_ty, _) => {
                        *inner_ty = Box::new(self.get_substitution(&inner_ty, finalize)?);
                    }
                    _ => (),
                }
                break;
            } else if self.substitutions.contains_key(&cur_ty) {
                if let Some(sub_ty) = self.substitutions.get(&cur_ty) {
                    cur_ty = sub_ty.clone();
                } else {
                    unreachable!()
                }
            } else {
                match &cur_ty.clone() {
                    Type::Unknown(_) => {
                        return Err(LangError::new(
                            format!("Unable to find substitute for unknown type: {:?}.", cur_ty),
                            AnalyzeError {
                                line_nr: 0,
                                column_nr: 0,
                            },
                        ));
                    }

                    // Set default type for int/float if this is to be finalized.
                    Type::UnknownInt(..) if finalize => {
                        cur_ty = Type::I32;
                    }
                    Type::UnknownFloat(_) if finalize => {
                        cur_ty = Type::F32;
                    }

                    // Don't set default int/float if this isn't to be finalized.
                    Type::UnknownInt(..) | Type::UnknownFloat(_) => {}

                    Type::UnknownStructMember(struct_ty, member_name) => {
                        cur_ty = self.get_substitution(struct_ty, finalize)?;
                        match &cur_ty {
                            Type::Custom(struct_name) => {
                                let var = self
                                    .analyze_context
                                    .get_struct_member(struct_name, member_name)?;

                                if let Some(ret_type) = &var.ret_type {
                                    cur_ty = self.get_substitution(ret_type, finalize)?;
                                } else {
                                    return Err(LangError::new(
                                        format!("Array access in non Array type: {:?}", cur_ty),
                                        AnalyzeError {
                                            line_nr: 0,
                                            column_nr: 0,
                                        },
                                    ));
                                }
                            }
                            _ => {
                                return Err(LangError::new(
                                format!("Struct access (member name \"{}\") on non struct type: {:?}", member_name,cur_ty),
                                AnalyzeError {
                                    line_nr: 0,
                                    column_nr: 0,
                                },
                            ));
                            }
                        }
                    }
                    Type::UnknownArrayAccess(array_ty) => {
                        cur_ty = self.get_substitution(array_ty, finalize)?;
                        match &cur_ty {
                            Type::Array(inner_ty, _) => {
                                cur_ty = self.get_substitution(inner_ty, finalize)?;
                            }
                            _ => {
                                return Err(LangError::new(
                                    format!("Array access in non Array type: {:?}", cur_ty),
                                    AnalyzeError {
                                        line_nr: 0,
                                        column_nr: 0,
                                    },
                                ));
                            }
                        }
                    }

                    _ => unreachable!(),
                }
                break;
            }
        }

        Ok(cur_ty)
    }

    pub fn get_expr_type_opt<'b>(
        &self,
        expr_opt: Option<&'b mut Expr>,
    ) -> CustomResult<&'b mut Type> {
        if let Some(expr) = expr_opt {
            expr.get_expr_type()
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
