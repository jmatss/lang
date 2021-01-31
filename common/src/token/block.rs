use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    expr::{Expr, Var},
    stmt::Modifier,
};

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    util,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockHeader {
    // TODO: Make If(/else/elseif), match, while and loop expression.
    //  So that they can return values and be used in sub expressions.
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,

    Function(Rc<RefCell<Function>>),
    Struct(Rc<RefCell<Adt>>),
    Enum(Rc<RefCell<Adt>>),
    Union(Rc<RefCell<Adt>>),
    Trait(Rc<RefCell<Trait>>),

    /// The first string is the name of the structure that this impl block
    /// implements and the second optional string is the name of the trait if
    /// impl block implements a trait. If this is just a impl for the struct,
    /// the optional will be None.
    /// The body of this block will contain thefunctions.
    Implement(String, Option<String>),

    /// A anonymous block "{ ... }" that can be used to limit the scope.
    Anonymous,

    /// Any `IfCase` blocks should be grouped together under one `If`.
    /// A `Ifcase` is a if/elif/else with its corresponding eval expression.
    ///
    /// Example 1:
    ///   if(x) {} else {}
    /// =>
    ///   Block(If, [ Block(IfCase(x), ...), Block(IfCase(_), ...) ])
    ///
    /// Example 2:
    ///   if (x) {
    ///     if (y) {
    ///       ...
    ///     }
    ///   } else (z) {
    ///     ...
    ///   } else {
    ///     ...
    ///   }
    /// =>
    ///   Block(
    ///     header: If,
    ///     body: [
    ///        Block(
    ///          header: IfCase(x),
    ///          body: [
    ///            Block(
    ///              header: If,
    ///              body: [
    ///                Block(
    ///                  header: IfCase(y),
    ///                  body: [ ... ]
    ///                )
    ///              ]
    ///            )
    ///          ]
    ///        ),
    ///        Block(
    ///          header: IfCase(z),
    ///          body: [ ... ]
    ///        ),
    ///        Block(
    ///          header: IfCase(_),
    ///          body: [ ... ]
    ///        )
    ///     ]
    ///   )
    If,
    IfCase(Option<Expr>),

    /// Any `MatchCase` blocks should be grouped together under one `Match`.
    /// See the example above for `If`, should be structured ~similarly.
    Match(Expr),
    /// The optional is used to indicate if this is the default block or not.
    /// If it is set to None, this is the default block.
    MatchCase(Option<Expr>),

    For(Var, Expr),
    // TODO: Maybe merge while and loop (?)
    While(Option<Expr>),

    // Function for testing, allows strings as test names with spaces etc.
    Test(Function),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub generics: Option<Vec<String>>,

    /// The key is the the name of the generic type it and the values are the
    /// traits that the specific generic type needs to implement.
    pub implements: Option<HashMap<String, Vec<Ty>>>,
    pub members: Option<Vec<Rc<RefCell<Var>>>>,

    pub modifiers: Vec<Modifier>,

    /// The key is the name of the method.
    pub methods: Option<HashMap<String, Rc<RefCell<Function>>>>,
}

/// Represents a Algebraic Data Type (ADT). Struct, enum or union.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Adt {
    /* Values set for all Adt types */
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub members: Vec<Rc<RefCell<Var>>>,
    /// The key is the name of the method.
    pub methods: HashMap<String, Rc<RefCell<Function>>>,
    pub kind: AdtKind,

    /* Values set for Struct and Union */
    pub generics: Option<Vec<String>>,
    /// The key is the the name of the generic type and the values are the
    /// traits that the specific generic type needs to implement.
    pub implements: Option<HashMap<String, Vec<Ty>>>,

    /* Values set for Enum */
    /// The type of the enum values. Will most likely be a integer type.
    pub ty: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AdtKind {
    Struct,
    Union,
    Enum,
    Unknown,
}

impl Adt {
    pub fn new_struct(
        name: String,
        modifiers: Vec<Modifier>,
        members: Vec<Rc<RefCell<Var>>>,
        generics: Option<Vec<String>>,
        implements: Option<HashMap<String, Vec<Ty>>>,
    ) -> Self {
        Self {
            name,
            modifiers,
            members,
            kind: AdtKind::Struct,
            methods: HashMap::default(),
            generics,
            implements,
            ty: None,
        }
    }

    pub fn new_union(
        name: String,
        modifiers: Vec<Modifier>,
        members: Vec<Rc<RefCell<Var>>>,
        generics: Option<Vec<String>>,
        implements: Option<HashMap<String, Vec<Ty>>>,
    ) -> Self {
        Self {
            name,
            modifiers,
            members,
            kind: AdtKind::Union,
            methods: HashMap::default(),
            generics,
            implements,
            ty: None,
        }
    }

    pub fn new_enum(
        name: String,
        modifiers: Vec<Modifier>,
        members: Vec<Rc<RefCell<Var>>>,
        ty: Option<Ty>,
    ) -> Self {
        Self {
            name,
            modifiers,
            members,
            kind: AdtKind::Enum,
            methods: HashMap::default(),
            generics: None,
            implements: None,
            ty,
        }
    }

    /// Returns the index of the member with name `member_name` of this ADT.
    pub fn member_index(&self, member_name: &str) -> Option<usize> {
        for (idx, member) in self.members.iter().enumerate() {
            if member.borrow().name == member_name {
                return Some(idx);
            }
        }
        None
    }
}

pub enum TraitCompareError {
    /// Param len diff (`self`, `trait`). The bool indicates if `this`/`self`
    /// is exludeded.
    ParamLenDiff(usize, usize, bool),

    GenericsLenDiff(usize, usize),

    ImplsLenDiff(usize, usize),

    /// If some parameter type differs. The bool indicates if `this`/`self`
    /// is exludeded.
    ParamTypeDiff(usize, bool),

    GenericsNameDiff(usize),

    /// The first string is the name found in the structs implementation of the
    /// trait function, and the second string is the name found in the trait
    /// declaration of the function.
    /// If one of them is None, it means that the name couldn't be found.
    ImplsNameDiff(Option<String>, Option<String>),

    /// Diff return types.
    ReturnTypeDiff,

    // TODO: Can add more information here to better pin point where the error
    //       actualy are.
    /// The string is the name generic that the impl is for.
    ImplsTypeDiff(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub generic_names: Option<Vec<String>>,

    /// This is set when a new copy of this function is created that implements
    /// the generics types (if any).
    pub generic_impls: Option<Generics>,

    /// The key is the the name of the generic type it and the values are the
    /// traits that the specific generic type needs to implement.
    pub implements: Option<HashMap<String, Vec<Ty>>>,

    pub parameters: Option<Vec<Rc<RefCell<Var>>>>,
    pub ret_type: Option<Ty>,
    pub modifiers: Vec<Modifier>,
    pub is_var_arg: bool,

    /// Will be set if this is a function in a "impl" block which means that
    /// this is a function tied to a ADt. The type will be the ADT (or the "ident"
    /// of the impl block if other than ADT are allowed).
    pub method_adt: Option<Ty>,
}

impl Function {
    pub fn new(
        name: String,
        generic_names: Option<Vec<String>>,
        implements: Option<HashMap<String, Vec<Ty>>>,
        parameters: Option<Vec<Rc<RefCell<Var>>>>,
        ret_type: Option<Ty>,
        modifiers: Vec<Modifier>,
        is_var_arg: bool,
    ) -> Self {
        Function {
            name,
            generic_names,
            generic_impls: None,
            implements,
            parameters,
            ret_type,
            is_var_arg,
            modifiers,
            method_adt: None,
        }
    }

    /// Checks if the name, parameter count, parameters types, generic count,
    /// generic names, implements and return types are the same.
    pub fn trait_cmp(&self, trait_func: &Function) -> Result<(), Vec<TraitCompareError>> {
        let mut errors = Vec::default();

        // Since the `this`/`self` parameter won't be set for the trait function,
        // need to take that into consideration if the function as a `this` modifier.
        let contains_this = self.modifiers.contains(&Modifier::This)
            || self.modifiers.contains(&Modifier::ThisPointer);

        // Check parameters in this match-statement.
        match (&self.parameters, &trait_func.parameters, contains_this) {
            // Both functions have parameters and contains "this".
            (Some(self_params), Some(trait_params), true) => {
                if self_params.len() != trait_params.len() + 1 {
                    errors.push(TraitCompareError::ParamLenDiff(
                        self_params.len(),
                        trait_params.len(),
                        true,
                    ));
                    return Err(errors);
                }

                // Take a slice of `self_paramss` where the `this` parameter has
                // been skipped so that the parameters can be compared to the
                // `trait_func` parameters correctly.
                for (idx, (self_param, other_param)) in self_params[1..self_params.len()]
                    .iter()
                    .zip(trait_params)
                    .enumerate()
                {
                    if self_param.borrow().ty != other_param.borrow().ty {
                        errors.push(TraitCompareError::ParamTypeDiff(idx, true));
                    }
                }
            }

            // Both functions have parameters and does NOT contain "this".
            (Some(self_params), Some(trait_params), false) => {
                if self_params.len() != trait_params.len() {
                    errors.push(TraitCompareError::ParamLenDiff(
                        self_params.len(),
                        trait_params.len(),
                        false,
                    ));
                    return Err(errors);
                }

                for (idx, (self_param, other_param)) in
                    self_params.iter().zip(trait_params).enumerate()
                {
                    if self_param.borrow().ty != other_param.borrow().ty {
                        errors.push(TraitCompareError::ParamTypeDiff(idx, false));
                    }
                }
            }

            // `self` has a single `this`/`self` parameter, OK.
            (Some(self_params), None, true) if self_params.len() == 1 => (),

            // Only one has parameters, error unless `self` has single "this" param.
            // That edge case is handled in the above match case.
            (Some(self_params), None, contains_this) => errors.push(
                TraitCompareError::ParamLenDiff(self_params.len(), 0, contains_this),
            ),
            (None, Some(trait_params), contains_this) => errors.push(
                TraitCompareError::ParamLenDiff(0, trait_params.len(), contains_this),
            ),

            // Both have 0 parameters, OK.
            (None, None, _) => (),
        }

        // Check generics in this match-statement.
        match (&self.generic_names, &trait_func.generic_names) {
            // Diff length of impls, error.
            (Some(self_generics), Some(trait_generics))
                if self_generics.len() != trait_generics.len() =>
            {
                errors.push(TraitCompareError::GenericsLenDiff(
                    self_generics.len(),
                    trait_generics.len(),
                ));
            }
            (Some(self_generics), None) => {
                errors.push(TraitCompareError::GenericsLenDiff(self_generics.len(), 0));
            }
            (None, Some(trait_generics)) => {
                errors.push(TraitCompareError::GenericsLenDiff(0, trait_generics.len()));
            }

            // Both functions have generics.
            (Some(self_generics), Some(trait_generics)) => {
                for (idx, (self_param, other_param)) in
                    self_generics.iter().zip(trait_generics.iter()).enumerate()
                {
                    if self_param != other_param {
                        errors.push(TraitCompareError::GenericsNameDiff(idx));
                    }
                }
            }

            // Both have 0 generics, OK.
            (None, None) => (),
        }

        // Check implements in this match-statement.
        match (&self.implements, &trait_func.implements) {
            // Diff length of impls, error.
            (Some(self_impls), Some(trait_impls)) if self_impls.len() != trait_impls.len() => {
                errors.push(TraitCompareError::ImplsLenDiff(
                    self_impls.len(),
                    trait_impls.len(),
                ))
            }
            (Some(self_impls), None) => {
                errors.push(TraitCompareError::ImplsLenDiff(self_impls.len(), 0))
            }
            (None, Some(trait_impls)) => {
                errors.push(TraitCompareError::ImplsLenDiff(0, trait_impls.len()))
            }

            // Both functions have impls.
            (Some(self_impls), Some(trait_impls)) => {
                for (self_impl_name, self_impl_tys) in self_impls.iter() {
                    let trait_impl_tys =
                        if let Some(trait_impl_tys) = trait_impls.get(self_impl_name) {
                            trait_impl_tys
                        } else {
                            errors.push(TraitCompareError::ImplsNameDiff(
                                Some(self_impl_name.clone()),
                                None,
                            ));
                            continue;
                        };

                    if self_impl_tys != trait_impl_tys {
                        errors.push(TraitCompareError::ImplsTypeDiff(self_impl_name.clone()));
                    }
                }
            }

            // Both have 0 impls, OK.
            (None, None) => (),
        }

        if self.ret_type != trait_func.ret_type {
            errors.push(TraitCompareError::ReturnTypeDiff);
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Returns the "full name" which is the name containing possible structure
    /// and generics as well.
    pub fn full_name(&self) -> LangResult<String> {
        if let Some(ty) = &self.method_adt {
            let (structure_name, structure_generics) =
                if let Ty::CompoundType(inner_ty, generics, ..) = ty {
                    match inner_ty {
                        InnerTy::Struct(ident)
                        | InnerTy::Enum(ident)
                        | InnerTy::Union(ident)
                        | InnerTy::Trait(ident) => (ident, generics),
                        _ => unreachable!("Method on non structure type: {:#?}", self),
                    }
                } else {
                    return Err(LangError::new(
                        format!("Unable to get full name for method: {:#?}", self),
                        LangErrorKind::GeneralError,
                        None,
                    ));
                };

            Ok(util::to_method_name(
                structure_name,
                Some(structure_generics),
                &self.name,
                self.generic_impls.as_ref(),
            ))
        } else {
            Ok(self.name.clone())
        }
    }

    /// Returns the "half name" which is the name that does NOT contain anything
    /// related to the structure but will contain function generics (if any).
    pub fn half_name(&self) -> String {
        if let Some(generics) = &self.generic_impls {
            util::to_generic_name(&self.name, generics)
        } else {
            self.name.clone()
        }
    }

    pub fn is_static(&self) -> bool {
        (!self.modifiers.contains(&Modifier::This)
            && !self.modifiers.contains(&Modifier::ThisPointer))
            || self.modifiers.contains(&Modifier::Static)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuiltIn {
    pub name: &'static str,
    pub parameters: Vec<Var>,
    pub generics: Option<Vec<Ty>>,
    pub ret_type: Ty,
    pub is_var_arg: bool,
}

impl BuiltIn {
    pub fn new(
        name: &'static str,
        parameters: Vec<Var>,
        generics: Option<Vec<Ty>>,
        ret_type: Ty,
        is_var_arg: bool,
    ) -> Self {
        BuiltIn {
            name,
            parameters,
            generics,
            ret_type,
            is_var_arg,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub name: String,
    pub generics: Option<Vec<String>>,
    pub methods: Vec<Function>,
    pub modifiers: Vec<Modifier>,
}

impl Trait {
    pub fn new(
        name: String,
        generics: Option<Vec<String>>,
        methods: Vec<Function>,
        modifiers: Vec<Modifier>,
    ) -> Self {
        Trait {
            name,
            generics,
            methods,
            modifiers,
        }
    }
}
