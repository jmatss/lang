use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use super::{
    ast::AstToken,
    expr::{Expr, Var},
    stmt::Modifier,
};

use crate::{
    eq::path_eq,
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    hash::DerefType,
    hash_set::TyEnvHashSet,
    path::LangPath,
    ty::{generics::Generics, is::is_compatible, ty_env::TyEnv},
    util, BlockId, TypeId,
};

#[derive(Debug, Clone)]
pub struct Block {
    pub header: BlockHeader,
    pub body: Vec<AstToken>,
    pub id: BlockId,
    pub file_pos: FilePosition,
}

#[derive(Debug, Clone)]
pub enum BlockHeader {
    // TODO: Make If(/else/elseif), match, while and loop expression.
    //  So that they can return values and be used in sub expressions.
    // Default == None, i.e. if there are no current block. Ex. at the start of a file.
    Default,

    Fn(Arc<RwLock<Fn>>),
    Struct(Arc<RwLock<Adt>>),
    Enum(Arc<RwLock<Adt>>),
    Union(Arc<RwLock<Adt>>),
    Trait(Arc<RwLock<Trait>>),

    /// The first LangPath is the path of the ADT that this impl block implements
    /// and the second LangPath is the path of the trait that this impl block
    /// implements for the given ADT.
    /// The body of this block will contain the functions.
    Implement(LangPath, LangPath),

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
    Test(Fn),
}

/// Represents a Algebraic Data Type (ADT). Struct, enum or union.
#[derive(Debug, Clone)]
pub struct Adt {
    /* Values set for all Adt types */
    pub name: String,
    pub module: LangPath,
    pub modifiers: Vec<Modifier>,
    pub members: Vec<Arc<RwLock<Var>>>,
    pub file_pos: FilePosition,
    pub has_definition: bool,

    /// The key is the name of the method.
    pub methods: HashMap<String, Arc<RwLock<Fn>>>,
    pub kind: AdtKind,

    /* Values set for Struct and Union */
    pub generics: Option<Generics>,
    /// The key is the the name of the generic and the values are the
    /// traits that the specific generic type needs to implement.
    pub implements: Option<HashMap<String, Vec<LangPath>>>,
    /// A set of all traits that the ADT implements. The trait paths should
    /// contain the the full paths (including modules) and generics.
    pub implemented_traits: TyEnvHashSet<LangPath>,

    /* Values set for Enum */
    /// The type of the enum values. Will most likely be a integer type.
    pub enum_ty: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AdtKind {
    Struct,
    Union,
    Enum,
    Unknown,
}

impl Adt {
    /// Returns the index of the member with name `member_name` of this ADT.
    pub fn member_index(&self, member_name: &str) -> Option<usize> {
        for (idx, member) in self.members.iter().enumerate() {
            if member.as_ref().read().unwrap().name == member_name {
                return Some(idx);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct AdtBuilder {
    kind: AdtKind,
    name: Option<String>,
    module: Option<LangPath>,
    modifiers: Option<Vec<Modifier>>,
    file_pos: Option<FilePosition>,
    has_definition: Option<bool>,

    /// The key is the name of the method.
    methods: HashMap<String, Arc<RwLock<Fn>>>,
    members: Vec<Arc<RwLock<Var>>>,

    /* Values set for Struct and Union */
    generics: Option<Generics>,
    implements: Option<HashMap<String, Vec<LangPath>>>,

    /* Values set for Enum */
    enum_ty: Option<TypeId>,
}

impl AdtBuilder {
    pub fn new(kind: AdtKind) -> Self {
        Self {
            kind,
            name: None,
            module: None,
            modifiers: None,
            file_pos: None,
            has_definition: None,

            generics: None,
            implements: None,
            enum_ty: None,

            methods: HashMap::default(),
            members: Vec::default(),
        }
    }
    pub fn new_struct() -> Self {
        Self::new(AdtKind::Struct)
    }

    pub fn new_union() -> Self {
        Self::new(AdtKind::Union)
    }

    pub fn new_enum() -> Self {
        Self::new(AdtKind::Enum)
    }

    pub fn build(self) -> LangResult<Adt> {
        let name = self.name.ok_or_else(|| {
            LangError::new(
                "name not set in AdtBuilder.".into(),
                LangErrorKind::ParseError,
                None,
            )
        })?;
        let module = self.module.unwrap_or_else(LangPath::empty);
        let modifiers = self.modifiers.unwrap_or_else(|| Vec::with_capacity(0));
        let file_pos = self.file_pos.ok_or_else(|| {
            LangError::new(
                "file_pos not set in AdtBuilder.".into(),
                LangErrorKind::ParseError,
                None,
            )
        })?;
        let has_definition = self.has_definition.unwrap_or(false);

        Ok(Adt {
            name,
            module,
            modifiers,
            file_pos,
            has_definition,
            methods: self.methods,
            members: self.members,
            kind: self.kind,
            generics: self.generics,
            implements: self.implements,
            implemented_traits: TyEnvHashSet::default(),
            enum_ty: None,
        })
    }

    pub fn name(&mut self, name: String) -> &mut Self {
        self.name = Some(name);
        self
    }

    pub fn module(&mut self, module: LangPath) -> &mut Self {
        self.module = Some(module);
        self
    }

    pub fn modifiers(&mut self, modifiers: Vec<Modifier>) -> &mut Self {
        self.modifiers = Some(modifiers);
        self
    }

    pub fn file_pos(&mut self, file_pos: FilePosition) -> &mut Self {
        self.file_pos = Some(file_pos);
        self
    }

    pub fn has_definition(&mut self, has_definition: bool) -> &mut Self {
        self.has_definition = Some(has_definition);
        self
    }

    pub fn generics(&mut self, generics: Option<Generics>) -> &mut Self {
        self.generics = generics;
        self
    }

    pub fn impls(&mut self, impls: Option<HashMap<String, Vec<LangPath>>>) -> &mut Self {
        self.implements = impls;
        self
    }

    pub fn enum_ty(&mut self, enum_ty: TypeId) -> &mut Self {
        self.enum_ty = Some(enum_ty);
        self
    }

    pub fn insert_method(&mut self, method: &Arc<RwLock<Fn>>) -> &mut Self {
        let method_name = method.read().unwrap().name.clone();
        self.methods.insert(method_name, Arc::clone(method));
        self
    }

    pub fn insert_member(&mut self, member: &Arc<RwLock<Var>>) -> &mut Self {
        self.members.push(Arc::clone(member));
        self
    }
}

/// Used to store the modifer for fns when comparing with trait fn.
#[derive(Debug)]
pub enum FnModifier {
    This,
    ThisPointer,
    None,
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

    /// The impl and decl functions differs when it comes to this/thisPtr modifiers.
    /// The first modifier is the impl modifier and the second is the decl.
    ThisDiff(Option<FnModifier>, Option<FnModifier>),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub module: LangPath,
    pub generics: Option<Generics>,
    pub file_pos: FilePosition,

    /// The key is the the name of the generic type it and the values are the
    /// traits that the specific generic type needs to implement.
    pub implements: Option<HashMap<String, Vec<LangPath>>>,

    pub parameters: Option<Vec<Arc<RwLock<Var>>>>,
    pub ret_type: Option<TypeId>,
    pub modifiers: Vec<Modifier>,
    pub is_var_arg: bool,

    /// Will be set if this is a function in a "impl" block which means that
    /// this is a function tied to a ADT. The type will be the ADT (or the "ident"
    /// of the impl block if other than ADT are allowed).
    pub method_adt: Option<TypeId>,
}

impl Fn {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        name: String,
        module: LangPath,
        generics: Option<Generics>,
        file_pos: FilePosition,
        implements: Option<HashMap<String, Vec<LangPath>>>,
        parameters: Option<Vec<Arc<RwLock<Var>>>>,
        ret_type: Option<TypeId>,
        modifiers: Vec<Modifier>,
        is_var_arg: bool,
    ) -> Self {
        Self {
            name,
            module,
            generics,
            file_pos,
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
    pub fn trait_cmp(&self, ty_env: &TyEnv, trait_func: &Fn) -> Result<(), Vec<TraitCompareError>> {
        let mut errors = Vec::default();

        let trait_fn_modifier = if trait_func.modifiers.contains(&Modifier::This) {
            FnModifier::This
        } else if trait_func.modifiers.contains(&Modifier::ThisPointer) {
            FnModifier::ThisPointer
        } else {
            FnModifier::None
        };

        let self_fn_modifier = if self.modifiers.contains(&Modifier::This) {
            FnModifier::This
        } else if self.modifiers.contains(&Modifier::ThisPointer) {
            FnModifier::ThisPointer
        } else {
            FnModifier::None
        };

        // Since the `this`/`self` parameter won't be set for the trait function,
        // need to take that into consideration if the function as a `this` modifier.
        let contains_this = match (&self_fn_modifier, &trait_fn_modifier) {
            (FnModifier::This, FnModifier::This)
            | (FnModifier::ThisPointer, FnModifier::ThisPointer) => true,

            (FnModifier::None, FnModifier::None) => false,

            (FnModifier::This, FnModifier::ThisPointer)
            | (FnModifier::ThisPointer, FnModifier::This) => {
                errors.push(TraitCompareError::ThisDiff(
                    Some(self_fn_modifier),
                    Some(trait_fn_modifier),
                ));
                return Err(errors);
            }

            (FnModifier::This, FnModifier::None) | (FnModifier::ThisPointer, FnModifier::None) => {
                errors.push(TraitCompareError::ThisDiff(Some(self_fn_modifier), None));
                return Err(errors);
            }

            (FnModifier::None, FnModifier::This) | (FnModifier::None, FnModifier::ThisPointer) => {
                errors.push(TraitCompareError::ThisDiff(None, Some(trait_fn_modifier)));
                return Err(errors);
            }
        };

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

                // Take a slice of `self_params` where the `this` parameter has
                // been skipped so that the parameters can be compared to the
                // `trait_func` parameters correctly.
                for (idx, (self_param, other_param)) in self_params[1..self_params.len()]
                    .iter()
                    .zip(trait_params)
                    .enumerate()
                {
                    let first_id = self_param.as_ref().read().unwrap().ty.unwrap();
                    let second_id = other_param.as_ref().read().unwrap().ty.unwrap();
                    if !is_compatible(ty_env, first_id, second_id).unwrap() {
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
                    let first_id = self_param.as_ref().read().unwrap().ty.unwrap();
                    let second_id = other_param.as_ref().read().unwrap().ty.unwrap();
                    if !is_compatible(ty_env, first_id, second_id).unwrap() {
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
        match (&self.generics, &trait_func.generics) {
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
                for (idx, (self_param, other_param)) in self_generics
                    .iter_names()
                    .zip(trait_generics.iter_names())
                    .enumerate()
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
                for (self_impl_name, self_impl_traits) in self_impls.iter() {
                    let trait_impl_traits =
                        if let Some(trait_impl_traits) = trait_impls.get(self_impl_name) {
                            trait_impl_traits
                        } else {
                            errors.push(TraitCompareError::ImplsNameDiff(
                                Some(self_impl_name.clone()),
                                None,
                            ));
                            continue;
                        };

                    for (self_path, trait_path) in self_impl_traits.iter().zip(trait_impl_traits) {
                        if path_eq(ty_env, self_path, trait_path, DerefType::Deep).unwrap() {
                            errors.push(TraitCompareError::ImplsTypeDiff(self_impl_name.clone()));
                        }
                    }
                }
            }

            // Both have 0 impls, OK.
            (None, None) => (),
        }

        // Check return types.
        match (self.ret_type, trait_func.ret_type) {
            (Some(first_id), Some(second_id)) => {
                if !is_compatible(ty_env, first_id, second_id).unwrap() {
                    errors.push(TraitCompareError::ReturnTypeDiff);
                }
            }
            (None, Some(_)) | (Some(_), None) => errors.push(TraitCompareError::ReturnTypeDiff),
            (None, None) => (),
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Returns the "half name" which is the name that does NOT contain anything
    /// related to the structure but will contain function generics (if any).
    pub fn half_name(&self, ty_env: &TyEnv) -> String {
        if let Some(generics) = &self.generics {
            util::to_generic_name(ty_env, &self.name, generics)
        } else {
            self.name.clone()
        }
    }

    pub fn is_static(&self) -> bool {
        (!self.modifiers.contains(&Modifier::This)
            && !self.modifiers.contains(&Modifier::ThisPointer))
            || self.modifiers.contains(&Modifier::Static)
    }

    /// Returns true if this method takes `this` as a value (`this`).
    pub fn is_this_by_val(&self) -> bool {
        self.modifiers.contains(&Modifier::This)
    }

    /// Returns true if this method takes `this` as a reference/pointer (`{this}`).
    pub fn is_this_by_ref(&self) -> bool {
        self.modifiers.contains(&Modifier::ThisPointer)
    }
}

#[derive(Debug, Clone)]
pub struct BuiltIn {
    pub name: &'static str,
    pub parameters: Vec<Var>,
    pub generics: Option<Vec<TypeId>>,
    pub ret_type: TypeId,
    pub is_var_arg: bool,
}

impl BuiltIn {
    pub fn new(
        name: &'static str,
        parameters: Vec<Var>,
        generics: Option<Vec<TypeId>>,
        ret_type: TypeId,
        is_var_arg: bool,
    ) -> Self {
        Self {
            name,
            parameters,
            generics,
            ret_type,
            is_var_arg,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub module: LangPath,
    pub generics: Option<Vec<String>>,
    pub file_pos: FilePosition,
    pub methods: Vec<Fn>,
    pub modifiers: Vec<Modifier>,
}

impl Trait {
    pub fn new(
        name: String,
        module: LangPath,
        generics: Option<Vec<String>>,
        file_pos: FilePosition,
        methods: Vec<Fn>,
        modifiers: Vec<Modifier>,
    ) -> Self {
        Self {
            name,
            module,
            generics,
            file_pos,
            methods,
            modifiers,
        }
    }
}
