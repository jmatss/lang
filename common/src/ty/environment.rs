use std::collections::{hash_map::Entry, HashMap, HashSet};

use either::Either;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    path::{LangPath, LangPathPart},
    token::expr::Expr,
    type_info::TypeInfo,
    TypeId,
};

use super::{generics::Generics, inner_ty::InnerTy, substitution_sets::SubstitutionSets, ty::Ty};

/// A struct containing all types. All code outside of this environment will
/// only store references to types (TypeId). To access the actual type, they
/// will have to fetch it from this environment.
///
/// Handling all types in this environment allows for easy updating of types
/// during type inference. The same type can be used in multiple places in the
/// AST and they all have to be updated/synchronized when the type is inferred to
/// a new type. This environment handles that synchronization and the AST only
/// contains references to the type in this environment, ensuring that the type
/// only needs to be updated in one place and is always up-to-date.
#[derive(Debug)]
pub struct TypeEnvironment {
    id_to_ty: HashMap<TypeId, Ty>,
    ty_to_id: HashMap<Ty, TypeId>,

    /// A counter that is used to create new `TypeId`s. It will be increment
    /// for every new type created. The type ID 0 is used for special purposes,
    /// so this value will always start counting from 1.
    type_id: u64,
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnvironment {
    pub fn new() -> Self {
        Self {
            id_to_ty: HashMap::default(),
            ty_to_id: HashMap::default(),
            type_id: 1,
        }
    }

    /// Inserts a new type `ty` into this environment and return a new TypeId
    /// that refers to that type. If the type already exists in this environment,
    /// a errir will be returned.
    pub fn new_ty(&mut self, ty: Ty) -> LangResult<TypeId> {
        if !self.ty_to_id.contains_key(&ty) {
            let id = TypeId(self.type_id);
            self.type_id += 1;

            self.ty_to_id.insert(ty.clone(), id);
            self.id_to_ty.insert(id, ty);

            Ok(id)
        } else {
            Err(LangError::new(
                format!(
                    "Tried to create new type that already exists in environment: {:#?}",
                    ty
                ),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    pub fn all_types(&self) -> HashSet<TypeId> {
        self.id_to_ty.keys().cloned().collect::<HashSet<_>>()
    }

    pub fn to_string(&self, id: TypeId) -> LangResult<String> {
        let mut result = String::new();

        match self.ty(id)? {
            Ty::CompoundType(inner_ty, gens, ..) => {
                result.push_str(&inner_ty.to_string());
                if !gens.is_empty() {
                    let ids = gens.iter_types().cloned().collect::<Vec<_>>();
                    result.push('<');
                    result.push_str(&self.list_to_string(&ids)?);
                    result.push('>');
                }
            }

            Ty::Pointer(type_id, ..) => {
                result.push('{');
                result.push_str(&self.to_string(*type_id)?);
                result.push('}');
            }

            Ty::Fn(gen_type_ids, params, ret_type_id_opt, ..) => {
                result.push_str("fn");

                if !gen_type_ids.is_empty() {
                    result.push('<');
                    result.push_str(&self.list_to_string(&gen_type_ids)?);
                    result.push('>');
                }

                result.push('(');
                if !params.is_empty() {
                    result.push_str(&self.list_to_string(&params)?);
                }
                result.push(')');

                if let Some(ret_type_id) = ret_type_id_opt {
                    result.push_str("->");
                    result.push_str(&self.to_string(*ret_type_id)?);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    result.push_str(&self.to_string(type_id)?);
                } else {
                    panic!("Expr type None.");
                }
            }

            Ty::Array(inner_ty, dim_opt, ..) => {
                panic!("TODO: `to_string` for array type.")
                /*
                result.push('[');
                result.push_str(&ty.to_string());

                if let Some(dim) = dim_opt {
                    result.push(':');
                    result.push_str(&dim.as_ref().to_string());
                }
                result.push(']');
                */
            }

            _ => {
                return Err(LangError::new(
                    format!(
                        "Invalid type when calling `to_string` on type: {:?}",
                        self.ty(id)?
                    ),
                    LangErrorKind::GeneralError,
                    None,
                ))
            }
        }

        Ok(result)
    }

    pub fn list_to_string(&self, ids: &[TypeId]) -> LangResult<String> {
        let mut result_vec = Vec::default();
        for type_id in ids {
            result_vec.push(self.to_string(*type_id)?);
        }

        let mut result = String::new();
        result.push_str(&result_vec.join(","));
        Ok(result)
    }

    pub fn gens_to_string(&self, ids: &[TypeId]) -> LangResult<String> {
        let mut result = String::new();

        result.push('<');
        result.push_str(&self.list_to_string(ids)?);
        result.push('>');

        Ok(result)
    }

    pub fn to_string_debug(&self, id: TypeId) -> LangResult<String> {
        let mut result = String::new();

        match self.ty(id)? {
            Ty::CompoundType(inner_ty, gens, ..) => {
                result.push_str(&inner_ty.to_string_debug());
                if !gens.is_empty() {
                    let ids = gens.iter_types().cloned().collect::<Vec<_>>();
                    result.push('<');
                    result.push_str(&self.list_to_string_debug(&ids)?);
                    result.push('>');
                }
            }

            Ty::Pointer(type_id, ..) => {
                result.push('{');
                result.push_str(&self.to_string_debug(*type_id)?);
                result.push('}');
            }

            Ty::Fn(gen_type_ids, params, ret_type_id_opt, ..) => {
                result.push_str("fn");

                if !gen_type_ids.is_empty() {
                    result.push('<');
                    result.push_str(&self.list_to_string_debug(&gen_type_ids)?);
                    result.push('>');
                }

                result.push('(');
                if !params.is_empty() {
                    result.push_str(&self.list_to_string_debug(&params)?);
                }
                result.push(')');

                if let Some(ret_type_id) = ret_type_id_opt {
                    result.push_str("->");
                    result.push_str(&self.to_string_debug(*ret_type_id)?);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    result.push_str(&self.to_string_debug(type_id)?);
                } else {
                    panic!("Expr type None.");
                }
            }

            Ty::Array(type_id_i, dim_opt, ..) => {
                result.push('[');
                result.push_str(&self.to_string_debug(*type_id_i)?);
                result.push(']');

                // TODO: Dimension.
                /*
                if let Some(dim) = dim_opt {
                    result.push(':');
                    result.push_str(&dim.as_ref().to_string());
                }
                */
            }

            Ty::Any(_) => result.push_str("any"),
            Ty::Generic(gen_ident, ..) | Ty::GenericInstance(gen_ident, ..) => {
                result.push_str("gen(");
                result.push_str(&gen_ident);
                result.push(')')
            }
            Ty::UnknownAdtMember(adt_type_id, member_name, ..) => {
                result.push_str("adtMember(");
                result.push_str(&self.to_string_debug(*adt_type_id)?);
                result.push('.');
                result.push_str(member_name);
                result.push(')');
            }
            Ty::UnknownAdtMethod(adt_type_id, method_name, ..) => {
                result.push_str("adtMethod(");
                result.push_str(&self.to_string_debug(*adt_type_id)?);
                result.push('.');
                result.push_str(method_name);
                result.push(')');
            }
            Ty::UnknownMethodArgument(adt_type_id, method_name, _, name_or_idx, ..) => {
                result.push_str("adtMethodArg(");
                result.push_str(&self.to_string_debug(*adt_type_id)?);
                result.push('.');
                result.push_str(method_name);
                result.push('.');
                match name_or_idx {
                    Either::Left(name) => result.push_str(name),
                    Either::Right(idx) => result.push_str(&idx.to_string()),
                }
                result.push_str(method_name);
                result.push(')');
            }
            Ty::UnknownMethodGeneric(adt_type_id, method_name, name_or_idx, ..) => {
                result.push_str("adtMethodGen(");
                result.push_str(&self.to_string_debug(*adt_type_id)?);
                result.push('.');
                result.push_str(method_name);
                result.push('.');
                match name_or_idx {
                    Either::Left(idx) => result.push_str(&idx.to_string()),
                    Either::Right(name) => result.push_str(name),
                }
                result.push_str(method_name);
                result.push(')');
            }
            Ty::UnknownArrayMember(arr_type_id, ..) => {
                result.push_str("arr(");
                result.push_str(&self.to_string_debug(*arr_type_id)?);
                result.push(')')
            }
        }

        Ok(result)
    }

    pub fn list_to_string_debug(&self, ids: &[TypeId]) -> LangResult<String> {
        let mut result_vec = Vec::default();
        for type_id in ids {
            result_vec.push(self.to_string_debug(*type_id)?);
        }

        let mut result = String::new();
        result.push_str(&result_vec.join(","));
        Ok(result)
    }

    pub fn gens_to_string_debug(&self, ids: &[TypeId]) -> LangResult<String> {
        let mut result = String::new();

        result.push('<');
        result.push_str(&self.list_to_string_debug(ids)?);
        result.push('>');

        Ok(result)
    }

    /// Creates a new unknown identifier that will be given to a unkown type.
    /// The new string will containg information about the position of the type
    /// in a file and a free text to give the unknown type some more context for
    /// readability.
    pub fn new_unknown_ident(&mut self, text: &str) -> TypeId {
        /*
        let file_nr = self.type_context.analyze_context.file_pos.file_nr;
        let line_nr = self.type_context.analyze_context.file_pos.line_start;
        let column_nr = self.type_context.analyze_context.file_pos.column_start;
        let offset = self.type_context.analyze_context.file_pos.offset;
        let length = self.type_context.analyze_context.file_pos.length;

        let type_ident = format!(
            "ID:{}-F:{}-R:{}-C:{}-O:{}-L:{}-{}",
            self.type_id, file_nr, line_nr, column_nr, offset, length, text
        );
        self.type_id += 1;
        type_ident
        */
        TypeId(123)
    }

    /// Given a type `ty`, gets the TypeId that refers to the type.
    /// If this environment already have "seen" the type `ty`, the reference to
    /// that type is returned. If `ty` is a new type that haven't been "seen"
    /// before, a new TypeId will be assigned to it and returned.
    pub fn id(&mut self, ty: &Ty) -> LangResult<TypeId> {
        if let Some(id) = self.ty_to_id.get(ty) {
            Ok(*id)
        } else {
            self.new_ty(ty.clone())
        }
    }

    /// Given a type `ty`, gets the TypeId that refers to the type.
    /// If `ty` is a new type that haven't been "seen" before, error is returned.
    pub fn id_try(&self, ty: &Ty) -> LangResult<TypeId> {
        if let Some(id) = self.ty_to_id.get(ty) {
            Ok(*id)
        } else {
            Err(LangError::new(
                format!("Unable to find type in environment: {:#?}", ty),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Given a type ID `id`, gets the type that the `id` refers to.
    /// If `id` doesn't refer to a type, a error will be returned.
    pub fn ty(&self, id: TypeId) -> LangResult<&Ty> {
        if let Some(ty) = self.id_to_ty.get(&id) {
            Ok(ty)
        } else {
            Err(LangError::new(
                format!("Unable to find type with type ID {}.", id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Given a type ID `id`, gets the type that the `id` refers to as mutable.
    /// If `id` doesn't refer to a type, a error will be returned.
    pub fn ty_mut(&mut self, id: TypeId) -> LangResult<&mut Ty> {
        if let Some(ty) = self.id_to_ty.get_mut(&id) {
            Ok(ty)
        } else {
            Err(LangError::new(
                format!("Unable to find type with type ID {}.", id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    /// Updates the the type with type ID `id` to the type `ty` and returns
    /// the old type. If no type with ID `id` exists in the environment, a error
    /// is returned.
    pub fn update(&mut self, id: TypeId, ty: Ty) -> LangResult<Ty> {
        match self.id_to_ty.entry(id) {
            Entry::Occupied(mut o) => Ok(o.insert(ty)),
            Entry::Vacant(_) => Err(LangError::new(
                format!(
                    "Unable to find type with type ID {} when updating to type: {:#?}.",
                    id, ty
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }

    /// Checks if the type referenced by the type ID `id` only contains solved types.
    /// Solved types includes:
    ///  * Primitives
    ///  * ADTs
    ///  * Traits
    ///  * Pointers containing solved types
    ///  * Arrays containing solved types
    ///  * Unknown ints (default)
    ///  * Unknown floats (default)
    ///
    /// It first checks if the given `type_id` is solved. If that is not the case,
    /// it checks its inferred type for solvability as well (unless `type_id` is
    /// the preferred type in its set).
    pub fn is_solved(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        type_id: TypeId,
    ) -> LangResult<bool> {
        let is_solved = match self.ty(type_id)? {
            Ty::CompoundType(inner_ty, generics, ..) => {
                let inner_solved = inner_ty.is_solved();
                let mut gens_solved = true;
                for type_id in generics.iter_types() {
                    if !self.is_solved(sub_sets, *type_id)? {
                        gens_solved = false;
                    }
                }
                inner_solved && gens_solved
            }

            Ty::Pointer(type_id_i, ..) => self.is_solved(sub_sets, *type_id_i)?,

            Ty::Array(type_id_i, expr_opt, ..) => {
                let ty_solved = self.is_solved(sub_sets, *type_id_i)?;
                let expr_ty_solved = if let Some(expr_type_id) = expr_opt
                    .as_ref()
                    .map(|expr| expr.get_expr_type().ok())
                    .flatten()
                {
                    self.is_solved(sub_sets, expr_type_id)?
                } else {
                    true
                };
                ty_solved && expr_ty_solved
            }

            Ty::Fn(gens, params, ret_ty_id, ..) => {
                let ty_solved = if let Some(ret_ty_id) = ret_ty_id {
                    self.is_solved(sub_sets, *ret_ty_id)?
                } else {
                    true
                };
                let mut gens_solved = true;
                for gen_type_id in gens {
                    if !self.is_solved(sub_sets, *gen_type_id)? {
                        gens_solved = false;
                    }
                }
                let mut params_solved = true;
                for param_type_id in params {
                    if !self.is_solved(sub_sets, *param_type_id)? {
                        params_solved = false;
                    }
                }
                ty_solved && gens_solved && params_solved
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    self.is_solved(sub_sets, type_id_i)?
                } else {
                    true
                }
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => true,

            Ty::UnknownAdtMember(..)
            | Ty::UnknownAdtMethod(..)
            | Ty::UnknownMethodArgument(..)
            | Ty::UnknownMethodGeneric(..)
            | Ty::UnknownArrayMember(..) => false,
        };

        let inf_type_id = if let Some(sub_sets) = sub_sets {
            sub_sets.inferred_type(type_id)?
        } else {
            type_id
        };

        warn!(
            "in_solved: {}, type_id: {}, inf_type_id: {}",
            is_solved, type_id, inf_type_id
        );

        if is_solved || type_id == inf_type_id {
            Ok(is_solved)
        } else {
            self.is_solved(sub_sets, inf_type_id)
        }
    }

    /// Converts any unknown values to their corresponding "default" values
    /// if possible. This includes ints and floats that are converted to i32
    /// and f32 respectively.
    pub fn convert_defaults(&mut self, id: TypeId) -> LangResult<()> {
        match self.ty(id)?.clone() {
            Ty::CompoundType(inner_ty, generics, ..) => {
                if inner_ty.is_unknown_int() || inner_ty.is_unknown_float() {
                    self.replace_defaults(id)?;
                }

                for gen_type_id in generics.iter_types() {
                    self.convert_defaults(*gen_type_id)?;
                }
            }

            Ty::Array(type_id, expr_opt, ..) => {
                self.convert_defaults(type_id)?;

                if let Some(expr) = expr_opt {
                    if let Ok(expr_type_id) = expr.get_expr_type() {
                        self.convert_defaults(expr_type_id)?;
                    }
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.convert_defaults(ret_type_id)?;
                }
                for gen_type_id in gens {
                    self.convert_defaults(gen_type_id)?;
                }
                for param_type_id in params {
                    self.convert_defaults(param_type_id)?;
                }
            }

            Ty::Expr(expr, _) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.convert_defaults(expr_type_id)?;
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.convert_defaults(type_id)?;
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(())
    }

    /// When this function is called, the type with ID `id` is known to be a
    /// compound type containing either a int or float unknown.
    fn replace_defaults(&mut self, id: TypeId) -> LangResult<()> {
        if let Ty::CompoundType(inner_ty, ..) = self.ty_mut(id)? {
            if inner_ty.is_unknown_int() {
                *inner_ty = InnerTy::default_int();
            } else if inner_ty.is_unknown_float() {
                *inner_ty = InnerTy::default_float();
            }
        }
        Ok(())
    }

    pub fn get_inner(&self, id: TypeId) -> LangResult<&InnerTy> {
        if let Ty::CompoundType(inner_ty, ..) = self.ty(id)? {
            Ok(inner_ty)
        } else {
            Err(LangError::new(
                format!("Type with ID {} not a CompoundType.", id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    pub fn get_inner_mut(&mut self, id: TypeId) -> LangResult<&mut InnerTy> {
        if let Ty::CompoundType(inner_ty, ..) = self.ty_mut(id)? {
            Ok(inner_ty)
        } else {
            Err(LangError::new(
                format!("Type with ID {} not a CompoundType.", id),
                LangErrorKind::GeneralError,
                None,
            ))
        }
    }

    pub fn get_generic_ident(&self, id: TypeId) -> LangResult<&str> {
        match self.ty(id)? {
            Ty::Generic(ident, ..) | Ty::GenericInstance(ident, ..) => Ok(ident),
            _ => Err(LangError::new(
                format!(
                    "Type with ID {} not a Generic or GenericInstance: {:#?}",
                    id,
                    self.ty(id)?
                ),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }

    /// Returns the identifier if this type represents a structure or a generic.
    /// If this type isn't a structure or generic, None is returned.
    pub fn get_ident(&self, id: TypeId) -> LangResult<Option<LangPath>> {
        match self.ty(id)? {
            Ty::CompoundType(inner_ty, ..) => Ok(inner_ty.get_ident()),
            _ => Err(LangError::new(
                format!("Type with ID {} not a CompoundType.", id),
                LangErrorKind::GeneralError,
                None,
            )),
        }
    }

    pub fn file_pos(&self, id: TypeId) -> Option<&FilePosition> {
        match self.type_info(id)? {
            TypeInfo::Default(file_pos)
            | TypeInfo::VarUse(file_pos)
            | TypeInfo::BuiltInCall(file_pos)
            | TypeInfo::VarDecl(file_pos, _)
            | TypeInfo::FuncCall(file_pos)
            | TypeInfo::Enum(file_pos)
            | TypeInfo::EnumMember(_, (_, file_pos))
            | TypeInfo::Generic(file_pos) => Some(file_pos),

            TypeInfo::Lit(file_pos_opt) | TypeInfo::DefaultOpt(file_pos_opt) => {
                file_pos_opt.as_ref()
            }

            TypeInfo::None | TypeInfo::BuiltIn => None,
        }
    }

    pub fn file_pos_mut(&mut self, id: TypeId) -> Option<&mut FilePosition> {
        match self.type_info_mut(id)? {
            TypeInfo::Default(file_pos)
            | TypeInfo::VarUse(file_pos)
            | TypeInfo::BuiltInCall(file_pos)
            | TypeInfo::VarDecl(file_pos, _)
            | TypeInfo::FuncCall(file_pos)
            | TypeInfo::Enum(file_pos)
            | TypeInfo::EnumMember(_, (_, file_pos))
            | TypeInfo::Generic(file_pos) => Some(file_pos),

            TypeInfo::Lit(file_pos_opt) | TypeInfo::DefaultOpt(file_pos_opt) => {
                file_pos_opt.as_mut()
            }

            TypeInfo::None | TypeInfo::BuiltIn => None,
        }
    }

    pub fn type_info(&self, id: TypeId) -> Option<&TypeInfo> {
        match self.ty(id).ok()? {
            Ty::CompoundType(.., type_info)
            | Ty::Pointer(.., type_info)
            | Ty::Array(.., type_info)
            | Ty::Fn(.., type_info)
            | Ty::Any(.., type_info)
            | Ty::Generic(.., type_info)
            | Ty::GenericInstance(.., type_info)
            | Ty::Expr(.., type_info)
            | Ty::UnknownAdtMember(.., type_info)
            | Ty::UnknownAdtMethod(.., type_info)
            | Ty::UnknownMethodArgument(.., type_info)
            | Ty::UnknownMethodGeneric(.., type_info)
            | Ty::UnknownArrayMember(.., type_info) => Some(type_info),
        }
    }

    pub fn type_info_mut(&mut self, id: TypeId) -> Option<&mut TypeInfo> {
        match self.ty_mut(id).ok()? {
            Ty::CompoundType(.., type_info)
            | Ty::Pointer(.., type_info)
            | Ty::Array(.., type_info)
            | Ty::Fn(.., type_info)
            | Ty::Any(.., type_info)
            | Ty::Generic(.., type_info)
            | Ty::GenericInstance(.., type_info)
            | Ty::Expr(.., type_info)
            | Ty::UnknownAdtMember(.., type_info)
            | Ty::UnknownAdtMethod(.., type_info)
            | Ty::UnknownMethodArgument(.., type_info)
            | Ty::UnknownMethodGeneric(.., type_info)
            | Ty::UnknownArrayMember(.., type_info) => Some(type_info),
        }
    }

    // TODO: This only fetched a expression if it is the outer most type.
    //       How should expression in ex. generics be handled? Should this
    //       return a iterator or a list?
    // TODO: Rewrite this is a safe way. Temporary hack to get something to work.
    pub fn get_exprs_mut(&mut self, id: TypeId) -> LangResult<Vec<&mut Expr>> {
        let mut exprs = Vec::default();

        match self.ty(id)?.clone() {
            Ty::Expr(..) => {
                exprs.push(self.get_exprs_mut_priv(id)?);
            }

            Ty::Array(type_id, dim_expr_opt, ..) => {
                let inner_exprs = self.get_exprs_mut(type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }

                if dim_expr_opt.is_some() {
                    exprs.push(self.get_exprs_mut_priv(id)?);
                }
            }

            Ty::CompoundType(_, gens, _) => {
                for gen_type_id in gens.iter_types() {
                    let inner_exprs = self.get_exprs_mut(*gen_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let inner_exprs = self.get_exprs_mut(type_id)?;
                for inner_expr in inner_exprs {
                    exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                }
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let inner_exprs = self.get_exprs_mut(ret_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }

                for gen_type_id in gens.iter() {
                    let inner_exprs = self.get_exprs_mut(*gen_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }

                for param_type_id in params.iter() {
                    let inner_exprs = self.get_exprs_mut(*param_type_id)?;
                    for inner_expr in inner_exprs {
                        exprs.push(unsafe { (inner_expr as *mut Expr).as_mut().unwrap() });
                    }
                }
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(exprs)
    }

    /// This function is called when the type with ID `id` is known to contain
    /// a expression. This is true for: Ty::Expr & Ty::Array.
    fn get_exprs_mut_priv(&mut self, id: TypeId) -> LangResult<&mut Expr> {
        match self.ty_mut(id)? {
            Ty::Expr(expr, ..) | Ty::Array(_, Some(expr), ..) => unsafe {
                Ok((expr.as_mut() as *mut Expr).as_mut().unwrap())
            },
            _ => Err(LangError::new(
                format!(
                    "Unable to get mutable exprs from type: {:#?}",
                    self.ty_mut(id)?
                ),
                LangErrorKind::AnalyzeError,
                None,
            )),
        }
    }

    /// Recursively replaces any generic identifiers from "UnknownIdent" wrapped
    /// inside a "CompoundType" into "Generic"s.
    pub fn replace_generics(&mut self, id: TypeId, generics: &Generics) -> LangResult<()> {
        match self.ty(id)?.clone() {
            Ty::CompoundType(InnerTy::UnknownIdent(path, ..), gens, type_info) => {
                for gen_type_id in gens.iter_types() {
                    self.replace_generics(*gen_type_id, generics)?;
                }

                if path.count() == 1 {
                    let possible_gen_name = path.first().unwrap().name();
                    for gen_name in generics.iter_names() {
                        if gen_name == possible_gen_name {
                            let new_ty = Ty::Generic(possible_gen_name.into(), type_info.clone());
                            self.update(id, new_ty)?;
                        }
                    }
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.replace_generics(type_id, generics)?;
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.replace_generics(ret_type_id, generics)?;
                }
                for gen_type_id in gens.iter() {
                    self.replace_generics(*gen_type_id, generics)?;
                }
                for param_type_id in params.iter() {
                    self.replace_generics(*param_type_id, generics)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.replace_generics(type_id, generics)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    /// Recursively replaces any "Generic" types with the actual implementation
    /// type. I.e. any "Generic" type that has a ident that is a key in the
    /// `generics_impl` will be replaced with the value in the map.
    ///
    /// The types given in `generics_impl` will be weighted agains the inferred
    /// type in the "Ty::Generic(ident, ty)" `ty` type, and the type with the
    /// highest precedence will be used.
    pub fn replace_generics_impl(
        &mut self,
        id: TypeId,
        generics_impl: &Generics,
    ) -> LangResult<()> {
        match self.ty(id)?.clone() {
            Ty::Generic(ident, ..) | Ty::GenericInstance(ident, ..) => {
                // TODO: Will this specific generic always be found in the
                //       `generics_impl` map?
                if let Some(impl_ty) = generics_impl.get(&ident) {
                    let new_ty = self.ty(impl_ty)?.clone();
                    self.update(id, new_ty)?;
                }
            }

            Ty::CompoundType(_, generics, ..) => {
                for gen_type_id in generics.iter_types() {
                    self.replace_generics_impl(*gen_type_id, generics_impl)?;
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.replace_generics_impl(type_id, generics_impl)?;
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.replace_generics_impl(ret_type_id, generics_impl)?;
                }
                for gen_type_id in gens.iter() {
                    self.replace_generics_impl(*gen_type_id, generics_impl)?;
                }
                for param_type_id in params.iter() {
                    self.replace_generics_impl(*param_type_id, generics_impl)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.replace_generics_impl(type_id, generics_impl)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    /// Gets a vector of all "Generic" types that is contained in the given
    /// type `self`.
    pub fn get_generics(&self, id: TypeId) -> LangResult<Vec<TypeId>> {
        let mut generics = Vec::default();

        match self.ty(id)? {
            Ty::Generic(..) => {
                generics.push(id);
            }

            Ty::CompoundType(_, gens, _) => {
                for gen_type_id in gens.iter_types() {
                    let mut inner_generics = self.get_generics(*gen_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let mut inner_generics = self.get_generics(*type_id)?;
                generics.append(&mut inner_generics);
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let mut inner_generics = self.get_generics(*ret_type_id)?;
                    generics.append(&mut inner_generics);
                }
                for gen_type_id in gens {
                    let mut inner_generics = self.get_generics(*gen_type_id)?;
                    generics.append(&mut inner_generics);
                }
                for param_type_id in params {
                    let mut inner_generics = self.get_generics(*param_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    let mut inner_generics = self.get_generics(expr_type_id)?;
                    generics.append(&mut inner_generics);
                }
            }

            _ => (),
        }

        Ok(generics)
    }

    /// Gets a vector of all unsolvable types that is contained in the given
    /// type `self`.
    pub fn get_unsolvable(&self, id: TypeId) -> LangResult<Vec<TypeId>> {
        let mut unsolvable = Vec::default();

        match self.ty(id)? {
            Ty::CompoundType(inner_ty, gens, _) => {
                if !inner_ty.is_solved() {
                    unsolvable.push(id);
                }
                for gen_type_id in gens.iter_types() {
                    let mut inner_unsolvables = self.get_unsolvable(*gen_type_id)?;
                    unsolvable.append(&mut inner_unsolvables);
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let mut inner_unsolvables = self.get_unsolvable(*type_id)?;
                unsolvable.append(&mut inner_unsolvables);
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let mut inner_unsolvables = self.get_unsolvable(*ret_type_id)?;
                    unsolvable.append(&mut inner_unsolvables);
                }
                for gen_type_id in gens {
                    let mut inner_unsolvables = self.get_unsolvable(*gen_type_id)?;
                    unsolvable.append(&mut inner_unsolvables);
                }
                for param_type_id in params {
                    let mut inner_unsolvables = self.get_unsolvable(*param_type_id)?;
                    unsolvable.append(&mut inner_unsolvables);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    let mut inner_unsolvables = self.get_unsolvable(type_id)?;
                    unsolvable.append(&mut inner_unsolvables);
                }
            }

            Ty::Any(..) | Ty::Generic(..) | Ty::GenericInstance(..) => (),
        }

        Ok(unsolvable)
    }

    /// Gets a set of the paths of all ADTs/traits that is contained in the type.
    /// Set `full_paths` to true to fetch the names as full names, set to false
    /// to just return the names without generics.
    pub fn get_adt_and_trait_paths(
        &self,
        id: TypeId,
        full_paths: bool,
    ) -> LangResult<HashSet<LangPath>> {
        let mut paths = HashSet::default();

        match self.ty(id)? {
            Ty::CompoundType(inner_ty, gens, _) => {
                for gen_type_id in gens.iter_types() {
                    let inner_paths = self.get_adt_and_trait_paths(*gen_type_id, full_paths)?;
                    paths.extend(inner_paths.into_iter());
                }

                let generics_opt = if gens.len_types() > 0 {
                    Some(gens.clone())
                } else {
                    None
                };

                match inner_ty {
                    InnerTy::Struct(path)
                    | InnerTy::Enum(path)
                    | InnerTy::Union(path)
                    | InnerTy::Trait(path)
                    | InnerTy::UnknownIdent(path, ..) => {
                        let mut path_clone = path.clone();
                        let last_part = path_clone.pop().unwrap();
                        if full_paths {
                            path_clone.push(LangPathPart(last_part.0, generics_opt));
                        } else {
                            path_clone.push(LangPathPart(last_part.0, None));
                        }

                        paths.insert(path_clone);
                    }
                    _ => (),
                }
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                let inner_paths = self.get_adt_and_trait_paths(*type_id, full_paths)?;
                paths.extend(inner_paths.into_iter());
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let inner_paths = self.get_adt_and_trait_paths(*ret_type_id, full_paths)?;
                    paths.extend(inner_paths.into_iter());
                }
                for gen_type_id in gens {
                    let inner_paths = self.get_adt_and_trait_paths(*gen_type_id, full_paths)?;
                    paths.extend(inner_paths);
                }
                for param_type_id in params {
                    let inner_paths = self.get_adt_and_trait_paths(*param_type_id, full_paths)?;
                    paths.extend(inner_paths);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    let inner_paths = self.get_adt_and_trait_paths(type_id, full_paths)?;
                    paths.extend(inner_paths.into_iter());
                }
            }

            _ => (),
        }

        Ok(paths)
    }

    /// Recursively replaces any structure types with idents that matches the
    /// old structure name. These will be replaced with the new type with the
    /// generics "replaced"/"implemented".
    pub fn replace_self(
        &mut self,
        id: TypeId,
        old_path: &LangPath,
        new_self_ty: &Ty,
    ) -> LangResult<()> {
        match self.ty(id)?.clone() {
            Ty::CompoundType(inner_ty, ..) => match inner_ty {
                InnerTy::Struct(path)
                | InnerTy::Enum(path)
                | InnerTy::Union(path)
                | InnerTy::Trait(path)
                | InnerTy::UnknownIdent(path, ..) => {
                    if &path == old_path {
                        self.update(id, new_self_ty.clone())?;
                    }
                }
                _ => (),
            },

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.replace_self(type_id, old_path, new_self_ty)?;
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.replace_self(ret_type_id, old_path, new_self_ty)?;
                }
                for gen_type_id in gens.iter() {
                    self.replace_self(*gen_type_id, old_path, new_self_ty)?;
                }
                for param_type_id in params.iter() {
                    self.replace_self(*param_type_id, old_path, new_self_ty)?;
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.replace_self(type_id, old_path, new_self_ty)?;
                }
            }

            _ => (),
        }

        Ok(())
    }

    pub fn is_int(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_int())
            .unwrap_or(false))
    }

    pub fn is_float(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_float())
            .unwrap_or(false))
    }

    pub fn is_bool(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_bool())
            .unwrap_or(false))
    }

    pub fn is_char(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_char())
            .unwrap_or(false))
    }

    pub fn is_string(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_string())
            .unwrap_or(false))
    }

    pub fn is_primitive(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_primitive())
            .unwrap_or(false))
    }

    pub fn is_unknown(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown())
            .unwrap_or(false))
    }

    pub fn is_unknown_ident(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown_ident())
            .unwrap_or(false))
    }

    pub fn is_unknown_int(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown_int())
            .unwrap_or(false))
    }

    pub fn is_unknown_float(&self, id: TypeId) -> LangResult<bool> {
        Ok(self
            .get_inner(id)
            .map(|inner_ty| inner_ty.is_unknown_float())
            .unwrap_or(false))
    }

    pub fn is_aggregate(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::CompoundType(..)))
    }

    pub fn is_pointer(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Pointer(..)))
    }

    pub fn is_array(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Array(..)))
    }

    pub fn is_fn(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Fn(..)))
    }

    pub fn is_expr(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Expr(..)))
    }

    pub fn is_any(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Any(..)))
    }

    pub fn is_generic(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::Generic(..) | Ty::GenericInstance(..)))
    }

    pub fn is_unknown_adt_member(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownAdtMember(..)))
    }

    pub fn is_unknown_adt_method(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownAdtMethod(..)))
    }

    pub fn is_unknown_method_argument(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownMethodArgument(..)))
    }

    pub fn is_unknown_method_generic(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownMethodGeneric(..)))
    }

    pub fn is_unknown_array_member(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(matches!(ty, Ty::UnknownArrayMember(..)))
    }

    pub fn is_unknown_any(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(match ty {
            Ty::UnknownAdtMember(..)
            | Ty::UnknownAdtMethod(..)
            | Ty::UnknownMethodArgument(..)
            | Ty::UnknownMethodGeneric(..)
            | Ty::UnknownArrayMember(..) => true,
            Ty::CompoundType(inner_ty, ..) => {
                inner_ty.is_unknown()
                    || inner_ty.is_unknown_ident()
                    || inner_ty.is_unknown_int()
                    || inner_ty.is_unknown_float()
            }
            _ => false,
        })
    }

    /// Checks if the given type is signed. This returns true if this is a signed
    /// integer, returns false for every other type (includingn non-int types).
    pub fn is_signed(&self, id: TypeId) -> LangResult<bool> {
        let ty = self.ty(id)?;
        Ok(match ty {
            Ty::CompoundType(inner_ty, ..) => matches!(
                inner_ty,
                InnerTy::I8 | InnerTy::I16 | InnerTy::I32 | InnerTy::I64 | InnerTy::I128
            ),
            _ => false,
        })
    }

    /// Given a type ID `type_id`, returns all "nested" type IDs that it contains.
    /// This will include type IDs for any inferred types as well.
    pub fn nested_type_ids(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        type_id: TypeId,
    ) -> LangResult<HashSet<TypeId>> {
        let mut all_nested_type_ids = HashSet::default();

        match self.ty(type_id)? {
            Ty::CompoundType(_, gens, ..) => {
                for gen_type_id in gens.iter_types() {
                    let nested_type_ids = self.nested_type_ids(sub_sets, *gen_type_id)?;
                    all_nested_type_ids.insert(*gen_type_id);
                    all_nested_type_ids.extend(nested_type_ids);
                }
            }

            Ty::Pointer(type_id_i, ..)
            | Ty::Array(type_id_i, ..)
            | Ty::UnknownAdtMember(type_id_i, ..)
            | Ty::UnknownAdtMethod(type_id_i, ..)
            | Ty::UnknownMethodArgument(type_id_i, ..)
            | Ty::UnknownMethodGeneric(type_id_i, ..)
            | Ty::UnknownArrayMember(type_id_i, ..) => {
                let nested_type_ids = self.nested_type_ids(sub_sets, *type_id_i)?;
                all_nested_type_ids.insert(*type_id_i);
                all_nested_type_ids.extend(nested_type_ids);
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    let nested_type_ids = self.nested_type_ids(sub_sets, *ret_type_id)?;
                    all_nested_type_ids.insert(*ret_type_id);
                    all_nested_type_ids.extend(nested_type_ids);
                }
                for type_id_i in gens.iter().chain(params.iter()) {
                    let nested_type_ids = self.nested_type_ids(sub_sets, *type_id_i)?;
                    all_nested_type_ids.insert(*type_id_i);
                    all_nested_type_ids.extend(nested_type_ids);
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id_i) = expr.get_expr_type() {
                    let nested_type_ids = self.nested_type_ids(sub_sets, type_id_i)?;
                    all_nested_type_ids.insert(type_id_i);
                    all_nested_type_ids.extend(nested_type_ids);
                }
            }

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => (),
        }

        if let Some(sub_sets) = sub_sets {
            let inf_type_id = sub_sets.inferred_type(type_id)?;
            if type_id != inf_type_id {
                let nested_type_ids = self.nested_type_ids(Some(sub_sets), inf_type_id)?;
                all_nested_type_ids.insert(inf_type_id);
                all_nested_type_ids.extend(nested_type_ids);
            }
        }

        Ok(all_nested_type_ids)
    }

    /// Checks if the type with ID `child_id` can be found in type with ID `parent_id`.
    /// This is a "deep" check i.e. the "contents" of the type matters as well,
    /// NOT only that the "type" of the type. This function will only return true
    /// if at any recursive call `parent_id` == `child_id`.
    pub fn contains_ty_deep(&self, parent_id: TypeId, child_id: TypeId) -> LangResult<bool> {
        if parent_id == child_id {
            return Ok(true);
        }

        Ok(match self.ty(parent_id)? {
            Ty::CompoundType(_, gens, _) => {
                let mut contains = false;
                for type_id in gens.iter_types() {
                    if self.contains_ty_deep(*type_id, child_id)? {
                        contains = true;
                        break;
                    }
                }
                contains
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => self.contains_ty_deep(*type_id, child_id)?,

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.contains_ty_deep(type_id, child_id)?
                } else {
                    false
                }
            }

            _ => false,
        })
    }

    /// Checks if the type with ID `child_id` can be found in type with ID `parent_id`.
    /// This is a "shallow" check i.e. it doesn't check the contents of the type,
    /// only that the "type" of the type is correct.
    ///
    /// # Example
    ///
    /// If this function is called with:
    ///
    ///  `parent_id` = CompoundType("abc")
    ///   &
    ///  `child_id` = CompoundType("xyz")
    ///
    /// the function would return true even though "abc" != "xyz".
    pub fn contains_ty_shallow(&self, parent_id: TypeId, child_id: TypeId) -> LangResult<bool> {
        let parent_ty = self.ty(parent_id)?;
        let child_ty = self.ty(child_id)?;

        match (parent_ty, child_ty) {
            (Ty::CompoundType(..), Ty::CompoundType(..))
            | (Ty::Pointer(..), Ty::Pointer(..))
            | (Ty::Array(..), Ty::Array(..))
            | (Ty::Fn(..), Ty::Fn(..))
            | (Ty::Expr(..), Ty::Expr(..))
            | (Ty::Generic(..), Ty::Generic(..))
            | (Ty::Any(..), Ty::Any(..))
            | (Ty::GenericInstance(..), Ty::GenericInstance(..))
            | (Ty::UnknownAdtMember(..), Ty::UnknownAdtMember(..))
            | (Ty::UnknownAdtMethod(..), Ty::UnknownAdtMethod(..))
            | (Ty::UnknownMethodArgument(..), Ty::UnknownMethodArgument(..))
            | (Ty::UnknownMethodGeneric(..), Ty::UnknownMethodGeneric(..))
            | (Ty::UnknownArrayMember(..), Ty::UnknownArrayMember(..)) => return Ok(true),
            _ => (),
        }

        Ok(match parent_ty {
            Ty::CompoundType(_, gens, _) => {
                let mut contains = false;
                for type_id in gens.iter_types() {
                    if self.contains_ty_shallow(*type_id, child_id)? {
                        contains = true;
                        break;
                    }
                }
                contains
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.contains_ty_shallow(*type_id, child_id)?
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.contains_ty_shallow(type_id, child_id)?
                } else {
                    false
                }
            }

            _ => false,
        })
    }

    fn contains_inner_ty_shallow(&self, id: TypeId, inner_ty: &InnerTy) -> LangResult<bool> {
        match self.ty(id)? {
            Ty::CompoundType(cur_inner_ty, gens, _) => {
                if cur_inner_ty.contains_inner_ty_shallow(inner_ty) {
                    return Ok(true);
                }

                for gen_type_id in gens.iter_types() {
                    if self.contains_inner_ty_shallow(*gen_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }

                Ok(false)
            }

            Ty::Pointer(type_id, ..)
            | Ty::Array(type_id, ..)
            | Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..)
            | Ty::UnknownArrayMember(type_id, ..) => {
                self.contains_inner_ty_shallow(*type_id, inner_ty)
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                if let Some(ret_type_id) = ret_type_id_opt {
                    if self.contains_inner_ty_shallow(*ret_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }
                for gen_type_id in gens {
                    if self.contains_inner_ty_shallow(*gen_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }
                for param_type_id in params {
                    if self.contains_inner_ty_shallow(*param_type_id, inner_ty)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    self.contains_inner_ty_shallow(type_id, inner_ty)
                } else {
                    Ok(false)
                }
            }

            Ty::Generic(..) | Ty::GenericInstance(..) | Ty::Any(..) => Ok(false),
        }
    }

    pub fn contains_generic_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let gen_type_id = self.id(&Ty::Generic("".into(), TypeInfo::None))?;
        let gen_inst_type_id =
            self.id(&Ty::GenericInstance("".into(), "".into(), TypeInfo::None))?;
        Ok(self.contains_ty_shallow(id, gen_type_id)?
            || self.contains_ty_shallow(id, gen_inst_type_id)?)
    }

    pub fn contains_any_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let any_type_id = self.id(&Ty::Any(TypeInfo::None))?;
        self.contains_ty_shallow(id, any_type_id)
    }

    pub fn contains_unknown_any_shallow(&mut self, id: TypeId) -> LangResult<bool> {
        let unknown_adt_member = self.id(&Ty::UnknownAdtMember(
            TypeId(0),
            "".into(),
            "".into(),
            TypeInfo::None,
        ))?;
        let unknown_adt_method = self.id(&Ty::UnknownAdtMethod(
            TypeId(0),
            "".into(),
            Vec::with_capacity(0),
            "".into(),
            TypeInfo::None,
        ))?;
        let unknown_method_argument = self.id(&Ty::UnknownMethodArgument(
            TypeId(0),
            "".into(),
            Vec::with_capacity(0),
            Either::Right(0),
            "".into(),
            TypeInfo::None,
        ))?;
        let unknown_method_generic = self.id(&Ty::UnknownMethodGeneric(
            TypeId(0),
            "".into(),
            Either::Left(0),
            "".into(),
            TypeInfo::None,
        ))?;
        let unknown_array_member = self.id(&Ty::UnknownArrayMember(
            TypeId(0),
            "".into(),
            TypeInfo::None,
        ))?;

        Ok(self.contains_unknown_inner_shallow(id)?
            || self.contains_unknown_ident_shallow(id)?
            || self.contains_unknown_int_shallow(id)?
            || self.contains_unknown_float_shallow(id)?
            || self.contains_ty_shallow(id, unknown_adt_member)?
            || self.contains_ty_shallow(id, unknown_adt_method)?
            || self.contains_ty_shallow(id, unknown_method_argument)?
            || self.contains_ty_shallow(id, unknown_method_generic)?
            || self.contains_ty_shallow(id, unknown_array_member)?)
    }

    pub fn contains_unknown_inner_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::Unknown("".into()))
    }

    pub fn contains_unknown_int_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::UnknownInt("".into(), 0))
    }

    pub fn contains_unknown_float_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::UnknownFloat("".into()))
    }

    pub fn contains_unknown_ident_shallow(&self, id: TypeId) -> LangResult<bool> {
        self.contains_inner_ty_shallow(id, &InnerTy::UnknownIdent(LangPath::empty(), 0))
    }

    pub fn assert_compatible(&self, first_id: TypeId, second_id: TypeId) -> LangResult<()> {
        if self.is_compatible(first_id, second_id)? {
            Ok(())
        } else {
            let first_ty = self.ty(first_id)?;
            let second_ty = self.ty(second_id)?;
            Err(LangError::new(
                format!(
                    "Tried to map incompatible types.\nFirst:\n{:#?}\nSecond:\n{:#?}",
                    first_ty, second_ty
                ),
                LangErrorKind::AnalyzeError,
                None,
            ))
        }
    }

    // TODO: Currently aggregated types with different inner types will return
    //       true. Should this be the case?
    pub fn is_compatible(&self, first_id: TypeId, second_id: TypeId) -> LangResult<bool> {
        // Handles all cases with "Unknown" types.
        if self.is_generic(first_id)?
            || self.is_generic(second_id)?
            || self.is_any(first_id)?
            || self.is_any(second_id)?
        {
            return Ok(true);
        } else if self.is_unknown_int(first_id)? {
            if self.is_unknown_float(second_id)? || self.is_unknown_ident(second_id)? {
                return Ok(false);
            } else if self.is_int(second_id)? || self.is_unknown_any(second_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_int(second_id)? {
            if self.is_unknown_float(first_id)? || self.is_unknown_ident(first_id)? {
                return Ok(false);
            } else if self.is_int(first_id)? || self.is_unknown_any(first_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_float(first_id)? {
            if self.is_unknown_int(second_id)? || self.is_unknown_ident(second_id)? {
                return Ok(false);
            } else if self.is_float(second_id)? || self.is_unknown_any(second_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_float(second_id)? {
            if self.is_unknown_int(first_id)? || self.is_unknown_ident(first_id)? {
                return Ok(false);
            } else if self.is_float(first_id)? || self.is_unknown_any(first_id)? {
                return Ok(true);
            }
        } else if self.is_unknown_any(first_id)? || self.is_unknown_any(second_id)? {
            return Ok(true);
        } else if self.is_string(first_id)? || self.is_string(second_id)? {
            // Add support for compatibility with string and {u8}. This is what
            // the string will be compiled down to for now, but should be changed
            // to a custom String struct later.
            // This allows for easy interop with C string during development.
            match (self.ty(first_id)?, self.ty(second_id)?) {
                (Ty::Pointer(ptr_type_id, ..), _) | (_, Ty::Pointer(ptr_type_id, ..)) => {
                    if let Ty::CompoundType(InnerTy::U8, ..) = self.ty(*ptr_type_id)? {
                        return Ok(true);
                    }
                }
                _ => (),
            }
        }

        // Handles all cases regarding types that isn't "Unknown" or generic.
        match (self.ty(first_id)?, self.ty(second_id)?) {
            (Ty::Pointer(inner_a, ..), Ty::Pointer(inner_b, ..))
            | (Ty::Array(inner_a, ..), Ty::Array(inner_b, ..)) => {
                self.is_compatible(*inner_a, *inner_b)
            }

            (Ty::CompoundType(comp_a, gens_a, ..), Ty::CompoundType(comp_b, gens_b, ..)) => {
                Ok(comp_a == comp_b && gens_a.len() == gens_b.len())
            }

            (
                Ty::Fn(gens_a, params_a, ret_type_id_opt_a, ..),
                Ty::Fn(gens_b, params_b, ret_type_id_opt_b, ..),
            ) => {
                match (ret_type_id_opt_a, ret_type_id_opt_b) {
                    (Some(ret_type_id_a), Some(ret_type_id_b)) => {
                        return self.is_compatible(*ret_type_id_a, *ret_type_id_b);
                    }
                    (None, Some(_)) => return Ok(false),
                    (Some(_), None) => return Ok(false),
                    (None, None) => (),
                }

                if gens_a.len() != gens_b.len() {
                    return Ok(false);
                }
                for (gen_type_id_a, gen_type_id_b) in gens_a.iter().zip(gens_b) {
                    if !self.is_compatible(*gen_type_id_a, *gen_type_id_b)? {
                        return Ok(false);
                    }
                }

                if params_a.len() != params_b.len() {
                    return Ok(false);
                }
                for (param_type_id_a, param_type_id_b) in params_a.iter().zip(params_b) {
                    if !self.is_compatible(*param_type_id_a, *param_type_id_b)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }

            (Ty::Expr(expr, ..), other_ty) | (other_ty, Ty::Expr(expr, ..)) => {
                let other_type_id = self.id_try(other_ty)?;
                if let Ok(expr_type_id) = expr.get_expr_type() {
                    self.is_compatible(other_type_id, expr_type_id)
                } else {
                    Ok(false)
                }
            }

            _ => Ok(false),
        }
    }

    /// Checks the precedence for the two given types when it comes to mapping
    /// during type inference. If this function return true, it means that
    /// `first_id` has precedence and that `second_id` should be mapped to
    /// `first_id` during type inference.
    pub fn precedence(
        &self,
        sub_sets: Option<&SubstitutionSets>,
        first_id: TypeId,
        second_id: TypeId,
    ) -> LangResult<bool> {
        // How many "layers" down the search will go when look at nested types.
        // After this number of iterations it will give up and return the current
        // highest precedence number.
        static MAX_DEPTH: usize = 10;

        let first_contains_second = {
            info!(
                "before first contains. first_id: {}, second_id: {}",
                first_id, second_id
            );
            let nested_type_ids = self.nested_type_ids(sub_sets, first_id)?;
            warn!("nested_type_ids: {:#?}", nested_type_ids);
            info!(
                "after first contains. first_id: {}, second_id: {}",
                first_id, second_id
            );
            nested_type_ids.contains(&second_id)
        };
        if first_contains_second {
            return Ok(false);
        }

        let second_contains_first = {
            let nested_type_ids = self.nested_type_ids(sub_sets, second_id)?;
            nested_type_ids.contains(&first_id)
        };
        if second_contains_first {
            return Ok(true);
        }

        // OBS! `is_solved()` checks inferred type(s) while the `contains_...()`
        //      functions does NOT.
        let first_is_solved = self.is_solved(sub_sets, first_id)?;
        let second_is_solved = self.is_solved(sub_sets, second_id)?;
        let first_contains_unknown_primitive = self.contains_unknown_int_shallow(first_id)?
            || self.contains_unknown_float_shallow(first_id)?;
        let second_contains_unknown_primitive = self.contains_unknown_int_shallow(second_id)?
            || self.contains_unknown_float_shallow(second_id)?;

        if first_is_solved && !second_is_solved && !first_contains_unknown_primitive {
            return Ok(true);
        } else if !first_is_solved && second_is_solved && !second_contains_unknown_primitive {
            return Ok(false);
        }

        // TODO: Makes this in a more effective way so that one doesn't have to
        //       re-calculate the "1..i" every iteration.
        // Start by just looking at the outer most types and then interatively
        // look deeper to find differences.
        for i in 1..=MAX_DEPTH {
            let first_prec = self.precedence_priv(first_id, 0, i, false)?;
            let second_prec = self.precedence_priv(second_id, 0, i, false)?;

            if first_prec != second_prec {
                return Ok(first_prec < second_prec);
            }
        }

        // Give up and default to giving `first_id` precedence.
        Ok(true)
    }

    /// Returns a number indicating the precedence for the type when it comes
    /// to the mapping of types during type inference. A lower number indicates
    /// that this type is strongly prefered while a higner number is the opposite.
    /// So the type with the higher number should be mapped to the the with the
    /// lower number.
    ///
    /// The precedence number that is returned is the type that is found in the
    /// given `id` recursively with the highest precedence. This includes the
    /// generics.
    ///
    /// If the "highest precedence type" is NOT found in the generic parameter
    /// list, it will get the number incremented by one. This means that if two
    /// types contains have the same "highest type", the one with the highest
    /// type in the generic parameter list is prefered.
    ///
    /// Examples:
    ///   i32<UnknownIdent>       =>  4
    ///   ExampleStruct<i32, T>   =>  12 (where T is generic)
    ///
    /// Precedence:
    ///   0   contains primitive
    ///   2   contains ADT or trait (struct/enum/union/trait)
    ///   4   contains unknown ADT (UnknownIdent)
    ///       contains unknown ADT member (UnknownAdtMember)
    ///       contains unknown ADT method (UnknownAdtMethod)
    ///       contains unknown ADT method argument (UnknownMethodArgument)
    ///       contains unknown ADT method generic (UnknownMethodGeneric)
    ///   6   contains pointer
    ///       contains array
    ///       contains fn
    ///   8   contains unknown array member (UnknownArrayMember)
    ///   10  contains generic (Generic)
    ///   12  contains unknown int (UnknownInt)
    ///       contains unknown float (UnknownFloat)
    ///   14  contains generic implementation (GenericImpl)
    ///   16  contains unknown (Unknown)
    ///   18  contains any (Any)
    fn precedence_priv(
        &self,
        id: TypeId,
        mut highest: usize,
        depth: usize,
        is_generic_param: bool,
    ) -> LangResult<usize> {
        if depth == 0 {
            return Ok(highest);
        }

        let next_depth = depth - 1;

        // Add a extra precedence point if this type was NOT found in the generic
        // parameter list. This ensures that if two types have the same highest
        // "precedence type", if one of them has that type in the generic list,
        // that type will be prefered.
        let extra = if is_generic_param { 0 } else { 1 };

        Ok(match self.ty(id)? {
            Ty::CompoundType(inner_ty, generics, ..) => {
                for gen_type_id in generics.iter_types() {
                    // The generics are treated as being at the same depth as
                    // the current type.
                    highest = self.precedence_priv(*gen_type_id, highest, depth, true)?;
                }

                if inner_ty.is_primitive() {
                    usize::max(highest, extra)
                } else if inner_ty.is_adt() || inner_ty.is_trait() {
                    usize::max(highest, 2 + extra)
                } else {
                    match inner_ty {
                        InnerTy::UnknownIdent(..) => usize::max(highest, 4 + extra),
                        InnerTy::UnknownInt(..) | InnerTy::UnknownFloat(_) => {
                            usize::max(highest, 10 + extra)
                        }
                        InnerTy::Unknown(..) => usize::max(highest, 16 + extra),
                        _ => unreachable!("All other inner types already \"matched\"."),
                    }
                }
            }

            Ty::Pointer(type_id, ..) | Ty::Array(type_id, ..) => {
                highest = usize::max(highest, 6 + extra);
                self.precedence_priv(*type_id, highest, next_depth, is_generic_param)?
            }

            Ty::Fn(gens, params, ret_type_id_opt, ..) => {
                highest = usize::max(highest, 6 + extra);
                for gen_type_id in gens.iter().chain(params) {
                    highest = usize::max(
                        highest,
                        self.precedence_priv(*gen_type_id, highest, next_depth, is_generic_param)?,
                    );
                }
                if let Some(ret_type_id) = ret_type_id_opt {
                    self.precedence_priv(*ret_type_id, highest, next_depth, is_generic_param)?
                } else {
                    highest
                }
            }

            Ty::Expr(expr, ..) => {
                if let Ok(type_id) = expr.get_expr_type() {
                    // Use the same depth.
                    self.precedence_priv(type_id, highest, depth, is_generic_param)?
                } else {
                    highest
                }
            }

            Ty::GenericInstance(..) => usize::max(highest, 14 + extra),
            Ty::Generic(..) => usize::max(highest, 10 + extra),
            Ty::Any(..) => usize::max(highest, 18 + extra),

            Ty::UnknownAdtMember(type_id, ..)
            | Ty::UnknownAdtMethod(type_id, ..)
            | Ty::UnknownMethodArgument(type_id, ..)
            | Ty::UnknownMethodGeneric(type_id, ..) => {
                highest = usize::max(highest, 4 + extra);
                self.precedence_priv(*type_id, highest, next_depth, is_generic_param)?
            }

            Ty::UnknownArrayMember(type_id, ..) => {
                highest = usize::max(highest, 8 + extra);
                self.precedence_priv(*type_id, highest, next_depth, is_generic_param)?
            }
        })
    }
}
