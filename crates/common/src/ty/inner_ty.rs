use std::hash::Hash;

use crate::{
    error::LangResult,
    hash::{DerefType, TyEnvHash},
    path::{LangPath, LangPathPart},
    BlockId, UniqueId,
};

use super::{generics::Generics, ty::SolveCond, ty_env::TyEnv};

#[derive(Debug, Clone)]
pub enum InnerTy {
    Struct(LangPath),
    Enum(LangPath),
    Union(LangPath),
    Trait(LangPath),
    Tuple(LangPath),

    Void,
    Character,
    String, // TODO: String type (?)
    Boolean,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    F32,
    I64,
    U64,
    F64,
    I128,
    U128,

    /// This is used during the type inference stage. Every expr must have a type,
    /// so this is used for expr that doesn't have a known type.
    Unknown(UniqueId),

    /// This is used during the type inference stage. This type will be given to
    /// found identifier type. This can be ex. the name of a struct/interface/enum.
    /// The block id is used to find the structure in the correct scope.
    UnknownIdent(LangPath, BlockId),

    /// A int type used during type inferece. It is known that it is of a int
    /// type, but the bit size isn't known. The `u32` is the radix.
    UnknownInt(UniqueId, u32),

    /// A float type used during type inferece. It is known that it is of a float
    /// type, but the bit size isn't known.
    UnknownFloat(UniqueId),
}

#[allow(clippy::match_like_matches_macro)]
impl InnerTy {
    pub fn is_solved(&self, solve_cond: SolveCond) -> bool {
        self.is_primitive()
            || self.is_string()
            || self.is_adt()
            || self.is_trait()
            || (solve_cond.can_solve_default()
                && matches!(self, InnerTy::UnknownInt(..) | InnerTy::UnknownFloat(..)))
    }

    pub fn get_ident(&self) -> Option<LangPath> {
        match self {
            InnerTy::Struct(ident)
            | InnerTy::Enum(ident)
            | InnerTy::Union(ident)
            | InnerTy::Trait(ident)
            | InnerTy::Tuple(ident)
            | InnerTy::UnknownIdent(ident, ..) => Some(ident.clone()),
            _ => None,
        }
    }

    pub fn get_ident_ref(&self) -> Option<&LangPath> {
        match self {
            InnerTy::Struct(ident)
            | InnerTy::Enum(ident)
            | InnerTy::Union(ident)
            | InnerTy::Trait(ident)
            | InnerTy::Tuple(ident)
            | InnerTy::UnknownIdent(ident, ..) => Some(ident),
            _ => None,
        }
    }

    pub fn get_ident_mut(&mut self) -> Option<&mut LangPath> {
        match self {
            InnerTy::Struct(ident)
            | InnerTy::Enum(ident)
            | InnerTy::Union(ident)
            | InnerTy::Trait(ident)
            | InnerTy::Tuple(ident)
            | InnerTy::UnknownIdent(ident, ..) => Some(ident),
            _ => None,
        }
    }

    pub fn get_primitive_ident(&self) -> &'static str {
        match self {
            InnerTy::Character => "char",
            InnerTy::Boolean => "bool",
            InnerTy::I8 => "i8",
            InnerTy::U8 => "u8",
            InnerTy::I16 => "i16",
            InnerTy::U16 => "u16",
            InnerTy::I32 => "i32",
            InnerTy::U32 => "u32",
            InnerTy::F32 => "f32",
            InnerTy::I64 => "i64",
            InnerTy::U64 => "u64",
            InnerTy::F64 => "f64",
            InnerTy::I128 => "i128",
            InnerTy::U128 => "u128",
            InnerTy::UnknownIdent(path, ..) | InnerTy::Struct(path) if path.count() == 1 => {
                let ident = path.last().unwrap().name();
                let inner_ty = InnerTy::ident_to_type(ident, 0);
                if inner_ty.is_primitive() {
                    inner_ty.get_primitive_ident()
                } else {
                    unreachable!("{:#?}", self)
                }
            }
            _ => unreachable!("{:#?}", self),
        }
    }

    pub fn gens(&self) -> Option<&Generics> {
        if let Some(ident) = self.get_ident_ref() {
            ident.last().map(|part| part.1.as_ref()).flatten()
        } else {
            None
        }
    }

    pub fn gens_mut(&mut self) -> Option<&mut Generics> {
        if let Some(ident) = self.get_ident_mut() {
            ident.last_mut().map(|part| part.1.as_mut()).flatten()
        } else {
            None
        }
    }

    pub fn default_int() -> InnerTy {
        InnerTy::I32
    }

    pub fn default_float() -> InnerTy {
        InnerTy::F32
    }

    // TODO: Converting from `&str` to `LangPath` doesn't work for generics.
    //       Is this ok?
    pub fn ident_to_type(s: &str, id: BlockId) -> Self {
        match s {
            "void" => InnerTy::Void,
            "char" => InnerTy::Character,
            "BuiltInString" => InnerTy::String,
            "bool" => InnerTy::Boolean,
            "i8" => InnerTy::I8,
            "u8" => InnerTy::U8,
            "i16" => InnerTy::I16,
            "u16" => InnerTy::U16,
            "i32" => InnerTy::I32,
            "u32" => InnerTy::U32,
            "f32" => InnerTy::F32,
            "i64" => InnerTy::I64,
            "u64" => InnerTy::U64,
            "f64" => InnerTy::F64,
            "i128" => InnerTy::I128,
            "u128" => InnerTy::U128,
            _ => {
                let parts = s
                    .split("::")
                    .map(|s| LangPathPart(s.to_string(), None))
                    .collect::<Vec<_>>();
                InnerTy::UnknownIdent(parts.into(), id)
            }
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            InnerTy::I8
            | InnerTy::U8
            | InnerTy::I16
            | InnerTy::U16
            | InnerTy::I32
            | InnerTy::U32
            | InnerTy::I64
            | InnerTy::U64
            | InnerTy::I128
            | InnerTy::U128
            | InnerTy::UnknownInt(..) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            InnerTy::F32 | InnerTy::F64 | InnerTy::UnknownFloat(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        if let InnerTy::Boolean = self {
            true
        } else {
            false
        }
    }

    pub fn is_char(&self) -> bool {
        if let InnerTy::Character = self {
            true
        } else {
            false
        }
    }

    pub fn is_string(&self) -> bool {
        if let InnerTy::String = self {
            true
        } else {
            false
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            InnerTy::Void
            | InnerTy::Character
            | InnerTy::String
            | InnerTy::Boolean
            | InnerTy::I8
            | InnerTy::U8
            | InnerTy::I16
            | InnerTy::U16
            | InnerTy::I32
            | InnerTy::U32
            | InnerTy::F32
            | InnerTy::I64
            | InnerTy::U64
            | InnerTy::F64
            | InnerTy::I128
            | InnerTy::U128 => true,
            InnerTy::UnknownIdent(path, ..) | InnerTy::Struct(path) => {
                let ident = path.last().unwrap().name();
                let inner_ty = InnerTy::ident_to_type(ident, 0);
                path.count() == 1
                    && !inner_ty.is_adt()
                    && !inner_ty.is_unknown_ident()
                    && inner_ty.is_primitive()
            }
            _ => false,
        }
    }

    /// Checks if this InnerTy is a ADT i.e. a struct, enum or union.
    /// It does NOT check if this is a UnknownIdent.
    pub fn is_adt(&self) -> bool {
        matches!(
            self,
            InnerTy::Struct(_) | InnerTy::Enum(_) | InnerTy::Union(_) | InnerTy::Tuple(_)
        )
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, InnerTy::Enum(_))
    }

    pub fn is_trait(&self) -> bool {
        matches!(self, InnerTy::Trait(..))
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self, InnerTy::Tuple(_))
    }

    pub fn is_unknown(&self) -> bool {
        if let InnerTy::Unknown(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_ident(&self) -> bool {
        if let InnerTy::UnknownIdent(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_int(&self) -> bool {
        if let InnerTy::UnknownInt(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_unknown_float(&self) -> bool {
        if let InnerTy::UnknownFloat(..) = self {
            true
        } else {
            false
        }
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            InnerTy::I8 | InnerTy::I16 | InnerTy::I32 | InnerTy::I64 | InnerTy::I128
        )
    }

    pub fn contains_inner_ty_shallow(&self, inner_ty: &InnerTy) -> bool {
        match (self, inner_ty) {
            (InnerTy::Struct(_), InnerTy::Struct(_))
            | (InnerTy::Enum(_), InnerTy::Enum(_))
            | (InnerTy::Union(_), InnerTy::Union(_))
            | (InnerTy::Trait(_), InnerTy::Trait(_))
            | (InnerTy::Tuple(_), InnerTy::Tuple(_))
            | (InnerTy::Void, InnerTy::Void)
            | (InnerTy::Character, InnerTy::Character)
            | (InnerTy::String, InnerTy::String)
            | (InnerTy::Boolean, InnerTy::Boolean)
            | (InnerTy::I8, InnerTy::I8)
            | (InnerTy::U8, InnerTy::U8)
            | (InnerTy::I16, InnerTy::I16)
            | (InnerTy::U16, InnerTy::U16)
            | (InnerTy::I32, InnerTy::I32)
            | (InnerTy::U32, InnerTy::U32)
            | (InnerTy::F32, InnerTy::F32)
            | (InnerTy::I64, InnerTy::I64)
            | (InnerTy::U64, InnerTy::U64)
            | (InnerTy::F64, InnerTy::F64)
            | (InnerTy::I128, InnerTy::I128)
            | (InnerTy::U128, InnerTy::U128)
            | (InnerTy::Unknown(_), InnerTy::Unknown(_))
            | (InnerTy::UnknownIdent(..), InnerTy::UnknownIdent(..))
            | (InnerTy::UnknownInt(_, _), InnerTy::UnknownInt(_, _))
            | (InnerTy::UnknownFloat(_), InnerTy::UnknownFloat(_)) => true,
            _ => false,
        }
    }
}

impl TyEnvHash for InnerTy {
    fn hash_with_state<H: std::hash::Hasher>(
        &self,
        ty_env: &TyEnv,
        deref_type: DerefType,
        state: &mut H,
    ) -> LangResult<()> {
        match self {
            InnerTy::Struct(path) => {
                0.hash(state);
                path.hash_with_state(ty_env, deref_type, state)?;
            }
            InnerTy::Enum(path) => {
                1.hash(state);
                path.hash_with_state(ty_env, deref_type, state)?;
            }
            InnerTy::Union(path) => {
                2.hash(state);
                path.hash_with_state(ty_env, deref_type, state)?;
            }
            InnerTy::Trait(path) => {
                3.hash(state);
                path.hash_with_state(ty_env, deref_type, state)?;
            }
            InnerTy::Tuple(path) => {
                4.hash(state);
                path.hash_with_state(ty_env, deref_type, state)?;
            }
            InnerTy::Void => 5.hash(state),
            InnerTy::Character => 6.hash(state),
            InnerTy::String => 7.hash(state),
            InnerTy::Boolean => 8.hash(state),
            InnerTy::I8 => 9.hash(state),
            InnerTy::U8 => 10.hash(state),
            InnerTy::I16 => 11.hash(state),
            InnerTy::U16 => 12.hash(state),
            InnerTy::I32 => 13.hash(state),
            InnerTy::U32 => 14.hash(state),
            InnerTy::F32 => 15.hash(state),
            InnerTy::I64 => 16.hash(state),
            InnerTy::U64 => 17.hash(state),
            InnerTy::F64 => 18.hash(state),
            InnerTy::I128 => 19.hash(state),
            InnerTy::U128 => 20.hash(state),
            InnerTy::Unknown(unique_id) => {
                21.hash(state);
                unique_id.hash(state);
            }
            InnerTy::UnknownIdent(path, block_id) => {
                22.hash(state);
                path.hash_with_state(ty_env, deref_type, state)?;
                block_id.hash(state);
            }
            InnerTy::UnknownInt(unique_id, _) => {
                23.hash(state);
                unique_id.hash(state);
            }
            InnerTy::UnknownFloat(unique_id) => {
                24.hash(state);
                unique_id.hash(state);
            }
        }
        Ok(())
    }
}
