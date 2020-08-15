use crate::error::{LangError, LangErrorKind::CodeGenError};
use crate::{parse::token::Expression, CustomResult};
use inkwell::{context::Context, types::AnyTypeEnum, AddressSpace};

// TODO: Make Int & Uint unbounded? But then what about pointer sized int?

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Pointer(Box<Type>),
    // The Option in the "Array" enum indicates the size. If it is None, assume
    // size is unknown (probably slice).
    Array(Box<Type>, Option<Box<Expression>>),
    Void,
    Character,
    String, // TODO: String type (?)
    Boolean,
    Int,
    Uint,
    Float,
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
    Unknown(String),
}

impl Type {
    pub fn ident_to_type(s: &str) -> Self {
        match s {
            "void" => Type::Void,
            "char" => Type::Character,
            "String" => Type::String,
            "bool" => Type::Boolean,
            "int" => Type::Int,
            "uint" => Type::Uint,
            "float" => Type::Float,
            "i8" => Type::I8,
            "u8" => Type::U8,
            "i16" => Type::I16,
            "u16" => Type::U16,
            "i32" => Type::I32,
            "u32" => Type::U32,
            "f32" => Type::F32,
            "i64" => Type::I64,
            "u64" => Type::U64,
            "f64" => Type::F64,
            "i128" => Type::I128,
            "u128" => Type::U128,
            _ => Type::Unknown(s.to_string()),
        }
    }

    pub fn to_codegen<'ctx>(&self, gen_context: &'ctx Context) -> CustomResult<AnyTypeEnum<'ctx>> {
        // TODO: What AddressSpace should be used?
        let address_space = AddressSpace::Global;

        Ok(match self {
            Type::Pointer(ptr) => {
                // Get the type of the inner type and wrap into a "PointerType".
                match ptr.to_codegen(gen_context)? {
                    AnyTypeEnum::ArrayType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::FloatType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::FunctionType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::IntType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::PointerType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::StructType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::VectorType(ty) => ty.ptr_type(address_space).into(),
                    AnyTypeEnum::VoidType(_) => {
                        // TODO: FIXME: Is this OK? Can't use pointer to void, use
                        //              poniter to a igeneric" I8 instead.
                        gen_context.i8_type().ptr_type(address_space).into()
                    }
                }
            }
            Type::Array(t, dim_opt) => {
                // TODO: Can fetch the inner type and call "array_type()" on it,
                //       but the function takes a "u32" as argument, so need to
                //       convert the "dim_opt" Expression into a u32 if possible.
                return Err(LangError::new(
                    "TODO: Array. Need to calculate dimension and the return a \"ArrayType\""
                        .into(),
                    CodeGenError,
                ));
            }
            Type::Void => AnyTypeEnum::VoidType(gen_context.void_type()),
            Type::Character => AnyTypeEnum::IntType(gen_context.i32_type()),
            // TODO: What type should the string be?
            Type::String => AnyTypeEnum::PointerType(gen_context.i8_type().ptr_type(address_space)),
            Type::Boolean => AnyTypeEnum::IntType(gen_context.bool_type()),
            Type::Int => AnyTypeEnum::IntType(gen_context.i32_type()),
            Type::Uint => AnyTypeEnum::IntType(gen_context.i32_type()),
            Type::Float => AnyTypeEnum::FloatType(gen_context.f32_type()),
            Type::I8 => AnyTypeEnum::IntType(gen_context.i8_type()),
            Type::U8 => AnyTypeEnum::IntType(gen_context.i8_type()),
            Type::I16 => AnyTypeEnum::IntType(gen_context.i16_type()),
            Type::U16 => AnyTypeEnum::IntType(gen_context.i16_type()),
            Type::I32 => AnyTypeEnum::IntType(gen_context.i32_type()),
            Type::U32 => AnyTypeEnum::IntType(gen_context.i32_type()),
            Type::F32 => AnyTypeEnum::FloatType(gen_context.f32_type()),
            Type::I64 => AnyTypeEnum::IntType(gen_context.i64_type()),
            Type::U64 => AnyTypeEnum::IntType(gen_context.i64_type()),
            Type::F64 => AnyTypeEnum::FloatType(gen_context.f64_type()),
            Type::I128 => AnyTypeEnum::IntType(gen_context.i128_type()),
            Type::U128 => AnyTypeEnum::IntType(gen_context.i128_type()),
            Type::Unknown(s) => {
                return Err(LangError::new(format!("Unknown type: {}", s), CodeGenError))
            }
        })
    }
}
