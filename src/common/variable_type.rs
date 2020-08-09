use crate::error::CustomError::CodeGenError;
use crate::{parse::token::Expression, CustomResult};
use inkwell::{context::Context, types::BasicTypeEnum, AddressSpace};

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

    pub fn to_codegen<'ctx>(
        &self,
        gen_context: &'ctx Context,
    ) -> CustomResult<BasicTypeEnum<'ctx>> {
        // TODO: What AddressSpace should be used?
        let address_space = AddressSpace::Global;

        Ok(match self {
            Type::Pointer(ptr) => {
                // Get the type of the inner type and wrap into a "PointerType".
                match ptr.to_codegen(gen_context)? {
                    BasicTypeEnum::ArrayType(ty) => ty.ptr_type(address_space).into(),
                    BasicTypeEnum::FloatType(ty) => ty.ptr_type(address_space).into(),
                    BasicTypeEnum::IntType(ty) => ty.ptr_type(address_space).into(),
                    BasicTypeEnum::PointerType(ty) => ty.ptr_type(address_space).into(),
                    BasicTypeEnum::StructType(ty) => ty.ptr_type(address_space).into(),
                    BasicTypeEnum::VectorType(ty) => ty.ptr_type(address_space).into(),
                }
            }
            Type::Array(t, dim_opt) => {
                // TODO: Can fetch the inner type and call "array_type()" on it,
                //       but the function takes a "u32" as argument, so need to
                //       convert the "dim_opt" Expression into a u32 if possible.
                return Err(CodeGenError(
                    "TODO: Array. Need to calculate dimension and the return a \"ArrayType\""
                        .into(),
                ));
            }
            Type::Void => return Err(CodeGenError("TODO: Void".into())),
            Type::Character => BasicTypeEnum::IntType(gen_context.i32_type()),
            Type::String => return Err(CodeGenError("TODO: String".into())),
            Type::Boolean => BasicTypeEnum::IntType(gen_context.bool_type()),
            Type::Int => BasicTypeEnum::IntType(gen_context.i32_type()),
            Type::Uint => BasicTypeEnum::IntType(gen_context.i32_type()),
            Type::Float => BasicTypeEnum::FloatType(gen_context.f32_type()),
            Type::I8 => BasicTypeEnum::IntType(gen_context.i8_type()),
            Type::U8 => BasicTypeEnum::IntType(gen_context.i8_type()),
            Type::I16 => BasicTypeEnum::IntType(gen_context.i16_type()),
            Type::U16 => BasicTypeEnum::IntType(gen_context.i16_type()),
            Type::I32 => BasicTypeEnum::IntType(gen_context.i32_type()),
            Type::U32 => BasicTypeEnum::IntType(gen_context.i32_type()),
            Type::F32 => BasicTypeEnum::FloatType(gen_context.f32_type()),
            Type::I64 => BasicTypeEnum::IntType(gen_context.i64_type()),
            Type::U64 => BasicTypeEnum::IntType(gen_context.i64_type()),
            Type::F64 => BasicTypeEnum::FloatType(gen_context.f64_type()),
            Type::I128 => BasicTypeEnum::IntType(gen_context.i128_type()),
            Type::U128 => BasicTypeEnum::IntType(gen_context.i128_type()),
            Type::Unknown(s) => return Err(CodeGenError(format!("Unknown type: {}", s))),
        })
    }
}
