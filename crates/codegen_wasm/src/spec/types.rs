use super::{
    util::{to_vec, to_vec_raw},
    Bytes, UnSignedLeb128,
};

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum NumType {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum RefType {
    FuncRef = 0x70,
    ExternRef = 0x6f,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum Mut {
    Const = 0x00,
    Var = 0x01,
}

pub enum ValType {
    NumType(NumType),
    RefType(RefType),
}

impl Bytes for ValType {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            ValType::NumType(num_type) => vec![*num_type as u8],
            ValType::RefType(ref_type) => vec![*ref_type as u8],
        }
    }
}

pub struct ResultType(Vec<ValType>);

impl Bytes for ResultType {
    fn to_bytes(&self) -> Vec<u8> {
        to_vec(&self.0)
    }
}

// First ResultType is parameters, second is return type.
pub struct FuncType(ResultType, ResultType);

impl Bytes for FuncType {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = vec![0x60];
        buf.extend_from_slice(&self.0.to_bytes());
        buf.extend_from_slice(&self.1.to_bytes());
        buf
    }
}

// First is min, second is optional max.
pub struct Limits(u32, Option<u32>);

impl Bytes for Limits {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::default();
        let min = self.0;
        if let Some(max) = self.1 {
            buf.push(0x01);
            buf.extend_from_slice(&min.to_leb128());
            buf.extend_from_slice(&max.to_leb128());
        } else {
            buf.push(0x00);
            buf.extend_from_slice(&min.to_leb128());
        }
        buf
    }
}

pub struct TableType(RefType, Limits);

impl Bytes for TableType {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = vec![self.0 as u8];
        buf.extend_from_slice(&self.1.to_bytes());
        buf
    }
}

pub struct MemType(Limits);

impl Bytes for MemType {
    fn to_bytes(&self) -> Vec<u8> {
        self.0.to_bytes()
    }
}

pub struct GlobalType(ValType, Mut);

impl Bytes for GlobalType {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = self.0.to_bytes();
        buf.push(self.1 as u8);
        buf
    }
}

pub struct Name(String);

impl Bytes for Name {
    fn to_bytes(&self) -> Vec<u8> {
        to_vec_raw(self.0.as_bytes())
    }
}

pub enum BlockType {
    EmptyType,
    ValType(ValType),
    TypeIdx(TypeIdx),
}

impl Bytes for BlockType {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            BlockType::EmptyType => vec![0x40],
            BlockType::ValType(val_type) => val_type.to_bytes(),
            BlockType::TypeIdx(type_idx) => type_idx.to_bytes(),
        }
    }
}

pub struct MemArg {
    align: u32,
    offset: u32,
}

impl Bytes for MemArg {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::default();
        buf.extend_from_slice(&self.align.to_leb128());
        buf.extend_from_slice(&self.offset.to_leb128());
        buf
    }
}

// TODO: Macros
pub struct TypeIdx(u32);
pub struct FuncIdx(u32);
pub struct TableIdx(u32);
pub struct MemIdx(u32);
pub struct GlobalIdx(u32);
pub struct ElemIdx(u32);
pub struct DataIdx(u32);
pub struct LocalIdx(u32);
pub struct LabelIdx(u32);

macro_rules! impl_leb128_trait {
    ($t:ty) => {
        impl UnSignedLeb128 for $t {
            fn to_leb128(&self) -> Vec<u8> {
                let mut buf = Vec::default();
                leb128::write::unsigned(&mut buf, (*self).0 as u64).unwrap();
                buf
            }
        }

        impl Bytes for $t {
            fn to_bytes(&self) -> Vec<u8> {
                self.to_leb128()
            }
        }
    };
}

impl_leb128_trait!(TypeIdx);
impl_leb128_trait!(FuncIdx);
impl_leb128_trait!(TableIdx);
impl_leb128_trait!(MemIdx);
impl_leb128_trait!(GlobalIdx);
impl_leb128_trait!(ElemIdx);
impl_leb128_trait!(DataIdx);
impl_leb128_trait!(LocalIdx);
impl_leb128_trait!(LabelIdx);
