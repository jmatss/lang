use crate::spec::util::to_vec_raw;

use super::{
    instr::Expr,
    types::{
        FuncIdx, FuncType, GlobalIdx, GlobalType, MemIdx, MemType, Name, RefType, TableIdx,
        TableType, TypeIdx, ValType,
    },
    util::{to_section, to_vec},
    Bytes, UnSignedLeb128,
};

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
    DataCount = 12,
}

#[derive(Clone, Debug)]
pub struct SectionStruct(SectionId, u32, Vec<u8>);

#[derive(Clone, Debug)]
pub enum Section {
    Type(Vec<FuncType>),
    // First name is module name, second is "nm" (?).
    Import(Vec<Import>),
    Function(Vec<TypeIdx>),
    Table(Vec<TableType>),
    Memory(Vec<MemType>),
    Global(Vec<Global>),
    Export(Vec<Export>),
    Start(FuncIdx),
    Elem(Vec<Elem>),
    Code(Vec<Code>),
    Data(Vec<Data>),
    DataCount(u32),
}

impl Bytes for Section {
    fn to_bytes(&self) -> Vec<u8> {
        let (section_id, data) = match self {
            Section::Type(func_types) => (SectionId::Type, to_vec(func_types)),
            Section::Import(imports) => (SectionId::Import, to_vec(imports)),
            Section::Function(type_idxs) => (SectionId::Function, to_vec(type_idxs)),
            Section::Table(table_types) => (SectionId::Table, to_vec(table_types)),
            Section::Memory(memories) => (SectionId::Memory, to_vec(memories)),
            Section::Global(globals) => (SectionId::Global, to_vec(globals)),
            Section::Export(exports) => (SectionId::Export, to_vec(exports)),
            Section::Start(func_idx) => (SectionId::Start, func_idx.to_bytes()),
            Section::Elem(elems) => (SectionId::Element, to_vec(elems)),
            Section::Code(codes) => (SectionId::Code, to_vec(codes)),
            Section::Data(datas) => (SectionId::Data, to_vec(datas)),
            Section::DataCount(count) => (SectionId::DataCount, count.to_leb128()),
        };

        to_section(section_id, data)
    }
}

#[derive(Clone, Debug)]
pub struct Global(GlobalType, Expr);

impl Bytes for Global {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = self.0.to_bytes();
        buf.extend_from_slice(&self.1.to_bytes());
        buf
    }
}

#[derive(Clone, Debug)]
pub struct Import(Name, Name, ImportDesc);

impl Bytes for Import {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = self.0.to_bytes();
        buf.extend_from_slice(&self.1.to_bytes());
        buf.extend_from_slice(&self.2.to_bytes());
        buf
    }
}

#[derive(Clone, Debug)]
pub enum ImportDesc {
    TypeIdx(TypeIdx),
    TableType(TableType),
    MemType(MemType),
    GlobalType(GlobalType),
}

impl Bytes for ImportDesc {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            ImportDesc::TypeIdx(type_idx) => type_idx.to_bytes(),
            ImportDesc::TableType(table_type) => table_type.to_bytes(),
            ImportDesc::MemType(mem_type) => mem_type.to_bytes(),
            ImportDesc::GlobalType(global_type) => global_type.to_bytes(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Export(Name, ExportDesc);

impl Bytes for Export {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = self.0.to_bytes();
        buf.extend_from_slice(&self.1.to_bytes());
        buf
    }
}

#[derive(Clone, Debug)]
pub enum ExportDesc {
    FuncIdx(FuncIdx),
    TableIdx(TableIdx),
    MemIdx(MemIdx),
    GlobalIdx(GlobalIdx),
}

impl Bytes for ExportDesc {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            ExportDesc::FuncIdx(func_idx) => func_idx.to_bytes(),
            ExportDesc::TableIdx(table_idx) => table_idx.to_bytes(),
            ExportDesc::MemIdx(mem_idx) => mem_idx.to_bytes(),
            ExportDesc::GlobalIdx(global_idx) => global_idx.to_bytes(),
        }
    }
}

// TODO: What should the variants be named?
#[derive(Clone, Debug)]
pub enum Elem {
    Zero(Expr, Vec<FuncIdx>),
    One(ElemKind, Vec<FuncIdx>),
    Two(TableIdx, Expr, ElemKind, Vec<FuncIdx>),
    Three(ElemKind, Vec<FuncIdx>),
    Four(Expr, Vec<Expr>),
    Five(RefType, Vec<Expr>),
    Six(TableIdx, Expr, RefType, Vec<Expr>),
    Seven(RefType, Vec<Expr>),
}

impl Bytes for Elem {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            Elem::Zero(expr, func_idxs) => {
                let mut buf = vec![0x00];
                buf.extend_from_slice(&expr.to_bytes());
                buf.extend_from_slice(&to_vec(func_idxs));
                buf
            }
            Elem::One(elem_kind, func_idxs) => {
                let mut buf = vec![0x01];
                buf.extend_from_slice(&elem_kind.to_bytes());
                buf.extend_from_slice(&to_vec(func_idxs));
                buf
            }
            Elem::Two(table_idx, expr, elem_kind, func_idxs) => {
                let mut buf = vec![0x02];
                buf.extend_from_slice(&table_idx.to_bytes());
                buf.extend_from_slice(&expr.to_bytes());
                buf.extend_from_slice(&elem_kind.to_bytes());
                buf.extend_from_slice(&to_vec(func_idxs));
                buf
            }
            Elem::Three(elem_kind, func_idxs) => {
                let mut buf = vec![0x03];
                buf.extend_from_slice(&elem_kind.to_bytes());
                buf.extend_from_slice(&to_vec(func_idxs));
                buf
            }
            Elem::Four(expr, exprs) => {
                let mut buf = vec![0x04];
                buf.extend_from_slice(&expr.to_bytes());
                buf.extend_from_slice(&to_vec(exprs));
                buf
            }
            Elem::Five(ref_type, exprs) => {
                let mut buf = vec![0x05];
                buf.push(*ref_type as u8);
                buf.extend_from_slice(&to_vec(exprs));
                buf
            }
            Elem::Six(table_idx, expr, ref_type, exprs) => {
                let mut buf = vec![0x06];
                buf.extend_from_slice(&table_idx.to_bytes());
                buf.extend_from_slice(&expr.to_bytes());
                buf.push(*ref_type as u8);
                buf.extend_from_slice(&to_vec(exprs));
                buf
            }
            Elem::Seven(ref_type, exprs) => {
                let mut buf = vec![0x07];
                buf.push(*ref_type as u8);
                buf.extend_from_slice(&to_vec(exprs));
                buf
            }
        }
    }
}

// 0x00 is currently the only possible ElemKind value according to the wasm spec.
pub const ELEM_KIND: u8 = 0x00;

#[derive(Clone, Debug)]
pub struct ElemKind(u8);

impl Bytes for ElemKind {
    fn to_bytes(&self) -> Vec<u8> {
        vec![self.0]
    }
}

#[derive(Clone, Debug)]
pub struct Code(u32, Func);

impl Bytes for Code {
    fn to_bytes(&self) -> Vec<u8> {
        let func_code = self.1.to_bytes();
        let mut buf = Vec::with_capacity(func_code.len() + 4);
        buf.extend_from_slice(&func_code.len().to_leb128());
        buf.extend_from_slice(&func_code);
        buf
    }
}

#[derive(Clone, Debug)]
pub struct Func(Vec<Locals>, Expr);

impl Bytes for Func {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = to_vec(&self.0);
        buf.extend_from_slice(&self.1.to_bytes());
        buf
    }
}

#[derive(Clone, Debug)]
pub struct Locals(u32, ValType);

impl Bytes for Locals {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = self.0.to_leb128();
        buf.extend_from_slice(&self.1.to_bytes());
        buf
    }
}

// TODO: What should the variants be named?
#[derive(Clone, Debug)]
pub enum Data {
    Zero(Expr, Vec<u8>),
    One(Vec<u8>),
    Two(MemIdx, Expr, Vec<u8>),
}

impl Bytes for Data {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            Data::Zero(expr, data) => {
                let mut buf = vec![0x00];
                buf.extend_from_slice(&expr.to_bytes());
                buf.extend_from_slice(&to_vec_raw(data));
                buf
            }
            Data::One(data) => {
                let mut buf = vec![0x01];
                buf.extend_from_slice(&to_vec_raw(data));
                buf
            }
            Data::Two(mem_idx, expr, data) => {
                let mut buf = vec![0x02];
                buf.extend_from_slice(&mem_idx.to_bytes());
                buf.extend_from_slice(&expr.to_bytes());
                buf.extend_from_slice(&to_vec_raw(data));
                buf
            }
        }
    }
}
