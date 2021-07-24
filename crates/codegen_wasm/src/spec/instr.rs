use crate::spec::{util::to_vec, SignedLeb128, UnSignedLeb128, END_TAG, START_TAG};

use super::{
    types::{
        BlockType, DataIdx, ElemIdx, FuncIdx, GlobalIdx, LabelIdx, LocalIdx, MemArg, RefType,
        TableIdx, TypeIdx, ValType,
    },
    Bytes,
};

#[derive(Clone, Debug)]
pub struct Expr(Vec<Instr>);

impl Bytes for Expr {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::default();
        for instr in &self.0 {
            buf.extend_from_slice(&instr.to_bytes());
        }
        buf.push(END_TAG);
        buf
    }
}

#[derive(Clone, Debug)]
pub enum Instr {
    Unreachable,
    Nop,
    Block(BlockType, Vec<Instr>),
    Loop(BlockType, Vec<Instr>),
    // The second vector are instructions contained in an optional else block.
    If(BlockType, Vec<Instr>, Option<Vec<Instr>>),

    Br(LabelIdx),
    BrIf(LabelIdx),
    // TODO: How does this work? What is the second LabelIdx?
    BrTable(Vec<LabelIdx>, LabelIdx),
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx, TableIdx),

    RefNull(RefType),
    RefIsNull,
    RefFunc(FuncIdx),

    Drop,
    Select,
    SelectT(Vec<ValType>),

    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    LocalTee(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),

    TableGet(TableIdx),
    TableSet(TableIdx),
    TableInit(ElemIdx, TableIdx),
    ElemDrop(ElemIdx),
    TableCopy(ElemIdx, TableIdx),
    TableGrow(TableIdx),
    TableSize(TableIdx),
    TableFill(TableIdx),

    MemI32Load(MemArg),
    MemI64Load(MemArg),
    MemF32Load(MemArg),
    MemF64Load(MemArg),
    MemI32Load8s(MemArg),
    MemI32Load8u(MemArg),
    MemI32Load16s(MemArg),
    MemI32Load16u(MemArg),
    MemI64Load8s(MemArg),
    MemI64Load8u(MemArg),
    MemI64Load16s(MemArg),
    MemI64Load16u(MemArg),
    MemI64Load32s(MemArg),
    MemI64Load32u(MemArg),
    MemI32Store(MemArg),
    MemI64Store(MemArg),
    MemF32Store(MemArg),
    MemF64Store(MemArg),
    MemI32Store8(MemArg),
    MemI32Store16(MemArg),
    MemI64Store8(MemArg),
    MemI64Store16(MemArg),
    MemI64Store32(MemArg),
    MemSize,
    MemGrow,
    MemInit(DataIdx),
    MemDataDrop(DataIdx),
    MemCopy,
    MemFill,

    ConstI32(i32),
    ConstI64(i64),
    ConstF32(f32),
    ConstF64(f64),

    EqzI32,
    EqI32,
    NeI32,
    LtsI32,
    LtuI32,
    GtsI32,
    GtuI32,
    LesI32,
    LeuI32,
    GesI32,
    GeuI32,

    EqzI64,
    EqI64,
    NeI64,
    LtsI64,
    LtuI64,
    GtsI64,
    GtuI64,
    LesI64,
    LeuI64,
    GesI64,
    GeuI64,

    EqF32,
    NeF32,
    LtF32,
    GtF32,
    LeF32,
    GeF32,

    EqF64,
    NeF64,
    LtF64,
    GtF64,
    LeF64,
    GeF64,

    ClzI32,
    CtzI32,
    PopCntI32,
    AddI32,
    SubI32,
    MulI32,
    DivsI32,
    DivuI32,
    RemsI32,
    RemuI32,
    AndI32,
    OrI32,
    XorI32,
    ShlI32,
    ShrsI32,
    ShruI32,
    RotlI32,
    RotrI32,

    ClzI64,
    CtzI64,
    PopCntI64,
    AddI64,
    SubI64,
    MulI64,
    DivsI64,
    DivuI64,
    RemsI64,
    RemuI64,
    AndI64,
    OrI64,
    XorI64,
    ShlI64,
    ShrsI64,
    ShruI64,
    RotlI64,
    RotrI64,

    AbsF32,
    NegF32,
    CeilF32,
    FloorF32,
    TruncF32,
    NearestF32,
    SqrtF32,
    AddF32,
    SubF32,
    MulF32,
    DivF32,
    MinF32,
    MaxF32,
    CopySignF32,

    AbsF64,
    NegF64,
    CeilF64,
    FloorF64,
    TruncF64,
    NearestF64,
    SqrtF64,
    AddF64,
    SubF64,
    MulF64,
    DivF64,
    MinF64,
    MaxF64,
    CopySignF64,

    WrapI32FromI64,
    TruncI32FromF32s,
    TruncI32FromF32u,
    TruncI32FromF64s,
    TruncI32FromF64u,
    ExtendI64FromI32s,
    ExtendI64FromI32u,
    TruncI64FromF32s,
    TruncI64FromF32u,
    TruncI64FromF64s,
    TruncI64FromF64u,
    ConvertF32FromI32s,
    ConvertF32FromI32u,
    ConvertF32FromI64s,
    ConvertF32FromI64u,
    DemoteF32FromF64,
    ConvertF64FromI32s,
    ConvertF64FromI32u,
    ConvertF64FromI64s,
    ConvertF64FromI64u,
    PromoteF64FromF32,
    ReinterpretI32FromF32,
    ReinterpretI64FromF64,
    ReinterpretF32FromI32,
    ReinterpretF64FromI64,

    ExtendI32From8s,
    ExtendI32From16s,
    ExtendI64From8s,
    ExtendI64From16s,
    ExtendI64From32s,

    TruncSatI32FromF32s,
    TruncSatI32FromF32u,
    TruncSatI32FromF64s,
    TruncSatI32FromF64u,
    TruncSatI64FromF32s,
    TruncSatI64FromF32u,
    TruncSatI64FromF64s,
    TruncSatI64FromF64u,
}

impl Bytes for Instr {
    fn to_bytes(&self) -> Vec<u8> {
        match self {
            Instr::Unreachable => vec![0x00],
            Instr::Nop => vec![0x01],
            Instr::Block(bt, instrs) => {
                let mut buf = vec![0x02];
                buf.extend_from_slice(&bt.to_bytes());
                for instr in instrs {
                    buf.extend_from_slice(&instr.to_bytes());
                }
                buf.push(END_TAG);
                buf
            }
            Instr::Loop(bt, instrs) => {
                let mut buf = vec![0x03];
                buf.extend_from_slice(&bt.to_bytes());
                for instr in instrs {
                    buf.extend_from_slice(&instr.to_bytes());
                }
                buf.push(END_TAG);
                buf
            }
            Instr::If(bt, if_instrs, else_instrs) => {
                let mut buf = vec![0x04];
                buf.extend_from_slice(&bt.to_bytes());
                for instr in if_instrs {
                    buf.extend_from_slice(&instr.to_bytes());
                }
                if let Some(else_instrs) = else_instrs {
                    buf.push(0x05);
                    for instr in else_instrs {
                        buf.extend_from_slice(&instr.to_bytes());
                    }
                }
                buf.push(END_TAG);
                buf
            }
            Instr::Br(label_idx) => {
                let mut buf = vec![0x0c];
                buf.extend_from_slice(&label_idx.to_leb128());
                buf
            }
            Instr::BrIf(label_idx) => {
                let mut buf = vec![0x0d];
                buf.extend_from_slice(&label_idx.to_leb128());
                buf
            }
            Instr::BrTable(label_idxs, label_idx) => {
                let mut res = vec![0x0e];
                res.extend_from_slice(&to_vec(label_idxs));
                res.extend_from_slice(&label_idx.to_leb128());
                res
            }
            Instr::Return => vec![0x0f],
            Instr::Call(func_idx) => {
                let mut res = vec![0x0e];
                res.extend_from_slice(&func_idx.to_leb128());
                res
            }
            Instr::CallIndirect(type_idx, table_idx) => {
                let mut res = vec![0x0e];
                res.extend_from_slice(&type_idx.to_leb128());
                res.extend_from_slice(&table_idx.to_leb128());
                res
            }

            Instr::RefNull(ref_type) => vec![0xd0, *ref_type as u8],
            Instr::RefIsNull => vec![0xd1],
            Instr::RefFunc(func_idx) => {
                let mut buf = vec![0xd2];
                buf.extend_from_slice(&func_idx.to_bytes());
                buf
            }

            Instr::Drop => vec![0x1a],
            Instr::Select => vec![0x1b],
            Instr::SelectT(val_types) => {
                let mut buf = vec![0x1c];
                buf.extend_from_slice(&to_vec(val_types));
                buf
            }

            Instr::LocalGet(local_idx) => {
                let mut buf = vec![0x20];
                buf.extend_from_slice(&local_idx.to_bytes());
                buf
            }
            Instr::LocalSet(local_idx) => {
                let mut buf = vec![0x21];
                buf.extend_from_slice(&local_idx.to_bytes());
                buf
            }
            Instr::LocalTee(local_idx) => {
                let mut buf = vec![0x22];
                buf.extend_from_slice(&local_idx.to_bytes());
                buf
            }
            Instr::GlobalGet(global_idx) => {
                let mut buf = vec![0x23];
                buf.extend_from_slice(&global_idx.to_bytes());
                buf
            }
            Instr::GlobalSet(global_idx) => {
                let mut buf = vec![0x24];
                buf.extend_from_slice(&global_idx.to_bytes());
                buf
            }

            Instr::TableGet(table_idx) => {
                let mut buf = vec![0x25];
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }
            Instr::TableSet(table_idx) => {
                let mut buf = vec![0x26];
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }
            Instr::TableInit(elem_idx, table_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&12_u32.to_leb128());
                buf.extend_from_slice(&elem_idx.to_bytes());
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }
            Instr::ElemDrop(elem_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&13_u32.to_leb128());
                buf.extend_from_slice(&elem_idx.to_bytes());
                buf
            }
            Instr::TableCopy(elem_idx, table_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&14_u32.to_leb128());
                buf.extend_from_slice(&elem_idx.to_bytes());
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }
            Instr::TableGrow(table_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&15_u32.to_leb128());
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }
            Instr::TableSize(table_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&16_u32.to_leb128());
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }
            Instr::TableFill(table_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&17_u32.to_leb128());
                buf.extend_from_slice(&table_idx.to_bytes());
                buf
            }

            Instr::MemI32Load(mem_arg) => {
                let mut buf = vec![0x28];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load(mem_arg) => {
                let mut buf = vec![0x29];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemF32Load(mem_arg) => {
                let mut buf = vec![0x2a];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemF64Load(mem_arg) => {
                let mut buf = vec![0x2b];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Load8s(mem_arg) => {
                let mut buf = vec![0x2c];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Load8u(mem_arg) => {
                let mut buf = vec![0x2d];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Load16s(mem_arg) => {
                let mut buf = vec![0x2e];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Load16u(mem_arg) => {
                let mut buf = vec![0x2f];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load8s(mem_arg) => {
                let mut buf = vec![0x30];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load8u(mem_arg) => {
                let mut buf = vec![0x31];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load16s(mem_arg) => {
                let mut buf = vec![0x32];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load16u(mem_arg) => {
                let mut buf = vec![0x33];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load32s(mem_arg) => {
                let mut buf = vec![0x34];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Load32u(mem_arg) => {
                let mut buf = vec![0x35];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Store(mem_arg) => {
                let mut buf = vec![0x36];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Store(mem_arg) => {
                let mut buf = vec![0x37];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemF32Store(mem_arg) => {
                let mut buf = vec![0x38];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemF64Store(mem_arg) => {
                let mut buf = vec![0x39];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Store8(mem_arg) => {
                let mut buf = vec![0x3a];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI32Store16(mem_arg) => {
                let mut buf = vec![0x3b];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Store8(mem_arg) => {
                let mut buf = vec![0x3c];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Store16(mem_arg) => {
                let mut buf = vec![0x3d];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemI64Store32(mem_arg) => {
                let mut buf = vec![0x3e];
                buf.extend_from_slice(&mem_arg.to_bytes());
                buf
            }
            Instr::MemSize => vec![0x3f, 0x00],
            Instr::MemGrow => vec![0x40, 0x00],
            Instr::MemInit(data_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&8_u32.to_leb128());
                buf.extend_from_slice(&data_idx.to_bytes());
                buf.push(0x00);
                buf
            }
            Instr::MemDataDrop(data_idx) => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&9_u32.to_leb128());
                buf.extend_from_slice(&data_idx.to_bytes());
                buf
            }
            Instr::MemCopy => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&10_u32.to_leb128());
                buf.push(0x00);
                buf.push(0x00);
                buf
            }
            Instr::MemFill => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&11_u32.to_leb128());
                buf.push(0x00);
                buf
            }

            Instr::ConstI32(n) => {
                let mut buf = vec![0x41];
                buf.extend_from_slice(&n.to_leb128());
                buf
            }
            Instr::ConstI64(n) => {
                let mut buf = vec![0x42];
                buf.extend_from_slice(&n.to_leb128());
                buf
            }
            Instr::ConstF32(n) => {
                let mut buf = vec![0x43];
                buf.extend_from_slice(&n.to_le_bytes());
                buf
            }
            Instr::ConstF64(n) => {
                let mut buf = vec![0x44];
                buf.extend_from_slice(&n.to_le_bytes());
                buf
            }

            Instr::EqzI32 => vec![0x45],
            Instr::EqI32 => vec![0x46],
            Instr::NeI32 => vec![0x47],
            Instr::LtsI32 => vec![0x48],
            Instr::LtuI32 => vec![0x49],
            Instr::GtsI32 => vec![0x4a],
            Instr::GtuI32 => vec![0x4b],
            Instr::LesI32 => vec![0x4c],
            Instr::LeuI32 => vec![0x4d],
            Instr::GesI32 => vec![0x4e],
            Instr::GeuI32 => vec![0x4f],

            Instr::EqzI64 => vec![0x50],
            Instr::EqI64 => vec![0x51],
            Instr::NeI64 => vec![0x52],
            Instr::LtsI64 => vec![0x53],
            Instr::LtuI64 => vec![0x54],
            Instr::GtsI64 => vec![0x55],
            Instr::GtuI64 => vec![0x56],
            Instr::LesI64 => vec![0x57],
            Instr::LeuI64 => vec![0x58],
            Instr::GesI64 => vec![0x59],
            Instr::GeuI64 => vec![0x5a],

            Instr::EqF32 => vec![0x5b],
            Instr::NeF32 => vec![0x5c],
            Instr::LtF32 => vec![0x5d],
            Instr::GtF32 => vec![0x5e],
            Instr::LeF32 => vec![0x5f],
            Instr::GeF32 => vec![0x60],

            Instr::EqF64 => vec![0x61],
            Instr::NeF64 => vec![0x62],
            Instr::LtF64 => vec![0x63],
            Instr::GtF64 => vec![0x64],
            Instr::LeF64 => vec![0x65],
            Instr::GeF64 => vec![0x66],

            Instr::ClzI32 => vec![0x67],
            Instr::CtzI32 => vec![0x68],
            Instr::PopCntI32 => vec![0x69],
            Instr::AddI32 => vec![0x6a],
            Instr::SubI32 => vec![0x6b],
            Instr::MulI32 => vec![0x6c],
            Instr::DivsI32 => vec![0x6d],
            Instr::DivuI32 => vec![0x6e],
            Instr::RemsI32 => vec![0x6f],
            Instr::RemuI32 => vec![0x70],
            Instr::AndI32 => vec![0x71],
            Instr::OrI32 => vec![0x72],
            Instr::XorI32 => vec![0x73],
            Instr::ShlI32 => vec![0x74],
            Instr::ShrsI32 => vec![0x75],
            Instr::ShruI32 => vec![0x76],
            Instr::RotlI32 => vec![0x77],
            Instr::RotrI32 => vec![0x78],
            Instr::ClzI64 => vec![0x79],
            Instr::CtzI64 => vec![0x7a],
            Instr::PopCntI64 => vec![0x7b],
            Instr::AddI64 => vec![0x7c],
            Instr::SubI64 => vec![0x7d],
            Instr::MulI64 => vec![0x7e],
            Instr::DivsI64 => vec![0x7f],
            Instr::DivuI64 => vec![0x80],
            Instr::RemsI64 => vec![0x81],
            Instr::RemuI64 => vec![0x82],
            Instr::AndI64 => vec![0x83],
            Instr::OrI64 => vec![0x84],
            Instr::XorI64 => vec![0x85],
            Instr::ShlI64 => vec![0x86],
            Instr::ShrsI64 => vec![0x87],
            Instr::ShruI64 => vec![0x88],
            Instr::RotlI64 => vec![0x89],
            Instr::RotrI64 => vec![0x8a],
            Instr::AbsF32 => vec![0x8b],
            Instr::NegF32 => vec![0x8c],
            Instr::CeilF32 => vec![0x8d],
            Instr::FloorF32 => vec![0x8e],
            Instr::TruncF32 => vec![0x8f],
            Instr::NearestF32 => vec![0x90],
            Instr::SqrtF32 => vec![0x91],
            Instr::AddF32 => vec![0x92],
            Instr::SubF32 => vec![0x93],
            Instr::MulF32 => vec![0x94],
            Instr::DivF32 => vec![0x95],
            Instr::MinF32 => vec![0x96],
            Instr::MaxF32 => vec![0x97],
            Instr::CopySignF32 => vec![0x98],
            Instr::AbsF64 => vec![0x99],
            Instr::NegF64 => vec![0x9a],
            Instr::CeilF64 => vec![0x9b],
            Instr::FloorF64 => vec![0x9c],
            Instr::TruncF64 => vec![0x9d],
            Instr::NearestF64 => vec![0x9e],
            Instr::SqrtF64 => vec![0x9f],
            Instr::AddF64 => vec![0xa0],
            Instr::SubF64 => vec![0xa1],
            Instr::MulF64 => vec![0xa2],
            Instr::DivF64 => vec![0xa3],
            Instr::MinF64 => vec![0xa4],
            Instr::MaxF64 => vec![0xa5],
            Instr::CopySignF64 => vec![0xa6],
            Instr::WrapI32FromI64 => vec![0xa7],
            Instr::TruncI32FromF32s => vec![0xa8],
            Instr::TruncI32FromF32u => vec![0xa9],
            Instr::TruncI32FromF64s => vec![0xaa],
            Instr::TruncI32FromF64u => vec![0xab],
            Instr::ExtendI64FromI32s => vec![0xac],
            Instr::ExtendI64FromI32u => vec![0xad],
            Instr::TruncI64FromF32s => vec![0xae],
            Instr::TruncI64FromF32u => vec![0xaf],
            Instr::TruncI64FromF64s => vec![0xb0],
            Instr::TruncI64FromF64u => vec![0xb1],
            Instr::ConvertF32FromI32s => vec![0xb2],
            Instr::ConvertF32FromI32u => vec![0xb3],
            Instr::ConvertF32FromI64s => vec![0xb4],
            Instr::ConvertF32FromI64u => vec![0xb5],
            Instr::DemoteF32FromF64 => vec![0xb6],
            Instr::ConvertF64FromI32s => vec![0xb7],
            Instr::ConvertF64FromI32u => vec![0xb8],
            Instr::ConvertF64FromI64s => vec![0xb9],
            Instr::ConvertF64FromI64u => vec![0xba],
            Instr::PromoteF64FromF32 => vec![0xbb],
            Instr::ReinterpretI32FromF32 => vec![0xbc],
            Instr::ReinterpretI64FromF64 => vec![0xbd],
            Instr::ReinterpretF32FromI32 => vec![0xbe],
            Instr::ReinterpretF64FromI64 => vec![0xbf],

            Instr::ExtendI32From8s => vec![0xc0],
            Instr::ExtendI32From16s => vec![0xc1],
            Instr::ExtendI64From8s => vec![0xc2],
            Instr::ExtendI64From16s => vec![0xc3],
            Instr::ExtendI64From32s => vec![0xc4],

            Instr::TruncSatI32FromF32s => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&0_u32.to_leb128());
                buf
            }
            Instr::TruncSatI32FromF32u => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&1_u32.to_leb128());
                buf
            }
            Instr::TruncSatI32FromF64s => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&2_u32.to_leb128());
                buf
            }
            Instr::TruncSatI32FromF64u => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&3_u32.to_leb128());
                buf
            }
            Instr::TruncSatI64FromF32s => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&4_u32.to_leb128());
                buf
            }
            Instr::TruncSatI64FromF32u => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&5_u32.to_leb128());
                buf
            }
            Instr::TruncSatI64FromF64s => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&6_u32.to_leb128());
                buf
            }
            Instr::TruncSatI64FromF64u => {
                let mut buf = vec![START_TAG];
                buf.extend_from_slice(&7_u32.to_leb128());
                buf
            }
        }
    }
}
