pub(crate) mod instr;
pub(crate) mod section;
pub(crate) mod types;
pub(crate) mod util;

// https://webassembly.github.io/spec/core/binary/index.html

pub trait Bytes {
    fn to_bytes(&self) -> Vec<u8>;
}

pub trait SignedLeb128 {
    fn to_leb128(&self) -> Vec<u8>;
}

pub trait UnSignedLeb128 {
    fn to_leb128(&self) -> Vec<u8>;
}

pub const MAGIC_BYTES: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];
pub const VERSION_BYTES: [u8; 4] = [0x01, 0x00, 0x00, 0x00];

pub const START_TAG: u8 = 0xfc;
pub const END_TAG: u8 = 0x0b;

macro_rules! impl_leb128_trait {
    ($t:ty, signed) => {
        impl SignedLeb128 for $t {
            fn to_leb128(&self) -> Vec<u8> {
                let mut buf = Vec::default();
                leb128::write::signed(&mut buf, *self as i64).unwrap();
                buf
            }
        }
    };
    ($t:ty, unsigned) => {
        impl UnSignedLeb128 for $t {
            fn to_leb128(&self) -> Vec<u8> {
                let mut buf = Vec::default();
                leb128::write::unsigned(&mut buf, *self as u64).unwrap();
                buf
            }
        }
    };
}

impl_leb128_trait!(i8, signed);
impl_leb128_trait!(i16, signed);
impl_leb128_trait!(i32, signed);
impl_leb128_trait!(i64, signed);
impl_leb128_trait!(isize, signed);
impl_leb128_trait!(u8, unsigned);
impl_leb128_trait!(u16, unsigned);
impl_leb128_trait!(u32, unsigned);
impl_leb128_trait!(u64, unsigned);
impl_leb128_trait!(usize, unsigned);
