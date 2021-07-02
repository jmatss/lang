use super::{section::SectionId, Bytes, UnSignedLeb128};

pub fn to_vec<T: Bytes>(vec: &[T]) -> Vec<u8> {
    let mut buf = Vec::new();
    // TODO: Should this be the length of the vector or the amount of bytes it
    //       contains?
    buf.extend_from_slice(&vec.len().to_leb128());
    for item in vec {
        buf.extend_from_slice(&item.to_bytes());
    }
    buf
}

pub fn to_vec_raw(data: &[u8]) -> Vec<u8> {
    let mut buf = Vec::with_capacity(data.len() + 4);
    buf.extend_from_slice(&data.len().to_leb128());
    buf.extend_from_slice(data);
    buf
}

pub fn to_section(section_id: SectionId, data: Vec<u8>) -> Vec<u8> {
    let mut buf = Vec::with_capacity(data.len() + 5);
    buf.push(section_id as u8);
    buf.extend_from_slice(&data.len().to_leb128());
    buf.extend_from_slice(&data);
    buf
}
