use crate::{IrError, IrResult, Module, Type};

/// Returns the size of the given type `ir_type` in bytes.
///
/// Padding rules for struct:
/// * All members starts at address divisible by the members size.
/// * The whole struct should be divisible by its largest member.
pub fn size_with_padding(module: &Module, ir_type: &Type) -> IrResult<usize> {
    // This function is called recursively for ADTs. In all cases, the caller
    // will handle the "start" padding.
    Ok(match &ir_type {
        Type::Adt(adt_name) => {
            if let Some(members) = module.get_struct(adt_name)? {
                let mut largest_size = 0;
                let mut prev_size = 0;
                let mut acc_size = 0;
                for member_type in members {
                    let cur_size = size_with_padding(module, member_type)?;
                    let padding = if prev_size % cur_size != 0 {
                        cur_size - (prev_size % cur_size)
                    } else {
                        0
                    };
                    let cur_size_with_padding = cur_size + padding;

                    if cur_size_with_padding > largest_size {
                        largest_size = cur_size_with_padding;
                    }
                    acc_size += cur_size_with_padding;
                    prev_size = cur_size_with_padding;
                }

                let struct_padding = if largest_size > 0 && acc_size % largest_size != 0 {
                    largest_size - (acc_size % largest_size)
                } else {
                    0
                };
                acc_size + struct_padding
            } else {
                return Err(IrError::new(format!(
                    "Tried to take size_of externally declared struct \"{}\".",
                    adt_name
                )));
            }
        }
        Type::Pointer(_) | Type::FuncPointer(..) => module.ptr_size,
        Type::Array(ir_type_i, Some(dim)) => {
            size_with_padding(module, &*ir_type_i.clone())? * (*dim as usize)
        }
        Type::Char => 4,
        Type::Bool => 1,
        Type::I8 | Type::U8 => 1,
        Type::I16 | Type::U16 => 2,
        Type::I32 | Type::U32 | Type::F32 => 4,
        Type::I64 | Type::U64 | Type::F64 => 8,
        Type::I128 | Type::U128 => 16,

        Type::Func(_) => {
            return Err(IrError::new(
                "Tried to take size_of() function (is not sized).".into(),
            ))
        }
        Type::Array(_, None) => {
            return Err(IrError::new(
                "Tried to take size_of() array slice (is not sized).".into(),
            ))
        }
        Type::Void => {
            return Err(IrError::new(
                "Tried to take size_of() void (is not sized).".into(),
            ))
        }
    })
}
