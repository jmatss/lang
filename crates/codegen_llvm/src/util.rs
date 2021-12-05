use std::convert::TryFrom;

use inkwell::{
    types::{AnyTypeEnum, BasicTypeEnum},
    values::{AnyValueEnum, BasicValueEnum},
};

use common::error::{LangError, LangErrorKind::CodeGenError, LangResult};

pub fn to_param_name(idx: usize) -> String {
    format!("param_{}", idx)
}

pub fn to_local_name(idx: usize) -> String {
    format!("local_{}", idx)
}

pub fn to_global_name(idx: usize) -> String {
    format!("global_{}", idx)
}

pub fn to_data_name(idx: usize) -> String {
    format!("data_{}", idx)
}

pub(super) fn any_into_basic_value(any_value: AnyValueEnum) -> LangResult<BasicValueEnum> {
    BasicValueEnum::try_from(any_value).map_err(|_| {
        LangError::new(
            format!(
                "Unable to convert AnyValueEnum: {:#?} into BasicValueEnum.",
                &any_value
            ),
            CodeGenError,
            None,
        )
    })
}

pub(super) fn any_into_basic_type(any_type: AnyTypeEnum) -> LangResult<BasicTypeEnum> {
    BasicTypeEnum::try_from(any_type).map_err(|_| {
        LangError::new(
            format!(
                "Unable to convert AnyTypeEnum: {:#?} into BasicTypeEnum.",
                &any_type,
            ),
            CodeGenError,
            None,
        )
    })
}

/// Returns true if all basic values in `values` are const.
pub(super) fn is_const(values: &[AnyValueEnum]) -> bool {
    for value in values.iter() {
        let is_const = match *value {
            AnyValueEnum::ArrayValue(val) => val.is_const(),
            AnyValueEnum::IntValue(val) => val.is_const(),
            AnyValueEnum::FloatValue(val) => val.is_const(),
            AnyValueEnum::PointerValue(val) => val.is_const(),
            AnyValueEnum::StructValue(val) => {
                // TODO: Should probably be some way to iterate through all its member
                //       recursively and figure out if all fields of the struct is
                //       const. If that is the case, one can assume that the struct
                //       is also const.
                // If the struct has no name, this is a constant struct
                // according to inkwell documentation.
                val.get_name().to_bytes().is_empty()
            }
            AnyValueEnum::VectorValue(val) => val.is_const(),

            AnyValueEnum::PhiValue(_)
            | AnyValueEnum::FunctionValue(_)
            | AnyValueEnum::InstructionValue(_) => false,
        };

        if !is_const {
            return false;
        }
    }
    true
}
