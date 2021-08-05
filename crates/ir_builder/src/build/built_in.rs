use common::{error::LangResult, token::expr::BuiltInCall};
use ir::Val;

use crate::state::BuildState;

pub fn build_built_in_call(state: &mut BuildState, built_in_call: &BuiltInCall) -> LangResult<Val> {
    match built_in_call.name.as_ref() {
        "size" => todo!(),
        "type" => todo!(),
        "name" => todo!(),
        "null" => todo!(),
        "is_null" => todo!(),
        "ptr_add" => todo!(),
        "ptr_sub" => todo!(),
        "format" => todo!(),
        "array" => todo!(),
        "argc" => todo!(),
        "argv" => todo!(),
        "file" => todo!(),
        "line" => todo!(),
        "column" => todo!(),
        "unreachable" => todo!(),
        _ => {
            unreachable!("Bad built in name: {:#?}", built_in_call);
        }
    }
}
