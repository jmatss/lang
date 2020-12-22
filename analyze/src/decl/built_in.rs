use std::collections::HashMap;

use common::{
    token::{block::BuiltIn, expr::Var},
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
};

/// Stores information about all built-in functions into a hashmap.
pub fn init_built_ins() -> HashMap<&'static str, BuiltIn> {
    let mut built_ins = HashMap::with_capacity(1);

    let name = "size_of";
    let parameters = Vec::with_capacity(0);
    let generics = Some(vec![Ty::Any]);
    let ret_ty = Ty::CompoundType(InnerTy::U32, Generics::new());
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    // TODO: How should this work? How will the type be represented in the
    //       LLVM codegen?
    let name = "type_of";
    let parameters = vec![Var::new(
        "expr".into(),
        Some(Ty::Any),
        None,
        None,
        None,
        false,
    )];
    let generics = None;
    let ret_ty = Ty::Any;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    built_ins
}
