use std::collections::HashMap;

use common::{
    token::{block::BuiltIn, expr::Var},
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    type_info::TypeInfo,
};

/// Stores information about all built-in functions into a hashmap.
pub fn init_built_ins() -> HashMap<&'static str, BuiltIn> {
    let mut built_ins = HashMap::with_capacity(5);

    let name = "size";
    let parameters = Vec::with_capacity(0);
    let generics = Some(vec![Ty::Any(TypeInfo::None)]);
    let ret_ty = Ty::CompoundType(InnerTy::U32, Generics::empty(), TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    // TODO: How should this work? How will the type be represented in the
    //       LLVM codegen?
    let name = "type";
    let parameters = vec![Var::new(
        "expr".into(),
        Some(Ty::Any(TypeInfo::None)),
        None,
        None,
        None,
        None,
        false,
    )];
    let generics = None;
    let ret_ty = Ty::Any(TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    let name = "null";
    let parameters = Vec::with_capacity(0);
    let generics = Some(vec![Ty::Any(TypeInfo::None)]);
    let ret_ty = Ty::Any(TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    let name = "is_null";
    let parameters = vec![Var::new(
        "value".into(),
        Some(Ty::Any(TypeInfo::None)),
        None,
        None,
        None,
        None,
        false,
    )];
    let generics = None;
    let ret_ty = Ty::CompoundType(InnerTy::Boolean, Generics::empty(), TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    let name = "file";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    let ret_ty = Ty::CompoundType(InnerTy::String, Generics::empty(), TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    let name = "line";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    let ret_ty = Ty::CompoundType(InnerTy::U32, Generics::empty(), TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    let name = "column";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    let ret_ty = Ty::CompoundType(InnerTy::U32, Generics::empty(), TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    let name = "unreachable";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    let ret_ty = Ty::CompoundType(InnerTy::Void, Generics::empty(), TypeInfo::None);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, ret_ty, false),
    );

    built_ins
}
