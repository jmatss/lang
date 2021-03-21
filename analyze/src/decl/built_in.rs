use std::collections::HashMap;

use common::{
    error::LangResult,
    token::{block::BuiltIn, expr::Var},
    ty::{environment::TypeEnvironment, generics::Generics, inner_ty::InnerTy, ty::Ty},
    type_info::TypeInfo,
};

/// Stores information about all built-in functions into a hashmap.
pub fn init_built_ins(ty_env: &mut TypeEnvironment) -> LangResult<HashMap<&'static str, BuiltIn>> {
    let mut built_ins = HashMap::with_capacity(9);

    let any_type_id = ty_env.id(&Ty::Any(TypeInfo::BuiltIn))?;
    let u32_type_id = ty_env.id(&Ty::CompoundType(
        InnerTy::U32,
        Generics::empty(),
        TypeInfo::BuiltIn,
    ))?;
    let u8_type_id = ty_env.id(&Ty::CompoundType(
        InnerTy::U8,
        Generics::empty(),
        TypeInfo::BuiltIn,
    ))?;
    let u8_ptr_type_id = ty_env.id(&Ty::Pointer(u8_type_id, TypeInfo::BuiltIn))?;
    let bool_type_id = ty_env.id(&Ty::CompoundType(
        InnerTy::Boolean,
        Generics::empty(),
        TypeInfo::BuiltIn,
    ))?;
    let string_type_id = ty_env.id(&Ty::CompoundType(
        InnerTy::String,
        Generics::empty(),
        TypeInfo::BuiltIn,
    ))?;
    let void_type_id = ty_env.id(&Ty::CompoundType(
        InnerTy::Void,
        Generics::empty(),
        TypeInfo::BuiltIn,
    ))?;

    let name = "size";
    let parameters = Vec::with_capacity(0);
    let generics = Some(vec![any_type_id]);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, u32_type_id, false),
    );

    // TODO: How should this work? How will the type be represented in the
    //       LLVM codegen?
    let name = "type";
    let parameters = vec![Var::new(
        "expr".into(),
        Some(any_type_id),
        None,
        None,
        None,
        None,
        false,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_type_id, false),
    );

    let name = "name";
    let parameters = vec![Var::new(
        "name".into(),
        Some(any_type_id),
        None,
        None,
        None,
        None,
        false,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, u8_ptr_type_id, false),
    );

    let name = "null";
    let parameters = Vec::with_capacity(0);
    let generics = Some(vec![any_type_id]);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_type_id, false),
    );

    let name = "is_null";
    let parameters = vec![Var::new(
        "value".into(),
        Some(any_type_id),
        None,
        None,
        None,
        None,
        false,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, bool_type_id, false),
    );

    let name = "file";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, string_type_id, false),
    );

    let name = "line";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, u32_type_id, false),
    );

    let name = "column";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, u32_type_id, false),
    );

    let name = "unreachable";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, void_type_id, false),
    );

    Ok(built_ins)
}
