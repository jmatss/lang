use std::collections::HashMap;

use parking_lot::Mutex;

use common::{
    error::LangResult,
    token::{
        block::BuiltIn,
        expr::{Var, VarType},
    },
    ty::{inner_ty::InnerTy, ty::Ty, ty_env::TyEnv, type_info::TypeInfo},
};

/// Stores information about all built-in functions into a hashmap.
pub fn init_built_ins(ty_env: &Mutex<TyEnv>) -> LangResult<HashMap<&'static str, BuiltIn>> {
    let mut ty_env_guard = ty_env.lock();
    let mut built_ins = HashMap::with_capacity(9);

    let unique_id = ty_env_guard.new_unique_id();
    let any_type_id = ty_env_guard.id(&Ty::Any(unique_id, TypeInfo::BuiltIn))?;
    let any_ptr_type_id = ty_env_guard.id(&Ty::Pointer(any_type_id, TypeInfo::BuiltIn))?;
    let uint_type_id = ty_env_guard.id(&Ty::CompoundType(InnerTy::Uint, TypeInfo::BuiltIn))?;
    let u8_type_id = ty_env_guard.id(&Ty::CompoundType(InnerTy::U8, TypeInfo::BuiltIn))?;
    let u8_ptr_type_id = ty_env_guard.id(&Ty::Pointer(u8_type_id, TypeInfo::BuiltIn))?;
    let u8_ptr_ptr_type_id = ty_env_guard.id(&Ty::Pointer(u8_ptr_type_id, TypeInfo::BuiltIn))?;
    let bool_type_id = ty_env_guard.id(&Ty::CompoundType(InnerTy::Boolean, TypeInfo::BuiltIn))?;
    let void_type_id = ty_env_guard.id(&Ty::CompoundType(InnerTy::Void, TypeInfo::BuiltIn))?;
    let string_type_id = ty_env_guard.id(&Ty::CompoundType(
        InnerTy::Struct(["std".into(), "string".into(), "String".into()].into()),
        TypeInfo::BuiltIn,
    ))?;
    let string_view_type_id = ty_env_guard.id(&Ty::CompoundType(
        InnerTy::Struct(["std".into(), "string".into(), "StringView".into()].into()),
        TypeInfo::BuiltIn,
    ))?;

    let name = "size";
    let parameters = Vec::with_capacity(0);
    let generics = Some(vec![any_type_id]);
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, uint_type_id, false),
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
        VarType::Unknown,
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
        VarType::Unknown,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, string_view_type_id, false),
    );

    let name = "null";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_type_id, false),
    );

    let name = "is_null";
    let parameters = vec![Var::new(
        "value".into(),
        Some(any_ptr_type_id),
        None,
        None,
        None,
        None,
        VarType::Unknown,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, bool_type_id, false),
    );

    let name = "is_not_null";
    let parameters = vec![Var::new(
        "value".into(),
        Some(any_ptr_type_id),
        None,
        None,
        None,
        None,
        VarType::Unknown,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, bool_type_id, false),
    );

    let name = "ptr_add";
    let parameters = vec![
        Var::new(
            "ptr".into(),
            Some(any_ptr_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        ),
        Var::new(
            "amount".into(),
            Some(uint_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        ),
    ];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_ptr_type_id, false),
    );

    let name = "ptr_sub";
    let parameters = vec![
        Var::new(
            "ptr".into(),
            Some(any_ptr_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        ),
        Var::new(
            "amount".into(),
            Some(uint_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        ),
    ];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_ptr_type_id, false),
    );

    let name = "array";
    let parameters = vec![
        Var::new(
            "init_value".into(),
            Some(any_type_id),
            None,
            None,
            None,
            None,
            VarType::Unknown,
        ),
        Var::new(
            "dimension".into(),
            Some(uint_type_id),
            None,
            None,
            None,
            None,
            VarType::Const,
        ),
    ];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_type_id, false),
    );

    // The parameters and generics are variadic/varargs. The amount of params
    // and generics should be the same. The generic at index 0 represents the
    // type of the parameter at index 0 and so on.
    let name = "tuple";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, any_type_id, true),
    );

    // TODO: Is it possible to make this StringView in some scenarios? Ex.
    //       if the arguments are constants and the whole string result can be
    //       calculated at compile time?
    let name = "format";
    let parameters = vec![Var::new(
        "format".into(),
        Some(any_type_id),
        None,
        None,
        None,
        None,
        VarType::Const,
    )];
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, string_type_id, true),
    );

    let name = "argc";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, uint_type_id, false),
    );

    let name = "argv";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, u8_ptr_ptr_type_id, false),
    );

    let name = "file";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, string_view_type_id, false),
    );

    let name = "line";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, uint_type_id, false),
    );

    let name = "column";
    let parameters = Vec::with_capacity(0);
    let generics = None;
    built_ins.insert(
        name,
        BuiltIn::new(name, parameters, generics, uint_type_id, false),
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
