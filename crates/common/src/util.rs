use crate::{
    path::LangPath,
    ty::{
        generics::Generics,
        to_string::{to_string_generics, to_string_path},
        ty_env::TyEnv,
    },
};

/// Concatenates a ADT name and a method name to create the name that will
/// be used to refer to this function. The names are concatenated with a dot.
pub fn to_method_name(
    ty_env: &TyEnv,
    adt_path: &LangPath,
    method_name: &str,
    method_generics: Option<&Generics>,
) -> String {
    let adt_name_with_gens = to_string_path(ty_env, adt_path);
    let method_name_with_gens = if let Some(method_generics) = method_generics {
        to_generic_name(ty_env, method_name, method_generics)
    } else {
        method_name.into()
    };

    format!("{}.{}", adt_name_with_gens, method_name_with_gens)
}

/// Adds the `copy_nr` information to the end of a variable name. This will come
/// in handy for variables that have generic types; they will be duplicated which
/// means that a name+blockID is not enough not uniqely identify it.
pub fn to_var_name(name: &str, copy_nr: usize) -> String {
    format!("{}:{}", name, copy_nr)
}

/// Formats the name of a ADT/func with generics.
///
/// Example A struct with two generics K and V:
///    TestStruct<K,V>
/// This would be the exact format, case sensitive, no spaces etc.
pub fn to_generic_name(ty_env: &TyEnv, old_name: &str, generics: &Generics) -> String {
    if generics.is_empty_types() {
        old_name.to_owned()
    } else {
        format!(
            "{}{}",
            old_name,
            to_string_generics(ty_env, generics).unwrap()
        )
    }
}

/// Formats the name of a union variant.
/// Concatenates the name of the union with the name of the member that is being
/// accessed/used.
pub fn to_union_variant_name(union_name: &str, member_name: &str) -> String {
    format!("{}${}", union_name, member_name)
}
