use crate::ty::generics::Generics;

/// Concatenates a ADT name and a method name to create the name that will
/// be used to refer to this function. The name is concatenated with a dash.
pub fn to_method_name(
    adt_name: &str,
    adt_generics: Option<&Generics>,
    method_name: &str,
    method_generics: Option<&Generics>,
) -> String {
    let adt_generic_name = if let Some(adt_generics) = adt_generics {
        to_generic_name(adt_name, adt_generics)
    } else {
        adt_name.into()
    };
    let method_generic_name = if let Some(method_generics) = method_generics {
        to_generic_name(method_name, method_generics)
    } else {
        method_name.into()
    };

    format!("{}-{}", adt_generic_name, method_generic_name)
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
pub fn to_generic_name(old_name: &str, generics: &Generics) -> String {
    if generics.is_empty() {
        old_name.to_owned()
    } else {
        format!("{}{}", old_name, generics.to_string())
    }
}

/// Formats the name of a union variant.
/// Concatenates the name of the union with the name of the member that is being
/// accessed/used.
pub fn to_union_variant_name(union_name: &str, member_name: &str) -> String {
    format!("{}${}", union_name, member_name)
}
