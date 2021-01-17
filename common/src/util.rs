use crate::ty::generics::Generics;

/// Concatenates a structure name and a method name to create the name that will
/// be used to refer to this function. The name is concatenated with a dash.
pub fn to_method_name(
    structure_name: &str,
    structure_generics: Option<&Generics>,
    method_name: &str,
    method_generics: Option<&Generics>,
) -> String {
    let struct_generic_name = if let Some(structure_generics) = structure_generics {
        to_generic_name(structure_name, structure_generics)
    } else {
        structure_name.into()
    };
    let method_generic_name = if let Some(method_generics) = method_generics {
        to_generic_name(method_name, method_generics)
    } else {
        method_name.into()
    };

    format!("{}-{}", struct_generic_name, method_generic_name)
}

/// Adds the `copy_nr` information to the end of a variable name. This will come
/// in handy for variables that have generic types; they will be duplicated which
/// means that a name+blockID is not enough not uniqely identify it.
pub fn to_var_name(name: &str, copy_nr: usize) -> String {
    format!("{}:{}", name, copy_nr)
}

/// Formats the name of a struct/func with generics.
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
