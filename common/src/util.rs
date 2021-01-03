use crate::ty::generics::Generics;

/// Concatenates a structure name and a method name to create the name that will
/// be used to refer to this function. The name is concatenated with a dash.
///
/// Format:
///   "<STRUCTURE_NAME>:<STRUCTURE_GENERICS>-<FUNCTION_NAME>"
pub fn to_method_name(
    structure_name: &str,
    structure_generics: &Generics,
    method_name: &str,
) -> String {
    format!(
        "{}-{}",
        to_generic_struct_name(structure_name, structure_generics),
        method_name
    )
}

/// Adds the `copy_nr` information to the end of a variable name. This will come
/// in handy for variables that have generic types; they will be duplicated which
/// means that a name+blockID is not enough not uniqely identify it.
pub fn to_var_name(name: &str, copy_nr: usize) -> String {
    format!("{}:{}", name, copy_nr)
}

/// Formats the name of a struct with generics.
/// The struct names will be prepended with its generics as a comma seperated
/// list of the new real types that have replaced the generics.
///
/// Example A struct with two generics K and V:
///    TestStruct<K,V>
/// This would be the exact format, case sensitive, no spaces etc.
pub fn to_generic_struct_name(old_struct_name: &str, generics: &Generics) -> String {
    if generics.is_empty() {
        old_struct_name.to_owned()
    } else {
        format!("{}{}", old_struct_name, generics.to_string())
    }
}
