use crate::types::Type;

/// Concatenates a struct name and a method name to create the name that will
/// be used to refer to this function. The name is concatenated with a dash.
pub fn to_method_name(struct_name: &str, method_name: &str) -> String {
    format!("{}-{}", struct_name, method_name)
}

/// Formats the name of a struct with generics. The names of the generic struct
/// implementations will need to be changed so that they are unique.
/// The struct names will be prepended with a colon followed by a comma seperated
/// list of the new real types that have replaced the generics.
///
/// Example:
///    TestStruct<K, V>
///  (K == u64) and (V == String) =>
///    TestStruct:u64,String
pub fn to_generic_struct_name(old_struct_name: &str, generics: &[Type]) -> String {
    let mut new_name: String = old_struct_name.into();
    new_name.push(':');
    new_name.push_str(
        &generics
            .iter()
            .map(|ty| ty.to_string())
            .collect::<Vec<_>>()
            .join(","),
    );
    new_name
}

/// Given a formatted new struct name (see `to_generic_struct_name()`), returns
/// the actual struct name of the struct with the parts about generic types
/// removed.
///
/// Example input:
///   TestStruct:u64,String
/// would return:
///   TestStruct
pub fn from_generic_struct_name(new_struct_name: &str) -> String {
    if let Some(old_struct_name) = new_struct_name.split(':').next() {
        old_struct_name.into()
    } else {
        unreachable!("from_generic_struct_name None: {}", new_struct_name);
    }
}
