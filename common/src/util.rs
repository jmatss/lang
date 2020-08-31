/// Concatenates a struct name and a method name to create the name that will
/// be used to refer to this function. The name is concatenated with a dash.
pub fn to_method_name(struct_name: &str, method_name: &str) -> String {
    format!("{}-{}", struct_name, method_name)
}
