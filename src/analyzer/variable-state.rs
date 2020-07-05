pub struct VariableState<'a> {
    tokens: Vec<&'a Token>,
    base_type: Option<TypeStruct>,
}
