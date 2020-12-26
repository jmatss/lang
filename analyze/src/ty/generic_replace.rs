use common::{
    traverser::TraverseContext,
    ty::{generics::Generics, ty::Ty},
    visitor::Visitor,
};
use log::warn;

pub struct GenericsReplacer<'a> {
    generics_impl: &'a Generics,
    old_name: &'a str,
    new_ty: &'a Ty,
}

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
impl<'a> GenericsReplacer<'a> {
    pub fn new(generics_impl: &'a Generics, old_name: &'a str, new_ty: &'a Ty) -> Self {
        Self {
            generics_impl,
            old_name,
            new_ty,
        }
    }
}

impl<'a> Visitor for GenericsReplacer<'a> {
    fn visit_type(&mut self, ty: &mut Ty, _ctx: &TraverseContext) {
        warn!(
            "REPLACE GEN -- ty: {:#?}, gen_impl: {:#?}, old_name: {}, new_ty: {:#?} ",
            ty, self.generics_impl, self.old_name, self.new_ty
        );
        ty.replace_generics_impl(self.generics_impl);
        ty.replace_self(self.old_name, self.new_ty);
    }
}
