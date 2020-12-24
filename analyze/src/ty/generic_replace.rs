use common::{
    token::{
        expr::{BuiltInCall, Expr, StructInit},
        op::{UnOp, UnOperator},
    },
    traverser::TraverseContext,
    ty::{generics::Generics, ty::Ty},
    visitor::Visitor,
};

pub struct GenericsReplacer<'a> {
    generics_impl: &'a Generics,
    old_name: &'a str,
    gen_struct_ty: &'a Ty,
}

/// Used when replacing generics in methods containing to a specific generic
/// implementation. This will be used to replace all types in the body of the
/// methods.
impl<'a> GenericsReplacer<'a> {
    pub fn new(generics_impl: &'a Generics, old_name: &'a str, gen_struct_ty: &'a Ty) -> Self {
        Self {
            generics_impl,
            old_name,
            gen_struct_ty,
        }
    }
}

impl<'a> Visitor for GenericsReplacer<'a> {
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Ok(ty) = expr.get_expr_type_mut() {
            ty.replace_generics_impl(self.generics_impl);
            ty.replace_self(self.old_name, self.gen_struct_ty);
        }
    }

    // TODO: Need to replace some types that isn't expression. Should there be
    //       a `visit_type()` or something similar in the `Visitor` trait that
    //       lets one handle all cases in a single function?
    //       Would probably be best to implement, then it could be used for
    //       multiple Analyze structs.

    fn visit_un_op(&mut self, un_op: &mut UnOp, _ctx: &TraverseContext) {
        // Edge case for struct access. Need to replace thev operators type as well.
        if let UnOperator::StructAccess(.., Some(ty)) = &mut un_op.operator {
            ty.replace_generics_impl(self.generics_impl);
            ty.replace_self(self.old_name, self.gen_struct_ty);
        }
    }

    fn visit_struct_init(&mut self, struct_init: &mut StructInit, _ctx: &TraverseContext) {
        if let Some(generics) = &mut struct_init.generics {
            for ty in generics.iter_types_mut() {
                ty.replace_generics_impl(self.generics_impl);
                ty.replace_self(self.old_name, self.gen_struct_ty);
            }
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, _ctx: &TraverseContext) {
        if let Some(generics) = &mut built_in_call.generics {
            for ty in generics.iter_types_mut() {
                ty.replace_generics_impl(self.generics_impl);
                ty.replace_self(self.old_name, self.gen_struct_ty);
            }
        }
    }
}
