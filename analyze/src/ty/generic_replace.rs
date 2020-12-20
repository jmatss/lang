use common::{
    token::{
        expr::Expr,
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

    fn visit_un_op(&mut self, un_op: &mut UnOp, _ctx: &TraverseContext) {
        // Edge case for struct access. Need to replace thev operators type as well.
        if let UnOperator::StructAccess(.., Some(ty)) = &mut un_op.operator {
            ty.replace_generics_impl(self.generics_impl);
            ty.replace_self(self.old_name, self.gen_struct_ty);
        }
    }
}
