use common::{
    error::LangError,
    token::expr::FnCall,
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
};

/// Converts any method calls on primitives to the corresponding primitive
/// struct. For example a method call on a type containing `InnerTy::u8` would
/// be converted to `InnerTy::Struct(u8)`.
pub struct PrimitiveToAdtAnalyzer {
    errors: Vec<LangError>,
}

impl PrimitiveToAdtAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }
}

impl Visitor for PrimitiveToAdtAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        if !fn_call.is_method {
            return;
        }

        if let Some(adt_type_id) = fn_call.method_adt {
            let mut ty_env_guard = ctx.ty_env.lock().unwrap();
            let ty = match ty_env_guard.ty(adt_type_id) {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let inner_ty = match ty {
                Ty::CompoundType(inner_ty, ..) if inner_ty.is_primitive() => {
                    let ident = inner_ty.get_primitive_ident();
                    InnerTy::Struct(ident.into())
                }
                _ => return,
            };

            let type_id = match ty_env_guard.id(&Ty::CompoundType(inner_ty, TypeInfo::BuiltIn)) {
                Ok(type_id) => type_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            fn_call.method_adt = Some(type_id);
        }
    }
}
