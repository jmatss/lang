use common::{
    error::{LangError, LangResult},
    path::LangPath,
    token::{
        expr::{Argument, Expr, FnCall},
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{
        generics::Generics,
        get::get_ident,
        inner_ty::InnerTy,
        is::{is_array, is_enum, is_pointer, is_primitive},
        to_string::to_string_path,
        ty::Ty,
        type_info::TypeInfo,
    },
};

/// Looks through all eq/neq (`==` & `!=`) operations. If the operands are ADTs,
/// converts the binary operation into a function call to the `eq()` function
/// found in the `Eq<T>` trait.
///
/// If the ADTs doesn't implement the `Eq<T>` trait, a error will be reported.
pub struct EqAnalyzer {
    errors: Vec<LangError>,
}

impl EqAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn convert_adt_eq(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) -> LangResult<()> {
        // Assume that lhs and rhs have the same type. This is ensures during
        // the type inference stage.
        let type_id = if let Expr::Op(Op::BinOp(BinOp { lhs, .. })) = expr {
            lhs.get_expr_type()?
        } else {
            unreachable!();
        };

        let mut ty_env_guard = ctx.ty_env.lock();

        if is_primitive(&ty_env_guard, type_id)?
            || is_pointer(&ty_env_guard, type_id)?
            || is_array(&ty_env_guard, type_id)?
            || is_enum(&ty_env_guard, type_id)?
        {
            return Ok(());
        }

        // The `type_id` should be an ADT at this point since it isn't a primitive,
        // pointer or array should be solved.
        let adt_path = get_ident(&ty_env_guard, type_id)?.unwrap();
        let adt = ctx.ast_ctx.get_adt(&ty_env_guard, &adt_path)?;
        let adt = adt.read();

        // The generic type of the `Eq` trait should be equal to the current ADT
        // type.
        let mut eq_trait_gens = Generics::new();
        eq_trait_gens.insert_type(type_id);
        let eq_trait_path =
            LangPath::new(vec!["std".into(), "Eq".into()], expr.file_pos().cloned());
        let eq_trait_path = eq_trait_path.with_gens(eq_trait_gens);

        // Since the types might come from different type environment, a textual
        // comparison is done.
        let mut impls_eq_trait = false;
        let eq_trait_name = to_string_path(&ty_env_guard, &eq_trait_path);
        for impl_trait_path in adt.implemented_traits.values() {
            let impl_trait_name = to_string_path(&ty_env_guard, impl_trait_path);
            if eq_trait_name == impl_trait_name {
                impls_eq_trait = true;
                break;
            }
        }

        if !impls_eq_trait {
            let implemented_traits = adt
                .implemented_traits
                .values()
                .map(|path| format!(" * {}", to_string_path(&ty_env_guard, path)))
                .collect::<Vec<_>>();
            return Err(ctx.ast_ctx.err(format!(
                "ADT \"{}\" was used in a eq/neq binary operation but the ADT \
                doesn't implement the required \"{}\" trait. The ADT implements the traits:\n{}",
                to_string_path(&ty_env_guard, &adt_path),
                eq_trait_name,
                implemented_traits.join("\n"),
            )));
        }

        let (lhs, rhs, oper) = if let Expr::Op(Op::BinOp(BinOp {
            lhs, rhs, operator, ..
        })) = expr
        {
            (*lhs.clone(), *rhs.clone(), *operator)
        } else {
            unreachable!();
        };

        let this_arg = Argument::new(Some("this".into()), None, lhs);
        let other_arg = Argument::new(None, None, rhs);
        let bool_type_id =
            ty_env_guard.id(&Ty::CompoundType(InnerTy::Boolean, TypeInfo::BuiltIn))?;

        let mut fn_call = FnCall::new(
            "eq".into(),
            LangPath::empty(),
            vec![this_arg, other_arg],
            None,
            expr.file_pos().cloned(),
        );
        fn_call.ret_type = Some(bool_type_id);
        fn_call.is_method = true;
        fn_call.method_adt = Some(type_id);

        let mut new_expr = Expr::FnCall(fn_call);
        if let BinOperator::Neq = oper {
            let is_const = new_expr.is_const();
            let mut un_op = UnOp::new(
                UnOperator::BoolNot,
                Box::new(new_expr),
                is_const,
                expr.file_pos().cloned(),
            );
            un_op.ret_type = Some(bool_type_id);

            new_expr = Expr::Op(Op::UnOp(un_op));
        }

        *expr = new_expr;

        Ok(())
    }
}

impl Visitor for EqAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        if let Expr::Op(Op::BinOp(BinOp {
            operator: BinOperator::Eq | BinOperator::Neq,
            ..
        })) = expr
        {
            if let Err(err) = self.convert_adt_eq(expr, ctx) {
                self.errors.push(err);
            }
        }
    }
}
