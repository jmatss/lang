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

/// Looks through all compare operations (`>`, `<`, `>=` & `<=`) operations.
/// If the operands are ADTs, converts the binary operation into a function call
/// to the `cmp()` function found in the `Cmp<T>` trait.
///
/// If the ADTs doesn't implement the `Cmp<T>` trait, an error will be reported.
/// This `CmpAnalyzer` should be ran before `EqAnalyzer` since this analyzer
/// might create new eq/neq operations which then needs to be re-written by the
/// `EqAnalyzer`.
pub struct CmpAnalyzer {
    errors: Vec<LangError>,
}

impl CmpAnalyzer {
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn convert_adt_cmp(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) -> LangResult<()> {
        // Assume that lhs and rhs have the same type. This is ensures during
        // the type inference stage.
        let type_id = if let Expr::Op(Op::BinOp(BinOp { lhs, .. })) = expr {
            lhs.get_expr_type()?
        } else {
            unreachable!();
        };

        let mut ty_env_guard = ctx.ty_env.lock().unwrap();

        if is_primitive(&ty_env_guard, type_id)?
            || is_pointer(&ty_env_guard, type_id)?
            || is_array(&ty_env_guard, type_id)?
            || is_enum(&ty_env_guard, type_id)?
        {
            return Ok(());
        }

        // The `type_id` should be an ADT at this point since it isn't a primitive,
        // pointer or array. All types should be solved.
        let adt_path = get_ident(&ty_env_guard, type_id)?.unwrap();
        let adt = ctx.ast_ctx.get_adt(&ty_env_guard, &adt_path)?;
        let adt = adt.read().unwrap();

        // The generic type of the `Cmp` trait should be equal to the current ADT
        // type.
        let mut cmp_trait_gens = Generics::new();
        cmp_trait_gens.insert_type(type_id);
        let cmp_trait_path =
            LangPath::new(vec!["std".into(), "Cmp".into()], expr.file_pos().cloned());
        let cmp_trait_path = cmp_trait_path.with_gens(cmp_trait_gens);

        // Since the types might come from different type environment, a textual
        // comparison is done.
        let mut impls_cmp_trait = false;
        let cmp_trait_name = to_string_path(&ty_env_guard, &cmp_trait_path);
        for impl_trait_path in adt.implemented_traits.values() {
            let impl_trait_name = to_string_path(&ty_env_guard, impl_trait_path);
            if cmp_trait_name == impl_trait_name {
                impls_cmp_trait = true;
                break;
            }
        }

        if !impls_cmp_trait {
            let implemented_traits = adt
                .implemented_traits
                .values()
                .map(|path| format!(" * {}", to_string_path(&ty_env_guard, path)))
                .collect::<Vec<_>>();
            return Err(ctx.ast_ctx.err(format!(
                "ADT \"{}\" was used in a compare binary operation but the ADT \
                doesn't implement the required \"{}\" trait. The ADT implements the traits:\n{}",
                to_string_path(&ty_env_guard, &adt_path),
                cmp_trait_name,
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

        let order_type_id = ty_env_guard.id(&Ty::CompoundType(
            InnerTy::Enum(["std".into(), "Order".into()].into()),
            TypeInfo::DefaultOpt(expr.file_pos().cloned()),
        ))?;

        let mut order_lt = UnOp::new(
            UnOperator::EnumAccess("Lt".into(), ctx.block_id),
            Box::new(Expr::Type(order_type_id, expr.file_pos().cloned())),
            expr.file_pos().cloned(),
        );
        order_lt.ret_type = Some(order_type_id);

        let mut order_eq = UnOp::new(
            UnOperator::EnumAccess("Eq".into(), ctx.block_id),
            Box::new(Expr::Type(order_type_id, expr.file_pos().cloned())),
            expr.file_pos().cloned(),
        );
        order_eq.ret_type = Some(order_type_id);

        let mut order_gt = UnOp::new(
            UnOperator::EnumAccess("Gt".into(), ctx.block_id),
            Box::new(Expr::Type(order_type_id, expr.file_pos().cloned())),
            expr.file_pos().cloned(),
        );
        order_gt.ret_type = Some(order_type_id);

        let mut fn_call = FnCall::new(
            "cmp".into(),
            LangPath::empty(),
            vec![this_arg, other_arg],
            None,
            expr.file_pos().cloned(),
        );
        fn_call.ret_type = Some(order_type_id);
        fn_call.is_method = true;
        fn_call.method_adt = Some(type_id);

        // Converts the given primitve comparison into a `cmp` function call
        // and compares to the returned enum `Order` value.
        // (x > y)   =>  (x.cmp(y) == Order::Gt)
        // (x < y)   =>  (x.cmp(y) == Order::Lt)
        // (x >= y)  =>  ((x.cmp(y) == Order::Gt) or (x.cmp(y) == Order::Eq))
        // (x <= y)  =>  ((x.cmp(y) == Order::Lt) or (x.cmp(y) == Order::Eq))
        let fn_call_expr = Expr::FnCall(fn_call);
        let new_expr = match oper {
            BinOperator::Lt => {
                let mut cmp_lt_op = BinOp::new(
                    BinOperator::Eq,
                    Box::new(fn_call_expr),
                    Box::new(Expr::Op(Op::UnOp(order_lt))),
                    expr.file_pos().cloned(),
                );
                cmp_lt_op.ret_type = Some(bool_type_id);

                Expr::Op(Op::BinOp(cmp_lt_op))
            }

            BinOperator::Gt => {
                let mut cmp_gt_op = BinOp::new(
                    BinOperator::Eq,
                    Box::new(fn_call_expr),
                    Box::new(Expr::Op(Op::UnOp(order_gt))),
                    expr.file_pos().cloned(),
                );
                cmp_gt_op.ret_type = Some(bool_type_id);

                Expr::Op(Op::BinOp(cmp_gt_op))
            }

            BinOperator::Lte => {
                let mut cmp_lt_op = BinOp::new(
                    BinOperator::Eq,
                    Box::new(fn_call_expr.clone()),
                    Box::new(Expr::Op(Op::UnOp(order_lt))),
                    expr.file_pos().cloned(),
                );
                cmp_lt_op.ret_type = Some(bool_type_id);

                let mut cmp_eq_op = BinOp::new(
                    BinOperator::Eq,
                    Box::new(fn_call_expr),
                    Box::new(Expr::Op(Op::UnOp(order_eq))),
                    expr.file_pos().cloned(),
                );
                cmp_eq_op.ret_type = Some(bool_type_id);

                let mut cmp_lt_eq_op = BinOp::new(
                    BinOperator::BoolOr,
                    Box::new(Expr::Op(Op::BinOp(cmp_lt_op))),
                    Box::new(Expr::Op(Op::BinOp(cmp_eq_op))),
                    expr.file_pos().cloned(),
                );
                cmp_lt_eq_op.ret_type = Some(bool_type_id);

                Expr::Op(Op::BinOp(cmp_lt_eq_op))
            }

            BinOperator::Gte => {
                let mut cmp_gt_op = BinOp::new(
                    BinOperator::Eq,
                    Box::new(fn_call_expr.clone()),
                    Box::new(Expr::Op(Op::UnOp(order_gt))),
                    expr.file_pos().cloned(),
                );
                cmp_gt_op.ret_type = Some(bool_type_id);

                let mut cmp_eq_op = BinOp::new(
                    BinOperator::Eq,
                    Box::new(fn_call_expr),
                    Box::new(Expr::Op(Op::UnOp(order_eq))),
                    expr.file_pos().cloned(),
                );
                cmp_eq_op.ret_type = Some(bool_type_id);

                let mut cmp_gt_eq_op = BinOp::new(
                    BinOperator::BoolOr,
                    Box::new(Expr::Op(Op::BinOp(cmp_gt_op))),
                    Box::new(Expr::Op(Op::BinOp(cmp_eq_op))),
                    expr.file_pos().cloned(),
                );
                cmp_gt_eq_op.ret_type = Some(bool_type_id);

                Expr::Op(Op::BinOp(cmp_gt_eq_op))
            }

            _ => unreachable!("{:#?}", expr),
        };

        *expr = new_expr;

        Ok(())
    }
}

impl Visitor for CmpAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr, ctx: &mut TraverseCtx) {
        if let Expr::Op(Op::BinOp(BinOp {
            operator: BinOperator::Gt | BinOperator::Lt | BinOperator::Gte | BinOperator::Lte,
            ..
        })) = expr
        {
            if let Err(err) = self.convert_adt_cmp(expr, ctx) {
                self.errors.push(err);
            }
        }
    }
}
