use std::cell::RefCell;

use common::{
    error::LangError,
    path::LangPath,
    token::{
        ast::AstToken,
        block::BlockHeader,
        expr::{Argument, Expr, FnCall},
        op::{BinOperator, Op},
    },
    traverser::TraverseContext,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    type_info::TypeInfo,
    visitor::Visitor,
};

use crate::AnalyzeContext;

/// Iterates through all method calls and inserts "this"/"self" into the calls
/// as the first argument. The bin ops representing the method call will be
/// transformed into a single FunctionCall expr where the lhs will have been
/// moved into the first parameter of the function call.
///
/// This step also checks for function calls that are done on variables containing
/// function pointers rather than functions. A bool flag will be set in those calls.
pub struct MethodAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,

    /// Contains the generics for the ADT of the latest traversed impl block.
    impl_generics: Option<Generics>,
    /// Contains the generics for the fn of the latest traversed fn block.
    fn_generics: Option<Generics>,

    errors: Vec<LangError>,
}

// TODO: Where should the name "this" be fetched from?
const THIS_VAR_NAME: &str = "this";

impl<'a> MethodAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            impl_generics: None,
            fn_generics: None,
            errors: Vec::default(),
        }
    }
}

impl<'a> Visitor for MethodAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// Modify the AST for any method calls (NOT static) represented as binary
    /// operations into a FuncCall expression.
    ///
    /// This function will also add "this" as the first argument of the method
    /// calls that are called on a instance. This can either be "this" by value
    /// or by pointer depending on the modified attached to the method.
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &mut TraverseContext) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let Some(method_call) = bin_op.rhs.eval_to_fn_call() {
                if let BinOperator::Dot = bin_op.operator {
                    let arg = Argument::new(Some(THIS_VAR_NAME.into()), None, *bin_op.lhs.clone());

                    method_call.arguments.insert(0, arg);
                    method_call.is_method = true;

                    *expr = Expr::FnCall(method_call.clone());
                }
            }
        }
    }

    fn visit_impl(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseContext) {
        if let AstToken::Block(BlockHeader::Implement(adt_name, ..), _, id, _) = ast_token {
            let analyze_context = self.analyze_context.borrow();

            let module = match analyze_context.get_module(*id) {
                Ok(Some(module)) => module,
                Ok(None) => LangPath::default(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let last_part = adt_name.last().unwrap();
            let path = module.clone_push(&last_part.0, None);

            let adt = match analyze_context.get_adt(&path, *id) {
                Ok(adt) => adt,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.impl_generics = adt.borrow().generics.clone();
        }
    }

    fn visit_fn(&mut self, ast_token: &mut AstToken, _ctx: &mut TraverseContext) {
        if let AstToken::Block(BlockHeader::Fn(func), ..) = ast_token {
            self.fn_generics = func.borrow().generics.clone();

            // This isn't a method, reset the generics for impl blocks.
            // This ensures that the function calls traversed in this function
            // doesn't use any generics from a impl block.
            if func.borrow().method_adt.is_none() && self.impl_generics.is_some() {
                self.impl_generics = None;
            }
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseContext) {
        if self
            .analyze_context
            .borrow()
            .get_var(&fn_call.name, ctx.block_id)
            .is_ok()
        {
            fn_call.is_fn_ptr_call = true;
            return;
        }

        // This function call can either be a static function call on a ADT or
        // a call on a stand-alone function. The only way to figure it out is
        // to see if the module/path of the function call represents a ADT.
        // If that is the case, this is a static function call on that ADT.
        if fn_call.module.count() > 0 {
            let full_path_opt = if let Ok(adt) = self
                .analyze_context
                .borrow()
                .get_adt_partial(&fn_call.module, ctx.block_id)
            {
                let adt = adt.borrow();
                let fn_call_gens = fn_call.module.last().unwrap().1.as_ref();
                Some(adt.module.clone_push(&adt.name, fn_call_gens))
            } else {
                None
            };

            if let Some(full_path) = full_path_opt {
                let type_id = match self
                    .analyze_context
                    .borrow_mut()
                    .ty_env
                    .id(&Ty::CompoundType(
                        InnerTy::UnknownIdent(full_path, ctx.block_id),
                        Generics::empty(),
                        TypeInfo::Default(ctx.file_pos.to_owned()),
                    )) {
                    Ok(type_id) => type_id,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                fn_call.is_method = true;
                fn_call.method_adt = Some(type_id);

                return;
            }
        }

        // If the first part of the "module" for the function call is a generic,
        // it should be treated as static method call on the ADT represented by
        // the generic.
        if fn_call.module.count() == 1 {
            let first_part = fn_call.module.first().unwrap();
            let possible_generic_name = &first_part.0;

            if let Some(fn_gens) = &self.fn_generics {
                if fn_gens.contains(possible_generic_name) {
                    let type_id = match self.analyze_context.borrow_mut().ty_env.id(&Ty::Generic(
                        possible_generic_name.into(),
                        TypeInfo::Default(ctx.file_pos.to_owned()),
                    )) {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    fn_call.is_method = true;
                    fn_call.method_adt = Some(type_id);
                }
            }

            if let Some(impl_gens) = &self.impl_generics {
                if impl_gens.contains(possible_generic_name) {
                    let type_id = match self.analyze_context.borrow_mut().ty_env.id(&Ty::Generic(
                        possible_generic_name.into(),
                        TypeInfo::Default(ctx.file_pos.to_owned()),
                    )) {
                        Ok(type_id) => type_id,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    fn_call.is_method = true;
                    fn_call.method_adt = Some(type_id);
                }
            }
        }
    }
}
