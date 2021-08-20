use common::{
    error::LangError,
    path::LangPath,
    token::{
        block::{Block, BlockHeader},
        expr::{Argument, Expr, FnCall},
        op::{BinOperator, Op},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty, type_info::TypeInfo},
};

use crate::util::generics::combine_generics_adt;

/// Iterates through all method calls and inserts "this"/"self" into the calls
/// as the first argument. The bin ops representing the method call will be
/// transformed into a single FunctionCall expr where the lhs will have been
/// moved into the first parameter of the function call.
///
/// The `this` in static calls (ex. `this::func()`) will have the `this` be
/// re-written to the ADT that it represents.
///
/// This step also checks for function calls that are done on variables containing
/// function pointers rather than functions. A bool flag will be set in those calls.
pub struct MethodAnalyzer {
    /// Contains the name for the ADT of the latest traversed impl block.
    adt_path: Option<LangPath>,

    /// Contains the generics for the ADT of the latest traversed impl block.
    adt_gens: Option<Generics>,

    /// Contains the generics for the fn of the latest traversed fn block.
    fn_generics: Option<Generics>,

    errors: Vec<LangError>,
}

// TODO: Where should the name "this" be fetched from?
const THIS_VAR_NAME: &str = "this";

impl MethodAnalyzer {
    pub fn new() -> Self {
        Self {
            adt_path: None,
            adt_gens: None,
            fn_generics: None,
            errors: Vec::default(),
        }
    }
}

impl Visitor for MethodAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
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
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &mut TraverseCtx) {
        if let Expr::Op(Op::BinOp(bin_op)) = expr {
            if let Some(method_call) = bin_op.rhs.eval_to_fn_call() {
                if let BinOperator::Dot = bin_op.operator {
                    let arg = Argument::new(Some(THIS_VAR_NAME.into()), None, *bin_op.lhs.clone());

                    method_call.arguments.insert(0, arg);
                    method_call.is_method = true;
                    method_call.is_fn_ptr_call = false;

                    *expr = Expr::FnCall(method_call.clone());
                }
            }
        }
    }

    fn visit_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { header, id, .. } = block;

        let (path_without_gens, adt_gens) = match header {
            BlockHeader::Implement(adt_path, ..) => {
                let module = match ctx.ast_ctx.get_module(*id) {
                    Ok(Some(module)) => module,
                    Ok(None) => LangPath::default(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let last_part = adt_path.last().unwrap();
                let path = module.clone_push(&last_part.0, None, adt_path.file_pos);

                let adt = match ctx.ast_ctx.get_adt(&ctx.ty_env.lock().unwrap(), &path) {
                    Ok(adt) => adt,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let adt = adt.read().unwrap();
                (path, adt.generics.clone())
            }

            BlockHeader::Struct(adt) | BlockHeader::Union(adt) => {
                let adt = adt.read().unwrap();
                let path = adt.module.clone_push(&adt.name, None, Some(adt.file_pos));

                (path, adt.generics.clone())
            }

            _ => return,
        };

        self.adt_path = Some(path_without_gens);
        self.adt_gens = adt_gens;
    }

    fn visit_fn(&mut self, block: &mut Block, _ctx: &mut TraverseCtx) {
        if let Block {
            header: BlockHeader::Fn(func),
            ..
        } = block
        {
            self.fn_generics = func.as_ref().read().unwrap().generics.clone();

            // This isn't a method, reset the variable for any impl block
            // because we have left the impl block.
            if func.as_ref().read().unwrap().method_adt.is_none() {
                self.adt_path = None;
                self.adt_gens = None;
            }
        }
    }

    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &mut TraverseCtx) {
        // This function call can either be a static function call on a ADT or
        // a call on a stand-alone function. The only way to figure it out is
        // to see if the module/path of the function call represents a ADT.
        // If that is the case, this is a static function call on that ADT.
        if fn_call.module.count() > 0 {
            let potential_adt_path = &fn_call.module;
            let full_path_opt = if let Ok(adt) = ctx.ast_ctx.get_adt_partial(
                &ctx.ty_env.lock().unwrap(),
                &potential_adt_path.without_gens(),
                ctx.block_id,
            ) {
                let adt = adt.as_ref().read().unwrap();
                let fn_call_gens = fn_call.module.last().unwrap().1.as_ref();
                Some(
                    adt.module
                        .clone_push(&adt.name, fn_call_gens, fn_call.file_pos),
                )
            } else {
                None
            };

            if let Some(full_path) = full_path_opt {
                let type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                    InnerTy::UnknownIdent(full_path, ctx.block_id),
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

        // If the first part is "this", this is a static function call on the
        // current ADT.
        // If the first part of the "module" for the function call is a generic,
        // it should be treated as static method call on the ADT represented by
        // the generic.
        if fn_call.module.count() == 1 {
            let first_part = fn_call.module.first().unwrap();
            let possible_gen_or_this = &first_part.0;

            if let Some(adt_path) = &self.adt_path {
                if possible_gen_or_this == "this" {
                    let this_gens = first_part.generics().as_ref();
                    let fn_call_path = fn_call.module.clone_push(
                        &fn_call.name,
                        fn_call.generics.as_ref(),
                        fn_call.file_pos,
                    );

                    let new_gens =
                        match combine_generics_adt(ctx, adt_path, this_gens, &fn_call_path) {
                            Ok(new_gens) => new_gens,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                    let adt_path_with_gens = adt_path.with_gens_opt(new_gens);
                    let type_id = match ctx.ty_env.lock().unwrap().id(&Ty::CompoundType(
                        InnerTy::UnknownIdent(adt_path_with_gens, ctx.block_id),
                        TypeInfo::None,
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

            if let Some(fn_gens) = &self.fn_generics {
                if fn_gens.contains(possible_gen_or_this) {
                    let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                    let type_id = match ctx.ty_env.lock().unwrap().id(&Ty::Generic(
                        possible_gen_or_this.into(),
                        unique_id,
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

            if let Some(impl_gens) = &self.adt_gens {
                if impl_gens.contains(possible_gen_or_this) {
                    let unique_id = ctx.ty_env.lock().unwrap().new_unique_id();
                    let type_id = match ctx.ty_env.lock().unwrap().id(&Ty::Generic(
                        possible_gen_or_this.into(),
                        unique_id,
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
        }

        // OBS! There is a possiblity that a variable containing a function pointer
        // and a normal function have the same name. In those cases, this will
        // incorrectly set `is_fn_ptr_call` to true. This can then be set to false
        // again in the `visit_expr()` function where we have more context about
        // how this `fn_call` is "used" in the AST.
        if ctx.ast_ctx.get_var(&fn_call.name, ctx.block_id).is_ok() {
            fn_call.is_fn_ptr_call = true;
        }
    }
}
