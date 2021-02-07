use super::context::TypeContext;
use common::{
    error::LangError,
    file::FilePosition,
    token::{
        ast::AstToken,
        block::{AdtKind, BlockHeader, Fn},
        expr::{AdtInit, ArrayInit, BuiltInCall, Expr, FnCall, Var},
        lit::Lit,
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Modifier,
        stmt::Stmt,
    },
    traverser::TraverseContext,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    type_info::TypeInfo,
    visitor::Visitor,
    BlockId, TypeId,
};
use either::Either;
use log::debug;
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

/// Infers types for exprs that doesn't have a type explicitly set.
/// For more information about the algorithm, see:
///   http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
///
/// This struct only runs the first step of the algorithm which assigns temp
/// types to expressions and populates the "substitution" sets in "TypeContext"
/// that will be used to convert those temp types to "real" ones.
/// That conversion is done in another step by the "TypeSolver".
pub struct TypeInferencer<'a, 'b> {
    type_context: &'a mut TypeContext<'b>,

    /// Keep a copy of the current function which body is being traversed.
    /// This will let the statements/exprs etc. inside the function know
    /// about the types of the parameters and the return type.
    cur_func: Option<Rc<RefCell<Fn>>>,

    /// Contains the current match expression. Its type needs to be the same
    /// as the type in the match cases.
    cur_match_expr: Option<Expr>,

    /// A id used when creating temporary "Unknown" types. This ID will be given
    /// so that they can be identified uniquely. It will be increment for every
    /// new type.
    type_id: usize,

    errors: Vec<LangError>,
}

impl<'a, 'b> TypeInferencer<'a, 'b> {
    pub fn new(type_context: &'a mut TypeContext<'b>) -> Self {
        Self {
            type_context,
            cur_func: None,
            cur_match_expr: None,
            type_id: 0,
            errors: Vec::default(),
        }
    }

    /// Creates a new unknown identifier that will be given to a unkown type.
    /// The new string will containg information about the position of the type
    /// in a file and a free text to give the unknown type some more context for
    /// readability.
    fn new_unknown_ident(&mut self, text: &str) -> TypeId {
        let file_nr = self.type_context.analyze_context.file_pos.file_nr;
        let line_nr = self.type_context.analyze_context.file_pos.line_start;
        let column_nr = self.type_context.analyze_context.file_pos.column_start;
        let offset = self.type_context.analyze_context.file_pos.offset;
        let length = self.type_context.analyze_context.file_pos.length;

        let type_ident = format!(
            "ID:{}-F:{}-R:{}-C:{}-O:{}-L:{}-{}",
            self.type_id, file_nr, line_nr, column_nr, offset, length, text
        );
        self.type_id += 1;
        type_ident
    }

    /// Helper function to insert a new constraint and store a potential error
    /// in `self.errors()`. Use this function do it in a single line func call
    /// instead of having to check for errors every time.
    fn insert_constraint(&mut self, ty_a: &Ty, ty_b: &Ty, root_id: BlockId) {
        if let Err(err) = self.type_context.insert_constraint(ty_a, ty_b, root_id) {
            self.errors.push(err);
        }
    }
}

impl<'a, 'b> Visitor for TypeInferencer<'a, 'b> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.type_context.analyze_context.file_pos =
            ast_token.file_pos().cloned().unwrap_or_default();
    }

    fn visit_eof(&mut self, _ast_token: &mut AstToken, _ctx: &TraverseContext) {
        debug!(
            "Type inference colleting Done.\nSubs: {}",
            self.type_context.pretty_print_subs()
        );
    }

    /// Assigns a "Unknown" type for every expression that doesn't have a type
    /// explicitly set. This new type will then be temporarilty used during this
    /// stage and should be converted/subtituted into a "real" type before this
    /// analyzing step is done.

    fn visit_lit(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        let (lit, ty_opt, type_info) = if let Expr::Lit(lit, ty_opt, file_pos) = expr {
            let type_info = TypeInfo::Lit(file_pos.to_owned());
            (lit, ty_opt, type_info)
        } else {
            unreachable!()
        };

        if ty_opt.is_some() {
            return;
        }

        let inner_ty = match lit {
            Lit::String(_) => {
                // TODO: Have a custom struct "String" instead of "*u8"?
                *ty_opt = Some(Ty::Pointer(
                    Box::new(Ty::CompoundType(
                        InnerTy::U8,
                        Generics::empty(),
                        type_info.clone(),
                    )),
                    type_info,
                ));

                return;
            }

            Lit::Char(_) => InnerTy::Character,
            Lit::Bool(_) => InnerTy::Boolean,
            Lit::Integer(_, radix) => {
                InnerTy::UnknownInt(self.new_unknown_ident("int_literal"), *radix)
            }
            Lit::Float(_) => InnerTy::UnknownFloat(self.new_unknown_ident("float_literal")),
        };

        *ty_opt = Some(Ty::CompoundType(inner_ty, Generics::empty(), type_info));
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &TraverseContext) {
        let var_decl = match self
            .type_context
            .analyze_context
            .get_var(&var.name, ctx.block_id)
        {
            Ok(var_decl) => var_decl,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let var_decl_ty = if let Some(ty) = var_decl.borrow().ty.clone() {
            ty
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Ret type not set for var decl: {:?}", var_decl));
            self.errors.push(err);
            return;
        };

        let var_ty = if let Some(ty) = &var.ty {
            ty.clone()
        } else {
            let new_type = Ty::CompoundType(
                InnerTy::Unknown(self.new_unknown_ident(&format!("var_use({})", var.name))),
                Generics::new(),
                TypeInfo::VarUse(var.file_pos.unwrap()),
            );
            var.ty = Some(new_type.clone());
            new_type
        };

        // Add type constraint between var "use" and var "decl",
        self.insert_constraint(&var_decl_ty, &var_ty, ctx.block_id);
    }

    // TODO: Clean up.
    /// Assign the return type of the function to the function call expr.
    /// Also tie the types of the function parameter to argument types.
    ///
    /// If either the return type or the parameters contains references to a
    /// generic type, creates new "GenericInstance"s of them instead and does NOT
    /// tie them together with a constraint. This is done since a Generic can
    /// have multiple differet types depending on the context, which isn't solvable
    /// through the regular type inference logic.
    fn visit_fn_call(&mut self, fn_call: &mut FnCall, ctx: &TraverseContext) {
        // TODO: Support varargs for fn pointers?
        // TODO: Support named arguments for fn pointers?

        let mut fn_ret_ty = if fn_call.is_fn_ptr_call {
            let var_name = fn_call.name.clone();
            let decl_id = match self
                .type_context
                .analyze_context
                .get_var_decl_scope(&var_name, ctx.block_id)
            {
                Ok(decl_id) => decl_id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (fn_call.name.clone(), decl_id);
            let var = match self.type_context.analyze_context.variables.get(&key) {
                Some(var) => var,
                None => {
                    let err = self.type_context.analyze_context.err(
                        format!(
                            "Unable to find variable named \"{}\" containing a fn ptr in decl scope {}.",
                            var_name, decl_id
                        ),
                    );
                    self.errors.push(err);
                    return;
                }
            };

            let (fn_gens, fn_params, fn_ret_ty) = if let Some(ty) = var.borrow().ty.clone() {
                if let Ty::Fn(gens, params, ret_ty, _) = ty {
                    (gens, params, ret_ty)
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "Variable named \"{}\" expected to contain a fn ptr, but didn't: {:#?}",
                        var_name, ty
                    ));
                    self.errors.push(err);
                    return;
                }
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "No type set for variable named \"{}\" expected to contain a fn ptr.",
                    var_name
                ));
                self.errors.push(err);
                return;
            };

            // Fn pointers doesn't support named arguments, so can zip the args
            // and param types since their indices is always used to map them.
            if fn_call.arguments.len() == fn_params.len() {
                for (arg, param_ty) in fn_call.arguments.iter().zip(fn_params.iter()) {
                    let arg_ty = match arg.value.get_expr_type() {
                        Ok(ty) => ty.clone(),
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };
                    self.insert_constraint(&arg_ty, param_ty, ctx.block_id);
                }
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Wrong amount of arguments for fn pointer call.\n\
                    Func call: {:#?}\nfn_params: {:#?}",
                    fn_call, fn_params
                ));
                self.errors.push(err);
                return;
            }

            let fn_call_gens = if let Some(gens) = &fn_call.generics {
                gens.iter_types().cloned().collect::<Vec<_>>()
            } else {
                Vec::with_capacity(0)
            };

            if fn_call_gens.len() == fn_gens.len() {
                for (fn_call_gen, fn_gen) in fn_call_gens.iter().zip(fn_gens.iter()) {
                    self.insert_constraint(fn_call_gen, fn_gen, ctx.block_id);
                }
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Wrong amount of generics for fn pointer call.\n\
                    Func call: {:#?}\nfn_gens: {:#?}",
                    fn_call, fn_gens
                ));
                self.errors.push(err);
                return;
            }

            if let Some(mut ty) = fn_ret_ty {
                *ty.file_pos_mut().unwrap() = fn_call.file_pos.unwrap();
                *ty
            } else {
                Ty::CompoundType(
                    InnerTy::Void,
                    Generics::empty(),
                    TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
                )
            }
        } else if fn_call.is_method {
            // Get the "owning" structure type of this method. If it isn't set
            // explicitly, it should be set as a expression in the first argument
            // with the name "this".
            let mut adt_ty = if let Some(adt_ty) = &fn_call.method_adt {
                adt_ty.clone()
            } else if let Some(first_arg) = fn_call.arguments.first() {
                if first_arg.name.as_ref().map_or(false, |name| name == "this") {
                    match first_arg.value.get_expr_type() {
                        Ok(ty) => ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    panic!(
                        "First arg of method with no method_adt set not \"this\": {:#?}",
                        fn_call
                    );
                }
            } else {
                panic!(
                    "No params for method with no method_adt set: {:#?}",
                    fn_call
                );
            };

            // If the ADT type is know and contains generics, this logic will fetch
            // the ADT and combine the names for the generics found in the ADT
            // declaration with potential generic impls in the ADT init/func call.
            if let Ty::CompoundType(inner_ty, generic_types, ..) = &mut adt_ty {
                let generic_names = if let Some(ident) = inner_ty.get_ident() {
                    if let Ok(adt) = self
                        .type_context
                        .analyze_context
                        .get_adt(&ident, ctx.block_id)
                    {
                        if let Some(generics) = &adt.borrow().generics {
                            generics.clone()
                        } else {
                            Vec::default()
                        }
                    } else {
                        Vec::default()
                    }
                } else {
                    Vec::default()
                };

                let mut generics = Generics::new();

                // If the generics impls have been specified, use those
                // to populate the Generics.
                // Else if no generic implements have been specified,
                // create new "GenericInstance"s.
                if generic_types.len_types() > 0 {
                    if generic_names.len() != generic_types.len_types() {
                        let err = self.type_context.analyze_context.err(format!(
                            "Wrong amount of generics for static call. Func call: {:#?}, generic_names: {:#?}",
                            fn_call, generic_names
                        ));
                        self.errors.push(err);
                        return;
                    }

                    generic_names
                        .iter()
                        .cloned()
                        .zip(generic_types.iter_types().cloned())
                        .for_each(|(gen_name, gen_ty)| generics.insert(gen_name, gen_ty));
                } else {
                    for gen_name in generic_names {
                        let unknown_ident =
                            self.new_unknown_ident(&format!("generic_{}", gen_name));
                        let gen_ty =
                            Ty::GenericInstance(gen_name.clone(), unknown_ident, TypeInfo::None);

                        generics.insert(gen_name.clone(), gen_ty);
                    }
                }

                *generic_types = generics;
            }

            // Set the `method_adt` for the function call now that the `method_adt`
            // might have been updated. This call might have no effect if no
            // modifications have been done in the logic above.
            fn_call.method_adt = Some(adt_ty.clone());

            // Insert constraints between the function call argument type and
            // the method parameter types that will be figured out later.
            for (idx, arg) in fn_call.arguments.iter().enumerate() {
                // If the argument is a named argument, give the argument name
                // to the new "UnknownMethodArgument" to try and figure out the
                // position of the argument through it. Otherwise use the index.
                let position = if let Some(arg_name) = &arg.name {
                    Either::Left(arg_name.into())
                } else {
                    Either::Right(idx)
                };

                let arg_ty = Ty::UnknownMethodArgument(
                    Box::new(adt_ty.clone()),
                    fn_call.name.clone(),
                    position,
                    self.new_unknown_ident(""),
                    TypeInfo::DefaultOpt(arg.value.file_pos().cloned()),
                );

                let arg_expr_ty = match arg.value.get_expr_type() {
                    Ok(ty) => ty.clone(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                // TODO: Need to do this for a more general case and it should
                //       prevent any kind of infinite loops. Should be implemented
                //       somewhere else.
                // Don't add a constraint if the argument has the same type as
                // the ADT.
                if arg_expr_ty != adt_ty {
                    self.insert_constraint(&arg_ty, &arg_expr_ty, ctx.block_id);
                }
            }

            // Insert constraints between the function call generic type and
            // the method generic types that will be figured out later.
            if let Some(generics) = &fn_call.generics {
                for (idx, ty) in generics.iter_types().enumerate() {
                    let unknown_ty = Ty::UnknownMethodGeneric(
                        Box::new(adt_ty.clone()),
                        fn_call.name.clone(),
                        idx,
                        self.new_unknown_ident(""),
                        TypeInfo::DefaultOpt(ty.file_pos().cloned()),
                    );

                    self.insert_constraint(&unknown_ty, ty, ctx.block_id);
                }
            }

            // The expected return type of the function call.
            Ty::UnknownAdtMethod(
                Box::new(adt_ty),
                fn_call.name.clone(),
                self.new_unknown_ident(""),
                TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
            )
        } else {
            let func = match self
                .type_context
                .analyze_context
                .get_func(&fn_call.name, ctx.block_id)
            {
                Ok(func) => func,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Iterate through all arguments of the function and match
            // up their types with the parameters of the function.
            // The amount of args/params will already have been checked before,
            // just make sure that this doesn't break for vararg functions.
            // The "similar" logic for methods will be done during type solving
            // in `type_context` since at this point there is no way to know
            // the type of the struct and indirectly the method.
            if let Some(params) = &func.borrow().parameters {
                for (idx, arg) in fn_call.arguments.iter().enumerate() {
                    // If the argument is a named argument, get the index for the
                    // named parameter instead of using the index of its position
                    // in the function call.
                    let inner_idx = if let Some(arg_name) = &arg.name {
                        match self.type_context.analyze_context.get_fn_param_idx(
                            &fn_call.name,
                            &arg_name,
                            ctx.block_id,
                        ) {
                            Ok(idx) => idx,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        idx
                    };

                    let arg_ty = match arg.value.get_expr_type() {
                        Ok(ty) => ty.clone(),
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    if func.borrow().is_var_arg && inner_idx >= params.len() {
                        continue;
                    }

                    let par_ty = if let Some(ty) = &params
                        .get(inner_idx as usize)
                        .map(|param| param.borrow().ty.clone())
                        .flatten()
                    {
                        ty.clone()
                    } else {
                        let err = self.type_context.analyze_context.err(
                            format!(
                                "Type for parameter \"{:?}\" with index {} in function \"{}\" set to None.",
                                arg.name, inner_idx, func.borrow().name
                            ),
                        );
                        self.errors.push(err);
                        return;
                    };

                    self.insert_constraint(&arg_ty, &par_ty, ctx.block_id);
                }
            }

            let func = func.borrow();
            if let Some(mut ty) = func.ret_type.clone() {
                *ty.file_pos_mut().unwrap() = fn_call.file_pos.unwrap();
                ty
            } else {
                Ty::CompoundType(
                    InnerTy::Void,
                    Generics::empty(),
                    TypeInfo::FuncCall(fn_call.file_pos.unwrap()),
                )
            }
        };

        // Replace any "Generic"s with "GenericInstances"s instead so that the
        // "Generic"s doesn't leak out to outside the function. Instead a
        // unique instance of a generic should be used instead. This will allow
        // for multiple different types to be mapped to the same single "Generic".
        if let Some(generics) = fn_ret_ty.get_generics() {
            let mut generics_impl = Generics::new();

            for generic in &generics {
                if let Ty::Generic(ident, ..) = generic {
                    let generic_impl = Ty::GenericInstance(
                        ident.into(),
                        self.type_id.to_string(),
                        TypeInfo::DefaultOpt(generic.file_pos().cloned()),
                    );
                    self.type_id += 1;

                    generics_impl.insert(ident.into(), generic_impl);
                } else {
                    unreachable!("Got non generic from `get_generics()`.");
                }
            }

            fn_ret_ty.replace_generics_impl(&generics_impl)
        }

        // TODO: Is it correct to directly set the return type for the function
        //       call? Should be inserted as a constraint instead? Will this
        //       affect generics?
        fn_call.ret_type = Some(fn_ret_ty);
    }

    fn visit_fn_ptr(&mut self, expr: &mut Expr, ctx: &TraverseContext) {
        let (fn_name, fn_ptr_gens, fn_ty, file_pos) =
            if let Expr::FnPtr(fn_name, gens, fn_ty, file_pos) = expr {
                (fn_name, gens, fn_ty, file_pos)
            } else {
                unreachable!()
            };

        let func = match self
            .type_context
            .analyze_context
            .get_func(fn_name, ctx.block_id)
        {
            Ok(func) => func,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let func = func.borrow();

        let fn_gen_names = if let Some(gens) = &func.generic_names {
            gens.clone()
        } else {
            Vec::with_capacity(0)
        };

        let mut fn_param_tys = if let Some(params) = &func.parameters {
            params
                .iter()
                .map(|var| var.borrow().ty.as_ref().unwrap().clone())
                .collect::<Vec<_>>()
        } else {
            Vec::with_capacity(0)
        };

        let mut fn_ret_ty = func.ret_type.clone();

        if fn_ptr_gens.len() != fn_gen_names.len() {
            let err = self.type_context.analyze_context.err(format!(
                "Function pointer to \"{}\" has incorrect amount of generics. Expected: {}, got: {}",
                fn_name,
                fn_gen_names.len(),
                fn_ptr_gens.len()
            ));
            self.errors.push(err);
            return;
        }

        // Combine the names for the fn declaration generics with the impl types
        // of the function pointer. This will be used to replace the generics
        // in the param and return types of the functions so that the generic
        // decls doesn't "leak" outside the scope of the function.
        let mut gens_impl = Generics::new();
        for (gen_name, gen_ty) in fn_gen_names.iter().zip(fn_ptr_gens.iter_types()) {
            gens_impl.insert(gen_name.clone(), gen_ty.clone());
        }

        fn_param_tys
            .iter_mut()
            .for_each(|ty| ty.replace_generics_impl(&gens_impl));
        if let Some(ret_ty) = &mut fn_ret_ty {
            ret_ty.replace_generics_impl(&gens_impl);
        }

        let new_fn_ty = Ty::Fn(
            fn_ptr_gens.iter_types().cloned().collect::<Vec<_>>(),
            fn_param_tys,
            fn_ret_ty.map(Box::new),
            TypeInfo::DefaultOpt(*file_pos),
        );

        if let Some(fn_ty) = fn_ty {
            if fn_ty != &new_fn_ty {
                let err = self.type_context.analyze_context.err(format!(
                    "Bad function signature for function pointer, fn_name: {}. \
                    fn_ty: {:#?}, new_fn_ty: {:#?}. Function pointer pos: {:#?}",
                    fn_name, fn_ty, new_fn_ty, file_pos
                ));
                self.errors.push(err);
                return;
            }
        } else {
            *fn_ty = Some(new_fn_ty);
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, _ctx: &TraverseContext) {
        let built_in = match self
            .type_context
            .analyze_context
            .get_built_in(&built_in_call.name)
        {
            Ok(built_in) => built_in.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // TODO: Tie the types of the parameters as well. To lazy to implement atm.

        // Make sure that the amount of arguments are equal to the amount of parameters.
        if built_in_call.arguments.len() != built_in.parameters.len() {
            let err = self.type_context.analyze_context.err(format!(
                "Incorrect amount of arguments given for built-in call to \"{}\". Expected amount: {}, got: {}",
                &built_in.name,
                built_in.parameters.len(),
                built_in_call.arguments.len()
            ));
            self.errors.push(err);
            return;
        }

        // Make sure that the amount of generic arguments are equals to the
        // amount of generic parameters.
        if !(built_in.generics.is_none() && built_in_call.generics.is_none()) {
            let built_in_gens = if let Some(built_in_gens) = &built_in.generics {
                built_in_gens
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Built-in function doesn't have generics, but call has generics. Built-in: {:#?}, call: {:#?}",
                    &built_in,
                    &built_in_call
                ));
                self.errors.push(err);
                return;
            };

            let built_in_call_gens = if let Some(built_in_call_gens) = &built_in_call.generics {
                built_in_call_gens
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Built-in function have generic parameters, but call doesn't. Built-in: {:#?}, call: {:#?}",
                    &built_in,
                    &built_in_call
                ));
                self.errors.push(err);
                return;
            };

            if built_in_gens.len() != built_in_call_gens.len() {
                let err = self.type_context.analyze_context.err(format!(
                    "Incorrect amount of generic arguments given for built-in call to \"{}\". Expected amount: {}, got: {}",
                    &built_in.name,
                    built_in_gens.len(),
                    built_in_call_gens.len()
                ));
                self.errors.push(err);
                return;
            }

            for (idx, (built_in_gen, built_in_call_gen)) in built_in_gens
                .iter()
                .zip(built_in_call_gens.iter_types())
                .enumerate()
            {
                if !built_in_gen.is_compatible(built_in_call_gen) {
                    let err = self.type_context.analyze_context.err(format!(
                        "Generic parameter at index {} not compatible. Built-in: {:#?}, call: {:#?}",
                        idx,
                        built_in,
                        built_in_call,
                    ));
                    self.errors.push(err);
                    return;
                }
            }
        }

        // TODO: Temporary ugly hack to make "@type" to work. Should do this in
        //       a different way and somewhere else.
        if &built_in_call.name == "type" {
            built_in_call.ret_type = Some(Ty::Expr(
                Box::new(built_in_call.arguments.first().unwrap().value.clone()),
                TypeInfo::BuiltInCall(built_in_call.file_pos),
            ));
        }

        // TODO: Is it correct to directly set the return type for the function
        //       call? Should be inserted as a constraint instead? Will this
        //       affect generics?
        if built_in_call.ret_type.is_none() {
            built_in_call.ret_type = Some(built_in.ret_type);
        }
    }

    /// Adds the correct type for the ADT init and ties the types of the ADT
    /// members with the type of the ADT init arguments.
    fn visit_adt_init(&mut self, adt_init: &mut AdtInit, ctx: &TraverseContext) {
        let adt = match self
            .type_context
            .analyze_context
            .get_adt(&adt_init.name, ctx.block_id)
        {
            Ok(adt) => adt,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };
        let adt = adt.borrow();

        adt_init.kind = adt.kind.clone();

        // Gets a map if the generics that maps the ident of the generic
        // (ex. "T", "U" etc.) to a new unknown generic type. This is needed
        // to ensure that two members of a ADT with the same ident uses the same
        // unknown generic type. It is also needed to ensure that different ADTs
        // uses different types for the generics.
        let generics = if let Some(generic_names) = &adt.generics {
            let mut generics = Generics::new();

            // If the ADT init call has specified explicitly the implementation
            // types for the generics, use those instead of unknown generics.
            // Currently these explicit types must be solved types.
            if let Some(generics_impl) = &adt_init.generics {
                if generic_names.len() != generics_impl.len() {
                    let err = self.type_context.analyze_context.err(format!(
                        "Wrong amount of generics for ADT init. ADT init: {:#?}, ADT: {:#?}",
                        adt_init, adt
                    ));
                    self.errors.push(err);
                    return;
                }

                for (name, gen_ty) in generic_names.iter().zip(generics_impl.iter_types()) {
                    generics.insert(name.clone(), gen_ty.clone());
                }
            } else {
                for generic_name in generic_names {
                    let unknown_ident =
                        self.new_unknown_ident(&format!("generic_{}", generic_name));
                    let gen_ty =
                        Ty::GenericInstance(generic_name.clone(), unknown_ident, TypeInfo::None);

                    generics.insert(generic_name.clone(), gen_ty);
                }
            }

            generics
        } else {
            Generics::new()
        };

        match &adt_init.ret_type {
            Some(Ty::CompoundType(..)) => {
                // If the type already is set to a compound, use that
                // already set type.
            }
            _ => {
                adt_init.ret_type = Some(Ty::CompoundType(
                    InnerTy::UnknownIdent(adt_init.name.clone(), ctx.block_id),
                    generics.clone(),
                    TypeInfo::Default(adt_init.file_pos.unwrap()),
                ));
            }
        }

        let members = &adt.members;

        // TODO: Move out logic to new function.
        match adt.kind {
            AdtKind::Struct => {
                if members.len() != adt_init.arguments.len() {
                    let err = self.type_context.analyze_context.err(format!(
                        "ADT \"{}\" and ADT init has diff amount of members. ADT#: {:?}, init#: {:?}.",
                        &adt.name,
                        members.len(),
                        adt_init.arguments.len()
                    ));
                    self.errors.push(err);
                    return;
                }

                // TODO: Verify that all members are initialized.

                for (i, arg) in adt_init.arguments.iter_mut().enumerate() {
                    // If a name is set, this is a named member init. Don't use the
                    // iterator index, get the corrent index of the struct field with
                    // the name `arg.name`.
                    let index: usize = if let Some(arg_name) = &arg.name {
                        match self.type_context.analyze_context.get_adt_member_index(
                            &adt.name,
                            arg_name,
                            ctx.block_id,
                        ) {
                            Ok(idx) => idx as usize,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        i
                    };

                    // TODO: Make sure that the ADT init argument is compatible
                    //       with the member ADT type. Currently this doesn't
                    //       get caught until the codegen stage.

                    // Add constraints mapping the type of the ADT init argument
                    // to the corresponding actual ADT member type.
                    match arg.value.get_expr_type() {
                        Ok(arg_ty) => {
                            if let Some(member) = members.get(index) {
                                // Make a copy of the type to allow for multiple
                                // struct inits with different types for the generics.
                                let mut new_member = member.borrow().clone();

                                // Get the "actual" type of the member. If it contains
                                // a generic, it needs to get the actual unknown
                                // generic type from the `unknown_generics` map.
                                // Otherwise reuse the already set type.
                                let member_type = if let Some(ty) = &mut new_member.ty {
                                    ty.replace_generics_impl(&generics);
                                    ty.clone()
                                } else {
                                    let err = self.type_context.analyze_context.err(format!(
                                        "Member \"{:?}\" in struct \"{:?}\" doesn't have a type set.",
                                        members.get(index),
                                        &adt.name
                                    ));
                                    self.errors.push(err);
                                    return;
                                };

                                // Bind init member to actual type in struct definition.
                                self.insert_constraint(&arg_ty, &member_type, ctx.block_id);

                                let arg_file_pos = arg_ty.file_pos().cloned();

                                // Bind type of member to the struct.
                                let unknown_id = self.new_unknown_ident("");
                                self.insert_constraint(
                                    &arg_ty,
                                    &Ty::UnknownAdtMember(
                                        Box::new(
                                            adt_init
                                                .ret_type
                                                .clone()
                                                .expect("Will always be set at this point"),
                                        ),
                                        new_member.name.clone(),
                                        unknown_id,
                                        TypeInfo::DefaultOpt(arg_file_pos),
                                    ),
                                    ctx.block_id,
                                );
                            } else {
                                let err = self.type_context.analyze_context.err(format!(
                                    "Unable to get member at index {} in struct \"{:?}\".",
                                    index, &adt.name
                                ));
                                self.errors.push(err);
                                return;
                            }
                        }
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                }
            }

            AdtKind::Union => {
                if adt_init.arguments.len() > 1 {
                    let err = self.type_context.analyze_context.err(format!(
                        "ADT init of union \"{}\" has more than one argument, has#: {}.",
                        &adt.name,
                        adt_init.arguments.len()
                    ));
                    self.errors.push(err);
                    return;
                } else if adt_init.arguments.is_empty() {
                    let err = self.type_context.analyze_context.err(format!(
                        "ADT init of union \"{}\" has no argument, expected one.",
                        &adt.name,
                    ));
                    self.errors.push(err);
                    return;
                }

                let arg_init = adt_init.arguments.first().unwrap();

                let member_name = if let Some(member_name) = &arg_init.name {
                    member_name
                } else {
                    let err = self.type_context.analyze_context.err(format!(
                        "ADT init of union \"{}\" doesn't have NAMED argument as expected.",
                        &adt.name,
                    ));
                    self.errors.push(err);
                    return;
                };

                let member = match self.type_context.analyze_context.get_adt_member(
                    &adt.name,
                    member_name,
                    ctx.block_id,
                    adt_init.file_pos,
                ) {
                    Ok(member) => member,
                    Err(err) => {
                        if !self.errors.contains(&err) {
                            self.errors.push(err);
                        }
                        return;
                    }
                };

                match arg_init.value.get_expr_type() {
                    Ok(arg_ty) => {
                        // Make a copy of the type to allow for multiple
                        // struct inits with different types for the generics.
                        let mut new_member = member.borrow().clone();

                        let member_type = if let Some(ty) = &mut new_member.ty {
                            ty.replace_generics_impl(&generics);
                            ty.clone()
                        } else {
                            let err = self.type_context.analyze_context.err(format!(
                                "Member \"{:?}\" in union \"{:?}\" doesn't have a type set.",
                                member.borrow(),
                                &adt.name
                            ));
                            self.errors.push(err);
                            return;
                        };

                        self.insert_constraint(&arg_ty, &member_type, ctx.block_id);

                        // Bind type of member arg to the union.
                        let unknown_id = self.new_unknown_ident("");
                        self.insert_constraint(
                            &arg_ty,
                            &Ty::UnknownAdtMember(
                                Box::new(
                                    adt_init
                                        .ret_type
                                        .clone()
                                        .expect("Will always be set at this point"),
                                ),
                                new_member.name,
                                unknown_id,
                                TypeInfo::DefaultOpt(arg_ty.file_pos().cloned()),
                            ),
                            ctx.block_id,
                        );
                    }
                    Err(err) => {
                        self.errors.push(err);
                    }
                }
            }

            _ => unreachable!("ADT init kind: {:?}", adt.kind),
        };
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, ctx: &TraverseContext) {
        let ret_ty = if let Some(ret_ty) = &array_init.ret_type {
            ret_ty.clone()
        } else {
            let new_ty = Ty::CompoundType(
                InnerTy::Unknown(self.new_unknown_ident("array_init")),
                Generics::new(),
                TypeInfo::Default(array_init.file_pos),
            );

            array_init.ret_type = Some(new_ty.clone());
            new_ty
        };

        let mut arg_types = Vec::new();
        for arg in &mut array_init.arguments {
            match arg.value.get_expr_type() {
                Ok(arg_ty) => {
                    arg_types.push(arg_ty.clone());
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            }
        }

        // TODO: What should the type of the index for the array size be?
        let array_index_type = Ty::CompoundType(InnerTy::U32, Generics::empty(), TypeInfo::None);
        let dim = array_init.arguments.len();
        let dim_expr = Expr::Lit(
            Lit::Integer(dim.to_string(), 10),
            Some(array_index_type),
            Some(FilePosition::default()),
        );

        // Add a constraint for all arguments that they are members of the same
        // array type and and also add constraint between all the values in the
        // array init.
        for i in 0..array_init.arguments.len() {
            let left = arg_types.get(i).cloned().unwrap();

            self.insert_constraint(
                &ret_ty,
                &Ty::Array(
                    Box::new(left.clone()),
                    Some(Box::new(dim_expr.clone())),
                    TypeInfo::Default(array_init.file_pos),
                ),
                ctx.block_id,
            );

            for j in i + 1..array_init.arguments.len() {
                let right = arg_types.get(j).cloned().unwrap();
                self.insert_constraint(&left, &right, ctx.block_id);
            }
        }
    }

    /// Adds constraints for binary operations. Most of the bin ops requires
    /// that the lhs and rhs has the same type.
    fn visit_bin_op(&mut self, bin_op: &mut BinOp, ctx: &TraverseContext) {
        // The lhs and rhs exprs will already have been traversed and should
        // have been given a "unknown" type if they didn't have a type already.
        // The "ret_type" of this bin op will also be given a ret_type if it
        // doesn't already have a type set.
        let ret_ty = if let Some(ty) = &bin_op.ret_type {
            ty.clone()
        } else {
            let new_ty = Ty::CompoundType(
                InnerTy::Unknown(self.new_unknown_ident("bin_op")),
                Generics::new(),
                TypeInfo::Default(bin_op.file_pos.unwrap()),
            );

            bin_op.ret_type = Some(new_ty.clone());
            new_ty
        };

        let lhs_ty = match bin_op.lhs.get_expr_type() {
            Ok(lhs_ty) => lhs_ty,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let rhs_ty = match bin_op.rhs.get_expr_type() {
            Ok(rhs_ty) => rhs_ty,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let boolean = Ty::CompoundType(
            InnerTy::Boolean,
            Generics::empty(),
            TypeInfo::Default(bin_op.file_pos.unwrap()),
        );

        match bin_op.operator {
            // The lhs and rhs can be different in these operations, so shouldn't
            // add as constraints.
            // TODO:
            BinOperator::In | BinOperator::Is | BinOperator::Of => (),

            BinOperator::As => {
                if let Expr::Type(rhs_ty, ..) = &*bin_op.rhs {
                    // The rhs of a "as" will be a hardcoded type. The lhs
                    // doesn't have to be the same type (since it should be
                    // casted at this point), but the return type of the bin op
                    // must be the same type as the rhs.
                    // Change the type of the bin op directly so that it takes
                    // precedence during type inferencing.
                    bin_op.ret_type = Some(rhs_ty.clone());
                } else {
                    let err = self
                        .type_context
                        .analyze_context
                        .err(format!("Rhs of \"as\" not a valid type: {:?}", bin_op.rhs));
                    self.errors.push(err);
                }
            }

            BinOperator::Dot | BinOperator::DoubleColon => {
                self.insert_constraint(&ret_ty, &rhs_ty, ctx.block_id);
            }

            // TODO: What ret type should they have?
            BinOperator::Range | BinOperator::RangeInclusive => {
                self.insert_constraint(&lhs_ty, &rhs_ty, ctx.block_id);
            }

            BinOperator::Eq
            | BinOperator::Neq
            | BinOperator::Lt
            | BinOperator::Gt
            | BinOperator::Lte
            | BinOperator::Gte => {
                self.insert_constraint(&ret_ty, &boolean, ctx.block_id);
                self.insert_constraint(&lhs_ty, &rhs_ty, ctx.block_id);
            }

            BinOperator::BoolAnd | BinOperator::BoolOr => {
                self.insert_constraint(&ret_ty, &boolean, ctx.block_id);
                self.insert_constraint(&lhs_ty, &boolean, ctx.block_id);
                self.insert_constraint(&rhs_ty, &boolean, ctx.block_id);
            }

            BinOperator::Add
            | BinOperator::Sub
            | BinOperator::Mul
            | BinOperator::Div
            | BinOperator::Mod
            | BinOperator::BitAnd
            | BinOperator::BitOr
            | BinOperator::BitXor
            | BinOperator::ShiftLeft
            | BinOperator::ShiftRight => {
                self.insert_constraint(&ret_ty, &lhs_ty, ctx.block_id);
                self.insert_constraint(&ret_ty, &rhs_ty, ctx.block_id);
                self.insert_constraint(&lhs_ty, &rhs_ty, ctx.block_id);
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {
        // The expr value of this un op will already have been traversed and should
        // have been given a "unknown" type if it didn't have one type already.
        // The "ret_type" of this un op will also be given a ret_type if it
        // doesn't already have a type set.
        let ret_ty = if let Some(ty) = &un_op.ret_type {
            ty.clone()
        } else {
            let new_ty = Ty::CompoundType(
                InnerTy::Unknown(self.new_unknown_ident("un_op")),
                Generics::new(),
                TypeInfo::Default(un_op.file_pos.unwrap()),
            );

            un_op.ret_type = Some(new_ty.clone());
            new_ty
        };

        let val_ty = match un_op.value.get_expr_type() {
            Ok(rhs_ty) => rhs_ty,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let type_info = TypeInfo::Default(un_op.file_pos.unwrap());

        match &mut un_op.operator {
            UnOperator::Positive
            | UnOperator::Negative
            | UnOperator::BitComplement
            | UnOperator::BoolNot => {
                self.insert_constraint(&ret_ty, &val_ty, ctx.block_id);
            }
            UnOperator::Deref => {
                self.insert_constraint(
                    &Ty::Pointer(Box::new(ret_ty), type_info),
                    &val_ty,
                    ctx.block_id,
                );
            }
            UnOperator::Address => {
                self.insert_constraint(
                    &ret_ty,
                    &Ty::Pointer(Box::new(val_ty), type_info),
                    ctx.block_id,
                );
            }
            UnOperator::ArrayAccess(_) => {
                let unknown_id = self.new_unknown_ident("");
                self.insert_constraint(
                    &ret_ty,
                    &Ty::UnknownArrayMember(Box::new(val_ty), unknown_id, type_info),
                    ctx.block_id,
                );
            }
            UnOperator::UnionIs(member_name, var_decl) => {
                let var_decl_ty = if let Stmt::VariableDecl(var, ..) = var_decl.as_ref() {
                    var.borrow().ty.as_ref().unwrap().clone()
                } else {
                    let err = self
                        .type_context
                        .analyze_context
                        .err(format!("lhs of \"UnionIs\" not a var decl: {:#?}", un_op));
                    self.errors.push(err);
                    return;
                };

                // TODO: Do in cleaner way.
                // The `val_ty` will be the return type of the union member,
                // so need to get the type for the "inner_un_op" which should be
                // a ADT access where the `value` will reference the ADT that is
                // being accessed. That will be inferred to the type of the union.
                let union_ty = if let Expr::Op(Op::UnOp(inner_un_op)) = un_op.value.as_ref() {
                    match inner_un_op.value.get_expr_type() {
                        Ok(union_ty) => union_ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    }
                } else {
                    unreachable!("un_op.value not union access: {:#?}", un_op);
                };

                // Link the type of the new var decl to the type of the member.
                let unknown_id = self.new_unknown_ident("");
                self.insert_constraint(
                    &var_decl_ty,
                    &Ty::UnknownAdtMember(
                        Box::new(union_ty),
                        member_name.clone(),
                        unknown_id,
                        type_info.clone(),
                    ),
                    ctx.block_id,
                );

                // Link the type of the whole expression to a boolean.
                self.insert_constraint(
                    &ret_ty,
                    &Ty::CompoundType(InnerTy::Boolean, Generics::empty(), type_info),
                    ctx.block_id,
                );
            }
            UnOperator::AdtAccess(member_name, ..) | UnOperator::EnumAccess(member_name, ..) => {
                let unknown_id = self.new_unknown_ident("");
                self.insert_constraint(
                    &ret_ty,
                    &Ty::UnknownAdtMember(
                        Box::new(val_ty),
                        member_name.clone(),
                        unknown_id,
                        type_info,
                    ),
                    ctx.block_id,
                );
            }
        }
    }

    fn visit_fn(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Fn(func), ..) = &mut ast_token {
            let func_ref = func.borrow_mut();

            // If this is a method and the first argument is named "this", set
            // the type of it to the ADT that this method belongs to (which already
            // is stored in `method_adt`).
            if let Some(first_arg) = func_ref.parameters.as_ref().and_then(|args| args.first()) {
                let mut first_arg = first_arg.borrow_mut();

                if &first_arg.name == "this" {
                    if let Some(adt_ty) = &func_ref.method_adt {
                        let adt_ty = adt_ty.clone();

                        let ty = if func_ref.modifiers.contains(&Modifier::This) {
                            adt_ty
                        } else if func_ref.modifiers.contains(&Modifier::ThisPointer) {
                            // TODO: What file_pos should this pointer have?
                            Ty::Pointer(Box::new(adt_ty), TypeInfo::None)
                        } else {
                            // TODO: This should be caught somewhere else earlier.
                            //       Keyword is not allowed to be used as parameter
                            //       names. Make this a unreachable at that point.
                            panic!(
                                "First parameter to function named keyword \"this\": {:#?}",
                                func_ref
                            );
                        };

                        first_arg.ty = Some(ty);
                    }
                }
            }

            // Save the current function in a place so that the stmts/exprs in the body
            // can access the types of the parameters and the return type of the func.
            self.cur_func = Some(Rc::clone(func));
        }
    }

    // TODO: Clean up this logic, can merge stuff from `visit_struct` and `visit_impl`.

    // TODO: Implement for unions.
    /// Tie the generics in this specific struct to each other with constraints.
    /// Ties the generics in the struct members, method parameters and method
    /// return types.
    fn visit_struct(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(adt), ..) = &ast_token {
            let adt = adt.borrow();

            // Populate this map with the "Generic(ident)" types where the key
            // is the name of the generic and the value is a list of all the
            // Generics that should have constraints between each other.
            let mut generics: HashMap<_, Vec<_>> = HashMap::default();

            // Gather all "Generic" types found in the members types into the
            // `generics` map. All the generic types in every entry will then
            // be tied together so that they all get infered to the same type.
            for member in &adt.members {
                let member = member.borrow();

                if let Some(ty) = &member.ty {
                    let inner_generics = if let Some(inner_generics) = ty.get_generics() {
                        inner_generics
                    } else {
                        continue;
                    };

                    for gen_ty in inner_generics {
                        let ident = if let Ty::Generic(ident, ..) = gen_ty.clone() {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_ty);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_ty);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_ty]);
                            }
                        }
                    }
                }
            }

            for method in adt.methods.values() {
                // Gather "Generic" types from method parameters.
                if let Some(params) = &method.borrow().parameters {
                    for param in params {
                        if let Some(ty) = param.borrow().ty.as_ref() {
                            let inner_generics = if let Some(inner_generics) = ty.get_generics() {
                                inner_generics
                            } else {
                                continue;
                            };

                            for gen_ty in inner_generics {
                                let ident = if let Ty::Generic(ident, ..) = gen_ty.clone() {
                                    ident
                                } else {
                                    unreachable!("gen_ty not generic: {:#?}", gen_ty);
                                };

                                match generics.entry(ident.clone()) {
                                    Entry::Occupied(mut o) => {
                                        o.get_mut().push(gen_ty);
                                    }
                                    Entry::Vacant(v) => {
                                        v.insert(vec![gen_ty]);
                                    }
                                }
                            }
                        }
                    }
                }

                // Gather "Generic" types from method return type.
                if let Some(ret_ty) = &mut method.borrow().ret_type.as_ref() {
                    let inner_generics = if let Some(inner_generics) = ret_ty.get_generics() {
                        inner_generics
                    } else {
                        continue;
                    };

                    for gen_ty in inner_generics {
                        let ident = if let Ty::Generic(ident, ..) = gen_ty.clone() {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_ty);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_ty);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_ty]);
                            }
                        }
                    }
                }
            }

            // Tie the types of the generics with the same ident to each other.
            for ident_generics in generics.values() {
                for i in 0..ident_generics.len() {
                    let left = ident_generics.get(i).cloned().unwrap();

                    for j in i + 1..ident_generics.len() {
                        let right = ident_generics.get(j).cloned().unwrap();
                        self.insert_constraint(&left, &right, ctx.block_id);
                    }
                }
            }
        }
    }

    fn visit_union(&mut self, ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Union(adt), ..) = &ast_token {
            let adt = adt.borrow();

            // Populate this map with the "Generic(ident)" types where the key
            // is the name of the generic and the value is a list of all the
            // Generics that should have constraints between each other.
            let mut generics: HashMap<_, Vec<_>> = HashMap::default();

            // Gather all "Generic" types found in the members types into the
            // `generics` map. All the generic types in every entry will then
            // be tied together so that they all get infered to the same type.
            for member in &adt.members {
                let member = member.borrow();

                if let Some(ty) = &member.ty {
                    let inner_generics = if let Some(inner_generics) = ty.get_generics() {
                        inner_generics
                    } else {
                        continue;
                    };

                    for gen_ty in inner_generics {
                        let ident = if let Ty::Generic(ident, ..) = gen_ty.clone() {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_ty);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_ty);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_ty]);
                            }
                        }
                    }
                }
            }

            for method in adt.methods.values() {
                // Gather "Generic" types from method parameters.
                if let Some(params) = &method.borrow().parameters {
                    for param in params {
                        if let Some(ty) = param.borrow().ty.as_ref() {
                            let inner_generics = if let Some(inner_generics) = ty.get_generics() {
                                inner_generics
                            } else {
                                continue;
                            };

                            for gen_ty in inner_generics {
                                let ident = if let Ty::Generic(ident, ..) = gen_ty.clone() {
                                    ident
                                } else {
                                    unreachable!("gen_ty not generic: {:#?}", gen_ty);
                                };

                                match generics.entry(ident.clone()) {
                                    Entry::Occupied(mut o) => {
                                        o.get_mut().push(gen_ty);
                                    }
                                    Entry::Vacant(v) => {
                                        v.insert(vec![gen_ty]);
                                    }
                                }
                            }
                        }
                    }
                }

                // Gather "Generic" types from method return type.
                if let Some(ret_ty) = &mut method.borrow().ret_type.as_ref() {
                    let inner_generics = if let Some(inner_generics) = ret_ty.get_generics() {
                        inner_generics
                    } else {
                        continue;
                    };

                    for gen_ty in inner_generics {
                        let ident = if let Ty::Generic(ident, ..) = gen_ty.clone() {
                            ident
                        } else {
                            unreachable!("gen_ty not generic: {:#?}", gen_ty);
                        };

                        match generics.entry(ident.clone()) {
                            Entry::Occupied(mut o) => {
                                o.get_mut().push(gen_ty);
                            }
                            Entry::Vacant(v) => {
                                v.insert(vec![gen_ty]);
                            }
                        }
                    }
                }
            }

            // Tie the types of the generics with the same ident to each other.
            for ident_generics in generics.values() {
                for i in 0..ident_generics.len() {
                    let left = ident_generics.get(i).cloned().unwrap();

                    for j in i + 1..ident_generics.len() {
                        let right = ident_generics.get(j).cloned().unwrap();
                        self.insert_constraint(&left, &right, ctx.block_id);
                    }
                }
            }
        }
    }

    /// Need to make sure that a return statement has the same type as the
    /// function return type. Add it as a constraint.
    fn visit_return(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::Return(expr_opt, ..) = stmt {
            if let Some(func) = &self.cur_func {
                let func_ret_ty = if let Some(ty) = &func.borrow().ret_type {
                    ty.clone()
                } else {
                    // TODO: Where should this pos be fetched from?
                    Ty::CompoundType(InnerTy::Void, Generics::empty(), TypeInfo::None)
                };

                let expr_ty = match self.type_context.get_expr_type(expr_opt.as_ref()) {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.insert_constraint(&func_ret_ty, &expr_ty, ctx.block_id);
            } else {
                let err = self
                    .type_context
                    .analyze_context
                    .err("Unable to get cur func when looking at return stmt type.".into());
                self.errors.push(err);
            }
        }
    }

    // TODO: Write when yield gets implemented.
    fn visit_yield(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {}

    /// Save the current match expr in a place so that the match cases in the body
    /// can access the type of the expr.
    fn visit_match(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Match(expr), ..) = &ast_token {
            self.cur_match_expr = Some(expr.clone());
        }
    }

    /// Need to make sure that the match expr and the match case exprs have the
    /// same type. Add it as a constraint.
    fn visit_match_case(&mut self, mut ast_token: &mut AstToken, ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::MatchCase(match_case_expr), ..) = &mut ast_token {
            if let Some(match_expr) = self.cur_match_expr.clone() {
                let match_expr_ty = match match_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                if let Some(match_case_expr) = match_case_expr {
                    let case_expr_ty = match match_case_expr.get_expr_type() {
                        Ok(expr_ty) => expr_ty,
                        Err(err) => {
                            self.errors.push(err);
                            return;
                        }
                    };

                    self.insert_constraint(&match_expr_ty, &case_expr_ty, ctx.block_id);
                }
            } else {
                let err = self
                    .type_context
                    .analyze_context
                    .err("Unable to get cur match expr when looking at match case type.".into());
                self.errors.push(err);
            }
        }
    }

    /// The types of the lhs and rhs of a assignment should be of the same type.
    /// Add it as a constraint.
    fn visit_assignment(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::Assignment(_, lhs, rhs, ..) = stmt {
            debug!("ASSIGNMENT\nlhs: {:#?}\nrhs: {:#?}", lhs, rhs);

            let lhs_ty = match lhs.get_expr_type() {
                Ok(lhs_ty) => lhs_ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let rhs_ty = match rhs.get_expr_type() {
                Ok(rhs_ty) => rhs_ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.insert_constraint(&lhs_ty, &rhs_ty, ctx.block_id);
        }
    }

    /// The types of the lhs and rhs of a variable declaration with a init value
    /// should be of the same type, add as constraints.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::VariableDecl(var, ..) = stmt {
            let mut var = var.borrow_mut();

            // No way to do type inference of rhs on var decl with no init value.
            let rhs_ty_opt = if var.value.is_some() {
                match self
                    .type_context
                    .get_expr_type(var.value.clone().map(|x| *x).as_ref())
                {
                    Ok(rhs_ty) => Some(rhs_ty),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                }
            } else {
                None
            };

            // Create a unkown type if a type isn't already set. For simplicity
            // this new type will always be set, but it will contain the old type
            // if a type was already set.
            let new_type = if var.ty.is_some() {
                var.ty.clone()
            } else {
                // If the type isn't hardcoded, there are no file position since
                // it doesn't exist in the source code. In that case use the
                // position of the variable identifier instead.
                Some(Ty::CompoundType(
                    InnerTy::Unknown(self.new_unknown_ident(&format!("var_decl({})", var.name))),
                    Generics::new(),
                    TypeInfo::VarDecl(var.file_pos.unwrap(), false),
                ))
            };
            var.ty = new_type;

            let lhs_ty = if let Some(ty) = &var.ty {
                ty.clone()
            } else {
                let err = self
                    .type_context
                    .analyze_context
                    .err(format!("Lhs of var decl has no type: {:?}", var.ty));
                self.errors.push(err);
                return;
            };

            // Add constraints only if this var decl has a init value.
            if let Some(rhs_ty) = rhs_ty_opt {
                self.insert_constraint(&lhs_ty, &rhs_ty, ctx.block_id);
            }
        }
    }

    fn visit_inc(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::Increment(expr, ..) = stmt {
            let expr_ty = match expr.get_expr_type() {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let int_ty = Ty::CompoundType(
                InnerTy::UnknownInt(self.new_unknown_ident("increment"), 10),
                Generics::new(),
                TypeInfo::Default(*expr_ty.file_pos().unwrap()),
            );

            self.insert_constraint(&expr_ty, &int_ty, ctx.block_id);
        }
    }

    fn visit_dec(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::Increment(expr, ..) = stmt {
            let expr_ty = match expr.get_expr_type() {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let int_ty = Ty::CompoundType(
                InnerTy::UnknownInt(self.new_unknown_ident("decrement"), 10),
                Generics::new(),
                TypeInfo::Default(*expr_ty.file_pos().unwrap()),
            );

            self.insert_constraint(&expr_ty, &int_ty, ctx.block_id);
        }
    }
}
