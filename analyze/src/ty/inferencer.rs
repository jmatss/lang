use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use common::{
    error::LangError,
    file::FilePosition,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function},
        expr::{ArrayInit, BuiltInCall, Expr, FuncCall, StructInit, Var},
        lit::Lit,
        op::{BinOp, BinOperator, UnOp, UnOperator},
        stmt::Modifier,
        stmt::Stmt,
    },
    traverser::TraverseContext,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
};
use either::Either;
use log::{debug, warn};

use super::context::TypeContext;

// TODO: Better error messages. Ex. if two types arent't compatible,
//       print information about where the types "came" from.

/// Infers types for exprs that doesn't have a type explicitly set.
/// For more information about the algorithm, see:
///   http://smallcultfollowing.com/babysteps/blog/2017/03/25/unification-in-chalk-part-1/
///
/// This struct only runs the first step of the algorithm which assigns temp
/// types to expressions and populates the "substitution" table in the "TypeContext"
/// that will be used to convert those temp types to "real" ones.
/// That conversion is done in another step by the "TypeSolver".
pub struct TypeInferencer<'a, 'b> {
    type_context: &'a mut TypeContext<'b>,

    /// Keep a copy of the current function which body is being traversed.
    /// This will let the statements/exprs etc. inside the function know
    /// about the types of the parameters and the return type.
    cur_func: Option<Rc<RefCell<Function>>>,

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
    fn new_unknown_ident(&mut self, text: &str) -> String {
        let file_nr = self.type_context.analyze_context.file_pos.file_nr;
        let line_nr = self.type_context.analyze_context.file_pos.line_nr;
        let column_nr = self.type_context.analyze_context.file_pos.column_nr;
        let offset = self.type_context.analyze_context.file_pos.offset;
        let length = self.type_context.analyze_context.file_pos.length;

        let type_ident = format!(
            "ID:{}-F:{}-R:{}-C:{}-O:{}-L:{}-{}",
            self.type_id, file_nr, line_nr, column_nr, offset, length, text
        );
        self.type_id += 1;
        type_ident
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

    /// Solve the constraints at the EOF. Also debug log the results.
    fn visit_eof(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.type_context.analyze_context.file_pos = FilePosition::default();

        let mut result = self.type_context.solve_constraints(true);
        if let Err(errors) = &mut result {
            self.errors.append(errors);
        }

        debug!(
            "Type inference Done.\nConstraints: {:#?}\nSubs: {:#?}",
            self.type_context.constraints, self.type_context.substitutions
        );
    }

    /// Assigns a "Unknown" type for every expression that doesn't have a type
    /// explicitly set. This new type will then be temporarilty used during this
    /// stage and should be converted/subtituted into a "real" type before this
    /// analyzing step is done.

    fn visit_lit(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Expr::Lit(lit, gen_ty_opt, ..) = expr {
            if gen_ty_opt.is_none() {
                let inner_ty = match lit {
                    Lit::String(_) => {
                        // TODO: Have a custom struct "String" instead of "*u8"?
                        *gen_ty_opt = Some(Ty::Pointer(Box::new(Ty::CompoundType(
                            InnerTy::U8,
                            Generics::new(),
                        ))));
                        return;
                    }

                    Lit::Char(_) => InnerTy::Character,
                    Lit::Bool(_) => InnerTy::Boolean,
                    Lit::Integer(_, radix) => {
                        InnerTy::UnknownInt(self.new_unknown_ident("int_literal"), *radix)
                    }
                    Lit::Float(_) => InnerTy::UnknownFloat(self.new_unknown_ident("float_literal")),
                };

                let new_gen_ty = Ty::CompoundType(inner_ty, Generics::new());
                *gen_ty_opt = Some(new_gen_ty);
            }
        }
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
            );
            var.ty = Some(new_type.clone());
            new_type
        };

        // Add type constraint between var "use" and var "decl",
        self.type_context.insert_constraint(var_decl_ty, var_ty);
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
    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        let mut func_ret_ty = if func_call.is_method {
            // Get the "owning" structure type of this method. If it isn't set
            // explicitly, it should be set as a expression in the first argument
            // with the name "this".
            let mut structure_ty = if let Some(structure_ty) = &func_call.method_structure {
                structure_ty.clone()
            } else if let Some(first_arg) = func_call.arguments.first() {
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
                        "First arg of method with no method_structure set not \"this\": {:#?}",
                        func_call
                    );
                }
            } else {
                panic!(
                    "No params for method with no method_structure set: {:#?}",
                    func_call
                );
            };

            // If the structure type is know and contains generics, this logic
            // will fetch the structure and combine the names for the generics
            // found in the structure declaration with potential generic impls
            // in the struct init/func call.
            if let Ty::CompoundType(inner_ty, generic_types) = &mut structure_ty {
                match inner_ty {
                    InnerTy::Struct(ident)
                    | InnerTy::Enum(ident)
                    | InnerTy::Interface(ident)
                    | InnerTy::UnknownIdent(ident, ..) => {
                        let generic_names = if let Ok(struct_) = self
                            .type_context
                            .analyze_context
                            .get_struct(ident, ctx.block_id)
                        {
                            struct_
                                .borrow()
                                .generic_params
                                .clone()
                                .unwrap_or_else(Vec::default)
                        } else if let Ok(enum_) = self
                            .type_context
                            .analyze_context
                            .get_enum(ident, ctx.block_id)
                        {
                            enum_.borrow().generics.clone().unwrap_or_else(Vec::default)
                        } else if let Ok(interface) = self
                            .type_context
                            .analyze_context
                            .get_interface(ident, ctx.block_id)
                        {
                            interface
                                .borrow()
                                .generics
                                .clone()
                                .unwrap_or_else(Vec::default)
                        } else {
                            Vec::default()
                        };

                        let mut generics = Generics::new();
                        let mut idx = 0;

                        for (gen_name, gen_ty) in generic_names
                            .iter()
                            .cloned()
                            .zip(generic_types.iter_types().cloned())
                        {
                            generics.insert(gen_name, gen_ty);
                            idx += 1;
                        }

                        // If not all of the generics have implements, just insert
                        // the remanining generics as names without impls.
                        while idx < generic_names.len() {
                            let gen_name = generic_names.get(idx).expect("Known to be inbounds.");
                            generics.insert_lookup(gen_name.clone(), idx);
                            generics.insert_name(gen_name.clone());
                            idx += 1;
                        }

                        *generic_types = generics;
                    }
                    _ => (),
                }
            }

            // Set the `method_structure` for the function call now that the
            // `structure_type` might have been updated. This call might have
            // no effect if no modifications have been done in the logic above.
            func_call.method_structure = Some(structure_ty.clone());

            warn!("DDD");

            // Insert constraints between the function call argument type and
            // the method parameter types that will be figured out later.
            for (idx, arg) in func_call.arguments.iter().enumerate() {
                // If the argument is a named argument, give the argument name
                // to the new "UnknownMethodArgument" to try and figure out the
                // position of the argument through it. Otherwise use the index.
                let position = if let Some(arg_name) = &arg.name {
                    Either::Left(arg_name.into())
                } else {
                    Either::Right(idx)
                };

                let arg_ty = Ty::UnknownMethodArgument(
                    Box::new(structure_ty.clone()),
                    func_call.name.clone(),
                    position,
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
                // the structure.
                if arg_expr_ty != structure_ty {
                    self.type_context.insert_constraint(arg_ty, arg_expr_ty);
                }
            }

            // The expected return type of the function call.
            Ty::UnknownStructureMethod(Box::new(structure_ty), func_call.name.clone())
        } else {
            let func = match self
                .type_context
                .analyze_context
                .get_func(&func_call.name, ctx.block_id)
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
                for (idx, arg) in func_call.arguments.iter().enumerate() {
                    // If the argument is a named argument, get the index for the
                    // named parameter instead of using the index of its position
                    // in the function call.
                    let inner_idx = if let Some(arg_name) = &arg.name {
                        match self.type_context.analyze_context.get_func_param_idx(
                            &func_call.name,
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

                    self.type_context.insert_constraint(arg_ty, par_ty);
                }
            }

            let func = func.borrow();
            if let Some(ty) = func.ret_type.clone() {
                ty
            } else {
                Ty::CompoundType(InnerTy::Void, Generics::new())
            }
        };

        // Replace any "Generic"s with "GenericInstances"s instead so that the
        // "Generic"s doesn't leak out to outside the function. Instead a
        // unique instance of a generic should be used instead. This will allow
        // for multiple different types to be mapped to the same single "Generic".
        if let Some(generics) = func_ret_ty.get_generics() {
            let mut generics_impl = Generics::new();

            for generic in &generics {
                if let Ty::Generic(ident, ..) = generic {
                    let generic_impl =
                        Ty::GenericInstance(ident.into(), self.type_id.to_string(), None);
                    self.type_id += 1;

                    generics_impl.insert(ident.into(), generic_impl);
                } else {
                    unreachable!("Got non generic from `get_generics()`.");
                }
            }

            func_ret_ty.replace_generics_impl(&generics_impl)
        }

        // TODO: Is it correct to directly set the return type for the function
        //       call? Should be inserted as a constraint instead? Will this
        //       affect generics?
        func_call.ret_type = Some(func_ret_ty);
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

        // TODO: Is it correct to directly set the return type for the function
        //       call? Should be inserted as a constraint instead? Will this
        //       affect generics?
        built_in_call.ret_type = Some(built_in.ret_type);
    }

    /// Adds the correct type for the struct init and ties the types of the struct
    /// members with the type of the struct init arguments.
    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        let struct_ = match self
            .type_context
            .analyze_context
            .get_struct(&struct_init.name, ctx.block_id)
        {
            Ok(struct_) => struct_,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let struct_ = struct_.borrow();
        if let Some(members) = &struct_.members {
            // Gets a map if the generics that maps the ident of the generic
            // (ex. "T", "U" etc.) to a new unknown generic type. This is needed
            // to ensure that two members of a struct with the same ident uses
            // the same unknown generic type. It is also needed to ensure that
            // different struct uses different types for the generics.
            let generics = if let Some(generic_names) = &struct_.generic_params {
                let mut generics = Generics::new();

                // If the struct init call has specified explicitly the implementation
                // types for the generics, use those instead of unknown generics.
                // Currently these explicit types must be solved types.
                if let Some(generics_impl) = &struct_init.generics {
                    if generic_names.len() != generics_impl.len() {
                        let err = self.type_context.analyze_context.err(format!(
                            "Wrong amount of generics for struct init. Struct init: {:#?}, struct: {:#?}",
                            struct_init, struct_
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
                        let gen_ty = Ty::GenericInstance(generic_name.clone(), unknown_ident, None);

                        generics.insert(generic_name.clone(), gen_ty);
                    }
                }

                generics
            } else {
                Generics::new()
            };

            match &struct_init.ret_type {
                Some(Ty::CompoundType(..)) => {
                    // If the type already is set to a compound, use that
                    // already set type.
                }
                _ => {
                    struct_init.ret_type = Some(Ty::CompoundType(
                        InnerTy::UnknownIdent(struct_init.name.clone(), ctx.block_id),
                        generics.clone(),
                    ));
                }
            }

            if members.len() != struct_init.arguments.len() {
                let err = self.type_context.analyze_context.err(
                    format!(
                        "Struct \"{}\" and struct init has diff amount of members. Struct#: {:?}, init#: {:?}.",
                        &struct_.name, members.len(), struct_init.arguments.len()
                    ),
                );
                self.errors.push(err);
                return;
            }

            // TODO: Verify that all members are initialized.

            for (i, arg) in struct_init.arguments.iter_mut().enumerate() {
                // If a name is set, this is a named member init. Don't use the
                // iterator index, get the corrent index of the struct field with
                // the name `arg.name`.
                let index: usize = if let Some(arg_name) = &arg.name {
                    match self.type_context.analyze_context.get_struct_member_index(
                        &struct_.name,
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

                // TODO: Make sure that the struct init argument is compatible
                //       with the member struct type. Currently this doesn't
                //       get caught until the codegen stage.

                // Add constraints mapping the type of the struct init argument
                // to the corresponding actual struct member type.
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
                                    &struct_.name
                                ));
                                self.errors.push(err);
                                return;
                            };

                            // Bind init member to actual type in struct definition.
                            self.type_context
                                .insert_constraint(arg_ty.clone(), member_type);

                            // Bind type of member to the struct.
                            self.type_context.insert_constraint(
                                arg_ty.clone(),
                                Ty::UnknownStructureMember(
                                    Box::new(
                                        struct_init
                                            .ret_type
                                            .clone()
                                            .expect("Will always be set at this point"),
                                    ),
                                    new_member.name.clone(),
                                ),
                            );
                        } else {
                            let err = self.type_context.analyze_context.err(format!(
                                "Unable to get member at index {} in struct \"{:?}\".",
                                index, &struct_.name
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
        } else if !struct_init.arguments.is_empty() {
            let err = self.type_context.analyze_context.err(format!(
                "Struct \"{}\" has no members, but struct init specified members.",
                &struct_.name
            ));
            self.errors.push(err);
            return;
        }
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, _ctx: &TraverseContext) {
        let ret_ty = if let Some(ret_ty) = &array_init.ret_type {
            ret_ty.clone()
        } else {
            let new_ty = Ty::CompoundType(
                InnerTy::Unknown(self.new_unknown_ident("array_init")),
                Generics::new(),
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
        let array_index_type = Ty::CompoundType(InnerTy::U32, Generics::new());
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

            self.type_context.insert_constraint(
                ret_ty.clone(),
                Ty::Array(Box::new(left.clone()), Some(Box::new(dim_expr.clone()))),
            );

            for j in i + 1..array_init.arguments.len() {
                let right = arg_types.get(j).cloned().unwrap();
                self.type_context.insert_constraint(left.clone(), right)
            }
        }
    }

    /// Adds constraints for binary operations. Most of the bin ops requires
    /// that the lhs and rhs has the same type.
    fn visit_bin_op(&mut self, bin_op: &mut BinOp, _ctx: &TraverseContext) {
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

        let boolean = Ty::CompoundType(InnerTy::Boolean, Generics::new());

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
                self.type_context.insert_constraint(ret_ty, rhs_ty);
            }

            // TODO: What ret type should they have?
            BinOperator::Range | BinOperator::RangeInclusive => {
                self.type_context.insert_constraint(lhs_ty, rhs_ty);
            }

            BinOperator::Equals
            | BinOperator::NotEquals
            | BinOperator::LessThan
            | BinOperator::GreaterThan
            | BinOperator::LessThanOrEquals
            | BinOperator::GreaterThanOrEquals => {
                self.type_context.insert_constraint(ret_ty, boolean);
                self.type_context.insert_constraint(lhs_ty, rhs_ty);
            }

            BinOperator::BoolAnd | BinOperator::BoolOr => {
                self.type_context.insert_constraint(ret_ty, boolean.clone());
                self.type_context.insert_constraint(lhs_ty, boolean.clone());
                self.type_context.insert_constraint(rhs_ty, boolean);
            }

            BinOperator::Addition
            | BinOperator::Subtraction
            | BinOperator::Multiplication
            | BinOperator::Division
            | BinOperator::Modulus
            | BinOperator::BitAnd
            | BinOperator::BitOr
            | BinOperator::BitXor
            | BinOperator::ShiftLeft
            | BinOperator::ShiftRight => {
                self.type_context
                    .insert_constraint(ret_ty.clone(), lhs_ty.clone());
                self.type_context.insert_constraint(ret_ty, rhs_ty.clone());
                self.type_context.insert_constraint(lhs_ty, rhs_ty);
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, _ctx: &TraverseContext) {
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

        match &mut un_op.operator {
            UnOperator::Positive
            | UnOperator::Negative
            | UnOperator::BitComplement
            | UnOperator::BoolNot => {
                self.type_context.insert_constraint(ret_ty, val_ty);
            }
            UnOperator::Deref => {
                self.type_context
                    .insert_constraint(Ty::Pointer(Box::new(ret_ty)), val_ty);
            }
            UnOperator::Address => {
                self.type_context
                    .insert_constraint(ret_ty, Ty::Pointer(Box::new(val_ty)));
            }
            UnOperator::ArrayAccess(_) => {
                self.type_context
                    .insert_constraint(ret_ty, Ty::UnknownArrayMember(Box::new(val_ty)));
            }
            UnOperator::StructAccess(member_name, ..) => {
                self.type_context.insert_constraint(
                    ret_ty,
                    Ty::UnknownStructureMember(Box::new(val_ty), member_name.clone()),
                );
            }
        }
    }

    fn visit_func(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Function(func), ..) = &mut ast_token {
            let func_ref = func.borrow_mut();

            // If this is a method and the first argument is named "this", set
            // the type of it to the structure that this method belongs to
            // (which already is stored in `method_structure`).
            if let Some(first_arg) = func_ref.parameters.as_ref().and_then(|args| args.first()) {
                let mut first_arg = first_arg.borrow_mut();

                if &first_arg.name == "this" {
                    if let Some(structure) = &func_ref.method_structure {
                        let structure = structure.clone();

                        let ty = if func_ref.modifiers.contains(&Modifier::This) {
                            structure
                        } else if func_ref.modifiers.contains(&Modifier::ThisPointer) {
                            Ty::Pointer(Box::new(structure))
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

    // TODO: Implement for interfaces and enums.
    /// Tie the generics in this specific struct to each other with constraints.
    /// Ties the generics in the struct members, method parameters and method
    /// return types.
    fn visit_struct(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Struct(struct_), ..) = &ast_token {
            let struct_ = struct_.borrow();

            // Populate this map with the "Generic(ident)" types where the key
            // is the name of the generic and the value is a list of all the
            // Generics that should have constraints between each other.
            let mut generics: HashMap<_, Vec<_>> = HashMap::default();

            // Gather all "Generic" types found in the members types into the
            // `generics` map. All the generic types in every entry will then
            // be tied together so that they all get infered to the same type.
            if let Some(members) = &struct_.members {
                for member in members {
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
            }

            if let Some(methods) = &struct_.methods {
                for method in methods.values() {
                    // Gather "Generic" types from method parameters.
                    if let Some(params) = &method.borrow().parameters {
                        for param in params {
                            if let Some(ty) = param.borrow().ty.as_ref() {
                                let inner_generics = if let Some(inner_generics) = ty.get_generics()
                                {
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
            }

            // Tie the types of the generics with the same ident to each other.
            for ident_generics in generics.values() {
                for i in 0..ident_generics.len() {
                    let left = ident_generics.get(i).cloned().unwrap();

                    for j in i + 1..ident_generics.len() {
                        let right = ident_generics.get(j).cloned().unwrap();
                        self.type_context.insert_constraint(left.clone(), right)
                    }
                }
            }
        }
    }

    /// Need to make sure that a return statement has the same type as the
    /// function return type. Add it as a constraint.
    fn visit_return(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::Return(expr_opt, ..) = stmt {
            if let Some(func) = &self.cur_func {
                let func_ret_ty = if let Some(ty) = &func.borrow().ret_type {
                    ty.clone()
                } else {
                    Ty::CompoundType(InnerTy::Void, Generics::new())
                };

                let expr_ty = match self.type_context.get_expr_type(expr_opt.as_ref()) {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.type_context.insert_constraint(func_ret_ty, expr_ty);
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
    fn visit_match_case(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::MatchCase(match_case_expr), ..) = &mut ast_token {
            if let Some(match_expr) = self.cur_match_expr.clone() {
                let match_expr_ty = match match_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let case_expr_ty = match match_case_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.type_context
                    .insert_constraint(match_expr_ty, case_expr_ty);
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
    fn visit_assignment(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
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

            self.type_context.insert_constraint(lhs_ty, rhs_ty);
        }
    }

    /// The types of the lhs and rhs of a variable declaration with a init value
    /// should be of the same type, add as constraints.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::VariableDecl(var, expr_opt, ..) = stmt {
            let mut var = var.borrow_mut();

            // No way to do type inference of rhs on var decl with no init value.
            let rhs_ty_opt = if expr_opt.is_some() {
                match self.type_context.get_expr_type(expr_opt.as_ref()) {
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
                Some(Ty::CompoundType(
                    InnerTy::Unknown(self.new_unknown_ident(&format!("var_decl({})", var.name))),
                    Generics::new(),
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
                self.type_context.insert_constraint(lhs_ty, rhs_ty);
            }
        }
    }

    fn visit_inc(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
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
            );

            self.type_context.insert_constraint(expr_ty, int_ty)
        }
    }

    fn visit_dec(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
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
            );

            self.type_context.insert_constraint(expr_ty, int_ty)
        }
    }
}
