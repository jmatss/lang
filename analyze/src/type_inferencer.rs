use crate::type_context::{SubResult, TypeContext};
use common::{
    error::CustomResult,
    error::{LangError, LangErrorKind::AnalyzeError},
    token::ast::Token,
    token::op::Op,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function},
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        lit::Lit,
        op::{BinOp, BinOperator, UnOp, UnOperator},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    types::Type,
    visitor::Visitor,
};
use log::debug;
use std::collections::hash_map::Entry;

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

    // TODO: Ugly fix, do this in some other way.
    /// Keep a copy of the current function which body is being traversed.
    /// This will let the statements/exprs etc. inside the function know
    /// about the types of the parameters and the return type.
    cur_func: Option<Function>,

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

    // TODO: Should this be given as three numbers instead of a concatenated String?
    fn new_unknown_ident(&mut self) -> String {
        let cur_line_nr = self.type_context.analyze_context.cur_line_nr;
        let cur_column_nr = self.type_context.analyze_context.cur_column_nr;
        let type_ident = format!("{}-{}:{}", self.type_id, cur_line_nr, cur_column_nr);
        self.type_id += 1;
        type_ident
    }

    /// Takes in two types and sorts them in the correct "mapping order" i.e.
    /// which type should map to which in the substitution map.
    /// The left item in the returned tuple should be mapped to the right.
    /// Returns Err if something goes wrong and returns None if the types are
    /// equal and shouldn't be mapped.
    fn get_mapping_direction(&self, lhs: Type, rhs: Type) -> CustomResult<(Type, Type)> {
        // TODO: Can mapping unknowns (any/int/float) to each other cause infinite
        //       recursion. Probably; how can it be prevented?

        if !lhs.is_compatible(&rhs) {
            return Err(LangError::new(
                format!(
                    "Tried to map incompatible types. Lhs: {:?}, rhs: {:?}",
                    lhs, rhs
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            ));
        }

        Ok(if lhs == rhs || lhs.is_unknown() {
            (lhs, rhs)
        } else if rhs.is_unknown() {
            (rhs, lhs)
        } else if !lhs.is_unknown_any() && !rhs.is_unknown_any() {
            // True if both are known, but they aren't equal and they are still
            // compatible. This means that both are aggregates of the same type.
            // Structs will have been filtered earlier in this function.
            match (lhs.clone(), rhs.clone()) {
                (Type::Pointer(inner_lhs), Type::Pointer(inner_rhs)) => {
                    self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                }
                (Type::Array(inner_lhs, _), Type::Array(inner_rhs, _)) => {
                    self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                }
                _ => unreachable!(format!("lhs: {:?}, rhs: {:?}", lhs, rhs)),
            }
        } else if !lhs.is_unknown_any() {
            (rhs, lhs) // Only lhs known.
        } else if !rhs.is_unknown_any() {
            (lhs, rhs) // Only rhs known.
        } else if lhs.is_unknown_struct_member() {
            // Prefer struct member unknowns over int/float/array unknowns since
            // it will always have its type set in the struct definition.
            (rhs, lhs)
        } else if rhs.is_unknown_struct_member() {
            (lhs, rhs)
        } else if lhs.is_unknown_int() || lhs.is_unknown_float() {
            // Prefer int/float unknowns over array member unknowns.
            (rhs, lhs)
        } else if rhs.is_unknown_int() || rhs.is_unknown_float() {
            (lhs, rhs)
        } else {
            // Both are array member unknowns, direction doesn't matter.
            (rhs, lhs)
        })
    }

    /// Checks if adding a substitution from `from` to `type` creates a loop.
    /// This will be called recursivly to check if mapping `from` to a type
    /// that `to` maps to causes a loop... and so on.
    fn causes_loop(&self, from: &Type, to: &Type) -> bool {
        if let Some(new_to) = self.type_context.substitutions.get(to) {
            if from == new_to {
                true
            } else {
                self.causes_loop(from, new_to)
            }
        } else {
            false
        }
    }

    /// Inserts a new substitution. The order of `first` and `second` doesn't
    /// matter, this function will make sure that they are mapped in the correct
    /// direction.
    fn insert_substitution(&mut self, first: Type, second: Type) {
        let (from, to) = match self.get_mapping_direction(first, second) {
            Ok(types) => types,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        debug!("Insert substitution -- from: {:?}, to: {:?}", &from, &to);

        // Can't map to itself (infinite recursion) and shouldn't cause any kind
        // of loop.
        if from == to || self.causes_loop(&from, &to) {
            return;
        }

        // If the `from` doesn't already have a mapping, just insert the
        // new mapping to `to` and return. Otherwise add a new substitution
        // between the old and the new `to`.
        let old_to = match self.type_context.substitutions.entry(from) {
            Entry::Occupied(ref mut o) => o.get().clone(),
            Entry::Vacant(v) => {
                v.insert(to);
                return;
            }
        };

        match self.get_mapping_direction(to, old_to) {
            Ok((new_from, new_to)) => {
                if !self.causes_loop(&new_from, &new_to) {
                    self.insert_substitution(new_from, new_to)
                }
            }
            Err(err) => self.errors.push(err),
        };
    }

    // TODO: Solve infinite recursive solves when generics get implemented.
    // TODO: Should the constraint be removed when a new substitution is added?
    //       Will that constraint every by used again?
    fn solve_constraints(&mut self, finalize: bool) {
        // Iterate through the constraints until all of them have been converted
        // into substitutions in some way. If a constraint can't be solved, a
        // error will be reported.
        while !self.type_context.constraints.is_empty() {
            let start_constraint_len = self.type_context.constraints.len();

            let mut i = 0;
            while i < self.type_context.constraints.len() {
                // Safe to unwrap since `i` will always be less that the size
                // of `constraints` enforced by the while loop expr.
                let (lhs, rhs) = self.type_context.constraints.get(i).unwrap().clone();

                let lhs_sub = self.type_context.get_substitution(&lhs, finalize);
                let rhs_sub = self.type_context.get_substitution(&rhs, finalize);

                debug!(
                    "Solving constraint -- finalize: {}\nlhs: {:#?}\nlhs_sub: {:#?}\nrhs: {:#?}\nrhs_sub: {:#?}",
                    finalize, &lhs, &lhs_sub, &rhs, &rhs_sub
                );

                // If the types aren't compatible, report a error and remove the
                // constraint. This will allow the inference to continue to do as
                // much work as possible even though it will never be 100% solved.
                if !lhs.is_compatible(&rhs) {
                    let err = self.type_context.analyze_context.err(format!(
                        "Unsolvable type constraint. Lhs: {:?}, rhs: {:?}",
                        lhs, rhs
                    ));
                    self.errors.push(err);
                    self.type_context.constraints.remove(i);
                    continue;
                }

                match (lhs_sub, rhs_sub) {
                    (SubResult::Solved(_), SubResult::Solved(_)) => {
                        self.type_context.constraints.swap_remove(i);
                    }

                    (SubResult::Solved(first), SubResult::UnSolved(second))
                    | (SubResult::UnSolved(first), SubResult::Solved(second))
                    | (SubResult::UnSolved(first), SubResult::UnSolved(second)) => {
                        // The order doesn't matter(map to/from), it will be fixed
                        // in the function that is called.
                        self.insert_substitution(first, second);
                        self.type_context.constraints.remove(i);
                    }

                    (_, SubResult::Err(err)) | (SubResult::Err(err), _) => {
                        self.errors.push(err);
                        self.type_context.constraints.remove(i);
                    }
                }

                i += 1;
            }

            // If the `finalize` flag isn't set, run this recursively with it
            // set to make all possible substitutions.
            // If it after the second iteration isn't able to solve all types,
            // something has failed and a error will be reported.
            if start_constraint_len == self.type_context.constraints.len() {
                if finalize {
                    let err = LangError::new(
                        format!(
                            "Unable to solve all type constraints: {:?}",
                            self.type_context.constraints
                        ),
                        AnalyzeError {
                            line_nr: 0,
                            column_nr: 0,
                        },
                    );
                    self.errors.push(err);
                } else {
                    self.solve_constraints(true);
                }
                return;
            }
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
        self.type_context.analyze_context.cur_line_nr = ast_token.line_nr;
        self.type_context.analyze_context.cur_column_nr = ast_token.column_nr;
    }

    /// Solve the constraints at the EOF. Also debug log the results.
    fn visit_eof(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.type_context.analyze_context.cur_line_nr = ast_token.line_nr;
        self.type_context.analyze_context.cur_column_nr = ast_token.column_nr;

        self.solve_constraints(false);
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
        if let Expr::Lit(lit, gen_ty_opt) = expr {
            if gen_ty_opt.is_none() {
                let new_gen_ty = match lit {
                    // TODO: Have a custom struct "String" instead of "*u8"?
                    Lit::String(_) => Type::Pointer(Box::new(Type::U8)),
                    Lit::Char(_) => Type::Character,
                    Lit::Bool(_) => Type::Boolean,
                    Lit::Integer(_, radix) => Type::UnknownInt(self.new_unknown_ident(), *radix),
                    Lit::Float(_) => Type::UnknownFloat(self.new_unknown_ident()),
                };
                *gen_ty_opt = Some(new_gen_ty);
            }
        }
    }

    fn visit_var(&mut self, var: &mut Var, ctx: &TraverseContext) {
        let context_var_ty = match self
            .type_context
            .analyze_context
            .get_var(&var.name, ctx.block_id)
        {
            Ok(context_var) => {
                if let Some(ty) = &context_var.ret_type {
                    ty.clone()
                } else {
                    let err = LangError::new(
                        format!("Ret type not set for context var: {:?}", context_var),
                        AnalyzeError {
                            line_nr: 0,
                            column_nr: 0,
                        },
                    );
                    self.errors.push(err);
                    return;
                }
            }
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let var_ty = if let Some(ty) = &var.ret_type {
            ty.clone()
        } else {
            var.ret_type = Some(context_var_ty.clone());
            context_var_ty.clone()
        };

        // Add type constraint between var "use" and var "decl" if they are different.
        if context_var_ty != var_ty {
            self.type_context.insert_constraint(context_var_ty, var_ty);
        }
    }

    // TODO: Clean up.
    /// Assign the return type of the function to the function call expr.
    /// Also tie the types of the function parameter to argument types.
    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        let func_ty = if func_call.is_method {
            // If this is a method call, the first argument will be the address
            // of "this". So "unwrap" it to get the actual type of the struct
            // that will be used in type constraints.
            let struct_ty = if let Some(this_arg) = func_call.arguments.first_mut() {
                if let Expr::Op(Op::UnOp(un_op)) = &mut this_arg.value {
                    if let UnOperator::Address = un_op.operator {
                        match un_op.value.get_expr_type_mut() {
                            Ok(struct_ty) => struct_ty.clone(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        let err = LangError::new(
                            format!(
                                "Expected \"this\" for method call \"{}\" to be \"address\" of struct, was: \"{:?}\"",
                                &func_call.name, this_arg.value
                            ),
                            AnalyzeError {
                                line_nr: 0,
                                column_nr: 0,
                            },
                        );
                        self.errors.push(err);
                        return;
                    }
                } else {
                    let err = LangError::new(
                        format!(
                            "Expected \"this\" for method call \"{}\" to be \"address\" of struct, was: \"{:?}\"",
                            &func_call.name, this_arg.value
                        ),
                        AnalyzeError {
                            line_nr: 0,
                            column_nr: 0,
                        },
                    );
                    self.errors.push(err);
                    return;
                }
            } else {
                let err = LangError::new(
                    format!(
                        "Method call \"{}\" has no arguments, expected atleast \"this\"/\"self\".",
                        &func_call.name
                    ),
                    AnalyzeError {
                        line_nr: 0,
                        column_nr: 0,
                    },
                );
                self.errors.push(err);
                return;
            };

            // Insert constraints between the function call argument type and
            // the method parameter types that will be figured out later.
            let mut idx: u64 = 0;
            for arg in &func_call.arguments {
                let arg_ty = Type::UnknownMethodArgument(
                    Box::new(struct_ty.clone()),
                    func_call.name.clone(),
                    arg.name.clone(),
                    idx,
                );

                let arg_expr_ty = match arg.value.get_expr_type() {
                    Ok(ty) => ty.clone(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.type_context.insert_constraint(arg_ty, arg_expr_ty);

                idx += 1;
            }

            // The expected return type of the function call.
            Type::UnknownStructMethod(Box::new(struct_ty), func_call.name.clone())
        } else {
            // TODO: FIXME: For now all functions will be defined in the root
            //              block, so just hardcode zero. Must change later.
            let func_decl_block = 0;
            let key = (func_call.name.clone(), func_decl_block);
            if let Some(ref func) = self
                .type_context
                .analyze_context
                .functions
                .get(&key)
                .cloned()
            {
                // Iterate through all arguments of the function and match
                // up their types with the parameters of the function.
                // The amount of args/params will already have been checked before,
                // just make sure that this doesn't break for vararg functions.
                // The "similar" logic for methods will be done during type solving
                // in `type_context` since at this point there is no way to know
                // the type of the struct and indirectly the method.
                if let Some(params) = &func.parameters {
                    for (arg, par) in func_call.arguments.iter().zip(params) {
                        let arg_ty = match arg.value.get_expr_type() {
                            Ok(ty) => ty.clone(),
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        };

                        let par_ty = if let Some(ty) = &par.ret_type {
                            ty.clone()
                        } else {
                            let err = LangError::new(
                                format!(
                                    "Type for parameter \"{}\" in function \"{}\" set to None.",
                                    par.name, func.name
                                ),
                                AnalyzeError {
                                    line_nr: 0,
                                    column_nr: 0,
                                },
                            );
                            self.errors.push(err);
                            return;
                        };

                        self.type_context.insert_constraint(arg_ty, par_ty);
                    }
                }

                if let Some(ty) = &func.ret_type {
                    ty.clone()
                } else {
                    Type::Void
                }
            } else {
                let err = LangError::new(
                    format!(
                        "Unable to find function \"{}\" in block {}.",
                        &func_call.name, func_decl_block
                    ),
                    AnalyzeError {
                        line_nr: 0,
                        column_nr: 0,
                    },
                );
                self.errors.push(err);
                return;
            }
        };

        func_call.ret_type = Some(func_ty);
    }

    /// Adds the correct type for the struct init and ties the types of the struct
    /// members with the type of the struct init arguments.
    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        if struct_init.ret_type.is_none() {
            struct_init.ret_type = Some(Type::Custom(struct_init.name.clone()));
        }

        let struct_ = match self
            .type_context
            .analyze_context
            .get_struct(&struct_init.name, ctx.block_id)
        {
            Ok(struct_) => struct_.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        if let Some(members) = &struct_.members {
            if members.len() != struct_init.arguments.len() {
                let err = LangError::new(
                    format!(
                        "Struct \"{}\" and struct init has diff amount of members. Struct#: {:?}, init#: {:?}.",
                        &struct_.name, members.len(), struct_init.arguments.len()
                    ),
                    AnalyzeError {
                        line_nr: self.type_context.analyze_context.cur_line_nr,
                        column_nr: self.type_context.analyze_context.cur_column_nr,
                    },
                );
                self.errors.push(err);
                return;
            }

            // TODO: Verify that all members are initialized.

            for (i, arg) in struct_init.arguments.iter_mut().enumerate() {
                // If a name is set, this is a named member init. Shouldn't use
                // the index, set the correctly named member instead.
                let index: usize = if let Some(arg_name) = &arg.name {
                    let mut found = false;
                    for member in members {
                        if arg_name == &member.name {
                            found = true;
                            break;
                        }
                    }

                    if found {
                        i
                    } else {
                        let err = LangError::new(
                            format!(
                                "Struct \"{:?}\" has no member with name \"{:?}\".",
                                &struct_.name, &members[i].name
                            ),
                            AnalyzeError {
                                line_nr: self.type_context.analyze_context.cur_line_nr,
                                column_nr: self.type_context.analyze_context.cur_column_nr,
                            },
                        );
                        self.errors.push(err);
                        return;
                    }
                } else {
                    i
                };

                // TODO: Make sure that the struct init argument is compatible
                //       with the member struct type. Currently this doesn't
                //       get caught until the codegen stage.

                // Add constraints mapping the type of the struct init argument
                // to the corresponding actual struct member type.
                match arg.value.get_expr_type_mut() {
                    Ok(ty) => {
                        if let Some(member) = members.get(index) {
                            if let Some(ret_type) = &member.ret_type {
                                // Bind init member to actual type in struct definition.
                                self.type_context
                                    .insert_constraint(ty.clone(), ret_type.clone());

                                // Bind type to any array accessing of this member.
                                self.type_context.insert_constraint(
                                    ty.clone(),
                                    Type::UnknownStructMember(
                                        Box::new(Type::Custom(struct_init.name.clone())),
                                        member.name.clone(),
                                    ),
                                );
                            } else {
                                let err = LangError::new(
                                    format!(
                                        "Member \"{:?}\" in struct \"{:?}\" doesn't have a type set.",
                                        members.get(index),
                                        &struct_.name
                                    ),
                                    AnalyzeError {
                                        line_nr: self.type_context.analyze_context.cur_line_nr,
                                        column_nr: self.type_context.analyze_context.cur_column_nr,
                                    },
                                );
                                self.errors.push(err);
                                return;
                            }
                        } else {
                            let err = LangError::new(
                                format!(
                                    "Unable to get member at index {} in struct \"{:?}\".",
                                    index, &struct_.name
                                ),
                                AnalyzeError {
                                    line_nr: self.type_context.analyze_context.cur_line_nr,
                                    column_nr: self.type_context.analyze_context.cur_column_nr,
                                },
                            );
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
            let err = LangError::new(
                format!(
                    "Struct \"{}\" has no members, but struct init specified members.",
                    &struct_.name
                ),
                AnalyzeError {
                    line_nr: self.type_context.analyze_context.cur_line_nr,
                    column_nr: self.type_context.analyze_context.cur_column_nr,
                },
            );
            self.errors.push(err);
            return;
        }
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, _ctx: &TraverseContext) {
        let ret_ty = if let Some(ret_ty) = &array_init.ret_type {
            ret_ty.clone()
        } else {
            let new_ty = Type::Unknown(self.new_unknown_ident());
            array_init.ret_type = Some(new_ty.clone());
            new_ty
        };

        let mut arg_types = Vec::new();
        for arg in &mut array_init.arguments {
            match arg.value.get_expr_type_mut() {
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
        let array_index_type = Type::U32;
        let dim = array_init.arguments.len();
        let dim_expr = Expr::Lit(Lit::Integer(dim.to_string(), 10), Some(array_index_type));

        // Add a constraint for all arguments that they are members of the same
        // array type and and also add constraint between all the values in the
        // array init.
        for i in 0..array_init.arguments.len() {
            let left = arg_types.get(i).cloned().unwrap();

            self.type_context.insert_constraint(
                ret_ty.clone(),
                Type::Array(Box::new(left.clone()), Some(Box::new(dim_expr.clone()))),
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
            let new_type = Type::Unknown(self.new_unknown_ident());
            bin_op.ret_type = Some(new_type.clone());
            new_type
        };

        let lhs_ty = match bin_op.lhs.get_expr_type_mut() {
            Ok(lhs_ty) => lhs_ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let rhs_ty = match bin_op.rhs.get_expr_type_mut() {
            Ok(rhs_ty) => rhs_ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        match bin_op.operator {
            // The lhs and rhs can be different in these operations, so shouldn't
            // add as constraints.
            // TODO:
            BinOperator::In | BinOperator::Is | BinOperator::Of => (),

            BinOperator::As => {
                if let Expr::Type(ty) = &*bin_op.rhs {
                    // TODO: The lhs and rhs needs to be checked so that they
                    //       have compatible types but they don't need to have
                    //       the same type.
                    // Change the type of the bin op directly so that it takes
                    // precedence during type inferencing.
                    bin_op.ret_type = Some(ty.clone());
                } else {
                    let err = self
                        .type_context
                        .analyze_context
                        .err(format!("Rhs of \"as\" not a valid type: {:?}", bin_op.rhs));
                    self.errors.push(err);
                }
            }

            BinOperator::Dot => {
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
                self.type_context.insert_constraint(ret_ty, Type::Boolean);
                self.type_context.insert_constraint(lhs_ty, rhs_ty);
            }

            BinOperator::BoolAnd | BinOperator::BoolOr => {
                self.type_context.insert_constraint(ret_ty, Type::Boolean);
                self.type_context.insert_constraint(lhs_ty, Type::Boolean);
                self.type_context.insert_constraint(rhs_ty, Type::Boolean);
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
            let new_ty = Type::Unknown(self.new_unknown_ident());
            un_op.ret_type = Some(new_ty.clone());
            new_ty
        };

        let val_ty = match un_op.value.get_expr_type_mut() {
            Ok(rhs_ty) => rhs_ty.clone(),
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
                    .insert_constraint(Type::Pointer(Box::new(ret_ty)), val_ty);
            }
            UnOperator::Address => {
                self.type_context
                    .insert_constraint(ret_ty, Type::Pointer(Box::new(val_ty)));
            }
            UnOperator::ArrayAccess(_) => {
                self.type_context
                    .insert_constraint(ret_ty, Type::UnknownArrayMember(Box::new(val_ty)));
            }
            UnOperator::StructAccess(member_name, ..) => {
                self.type_context.insert_constraint(
                    ret_ty,
                    Type::UnknownStructMember(Box::new(val_ty), member_name.clone()),
                );
            }
        }
    }

    /// Save the current function in a place so that the stmts/exprs in the body
    /// can access the types of the parameters and the return type of the func.
    fn visit_func(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::Function(func), ..) = &ast_token.token {
            self.cur_func = Some(func.clone());
        }
    }

    /// Need to make sure that a return statement has the same type as the
    /// function return type. Add it as a constraint.
    fn visit_return(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::Return(expr_opt) = stmt {
            if let Some(func) = &self.cur_func {
                let func_ret_ty = if let Some(ty) = &func.ret_type {
                    ty.clone()
                } else {
                    Type::Void
                };

                let expr_ty = match self.type_context.get_expr_type_opt(expr_opt.as_mut()) {
                    Ok(expr_ty) => expr_ty,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                self.type_context
                    .insert_constraint(func_ret_ty, expr_ty.clone());
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
        if let Token::Block(BlockHeader::Match(expr), ..) = &ast_token.token {
            self.cur_match_expr = Some(expr.clone());
        }
    }

    /// Need to make sure that the match expr and the match case exprs have the
    /// same type. Add it as a constraint.
    fn visit_match_case(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::MatchCase(match_case_expr), ..) = &mut ast_token.token {
            if let Some(mut match_expr) = self.cur_match_expr.clone() {
                let match_expr_ty = match match_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty.clone(),
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                let case_expr_ty = match match_case_expr.get_expr_type() {
                    Ok(expr_ty) => expr_ty.clone(),
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
        if let Stmt::Assignment(_, lhs, rhs) = stmt {
            debug!("ASSIGNMENT\nlhs: {:#?}\nrhs: {:#?}", lhs, rhs);

            let lhs_ty = match lhs.get_expr_type() {
                Ok(lhs_ty) => lhs_ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let rhs_ty = match rhs.get_expr_type() {
                Ok(rhs_ty) => rhs_ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            self.type_context.insert_constraint(lhs_ty, rhs_ty);
        }
    }

    /// The types of the lhs and rhs of a variable declaration with a init value
    /// should be of the same type. The variable in `variables` in `analyze_context`
    /// should also have the same type. Add as constraints.
    fn visit_var_decl(&mut self, stmt: &mut Stmt, ctx: &TraverseContext) {
        if let Stmt::VariableDecl(var, expr_opt) = stmt {
            // No way to do type inference of rhs on var decl with no init value.
            let rhs_ty_opt = if expr_opt.is_some() {
                match self.type_context.get_expr_type_opt(expr_opt.as_mut()) {
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
            let new_type = if var.ret_type.is_some() {
                var.ret_type.clone()
            } else {
                Some(Type::Unknown(self.new_unknown_ident()))
            };

            var.ret_type = new_type.clone();
            let lhs_ty = match self.type_context.get_ret_type(var.ret_type.as_mut()) {
                Ok(lhs_ty) => lhs_ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Set the type for the variable in the `analyze_context` as well.
            let context_var_ty = match self
                .type_context
                .analyze_context
                .get_var(&var.name, ctx.block_id)
            {
                Ok(context_var) => {
                    context_var.ret_type = new_type;

                    if let Some(ty) = &context_var.ret_type {
                        ty.clone()
                    } else {
                        let err = LangError::new(
                            format!(""),
                            AnalyzeError {
                                line_nr: 0,
                                column_nr: 0,
                            },
                        );
                        self.errors.push(err);
                        return;
                    }
                }
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            // Add constraints only if this var decl has a init value.
            // The var and context_var will already have the same type.
            if let Some(rhs_ty) = rhs_ty_opt {
                self.type_context
                    .insert_constraint(lhs_ty.clone(), rhs_ty.clone());
                self.type_context
                    .insert_constraint(context_var_ty, rhs_ty.clone());
            }
        }
    }

    fn visit_inc(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::Increment(expr) = stmt {
            let expr_ty = match expr.get_expr_type() {
                Ok(ty) => ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let int_ty = Type::UnknownInt(self.new_unknown_ident(), 10);

            self.type_context.insert_constraint(expr_ty, int_ty)
        }
    }

    fn visit_dec(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::Increment(expr) = stmt {
            let expr_ty = match expr.get_expr_type() {
                Ok(ty) => ty.clone(),
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let int_ty = Type::UnknownInt(self.new_unknown_ident(), 10);

            self.type_context.insert_constraint(expr_ty, int_ty)
        }
    }
}
