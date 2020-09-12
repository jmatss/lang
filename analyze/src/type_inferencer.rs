use crate::type_context::TypeContext;
use common::{
    error::CustomResult,
    error::{LangError, LangErrorKind::AnalyzeError},
    token::ast::Token,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function},
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        lit::Lit,
        op::{BinOp, BinOperator, UnOp, UnOperator},
        stmt::Stmt,
    },
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

        Ok(if lhs == rhs {
            // If they are equal, the order doesn't matter.
            // This case should only happen if this is a member of a aggregate type,
            // this should happen if this function hasn't recursed at least one
            // level deep.
            (lhs, rhs)
        } else if let Type::Unknown(_) = rhs {
            (rhs, lhs)
        } else if let Type::Unknown(_) = lhs {
            (lhs, rhs)
        } else if !lhs.is_unknown() && rhs.is_unknown() {
            // True if `lhs` is known and `rhs` is a int/float unknown.
            (rhs, lhs)
        } else if lhs.is_unknown() {
            // True if `to` is a int/float unknown and `rhs` is known or
            // int/float unkown.
            (lhs, rhs)
        } else {
            // True if both are known, but they aren't equal and they are still
            // compatible. This means that both are aggregates of the same type.
            // Structs will either be equals and have been filtered out earlier
            // or the will not be compatible and was filtered out above in this func.
            match (lhs.clone(), rhs.clone()) {
                (Type::Pointer(inner_lhs), Type::Pointer(inner_rhs)) => {
                    self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                }
                (Type::Array(inner_lhs, _), Type::Array(inner_rhs, _)) => {
                    self.get_mapping_direction(*inner_lhs, *inner_rhs)?
                }
                _ => unreachable!(format!("lhs: {:?}, rhs: {:?}", lhs, rhs)),
            }
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

    fn insert_substitution(&mut self, from: Type, to: Type) {
        debug!("Insert substitution -- from: {:?}, to: {:?}", &from, &to);

        if from == to {
            return; // Can't map to itself (infinite recursion).
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
                // See if adding a sub from `new_from` to `new_to` would cause
                // a infinite loop. If that is the case, just don't add the
                // sub. This is OK because if a loop is found, it means that
                // the sub is indirectly already in the `substitution`, otherwise
                // there would be no loop.
                if !self.causes_loop(&new_from, &new_to) {
                    self.insert_substitution(new_from, new_to)
                }
            }
            Err(err) => self.errors.push(err),
        };
    }

    /// Inserts a constraint. This function sorts the lhs and rhs in order of:
    ///   primitive - aggregated -- unknown
    /// This will make it easier to match the types in other parts of the code.
    fn insert_constraint(&mut self, lhs: Type, rhs: Type) {
        debug!("Insert constraint -- lhs: {:?}, rhs: {:?}", &lhs, &rhs);
        if lhs.is_primitive() {
            self.type_context.constraints.push((lhs, rhs));
        } else if rhs.is_primitive() {
            self.type_context.constraints.push((rhs, lhs));
        } else if lhs.is_aggregated() {
            self.type_context.constraints.push((lhs, rhs));
        } else if rhs.is_aggregated() {
            self.type_context.constraints.push((rhs, lhs));
        } else {
            self.type_context.constraints.push((lhs, rhs));
        }
        self.solve_constraints();
    }

    // TODO: Solve infinite recursive solves when generics get implemented.
    // TODO: Should the constraint be removed when a new substitution is added?
    //       Will that constraint every by used again?
    fn solve_constraints(&mut self) {
        let mut i = 0;
        while i < self.type_context.constraints.len() {
            // Safe to unwrap since `i` will always be less that the size
            // of `constraints` enforced by the while loop expr.
            let (lhs, rhs) = self.type_context.constraints.get(i).unwrap().clone();

            debug!("Solving constraint -- lhs: {:?}, rhs: {:?}", &lhs, &rhs);

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

            // OBS! The lhs will always have the "better" type, this is ensured
            // when the types are added as a constraint. The better types are
            // ranked according to:
            //   1. Primitive
            //   2. Aggregated (struct, ptr, array)
            //   3. Unknown
            if (lhs.is_primitive() && rhs.is_primitive())
                || (lhs.is_aggregated() && rhs.is_aggregated())
            {
                // These types should never end up in constraints, but they are
                // not hurtful. Just remove them from the constraints, they
                // don't need any type inference.
                self.type_context.constraints.remove(i);
            } else if lhs.is_primitive() || lhs.is_aggregated() {
                // The rhs is a "Unknown" type that is compatible with the lhs.
                self.insert_substitution(rhs.clone(), lhs.clone());
                self.type_context.constraints.remove(i);
            } else {
                // Both of the types are unknown. If one of them is "Unknown"
                // and the other one are either a unknown int or float, add the
                // mapping "unknown -> int/float" into the substitutions.
                if let Type::Unknown(_) = lhs {
                    if let Type::Unknown(_) = &rhs {
                        // If lhs and rhs has the same type, just remove the
                        // constraint since it doesn't add any value.
                        // If one of the unknowns has a mapping in the `substitution`
                        // map, add a mapping from the other unknown to this
                        // unknown.
                        if lhs == rhs {
                            self.type_context.constraints.remove(i);
                        } else if self.type_context.substitutions.contains_key(&lhs) {
                            match self.type_context.get_substitution(&lhs, false) {
                                Ok(sub_ty) => self.insert_substitution(rhs.clone(), sub_ty),
                                Err(err) => self.errors.push(err),
                            }
                            self.type_context.constraints.remove(i);
                        } else if self.type_context.substitutions.contains_key(&rhs) {
                            match self.type_context.get_substitution(&rhs, false) {
                                Ok(sub_ty) => self.insert_substitution(lhs.clone(), sub_ty),
                                Err(err) => self.errors.push(err),
                            }
                            self.type_context.constraints.remove(i);
                        }
                    } else {
                        match self.type_context.get_substitution(&rhs, false) {
                            Ok(sub_ty) => self.insert_substitution(lhs.clone(), sub_ty),
                            Err(err) => self.errors.push(err),
                        }
                        self.type_context.constraints.remove(i);
                    }
                } else if let Type::Unknown(_) = rhs {
                    match self.type_context.get_substitution(&lhs, false) {
                        Ok(sub_ty) => self.insert_substitution(rhs.clone(), sub_ty),
                        Err(err) => self.errors.push(err),
                    }
                    self.type_context.constraints.remove(i);
                }
            }

            i += 1;
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

    fn visit_token(&mut self, ast_token: &mut AstToken) {
        self.type_context.analyze_context.cur_line_nr = ast_token.line_nr;
        self.type_context.analyze_context.cur_column_nr = ast_token.column_nr;
    }

    fn visit_block(&mut self, ast_token: &mut AstToken) {
        if let Token::Block(_, id, _) = &ast_token.token {
            self.type_context.analyze_context.cur_block_id = *id;
        }
    }

    /// Do one last solve before exiting to make sure that everything than can
    /// be solved are solved. Also debug log the results.
    fn visit_eof(&mut self, ast_token: &mut AstToken) {
        self.type_context.analyze_context.cur_line_nr = ast_token.line_nr;
        self.type_context.analyze_context.cur_column_nr = ast_token.column_nr;

        self.solve_constraints();
        debug!(
            "Type inference Done.\nConstraints: {:#?}\nSubs: {:#?}",
            self.type_context.constraints, self.type_context.substitutions
        );
    }

    /// Assigns a "Unknown" type for every expression that doesn't have a type
    /// explicitly set. This new type will then be temporarilty used during this
    /// stage and should be converted/subtituted into a "real" type before this
    /// analyzing step is done.
    fn visit_expr(&mut self, expr: &mut Expr) {}

    fn visit_lit(&mut self, expr: &mut Expr) {
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

    fn visit_var(&mut self, var: &mut Var) {
        let block_id = self.type_context.analyze_context.cur_block_id;
        let context_var_ty = match self
            .type_context
            .analyze_context
            .get_var(&var.name, block_id)
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
            self.insert_constraint(context_var_ty, var_ty);
        }
    }

    // TODO: Clean up.
    /// Assign the return type of the function to the function call expr.
    fn visit_func_call(&mut self, func_call: &mut FuncCall) {
        let cur_block_id = self.type_context.analyze_context.cur_block_id;

        let func_ty = if let Some(struct_name) = &func_call.method_struct {
            let struct_parent_id = match self
                .type_context
                .analyze_context
                .get_struct_parent_id(struct_name.clone(), cur_block_id)
            {
                Ok(id) => id,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let key = (struct_name.clone(), struct_parent_id);
            if let Some(methods) = self.type_context.analyze_context.methods.get(&key) {
                if let Some(method) = methods.get(&func_call.name) {
                    if let Some(ty) = &method.ret_type {
                        ty.clone()
                    } else {
                        Type::Void
                    }
                } else {
                    let err = LangError::new(
                        format!(
                            "Unable to find method \"{}\" in struct {}.",
                            &func_call.name, &struct_name
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
                        "Unable to find methods for struct \"{}\" in parent block {}.",
                        &struct_name, struct_parent_id
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
            // TODO: FIXME: For now all functions will be defined in the root
            //              block, so just hardcode zero. Must change later.
            let func_decl_block = 0;
            let key = (func_call.name.clone(), func_decl_block);
            if let Some(func) = self.type_context.analyze_context.functions.get(&key) {
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
    fn visit_struct_init(&mut self, struct_init: &mut StructInit) {
        if struct_init.ret_type.is_none() {
            struct_init.ret_type = Some(Type::Custom(struct_init.name.clone()));
        }

        let struct_ = match self
            .type_context
            .analyze_context
            .get_struct(&struct_init.name)
        {
            Ok(struct_) => struct_,
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
                                line_nr: 0,
                                column_nr: 0,
                            },
                        );
                        self.errors.push(err);
                        return;
                    }
                } else {
                    i
                };

                // TODO: Make sure that the struct init arugment is compatible
                //       with the member struct type. Currently this doesn't
                //       get caught until the codegen stage.

                match arg.value.get_expr_type() {
                    Ok(ty) => {
                        if let Some(ret_type) = members
                            .get(index)
                            .map(|var| var.ret_type.as_ref())
                            .flatten()
                        {
                            *ty = ret_type.clone();
                        } else {
                            let err = LangError::new(
                                format!(
                                    "Member \"{:?}\" in struct \"{:?}\" doesn't have a type set.",
                                    members.get(index),
                                    &struct_.name
                                ),
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
                }
            }
        } else if !struct_init.arguments.is_empty() {
            let err = LangError::new(
                format!(
                    "Struct \"{}\" has no members, but struct init specified members.",
                    &struct_.name
                ),
                AnalyzeError {
                    line_nr: 0,
                    column_nr: 0,
                },
            );
            self.errors.push(err);
            return;
        }
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit) {
        if array_init.ret_type.is_none() {
            array_init.ret_type = Some(Type::Unknown(self.new_unknown_ident()));
        }
    }

    /// Adds constraints for binary operations. Most of the bin ops requires
    /// that the lhs and rhs has the same type.
    fn visit_bin_op(&mut self, bin_op: &mut BinOp) {
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

        let lhs_ty = match bin_op.lhs.get_expr_type() {
            Ok(lhs_ty) => lhs_ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let rhs_ty = match bin_op.rhs.get_expr_type() {
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
                    // TODO: If lhs is a literal, just set the type directly.
                    //       This will prevent unnecessary conversions.
                    //       Ex: 1.0 as f64
                    //       currently evals 1.0 to f32, and it is then converted
                    //       to f64 during runtime.
                    self.insert_constraint(ty.clone(), ret_ty);
                } else {
                    let err = self
                        .type_context
                        .analyze_context
                        .err(format!("Rhs of \"as\" not a valid type: {:?}", bin_op.rhs));
                    self.errors.push(err);
                }
            }

            BinOperator::Dot => {
                self.insert_constraint(ret_ty, rhs_ty);
            }

            // TODO: What ret type should they have?
            BinOperator::Range | BinOperator::RangeInclusive => {
                self.insert_constraint(lhs_ty, rhs_ty);
            }

            BinOperator::Equals
            | BinOperator::NotEquals
            | BinOperator::LessThan
            | BinOperator::GreaterThan
            | BinOperator::LessThanOrEquals
            | BinOperator::GreaterThanOrEquals => {
                self.insert_constraint(ret_ty, Type::Boolean);
                self.insert_constraint(lhs_ty, rhs_ty);
            }

            BinOperator::BoolAnd | BinOperator::BoolOr => {
                self.insert_constraint(ret_ty, Type::Boolean);
                self.insert_constraint(lhs_ty, Type::Boolean);
                self.insert_constraint(rhs_ty, Type::Boolean);
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
                self.insert_constraint(ret_ty.clone(), lhs_ty.clone());
                self.insert_constraint(ret_ty, rhs_ty.clone());
                self.insert_constraint(lhs_ty, rhs_ty);
            }
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp) {
        // The expr value of this un op will already have been traversed and should
        // have been given a "unknown" type if it didn't have one type already.
        // The "ret_type" of this un op will also be given a ret_type if it
        // doesn't already have a type set.
        if un_op.ret_type.is_none() {
            un_op.ret_type = Some(Type::Unknown(self.new_unknown_ident()));
        }

        let ret_ty = match self.type_context.get_ret_type(un_op.ret_type.as_mut()) {
            Ok(ty) => ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let val_ty = match un_op.value.get_expr_type() {
            Ok(rhs_ty) => rhs_ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        match &mut un_op.operator {
            UnOperator::Positive
            | UnOperator::Negative
            | UnOperator::Increment
            | UnOperator::Decrement
            | UnOperator::BitComplement
            | UnOperator::BoolNot => {
                self.insert_constraint(ret_ty, val_ty);
            }
            UnOperator::Deref => {
                self.insert_constraint(Type::Pointer(Box::new(ret_ty)), val_ty);
            }
            UnOperator::Address => {
                self.insert_constraint(ret_ty, Type::Pointer(Box::new(val_ty)));
            }
            UnOperator::ArrayAccess(_) => {
                self.insert_constraint(ret_ty, Type::UnknownArrayAccess(Box::new(val_ty)));
            }
            UnOperator::StructAccess(member_name, ..) => {
                self.insert_constraint(
                    ret_ty,
                    Type::UnknownStructMember(Box::new(val_ty), member_name.clone()),
                );
            }
        }
    }

    /// Save the current function in a place so that the stmts/exprs in the body
    /// can access the types of the parameters and the return type of the func.
    fn visit_func(&mut self, ast_token: &mut AstToken) {
        if let Token::Block(BlockHeader::Function(func), ..) = &ast_token.token {
            self.cur_func = Some(func.clone());
        }
    }

    /// Need to make sure that a return statement has the same type as the
    /// function return type. Add it as a constraint.
    fn visit_return(&mut self, stmt: &mut Stmt) {
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

                self.insert_constraint(func_ret_ty, expr_ty.clone());
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
    fn visit_yield(&mut self, stmt: &mut Stmt) {}

    /// Save the current match expr in a place so that the match cases in the body
    /// can access the type of the expr.
    fn visit_match(&mut self, ast_token: &mut AstToken) {
        if let Token::Block(BlockHeader::Match(expr), ..) = &ast_token.token {
            self.cur_match_expr = Some(expr.clone());
        }
    }

    /// Need to make sure that the match expr and the match case exprs have the
    /// same type. Add it as a constraint.
    fn visit_match_case(&mut self, ast_token: &mut AstToken) {
        if let Token::Block(BlockHeader::MatchCase(match_case_expr), ..) = &mut ast_token.token {
            if let Some(mut match_expr) = self.cur_match_expr.clone() {
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

                self.insert_constraint(match_expr_ty.clone(), case_expr_ty.clone());
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
    fn visit_assignment(&mut self, stmt: &mut Stmt) {
        if let Stmt::Assignment(_, lhs, rhs) = stmt {
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

            self.insert_constraint(lhs_ty.clone(), rhs_ty.clone());
        }
    }

    /// The types of the lhs and rhs of a variable declaration with a init value
    /// should be of the same type. The variable in `variables` in `analyze_context`
    /// should also have the same type. Add as constraints.
    fn visit_var_decl(&mut self, stmt: &mut Stmt) {
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
            let block_id = self.type_context.analyze_context.cur_block_id;
            let context_var_ty = match self
                .type_context
                .analyze_context
                .get_var(&var.name, block_id)
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
                self.insert_constraint(lhs_ty.clone(), rhs_ty.clone());
                self.insert_constraint(context_var_ty, rhs_ty.clone());
            }
        }
    }

    fn visit_struct(&mut self, ast_token: &mut AstToken) {}

    fn visit_enum(&mut self, ast_token: &mut AstToken) {}

    fn visit_interface(&mut self, ast_token: &mut AstToken) {}

    fn visit_impl(&mut self, ast_token: &mut AstToken) {}

    fn visit_stmt(&mut self, stmt: &mut Stmt) {}

    fn visit_default_block(&mut self, ast_token: &mut AstToken) {}

    fn visit_anon(&mut self, ast_token: &mut AstToken) {}

    fn visit_if(&mut self, ast_token: &mut AstToken) {}

    fn visit_if_case(&mut self, ast_token: &mut AstToken) {}

    fn visit_for(&mut self, ast_token: &mut AstToken) {}

    fn visit_while(&mut self, ast_token: &mut AstToken) {}

    fn visit_test(&mut self, ast_token: &mut AstToken) {}

    fn visit_break(&mut self, stmt: &mut Stmt) {}

    fn visit_continue(&mut self, stmt: &mut Stmt) {}

    fn visit_use(&mut self, stmt: &mut Stmt) {}

    fn visit_package(&mut self, stmt: &mut Stmt) {}

    fn visit_defer(&mut self, stmt: &mut Stmt) {}

    fn visit_defer_exec(&mut self, stmt: &mut Stmt) {}

    fn visit_extern_decl(&mut self, stmt: &mut Stmt) {}

    fn visit_modifier(&mut self, stmt: &mut Stmt) {}
}
