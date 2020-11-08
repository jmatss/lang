use crate::type_context::{SubResult, TypeContext};
use common::{
    error::LangError,
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
use std::collections::BTreeMap;

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

    /// Creates a new unknown identifier that will be given to a unkown type.
    /// The new string will containg a unique ID (ID), the line number (L),
    /// the column number (C) and a free text to give the unkown type some more
    /// context for readability.
    fn new_unknown_ident(&mut self, text: &str) -> String {
        let cur_line_nr = self.type_context.analyze_context.line_nr;
        let cur_column_nr = self.type_context.analyze_context.column_nr;
        let type_ident = format!(
            "ID:{}-R:{}-C:{}-{}",
            self.type_id, cur_line_nr, cur_column_nr, text
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
        self.type_context.analyze_context.line_nr = ast_token.line_nr;
        self.type_context.analyze_context.column_nr = ast_token.column_nr;
    }

    /// Solve the constraints at the EOF. Also debug log the results.
    fn visit_eof(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.type_context.analyze_context.line_nr = ast_token.line_nr;
        self.type_context.analyze_context.column_nr = ast_token.column_nr;

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
        if let Expr::Lit(lit, gen_ty_opt) = expr {
            if gen_ty_opt.is_none() {
                let new_gen_ty = match lit {
                    // TODO: Have a custom struct "String" instead of "*u8"?
                    Lit::String(_) => Type::Pointer(Box::new(Type::U8)),
                    Lit::Char(_) => Type::Character,
                    Lit::Bool(_) => Type::Boolean,
                    Lit::Integer(_, radix) => {
                        Type::UnknownInt(self.new_unknown_ident("int_literal"), *radix)
                    }
                    Lit::Float(_) => Type::UnknownFloat(self.new_unknown_ident("float_literal")),
                };
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

        let var_decl_ty = if let Some(ty) = &var_decl.ret_type {
            ty.clone()
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Ret type not set for var decl: {:?}", var_decl));
            self.errors.push(err);
            return;
        };

        let var_ty = if let Some(ty) = &var.ret_type {
            ty.clone()
        } else {
            let new_type = Type::Unknown(self.new_unknown_ident("var_use"));
            var.ret_type = Some(new_type.clone());
            new_type
        };

        // Add type constraint between var "use" and var "decl",
        self.type_context.insert_constraint(var_decl_ty, var_ty);
    }

    // TODO: Clean up.
    /// Assign the return type of the function to the function call expr.
    /// Also tie the types of the function parameter to argument types.
    fn visit_func_call(&mut self, func_call: &mut FuncCall, ctx: &TraverseContext) {
        let func_ty = if func_call.is_method {
            // TODO: Move "this" to be a constant somewhere else.
            // This this is a method call on a instance of the struct object,
            // the first argument "this" will contain a reference to the struct
            // type.
            // If this is a static method call, the struct name will already
            // be stored in the `func_call.method_struct`, so one can get the
            // struct type from there.
            let struct_ty = if let Some(struct_name) = &func_call.method_struct {
                Type::CompoundType(struct_name.clone(), BTreeMap::default())
            } else if let Some(this_arg) = func_call.arguments.first_mut() {
                if let Expr::Op(Op::UnOp(un_op)) = &mut this_arg.value {
                    if let UnOperator::Address = un_op.operator {
                        match un_op.value.get_expr_type() {
                            Ok(struct_ty) => struct_ty,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        let err = self.type_context.analyze_context.err(format!(
                            "Expected \"this\" for method call \"{}\" to be \"address\" of struct, was: \"{:?}\"",
                            &func_call.name, this_arg.value
                        ));
                        self.errors.push(err);
                        return;
                    }
                } else {
                    let err = self.type_context.analyze_context.err(
                        format!(
                            "Expected \"this\" for method call \"{}\" to be \"address\" of struct, was: \"{:?}\"",
                            &func_call.name, this_arg.value
                        ),
                    );
                    self.errors.push(err);
                    return;
                }
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Method call \"{}\" has no arguments, expected atleast \"this\"/\"self\".",
                    &func_call.name
                ));
                self.errors.push(err);
                return;
            };

            // Insert constraints between the function call argument type and
            // the method parameter types that will be figured out later.
            let mut idx: u64 = 0;
            for arg in &func_call.arguments {
                // If the argument is a named argument, get the index for the
                // named parameter instead of using the index of its position
                // in the function call.
                let inner_idx = if let Some(arg_name) = &arg.name {
                    if let Type::CompoundType(struct_name, _) = &struct_ty {
                        match self.type_context.analyze_context.get_method_param_idx(
                            struct_name,
                            &func_call.name,
                            arg_name,
                            ctx.block_id,
                        ) {
                            Ok(idx) => idx,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        let err = self.type_context.analyze_context.err(format!(
                            "\"this\" in method no a struc type, was: {:?}",
                            struct_ty
                        ));
                        self.errors.push(err);
                        return;
                    }
                } else {
                    idx
                };

                let arg_ty = Type::UnknownMethodArgument(
                    Box::new(struct_ty.clone()),
                    func_call.name.clone(),
                    arg.name.clone(),
                    inner_idx,
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
            let func = match self
                .type_context
                .analyze_context
                .get_func(&func_call.name, ctx.block_id)
            {
                Ok(func) => func.clone(),
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
            if let Some(params) = &func.parameters {
                let mut idx: u64 = 0;
                for arg in &func_call.arguments {
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

                    if func.is_var_arg && inner_idx >= params.len() as u64 {
                        idx += 1;
                        continue;
                    }

                    let par_ty = if let Some(ty) = &params
                        .get(inner_idx as usize)
                        .map(|param| param.ret_type.clone())
                        .flatten()
                    {
                        ty.clone()
                    } else {
                        let err = self.type_context.analyze_context.err(
                            format!(
                                "Type for parameter \"{:?}\" with index {} in function \"{}\" set to None.",
                                arg.name, inner_idx, func.name
                            ),
                        );
                        self.errors.push(err);
                        return;
                    };

                    self.type_context.insert_constraint(arg_ty, par_ty);

                    idx += 1;
                }
            }

            if let Some(ty) = &func.ret_type {
                ty.clone()
            } else {
                Type::Void
            }
        };

        func_call.ret_type = Some(func_ty);
    }

    /// Adds the correct type for the struct init and ties the types of the struct
    /// members with the type of the struct init arguments.
    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        if struct_init.ret_type.is_none() {
            struct_init.ret_type = Some(Type::CompoundType(
                struct_init.name.clone(),
                BTreeMap::default(),
            ));
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
            // Gets a map if the generics that maps the ident of the generic
            // (ex. "T", "U" etc.) to a new unknown generic type. This is needed
            // to ensure that two members of a struct with the same ident uses
            // the same unknown generic type. It is also needed to ensure that
            // different struct uses different types for the generics.
            let unknown_generics = if let Some(gens) = &struct_.generic_params {
                let mut map = BTreeMap::new();
                for generic_ident in gens {
                    let unknown_ident =
                        self.new_unknown_ident(&format!("generic_{}", generic_ident));
                    let gen_ty = Type::Generic(unknown_ident);

                    map.insert(generic_ident.clone(), gen_ty);
                }
                map
            } else {
                BTreeMap::default()
            };

            // If this struct init is a init for a struct with generics, the
            // return type of the init should be wrapped in a "CompoundType"
            // with the generics attached to the return type.
            if !unknown_generics.is_empty() {
                match &struct_init.ret_type {
                    Some(Type::CompoundType(..)) => {
                        // If the type already is set to a compound, use that
                        // already set type.
                    }
                    Some(_) => {
                        struct_init.ret_type = Some(Type::CompoundType(
                            struct_init.name.clone(),
                            unknown_generics.clone(),
                        ));
                    }
                    None => unreachable!("Ret type not set for struct init."),
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
                        let err = self.type_context.analyze_context.err(format!(
                            "Struct \"{:?}\" has no member with name \"{:?}\".",
                            &struct_.name, &members[i].name
                        ));
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
                match arg.value.get_expr_type() {
                    Ok(arg_ty) => {
                        if let Some(member) = members.get(index) {
                            let mut member = member.clone();

                            // Get the "actual" type of the member. If it contains
                            // a generic, it needs to get the actual unknown
                            // generic type from the `unknown_generics` map.
                            // Otherwise reuse the already set type.
                            let member_type = if let Some(member_type) = &mut member.ret_type {
                                member_type.replace_generics_impl(&unknown_generics);
                                member_type.clone()
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
                                Type::UnknownStructMember(
                                    Box::new(
                                        struct_init
                                            .ret_type
                                            .clone()
                                            .expect("Will always be set at this point"),
                                    ),
                                    member.name.clone(),
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
            let new_ty = Type::Unknown(self.new_unknown_ident("array_init"));
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
            let new_type = Type::Unknown(self.new_unknown_ident("bin_op"));
            bin_op.ret_type = Some(new_type.clone());
            new_type
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

        match bin_op.operator {
            // The lhs and rhs can be different in these operations, so shouldn't
            // add as constraints.
            // TODO:
            BinOperator::In | BinOperator::Is | BinOperator::Of => (),

            BinOperator::As => {
                if let Expr::Type(rhs_ty) = &*bin_op.rhs {
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
            let new_ty = Type::Unknown(self.new_unknown_ident("un_op"));
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
        if let Token::Block(BlockHeader::Match(expr), ..) = &ast_token.token {
            self.cur_match_expr = Some(expr.clone());
        }
    }

    /// Need to make sure that the match expr and the match case exprs have the
    /// same type. Add it as a constraint.
    fn visit_match_case(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let Token::Block(BlockHeader::MatchCase(match_case_expr), ..) = &mut ast_token.token {
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
        if let Stmt::Assignment(_, lhs, rhs) = stmt {
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
        if let Stmt::VariableDecl(var, expr_opt) = stmt {
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
            let new_type = if var.ret_type.is_some() {
                var.ret_type.clone()
            } else {
                Some(Type::Unknown(self.new_unknown_ident("var_decl")))
            };
            var.ret_type = new_type;

            let lhs_ty = if let Some(ty) = &var.ret_type {
                ty.clone()
            } else {
                let err = self
                    .type_context
                    .analyze_context
                    .err(format!("Lhs of var decl has no type: {:?}", var.ret_type));
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
        if let Stmt::Increment(expr) = stmt {
            let expr_ty = match expr.get_expr_type() {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let int_ty = Type::UnknownInt(self.new_unknown_ident("increment"), 10);

            self.type_context.insert_constraint(expr_ty, int_ty)
        }
    }

    fn visit_dec(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::Increment(expr) = stmt {
            let expr_ty = match expr.get_expr_type() {
                Ok(ty) => ty,
                Err(err) => {
                    self.errors.push(err);
                    return;
                }
            };

            let int_ty = Type::UnknownInt(self.new_unknown_ident("decrement"), 10);

            self.type_context.insert_constraint(expr_ty, int_ty)
        }
    }
}
