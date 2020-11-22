use std::collections::{hash_map::Entry, HashMap};

use crate::type_context::{SubResult, TypeContext};
use common::{
    error::LangError,
    r#type::{inner_ty::InnerTy, ty::Ty},
    token::op::UnOperator,
    token::{
        ast::AstToken,
        block::{Function, Struct},
        expr::{ArrayInit, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    util,
    visitor::Visitor,
};

pub struct TypeSolver<'a> {
    type_context: &'a mut TypeContext<'a>,

    /// Will contain all generic structs that have had the generic types substituted
    /// with real types. The key is the name of the struct (WITHOUT generics).
    pub generic_structs: HashMap<String, Vec<Struct>>,

    // The outer key is the struct name WITH generics.
    // The inner key is the name of the method.
    pub generic_struct_methods: HashMap<String, HashMap<String, Function>>,

    errors: Vec<LangError>,
}

impl<'a> TypeSolver<'a> {
    pub fn new(type_context: &'a mut TypeContext<'a>) -> Self {
        Self {
            type_context,
            generic_structs: HashMap::default(),
            generic_struct_methods: HashMap::default(),
            errors: Vec::default(),
        }
    }

    fn subtitute_type(&mut self, ty: &mut Ty, finalize: bool) {
        match self.type_context.solve_substitution(ty, finalize) {
            SubResult::Solved(sub_ty) => {
                *ty = sub_ty;
            }
            SubResult::UnSolved(un_sub_ty) => {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to resolve type {:?}. Got back unsolved: {:?}.",
                    ty, un_sub_ty
                ));
                self.errors.push(err);
            }
            SubResult::Err(err) => {
                self.errors.push(err);
            }
        }
    }

    /// Creates new instances of structs that have had their generics replaced
    /// with the actual real types that will be used.
    /// All the types will still have the normal/old name without generics,
    /// but need to modify it for the "lookup" structs so that they are unique.
    fn create_generic_struct(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        // Get the actual struct implementation and create a copy of it.
        let mut gen_struct_ty = match self
            .type_context
            .analyze_context
            .get_struct(&struct_init.name, ctx.block_id)
        {
            Ok(struct_ty) => struct_ty.clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let (new_struct_name, generics) =
            if let Some(Ty::CompoundType(_, generics)) = &struct_init.ret_type {
                let full_name = match struct_init.full_name() {
                    Ok(full_name) => full_name,
                    Err(err) => {
                        self.errors.push(err);
                        return;
                    }
                };

                (full_name, generics)
            } else {
                unreachable!("create_generic_struct with bad type: {:#?}", struct_init);
            };

        // Give the new copy of the struct type the "new name" containing
        // information about the generic arguments.
        gen_struct_ty.name = new_struct_name.clone();

        // For every member of the struct, replace any generic types with
        // the type of the struct_init generics.
        if let Some(members) = &mut gen_struct_ty.members {
            for member in members {
                if let Some(ty) = &mut member.ret_type {
                    ty.replace_generics_impl(generics);
                }
            }
        }

        // For every method of the struct, replace any generic types with
        // the type of the struct_init generics.
        // The methods are just pointers inside the "Struct" struct, so need to
        // dereference and create copies of the methods.
        if let Some(methods) = &mut gen_struct_ty.methods {
            let mut new_methods = HashMap::default();

            for (method_name, method) in methods {
                let mut new_method = unsafe { method.as_ref() }.unwrap().clone();

                if let Some(parameters) = &mut new_method.parameters {
                    for param in parameters {
                        if let Some(ty) = &mut param.ret_type {
                            ty.replace_generics_impl(generics);
                        }
                    }
                }

                new_methods.insert(method_name.clone(), new_method);
            }

            self.generic_struct_methods
                .insert(new_struct_name.clone(), new_methods);
        }

        // This new struct variable will be added as a new struct type stored
        // in `self.generic_structs` and will then be inserted into the
        // AST later on by another "class" ("type_converter").
        match self.generic_structs.entry(struct_init.name.clone()) {
            Entry::Occupied(ref mut o) => {
                o.get_mut().push(gen_struct_ty);
            }
            Entry::Vacant(v) => {
                v.insert(vec![gen_struct_ty]);
            }
        }
    }
}

impl<'a> Visitor for TypeSolver<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// There will always be a single default block that wraps everything.
    /// So this code will be ran at the start only once. It will make sure that
    /// all expressions with implicit types had their types inferred correctly;
    /// otherwise a error will be reported.
    fn visit_default_block(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        // TODO: How to check that all types have been inferred?
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.type_context.analyze_context.line_nr = ast_token.line_nr;
        self.type_context.analyze_context.column_nr = ast_token.column_nr;
    }

    /// Iterate through all expressions and substitute all Unknown types that
    /// was used temporarily during the type inference stage. They should be
    /// now be replaced with "real" types that can be found in the `substitutions`.
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        // Do the substitute for "Type" here, all other exprs will make the subs
        // in their own visit funcs.
        if let Expr::Type(ty) = expr {
            self.subtitute_type(ty, true);
        }
    }

    fn visit_lit(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Expr::Lit(_, Some(ty)) = expr {
            self.subtitute_type(ty, true);
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Unable to find infer type for lit: {:?}", expr));
            self.errors.push(err);
        }
    }

    fn visit_var(&mut self, var: &mut Var, _ctx: &TraverseContext) {
        if let Some(ty) = &mut var.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Unable to find infer type for var: {:?}", var));
            self.errors.push(err);
        }
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        if let Some(ty) = &mut func_call.ret_type {
            if let Some(structure_ty) = &mut func_call.method_structure {
                self.subtitute_type(structure_ty, true);
            }
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for func call: {:?}",
                func_call
            ));
            self.errors.push(err);
        }
    }

    fn visit_struct_init(&mut self, struct_init: &mut StructInit, ctx: &TraverseContext) {
        if let Some(ty) = &mut struct_init.ret_type {
            self.subtitute_type(ty, true);

            // Get the mapping for the generics for this specific struct init
            // instance. This will be used to replace the generic types of a
            // struct which will create a new struct type.
            //
            // If this is a struct without generics, there is nothing more to do,
            // do early return from here.
            match ty {
                Ty::CompoundType(_, generics) => {
                    if !generics.is_empty() {
                        self.create_generic_struct(struct_init, ctx);
                    }
                }

                _ => {
                    let err = self.type_context.analyze_context.err(format!(
                        "Ret type of struct init not CompoundType: {:#?}",
                        ty
                    ));
                    self.errors.push(err);
                }
            }
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for struct init: {:?}",
                struct_init
            ));
            self.errors.push(err);
        }
    }

    fn visit_array_init(&mut self, array_init: &mut ArrayInit, _ctx: &TraverseContext) {
        if let Some(ty) = &mut array_init.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for array init: {:?}",
                array_init
            ));
            self.errors.push(err);
        }
    }

    fn visit_bin_op(&mut self, bin_op: &mut BinOp, _ctx: &TraverseContext) {
        if let Some(ty) = &mut bin_op.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for bin op ret_type: {:?}",
                bin_op
            ));
            self.errors.push(err);
        }
    }

    fn visit_un_op(&mut self, un_op: &mut UnOp, ctx: &TraverseContext) {
        if let Some(ty) = &mut un_op.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for un op ret_type: {:?}",
                un_op
            ));
            self.errors.push(err);
        }

        // Edge case logic for struct access. Need to figure out the index
        // of the member that is being accessed.
        if let UnOperator::StructAccess(member_name, member_idx, member_ty) = &mut un_op.operator {
            *member_ty = un_op.ret_type.clone();

            match un_op.value.get_expr_type() {
                // TODO: Implement for enum and interface as well.
                Ok(Ty::CompoundType(InnerTy::Struct(ref old_name), ref generics)) => {
                    let idx = if generics.is_empty() {
                        match self.type_context.analyze_context.get_struct_member_index(
                            old_name,
                            member_name,
                            ctx.block_id,
                        ) {
                            Ok(idx) => idx,
                            Err(err) => {
                                self.errors.push(err);
                                return;
                            }
                        }
                    } else {
                        // The struct type will have been resolved by this point
                        // so that the a struct with the real types for generics have
                        // been created and can be found in `self.generic_structs`.
                        let new_name = util::to_generic_struct_name(old_name, generics);

                        // TODO: Find a cleaner way to do this.
                        let empty_struct = Struct::new("".into());
                        if let Some(new_structs) = self.generic_structs.get(old_name) {
                            let mut new_struct = &empty_struct;
                            let mut is_found = false;

                            for curr_new_struct in new_structs {
                                if curr_new_struct.name == new_name {
                                    is_found = true;
                                    new_struct = curr_new_struct;
                                    break;
                                }
                            }

                            if is_found {
                                if let Some(idx) = new_struct.member_index(member_name) {
                                    idx as u64
                                } else {
                                    let err = self.type_context.analyze_context.err(format!(
                                        "Unable to find member \"{}\" in struct: {:#?}",
                                        member_name, new_struct
                                    ));
                                    self.errors.push(err);
                                    return;
                                }
                            } else {
                                let err = self.type_context.analyze_context.err(format!(
                                    "Unable to find generic struct in new_struct list: {:#?}",
                                    un_op
                                ));
                                self.errors.push(err);
                                return;
                            }
                        } else {
                            let err = self
                                .type_context
                                .analyze_context
                                .err(format!("Unable to find generic struct: {:#?}", un_op));
                            self.errors.push(err);
                            return;
                        }
                    };

                    *member_idx = Some(idx as u64);
                }

                // TODO:
                Err(err) => {
                    self.errors.push(err);
                }

                _ => {
                    let err = self.type_context.analyze_context.err(format!(
                        "Expression that was struct accessed wasn't struct or compound, was: {:#?}",
                        un_op.value
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_var_decl(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        if let Stmt::VariableDecl(var, _) = stmt {
            if let Some(ty) = &mut var.ret_type {
                self.subtitute_type(ty, true);
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to find infer type for var decl ret_type: {:?}",
                    stmt
                ));
                self.errors.push(err);
            }
        }
    }
}
