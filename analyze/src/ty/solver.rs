use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use common::{
    error::LangError,
    token::op::UnOperator,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function, Struct},
        expr::{ArrayInit, BuiltInCall, Expr, FuncCall, StructInit, Var},
        op::{BinOp, UnOp},
        stmt::Stmt,
    },
    traverser::TraverseContext,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    util,
    visitor::Visitor,
};
use log::warn;

use super::context::{SubResult, TypeContext};

pub struct TypeSolver<'a> {
    type_context: &'a mut TypeContext<'a>,

    /// Will contain all generic structs that have had the generic types substituted
    /// with real types. The key is the name of the struct (WITHOUT generics).
    /// The `Ty` in the value is the type of the struct including name+generics.
    pub generic_structs: HashMap<String, Vec<(Rc<RefCell<Struct>>, Ty)>>,

    // The outer key is the struct name WITH generics.
    // The inner key is the name of the method.
    // The inner value is the block ID for the function and the function itself.
    pub generic_struct_methods: HashMap<String, HashMap<String, Rc<RefCell<Function>>>>,

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

            SubResult::UnSolved(un_sub_ty) if finalize => {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to resolve type {:?}. Got back unsolved: {:?}.",
                    ty, un_sub_ty
                ));
                warn!(
                    "ERROR HERE -- Unable to resolve type {:?}. Got back unsolved: {:?}.",
                    ty, un_sub_ty
                );
                self.errors.push(err);
            }

            SubResult::UnSolved(un_sub_ty) => *ty = un_sub_ty,

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
        let full_name = match struct_init.full_name() {
            Ok(full_name) => full_name,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // See if a struct with these generics already exists. If that is the
        // case, nothing to do here, a struct with the same generic impls have
        // already been created. Early return.
        if let Some(struct_impls) = self.generic_structs.get(&struct_init.name) {
            for (struct_impl, _) in struct_impls {
                if struct_impl.borrow().name == full_name {
                    return;
                }
            }
        }

        // Get the actual struct implementation and create a copy of it.
        let mut gen_struct = match self
            .type_context
            .analyze_context
            .get_struct(&struct_init.name, ctx.block_id)
        {
            Ok(struct_ty) => struct_ty.borrow().clone(),
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        let generics = if let Some(generics) = struct_init.generics() {
            generics.clone()
        } else {
            Generics::new()
        };

        // Give the new copy of the struct type the "full name" containing
        // information about the generic arguments.
        gen_struct.name = full_name.clone();

        // Is used to replace all old references to the struct without generics
        // replaced. This will be done for all members/methods and also the
        // `method_structure` indicating which struct a method belongs to.
        let gen_struct_ty =
            Ty::CompoundType(InnerTy::Struct(struct_init.name.clone()), generics.clone());

        // For every member of the struct, replace any generic types with
        // the type of the struct_init generics. Also replace any reference
        // to the old name with the new full name (containing generics).
        if let Some(members) = &mut gen_struct.members {
            for member in members {
                let mut new_member = member.borrow().clone();

                warn!(
                    "REPLACE MEMBER -- gen_struct.name: {}, member: {:#?}",
                    gen_struct.name, new_member
                );

                if let Some(ty) = &mut new_member.ty {
                    ty.replace_generics_impl(&generics);
                    ty.replace_self(&struct_init.name, &gen_struct_ty);
                }

                warn!("RESULT -- member: {:#?}", new_member);

                *member = Rc::new(RefCell::new(new_member));
            }
        }

        // For every method of the struct, replace any generic types with
        // the type of the struct_init generics. Also replace any reference
        // to the old name with the new full name (containing generics).
        //
        // The methods are RCs inside the "Struct" struct which means that they
        // are still tied to the "old" struct, need create copies of the methods.
        if let Some(methods) = &mut gen_struct.methods {
            let mut new_methods = HashMap::default();

            for (method_name, method) in methods {
                let new_method = Rc::new(RefCell::new(method.borrow().clone()));

                // Replace generics in parameters.
                if let Some(parameters) = &mut new_method.borrow_mut().parameters {
                    for param in parameters {
                        let new_param = Rc::new(RefCell::new(param.borrow().clone()));

                        warn!(
                            "REPLACE PARAM -- method_name: {}, param: {:#?}",
                            method_name, new_param
                        );

                        if let Some(ty) = &mut new_param.borrow_mut().ty {
                            ty.replace_generics_impl(&generics);
                            ty.replace_self(&struct_init.name, &gen_struct_ty);
                        }

                        warn!("RESULT -- param: {:#?}", new_param);

                        *param = new_param;
                    }
                }

                // Replace generics in return type.
                if let Some(ret_ty) = &mut new_method.borrow_mut().ret_type {
                    ret_ty.replace_generics_impl(&generics);
                    ret_ty.replace_self(&struct_init.name, &gen_struct_ty)
                }

                new_method.borrow_mut().method_structure = Some(gen_struct_ty.clone());

                *method = new_method;
                new_methods.insert(method_name.clone(), Rc::clone(&method));
            }

            self.generic_struct_methods.insert(full_name, new_methods);
        }

        // This new struct variable will be added as a new struct type stored
        // in `self.generic_structs` and will then be inserted into the
        // AST later on by another "class" ("type_converter").
        match self.generic_structs.entry(struct_init.name.clone()) {
            Entry::Occupied(ref mut o) => {
                let entry = (Rc::new(RefCell::new(gen_struct)), gen_struct_ty);
                o.get_mut().push(entry);
            }
            Entry::Vacant(v) => {
                let entry = vec![(Rc::new(RefCell::new(gen_struct)), gen_struct_ty)];
                v.insert(entry);
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
        self.type_context.analyze_context.file_pos =
            ast_token.file_pos().cloned().unwrap_or_default();
    }

    /// Iterate through all expressions and substitute all Unknown types that
    /// was used temporarily during the type inference stage. They should be
    /// now be replaced with "real" types that can be found in the `substitutions`.
    fn visit_expr(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        // Do the substitute for "Type" here, all other exprs will make the subs
        // in their own visit funcs.
        if let Expr::Type(ty, ..) = expr {
            self.subtitute_type(ty, true);
        }
    }

    fn visit_lit(&mut self, expr: &mut Expr, _ctx: &TraverseContext) {
        if let Expr::Lit(_, Some(ty), ..) = expr {
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
        if let Some(ty) = &mut var.ty {
            self.subtitute_type(ty, true);
        } else {
            let err = self
                .type_context
                .analyze_context
                .err(format!("Unable to find infer type for var: {:?}", var));
            self.errors.push(err);
        }
    }

    fn visit_func(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Function(func), ..) = &mut ast_token {
            if let Some(params) = &func.borrow().parameters {
                for param in params {
                    if let Some(param_ty) = &mut param.borrow_mut().ty {
                        self.subtitute_type(param_ty, true);
                    }
                }
            }
        }
    }

    fn visit_func_call(&mut self, func_call: &mut FuncCall, _ctx: &TraverseContext) {
        if let Some(ty) = &mut func_call.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for func call: {:?}",
                func_call
            ));
            self.errors.push(err);
        }

        if let Some(structure_ty) = &mut func_call.method_structure {
            self.subtitute_type(structure_ty, true);

            // TODO: Fix this, seems very random to fix this here.
            // The `method_structure` might possible be a pointer to the
            // structure, need to get the actual structure type in that case.
            if let Ty::Pointer(ty) = structure_ty {
                *structure_ty = *ty.clone();
            }
        }

        // Solve the generic types.
        if let Some(generics) = &mut func_call.generics {
            for generic_ty in generics.iter_types_mut() {
                self.subtitute_type(generic_ty, true);
            }
        }
    }

    fn visit_built_in_call(&mut self, built_in_call: &mut BuiltInCall, _ctx: &TraverseContext) {
        if let Some(ty) = &mut built_in_call.ret_type {
            self.subtitute_type(ty, true);
        } else {
            let err = self.type_context.analyze_context.err(format!(
                "Unable to find infer type for built-in call: {:?}",
                built_in_call
            ));
            self.errors.push(err);
        }

        // Solve the generic types.
        if let Some(generics) = &mut built_in_call.generics {
            for generic_ty in generics.iter_types_mut() {
                self.subtitute_type(generic_ty, true);
            }
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
                        let empty_struct = Rc::new(RefCell::new(Struct::new("".into())));
                        if let Some(new_structs) = self.generic_structs.get(old_name) {
                            let mut new_struct = &empty_struct;
                            let mut is_found = false;

                            for (curr_new_struct, _) in new_structs {
                                if curr_new_struct.borrow().name == new_name {
                                    is_found = true;
                                    new_struct = curr_new_struct;
                                    break;
                                }
                            }

                            if is_found {
                                if let Some(idx) = new_struct.borrow().member_index(member_name) {
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
        if let Stmt::VariableDecl(var, ..) = stmt {
            if let Some(ty) = &mut var.borrow_mut().ty {
                self.subtitute_type(ty, true);
            } else {
                let err = self.type_context.analyze_context.err(format!(
                    "Unable to find infer type for var decl ret_type: {:?}",
                    var
                ));
                self.errors.push(err);
            }
        }
    }
}
