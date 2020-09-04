use super::type_inference::TypeChoice;
use crate::{type_inference::unify, AnalyzeContext};
use common::error::LangError;
use common::{
    token::{
        block::BlockHeader,
        expr::{AccessInstruction, Expression, FuncCall, StructInit, Var},
        lit::Lit,
        op::{BinOp, BinOperator, Op, UnOp, UnOperator},
        stmt::Statement,
    },
    variable_type::{Type, TypeStruct},
};
use log::debug;
use parse::token::{ParseToken, ParseTokenKind};
pub struct TypeAnalyzer<'a> {
    context: &'a mut AnalyzeContext,
    errors: Vec<LangError>,
}

// TODO: Infer the types if it can be figured out.
//       Should probably use the first type found as the type for the whole
//       variable.

impl<'a> TypeAnalyzer<'a> {
    /// Takes in the root of the AST and tries to infer all the missing types
    /// for variables and expressions (what type the expressions evaluates to).
    pub fn analyze(
        context: &'a mut AnalyzeContext,
        ast_root: &mut ParseToken,
    ) -> Result<(), Vec<LangError>> {
        let mut type_analyzer = TypeAnalyzer::new(context);

        // Run it twice. The first run will go through everything and will find
        // all the types and assign them to ex. the variables. The second
        // run will use that information and propagate all the types to the
        // expressions.
        type_analyzer.analyze_type(ast_root);
        type_analyzer.analyze_type(ast_root);

        if type_analyzer.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut type_analyzer.errors))
        }
    }

    fn new(context: &'a mut AnalyzeContext) -> Self {
        Self {
            context,
            errors: Vec::default(),
        }
    }

    fn analyze_type(&mut self, token: &mut ParseToken) {
        self.context.cur_line_nr = token.line_nr;
        self.context.cur_column_nr = token.column_nr;

        match token.kind {
            ParseTokenKind::Block(ref mut header, id, ref mut body) => {
                self.context.cur_block_id = id;
                self.analyze_header_type(header);
                for token in body {
                    self.context.cur_line_nr = token.line_nr;
                    self.context.cur_column_nr = token.column_nr;
                    self.analyze_type(token);
                }
            }
            ParseTokenKind::Statement(ref mut stmt) => {
                self.analyze_stmt_type(stmt);
            }
            ParseTokenKind::Expression(ref mut expr) => {
                self.analyze_expr_type(expr, None);
            }
            ParseTokenKind::EndOfFile => (),
        }
    }

    /// The `type_hitn` is a type that might have been found from a parent and
    /// should be cascaded down the expression and be set for them as well.
    fn analyze_expr_type(
        &mut self,
        expr: &mut Expression,
        type_hint_opt: Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        match expr {
            Expression::Lit(lit, cur_type_opt) => {
                // If this literal already has a type that is NOT inferred, don't
                // change it, do early return.
                if let Some(cur_type) = cur_type_opt {
                    if !cur_type.is_inferred {
                        return cur_type_opt.clone();
                    }
                }

                // If the new type is prefered, use it.
                let new_type_opt = Some(self.analyze_literal_type(lit));
                if let Some(TypeChoice::Second) =
                    unify(cur_type_opt.as_ref(), new_type_opt.as_ref())
                {
                    *cur_type_opt = new_type_opt;
                }

                // If no type is set for the literal, use the type hint. It
                // doesn't matter what the type hint is since it will always
                // be better or equal to the current None type.
                // Also make sure that the given type hint is of the same type
                // as the literal.
                if let Some(type_hint) = type_hint_opt.clone() {
                    let possible_type_hint = match lit {
                        Lit::String(_) if type_hint.ty.is_string() => Some(type_hint),
                        Lit::Char(_) if type_hint.ty.is_char() => Some(type_hint),
                        Lit::Bool(_) if type_hint.ty.is_bool() => Some(type_hint),
                        Lit::Integer(_, _) if type_hint.ty.is_int() => Some(type_hint),
                        Lit::Float(_) if type_hint.ty.is_float() => Some(type_hint),
                        _ => None,
                    };

                    // If the type hint is prefered, use it.
                    if let Some(TypeChoice::Second) =
                        unify(cur_type_opt.as_ref(), possible_type_hint.as_ref())
                    {
                        *cur_type_opt = possible_type_hint;
                    }
                }

                // Return the now (possible) updated type.
                cur_type_opt.clone()
            }
            Expression::Type(type_struct) => Some(type_struct.clone()),
            Expression::Var(var) => {
                debug!("ANALYZING VAR: {:#?}", var);

                // TODO: FIXME: Currently the type hint is used when analyzing
                //              a "regular" var. Should it be used when analyzing
                //              struct members as well? Will there be any problems
                //              if it isn't used?
                if let Some((ref root_var, _)) = var.access_instrs {
                    if root_var.is_struct {
                        self.analyze_struct_member(var)
                    } else {
                        self.analyze_var_type(var, type_hint_opt)
                    }
                } else {
                    self.analyze_var_type(var, type_hint_opt)
                }
            }
            Expression::Op(op) => self.analyze_op_type(op, type_hint_opt),
            Expression::FuncCall(func_call) => {
                // If true: This is a method. The type hint given from the lhs
                //          will be the type of the struct that this method
                //          is called on.
                // Else: This is a regular function call.
                if func_call.access_instrs.is_some() {
                    if let Some(type_hint) = type_hint_opt {
                        self.analyze_method_call(func_call, type_hint)
                    } else {
                        let err = self.context.err(format!(
                            "Analyzing type of method call with None type hint, method name: {}",
                            &func_call.name
                        ));
                        self.errors.push(err);
                        None
                    }
                } else {
                    self.analyze_func_call(func_call)
                }
            }
            Expression::StructInit(struct_init) => self.analyze_struct_init(struct_init),
            Expression::ArrayInit(args, init_type_opt) => {
                if args.is_empty() {
                    let err = self.context.err("ArrayInit with no arguments.".into());
                    self.errors.push(err);
                    return None;
                }

                let type_opt = if let Some(init_type) = init_type_opt {
                    Some(init_type.clone())
                } else if let Some(ref type_hint) = type_hint_opt {
                    Some(type_hint.clone())
                } else {
                    None
                };

                let mut member_type_opt = if let Some(type_struct) = type_opt {
                    if let Type::Array(inner, dim) = type_struct.ty {
                        Some(*inner)
                    } else {
                        None
                    }
                } else {
                    None
                };

                for arg in args.iter_mut() {
                    // TODO: Need to comapre `member_type_opt` and the actual type
                    //       of the args returned from the call below.
                    if member_type_opt.is_none() {
                        member_type_opt =
                            self.analyze_expr_type(&mut arg.value, member_type_opt.clone());
                    }
                }

                let member_type = if let Some(member_type) = member_type_opt {
                    member_type
                } else {
                    let err_msg = format!("Unable to deduce type for array init members. Args: {:?}, array type: {:?}", args, init_type_opt);
                    let err = self.context.err(err_msg);
                    self.errors.push(err);
                    return None;
                };

                // Infer a type from the type of the members of this array.
                let lit = Lit::Integer(args.len().to_string(), 10);
                let dim_type = TypeStruct::new(Type::I32, None, false);
                let dim_expr = Expression::Lit(lit, Some(dim_type));
                let generics = None;
                let inferred = false;
                let new_init_type = TypeStruct::new(
                    Type::Array(Box::new(member_type), Some(Box::new(dim_expr))),
                    generics,
                    inferred,
                );

                if let Some(init_type) = init_type_opt {
                    if init_type.is_inferred {
                        *init_type_opt = Some(new_init_type);
                    }
                } else {
                    *init_type_opt = Some(new_init_type);
                }
                init_type_opt.clone()
            }
        }
    }

    fn analyze_func_call(&mut self, func_call: &mut FuncCall) -> Option<TypeStruct> {
        let cur_block_id = self.context.cur_block_id;
        let func_block_id = match self
            .context
            .get_func_parent_id(func_call.name.clone(), cur_block_id)
        {
            Ok(id) => id,
            Err(e) => {
                self.errors.push(e);
                return None;
            }
        };

        // Get the function from the map parsed during "decl analyzing".
        let key = (func_call.name.clone(), func_block_id);
        let func = if let Some(func) = self.context.functions.get(&key) {
            func.clone()
        } else {
            let err_msg = format!("Unable to find func decl for: {}", &func_call.name);
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        };

        // Analyze all arguments of the function call and make sure that
        // the amount of types of the args/params are the same. They can
        // differ if this function is variadic.
        if let Some(func_params) = func.parameters {
            if (func_call.arguments.len() == func_params.len() && !func.is_var_arg)
                || (func_call.arguments.len() >= func_params.len() && func.is_var_arg)
            {
                for (i, arg) in func_call.arguments.iter_mut().enumerate() {
                    let prev_type_opt = if i < func_params.len() {
                        func_params
                            .get(i)
                            .expect("More args than params, unreachable.")
                            .ret_type
                            .clone()
                    } else {
                        None
                    };
                    self.analyze_expr_type(&mut arg.value, prev_type_opt);
                }
            } else {
                let err_msg = format!(
                    "Func call to {}, incorrect amount of param/arg (vararg={}). Actual func #: {}, got: {}.",
                    &func_call.name,
                    func.is_var_arg,
                    func_params.len(),
                    func_call.arguments.len()
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return None;
            }
        } else if !func_call.arguments.is_empty() {
            let err_msg = format!(
                "Func {} has no params, but func_call had {} args.",
                &func_call.name,
                func_call.arguments.len()
            );
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        }

        // Always return the type of the actual function.
        func.ret_type.clone()
    }

    fn analyze_method_call(
        &mut self,
        func_call: &mut FuncCall,
        type_hint: TypeStruct,
    ) -> Option<TypeStruct> {
        let struct_name = if let Type::Custom(struct_name) = type_hint.ty {
            struct_name
        } else {
            let err_msg = format!(
                "lhs type during method call not a struct: {:#?}",
                &type_hint
            );
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        };

        let cur_block_id = self.context.cur_block_id;
        let struct_block_id = match self
            .context
            .get_struct_parent_id(struct_name.clone(), cur_block_id)
        {
            Ok(struct_id) => struct_id,
            Err(err) => {
                self.errors.push(err);
                return None;
            }
        };

        // Get the method from the map parsed during "decl analyzing".
        let key = (struct_name.clone(), struct_block_id);
        let method = if let Some(methods) = self.context.methods.get(&key) {
            if let Some(method) = methods.get(&func_call.name) {
                method.clone()
            } else {
                let err_msg = format!(
                    "Unable to find method with name \"{}\" for struct \"{}\" in block {}.",
                    &func_call.name, struct_name, struct_block_id
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return None;
            }
        } else {
            let err_msg = format!(
                "Unable to find methods for struct \"{}\" in block {}.",
                &struct_name, struct_block_id
            );
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        };

        // Analyze all arguments of the method call and make sure that
        // the amount of types of the args/params are the same. The method
        // will have a extra parameters which represents "this"/"self". They can
        // differ if this method is variadic.
        if let Some(func_params) = method.parameters {
            if (func_call.arguments.len() + 1 == func_params.len() && !method.is_var_arg)
                || (func_call.arguments.len() + 1 >= func_params.len() && method.is_var_arg)
            {
                for (i, arg) in func_call.arguments.iter_mut().enumerate() {
                    let prev_type_opt = if i + 1 < func_params.len() {
                        func_params
                            .get(i + 1)
                            .expect("More args than params, unreachable.")
                            .ret_type
                            .clone()
                    } else {
                        None
                    };
                    self.analyze_expr_type(&mut arg.value, prev_type_opt);
                }
            } else {
                let err_msg = format!(
                    "Method call to {} for struct {}, incorrect amount of param/arg (vararg={}). \
                    Actual method #: {}, got: {}(+ 1).",
                    &func_call.name,
                    &struct_name,
                    method.is_var_arg,
                    func_params.len(),
                    func_call.arguments.len()
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return None;
            }
        } else if !func_call.arguments.is_empty() {
            let err_msg = format!(
                "Method {} has no params, but func_call had {} args.",
                &func_call.name,
                func_call.arguments.len()
            );
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        }

        // Always return the type of the actual function.
        method.ret_type.clone()
    }

    fn analyze_struct_init(&mut self, struct_init: &mut StructInit) -> Option<TypeStruct> {
        let cur_block_id = self.context.cur_block_id;
        let struct_block_id = match self
            .context
            .get_struct_parent_id(struct_init.name.clone(), cur_block_id)
        {
            Ok(id) => id,
            Err(e) => {
                self.errors.push(e);
                return None;
            }
        };

        // Get the struct from the map parsed during "decl analyzing".
        let key = (struct_init.name.clone(), struct_block_id);
        let struct_ = if let Some(struct_) = self.context.structs.get(&key) {
            struct_.clone()
        } else {
            let err_msg = format!("Unable to find struct decl for: {}", &struct_init.name);
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        };

        // Analyze all arguments of the struct init and make sure that
        // the amount of types of the args/members are the same.
        // This is done by using the members type as the `prev_type_opt`.
        if let Some(struct_members) = struct_.members {
            if struct_init.arguments.len() == struct_members.len() {
                for (i, arg) in struct_init.arguments.iter_mut().enumerate() {
                    let member = struct_members.get(i).expect("member/arg len diff.");
                    self.analyze_expr_type(&mut arg.value, member.ret_type.clone())?;
                }
            } else {
                let err_msg = format!(
                    "Struct/init to {}, members/args amount differs. Expected: {}, got: {}.",
                    &struct_init.name,
                    struct_members.len(),
                    struct_init.arguments.len()
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return None;
            }
        } else if !struct_init.arguments.is_empty() {
            let err_msg = format!(
                "Struct {} has no params, but struct_init had {} args.",
                &struct_init.name,
                struct_init.arguments.len()
            );
            let err = self.context.err(err_msg);
            self.errors.push(err);
            return None;
        }

        // Return the type of the struct.
        let ty = Type::Custom(struct_init.name.clone());
        let generics = None;
        let inferred = false;
        Some(TypeStruct::new(ty, generics, inferred))
    }

    // TODO: This function also sets the `block_info` (info about struct root
    //       and index in struct) for the member. This should be idealy be moved
    //       to "indexing_analyzer" if possible, it is unrelated to
    //       "type analyzing" but this was the easiest place to implement it in.
    /// This function is called when a "Dot" binary operator has been found
    /// with a "Variable" at the right hand side. This means that this is a
    /// indexing of a struct. Try to figure out the type of the member
    /// by looking up the struct, and then finding the type of the member
    /// in the struct definition.
    fn analyze_struct_member(&mut self, var: &mut Var) -> Option<TypeStruct> {
        if let Some((ref root_var, ref mut access_instrs)) = var.access_instrs {
            let root_var_name = &root_var.name;
            let decl_id = root_var.decl_block_id;

            // Find the first use of a struct. This will be the entry point to
            // accessing all structs/members recursively in this `var`.
            let key = (root_var_name.clone(), decl_id);
            let mut struct_name = if let Some(context_var) = self.context.variables.get(&key) {
                let mut ty = context_var.ret_type.clone()?.ty;
                loop {
                    match ty {
                        Type::Pointer(ptr) => {
                            ty = ptr.ty;
                        }
                        Type::Array(arr_ty, _) => {
                            ty = arr_ty.ty;
                        }
                        Type::Custom(struct_name) => {
                            break struct_name;
                        }
                        _ => {
                            let err_msg =
                                format!("Unable to find first struct in var: {:#?}", &var);
                            let err = self.context.err(err_msg);
                            self.errors.push(err);
                            return None;
                        }
                    }
                }
            } else {
                let err_msg = format!(
                    "Unable to find var {} in decl block id {}",
                    &root_var_name, decl_id
                );
                let err = self.context.err(err_msg);
                self.errors.push(err);
                return None;
            };

            for access_instr in &mut access_instrs.iter_mut() {
                match access_instr {
                    AccessInstruction::StructMember(
                        ref mut access_struct_var_name,
                        ref mut access_member_name,
                        ref mut access_index,
                    ) => {
                        let struct_ = match self.context.get_struct(&struct_name) {
                            Ok(Some(struct_)) => struct_,
                            Ok(None) => {
                                let err_msg =
                                    format!("Unable to get struct with name: {:?}", &struct_name);
                                let err = self.context.err(err_msg);
                                self.errors.push(err);
                                return None;
                            }
                            Err(e) => {
                                self.errors.push(e);
                                return None;
                            }
                        };

                        if let Some(members) = &struct_.members {
                            for (i, member) in members.iter().enumerate() {
                                if access_member_name == &member.name {
                                    *access_index = Some(i as u32);
                                    *access_struct_var_name = struct_name.clone();

                                    // These values might be update in multiple iterations,
                                    // it is only the last iteration that actualy counts
                                    // and it should also be the correct values.
                                    var.ret_type = member.ret_type.clone();
                                    var.modifiers = member.modifiers.clone();
                                    var.is_const = member.is_const;

                                    // If this member is a struct, store the nested
                                    // struct into `struct_`. This will be needed
                                    // since in the next loop iteration, it is the new
                                    // struct that will be indexed. Otherwise we have
                                    // recursed to the end of nesting, set the information
                                    // from this member to the var.
                                    let mut cur_type = member.ret_type.clone()?.ty;
                                    loop {
                                        match cur_type {
                                            Type::Pointer(ty) => {
                                                cur_type = ty.ty;
                                            }
                                            Type::Array(ty, _) => {
                                                // TODO: Is this logic valid for array,
                                                //       will the array indexing affect
                                                //       the "hierarchy" of indexing
                                                //       into it?
                                                cur_type = ty.ty;
                                            }
                                            Type::Custom(ref inner_struct_name) => {
                                                struct_name = inner_struct_name.clone();
                                                break;
                                            }
                                            _ => {
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                        } else {
                            let err_msg = format!(
                                "No members of struct {}, expected member with name {}.",
                                &struct_name, &access_member_name
                            );
                            self.errors.push(self.context.err(err_msg));
                            return None;
                        }
                    }
                    AccessInstruction::Deref
                    | AccessInstruction::Address
                    | AccessInstruction::ArrayAccess(_) => continue,
                };
            }

            var.ret_type.clone()
        } else {
            unreachable!("AccessInstructions not set in `analyze_struct_member`.");
        }
    }

    fn analyze_expr_type_opt(
        &mut self,
        expr_opt: &mut Option<Expression>,
        type_hint_opt: Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        if let Some(expr) = expr_opt {
            self.analyze_expr_type(expr, type_hint_opt)
        } else {
            None
        }
    }

    fn analyze_literal_type(&mut self, lit: &Lit) -> TypeStruct {
        let ty = match lit {
            Lit::String(_) => Type::String,
            Lit::Char(_) => Type::Character,
            Lit::Bool(_) => Type::Boolean,
            // Default type for int are Int.
            Lit::Integer(_, _) => Type::Int,
            // Default type for float are F32.
            Lit::Float(_) => Type::F32,
        };

        // TODO: Implement generics.
        let generics = None;
        let inferred = true;
        TypeStruct::new(ty, generics, inferred)
    }

    fn analyze_var_type(
        &mut self,
        var: &mut Var,
        type_hint_opt: Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        let cur_block_id = self.context.cur_block_id;
        let var_decl_id = match self.context.get_var_decl_scope(&var.name, cur_block_id) {
            Ok(id) => id,
            Err(e) => {
                self.errors.push(e);
                return None;
            }
        };

        // If the type hint is prefered, use it instead.
        if let Some(TypeChoice::Second) = unify(var.ret_type.as_ref(), type_hint_opt.as_ref()) {
            var.ret_type = type_hint_opt;
        }

        // TODO: Compare types from the current `var` and the var in context?
        // Check the type of the current variable and the global variable
        // that was put into the `context` during analyzing. Decide the prefered
        // type of the two and then assign it to both.
        let key = (var.name.clone(), var_decl_id);
        self.context.variables.entry(key).and_modify(|global_var| {
            match unify(global_var.ret_type.as_ref(), var.ret_type.as_ref()) {
                // Always prefer the type given to the global variable.
                Some(TypeChoice::First) | None => {
                    var.ret_type = global_var.ret_type.clone();
                }
                Some(TypeChoice::Second) => {
                    global_var.ret_type = var.ret_type.clone();
                }
            }

            // Update the fields that are set during declaration.
            var.modifiers = global_var.modifiers.clone();
            var.is_const = global_var.is_const;
        });

        var.ret_type.clone()
    }

    fn analyze_op_type(
        &mut self,
        op: &mut Op,
        type_hint_opt: Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        match op {
            Op::BinOp(ref mut bin_op) => {
                self.analyze_bin_op_type(bin_op, type_hint_opt)
            }
            Op::UnOp(ref mut un_op) => {
                self.analyze_un_op_type(un_op, type_hint_opt)
            }
        }
    }

    // TODO: This is completly broken. Need to check better. This will panic if the two expressions
    //       of the binary operation have different types, in most of those cases it should not panic.
    fn analyze_bin_op_type(
        &mut self,
        bin_op: &mut BinOp,
        type_hint_opt: Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        // TODO: If this is a assignment, the type of the right hand side should
        //       be compared to the type of the variable. If the variable doesn't
        //       have a type set but the right has, that type must be cascaded
        //       to the declaration of the variable so that it can be seen during
        //       codegen (since codegen will only look at the declaration of
        //       a variable).
        // If the left has a type set, use that as the "type_hint" when looking
        // at the type for the right side.
        let left_type = self.analyze_expr_type(&mut bin_op.left, type_hint_opt.clone());
        let left_type_hint_opt = if left_type.is_some() {
            left_type.clone()
        } else {
            type_hint_opt
        };
        let right_type = self.analyze_expr_type(&mut bin_op.right, left_type_hint_opt);

        let inferred_type = self.infer_type(bin_op, &left_type, &right_type);
        bin_op.ret_type = inferred_type.clone();
        inferred_type
    }

    fn analyze_un_op_type(
        &mut self,
        un_op: &mut UnOp,
        type_hint_opt: Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        self.analyze_expr_type(&mut un_op.value, None);

        let ret_type = match un_op.operator {
            UnOperator::Deref => {
                // Analyze the "outer" value that should be a pointer. Then deref
                // the result to get the value that is inside the pointer.
                if let Some(type_struct) = self.analyze_expr_type(&mut un_op.value, type_hint_opt) {
                    if let Type::Pointer(inner) = type_struct.ty {
                        Some(*inner)
                    } else {
                        let err_msg =
                            format!("Trying to dereference non pointer type: {:?}", type_struct);
                        self.errors.push(self.context.err(err_msg));
                        return None;
                    }
                } else {
                    let err_msg = "Type set to None when dereferencing.".into();
                    self.errors.push(self.context.err(err_msg));
                    return None;
                }
            }
            UnOperator::Address => {
                // The type hint should be a pointer. Since we want to analyze
                // the inner value, unwrap the pointer type hint.
                let inner_type_hint_opt = if let Some(ptr_type_hint) = type_hint_opt {
                    if let Type::Pointer(ref inner_type_hint) = ptr_type_hint.ty {
                        Some(*inner_type_hint.clone())
                    } else {
                        let err_msg = format!(
                            "Type hint during Address not a pointer: {:?}.",
                            ptr_type_hint
                        );
                        self.errors.push(self.context.err(err_msg));
                        return None;
                    }
                } else {
                    None
                };

                // Analyze the "inner" value. Then take the address of that
                // which will give us the address.
                if let Some(type_struct) =
                    self.analyze_expr_type(&mut un_op.value, inner_type_hint_opt)
                {
                    let new_ptr = Type::Pointer(Box::new(type_struct));
                    let generics = None;
                    let inferred = false;
                    Some(TypeStruct::new(new_ptr, generics, inferred))
                } else {
                    let err_msg = "Type set to None when taking address.".into();
                    self.errors.push(self.context.err(err_msg));
                    return None;
                }
            }
            UnOperator::ArrayAccess(_) => {
                // Analyze the "outer" value that should be a array. Then deref
                // the result to get the value that is inside the pointer.
                if let Some(type_struct) = self.analyze_expr_type(&mut un_op.value, type_hint_opt) {
                    if let Type::Array(inner, _) = type_struct.ty {
                        Some(*inner)
                    } else {
                        let err_msg = format!("Trying to index non array type: {:?}", type_struct);
                        self.errors.push(self.context.err(err_msg));
                        return None;
                    }
                } else {
                    let err_msg = "Type set to None when indexing.".into();
                    self.errors.push(self.context.err(err_msg));
                    return None;
                }
            }

            UnOperator::Increment
            | UnOperator::Decrement
            | UnOperator::Positive
            | UnOperator::Negative
            | UnOperator::BitComplement
            | UnOperator::BoolNot => self.analyze_expr_type(&mut un_op.value, type_hint_opt),
        };
        un_op.ret_type = ret_type.clone();
        ret_type
    }

    fn analyze_stmt_type(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Break
            | Statement::Continue
            | Statement::Use(_)
            | Statement::Package(_)
            | Statement::Modifier(_)
            | Statement::ExternalDecl(_) => (),

            Statement::Defer(expr) => {
                self.analyze_expr_type(expr, None);
            }
            Statement::DeferExecution(expr) => {
                self.analyze_expr_type(expr, None);
            }
            Statement::Return(expr_opt) => {
                self.analyze_expr_type_opt(expr_opt, None);
            }
            Statement::Yield(expr) => {
                self.analyze_expr_type(expr, None);
            }
            Statement::Assignment(_, lhs, rhs) => {
                // TODO: Should this check so that the left and right hand side
                //       have the same type (or just are compatible?).
                let lhs_type = self.analyze_expr_type(lhs, None);
                let expr_type = self.analyze_expr_type(rhs, lhs_type);

                // get the variable from the lhs expr.
                let var = if let Some(var) = lhs.eval_to_var() {
                    var
                } else {
                    let err_msg = format!("lhs of assignment not evaluated to var: {:?}", lhs);
                    self.errors.push(self.context.err(err_msg));
                    return;
                };

                // Update the variable type if it is None.
                if var.ret_type.is_none() {
                    var.ret_type = expr_type;
                }

                // Update the type of the variable in the "AnalyzeContext"
                // if the type is None. Don't do this for struct members (since
                // their types will always be hardcoded in the source code).
                if var.access_instrs.is_none() {
                    let cur_block_id = self.context.cur_block_id;
                    let var_decl_id = match self.context.get_var_decl_scope(&var.name, cur_block_id)
                    {
                        Ok(id) => id,
                        Err(e) => {
                            self.errors.push(e);
                            return;
                        }
                    };
                    let key = (var.name.clone(), var_decl_id);
                    self.context.variables.entry(key).and_modify(|global_var| {
                        match unify(global_var.ret_type.as_ref(), var.ret_type.as_ref()) {
                            // Always prefer the type given to the global variable.
                            Some(TypeChoice::First) | None => {
                                var.ret_type = global_var.ret_type.clone();
                            }
                            Some(TypeChoice::Second) => {
                                global_var.ret_type = var.ret_type.clone();
                            }
                        }
                    });
                }
            }
            Statement::VariableDecl(var, expr_opt) => {
                // TODO: Should this check so that the left and right hand side
                //       have the same type (or just are compatible?).
                // If a type can be found for the right hand size, set the left
                // side variable  to have the same type (if the variable doesn't
                // already have a pre-defined type).
                let expr_type = self.analyze_expr_type_opt(expr_opt, var.ret_type.clone());

                // Update the variable type if it is None.
                if var.ret_type.is_none() {
                    var.ret_type = expr_type;
                }

                // Update the type of the variable in the "AnalyzeContext"
                // if the type is None.
                let cur_block_id = self.context.cur_block_id;
                let var_decl_id = match self.context.get_var_decl_scope(&var.name, cur_block_id) {
                    Ok(id) => id,
                    Err(e) => {
                        self.errors.push(e);
                        return;
                    }
                };
                let key = (var.name.clone(), var_decl_id);
                self.context.variables.entry(key).and_modify(|global_var| {
                    match unify(global_var.ret_type.as_ref(), var.ret_type.as_ref()) {
                        // Always prefer the type given to the global variable.
                        Some(TypeChoice::First) | None => {
                            var.ret_type = global_var.ret_type.clone();
                        }
                        Some(TypeChoice::Second) => {
                            global_var.ret_type = var.ret_type.clone();
                        }
                    }
                });

                // TODO: Do this in a cleaner way.
                // Re-analyze the expressions since the var ret type might have
                // been updated.
                self.analyze_expr_type_opt(expr_opt, var.ret_type.clone());
            }
        }
    }

    fn analyze_header_type(&mut self, block_header: &mut BlockHeader) {
        match block_header {
            BlockHeader::IfCase(expr_opt) => {
                self.analyze_expr_type_opt(expr_opt, None);
            }
            BlockHeader::Match(expr) => {
                self.analyze_expr_type(expr, None);
            }
            BlockHeader::MatchCase(expr) => {
                self.analyze_expr_type(expr, None);
            }
            BlockHeader::While(expr_opt) => {
                self.analyze_expr_type_opt(expr_opt, None);
            }

            BlockHeader::For(var, expr) => {
                // The type of the variable `var` will be infered to the same
                // as the expression if no type is specified in the var itself.
                let ret_type = self.analyze_expr_type(expr, var.ret_type.clone());
                if var.ret_type.is_none() {
                    var.ret_type = ret_type;
                }
            }

            BlockHeader::Default
            | BlockHeader::Function(_)
            | BlockHeader::Implement(..)
            | BlockHeader::Struct(_)
            | BlockHeader::Enum(_)
            | BlockHeader::Interface(_)
            | BlockHeader::If
            | BlockHeader::Test(_)
            | BlockHeader::Anonymous => (),
        }
    }

    // TODO: How should inheritance/implements etc. work?
    fn infer_type(
        &mut self,
        bin_op: &mut BinOp,
        left_type_opt: &Option<TypeStruct>,
        right_type_opt: &Option<TypeStruct>,
    ) -> Option<TypeStruct> {
        if left_type_opt.is_some() && right_type_opt.is_none() {
            self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
            left_type_opt.clone()
        } else if left_type_opt.is_none() && right_type_opt.is_some() {
            self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
            right_type_opt.clone()
        } else if left_type_opt.is_some() && right_type_opt.is_some() {
            let left_type = left_type_opt.clone().expect("left type None");
            let right_type = right_type_opt.clone().expect("right type None");

            // TODO: Generics.
            // Match and see if this is a binary operation where one of the
            // sides should be prefered. Otherwise have a look at the both
            // types and try to figure out which one to prefer.
            Some(match bin_op.operator {
                BinOperator::In => left_type,
                BinOperator::Is => right_type,
                BinOperator::As => right_type,
                BinOperator::Of => right_type,
                BinOperator::Dot => right_type,
                BinOperator::ShiftLeft => left_type,
                BinOperator::ShiftRight => left_type,
                _ => {
                    // The "compare" function will try and promote values and take the
                    // one with the "higesht priority". Returns None if unable to compare
                    // the types.
                    if let Some(type_choice) = self.compare_type(&left_type, &right_type) {
                        match type_choice {
                            TypeChoice::First => {
                                self.set_type(bin_op.right.as_mut(), left_type_opt.clone());
                                left_type
                            }
                            TypeChoice::Second => {
                                self.set_type(bin_op.left.as_mut(), right_type_opt.clone());
                                right_type
                            }
                        }
                    } else {
                        // TODO: Arbitrary choice of left_type, just take one.
                        left_type
                    }
                }
            })
        } else {
            // Both none.
            None
        }
    }

    /// Set the type of a expression after it has ben inferred.
    fn set_type(&mut self, expr: &mut Expression, new_ty: Option<TypeStruct>) {
        match expr {
            Expression::Lit(_, old_ty) => *old_ty = new_ty,
            Expression::Var(var) => var.ret_type = new_ty,
            Expression::Op(op) => match op {
                Op::BinOp(bin_op) => bin_op.ret_type = new_ty,
                Op::UnOp(un_op) => un_op.ret_type = new_ty,
            },
            Expression::FuncCall(_) => {
                // Do nothing, the "return type" of a function call will be
                // taken from the function declaration.
            }

            // TODO: Make it so that one can set types to structInit/arrayInit
            //       so that they can be used inside expressions to allow for
            //       chaining.
            // Can't set type for struct init or array init.
            Expression::StructInit(_) | Expression::ArrayInit(..) | Expression::Type(_) => {
                let err_msg = format!("Tried to set type for unexpected expr: {:?}", &expr);
                let err = self.context.err(err_msg);
                self.errors.push(err);
            }
        }
    }
}
