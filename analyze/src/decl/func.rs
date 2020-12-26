use crate::{AnalyzeContext, BlockInfo};
use common::{
    error::LangError,
    token::expr::Var,
    token::{
        ast::AstToken,
        block::{BlockHeader, Function},
        stmt::Modifier,
        stmt::Stmt,
    },
    traverser::TraverseContext,
    ty::{generics::Generics, inner_ty::InnerTy, ty::Ty},
    visitor::Visitor,
    BlockId,
};

use std::{cell::RefCell, rc::Rc};

/// Gathers information about all function/method declarations found in the AST
/// and inserts them into the `analyze_context`. This includes external function
/// declarations, functions and methods (in implement block).
pub struct DeclFuncAnalyzer<'a> {
    analyze_context: &'a RefCell<AnalyzeContext>,
    errors: Vec<LangError>,
}

impl<'a> DeclFuncAnalyzer<'a> {
    pub fn new(analyze_context: &'a RefCell<AnalyzeContext>) -> Self {
        Self {
            analyze_context,
            errors: Vec::default(),
        }
    }

    fn analyze_func_header(&mut self, func: &mut Rc<RefCell<Function>>, func_id: BlockId) {
        // The function will be added in the scope of its parent, so fetch the
        // block id for the parent.
        let parent_id = match self.analyze_context.borrow().get_parent(func_id) {
            Ok(parent_id) => parent_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // If true: Function already declared somewhere, make sure that the
        // current declaration and the previous one matches.
        if let Ok(prev_func) = self
            .analyze_context
            .borrow()
            .get_func(&func.borrow().name, parent_id)
        {
            let func = func.borrow();

            let empty_vec = Vec::new();
            let cur_func_params = if let Some(params) = &func.parameters {
                params
            } else {
                &empty_vec
            };

            let prev_func = prev_func.borrow();
            let prev_func_params = if let Some(params) = &prev_func.parameters {
                params
            } else {
                &empty_vec
            };

            // Check that they have the same amount of parameters and
            // their types are equal.
            if cur_func_params.len() != prev_func_params.len() {
                let err_msg = format!(
                    "Two declarations of function \"{}\" have different amount of parameters. \
                    Prev amount: {}, current amount: {}",
                    &func.name,
                    cur_func_params.len(),
                    prev_func_params.len(),
                );
                let err = self.analyze_context.borrow().err(err_msg);
                self.errors.push(err);
            } else {
                for (i, (cur_param, prev_param)) in cur_func_params
                    .iter()
                    .zip(prev_func_params.iter())
                    .enumerate()
                {
                    let cur_param = cur_param.borrow();
                    let prev_param = prev_param.borrow();

                    if cur_param.name != prev_param.name {
                        let err_msg = format!(
                            "Two declarations of function \"{}\" have parameters with different names. \
                            Parameter at position {}. Prev name: {:?}, current name: {:?}.",
                            &func.name, i, &cur_param.name, &prev_param.name
                        );
                        let err = self.analyze_context.borrow().err(err_msg);
                        self.errors.push(err);
                    }
                    if cur_param.ty != prev_param.ty {
                        let param_name = if cur_param.name == prev_param.name {
                            cur_param.name.clone()
                        } else {
                            format!("{}/{}", &prev_param.name, &cur_param.name)
                        };
                        let err_msg = format!(
                            "Two declarations of function \"{}\" have parameters with different types. \
                            Parameter at position {} with name \"{}\". \
                            Prev type: {:?}, current type: {:?}",
                            &func.name, i, &param_name, cur_param.ty, prev_param.ty
                        );
                        let err = self.analyze_context.borrow().err(err_msg);
                        self.errors.push(err);
                    }
                }
            }

            // Need to do early return and not do the logic below in a else block
            // to make rust not fail becaose of the `analyze_context` borrw.
            return;
        }

        // Add the function into decl lookup maps.
        let key = (func.borrow().name.clone(), parent_id);
        self.analyze_context
            .borrow_mut()
            .functions
            .insert(key, Rc::clone(func));

        // Add the parameters as variables in the function scope decl lookup.
        if let Some(params) = &func.borrow().parameters {
            for param in params {
                let param_key = (param.borrow().name.clone(), func_id);
                self.analyze_context
                    .borrow_mut()
                    .variables
                    .insert(param_key, Rc::clone(param));
            }
        }
    }

    fn analyze_method_header(
        &mut self,
        struct_name: &str,
        func: &mut Rc<RefCell<Function>>,
        func_id: BlockId,
    ) {
        // TODO: Make this work for all structures.

        // The method will be added in the scope of its wrapping struct, so
        // fetch the block id for the struct.
        let struct_decl_id = match self
            .analyze_context
            .borrow()
            .get_struct_decl_scope(struct_name, func_id)
        {
            Ok(struct_decl_id) => struct_decl_id,
            Err(err) => {
                self.errors.push(err);
                return;
            }
        };

        // TODO: Should probably be changed to something better.
        // If this is a non-static method, the first parameter should be a
        // reference(/pointer) to "this"/"self".
        if !func.borrow().is_static() {
            static THIS_VAR_NAME: &str = "this";
            let mut func = func.borrow_mut();

            let inner_ty = InnerTy::Struct(struct_name.into());
            let generics = Generics::new();

            let ty = if func.modifiers.contains(&Modifier::This) {
                Ty::CompoundType(inner_ty, generics)
            } else if func.modifiers.contains(&Modifier::ThisPointer) {
                Ty::Pointer(Box::new(Ty::CompoundType(inner_ty, generics)))
            } else {
                let err = self.analyze_context.borrow().err(format!(
                    "Non static function did not contain \"this\" or \"this ptr\" reference. Struct name: {}, func: {:#?}.",
                    struct_name, func
                ));
                self.errors.push(err);
                return;
            };

            let var = Rc::new(RefCell::new(Var::new(
                THIS_VAR_NAME.into(),
                Some(ty),
                None,
                None,
                None,
                false,
            )));
            if let Some(ref mut params) = func.parameters {
                params.insert(0, var);
            } else {
                func.parameters = Some(vec![var]);
            }
        }

        // Insert this method into `methods` in the analyze context.
        if let Err(err) = self.analyze_context.borrow_mut().insert_method(
            struct_name,
            Rc::clone(func),
            struct_decl_id,
        ) {
            self.errors.push(err);
            return;
        }

        // Add the parameters as variables in the method scope.
        if let Some(params) = &mut func.borrow_mut().parameters {
            for param in params {
                let param_key = (param.borrow().name.clone(), func_id);
                self.analyze_context
                    .borrow_mut()
                    .variables
                    .insert(param_key, Rc::clone(param));
            }
        }
    }
}

impl<'a> Visitor for DeclFuncAnalyzer<'a> {
    fn take_errors(&mut self) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_token(&mut self, ast_token: &mut AstToken, _ctx: &TraverseContext) {
        self.analyze_context.borrow_mut().file_pos =
            ast_token.file_pos().cloned().unwrap_or_default();
    }

    /// Marks the functions in this implement block with the name of the implement
    /// block (equivalent to the struct name). This lets one differentiate between
    /// functions and methods by checking the `method_struct` field in "Function"s.
    fn visit_impl(&mut self, mut ast_token: &mut AstToken, ctx: &TraverseContext) {
        let analyze_context = self.analyze_context.borrow();

        if let AstToken::Block(BlockHeader::Implement(ident), _, body) = &mut ast_token {
            // Check if this unknown structure can be found and then
            // replaced the inner type with the correct structure.
            let inner_ty = if analyze_context.get_struct(ident, ctx.block_id).is_ok() {
                InnerTy::Struct(ident.clone())
            } else if analyze_context.get_enum(ident, ctx.block_id).is_ok() {
                InnerTy::Enum(ident.clone())
            } else if analyze_context.get_interface(ident, ctx.block_id).is_ok() {
                InnerTy::Interface(ident.clone())
            } else {
                let err = analyze_context.err(format!(
                    "Unable to find structure for impl block: {:#?}",
                    ast_token
                ));
                self.errors.push(err);
                return;
            };

            let ty = Ty::CompoundType(inner_ty, Generics::new());

            for mut body_token in body {
                if let AstToken::Block(BlockHeader::Function(func), ..) = &mut body_token {
                    func.borrow_mut().method_structure = Some(ty.clone());
                } else {
                    let err = analyze_context.err(format!(
                        "AST token in impl block with name \"{}\" not a function: {:?}",
                        ident, body_token
                    ));
                    self.errors.push(err);
                }
            }
        }
    }

    fn visit_func(&mut self, mut ast_token: &mut AstToken, _ctx: &TraverseContext) {
        if let AstToken::Block(BlockHeader::Function(func), func_id, ..) = &mut ast_token {
            let structure_ty = if let Some(structure_ty) = func.borrow().method_structure.clone() {
                Some(structure_ty)
            } else {
                None
            };

            if let Some(structure_ty) = structure_ty {
                if let Ty::CompoundType(inner_ty, _) = structure_ty {
                    match inner_ty {
                        InnerTy::Struct(ident)
                        | InnerTy::Enum(ident)
                        | InnerTy::Interface(ident) => {
                            self.analyze_method_header(&ident, func, *func_id);
                        }

                        // TODO: Clean this logic up, does it need to exists
                        //       logic for both known and unknown idents here?
                        InnerTy::UnknownIdent(ident, id) => {
                            self.analyze_method_header(&ident, func, id);
                        }

                        _ => unreachable!(
                            "Method method_structure inner type not structure: {:#?}",
                            func
                        ),
                    }
                } else {
                    unreachable!("Method method_structure not CompoundType: {:#?}", func);
                }
            } else {
                self.analyze_func_header(func, *func_id);
            }
        }
    }

    fn visit_extern_decl(&mut self, stmt: &mut Stmt, _ctx: &TraverseContext) {
        let mut analyze_context = self.analyze_context.borrow_mut();

        if let Stmt::ExternalDecl(func, ..) = stmt {
            // TODO: Should probably check that if there are multiple extern
            //       declarations of a function that they have the same
            //       parameters & return type.
            // External declarations should always be in the default block.
            let key = (func.borrow().name.clone(), BlockInfo::DEFAULT_BLOCK_ID);
            analyze_context.functions.insert(key, Rc::clone(func));
        }
    }
}