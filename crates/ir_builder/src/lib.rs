mod build;
mod collect;
mod state;

use build::expr::build_expr;
use common::{
    ctx::{analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx},
    error::{LangError, LangErrorKind, LangResult},
    order::dependency_order,
    token::{
        ast::AstToken,
        block::{Adt, Fn},
        stmt::Modifier,
    },
    ty::{
        get::get_ident, inner_ty::InnerTy, to_string::to_string_path, ty::Ty, ty_env::TyEnv,
        type_id::TypeId,
    },
    util,
};
use ir::{ExprTy, FuncDecl, FuncVisibility, IrError, IrResult, Module, Type};

use crate::{
    build::{block::build_block, stmt::build_stmt},
    builder::InstrBuilder,
    collect::{
        adt::collect_type_decls, func::collect_func_decls, global::collect_globals,
        local::collect_locals, param::collect_params,
    },
    state::BuildState,
};

mod builder;

#[derive(Debug, Clone, Copy)]
pub enum VarModifier {
    None,
    Const,
}

// TODO: Need to verify that the expresion in "match" is constant.
// TODO: Need to verify that value in while loop evaluates to i1/bool.
// TODO: Verify types when doing building "special" assign stmt (ex. +=, -= etc.).
// TODO: Verify types for integer/float literals.
// TODO: Make sure that the amount of arguments used when calling a function is
//       correct (need to consider variadic arguments, also check types).
// TODO: Make sure that function pointer calls are done on pointers.
// TODO: Check if struct/union init is const. This should probably be done for
//       more things ex. variables as well.
// TODO: Check so that lhs and rhs have the same type (or ar compatible).
//
//
//
//
//

fn into_err(err: IrError) -> LangError {
    LangError {
        msg: err.msg,
        kind: LangErrorKind::IrError,
        file_pos: None,
        backtrace: err.backtrace,
    }
}

pub fn build_module(
    module_name: String,
    ptr_size: usize,
    analyze_ctx: &mut AnalyzeCtx,
    ast_root: &mut AstToken,
) -> Result<Module, Vec<LangError>> {
    let mut module = Module::new(module_name, ptr_size);

    let adt_order = dependency_order(analyze_ctx, ast_root, false, true)?;
    let ast_ctx = &mut analyze_ctx.ast_ctx;
    let ty_env = analyze_ctx.ty_env;

    collect_type_decls(&mut module, ast_ctx, ty_env, &adt_order).map_err(|e| vec![e])?;
    collect_func_decls(&mut module, ast_ctx, ty_env, ast_root).map_err(|e| vec![e])?;
    let globals = collect_globals(&mut module, ast_ctx, ty_env).map_err(|e| vec![e])?;
    let locals = collect_locals(&mut module, ast_ctx, ty_env, ast_root)?;
    let params = collect_params(ty_env, ast_root).map_err(|e| vec![e])?;

    let mut instr_builder = InstrBuilder::default();
    let mut build_state = BuildState::new(
        &mut module,
        &mut instr_builder,
        analyze_ctx,
        globals,
        locals,
        params,
    );
    build_token(&mut build_state, ast_root).map_err(|e| vec![e])?;

    let mut structs_order = Vec::with_capacity(module.structs.len());
    for adt_path in &adt_order {
        let adt_name = to_string_path(&analyze_ctx.ty_env.lock(), adt_path);
        if module.structs.contains_key(&adt_name) {
            structs_order.push(adt_name);
        }
    }
    module.add_structs_order(&structs_order);

    Ok(module)
}

fn build_token(state: &mut BuildState, ast_token: &AstToken) -> LangResult<()> {
    match ast_token {
        AstToken::Block(block) => build_block(state, block),
        AstToken::Stmt(stmt) => build_stmt(state, stmt),
        AstToken::Expr(expr) => build_expr(state, expr, ExprTy::RValue).map(|_| ()),
        AstToken::Comment(..) | AstToken::Empty | AstToken::EOF => Ok(()),
    }
}

/// Converts the given `type_id` into the corresponding IR `Type`.
fn to_ir_type(ast_ctx: &AstCtx, ty_env: &TyEnv, type_id: TypeId) -> LangResult<Type> {
    let inf_type_id = ty_env.inferred_type(type_id)?;
    let ty = ty_env.ty(inf_type_id).unwrap();
    Ok(match ty {
        Ty::CompoundType(inner_ty, ..) => match inner_ty {
            InnerTy::Struct(adt_path)
            | InnerTy::Enum(adt_path)
            | InnerTy::Union(adt_path)
            | InnerTy::Tuple(adt_path) => {
                let adt = ast_ctx.get_adt(ty_env, adt_path)?;
                let adt = adt.read();
                to_ir_adt(ty_env, &adt)
            }

            InnerTy::Void => Type::Void,
            InnerTy::Character => Type::Char,
            InnerTy::Boolean => Type::Bool,

            InnerTy::I8 => Type::I8,
            InnerTy::U8 => Type::U8,
            InnerTy::I16 => Type::I16,
            InnerTy::U16 => Type::U16,
            InnerTy::I32 => Type::I32,
            InnerTy::U32 => Type::U32,
            InnerTy::F32 => Type::F32,
            InnerTy::I64 => Type::I64,
            InnerTy::U64 => Type::U64,
            InnerTy::F64 => Type::F64,
            InnerTy::I128 => Type::I128,
            InnerTy::U128 => Type::U128,

            // TODO: Traits shouldn't end up here?
            InnerTy::String
            | InnerTy::Trait(_)
            | InnerTy::Unknown(_)
            | InnerTy::UnknownIdent(_, _)
            | InnerTy::UnknownInt(_, _)
            | InnerTy::UnknownFloat(_) => unreachable!("Bad inner type: {:#?}", inner_ty),
        },

        Ty::Pointer(type_id_i, ..) => {
            Type::Pointer(Box::new(to_ir_type(ast_ctx, ty_env, *type_id_i)?))
        }
        Ty::Array(type_id_i, dim_expr, ..) => {
            let dim = if let Some(dim_expr) = dim_expr {
                Some(util::get_array_dim(dim_expr, dim_expr.file_pos().cloned())?)
            } else {
                None
            };

            Type::Array(Box::new(to_ir_type(ast_ctx, ty_env, *type_id_i)?), dim)
        }
        Ty::Fn(_, params, ret_type_id, _) => {
            let mut param_ir_types = Vec::with_capacity(params.len());
            for param_type_id in params {
                param_ir_types.push(to_ir_type(ast_ctx, ty_env, *param_type_id)?);
            }

            let ret_ir_type = if let Some(ret_type_id) = ret_type_id {
                to_ir_type(ast_ctx, ty_env, *ret_type_id)?
            } else {
                Type::Void
            };

            Type::FuncPointer(param_ir_types, Box::new(ret_ir_type))
        }

        _ => unreachable!("Bad type -- type_id: {}, ty: {:#?}", type_id, ty),
    })
}

/// Converts the given `func` into the corresponding IR `FuncDecl`.
fn to_ir_func(ast_ctx: &AstCtx, ty_env: &TyEnv, func: &Fn) -> LangResult<FuncDecl> {
    let fn_full_name = fn_full_name(ty_env, func)?;

    let param_types = if let Some(params) = &func.parameters {
        let mut param_types = Vec::with_capacity(params.len());
        for param in params {
            let param = param.read();
            if let Some(type_id) = param.ty {
                let ir_type = to_ir_type(ast_ctx, ty_env, type_id)?;
                param_types.push(ir_type);
            } else {
                return Err(LangError::new(
                    format!(
                        "Param with name \"{}\" in function \"{:?}\" has no type set.",
                        &param.full_name(),
                        fn_full_name
                    ),
                    LangErrorKind::IrError,
                    None,
                ));
            }
        }
        param_types
    } else {
        Vec::with_capacity(0)
    };

    let ret_type = if let Some(ret_type_id) = func.ret_type {
        to_ir_type(ast_ctx, ty_env, ret_type_id)?
    } else {
        Type::Void
    };

    let visibility = if func.modifiers.contains(&Modifier::External) {
        FuncVisibility::Import
    } else if func.modifiers.contains(&Modifier::Public) || &fn_full_name == "main" {
        FuncVisibility::Export
    } else {
        FuncVisibility::None
    };

    Ok(FuncDecl::new(
        fn_full_name,
        visibility,
        param_types,
        ret_type,
        func.is_var_arg,
    ))
}

/// Given the `adt`, returns a `Type` that acts as a reference to the given ADT.
fn to_ir_adt(ty_env: &TyEnv, adt: &Adt) -> Type {
    let adt_path = adt
        .module
        .clone_push(&adt.name, adt.generics.as_ref(), Some(adt.file_pos));
    Type::Adt(to_string_path(ty_env, &adt_path))
}

fn to_ir_adt_members(ast_ctx: &AstCtx, ty_env: &TyEnv, adt: &Adt) -> LangResult<Vec<Type>> {
    let adt_full_name = adt_full_name(ty_env, adt);

    let mut member_types = Vec::with_capacity(adt.members.len());
    for member in &adt.members {
        let member = member.read();
        if let Some(type_id) = member.ty {
            let ir_type = to_ir_type(ast_ctx, ty_env, type_id)?;
            member_types.push(ir_type);
        } else {
            return Err(LangError::new(
                format!(
                    "Member with name \"{}\" of ADT \"{}\" has no type set.",
                    &member.full_name(),
                    &adt_full_name
                ),
                LangErrorKind::IrError,
                None,
            ));
        }
    }
    Ok(member_types)
}

pub fn fn_full_name(ty_env: &TyEnv, func: &Fn) -> LangResult<String> {
    Ok(if let Some(adt_type_id) = func.method_adt {
        let adt_path = get_ident(ty_env, adt_type_id)?.unwrap();
        util::to_method_name(ty_env, &adt_path, &func.name, func.generics.as_ref())
    } else {
        let func_path =
            func.module
                .clone_push(&func.name, func.generics.as_ref(), Some(func.file_pos));
        to_string_path(ty_env, &func_path)
    })
}

pub fn adt_full_name(ty_env: &TyEnv, adt: &Adt) -> String {
    let adt_path = adt
        .module
        .clone_push(&adt.name, adt.generics.as_ref(), Some(adt.file_pos));
    to_string_path(ty_env, &adt_path)
}

fn assert_type_eq(lhs_type: &Type, rhs_type: &Type) -> IrResult<()> {
    match (lhs_type, rhs_type) {
        (Type::Pointer(inner_type_a), Type::Pointer(inner_type_b)) => {
            assert_type_eq(inner_type_a, inner_type_b)?;
        }
        (Type::Array(inner_type_a, dim_a), Type::Array(inner_type_b, dim_b)) if dim_a == dim_b => {
            assert_type_eq(inner_type_a, inner_type_b)?;
        }

        (Type::Adt(name_a), Type::Adt(name_b)) | (Type::Func(name_a), Type::Func(name_b))
            if name_a == name_b => {}

        (Type::FuncPointer(params_a, ret_a), Type::FuncPointer(params_b, ret_b)) => {
            let mut is_eq = true;

            if params_a.len() != params_b.len() {
                is_eq = false;
            }
            for (param_a, param_b) in params_a.iter().zip(params_b) {
                if assert_type_eq(param_a, param_b).is_err() {
                    is_eq = false;
                    break;
                }
            }
            if assert_type_eq(ret_a, ret_b).is_err() {
                is_eq = false;
            }

            if !is_eq {
                return Err(IrError::new(format!(
                    "Expected lhs and rhs types to match but they didn't (function pointers).\n\
                    Lhs type: {:#?}\nRhs type: {:#?}",
                    lhs_type, rhs_type,
                )));
            }
        }

        (Type::Void, Type::Void)
        | (Type::Char, Type::Char)
        | (Type::Bool, Type::Bool)
        | (Type::I8, Type::I8)
        | (Type::U8, Type::U8)
        | (Type::I16, Type::I16)
        | (Type::U16, Type::U16)
        | (Type::I32, Type::I32)
        | (Type::U32, Type::U32)
        | (Type::F32, Type::F32)
        | (Type::I64, Type::I64)
        | (Type::U64, Type::U64)
        | (Type::F64, Type::F64)
        | (Type::I128, Type::I128)
        | (Type::U128, Type::U128) => (),

        // TODO: Add line/column nr to error.
        _ => {
            return Err(IrError::new(format!(
                "Expected lhs and rhs types to match but they didn't.\n\
                Lhs type: {:#?}\nRhs type: {:#?}",
                lhs_type, rhs_type,
            )));
        }
    }
    Ok(())
}

fn assert_number(ir_type: &Type) -> IrResult<()> {
    if ir_type.is_number() {
        Ok(())
    } else {
        Err(IrError::new(format!(
            "Expected type to be number, got: {:?}.",
            ir_type,
        )))
    }
}

fn assert_number_or_pointer(ir_type: &Type) -> IrResult<()> {
    if ir_type.is_number() || ir_type.is_pointer() {
        Ok(())
    } else {
        Err(IrError::new(format!(
            "Expected type to be number or pointer, got: {:?}.",
            ir_type,
        )))
    }
}

fn assert_int(ir_type: &Type) -> IrResult<()> {
    if ir_type.is_int() {
        Ok(())
    } else {
        Err(IrError::new(format!(
            "Expected type to be integer, got: {:?}.",
            ir_type,
        )))
    }
}
