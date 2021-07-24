mod build;
mod collect;
mod state;

use common::{
    ctx::{analyze_ctx::AnalyzeCtx, ast_ctx::AstCtx},
    error::{LangError, LangErrorKind, LangResult},
    token::{
        ast::AstToken,
        block::{Adt, AdtKind, Fn},
        stmt::Modifier,
    },
    ty::{inner_ty::InnerTy, to_string::to_string_path, ty::Ty, ty_env::TyEnv, type_id::TypeId},
    util,
};

use ir::{
    decl::{
        func::{Func, FuncVisibility},
        ty::Type,
    },
    error::IrError,
    module::Module,
};

use crate::{
    build::block::build_block,
    collect::{
        adt::collect_type_decls, ext::collect_extern_decls, func::collect_func_decls,
        global::collect_global_vars,
    },
    state::BuildState,
};

// TODO: Need to verify that the expresion in "match" is constant.
// TODO: Need to verify that value in while loop evaluates to i1/bool.
// TODO: Verify types when doing building "special" assign stmt (ex. +=, -= etc.).
// TODO: Verify types for integer/float literals.
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
    analyze_ctx: &mut AnalyzeCtx,
    ast_root: &mut AstToken,
) -> Result<Module, Vec<LangError>> {
    let mut module = Module::new(module_name);

    let ast_ctx = &mut analyze_ctx.ast_ctx;
    let ty_env_mutex = analyze_ctx.ty_env;
    collect_extern_decls(&mut module, ast_ctx, ty_env_mutex, ast_root)?;

    let globals = {
        let ty_env_guard = ty_env_mutex.lock().unwrap();
        collect_type_decls(&mut module, ast_ctx, &ty_env_guard).map_err(|e| vec![e])?;
        collect_func_decls(&mut module, ast_ctx, &ty_env_guard, ast_root).map_err(|e| vec![e])?;
        collect_global_vars(&mut module, ast_ctx, &ty_env_guard).map_err(|e| vec![e])?
    };

    let mut build_state = BuildState::new(&mut module, analyze_ctx);
    build_token(&mut build_state, ast_root).map_err(|e| vec![e])?;

    Ok(module)
}

fn build_token(state: &mut BuildState, ast_token: &AstToken) -> LangResult<()> {
    match ast_token {
        AstToken::Block(block) => build_block(state, block),
        AstToken::Expr(expr) => todo!(),
        AstToken::Stmt(stmt) => build_stmt(state, stmt),
        AstToken::Comment(..) | AstToken::Empty | AstToken::EOF => Ok(()),
    }
}

/// Converts the given `type_id` into the corresponding IR `Type`.
fn to_ir_type(ast_ctx: &AstCtx, ty_env: &TyEnv, type_id: TypeId) -> LangResult<Type> {
    let ty = ty_env.ty(type_id).unwrap();
    Ok(match ty {
        Ty::CompoundType(inner_ty, ..) => match inner_ty {
            InnerTy::Struct(adt_path) | InnerTy::Enum(adt_path) | InnerTy::Union(adt_path) => {
                let adt = ast_ctx.get_adt(ty_env, adt_path)?;
                let adt = adt.as_ref().read().unwrap();
                to_ir_adt(ast_ctx, ty_env, &adt)?
            }

            InnerTy::Void => Type::Void,
            InnerTy::Character => Type::Character,
            InnerTy::Boolean => Type::Boolean,

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

            Type::Func(param_ir_types, Box::new(ret_ir_type))
        }

        _ => unreachable!("Bad type -- type_id: {}, ty: {:#?}", type_id, ty),
    })
}

/// Converts the given `func` into the corresponding IR `Func`.
fn to_ir_func(ast_ctx: &AstCtx, ty_env: &TyEnv, func: &Fn) -> LangResult<Func> {
    let func_path = func
        .module
        .clone_push(&func.name, func.generics.as_ref(), Some(func.file_pos));
    let func_full_name = to_string_path(ty_env, &func_path);

    let param_types = if let Some(params) = &func.parameters {
        let mut param_types = Vec::with_capacity(params.len());
        for param in params {
            let param = param.as_ref().read().unwrap();
            if let Some(type_id) = param.ty {
                let ir_type = to_ir_type(ast_ctx, ty_env, type_id)?;
                param_types.push(ir_type);
            } else {
                return Err(LangError::new(
                    format!(
                        "Param with name \"{}\" in function \"{:?}\" has no type set.",
                        &param.name, func_path
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
    } else if func.modifiers.contains(&Modifier::Public) {
        FuncVisibility::Export
    } else {
        FuncVisibility::None
    };

    Ok(Func::new(func_full_name, visibility, param_types, ret_type))
}

/// Converts the given `adt` into the corresponding IR ADT `Type`.
fn to_ir_adt(ast_ctx: &AstCtx, ty_env: &TyEnv, adt: &Adt) -> LangResult<Type> {
    let adt_path = adt
        .module
        .clone_push(&adt.name, adt.generics.as_ref(), Some(adt.file_pos));
    let adt_full_name = to_string_path(ty_env, &adt_path);

    let mut member_types = Vec::with_capacity(adt.members.len());
    for member in &adt.members {
        let member = member.as_ref().read().unwrap();
        if let Some(type_id) = member.ty {
            let ir_type = to_ir_type(ast_ctx, ty_env, type_id)?;
            member_types.push(ir_type);
        } else {
            return Err(LangError::new(
                format!(
                    "Member with name \"{}\" of ADT \"{:?}\" has no type set.",
                    &member.name, adt_path
                ),
                LangErrorKind::IrError,
                None,
            ));
        }
    }

    match adt.kind {
        AdtKind::Struct => Ok(Type::Struct(adt_full_name, member_types)),
        AdtKind::Enum => Ok(Type::Enum(adt_full_name, member_types.len())),
        AdtKind::Union => Ok(Type::Union(adt_full_name, member_types)),
        AdtKind::Unknown => Err(LangError::new(
            format!("Adt bad type: {:#?}", adt),
            LangErrorKind::IrError,
            None,
        )),
    }
}
