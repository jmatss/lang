use ir::{size_with_padding, Module, Type};
use parking_lot::Mutex;

use common::{
    ctx::{ast_ctx::AstCtx, block_ctx::BlockCtx},
    error::{LangError, LangErrorKind, LangResult},
    hash::DerefType,
    path::LangPath,
    token::block::AdtKind,
    ty::ty_env::TyEnv,
};

use crate::{adt_full_name, into_err, to_ir_adt_members};

/// Collects all ADTs (struct/enum/unions/tuples) that can be found in the
/// `AstCtx` and inserts them into the given `module`.
///
pub(crate) fn collect_type_decls(
    module: &mut Module,
    ast_ctx: &AstCtx,
    ty_env: &Mutex<TyEnv>,
    adt_order: &[LangPath],
) -> LangResult<()> {
    let ty_env_guard = ty_env.lock();

    for adt_path in adt_order {
        let parent_id = BlockCtx::DEFAULT_BLOCK_ID;
        let key = (adt_path.clone(), parent_id);

        let adt = if let Some(adt) = ast_ctx.adts.get(&ty_env_guard, DerefType::Deep, &key)? {
            adt
        } else {
            return Err(LangError::new(
                format!(
                    "collect_type_decls() - Unable to find ADT with path: {:?}",
                    adt_path
                ),
                LangErrorKind::IrError,
                adt_path.file_pos().cloned(),
            ));
        };

        let adt = adt.read();
        let adt_full_name = adt_full_name(&ty_env_guard, &adt);

        match adt.kind {
            AdtKind::Struct | AdtKind::Tuple => {
                let members = if adt.has_definition {
                    Some(to_ir_adt_members(
                        ast_ctx,
                        &ty_env_guard,
                        module.ptr_size,
                        &adt,
                    )?)
                } else {
                    None
                };
                module
                    .add_struct(adt_full_name, members)
                    .map_err(into_err)?;
            }
            AdtKind::Union => {
                let members = to_ir_adt_members(ast_ctx, &ty_env_guard, module.ptr_size, &adt)?;
                let mut largest_size = 0;
                for member in &members {
                    let cur_size = size_with_padding(module, member).map_err(into_err)?;
                    if cur_size > largest_size {
                        largest_size = cur_size;
                    }
                }

                let data_type = Type::Array(Box::new(Type::U8), Some(largest_size as u128));
                let tag_type = Type::U8;
                module
                    .add_struct(adt_full_name, Some(vec![data_type, tag_type]))
                    .map_err(into_err)?;
            }
            AdtKind::Enum => {
                // TODO: int/uint.
                let member_type = Type::U64;
                module
                    .add_struct(adt_full_name, Some(vec![member_type]))
                    .map_err(into_err)?;
            }
            AdtKind::Unknown => {
                unreachable!("Tried to compile {:#?}", adt.kind);
            }
        }
    }
    Ok(())
}
