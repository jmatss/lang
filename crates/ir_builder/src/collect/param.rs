use std::collections::HashMap;

use parking_lot::Mutex;

use common::{
    error::{LangError, LangErrorKind, LangResult},
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
    },
    ty::ty_env::TyEnv,
    BlockId,
};
use ir::ParamVarIdx;

use crate::{fn_full_name, VarModifier};

pub type ParamVarMap = HashMap<(String, BlockId), (ParamVarIdx, VarModifier)>;

/// Collects all parameter variables for every function. The parameters will
/// already be stored in their respective `FuncDecl`s at this point.
///
/// This function will return a map mapping the variable names to their
/// corresponding `ParamVarIdx`. This is needed since the names of the variables
/// aren't stored in the module, it only uses indices. So one can use the maps
/// to lookup the variable uses in the AST (using names) and translate it to the
/// correct variable in the IR (using indices).
///
/// In the returned map, the first String key is the name of the function that
/// the params was found in. The values of the map are maps with the keys being
/// the parameter name+decl scope and the values being the `ParamVarIdx`s.
pub fn collect_params(
    ty_env: &Mutex<TyEnv>,
    ast_root: &AstToken,
) -> LangResult<HashMap<String, ParamVarMap>> {
    let mut params = HashMap::default();
    if let AstToken::Block(Block {
        header: BlockHeader::Default,
        body,
        ..
    }) = ast_root
    {
        collect_params_rec(&ty_env.lock(), &mut params, body)?;
        Ok(params)
    } else {
        Err(LangError::new(
            format!("Given \"ast_root\" not default block, was: {:#?}", ast_root),
            LangErrorKind::IrError,
            None,
        ))
    }
}

fn collect_params_rec(
    ty_env: &TyEnv,
    params: &mut HashMap<String, ParamVarMap>,
    tokens: &[AstToken],
) -> LangResult<()> {
    for token in tokens {
        // If this is a function, inserts the mapping between name and index for
        // all its parameters into the `params_and_locals` map.
        if let AstToken::Block(Block {
            header: BlockHeader::Fn(func),
            id: block_id,
            ..
        }) = token
        {
            let func = func.read();
            let fn_params = if let Some(params) = &func.parameters {
                params
            } else {
                continue;
            };

            let fn_full_name = fn_full_name(ty_env, &func)?;
            let func_entry = params.entry(fn_full_name).or_default();
            for (idx, param) in fn_params.iter().enumerate() {
                let key = (param.read().full_name(), *block_id);
                func_entry.insert(key, (ParamVarIdx(idx), VarModifier::Var));
            }
        } else if let AstToken::Block(Block { body, .. }) = token {
            collect_params_rec(ty_env, params, body)?;
        }
    }
    Ok(())
}
