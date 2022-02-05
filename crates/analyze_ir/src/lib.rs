use common::error::{LangError, LangErrorKind};
use ir::{IrError, Module};

//use const_fold::const_fold_new;
use remove_unused::remove_unused;

//mod const_fold;
mod remove_unused;

fn into_err(err: IrError) -> LangError {
    LangError {
        msg: err.msg,
        kind: LangErrorKind::IrError,
        file_pos: None,
        backtrace: err.backtrace,
    }
}

pub fn analyze(ir_module: &mut Module, optimize: bool) -> Result<(), Vec<LangError>> {
    if optimize {
        //const_fold_new(ir_module).map_err(|e| vec![e])?;
        //remove_unused(ir_module);
    }
    Ok(())
}
