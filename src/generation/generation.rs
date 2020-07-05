/*
use crate::error::CustomError;
use crate::CustomResult;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

pub fn generate() -> CustomResult<()> {
    let context = Context::create();
    let module = context.create_module("test");
    let execution_engine = module.create_execution_engine()?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };
}
*/
