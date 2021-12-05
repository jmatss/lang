use common::error::LangResult;
use generator::CodeGen;
use inkwell::{builder::Builder, context::Context, module::Module, targets::TargetMachine};

mod generator;
mod instr_end;
mod instr_expr;
mod op;
mod util;

pub fn generate<'a, 'ctx>(
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
    target_machine: &'a TargetMachine,
    ir_module: ir::Module,
) -> LangResult<()> {
    CodeGen::new(context, builder, module, target_machine, ir_module).compile()
}
