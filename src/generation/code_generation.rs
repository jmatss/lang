/*
use crate::analyzer::analyzer::AnalyzeContext;
use crate::error::CustomError;
use crate::parser::abstract_syntax_tree::AST;
use crate::parser::token::{Function, Variable};
use crate::CustomResult;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{FunctionValue, PointerValue};

struct CodeGen<'a, 'ctx> {
    pub gen_context: &'ctx Context,
    pub analyze_context: &'a AnalyzeContext,

    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,

    pub ast: &'a AST,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    pub fn new(
        gen_context: &Context,
        analyze_context: &AnalyzeContext,
        builder: &Builder,
        fpm: &PassManager<FunctionValue>,
        module: &Module,
        ast: &AST,
    ) -> Self {
        Self {
            gen_context,
            analyze_context,
            builder,
            fpm,
            module,
            ast,
        }
    }

    pub fn create_entry_block_alloca(
        &self,
        func: &FunctionValue<'ctx>,
        name: &Variable,
    ) -> PointerValue<'ctx> {
        let builder = self.gen_context.create_builder();

        let entry = func
            .get_first_basic_block()
            .expect("Unable to unwrap first basic block in func.");

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => buider.position_at_end(entry),
        }

        builder.build_al
    }

    pub fn compile_func(&self, func: &Function) {}

    pub fn compile_func_prototype(&self, func: &Function) {
        /*
        let return_type = if let Some(return_type) = &func.return_type {
            return_type.
        } else {

        };
        let ret_type = func.return_type
        */
    }

    pub fn compile_expression() {}

    pub fn compile_variable() {}
}
 */
