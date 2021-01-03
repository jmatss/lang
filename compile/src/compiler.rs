use crate::CustomResult;
use common::error::{LangError, LangErrorKind::CompileError};
use inkwell::{
    module::Module,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    values::FunctionValue,
    OptimizationLevel,
};

pub fn setup_target() -> CustomResult<TargetMachine> {
    Target::initialize_all(&InitializationConfig::default());

    // TODO: Allow the user to specify target from command line.
    let triple = TargetMachine::get_default_triple();
    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();
    let level = OptimizationLevel::Default;
    let reloc_mode = RelocMode::Default;
    let code_model = CodeModel::Default;

    let target = Target::from_triple(&triple)?;
    if let Some(machine) =
        target.create_target_machine(&triple, &cpu, &features, level, reloc_mode, code_model)
    {
        Ok(machine)
    } else {
        Err(LangError::new(
            "Unable to create target machine.".into(),
            CompileError,
            None,
        ))
    }
}

pub fn compile(
    machine: TargetMachine,
    module: &Module,
    output_path: &str,
    optimize: bool,
) -> CustomResult<()> {
    if optimize {
        let func_pass_manager: PassManager<FunctionValue> = PassManager::create(module);
        func_pass_manager.add_instruction_combining_pass();
        func_pass_manager.add_reassociate_pass();
        func_pass_manager.add_constant_propagation_pass();
        func_pass_manager.add_gvn_pass();
        func_pass_manager.add_cfg_simplification_pass();
        func_pass_manager.add_basic_alias_analysis_pass();
        func_pass_manager.add_promote_memory_to_register_pass();
        func_pass_manager.add_memcpy_optimize_pass();
        func_pass_manager.add_tail_call_elimination_pass();
        func_pass_manager.add_ind_var_simplify_pass();
        func_pass_manager.add_loop_unroll_pass();
        func_pass_manager.add_instruction_combining_pass();
        func_pass_manager.add_reassociate_pass();
        func_pass_manager.initialize();

        let mut cur_func_opt = module.get_first_function();
        while let Some(cur_func) = cur_func_opt {
            func_pass_manager.run_on(&cur_func);
            cur_func_opt = cur_func.get_next_function();
        }

        let module_pass_manager = PassManager::create(());
        module_pass_manager.add_constant_merge_pass();
        module_pass_manager.add_argument_promotion_pass();
        module_pass_manager.add_dead_arg_elimination_pass();
        module_pass_manager.add_licm_pass();
        module_pass_manager.add_function_inlining_pass();
        module_pass_manager.add_cfg_simplification_pass();

        module_pass_manager.run_on(module);
    }

    let file_type = FileType::Object;
    machine
        .write_to_file(&module, file_type, output_path.as_ref())
        .map_err(|e| LangError::new(e.to_string(), CompileError, None))
}
