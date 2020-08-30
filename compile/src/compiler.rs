use crate::CustomResult;
use common::error::{LangError, LangErrorKind::CompileError};
use inkwell::{
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
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
        ))
    }
}

pub fn compile(machine: TargetMachine, module: &Module, output_path: &str) -> CustomResult<()> {
    let file_type = FileType::Object;
    machine
        .write_to_file(&module, file_type, output_path.as_ref())
        .map_err(|e| LangError::new(e.to_string(), CompileError))
}
