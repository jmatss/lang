use crate::error::CustomError::CompileError;
use crate::CustomResult;
use inkwell::{
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};

pub fn compile(module: &Module, output_path: &str) -> CustomResult<()> {
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
        let file_type = FileType::Object;
        machine
            .write_to_file(&module, file_type, output_path.as_ref())
            .map_err(|e| CompileError(e.to_string()))
    } else {
        Err(CompileError("Unable to create target machine.".into()))
    }
}
