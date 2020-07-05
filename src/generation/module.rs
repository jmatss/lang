/*
use iron_llvm::core::FunctionRef;

pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
}

pub struct SimpleModuleProvider {
    module: core::Module,
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
        let module = core::Module::new(name);

        SimpleModuleProvider { module }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None,
        }
    }
}
*/
