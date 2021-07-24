use crate::spec::{
    instr::Instr,
    section::Locals,
    types::{FuncType, Name},
};

#[derive(Debug, Clone)]
pub enum FuncVisibility {
    /// Will be added as an export in the wasm module. A function with this
    /// visibility needs to implement a function body.
    Export,

    /// Will be imported into the wasm module. A function with this visibility
    /// should not implement a function body.
    Import,

    /// A function that won't be exported from the wasm module. A function with
    /// this visibility needs to implement a function body.
    None,
}

/// When accessing the locals inside this func, the parameters will be prepended
/// to the locals. So the the first variable of `self.locals` will have the
/// index "params.len()", and the first param will have index 0 (if any).
#[derive(Debug, Clone)]
pub struct Func {
    name: Name,
    module: Option<Name>,
    ty: FuncType,
    visibility: FuncVisibility,

    /* These are not set for declarations */
    locals: Vec<Locals>,
    body: Vec<Instr>,
}

impl Func {
    pub fn new(name: Name, module: Option<Name>, ty: FuncType, visibility: FuncVisibility) -> Self {
        let (locals, body) = match visibility {
            FuncVisibility::Export | FuncVisibility::None => (Vec::default(), Vec::default()),
            FuncVisibility::Import => (Vec::with_capacity(0), Vec::with_capacity(0)),
        };
        Self {
            name,
            module,
            ty,
            visibility,
            locals,
            body,
        }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn module(&self) -> Option<&Name> {
        self.module.as_ref()
    }

    pub fn last_instr(&self) -> Option<&Instr> {
        self.body.last()
    }

    // TODO: Functions to access instructions in this func. Ex. see if the func
    //       has a return instruction etc.
}
