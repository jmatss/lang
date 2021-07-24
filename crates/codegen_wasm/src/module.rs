use std::collections::{hash_map::Entry, HashMap};

use common::error::{LangError, LangErrorKind, LangResult};

use crate::{func::Func, spec::types::Name};

pub struct Module {
    name: Name,
    fns: HashMap<Name, Func>,
}

impl Module {
    pub fn new(name: Name) -> Self {
        Self {
            name,
            fns: HashMap::default(),
        }
    }

    /// Creates a "full name" for a function, concatenating a optional module
    /// with a name.
    fn full_name(name: &Name, module: Option<&Name>) -> Name {
        if let Some(module) = module {
            Name::combine(module, name)
        } else {
            name.clone()
        }
    }

    pub fn add_function(&mut self, func: Func) -> LangResult<()> {
        let full_name = Self::full_name(func.name(), func.module());
        match self.fns.entry(full_name) {
            Entry::Occupied(entry) => Err(LangError::new(
                format!("Found multiple functions with name: {:?}", entry.key()),
                LangErrorKind::CodeGenError,
                None,
            )),
            Entry::Vacant(entry) => {
                entry.insert(func);
                Ok(())
            }
        }
    }

    pub fn get_function(&self, name: &Name, module: Option<&Name>) -> LangResult<&Func> {
        let full_name = Self::full_name(&name, module);
        self.fns.get(&full_name).ok_or_else(|| {
            LangError::new(
                format!("Unable to find function with name: {:?}", &full_name),
                LangErrorKind::CodeGenError,
                None,
            )
        })
    }

    pub fn get_function_mut(
        &mut self,
        name: &Name,
        module: Option<&Name>,
    ) -> LangResult<&mut Func> {
        let full_name = Self::full_name(&name, module);
        self.fns.get_mut(&full_name).ok_or_else(|| {
            LangError::new(
                format!("Unable to find function with name: {:?}", &full_name),
                LangErrorKind::CodeGenError,
                None,
            )
        })
    }
}
