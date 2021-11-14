use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
};

use crate::{
    error::{IrError, IrResult},
    func::FuncDecl,
    ty::Type,
    Data, DataIdx, GlobalVarIdx,
};

pub struct Module {
    name: String,

    /// The string is the name of the function and the `FuncDecl` is the
    /// declaration of the function.
    funcs: HashMap<String, FuncDecl>,

    // TODO: How to handle externally declared structs? They have no members,
    //       but there is currently no way to mark a struct in this hashmap
    //       explicitly as an external struct declaration.
    //       For now the members are just set to an empty vector.
    /// The first string is the name of the struct and the vector of `Type`s
    /// are the members of the struct (in order).
    ///
    /// At this point enums and tuples have been compiled into structs, so they
    /// are stored in here as well.
    pub structs: HashMap<String, Vec<Type>>,

    global_vars: Vec<Type>,

    /// Contains static data that can be used to initialize values. This will
    /// mostly be string literals.
    data: Vec<Data>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            funcs: HashMap::default(),
            structs: HashMap::default(),
            global_vars: Vec::default(),
            data: Vec::default(),
        }
    }

    pub fn add_func(&mut self, name: String, func: FuncDecl) -> IrResult<()> {
        match self.funcs.entry(name) {
            Entry::Occupied(entry) => Err(IrError::new(format!(
                "Function with name \"{}\" already exists.",
                entry.key()
            ))),
            Entry::Vacant(entry) => {
                entry.insert(func);
                Ok(())
            }
        }
    }

    pub fn get_func(&self, name: &str) -> Option<&FuncDecl> {
        self.funcs.get(name)
    }

    pub fn get_func_mut(&mut self, name: &str) -> Option<&mut FuncDecl> {
        self.funcs.get_mut(name)
    }

    pub fn remove_func(&mut self, name: &str) -> Option<FuncDecl> {
        self.funcs.remove(name)
    }

    pub fn add_struct(&mut self, name: String, members: Vec<Type>) -> IrResult<()> {
        match self.structs.entry(name) {
            Entry::Occupied(entry) => Err(IrError::new(format!(
                "Struct with name \"{}\" already exists.",
                entry.key()
            ))),
            Entry::Vacant(entry) => {
                entry.insert(members);
                Ok(())
            }
        }
    }

    pub fn get_struct(&self, name: &str) -> Option<&Vec<Type>> {
        self.structs.get(name)
    }

    pub fn remove_struct(&mut self, name: &str) -> Option<Vec<Type>> {
        self.structs.remove(name)
    }

    /// Returns the index of the added data.
    pub fn add_data(&mut self, data: Data) -> DataIdx {
        self.data.push(data);
        DataIdx(self.data.len() - 1)
    }

    pub fn get_data(&self, data_idx: DataIdx) -> Option<&Data> {
        self.data.get(*data_idx)
    }

    /// Returns the index of the added global.
    pub fn add_global_var(&mut self, ty: Type) -> GlobalVarIdx {
        self.global_vars.push(ty);
        GlobalVarIdx(self.global_vars.len() - 1)
    }

    pub fn get_global_var(&self, global_var_idx: GlobalVarIdx) -> Option<&Type> {
        self.global_vars.get(*global_var_idx)
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Module \"{}\"", self.name)?;

        writeln!(f, "Structs:")?;
        let mut struct_names = self.structs.keys().cloned().collect::<Vec<_>>();
        struct_names.sort_by_key(|key| key.to_uppercase());

        for name in struct_names {
            let members = self.structs.get(&name).unwrap();
            let members_string = members
                .iter()
                .map(|member| format!("{:?}", member))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(f, " {} - {{ {} }}", name, &members_string)?;
        }

        writeln!(f, "Globals:")?;
        for (i, global) in self.global_vars.iter().enumerate() {
            writeln!(f, " {}) {:?}", i, global)?;
        }

        writeln!(f, "Data:")?;
        for (i, data) in self.data.iter().enumerate() {
            writeln!(f, " {}) \"{:?}\"", i, data)?;
        }

        writeln!(f)?;

        let mut func_decls = self.funcs.values().collect::<Vec<_>>();
        func_decls.sort_by(|a, b| {
            if a.name == "main" {
                Ordering::Less
            } else if b.name == "main" {
                Ordering::Greater
            } else {
                a.name.cmp(&b.name)
            }
        });

        for func_decl in func_decls {
            let params_string = func_decl
                .params
                .iter()
                .map(|param| format!("{:?}", param))
                .collect::<Vec<_>>()
                .join(", ");
            let ret_string = if matches!(func_decl.ret_type, Type::Void) {
                "".into()
            } else {
                format!(" -> {:?}", func_decl.ret_type)
            };
            writeln!(
                f,
                "Function {:?}{}({}){}:",
                func_decl.visibility, func_decl.name, params_string, ret_string
            )?;

            if !func_decl.locals.is_empty() {
                writeln!(f, " Locals:")?;
                for (i, local) in func_decl.locals.iter().enumerate() {
                    writeln!(f, "  {}) {:?}", i, local)?;
                }
            }

            writeln!(f)?;
            for basic_block in &func_decl.basic_blocks {
                writeln!(f, "{:?}", basic_block)?;
            }
        }

        Ok(())
    }
}
