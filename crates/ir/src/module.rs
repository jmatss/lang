use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
};

use crate::{
    error::{IrError, IrResult},
    func::FuncDecl,
    ty::Type,
    Data, DataIdx, GlobalVarIdx, Lit,
};

pub struct Module {
    pub name: String,

    /// Size of a pointer in this IR module.
    pub ptr_size: usize,

    /// The string is the name of the function and the `FuncDecl` is the
    /// declaration of the function.
    pub funcs: HashMap<String, FuncDecl>,

    /// The first string is the name of the struct and the vector of `Type`s
    /// are the members of the struct (in order). If this is a externally
    /// declared struct, it will have no members (set to None).
    ///
    /// At this point enums and tuples have been compiled into structs, so they
    /// are stored in here as well.
    pub structs: HashMap<String, Option<Vec<Type>>>,

    /// Order in which the `structs` references each other and should therefore
    /// be compiled in. Ex. a struct `A` that has a member of type `B` should
    /// be compiled after `B`.
    ///
    /// This should be filled/set after all structs have been inserted into the
    /// `structs` map.
    pub structs_order: Vec<String>,

    /// The `Type` is the type of the global and the `Lit` is the init value of
    /// the global variable. If set to None, the global will be initialized with
    /// the value zero/null.
    pub global_vars: Vec<(Type, Option<Lit>)>,

    /// Contains static data that can be used to initialize values. This will
    /// mostly be string literals.
    pub data: Vec<Data>,
}

impl Module {
    pub fn new(name: String, ptr_size: usize) -> Self {
        Self {
            name,
            ptr_size,
            funcs: HashMap::default(),
            structs: HashMap::default(),
            structs_order: Vec::default(),
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

    pub fn add_struct(&mut self, name: String, members: Option<Vec<Type>>) -> IrResult<()> {
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

    pub fn get_struct(&self, name: &str) -> IrResult<Option<&Vec<Type>>> {
        if let Some(member_types) = self.structs.get(name) {
            Ok(member_types.as_ref())
        } else {
            Err(IrError::new(format!(
                "Unable to find struct with name \"{}\".",
                name
            )))
        }
    }

    pub fn remove_struct(&mut self, name: &str) {
        self.structs.remove(name);
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
    pub fn add_global_var(&mut self, ty: Type, lit_opt: Option<Lit>) -> GlobalVarIdx {
        self.global_vars.push((ty, lit_opt));
        GlobalVarIdx(self.global_vars.len() - 1)
    }

    pub fn add_structs_order(&mut self, order: &[String]) {
        self.structs_order.extend_from_slice(order);
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
            writeln!(f, " {}) {:?}", i, data)?;
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
