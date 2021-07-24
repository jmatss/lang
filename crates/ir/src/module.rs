use std::{
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
};

use crate::{
    decl::{func::Func, ty::Type},
    error::{IrError, IrResult},
    Data, DataIdx, GlobalVarIdx,
};

#[derive(Debug)]
pub struct Module {
    name: String,

    funcs: HashMap<String, Func>,

    /// The first string is the name of the ADT and the vector are the types of
    /// the members. At this point the members will accessed with indices, so
    /// there is no need to store their names.
    structs: HashMap<String, Vec<Type>>,
    unions: HashMap<String, Vec<Type>>,
    /// The value of the map is the amount of enum members.
    enums: HashMap<String, usize>,

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
            unions: HashMap::default(),
            enums: HashMap::default(),
            global_vars: Vec::default(),
            data: Vec::default(),
        }
    }

    pub fn add_function(&mut self, name: String, func: Func) -> IrResult<()> {
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

    pub fn get_function(&self, name: &str) -> Option<&Func> {
        self.funcs.get(name)
    }

    pub fn get_function_mut(&mut self, name: &str) -> Option<&mut Func> {
        self.funcs.get_mut(name)
    }

    pub fn remove_function(&mut self, name: &str) -> Option<Func> {
        self.funcs.remove(name)
    }

    /// Can be used to add any kind of ADT (struct/enum/union).
    pub fn add_adt(&mut self, adt_type: Type) -> IrResult<()> {
        match adt_type {
            Type::Struct(name, members) => self.add_struct(name, members),
            Type::Enum(name, member_count) => self.add_enum(name, member_count),
            Type::Union(name, members) => self.add_union(name, members),
            _ => Err(IrError::new(format!(
                "Invalid type when adding ADT: {:#?}",
                adt_type
            ))),
        }
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

    pub fn add_enum(&mut self, name: String, member_count: usize) -> IrResult<()> {
        match self.enums.entry(name) {
            Entry::Occupied(entry) => Err(IrError::new(format!(
                "Enum with name \"{}\" already exists.",
                entry.key()
            ))),
            Entry::Vacant(entry) => {
                entry.insert(member_count);
                Ok(())
            }
        }
    }

    pub fn get_enum(&self, name: &str) -> Option<&usize> {
        self.enums.get(name)
    }

    pub fn remove_enum(&mut self, name: &str) -> Option<usize> {
        self.enums.remove(name)
    }

    pub fn add_union(&mut self, name: String, members: Vec<Type>) -> IrResult<()> {
        match self.unions.entry(name) {
            Entry::Occupied(entry) => Err(IrError::new(format!(
                "Union with name \"{}\" already exists.",
                entry.key()
            ))),
            Entry::Vacant(entry) => {
                entry.insert(members);
                Ok(())
            }
        }
    }

    pub fn get_union(&self, name: &str) -> Option<&Vec<Type>> {
        self.unions.get(name)
    }

    pub fn remove_union(&mut self, name: &str) -> Option<Vec<Type>> {
        self.unions.remove(name)
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
