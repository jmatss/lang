use crate::{block::BlockInfo, decl};
use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    file::{FileId, FileInfo, FilePosition},
    token::{
        block::{BuiltIn, Enum, Function, Struct, Trait},
        expr::Var,
        stmt::Path,
    },
    ty::ty::Ty,
    BlockId,
};
use log::debug;
use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug)]
pub struct AnalyzeContext {
    /// Contains all declarations that have been seen traversing down to this
    /// part of the code. The BlockId represent the outer scope for a item.
    /// For variables it will be the scope in which they are declared in and for
    /// the rest, the BlockId will be the parent block.
    pub(super) variables: HashMap<(String, BlockId), Rc<RefCell<Var>>>,
    pub(super) functions: HashMap<(String, BlockId), Rc<RefCell<Function>>>,
    pub(super) structs: HashMap<(String, BlockId), Rc<RefCell<Struct>>>,
    pub(super) enums: HashMap<(String, BlockId), Rc<RefCell<Enum>>>,
    pub(super) traits: HashMap<(String, BlockId), Rc<RefCell<Trait>>>,

    /// Contains all built-in "functions".
    pub(super) built_ins: HashMap<&'static str, BuiltIn>,

    pub block_info: HashMap<BlockId, BlockInfo>,
    pub use_paths: Vec<Path>,

    /// Mapping file IDs to the corresponding file information. This can be used
    /// to find the filename and directory for file IDs stored in "FilePosition"s.
    pub file_info: HashMap<FileId, FileInfo>,

    /// The file position where the `analyzer` currently is. When the analyzing
    /// is done, this variable will not be used and will be invalid.
    pub file_pos: FilePosition,
}

impl Default for AnalyzeContext {
    fn default() -> Self {
        AnalyzeContext::new(HashMap::default())
    }
}

impl AnalyzeContext {
    pub fn new(file_info: HashMap<FileId, FileInfo>) -> Self {
        Self {
            variables: HashMap::default(),
            functions: HashMap::default(),
            structs: HashMap::default(),
            enums: HashMap::default(),
            traits: HashMap::default(),

            built_ins: decl::built_in::init_built_ins(),

            block_info: HashMap::default(),
            use_paths: Vec::default(),

            file_info,
            file_pos: FilePosition::default(),
        }
    }

    pub fn debug_print(&self) {
        debug!("Block info:\n{:#?}", self.block_info);
    }

    /// Returns the parent block ID for the block with ID `id`.
    pub fn get_parent_id(&self, id: BlockId) -> CustomResult<BlockId> {
        if let Some(block_info) = self.block_info.get(&id) {
            Ok(block_info.parent_id)
        } else {
            Err(self.err(format!(
                "Unable to find block info for block with id: {}",
                id
            )))
        }
    }

    /// Returns the root block ID for the block with ID `id`.
    pub fn get_root_id(&self, id: BlockId) -> CustomResult<BlockId> {
        if let Some(block_info) = self.block_info.get(&id) {
            if block_info.is_root_block {
                Ok(block_info.block_id)
            } else {
                self.get_root_id(block_info.parent_id)
            }
        } else {
            Err(self.err(format!(
                "Unable to find block info for block with id \"{}\" when looking for a root block.",
                id
            )))
        }
    }

    /// Given a name of a declaration `ident` and a block scope `id`, returns
    /// the block in which the sought after declaration was declared.
    fn get_decl_scope<T>(
        &self,
        ident: &str,
        id: BlockId,
        map: &HashMap<(String, BlockId), T>,
    ) -> CustomResult<BlockId>
    where
        T: std::fmt::Debug,
    {
        if map.get(&(ident.into(), id)).is_some() {
            Ok(id)
        } else if id == BlockInfo::DEFAULT_BLOCK_ID {
            Err(self.err(format!(
                "Unable to find decl for \"{}\", ended in up root block.\n map: {:#?}",
                ident, map
            )))
        } else {
            // Unable to find declaration in the current block scope. See
            // recursively if the declaration exists in a parent scope.
            let parent_id = self.get_parent_id(id)?;

            if id != parent_id {
                self.get_decl_scope(ident, parent_id, map)
            } else {
                Err(self.err(format!(
                    "Block with id {} is its own parent in block info.",
                    id
                )))
            }
        }
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// the block in which the sought after variable was declared.
    pub fn get_var_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        self.get_decl_scope(ident, id, &self.variables)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// the block in which the sought after function was declared.
    pub fn get_func_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        self.get_decl_scope(ident, id, &self.functions)
    }

    /// Given a name of a struct `ident` and a block scope `id`, returns
    /// the block in which the sought after struct was declared.
    pub fn get_struct_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        self.get_decl_scope(ident, id, &self.structs)
    }

    /// Given a name of a enum `ident` and a block scope `id`, returns
    /// the block in which the sought after enum was declared.
    pub fn get_enum_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        self.get_decl_scope(ident, id, &self.enums)
    }

    /// Given a name of a interface `ident` and a block scope `id`, returns
    /// the block in which the sought after interface was declared.
    pub fn get_trait_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        self.get_decl_scope(ident, id, &self.traits)
    }

    /// Given a name of a declaration `ident` and the block in which this
    /// declaration was declared, `decl_block_id`, returns a reference to the
    /// declaration in the AST.
    fn get<'a, T>(
        &self,
        ident: &str,
        decl_block_id: BlockId,
        map: &'a HashMap<(String, BlockId), Rc<RefCell<T>>>,
    ) -> CustomResult<Rc<RefCell<T>>> {
        let key = (ident.into(), decl_block_id);

        if let Some(item) = map.get(&key) {
            Ok(Rc::clone(item))
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" in decl block ID {}.",
                ident, decl_block_id
            )))
        }
    }

    /// Given a name of a declaration `ident` and the block in which this
    /// declaration was declared, `decl_block_id`, returns a mutable reference
    /// to the declaration in the AST.
    fn get_mut<'a, T>(
        &self,
        ident: &str,
        decl_block_id: BlockId,
        map: &'a HashMap<(String, BlockId), Rc<RefCell<T>>>,
    ) -> CustomResult<RefMut<'a, T>> {
        let key = (ident.into(), decl_block_id);

        if let Some(item) = map.get(&key) {
            Ok(item.borrow_mut())
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" in decl block ID {}.",
                ident, decl_block_id
            )))
        }
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_var(&self, ident: &str, id: BlockId) -> CustomResult<Rc<RefCell<Var>>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.variables)
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_var_mut(&self, ident: &str, id: BlockId) -> CustomResult<RefMut<Var>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.variables)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_func(&self, ident: &str, id: BlockId) -> CustomResult<Rc<RefCell<Function>>> {
        let decl_block_id = self.get_func_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.functions)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_func_mut(&self, ident: &str, id: BlockId) -> CustomResult<RefMut<Function>> {
        let decl_block_id = self.get_func_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.functions)
    }

    pub fn get_built_in(&self, ident: &str) -> CustomResult<&BuiltIn> {
        self.built_ins.get(ident).ok_or_else(|| {
            self.err(format!(
                "Unable to find built-in function with name \"{}\".",
                ident
            ))
        })
    }

    /// Given a name of a struct `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_struct(&self, ident: &str, id: BlockId) -> CustomResult<Rc<RefCell<Struct>>> {
        let decl_block_id = self.get_struct_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.structs)
    }

    /// Given a name of a struct `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_struct_mut(&self, ident: &str, id: BlockId) -> CustomResult<RefMut<Struct>> {
        let decl_block_id = self.get_struct_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.structs)
    }

    /// Given a name of a enum `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_enum(&self, ident: &str, id: BlockId) -> CustomResult<Rc<RefCell<Enum>>> {
        let decl_block_id = self.get_enum_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.enums)
    }

    /// Given a name of a enum `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_enum_mut(&self, ident: &str, id: BlockId) -> CustomResult<RefMut<Enum>> {
        let decl_block_id = self.get_enum_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.enums)
    }

    /// Given a name of a trait `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_trait(&self, ident: &str, id: BlockId) -> CustomResult<Rc<RefCell<Trait>>> {
        let decl_block_id = self.get_trait_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.traits)
    }

    /// Given a name of a trait `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_trait_mut(&self, ident: &str, id: BlockId) -> CustomResult<RefMut<Trait>> {
        let decl_block_id = self.get_trait_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.traits)
    }

    /// Given a name of a structure `structure_name`, a name of a method `func`
    /// and a block scope `id`, returns a reference to the declaration in the AST.
    pub fn get_method(
        &self,
        structure_name: &str,
        func_name: &str,
        id: BlockId,
    ) -> CustomResult<Rc<RefCell<Function>>> {
        if let Ok(decl_block_id) = self.get_struct_decl_scope(structure_name, id) {
            let struct_ = self.get_struct(structure_name, decl_block_id)?;
            let struct_ = struct_.borrow();

            if let Some(method) = struct_
                .methods
                .as_ref()
                .and_then(|ref map| map.get(func_name))
            {
                Ok(Rc::clone(method))
            } else {
                Err(self.err(format!(
                    "Unable to find method named \"{}\" in struct \"{:#?}\".",
                    &func_name, &struct_,
                )))
            }
        } else if let Ok(decl_block_id) = self.get_enum_decl_scope(structure_name, id) {
            let enum_ = self.get_enum(structure_name, decl_block_id)?;
            let enum_ = enum_.borrow();

            if let Some(method) = enum_
                .methods
                .as_ref()
                .and_then(|ref map| map.get(func_name))
            {
                Ok(Rc::clone(method))
            } else {
                Err(self.err(format!(
                    "Unable to find method named \"{}\" in enum \"{:#?}\".",
                    &func_name, &enum_,
                )))
            }
        } else if self.get_trait_decl_scope(structure_name, id).is_ok() {
            panic!("Not implemented for traits.");
        } else {
            Err(self.err(format!(
                "Unable to find structure with name \"{}\" from block ID {}.",
                structure_name, id
            )))
        }
    }

    /// Given a name of a structure `structure_name` and a block scope `id`, returns
    /// references to all methods declared for the structure in the AST.
    pub fn get_methods(
        &self,
        structure_name: &str,
        id: BlockId,
    ) -> CustomResult<Vec<Rc<RefCell<Function>>>> {
        if let Ok(decl_block_id) = self.get_struct_decl_scope(structure_name, id) {
            let struct_ = self.get_struct(structure_name, decl_block_id)?;
            let mut methods = Vec::default();

            if let Some(inner_methods) = struct_.borrow().methods.as_ref() {
                for method in inner_methods.values() {
                    methods.push(Rc::clone(method));
                }
            }

            Ok(methods)
        } else if let Ok(decl_block_id) = self.get_enum_decl_scope(structure_name, id) {
            let enum_ = self.get_struct(structure_name, decl_block_id)?;
            let mut methods = Vec::default();

            if let Some(inner_methods) = enum_.borrow().methods.as_ref() {
                for method in inner_methods.values() {
                    methods.push(Rc::clone(method));
                }
            }

            Ok(methods)
        } else if self.get_trait_decl_scope(structure_name, id).is_ok() {
            panic!("Not implemented for traits.");
        } else {
            Err(self.err(format!(
                "Unable to find structure with name \"{}\" from block ID {}.",
                structure_name, id
            )))
        }
    }

    /// Given a name of a structure `structure_name` and a block scope `id`, returns
    /// names of all methods declared for the structure in the AST.
    pub fn get_method_names(&self, structure_name: &str, id: BlockId) -> CustomResult<Vec<String>> {
        if let Ok(decl_block_id) = self.get_struct_decl_scope(structure_name, id) {
            let struct_ = self.get_struct(structure_name, decl_block_id)?;
            let struct_ = struct_.borrow();

            Ok(struct_
                .methods
                .as_ref()
                .map(|mets| mets.keys().cloned().collect::<Vec<_>>())
                .unwrap_or_default())
        } else if let Ok(decl_block_id) = self.get_enum_decl_scope(structure_name, id) {
            let enum_ = self.get_enum(structure_name, decl_block_id)?;
            let enum_ = enum_.borrow();

            Ok(enum_
                .methods
                .as_ref()
                .map(|mets| mets.keys().cloned().collect::<Vec<_>>())
                .unwrap_or_default())
        } else if let Ok(decl_block_id) = self.get_trait_decl_scope(structure_name, id) {
            let trait_ = self.get_trait(structure_name, decl_block_id)?;
            let trait_ = trait_.borrow();

            Ok(trait_
                .methods
                .iter()
                .map(|func| func.name.clone())
                .collect::<Vec<_>>())
        } else {
            Err(self.err(format!(
                "Unable to find structure with name \"{}\" from block ID {}.",
                structure_name, id
            )))
        }
    }

    /// Inserts the given method `func` into the structure with name `structure_name`
    /// that can be found from the block id `id`. This structure can be a
    /// struct, enum or interface.
    pub fn insert_method(
        &mut self,
        structure_name: &str,
        func: Rc<RefCell<Function>>,
        id: BlockId,
    ) -> CustomResult<()> {
        if let Ok(decl_block_id) = self.get_struct_decl_scope(structure_name, id) {
            let struct_ = self.get_struct(structure_name, decl_block_id)?;
            let mut struct_ = struct_.borrow_mut();

            let methods = if let Some(methods) = &mut struct_.methods {
                methods
            } else {
                struct_.methods = Some(HashMap::default());
                struct_.methods.as_mut().unwrap()
            };

            let func_name = func.borrow().half_name();
            methods.insert(func_name, Rc::clone(&func));

            Ok(())
        } else if let Ok(decl_block_id) = self.get_enum_decl_scope(structure_name, id) {
            let enum_ = self.get_enum(structure_name, decl_block_id)?;
            let mut enum_ = enum_.borrow_mut();

            let methods = if let Some(methods) = &mut enum_.methods {
                methods
            } else {
                enum_.methods = Some(HashMap::default());
                enum_.methods.as_mut().unwrap()
            };

            let func_name = func.borrow().half_name();
            methods.insert(func_name, Rc::clone(&func));

            Ok(())
        } else if self.get_trait_decl_scope(structure_name, id).is_ok() {
            panic!("Not implemented for traits.");
        } else {
            Err(self.err(format!(
                "Unable to find structure with name \"{}\" from block ID {}.",
                structure_name, id
            )))
        }
    }

    /// Removes the method with name `method_name` from the structure with name
    /// `structure_name` that can be found from the block id `id`. This structure
    /// can be a struct, enum or interface.
    /// Returns true if the method was removed, returns false if no method with
    /// that name exists for the structure. Returns error if the structure can't
    /// be found.
    pub fn remove_method(
        &mut self,
        structure_name: &str,
        method_name: &str,
        id: BlockId,
    ) -> CustomResult<bool> {
        if let Ok(decl_block_id) = self.get_struct_decl_scope(structure_name, id) {
            if let Some(methods) = &mut self
                .get_struct(structure_name, decl_block_id)?
                .borrow_mut()
                .methods
            {
                Ok(methods.remove(method_name).is_some())
            } else {
                Ok(false)
            }
        } else if let Ok(decl_block_id) = self.get_enum_decl_scope(structure_name, id) {
            if let Some(methods) = &mut self
                .get_enum(structure_name, decl_block_id)?
                .borrow_mut()
                .methods
            {
                Ok(methods.remove(method_name).is_some())
            } else {
                Ok(false)
            }
        } else if self.get_trait_decl_scope(structure_name, id).is_ok() {
            panic!("Not implemented for traits.");
        } else {
            Err(self.err(format!(
                "Unable to find structure with name \"{}\" from block ID {}.",
                structure_name, id
            )))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the member with name `member_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_member(
        &self,
        struct_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<Rc<RefCell<Var>>> {
        let struct_ = self.get_struct(struct_name, id)?;
        let struct_ = struct_.borrow();

        if let Some(members) = &struct_.members {
            if let Some(var) = members
                .iter()
                .find(|member| member.borrow().name == member_name)
            {
                Ok(Rc::clone(var))
            } else {
                Err(self.err(format!(
                    "Unable to find member with name \"{}\" in struct \"{}\".",
                    &member_name, &struct_name
                )))
            }
        } else {
            Err(self.err(format!(
                "Struct \"{}\" has no members but tried to access member \"{:?}\".",
                &struct_name, &member_name
            )))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the index of the member with name `member_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_member_index(
        &self,
        struct_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        if let Some(idx) = self
            .get_struct(struct_name, id)?
            .borrow()
            .member_index(member_name)
        {
            Ok(idx as u64)
        } else {
            Err(self.err(format!(
                "Unable to find member with name \"{}\" in struct \"{}\".",
                &member_name, &struct_name
            )))
        }
    }

    /// Finds the enum with the name `enum_name` in a scope containing the block
    /// with ID `id` and returns the member with name `member_name`.
    /// The enum can ex. be declared in parent block scope.
    pub fn get_enum_member(
        &self,
        enum_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<Rc<RefCell<Var>>> {
        let enum_ = self.get_enum(enum_name, id)?;
        let enum_ = enum_.borrow();

        if let Some(members) = &enum_.members {
            if let Some(var) = members
                .iter()
                .find(|member| member.borrow().name == member_name)
            {
                Ok(Rc::clone(var))
            } else {
                Err(self.err(format!(
                    "Unable to find member with name \"{}\" in enum \"{}\".",
                    &member_name, &enum_name
                )))
            }
        } else {
            Err(self.err(format!(
                "Enum \"{}\" has no members but tried to access member \"{:?}\".",
                &enum_name, &member_name
            )))
        }
    }

    /// Finds the enum with the name `enum_name` in a scope containing the block
    /// with ID `id` and returns the index of the member with name `member_name`.
    pub fn get_enum_member_index(
        &self,
        enum_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        if let Some(idx) = self
            .get_enum(enum_name, id)?
            .borrow()
            .member_index(member_name)
        {
            Ok(idx as u64)
        } else {
            Err(self.err(format!(
                "Unable to find member with name \"{}\" in enum \"{}\".",
                &member_name, &enum_name
            )))
        }
    }

    /// Given a function or method `func`, finds the parameter with the name
    // `param_name` and also its index.
    fn get_param(
        &self,
        func: Rc<RefCell<Function>>,
        param_name: &str,
    ) -> CustomResult<(usize, Var)> {
        if let Some(params) = &func.borrow().parameters {
            for (idx, param) in params.iter().enumerate() {
                if param_name == param.borrow().name {
                    return Ok((idx, param.borrow().clone()));
                }
            }

            Err(self.err(format!(
                "Unable to find param with name \"{}\" in function with name \"{}\".",
                &param_name,
                &func.borrow().name,
            )))
        } else {
            Err(self.err(format!(
                "Function \"{}\" had no parameters, expected param with name: {}",
                &func.borrow().name,
                &param_name
            )))
        }
    }

    /// Given a function or method `func`, finds the parameter with the name
    /// `param_name` and also its index.
    fn get_param_with_idx(&self, func: Rc<RefCell<Function>>, idx: usize) -> CustomResult<Var> {
        if let Some(params) = &func.borrow().parameters {
            for (i, param) in params.iter().enumerate() {
                if idx == i {
                    return Ok(param.borrow().clone());
                }
            }

            Err(self.err(format!(
                "Unable to find param with index \"{}\" in function with name \"{}\".",
                idx,
                &func.borrow().name,
            )))
        } else {
            Err(self.err(format!(
                "Function \"{}\" had no parameters, expected param with index: {}",
                &func.borrow().name,
                idx
            )))
        }
    }

    /// Given a function or method `func`, finds the index of the parameter with
    /// the name `param_name`. The index indicates the position of the parameter
    /// in the struct parameter list.
    fn get_param_idx(&self, func: Rc<RefCell<Function>>, param_name: &str) -> CustomResult<usize> {
        Ok(self.get_param(func, param_name)?.0)
    }

    /// Finds the structure with the name `structure_name` in a scope containing
    /// the block with ID `id` and returns the index of the parameter with name
    /// `param_name` in the method with name `method_name`.
    /// The structure_name can ex. be declared in parent block scope.
    pub fn get_method_param_idx(
        &self,
        structure_name: &str,
        method_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> CustomResult<usize> {
        let method = self.get_method(structure_name, method_name, id)?;
        self.get_param_idx(method, param_name)
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`.
    pub fn get_func_param_idx(
        &self,
        func_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> CustomResult<usize> {
        let func = self.get_func(func_name, id)?;
        self.get_param_idx(func, param_name)
    }

    /// Given a function or method `func`, finds the type of the parameter with
    /// the name `param_name`.
    fn get_param_type(&self, func: Rc<RefCell<Function>>, idx: usize) -> CustomResult<Ty> {
        if let Some(ty) = &self.get_param_with_idx(Rc::clone(&func), idx)?.ty {
            Ok(ty.clone())
        } else {
            Err(self.err(format!(
                "Parameter at index \"{}\" in function \"{}\" has no type set.",
                idx,
                func.borrow().name
            )))
        }
    }

    /// Finds the structure with the name `structure_name` in a scope containing
    /// the block with ID `id` and returns the type of the parameter with name
    /// `param_name` in the method with name `method_name`.
    /// The structure_name can ex. be declared in parent block scope.
    pub fn get_method_param_type(
        &self,
        structure_name: &str,
        method_name: &str,
        idx: usize,
        id: BlockId,
    ) -> CustomResult<Ty> {
        let method = self.get_method(structure_name, method_name, id)?;
        self.get_param_type(method, idx)
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the type of the parameter with name `param_name`.
    pub fn get_func_param_type(
        &self,
        func_name: &str,
        idx: usize,
        id: BlockId,
    ) -> CustomResult<Ty> {
        let func = self.get_func(func_name, id)?;
        self.get_param_type(func, idx)
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new(msg, AnalyzeError, Some(self.file_pos))
    }
}
