use crate::{block::BlockInfo, decl};
use common::{
    error::{LangError, LangErrorKind::AnalyzeError, LangResult},
    file::{FileId, FileInfo, FilePosition},
    token::{
        block::{Adt, AdtKind, BuiltIn, Function, Trait},
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
    /// A `Adt` represents either a struct, union or enum.
    pub(super) variables: HashMap<(String, BlockId), Rc<RefCell<Var>>>,
    pub(super) functions: HashMap<(String, BlockId), Rc<RefCell<Function>>>,
    pub(super) adts: HashMap<(String, BlockId), Rc<RefCell<Adt>>>,
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
            adts: HashMap::default(),
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
    pub fn get_parent_id(&self, id: BlockId) -> LangResult<BlockId> {
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
    pub fn get_root_id(&self, id: BlockId) -> LangResult<BlockId> {
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
    ) -> LangResult<BlockId>
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
    pub fn get_var_decl_scope(&self, ident: &str, id: BlockId) -> LangResult<BlockId> {
        self.get_decl_scope(ident, id, &self.variables)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// the block in which the sought after function was declared.
    pub fn get_func_decl_scope(&self, ident: &str, id: BlockId) -> LangResult<BlockId> {
        self.get_decl_scope(ident, id, &self.functions)
    }

    /// Given a name of a ADT `ident` and a block scope `id`, returns the block
    // in which the sought after ADT was declared.
    pub fn get_adt_decl_scope(&self, ident: &str, id: BlockId) -> LangResult<BlockId> {
        self.get_decl_scope(ident, id, &self.adts)
    }

    /// Given a name of a interface `ident` and a block scope `id`, returns
    /// the block in which the sought after interface was declared.
    pub fn get_trait_decl_scope(&self, ident: &str, id: BlockId) -> LangResult<BlockId> {
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
    ) -> LangResult<Rc<RefCell<T>>> {
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
    ) -> LangResult<RefMut<'a, T>> {
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

    /// Checks if there exists a struct with name `ident` in the scope of `id`.
    pub fn is_struct(&self, ident: &str, id: BlockId) -> bool {
        if let Ok(adt) = self.get_adt(ident, id) {
            matches!(adt.borrow().kind, AdtKind::Struct)
        } else {
            false
        }
    }

    /// Checks if there exists a union with name `ident` in the scope of `id`.
    pub fn is_union(&self, ident: &str, id: BlockId) -> bool {
        if let Ok(adt) = self.get_adt(ident, id) {
            matches!(adt.borrow().kind, AdtKind::Union)
        } else {
            false
        }
    }

    /// Checks if there exists a enum with name `ident` in the scope of `id`.
    pub fn is_enum(&self, ident: &str, id: BlockId) -> bool {
        if let Ok(adt) = self.get_adt(ident, id) {
            matches!(adt.borrow().kind, AdtKind::Enum)
        } else {
            false
        }
    }

    /// Checks if there exists a union with name `ident` in the scope of `id`.
    pub fn is_trait(&self, ident: &str, id: BlockId) -> bool {
        self.get_trait(ident, id).is_ok()
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_var(&self, ident: &str, id: BlockId) -> LangResult<Rc<RefCell<Var>>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.variables)
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_var_mut(&self, ident: &str, id: BlockId) -> LangResult<RefMut<Var>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.variables)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_func(&self, ident: &str, id: BlockId) -> LangResult<Rc<RefCell<Function>>> {
        let decl_block_id = self.get_func_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.functions)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_func_mut(&self, ident: &str, id: BlockId) -> LangResult<RefMut<Function>> {
        let decl_block_id = self.get_func_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.functions)
    }

    pub fn get_built_in(&self, ident: &str) -> LangResult<&BuiltIn> {
        self.built_ins.get(ident).ok_or_else(|| {
            self.err(format!(
                "Unable to find built-in function with name \"{}\".",
                ident
            ))
        })
    }

    /// Given a name of a ADT `ident` and a block scope `id`, returns a reference
    /// to the declaration in the AST.
    pub fn get_adt(&self, ident: &str, id: BlockId) -> LangResult<Rc<RefCell<Adt>>> {
        let decl_block_id = self.get_adt_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.adts)
    }

    /// Given a name of a ADT `ident` and a block scope `id`, returns a mutable
    /// reference to the declaration in the AST.
    pub fn get_adt_mut(&self, ident: &str, id: BlockId) -> LangResult<RefMut<Adt>> {
        let decl_block_id = self.get_adt_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.adts)
    }

    /// Given a name of a trait `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_trait(&self, ident: &str, id: BlockId) -> LangResult<Rc<RefCell<Trait>>> {
        let decl_block_id = self.get_trait_decl_scope(ident, id)?;
        self.get(ident, decl_block_id, &self.traits)
    }

    /// Given a name of a trait `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_trait_mut(&self, ident: &str, id: BlockId) -> LangResult<RefMut<Trait>> {
        let decl_block_id = self.get_trait_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.traits)
    }

    /// Given a name of a ADT `adt_name`, a name of a method `method_name` and a
    /// block scope `id`, returns a reference to the declaration in the AST.
    pub fn get_adt_method(
        &self,
        adt_name: &str,
        method_name: &str,
        id: BlockId,
    ) -> LangResult<Rc<RefCell<Function>>> {
        let adt = self.get_adt(adt_name, id)?;
        let adt = adt.borrow();

        if let Some(method) = adt.methods.get(method_name) {
            Ok(Rc::clone(method))
        } else {
            Err(self.err(format!(
                "Unable to find method named \"{}\" in ADT \"{:#?}\".",
                &method_name, &adt,
            )))
        }
    }

    /// Given a name of a trait `trait_name` and a block scope `id`, returns names
    /// of all methods declared for the trait.
    pub fn get_trait_method_names(&self, trait_name: &str, id: BlockId) -> LangResult<Vec<String>> {
        let trait_ = self.get_trait(trait_name, id)?;
        let trait_ = trait_.borrow();

        Ok(trait_
            .methods
            .iter()
            .map(|func| func.name.clone())
            .collect())
    }

    /// Inserts the given method `method` into the ADT with name `adt_name` that can
    /// be found from the block id `id`. This ADT can be a struct, enum or union.
    pub fn insert_method(
        &mut self,
        adt_name: &str,
        method: Rc<RefCell<Function>>,
        id: BlockId,
    ) -> LangResult<()> {
        let method_name = method.borrow().half_name();

        let adt = self.get_adt(adt_name, id)?;
        let mut adt = adt.borrow_mut();

        adt.methods.insert(method_name, Rc::clone(&method));

        Ok(())
    }

    /// Removes the method with name `method_name` from the ADT with name `adt_name`
    /// that can be found from the block id `id`.
    /// Returns true if the method was removed, returns false if no method with
    /// that name exists for the ADT. Returns error if the ADT can't be found.
    pub fn remove_method(
        &mut self,
        adt_name: &str,
        method_name: &str,
        id: BlockId,
    ) -> LangResult<bool> {
        let adt = self.get_adt(adt_name, id)?;
        let mut adt = adt.borrow_mut();

        Ok(adt.methods.remove(method_name).is_some())
    }

    /// Finds the ADT with the name `adt_name` in a scope containing the block
    /// with ID `id` and returns the member with name `member_name`.
    pub fn get_adt_member(
        &self,
        adt_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> LangResult<Rc<RefCell<Var>>> {
        let adt = self.get_adt(adt_name, id)?;
        let adt = adt.borrow();

        if let Some(member) = adt
            .members
            .iter()
            .find(|member| member.borrow().name == member_name)
        {
            Ok(Rc::clone(member))
        } else {
            Err(self.err(format!(
                "Unable to find member with name \"{}\" in ADT \"{}\".",
                &member_name, &adt_name
            )))
        }
    }

    /// Finds the ADT with the name `adt_name` in a scope containing the block
    /// with ID `id` and returns the index of the member with name `member_name`.
    pub fn get_adt_member_index(
        &self,
        adt_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> LangResult<u64> {
        if let Some(idx) = self
            .get_adt(adt_name, id)?
            .borrow()
            .member_index(member_name)
        {
            Ok(idx as u64)
        } else {
            Err(self.err(format!(
                "Unable to find member with name \"{}\" in ADT \"{}\".",
                &member_name, &adt_name
            )))
        }
    }

    /// Given a function or method `func`, finds the parameter with the name
    // `param_name` and also its index.
    fn get_param(&self, func: Rc<RefCell<Function>>, param_name: &str) -> LangResult<(usize, Var)> {
        let func = func.borrow();

        let params = if let Some(params) = &func.parameters {
            params
        } else {
            return Err(self.err(format!(
                "Function \"{}\" had no parameters, expected param with name: {}",
                &func.name, &param_name
            )));
        };

        for (idx, param) in params.iter().enumerate() {
            if param_name == param.borrow().name {
                return Ok((idx, param.borrow().clone()));
            }
        }

        Err(self.err(format!(
            "Unable to find param with name \"{}\" in function with name \"{}\".",
            &param_name, &func.name,
        )))
    }

    /// Given a function or method `func`, finds the parameter with the name
    /// `param_name` and also its index.
    fn get_param_with_idx(&self, func: Rc<RefCell<Function>>, idx: usize) -> LangResult<Var> {
        let func = func.borrow();

        let params = if let Some(params) = &func.parameters {
            params
        } else {
            return Err(self.err(format!(
                "Function \"{}\" had no parameters, expected param with index: {}",
                &func.name, idx
            )));
        };

        for (i, param) in params.iter().enumerate() {
            if idx == i {
                return Ok(param.borrow().clone());
            }
        }

        Err(self.err(format!(
            "Unable to find param with index \"{}\" in function with name \"{}\".",
            idx, &func.name,
        )))
    }

    /// Given a function or method `func`, finds the index of the parameter with
    /// the name `param_name`. The index indicates the position of the parameter
    /// in the struct parameter list.
    fn get_param_idx(&self, func: Rc<RefCell<Function>>, param_name: &str) -> LangResult<usize> {
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
    ) -> LangResult<usize> {
        let method = self.get_adt_method(structure_name, method_name, id)?;
        self.get_param_idx(method, param_name)
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`.
    pub fn get_func_param_idx(
        &self,
        func_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> LangResult<usize> {
        let func = self.get_func(func_name, id)?;
        self.get_param_idx(func, param_name)
    }

    /// Given a function or method `func`, finds the type of the parameter with
    /// the name `param_name`.
    fn get_param_type(&self, func: Rc<RefCell<Function>>, idx: usize) -> LangResult<Ty> {
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
    ) -> LangResult<Ty> {
        let method = self.get_adt_method(structure_name, method_name, id)?;
        self.get_param_type(method, idx)
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the type of the parameter with name `param_name`.
    pub fn get_func_param_type(&self, func_name: &str, idx: usize, id: BlockId) -> LangResult<Ty> {
        let func = self.get_func(func_name, id)?;
        self.get_param_type(func, idx)
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new(msg, AnalyzeError, Some(self.file_pos))
    }
}
