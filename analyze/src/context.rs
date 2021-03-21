use crate::{block::BlockInfo, decl};
use common::{
    error::{
        LangError,
        LangErrorKind::{self, AnalyzeError},
        LangResult,
    },
    file::{FileId, FileInfo, FilePosition},
    path::LangPath,
    token::{
        block::{Adt, AdtKind, BuiltIn, Fn, Trait},
        expr::Var,
    },
    ty::environment::TypeEnvironment,
    BlockId, TypeId,
};
use log::{debug, log_enabled, Level};
use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

#[derive(Debug)]
pub struct AnalyzeContext {
    /// A environment containing information about all types in this code.
    pub ty_env: TypeEnvironment,

    /// Contains all declarations that have been seen traversing down to this
    /// part of the code. The BlockId represent the outer scope for a item.
    /// For variables it will be the scope in which they are declared in and for
    /// the rest, the BlockId will be the parent block.
    /// A `Adt` represents either a struct, union or enum.
    pub(super) variables: HashMap<(String, BlockId), Rc<RefCell<Var>>>,
    pub(super) fns: HashMap<(LangPath, BlockId), Rc<RefCell<Fn>>>,
    pub(super) adts: HashMap<(LangPath, BlockId), Rc<RefCell<Adt>>>,
    pub(super) traits: HashMap<(LangPath, BlockId), Rc<RefCell<Trait>>>,

    /// Contains all built-in "fns".
    pub(super) built_ins: HashMap<&'static str, BuiltIn>,

    pub block_info: HashMap<BlockId, BlockInfo>,

    /// Mapping file IDs to the corresponding file information. This can be used
    /// to find the filename and directory for file IDs stored in "FilePosition"s.
    pub file_info: HashMap<FileId, FileInfo>,

    /// The file position where the `analyzer` currently is. When the analyzing
    /// is done, this variable will not be used and will be invalid.
    pub file_pos: FilePosition,
}

impl Default for AnalyzeContext {
    fn default() -> Self {
        Self::new(TypeEnvironment::default(), HashMap::default()).unwrap()
    }
}

impl AnalyzeContext {
    pub fn new(
        mut ty_env: TypeEnvironment,
        file_info: HashMap<FileId, FileInfo>,
    ) -> LangResult<Self> {
        let built_ins = decl::built_in::init_built_ins(&mut ty_env)?;
        Ok(Self {
            ty_env,

            variables: HashMap::default(),
            fns: HashMap::default(),
            adts: HashMap::default(),
            traits: HashMap::default(),

            built_ins,
            block_info: HashMap::default(),

            file_info,
            file_pos: FilePosition::default(),
        })
    }

    pub fn debug_print(&self) {
        debug!("Block Info:\n{:#?}", self.block_info);
        debug!("Variables:\n{:#?}", self.variables);
        debug!("Functions:\n{:#?}", self.fns);
        debug!("ADTs:\n{:#?}", self.adts);
        debug!("Traits:\n{:#?}", self.traits);
        debug!("Built-ins:\n{:#?}", self.built_ins);
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

    /// Given a block ID `id`, returns the module for the block.
    pub fn get_module(&self, id: BlockId) -> LangResult<Option<LangPath>> {
        if let Some(block_info) = self.block_info.get(&id) {
            Ok(block_info.module.clone())
        } else {
            Err(self.err(format!(
                "Unable to find block info for block with id \"{}\" when looking for module.",
                id
            )))
        }
    }

    /// Given a block ID `id`, returns the "use" statements for the block.
    pub fn get_uses(&self, id: BlockId) -> LangResult<&HashSet<LangPath>> {
        if let Some(block_info) = self.block_info.get(&id) {
            Ok(&block_info.uses)
        } else {
            Err(self.err(format!(
                "Unable to find block info for block with id \"{}\" when looking for uses.",
                id
            )))
        }
    }

    /// Given a name of a path `path` and a block scope `id`; tries to find a
    /// valid path that corresponds to a key in the given `map`.
    ///
    /// A potential path is found/resolved in this order:
    ///  1. The `path` itself.
    ///  2. Prepending the `path` with the "module" for the block with ID `id`.
    ///     This corresponds to accessing a item declared in the same module.
    ///  3. Prepending the `path` with "use" statements declared for the block
    ///     with ID `id`.
    fn calculate_full_path<T>(
        &self,
        path: &LangPath,
        id: BlockId,
        map: &HashMap<(LangPath, BlockId), T>,
    ) -> LangResult<LangPath>
    where
        T: std::fmt::Debug,
    {
        let mut err = match self.get_decl_scope(path, id, map) {
            Ok(_) => return Ok(path.clone()),
            Err(err) => err,
        };

        if let Some(module) = self.get_module(id)? {
            let module_path = module.join(path, module.file_pos().cloned());
            if self.get_decl_scope(&module_path, id, map).is_ok() {
                return Ok(module_path);
            }
        }

        let uses = self.get_uses(id)?;
        for use_path in uses {
            if use_path.last().unwrap().name() == path.first().unwrap().name() {
                let mut potential_use_path = use_path.clone();
                potential_use_path.pop();

                let combined_path = potential_use_path.join(path, path.file_pos().cloned());
                match self.get_decl_scope(&combined_path, id, map) {
                    Ok(_) => return Ok(combined_path),
                    Err(_) => continue,
                }
            }
        }

        if log_enabled!(Level::Debug) {
            err.msg
                .push_str(&format!("\nTried with the uses: {:#?}", uses));
        }
        Err(err)
    }

    pub fn calculate_fn_full_path(&self, path: &LangPath, id: BlockId) -> LangResult<LangPath> {
        self.calculate_full_path(path, id, &self.fns)
    }

    pub fn calculate_adt_full_path(&self, path: &LangPath, id: BlockId) -> LangResult<LangPath> {
        self.calculate_full_path(path, id, &self.adts)
    }

    pub fn calculate_trait_full_path(&self, path: &LangPath, id: BlockId) -> LangResult<LangPath> {
        self.calculate_full_path(path, id, &self.traits)
    }

    /// Given a name of a declaration `path` and a block scope `id`, returns
    /// the block in which the sought after declaration was declared.
    fn get_decl_scope<K, T>(
        &self,
        path: &K,
        id: BlockId,
        map: &HashMap<(K, BlockId), T>,
    ) -> LangResult<BlockId>
    where
        K: Clone + Debug + Display + Eq + Hash,
        T: std::fmt::Debug,
    {
        if map.get(&(path.clone(), id)).is_some() {
            Ok(id)
        } else if id == BlockInfo::DEFAULT_BLOCK_ID {
            let mut err_msg = format!("Unable to find decl for \"{}\".", path);
            if log_enabled!(Level::Debug) {
                err_msg.push_str(&format!("\nmap: {:#?}", map))
            }
            Err(self.err(err_msg))
        } else {
            // Unable to find declaration in the current block scope. See
            // recursively if the declaration exists in a parent scope.
            let parent_id = self.get_parent_id(id)?;

            if id != parent_id {
                self.get_decl_scope(path, parent_id, map)
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
        self.get_decl_scope(&ident.to_string(), id, &self.variables)
    }

    /// Given a name of a function `path` and a block scope `id`, returns
    /// the block in which the sought after function was declared.
    pub fn get_fn_decl_scope(&self, path: &LangPath, id: BlockId) -> LangResult<BlockId> {
        self.get_decl_scope(path, id, &self.fns)
    }

    /// Given a name of a ADT `path` and a block scope `id`, returns the block
    // in which the sought after ADT was declared.
    pub fn get_adt_decl_scope(&self, path: &LangPath, id: BlockId) -> LangResult<BlockId> {
        self.get_decl_scope(path, id, &self.adts)
    }

    /// Given a name of a interface `path` and a block scope `id`, returns
    /// the block in which the sought after interface was declared.
    pub fn get_trait_decl_scope(&self, path: &LangPath, id: BlockId) -> LangResult<BlockId> {
        self.get_decl_scope(path, id, &self.traits)
    }

    /// Given a name of a declaration `name` and the block in which this
    /// declaration was declared, `decl_block_id`, returns a reference to the
    /// declaration in the AST.
    fn get<'a, K, T>(
        &self,
        name: &K,
        decl_block_id: BlockId,
        map: &'a HashMap<(K, BlockId), Rc<RefCell<T>>>,
    ) -> LangResult<Rc<RefCell<T>>>
    where
        K: Clone + Debug + Display + Eq + Hash,
    {
        let key = (name.clone(), decl_block_id);

        if let Some(item) = map.get(&key) {
            Ok(Rc::clone(item))
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" in decl block ID {}.",
                name, decl_block_id
            )))
        }
    }

    /// Given a name of a declaration `name` and the block in which this
    /// declaration was declared, `decl_block_id`, returns a mutable reference
    /// to the declaration in the AST.
    fn get_mut<'a, K, T>(
        &self,
        name: &K,
        decl_block_id: BlockId,
        map: &'a HashMap<(K, BlockId), Rc<RefCell<T>>>,
    ) -> LangResult<RefMut<'a, T>>
    where
        K: Clone + Debug + Display + Eq + Hash,
    {
        let key = (name.clone(), decl_block_id);

        if let Some(item) = map.get(&key) {
            Ok(item.borrow_mut())
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" in decl block ID {}.",
                name, decl_block_id
            )))
        }
    }

    /// Checks if there exists a struct with name `path` in the scope of `id`.
    pub fn is_struct(&self, path: &LangPath, id: BlockId) -> bool {
        if let Ok(adt) = self.get_adt(path, id) {
            matches!(adt.borrow().kind, AdtKind::Struct)
        } else {
            false
        }
    }

    /// Checks if there exists a union with name `path` in the scope of `id`.
    pub fn is_union(&self, path: &LangPath, id: BlockId) -> bool {
        if let Ok(adt) = self.get_adt(path, id) {
            matches!(adt.borrow().kind, AdtKind::Union)
        } else {
            false
        }
    }

    /// Checks if there exists a enum with name `path` in the scope of `id`.
    pub fn is_enum(&self, path: &LangPath, id: BlockId) -> bool {
        if let Ok(adt) = self.get_adt(path, id) {
            matches!(adt.borrow().kind, AdtKind::Enum)
        } else {
            false
        }
    }

    /// Checks if there exists a union with name `path` in the scope of `id`.
    pub fn is_trait(&self, path: &LangPath, id: BlockId) -> bool {
        self.get_trait(path, id).is_ok()
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_var(&self, ident: &str, id: BlockId) -> LangResult<Rc<RefCell<Var>>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get(&ident.to_string(), decl_block_id, &self.variables)
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_var_mut(&self, ident: &str, id: BlockId) -> LangResult<RefMut<Var>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get_mut(&ident.to_string(), decl_block_id, &self.variables)
    }

    /// Given a name of a function `path` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_fn(&self, path: &LangPath, id: BlockId) -> LangResult<Rc<RefCell<Fn>>> {
        let decl_block_id = self.get_fn_decl_scope(path, id)?;
        self.get(path, decl_block_id, &self.fns)
    }

    pub fn get_built_in(&self, ident: &str) -> LangResult<&BuiltIn> {
        self.built_ins.get(ident).ok_or_else(|| {
            self.err(format!(
                "Unable to find built-in function with name \"{}\".",
                ident
            ))
        })
    }

    /// Given a name of a ADT `ident` and a block scope `id`; returns a reference
    /// to the declaration in the AST.
    pub fn get_adt(&self, path: &LangPath, id: BlockId) -> LangResult<Rc<RefCell<Adt>>> {
        let decl_block_id = self.get_adt_decl_scope(path, id)?;
        self.get(path, decl_block_id, &self.adts)
    }

    /// Given a partial path of a ADT `path` and a block scope `id`; tries to find
    /// the declaration of a ADT. If unable to find a ADT with path `path`, this
    /// function will look at the "use" statements for the current block to see
    /// if able to resolve the ADT.
    pub fn get_adt_partial(&self, path: &LangPath, id: BlockId) -> LangResult<Rc<RefCell<Adt>>> {
        let real_path = self.calculate_full_path(path, id, &self.adts)?;
        self.get_adt(&real_path, id)
    }

    /// Given a name of a trait `path` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_trait(&self, path: &LangPath, id: BlockId) -> LangResult<Rc<RefCell<Trait>>> {
        let decl_block_id = self.get_trait_decl_scope(path, id)?;
        self.get(path, decl_block_id, &self.traits)
    }

    /// Given a partial path of a trait `path` and a block scope `id`; tries to find
    /// the declaration of a trait. If unable to find a trait with path `path`, this
    /// function will look at the "use" statements for the current block to see
    /// if able to resolve the trait.
    pub fn get_trait_partial(
        &self,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<Rc<RefCell<Trait>>> {
        let real_path = self.calculate_full_path(path, id, &self.adts)?;
        self.get_trait(&real_path, id)
    }

    /// Given a name of a ADT `adt_name`, a name of a method `method_name` and a
    /// block scope `id`, returns a reference to the declaration in the AST.
    pub fn get_method(
        &self,
        adt_name: &LangPath,
        method_name: &str,
        id: BlockId,
    ) -> LangResult<Rc<RefCell<Fn>>> {
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
    pub fn get_trait_method_names(
        &self,
        trait_name: &LangPath,
        id: BlockId,
    ) -> LangResult<Vec<String>> {
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
        adt_name: &LangPath,
        method: Rc<RefCell<Fn>>,
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
        adt_name: &LangPath,
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
        adt_name: &LangPath,
        member_name: &str,
        id: BlockId,
        file_pos: Option<FilePosition>,
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
            Err(LangError::new(
                format!(
                    "Unable to find member with name \"{}\" in ADT \"{}\".",
                    &member_name, &adt_name
                ),
                LangErrorKind::GeneralError,
                file_pos,
            ))
        }
    }

    /// Finds the ADT with the name `adt_name` in a scope containing the block
    /// with ID `id` and returns the index of the member with name `member_name`.
    pub fn get_adt_member_index(
        &self,
        adt_name: &LangPath,
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
    fn get_param(&self, func: Rc<RefCell<Fn>>, param_name: &str) -> LangResult<(usize, Var)> {
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
    fn get_param_with_idx(&self, func: Rc<RefCell<Fn>>, idx: usize) -> LangResult<Var> {
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
    fn get_param_idx(&self, func: Rc<RefCell<Fn>>, param_name: &str) -> LangResult<usize> {
        Ok(self.get_param(func, param_name)?.0)
    }

    /// Finds the ADT with the name `adt_path` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name
    /// `param_name` in the method with name `method_name`.
    pub fn get_method_param_idx(
        &self,
        adt_path: &LangPath,
        method_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> LangResult<usize> {
        let method = self.get_method(adt_path, method_name, id)?;
        self.get_param_idx(method, param_name)
    }

    /// Finds the function with the path `fn_path` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`.
    pub fn get_fn_param_idx(
        &self,
        fn_path: &LangPath,
        param_name: &str,
        id: BlockId,
    ) -> LangResult<usize> {
        let func = self.get_fn(fn_path, id)?;
        self.get_param_idx(func, param_name)
    }

    /// Given a function or method `func`, finds the type of the parameter with
    /// the name `param_name`.
    fn get_param_type(&self, func: Rc<RefCell<Fn>>, idx: usize) -> LangResult<TypeId> {
        if let Some(type_id) = &self.get_param_with_idx(Rc::clone(&func), idx)?.ty {
            Ok(*type_id)
        } else {
            Err(self.err(format!(
                "Parameter at index \"{}\" in function \"{}\" has no type set.",
                idx,
                func.borrow().name
            )))
        }
    }

    /// Finds the ADT with the name `adt_path` in a scope containing the block
    /// with ID `id` and returns the type of the parameter with name `param_name`
    /// in the method with name `method_name`.
    pub fn get_method_param_type(
        &self,
        adt_path: &LangPath,
        method_name: &str,
        idx: usize,
        id: BlockId,
    ) -> LangResult<TypeId> {
        let method = self.get_method(adt_path, method_name, id)?;
        self.get_param_type(method, idx)
    }

    /// Finds the function with the path `fn_path` in a scope containing the block
    /// with ID `id` and returns the type of the parameter with name `param_name`.
    pub fn get_fn_param_type(
        &self,
        fn_path: &LangPath,
        idx: usize,
        id: BlockId,
    ) -> LangResult<TypeId> {
        let func = self.get_fn(fn_path, id)?;
        self.get_param_type(func, idx)
    }

    /// Reports a error related to a ADT that can't be found.
    ///
    /// Given the path `err_path`, finds all ADTs that has a path that ends with
    /// the same identifier as the `err_path`. If atleast one ADT matches,
    /// information is added to the error message informing the user about the
    /// ADT(s) with a similar name.
    pub fn err_adt(&self, mut err_msg: String, err_path: &LangPath) -> LangError {
        let mut adt_suggestions = Vec::default();
        let end_ident = &err_path.last().unwrap().0;

        for (adt_path, _) in self.adts.keys() {
            if end_ident == &adt_path.last().unwrap().0 {
                adt_suggestions.push(adt_path.clone())
            }
        }

        if !adt_suggestions.is_empty() {
            err_msg.push_str("\nFound potential ADTs in inaccessable modules:\n - ");
            let suggestions = adt_suggestions
                .into_iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join("\n - ");
            err_msg.push_str(&suggestions);
        }

        self.err(err_msg)
    }

    /// Reports a error related to a trait that can't be found.
    ///
    /// Given the path `err_path`, finds all traits that has a path that ends with
    /// the same identifier as the `err_path`. If atleast one trait matches,
    /// information is added to the error message informing the user about the
    /// trait(s) with a similar name.
    pub fn err_trait(&self, mut err_msg: String, err_path: &LangPath) -> LangError {
        let mut adt_suggestions = Vec::default();
        let end_ident = &err_path.last().unwrap().0;

        for (adt_path, _) in self.traits.keys() {
            if end_ident == &adt_path.last().unwrap().0 {
                adt_suggestions.push(adt_path.clone())
            }
        }

        if !adt_suggestions.is_empty() {
            err_msg.push_str("\nFound potential traits in inaccessable modules:\n - ");
            let suggestions = adt_suggestions
                .into_iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join("\n - ");
            err_msg.push_str(&suggestions);
        }

        self.err(err_msg)
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new(msg, AnalyzeError, Some(self.file_pos))
    }
}
