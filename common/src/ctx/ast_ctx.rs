use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, HashSet},
    fmt::Debug,
    rc::Rc,
};

use log::Level;

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    file::{FileId, FileInfo, FilePosition},
    path::LangPath,
    token::{
        block::{Adt, AdtKind, BuiltIn, Fn, Trait},
        expr::Var,
    },
    BlockId, TypeId,
};

use super::{block_ctx::BlockCtx, ty_ctx::TyCtx};

#[derive(Debug)]
pub struct AstCtx {
    /// Contains all declarations that have been seen traversing down to this
    /// part of the code. The BlockId represent the outer scope for a item.
    /// For variables it will be the scope in which they are declared in and for
    /// the rest, the BlockId will be the parent block.
    /// A `Adt` represents either a struct, union or enum.
    pub variables: HashMap<(String, BlockId), Rc<RefCell<Var>>>,
    pub fns: HashMap<(LangPath, BlockId), Rc<RefCell<Fn>>>,
    pub adts: HashMap<(LangPath, BlockId), Rc<RefCell<Adt>>>,
    pub traits: HashMap<(LangPath, BlockId), Rc<RefCell<Trait>>>,

    /// Contains all built-in "fns".
    pub built_ins: HashMap<&'static str, BuiltIn>,

    pub block_ctxs: HashMap<BlockId, BlockCtx>,

    /// Mapping file IDs to the corresponding file information. This can be used
    /// to find the filename and directory for file IDs stored in "FilePosition"s.
    pub file_info: HashMap<FileId, FileInfo>,

    /// The file position where the `analyzer` currently is. When the analyzing
    /// is done, this variable will not be used and will be invalid.
    pub file_pos: FilePosition,
}

impl AstCtx {
    pub fn new(
        built_ins: HashMap<&'static str, BuiltIn>,
        file_info: HashMap<FileId, FileInfo>,
    ) -> LangResult<Self> {
        Ok(Self {
            variables: HashMap::default(),
            fns: HashMap::default(),
            adts: HashMap::default(),
            traits: HashMap::default(),

            built_ins,
            block_ctxs: HashMap::default(),

            file_info,
            file_pos: FilePosition::default(),
        })
    }

    pub fn debug_print(&self) {
        debug!("Block Info:\n{:#?}", self.block_ctxs);
        debug!("Variables:\n{:#?}", self.variables);
        debug!("Functions:\n{:#?}", self.fns);
        debug!("ADTs:\n{:#?}", self.adts);
        debug!("Traits:\n{:#?}", self.traits);
        debug!("Built-ins:\n{:#?}", self.built_ins);
    }

    /// Returns the parent block ID for the block with ID `id`.
    pub fn get_parent_id(&self, id: BlockId) -> LangResult<BlockId> {
        if let Some(block_info) = self.block_ctxs.get(&id) {
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
        if let Some(block_info) = self.block_ctxs.get(&id) {
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
        if let Some(block_info) = self.block_ctxs.get(&id) {
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
        if let Some(block_info) = self.block_ctxs.get(&id) {
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
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
        map: &HashMap<(LangPath, BlockId), T>,
    ) -> LangResult<LangPath>
    where
        T: std::fmt::Debug,
    {
        let mut err = match self.get_decl_scope(ty_ctx, path, id, map) {
            Ok(_) => return Ok(path.clone()),
            Err(err) => err,
        };

        if let Some(module) = self.get_module(id)? {
            let module_path = module.join(path, module.file_pos().cloned());
            if self.get_decl_scope(ty_ctx, &module_path, id, map).is_ok() {
                return Ok(module_path);
            }
        }

        let uses = self.get_uses(id)?;
        for use_path in uses {
            if use_path.last().unwrap().name() == path.first().unwrap().name() {
                let mut potential_use_path = use_path.clone();
                potential_use_path.pop();

                let combined_path = potential_use_path.join(path, path.file_pos().cloned());
                match self.get_decl_scope(ty_ctx, &combined_path, id, map) {
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

    pub fn calculate_fn_full_path(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<LangPath> {
        self.calculate_full_path(ty_ctx, path, id, &self.fns)
    }

    pub fn calculate_adt_full_path(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<LangPath> {
        self.calculate_full_path(ty_ctx, path, id, &self.adts)
    }

    pub fn calculate_trait_full_path(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<LangPath> {
        self.calculate_full_path(ty_ctx, path, id, &self.traits)
    }

    /// Given a name of a declaration `path` and a block scope `id`, returns
    /// the block in which the sought after declaration was declared.
    fn get_decl_scope<T>(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
        map: &HashMap<(LangPath, BlockId), T>,
    ) -> LangResult<BlockId>
    where
        T: std::fmt::Debug,
    {
        if map.get(&(path.clone(), id)).is_some() {
            Ok(id)
        } else if id == BlockCtx::DEFAULT_BLOCK_ID {
            let mut err_msg = format!(
                "Unable to find decl for \"{}\" ({:#?}).",
                ty_ctx.to_string_path(path),
                path
            );
            if log_enabled!(Level::Debug) {
                err_msg.push_str(&format!("\nmap: {:#?}", map))
            }
            Err(self.err(err_msg))
        } else {
            // Unable to find declaration in the current block scope. See
            // recursively if the declaration exists in a parent scope.
            let parent_id = self.get_parent_id(id)?;

            if id != parent_id {
                self.get_decl_scope(ty_ctx, path, parent_id, map)
            } else {
                Err(self.err(format!(
                    "Block with id {} is its own parent in block info.",
                    id
                )))
            }
        }
    }

    // TODO: See if possible to merge with `get_decl_scope()`
    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// the block in which the sought after variable was declared.
    pub fn get_var_decl_scope(&self, ident: &str, id: BlockId) -> LangResult<BlockId> {
        // old impl:
        // self.get_decl_scope(&ident.to_string(), id, &self.variables)

        if self.variables.get(&(ident.into(), id)).is_some() {
            Ok(id)
        } else if id == BlockCtx::DEFAULT_BLOCK_ID {
            let mut err_msg = format!("Unable to find decl for \"{}\".", ident);
            if log_enabled!(Level::Debug) {
                err_msg.push_str(&format!("\nvariables: {:#?}", self.variables))
            }
            Err(self.err(err_msg))
        } else {
            // Unable to find declaration in the current block scope. See
            // recursively if the declaration exists in a parent scope.
            let parent_id = self.get_parent_id(id)?;

            if id != parent_id {
                self.get_var_decl_scope(ident, parent_id)
            } else {
                Err(self.err(format!(
                    "Block with id {} is its own parent in block info.",
                    id
                )))
            }
        }
    }

    /// Given a name of a function `path` and a block scope `id`, returns
    /// the block in which the sought after function was declared.
    pub fn get_fn_decl_scope(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<BlockId> {
        self.get_decl_scope(ty_ctx, path, id, &self.fns)
    }

    /// Given a name of a ADT `path` and a block scope `id`, returns the block
    // in which the sought after ADT was declared.
    pub fn get_adt_decl_scope(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<BlockId> {
        self.get_decl_scope(ty_ctx, path, id, &self.adts)
    }

    /// Given a name of a interface `path` and a block scope `id`, returns
    /// the block in which the sought after interface was declared.
    pub fn get_trait_decl_scope(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<BlockId> {
        self.get_decl_scope(ty_ctx, path, id, &self.traits)
    }

    /// Given a name of a declaration `name`, returns a reference to the
    /// declaration in the AST.
    /// The assumption is that all declarations (ADTs/Traits/Functions) are
    /// declared in the default block.
    fn get<'a, T>(
        &self,
        ty_ctx: &TyCtx,
        name: &LangPath,
        map: &'a HashMap<(LangPath, BlockId), Rc<RefCell<T>>>,
    ) -> LangResult<Rc<RefCell<T>>> {
        let decl_block_id = BlockCtx::DEFAULT_BLOCK_ID;
        let key = (name.clone(), decl_block_id);

        if let Some(item) = map.get(&key) {
            Ok(Rc::clone(item))
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" ({:#?}) in decl block ID {}.\nMap keys:\n{:#?}",
                ty_ctx.to_string_path(name),
                name,
                decl_block_id,
                map.keys()
            )))
        }
    }

    /// Given a name of a declaration `name`, returns a mutable reference
    /// to the declaration in the AST.
    /// The assumption is that all declarations (ADTs/Traits) are declared in
    /// the default block.
    fn get_mut<'a, T>(
        &self,
        ty_ctx: &TyCtx,
        name: &LangPath,
        map: &'a HashMap<(LangPath, BlockId), Rc<RefCell<T>>>,
    ) -> LangResult<RefMut<'a, T>> {
        let decl_block_id = BlockCtx::DEFAULT_BLOCK_ID;
        let key = (name.clone(), decl_block_id);

        if let Some(item) = map.get(&key) {
            Ok(item.borrow_mut())
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" ({:#?}) in decl block ID {}.\nMap keys:\n{:#?}",
                ty_ctx.to_string_path(name),
                name,
                decl_block_id,
                map.keys()
            )))
        }
    }

    /// Checks if there exists a struct with name `path.
    pub fn is_struct(&self, ty_ctx: &TyCtx, path: &LangPath) -> bool {
        if let Ok(adt) = self.get_adt(ty_ctx, path) {
            matches!(adt.borrow().kind, AdtKind::Struct)
        } else {
            false
        }
    }

    /// Checks if there exists a union with name `path`.
    pub fn is_union(&self, ty_ctx: &TyCtx, path: &LangPath) -> bool {
        if let Ok(adt) = self.get_adt(ty_ctx, path) {
            matches!(adt.borrow().kind, AdtKind::Union)
        } else {
            false
        }
    }

    /// Checks if there exists a enum with name `path`.
    pub fn is_enum(&self, ty_ctx: &TyCtx, path: &LangPath) -> bool {
        if let Ok(adt) = self.get_adt(ty_ctx, path) {
            matches!(adt.borrow().kind, AdtKind::Enum)
        } else {
            false
        }
    }

    /// Checks if there exists a union with name `path`.
    pub fn is_trait(&self, ty_ctx: &TyCtx, path: &LangPath) -> bool {
        self.get_trait(ty_ctx, path).is_ok()
    }

    // TODO: Merge with `get()`.
    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_var(&self, ident: &str, id: BlockId) -> LangResult<Rc<RefCell<Var>>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        let key = (ident.into(), decl_block_id);

        if let Some(item) = self.variables.get(&key) {
            Ok(Rc::clone(item))
        } else {
            Err(self.err(format!(
                "Unable to find var decl with name \"{}\" in decl block ID {}.",
                ident, decl_block_id
            )))
        }
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_var_mut(&self, ident: &str, id: BlockId) -> LangResult<RefMut<Var>> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        let key = (ident.into(), decl_block_id);

        if let Some(item) = self.variables.get(&key) {
            Ok(item.borrow_mut())
        } else {
            Err(self.err(format!(
                "Unable to find var decl with name \"{}\" in decl block ID {}.",
                ident, decl_block_id
            )))
        }
    }

    /// Given a name of a function `path` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_fn(&self, ty_env: &TyCtx, path: &LangPath) -> LangResult<Rc<RefCell<Fn>>> {
        self.get(ty_env, path, &self.fns)
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
    pub fn get_adt(&self, ty_ctx: &TyCtx, path: &LangPath) -> LangResult<Rc<RefCell<Adt>>> {
        self.get(ty_ctx, path, &self.adts)
    }

    /// Given a partial path of a ADT `path` and a block scope `id`; tries to find
    /// the declaration of a ADT. If unable to find a ADT with path `path`, this
    /// function will look at the "use" statements for the current block to see
    /// if able to resolve the ADT.
    pub fn get_adt_partial(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<Rc<RefCell<Adt>>> {
        let real_path = self.calculate_full_path(ty_ctx, path, id, &self.adts)?;
        self.get_adt(ty_ctx, &real_path)
    }

    /// Given a name of a trait `path`, returns a reference to the declaration in
    /// the AST.
    pub fn get_trait(&self, ty_ctx: &TyCtx, path: &LangPath) -> LangResult<Rc<RefCell<Trait>>> {
        self.get(ty_ctx, path, &self.traits)
    }

    /// Given a partial path of a trait `path` and a block scope `id`; tries to find
    /// the declaration of a trait. If unable to find a trait with path `path`, this
    /// function will look at the "use" statements for the current block to see
    /// if able to resolve the trait.
    pub fn get_trait_partial(
        &self,
        ty_ctx: &TyCtx,
        path: &LangPath,
        id: BlockId,
    ) -> LangResult<Rc<RefCell<Trait>>> {
        let real_path = self.calculate_full_path(ty_ctx, path, id, &self.adts)?;
        self.get_trait(ty_ctx, &real_path)
    }

    /// Given a name of a ADT `adt_name`, a name of a method `method_name`;
    /// returns a reference to the declaration in the AST.
    pub fn get_method(
        &self,
        ty_ctx: &TyCtx,
        adt_name: &LangPath,
        method_name: &str,
    ) -> LangResult<Rc<RefCell<Fn>>> {
        let adt = self.get_adt(ty_ctx, adt_name)?;
        let adt = adt.borrow();

        if let Some(method) = adt.methods.get(method_name) {
            Ok(Rc::clone(method))
        } else {
            Err(self.err(format!(
                "Unable to find method named \"{}\" in ADT \"{:#?}\".",
                &method_name,
                &adt.methods.keys(),
            )))
        }
    }

    /// Given a name of a trait `trait_name`; returns names of all methods declared
    /// for the trait.
    pub fn get_trait_method_names(
        &self,
        ty_ctx: &TyCtx,
        trait_name: &LangPath,
    ) -> LangResult<Vec<String>> {
        let trait_ = self.get_trait(ty_ctx, trait_name)?;
        let trait_ = trait_.borrow();

        Ok(trait_
            .methods
            .iter()
            .map(|func| func.name.clone())
            .collect())
    }

    /// Inserts the given method `method` into the ADT with name `adt_name`.
    /// This ADT can be a struct, enum or union.
    pub fn insert_method(
        &mut self,
        ty_ctx: &TyCtx,
        adt_name: &LangPath,
        method: Rc<RefCell<Fn>>,
    ) -> LangResult<()> {
        let method_name = method.borrow().half_name(ty_ctx);

        let adt = self.get_adt(ty_ctx, adt_name)?;
        let mut adt = adt.borrow_mut();

        adt.methods.insert(method_name, Rc::clone(&method));

        Ok(())
    }

    /// Removes the method with name `method_name` from the ADT with name `adt_name`.
    /// Returns true if the method was removed, returns false if no method with
    /// that name exists for the ADT. Returns error if the ADT can't be found.
    pub fn remove_method(
        &mut self,
        ty_ctx: &TyCtx,
        adt_name: &LangPath,
        method_name: &str,
    ) -> LangResult<bool> {
        let adt = self.get_adt(ty_ctx, adt_name)?;
        let mut adt = adt.borrow_mut();

        Ok(adt.methods.remove(method_name).is_some())
    }

    /// Finds the ADT with the name `adt_name` and returns the member with name
    /// `member_name`.
    pub fn get_adt_member(
        &self,
        ty_ctx: &TyCtx,
        adt_name: &LangPath,
        member_name: &str,
        file_pos: Option<FilePosition>,
    ) -> LangResult<Rc<RefCell<Var>>> {
        let adt = self.get_adt(ty_ctx, adt_name)?;
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
                    &member_name,
                    ty_ctx.to_string_path(adt_name),
                ),
                LangErrorKind::GeneralError,
                file_pos,
            ))
        }
    }

    /// Finds the ADT with the name `adt_name` and returns the index of the member
    /// with name `member_name`.
    pub fn get_adt_member_index(
        &self,
        ty_ctx: &TyCtx,
        adt_name: &LangPath,
        member_name: &str,
    ) -> LangResult<u64> {
        if let Some(idx) = self
            .get_adt(ty_ctx, adt_name)?
            .borrow()
            .member_index(member_name)
        {
            Ok(idx as u64)
        } else {
            Err(self.err(format!(
                "Unable to find member with name \"{}\" in ADT \"{}\".",
                &member_name,
                ty_ctx.to_string_path(adt_name),
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

    /// Finds the ADT with the name `adt_path` and returns the index of the
    /// parameter with name `param_name` in the method with name `method_name`.
    pub fn get_method_param_idx(
        &self,
        ty_ctx: &TyCtx,
        adt_path: &LangPath,
        method_name: &str,
        param_name: &str,
    ) -> LangResult<usize> {
        let method = self.get_method(ty_ctx, adt_path, method_name)?;
        self.get_param_idx(method, param_name)
    }

    /// Finds the function with the path `fn_path` and returns the index of the
    /// parameter with name `param_name`.
    pub fn get_fn_param_idx(
        &self,
        ty_ctx: &TyCtx,
        fn_path: &LangPath,
        param_name: &str,
    ) -> LangResult<usize> {
        let func = self.get_fn(ty_ctx, fn_path)?;
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

    /// Finds the ADT with the name `adt_path` and returns the type of the parameter
    /// with name `param_name` in the method with name `method_name`.
    pub fn get_method_param_type(
        &self,
        ty_ctx: &TyCtx,
        adt_path: &LangPath,
        method_name: &str,
        idx: usize,
    ) -> LangResult<TypeId> {
        let method = self.get_method(ty_ctx, adt_path, method_name)?;
        self.get_param_type(method, idx)
    }

    /// Finds the function with the path `fn_path` and returns the type of the
    /// parameter with name `param_name`.
    pub fn get_fn_param_type(
        &self,
        ty_ctx: &TyCtx,
        fn_path: &LangPath,
        idx: usize,
    ) -> LangResult<TypeId> {
        let func = self.get_fn(ty_ctx, fn_path)?;
        self.get_param_type(func, idx)
    }

    /// Reports a error related to a ADT that can't be found.
    ///
    /// Given the path `err_path`, finds all ADTs that has a path that ends with
    /// the same identifier as the `err_path`. If atleast one ADT matches,
    /// information is added to the error message informing the user about the
    /// ADT(s) with a similar name.
    pub fn err_adt(&self, ty_ctx: &TyCtx, mut err_msg: String, err_path: &LangPath) -> LangError {
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
                .map(|p| ty_ctx.to_string_path(&p))
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
    pub fn err_trait(&self, ty_ctx: &TyCtx, mut err_msg: String, err_path: &LangPath) -> LangError {
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
                .map(|p| ty_ctx.to_string_path(&p))
                .collect::<Vec<_>>()
                .join("\n - ");
            err_msg.push_str(&suggestions);
        }

        self.err(err_msg)
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new(msg, LangErrorKind::AnalyzeError, Some(self.file_pos))
    }
}
