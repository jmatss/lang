mod block;
mod call_args;
mod decl_func;
mod decl_type;
mod decl_var;
mod defer;
mod indexing;
mod method;
mod type_context;
mod type_converter;
mod type_inferencer;
mod type_solver;
//mod unitialized;

use block::BlockAnalyzer;
use call_args::CallArgs;
use common::{
    error::{CustomResult, LangError, LangErrorKind::AnalyzeError},
    r#type::ty::Ty,
    token::{
        ast::AstToken,
        block::{Enum, Function, Interface, Struct},
        expr::Var,
        stmt::Path,
    },
    traverser::AstTraverser,
    BlockId,
};
use decl_func::DeclFuncAnalyzer;
use decl_type::DeclTypeAnalyzer;
use decl_var::DeclVarAnalyzer;
use defer::DeferAnalyzer;
use indexing::IndexingAnalyzer;
use log::debug;
use method::MethodAnalyzer;
use std::{cell::RefCell, collections::HashMap};
use type_context::TypeContext;
use type_converter::TypeConverter;
use type_inferencer::TypeInferencer;
use type_solver::TypeSolver;

// TODO: Error if a function that doesn't have a return type has a return in it.
// TODO: Make it so that one doesn't have to recreate a new AstVisitor for every
//       analyzing step. They all want to shared the same `analyze_context` but
//       moving the analyzer contianing the `analyze_context` into the traverser
//       will not work since rust assumes that the analyzer will live for the
//       whole life of the traverser. And since the analyzer contains a mut ref
//       to the `analyze_context`, rust will assume that the `analyze_context`
//       will live for the while "analyze" function inside the analyzer object.
// TODO: Since the "defer" copies expressions in the AST, is there a possiblity
//       that the corresponding "DeferExec" and its contents gets different types
//       during type analyzing? Would that be a problem if it is true?

/// Updates the AST with information about function prototypes and declarations
/// of structures.
/// The "defer analyzing" depends on being able to lookup block info, so it needs
/// to be ran after "block analyzing".
/// The "TypeInference/TypeSolve" depends on the "Decl...Analyzer"s to figure out
/// types for function calls and function parameters used in expressions, so the
/// "Decl...Analyzer"s needs to be ran before "Type...".
/// The "var & func decl analyzer" depends on the "type decl analyzer" since the
/// types might be used by variables/functions.
/// The "IndexingAnalyzer" should be ran before the "Type..." since the "Type..."
/// will fail if the indexing analyzing doesn't rewrite the AST before that.
/// The "MethodAnalyzer" should be ran before "Type..." since it rewrites the AST
/// so that the "type analyzing" can know that it is a method vs function.
/// The "MethodAnalyzer" should be ran after "decl analyzers" since it will
/// need struct/func information to figure out types for method calls.
/// "decl_func_analyzer"/"decl_type_analyzer"/"type..." needs to be ran before
/// the "call_args" since it needs to access structs, functions and methods.
pub fn analyze(ast_root: &mut AstToken) -> Result<AnalyzeContext, Vec<LangError>> {
    let analyze_context = RefCell::new(AnalyzeContext::new());

    debug!("Running IndexingAnalyzer");
    let mut indexing_analyzer = IndexingAnalyzer::new();
    AstTraverser::new()
        .add_visitor(&mut indexing_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running BlockAnalyzer");
    let mut block_analyzer = BlockAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut block_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running DeclTypeAnalyzer");
    let mut decl_type_analyzer = DeclTypeAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut decl_type_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running DeclVarAnalyzer, running DeclFuncAnalyzer");
    let mut decl_var_analyzer = DeclVarAnalyzer::new(&analyze_context);
    let mut decl_func_analyzer = DeclFuncAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut decl_func_analyzer)
        .add_visitor(&mut decl_var_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running MethodAnalyzer");
    let mut method_analyzer = MethodAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut method_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running DeferAnalyzer");
    let mut defer_analyzer = DeferAnalyzer::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut defer_analyzer)
        .traverse(ast_root)
        .take_errors()?;

    let mut analyze_context = analyze_context.replace(AnalyzeContext::default());
    let mut type_context = TypeContext::new(&mut analyze_context);

    debug!("Running TypeInferencer");
    let mut type_inferencer = TypeInferencer::new(&mut type_context);
    AstTraverser::new()
        .add_visitor(&mut type_inferencer)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running TypeSolver");
    let mut type_solver = TypeSolver::new(&mut type_context);
    AstTraverser::new()
        .add_visitor(&mut type_solver)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running TypeConverter");
    let generic_structs = type_solver.generic_structs;
    let generic_struct_methods = type_solver.generic_struct_methods;
    let mut type_converter = TypeConverter::new(
        &mut analyze_context,
        generic_structs,
        generic_struct_methods,
    );
    AstTraverser::new()
        .add_visitor(&mut type_converter)
        .traverse(ast_root)
        .take_errors()?;

    debug!("Running CallArgs");
    let mut call_args = CallArgs::new(&analyze_context);
    AstTraverser::new()
        .add_visitor(&mut call_args)
        .traverse(ast_root)
        .take_errors()?;

    Ok(analyze_context)
}

#[derive(Debug, Clone)]
pub struct BlockInfo {
    pub block_id: BlockId,
    pub parent_id: BlockId,

    /// Contains information about which control flow statements this block contains.
    /// This will be used during code generation to figure out which
    /// instructions needs to be generated and where branches should jump etc.
    pub contains_return: bool,
    pub contains_yield: bool,
    pub contains_break: bool,
    pub contains_continue: bool,
    pub contains_defer: bool,

    /// Contains information about the control flow of children. If all children
    /// contains a branch instruction, this block doesn't not need to add a
    /// implicit "terminator" at the end of the basic block, since there is no
    /// logical path that leads to the end of this block.
    pub all_children_contains_returns: bool,

    /// A "block root" is a block that starts a new scope that contains blocks that
    /// only have access to items inside this scope.
    /// For example:
    /// ```
    /// fn f() {            // <- BlockId 0
    ///   if (true) {       // <- BlockId 1
    ///     if (false) {}   // <- BlockId 2
    ///   }
    /// }
    /// ```
    /// In this example blocks 1 and 2 only lives inside block 0 (a function).
    /// So the "root block" for 1 & 2 would be 0. In turn the function would
    /// have a surrounding "root block" (which isn't shown in this example).
    pub is_root_block: bool,

    /// Indicates if this block is a "branchable" block that can contain "break"
    /// and "continue" statements. This is true for while-loops while it is not
    /// true for if-statements.
    pub is_branchable_block: bool,
}

impl BlockInfo {
    /// The block id given to the default block.
    const DEFAULT_BLOCK_ID: BlockId = 0;

    pub fn new(block_id: BlockId, is_root_block: bool, is_branchable_block: bool) -> Self {
        Self {
            block_id,
            parent_id: usize::MAX,
            contains_return: false,
            contains_yield: false,
            contains_break: false,
            contains_continue: false,
            contains_defer: false,
            all_children_contains_returns: false,
            is_root_block,
            is_branchable_block,
        }
    }
}

#[derive(Debug)]
pub struct AnalyzeContext {
    /// Contains all declarations that have been seen traversing down to this
    /// part of the code. The BlockId represent the outer scope for a item.
    /// For variables it will be the scope in which they are declared in and for
    /// the rest, the BlockId will be the parent block.
    variables: HashMap<(String, BlockId), *mut Var>,
    functions: HashMap<(String, BlockId), *mut Function>,
    structs: HashMap<(String, BlockId), *mut Struct>,
    enums: HashMap<(String, BlockId), *mut Enum>,
    interfaces: HashMap<(String, BlockId), *mut Interface>,

    pub block_info: HashMap<BlockId, BlockInfo>,
    pub use_paths: Vec<Path>,

    /// The line/column where the `analyzer` currently is. When the analyzing is
    /// done, these variable will not be used and will be invalid.
    pub line_nr: u64,
    pub column_nr: u64,
}

impl Default for AnalyzeContext {
    fn default() -> Self {
        AnalyzeContext::new()
    }
}

impl AnalyzeContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::default(),
            functions: HashMap::default(),
            structs: HashMap::default(),
            enums: HashMap::default(),
            interfaces: HashMap::default(),

            block_info: HashMap::default(),
            use_paths: Vec::default(),

            line_nr: 0,
            column_nr: 0,
        }
    }

    pub fn debug_print(&self) {
        /*
        debug!(
            "Variables:\n{:#?}",
            self.variables
                .iter()
                .map(|(k, v)| (k, unsafe { v.as_ref() }.unwrap()))
                .collect::<HashMap<_, _>>()
        );
        debug!(
            "Functions:\n{:#?}",
            self.functions
                .iter()
                .map(|(k, v)| (k, unsafe { v.as_ref() }.unwrap()))
                .collect::<HashMap<_, _>>()
        );
        debug!(
            "Structs:\n{:#?}",
            self.structs
                .iter()
                .map(|(k, v)| (k, unsafe { v.as_ref() }.unwrap()))
                .collect::<HashMap<_, _>>()
        );
        debug!(
            "Enums:\n{:#?}",
            self.enums
                .iter()
                .map(|(k, v)| (k, unsafe { v.as_ref() }.unwrap()))
                .collect::<HashMap<_, _>>()
        );
        debug!(
            "Interfaces:\n{:#?}",
            self.interfaces
                .iter()
                .map(|(k, v)| (k, unsafe { v.as_ref() }.unwrap()))
                .collect::<HashMap<_, _>>()
        );
        debug!(
            "Methods:\n{:#?}",
            self.structs
                .iter()
                .map(|(ks, vs)| (
                    ks,
                    unsafe { vs.as_ref().unwrap() }.clone().methods.map(|m| m
                        .iter()
                        .map(|(n, f)| (n.clone(), unsafe { f.as_ref() }.unwrap()))
                        .collect::<Vec<_>>())
                ))
                .collect::<HashMap<_, _>>()
        );
        */
        debug!("Block info:\n{:#?}", self.block_info);
    }

    /// Returns the block ID for the block with ID `id`.
    pub fn get_parent(&self, id: BlockId) -> CustomResult<BlockId> {
        if let Some(block_info) = self.block_info.get(&id) {
            Ok(block_info.parent_id)
        } else {
            Err(self.err(format!(
                "Unable to find block info for block with id: {}",
                id
            )))
        }
    }

    /// Given a name of a declaration `ident` and a block scope `id`, returns
    /// the block in which the sought after declaration was declared.
    ///
    // TODO: Is `stop_at_root` even needed? In which cases is it needed to stop
    //       stop at the first root block? Can there be sted functions?
    /// `stop_at_root` indicates if the search should stop once the first root
    /// block is found. This should be used for ex. variables so that only the
    /// variables with the name `ident` in the current scope is considered and
    /// not any parent scopes.
    fn get_decl_scope<T>(
        &self,
        ident: &str,
        id: BlockId,
        map: &HashMap<(String, BlockId), T>,
    ) -> CustomResult<BlockId> {
        if map.get(&(ident.into(), id)).is_some() {
            Ok(id)
        } else if id == BlockInfo::DEFAULT_BLOCK_ID {
            Err(self.err(format!(
                "Unable to find decl for \"{}\", ended in up root block.",
                ident
            )))
        } else {
            // Unable to find declaration in the current block scope. See
            // recursively if the declaration exists in a parent scope.
            let parent_id = self.get_parent(id)?;

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
    pub fn get_interface_decl_scope(&self, ident: &str, id: BlockId) -> CustomResult<BlockId> {
        self.get_decl_scope(ident, id, &self.interfaces)
    }

    /// Given a name of a declaration `ident` and the block in which this
    /// declaration was declared, `decl_block_id`, returns a mutable reference
    /// to the declaration in the AST.
    fn get_mut<'a, T>(
        &self,
        ident: &str,
        decl_block_id: BlockId,
        map: &HashMap<(String, BlockId), *mut T>,
    ) -> CustomResult<&'a mut T> {
        let key = (ident.into(), decl_block_id);

        if let Some(ptr) = map.get(&key) {
            if let Some(ref_) = unsafe { ptr.as_mut() } {
                Ok(ref_)
            } else {
                panic!(
                    "Invalid pointer to decl with name \"{}\" in decl block ID {}.",
                    ident, decl_block_id
                );
            }
        } else {
            Err(self.err(format!(
                "Unable to find decl with name \"{}\" in decl block ID {}.",
                ident, decl_block_id
            )))
        }
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_var(&self, ident: &str, id: BlockId) -> CustomResult<&Var> {
        Ok(self.get_var_mut(ident, id)?)
    }

    /// Given a name of a variable `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_var_mut(&self, ident: &str, id: BlockId) -> CustomResult<&mut Var> {
        let decl_block_id = self.get_var_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.variables)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_func(&self, ident: &str, id: BlockId) -> CustomResult<&Function> {
        Ok(self.get_func_mut(ident, id)?)
    }

    /// Given a name of a function `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_func_mut(&self, ident: &str, id: BlockId) -> CustomResult<&mut Function> {
        let decl_block_id = self.get_func_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.functions)
    }

    /// Given a name of a struct `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_struct(&self, ident: &str, id: BlockId) -> CustomResult<&Struct> {
        Ok(self.get_struct_mut(ident, id)?)
    }

    /// Given a name of a struct `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_struct_mut(&self, ident: &str, id: BlockId) -> CustomResult<&mut Struct> {
        let decl_block_id = self.get_struct_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.structs)
    }

    /// Given a name of a enum `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_enum(&self, ident: &str, id: BlockId) -> CustomResult<&Enum> {
        Ok(self.get_enum_mut(ident, id)?)
    }

    /// Given a name of a enum `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_enum_mut(&self, ident: &str, id: BlockId) -> CustomResult<&mut Enum> {
        let decl_block_id = self.get_enum_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.enums)
    }

    /// Given a name of a interface `ident` and a block scope `id`, returns
    /// a reference to the declaration in the AST.
    pub fn get_interface(&self, ident: &str, id: BlockId) -> CustomResult<&Interface> {
        Ok(self.get_interface_mut(ident, id)?)
    }

    /// Given a name of a interface `ident` and a block scope `id`, returns
    /// a mutable reference to the declaration in the AST.
    pub fn get_interface_mut(&self, ident: &str, id: BlockId) -> CustomResult<&mut Interface> {
        let decl_block_id = self.get_interface_decl_scope(ident, id)?;
        self.get_mut(ident, decl_block_id, &self.interfaces)
    }

    /// Given a name of a struct `struct_`, a name of a method `func` and a
    /// block scope `id`, returns a reference to the declaration in the AST.
    pub fn get_method(&self, struct_: &str, func: &str, id: BlockId) -> CustomResult<&Function> {
        Ok(self.get_method_mut(struct_, func, id)?)
    }

    /// Given a name of a struct `struct_`, a name of a method `func` and a
    /// block scope `id`, returns a reference mutable to the declaration in
    /// the AST.
    pub fn get_method_mut(
        &self,
        struct_name: &str,
        func_name: &str,
        id: BlockId,
    ) -> CustomResult<&mut Function> {
        let decl_block_id = self.get_struct_decl_scope(struct_name, id)?;
        let struct_ = self.get_struct(struct_name, decl_block_id)?;

        if let Some(ptr) = struct_
            .methods
            .as_ref()
            .and_then(|ref map| map.get(func_name))
        {
            if let Some(ref_) = unsafe { ptr.as_mut() } {
                Ok(ref_)
            } else {
                panic!(
                    "Invalid pointer to method with name \"{}\" in struct {:#?}.",
                    &func_name, &struct_
                );
            }
        } else {
            Err(self.err(format!(
                "Unable to find method named \"{}\" in struct \"{:#?}\".",
                &func_name, &struct_,
            )))
        }
    }

    /// Inserts the given method `func` into the struct with name `struct_name`
    /// that can be found from the block id `id`.
    pub fn insert_method(
        &mut self,
        struct_name: &str,
        func: &mut Function,
        id: BlockId,
    ) -> CustomResult<()> {
        let decl_block_id = self.get_struct_decl_scope(struct_name, id)?;
        let struct_ = self.get_struct_mut(struct_name, decl_block_id)?;

        let methods = if let Some(methods) = &mut struct_.methods {
            methods
        } else {
            struct_.methods = Some(HashMap::default());
            struct_.methods.as_mut().unwrap()
        };

        let ptr = func as *mut Function;
        methods.insert(func.name.clone(), ptr);

        Ok(())
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the member with name `member_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_struct_member(
        &self,
        struct_name: &str,
        member_name: &str,
        id: BlockId,
    ) -> CustomResult<&Var> {
        let struct_ = self.get_struct(struct_name, id)?;
        if let Some(members) = &struct_.members {
            if let Some(var) = members.iter().find(|member| member.name == member_name) {
                Ok(var)
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
        if let Some(idx) = self.get_struct(struct_name, id)?.member_index(member_name) {
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
    ) -> CustomResult<&Var> {
        let enum_ = self.get_enum(enum_name, id)?;
        if let Some(members) = &enum_.members {
            if let Some(var) = members.iter().find(|member| member.name == member_name) {
                Ok(var)
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

    /// Given a function or method `func`, finds the parameter with the name
    // `param_name` and also its index.
    fn get_param(&self, func: &Function, param_name: &str) -> CustomResult<(u64, Var)> {
        if let Some(params) = &func.parameters {
            for (idx, param) in params.iter().enumerate() {
                if param_name == param.name {
                    return Ok((idx as u64, param.clone()));
                }
            }

            Err(self.err(format!(
                "Unable to find param with name \"{}\" in function with name \"{}\".",
                &param_name, &func.name,
            )))
        } else {
            Err(self.err(format!(
                "Function \"{}\" had no parameters, expected param with name: {}",
                &func.name, &param_name
            )))
        }
    }

    /// Given a function or method `func`, finds the parameter with the name
    // `param_name` and also its index.
    fn get_param_with_idx(&self, func: &Function, idx: u64) -> CustomResult<Var> {
        if let Some(params) = &func.parameters {
            for (i, param) in params.iter().enumerate() {
                if idx == i as u64 {
                    return Ok(param.clone());
                }
            }

            Err(self.err(format!(
                "Unable to find param with index \"{}\" in function with name \"{}\".",
                idx, &func.name,
            )))
        } else {
            Err(self.err(format!(
                "Function \"{}\" had no parameters, expected param with index: {}",
                &func.name, idx
            )))
        }
    }

    /// Given a function or method `func`, finds the index of the parameter with
    /// the name `param_name`. The index indicates the position of the parameter
    /// in the struct parameter list.
    fn get_param_idx(&self, func: &Function, param_name: &str) -> CustomResult<u64> {
        Ok(self.get_param(func, param_name)?.0)
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`
    /// in the method with name `method_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_method_param_idx(
        &self,
        struct_name: &str,
        method_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        let method = self.get_method(struct_name, method_name, id)?;
        self.get_param_idx(method, param_name)
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the index of the parameter with name `param_name`.
    pub fn get_func_param_idx(
        &self,
        func_name: &str,
        param_name: &str,
        id: BlockId,
    ) -> CustomResult<u64> {
        let func = self.get_func(func_name, id)?;
        self.get_param_idx(func, param_name)
    }

    /// Given a function or method `func`, finds the type of the parameter with
    /// the name `param_name`.
    fn get_param_type(&self, func: &Function, idx: u64) -> CustomResult<Ty> {
        if let Some(ty) = &self.get_param_with_idx(func, idx)?.ret_type {
            Ok(ty.clone())
        } else {
            Err(self.err(format!(
                "Parameter at index \"{}\" in function \"{}\" has no type set.",
                idx, func.name
            )))
        }
    }

    /// Finds the struct with the name `struct_name` in a scope containing the block
    /// with ID `id` and returns the type of the parameter with name `param_name`
    /// in the method with name `method_name`.
    /// The struct can ex. be declared in parent block scope.
    pub fn get_method_param_type(
        &self,
        struct_name: &str,
        method_name: &str,
        idx: u64,
        id: BlockId,
    ) -> CustomResult<Ty> {
        let method = self.get_method(struct_name, method_name, id)?;
        self.get_param_type(method, idx)
    }

    /// Finds the function with the name `func_name` in a scope containing the block
    /// with ID `id` and returns the type of the parameter with name `param_name`.
    pub fn get_func_param_type(&self, func_name: &str, idx: u64, id: BlockId) -> CustomResult<Ty> {
        let func = self.get_func(func_name, id)?;
        self.get_param_type(func, idx)
    }

    /// Used when returing errors to include current line/column number.
    pub fn err(&self, msg: String) -> LangError {
        LangError::new_backtrace(
            msg,
            AnalyzeError {
                line_nr: self.line_nr,
                column_nr: self.column_nr,
            },
            true,
        )
    }
}
