use crate::{
    file::FilePosition, hash::DerefType, hash_map::TyEnvHashMap, hash_set::TyEnvHashSet,
    path::LangPath, ty::ty_env::TyEnv, BlockId,
};

#[derive(Debug)]
pub struct BlockCtx {
    pub block_id: BlockId,
    pub parent_id: BlockId,

    /// Currently the position of the token at the start of the block. This
    /// should in the future be the full range of the whole block.
    pub file_pos: FilePosition,

    /// Contains information about which control flow statements this block contains.
    /// This will be used during code generation to figure out which
    /// instructions needs to be generated and where branches should jump etc.
    pub contains_return: bool,
    pub contains_yield: bool,
    pub contains_break: bool,
    pub contains_continue: bool,
    pub contains_defer: bool,

    /// Contains information about the control flow of children. If all children
    /// contains a return instruction, this block doesn't not need to add a
    /// implicit "terminator" at the end of the basic block, since there is no
    /// logical path that leads to the end of this block.
    pub all_children_contains_return: bool,

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

    /// The module/nameSpace that this block is stored in.
    pub module: Option<LangPath>,

    /// The "use" paths that are "active" for the current block. These "use"s
    /// can be used to resolve type/function paths in the current block.
    /// Currently ALL "use" statements declared in a block is accessable from the
    /// whole block, even if the "use" is declared below a statement that uses it,
    /// order doesn't matter. This might change in the future.
    pub uses: TyEnvHashSet<LangPath>,

    /// Same as `uses` but these are the uses that have a `as` to "rename" the
    /// use. The key of the map is the new `as` ident, the LangPath only consists
    /// of the single ident but are stored as a LangPath for easier look-up.
    /// The values of the map are the actual `use` path.
    pub uses_as: TyEnvHashMap<LangPath, LangPath>,
}

impl BlockCtx {
    /// The block id given to the default block.
    pub const DEFAULT_BLOCK_ID: BlockId = 0;

    pub fn new(
        block_id: BlockId,
        file_pos: FilePosition,
        is_root_block: bool,
        is_branchable_block: bool,
        module: Option<LangPath>,
        uses: TyEnvHashSet<LangPath>,
        uses_as: TyEnvHashMap<LangPath, LangPath>,
    ) -> Self {
        Self {
            block_id,
            parent_id: usize::MAX,
            file_pos,
            contains_return: false,
            contains_yield: false,
            contains_break: false,
            contains_continue: false,
            contains_defer: false,
            all_children_contains_return: false,
            is_root_block,
            is_branchable_block,
            module,
            uses,
            uses_as,
        }
    }

    pub fn add_use(&mut self, ty_env: &TyEnv, path: LangPath, ident: Option<String>) {
        if let Some(ident) = ident {
            let ident_path = LangPath::new(vec![ident.into()], path.file_pos);
            self.uses_as
                .insert(ty_env, DerefType::None, ident_path, path.without_gens())
                .unwrap();
        } else {
            self.uses
                .insert(ty_env, DerefType::None, path.without_gens())
                .unwrap();
        }
    }
}
