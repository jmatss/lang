use std::collections::{hash_map::Entry, HashMap};

use log::{debug, log_enabled, Level};

use common::{
    ctx::{ast_ctx::AstCtx, block_ctx::BlockCtx},
    error::{LangError, LangErrorKind, LangResult},
    file::FilePosition,
    hash::DerefType,
    hash_set::TyEnvHashSet,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Block, BlockHeader},
        stmt::Stmt,
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::ty_env::TyEnv,
    BlockId,
};

/// Visits all blocks and gathers information related to them. It will add
/// "BlockInfo"s into the analyze context containing information about block
/// parent, specific branch statements the block contains etc.
pub struct BlockAnalyzer {
    /// Will contain the current module/nameSpace for the block that is being
    /// analyzed.
    module: Option<LangPath>,

    /// Contains all "use" statements found in the blocks with ID `BlockId` that
    /// have been traversed so far.
    uses: HashMap<BlockId, TyEnvHashSet<LangPath>>,

    /// Will be set to true every time a new file is started to be analyzed.
    /// A "mod" statement must be the first statement in a file, it is not allowed
    /// to be anywhere else.
    /// This variable will be used to make sure that this is the case.
    is_first_stmt: bool,

    errors: Vec<LangError>,
}

impl BlockAnalyzer {
    pub fn new() -> Self {
        Self {
            module: None,
            uses: HashMap::default(),
            is_first_stmt: true,
            errors: Vec::default(),
        }
    }

    /// Given a block ID `id`, returns every "use" statements for this block
    /// and all its parent blocks located in the same source file.
    ///
    /// Since the constructor for a BlockInfo takes the uses, this function
    /// should usualy be called with the parent ID of the block that you are
    /// currently trying to construct.
    ///
    /// The returned vector will only contain the uses that have been traversed
    /// in the current block. For example:
    ///
    /// ```no_run
    /// {
    ///     use a;
    ///     println!("Analyzing this line <-");
    ///     use b;
    /// }
    /// ```
    ///
    /// In this example, if this `get_uses` is called at the println line,
    /// only the "use a" will be contained in the returned set.
    /// The "use b" will not be contained in the uses since it hasn't been
    /// traversed yet.
    fn get_uses(
        &self,
        ty_env: &TyEnv,
        ast_ctx: &mut AstCtx,
        id: BlockId,
    ) -> LangResult<TyEnvHashSet<LangPath>> {
        let mut uses = TyEnvHashSet::default();

        let mut cur_id = id;
        while let Some(cur_block_info) = ast_ctx.block_ctxs.get(&cur_id) {
            if let Some(cur_uses) = self.uses.get(&cur_id) {
                uses.extend(ty_env, DerefType::None, cur_uses)?;
            }

            if cur_id == BlockCtx::DEFAULT_BLOCK_ID {
                break;
            }

            cur_id = cur_block_info.parent_id;
        }

        Ok(uses)
    }

    /// Returns true if the given header is a "root" i.e. a block that creates
    /// a new scope where every block created inside them only has access to
    /// this block + globals.
    fn is_root(&self, header: &BlockHeader) -> bool {
        matches!(
            header,
            BlockHeader::Fn(_)
                | BlockHeader::Struct(_)
                | BlockHeader::Enum(_)
                | BlockHeader::Trait(_)
                | BlockHeader::Implement(..)
                | BlockHeader::Default
        )
    }

    /// Returns true if the given header is a "branchable" block i.e. a block
    /// that can contain branch instructions like "break".
    fn is_branchable(&self, header: &BlockHeader) -> bool {
        matches!(
            header,
            BlockHeader::Match(_) | BlockHeader::For(_, _) | BlockHeader::While(_)
        )
    }

    /// Sets the information about if a statement exists in `block_info`.
    /// Also gathers information about "mod" and "use" statements for blocks.
    fn analyze_stmt(
        &mut self,
        ty_env: &TyEnv,
        ast_ctx: &mut AstCtx,
        stmt: &Stmt,
        id: usize,
        file_pos: FilePosition,
    ) -> LangResult<()> {
        let block_ctx = ast_ctx.block_ctxs.get_mut(&id).unwrap();

        match stmt {
            Stmt::Return(..) => block_ctx.contains_return = true,
            Stmt::Yield(..) => block_ctx.contains_yield = true,
            Stmt::Break(..) => block_ctx.contains_break = true,
            Stmt::Continue(..) => block_ctx.contains_continue = true,
            Stmt::Defer(..) => block_ctx.contains_defer = true,

            // Add the use to the current block. Also add it to the `self.uses`
            // so that it can be found from the child blocks as well.
            Stmt::Use(path) => {
                match self.uses.entry(block_ctx.block_id) {
                    Entry::Occupied(mut o) => {
                        o.get_mut().insert(ty_env, DerefType::None, path.clone())?;
                    }
                    Entry::Vacant(v) => {
                        let mut set = TyEnvHashSet::default();
                        set.insert(ty_env, DerefType::None, path.clone())?;
                        v.insert(set);
                    }
                }

                block_ctx.add_use(ty_env, path.clone());
            }

            // Module statements are only allowed in the default block (top
            // level of a file) and there can be max one statement per file.
            // It must also be the first statement in the file.
            Stmt::Module(path) => {
                if block_ctx.block_id != BlockCtx::DEFAULT_BLOCK_ID {
                    let err = LangError::new(
                        format!(
                            "Module statement found in non default block with ID {}. Stmt: {:#?}",
                            block_ctx.block_id, stmt
                        ),
                        LangErrorKind::AnalyzeError,
                        Some(file_pos),
                    );
                    return Err(err);
                } else if self.module.is_some() {
                    let err = LangError::new(
                        format!(
                            "Module already set for block with ID {}.\nPrevious mod: {:#?}, new: {:#?}",
                            block_ctx.block_id, self.module, stmt
                        ),
                        LangErrorKind::AnalyzeError,
                        Some(file_pos),
                    );
                    return Err(err);
                } else if !self.is_first_stmt {
                    let err = LangError::new(
                        format!(
                            "Module statement needs to be first statement in file (but wasn't). Stmt: {:#?}",
                            stmt
                        ),
                        LangErrorKind::AnalyzeError,
                        Some(file_pos),
                    );
                    return Err(err);
                }

                self.module = Some(path.clone());
            }

            Stmt::Assignment(..)
            | Stmt::VariableDecl(..)
            | Stmt::ExternalDecl(..)
            | Stmt::DeferExec(_) => (),
        }

        Ok(())
    }

    // TODO: Do this borrowing of analyze_context in a less ugly way.
    fn analyze_block(
        &mut self,
        ty_env: &TyEnv,
        ast_ctx: &mut AstCtx,
        block: &Block,
        parent_id: usize,
    ) -> LangResult<()> {
        let is_root_block = self.is_root(&block.header);
        let is_branchable_block = self.is_branchable(&block.header);

        // OBS! The "module" and "uses" set at this point might not be the
        //      final values. The module might not be set at this point (ex.
        //      if this is the first block being traversed in a new file) and
        //      the uses that is fetched with the `get_uses` only contains
        //      the "use"s that have been seen/traversed.
        //      These values might be updated when statements in this block
        //      is traversed further down in this function.
        let mut block_ctx = BlockCtx::new(
            block.id,
            block.file_pos.to_owned(),
            is_root_block,
            is_branchable_block,
            self.module.clone(),
            self.get_uses(ty_env, ast_ctx, parent_id)?,
        );

        if block.id != BlockCtx::DEFAULT_BLOCK_ID {
            block_ctx.parent_id = parent_id;
        }

        ast_ctx.block_ctxs.insert(block.id, block_ctx);

        // When iterating through all tokens, look at the If and Ifcases
        // and keep track if all their children contains return statements.
        //
        // The `all_paths_exhausted` is used to indicate that the given block
        // have child blocks that are "exhaustive". This will be used for
        // if and match-statements to see if they have a else/default block.
        // If they don't, we can't be sure that all paths are covered and we
        // have to make the assumption that we can't be sure that the block
        // will always branch even in situations when all its children
        // contain branch instructions. This might be an incorrect assumption,
        // but in those cases we reject valid programs rather than allow
        // potential invalid ones.
        //
        // All other blocks (other than "if" and "match") are assumed to be
        // exhaustive blocks since all their execution paths will always
        // be "followed"/ran.
        let mut all_children_contains_return = true;
        let mut all_paths_exhausted =
            !matches!(&block.header, BlockHeader::If | BlockHeader::Match(_));
        let mut child_count = 0;

        for child_token in block.body.iter() {
            // TODO: Fix ugly hack. If this token is a EOF, the `self.is_first_stmt`
            //       shouldn't be reset.
            let mut is_eof = false;

            match child_token {
                AstToken::Block(child_block) => {
                    match &child_block.header {
                        BlockHeader::If
                        | BlockHeader::IfCase(..)
                        | BlockHeader::Fn(..)
                        | BlockHeader::Match(..)
                        | BlockHeader::MatchCase(..)
                        | BlockHeader::For(..)
                        | BlockHeader::While(..)
                        | BlockHeader::Test(..)
                        | BlockHeader::Anonymous => {
                            self.analyze_block(ty_env, ast_ctx, child_block, block.id)?;

                            // True for "else" in a if-block and "default" in a match-block.
                            if matches!(
                                child_block.header,
                                BlockHeader::IfCase(None) | BlockHeader::MatchCase(None)
                            ) {
                                all_paths_exhausted = true;
                            }

                            if let Some(child_block_info) = ast_ctx.block_ctxs.get(&child_block.id)
                            {
                                if !child_block_info.all_children_contains_return {
                                    all_children_contains_return = false;
                                }
                            } else {
                                let err = ast_ctx.err(format!(
                                    "Unable to get block info for ID {} when in ID {}.",
                                    child_block.id, block.id
                                ));
                                return Err(err);
                            }
                            child_count += 1;
                        }

                        BlockHeader::Default
                        | BlockHeader::Struct(..)
                        | BlockHeader::Enum(..)
                        | BlockHeader::Union(..)
                        | BlockHeader::Trait(..)
                        | BlockHeader::Implement(..) => {
                            self.analyze_block(ty_env, ast_ctx, child_block, block.id)?;
                        }
                    }
                }

                AstToken::Stmt(ref stmt) => {
                    self.analyze_stmt(ty_env, ast_ctx, stmt, block.id, block.file_pos)?;
                }

                // Reset the "mod" statement when the end of the file is reached.
                AstToken::EOF => {
                    self.module = None;
                    self.is_first_stmt = true;
                    self.uses.remove(&BlockCtx::DEFAULT_BLOCK_ID);
                    is_eof = true;
                }

                AstToken::Empty | AstToken::Comment(..) | AstToken::Expr(_) => {
                    // Do nothing.
                }
            }

            if self.is_first_stmt && !is_eof {
                self.is_first_stmt = false;
            }
        }

        // Set the flag to indicate if all children contains return statements
        // for the current block. If this block doesn't have any children,
        // the value will be set to true if this block itself contains a return.
        let block_info = ast_ctx.block_ctxs.get_mut(&block.id).unwrap();
        block_info.all_children_contains_return = if child_count > 0 {
            all_children_contains_return && all_paths_exhausted
        } else {
            block_info.contains_return
        };

        Ok(())
    }
}

impl Visitor for BlockAnalyzer {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    /// All traversing is done from the default block, no other visit function
    /// will be used. The reason being that this needs to be called recursively
    /// on blocks, which currently isn't possible to do with the regular traverser.
    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        if let Err(err) =
            self.analyze_block(&ctx.ty_env.lock(), &mut ctx.ast_ctx, block, usize::MAX)
        {
            self.errors.push(err);
        }
    }

    fn visit_end(&mut self, ctx: &mut TraverseCtx) {
        if log_enabled!(Level::Debug) {
            debug!("BLOCK_CTX --\n{:#?}", ctx.ast_ctx.block_ctxs);
        }
    }
}
