use std::sync::Mutex;

use crate::{ctx::ast_ctx::AstCtx, file::FilePosition, ty::ty_env::TyEnv, BlockId};

/// Will contain context used during traversing of the AST.
/// This will be given to all "visit_...()" functions so that the visitor can
/// access it during traversing.
#[derive(Debug)]
pub struct TraverseCtx<'a> {
    pub ast_ctx: &'a mut AstCtx,
    pub ty_env: &'a Mutex<TyEnv>,

    /// "deep copy" indicates if any found shared references (ex. RefCount) should
    /// be "deep" copied before the traverser visits it. This comes in handy when
    /// working with generics for example, where you want to make transformations
    /// to different instances.
    ///
    /// If this values is set to Some, a "deep copy" will be done. It will not
    /// be done if it is set to None. The number will be used as a unique
    /// identifier to make the copies unique.
    pub copy_nr: Option<usize>,

    pub block_id: BlockId,

    /// Flag to indicate if the traverse should stop traversing. This will be
    /// set from one of the visitors and this traverser will check this flag
    /// before every new visit.
    pub stop: bool,
}

impl<'a> TraverseCtx<'a> {
    pub fn new(ast_ctx: &'a mut AstCtx, ty_env: &'a Mutex<TyEnv>) -> Self {
        Self {
            ast_ctx,
            ty_env,
            copy_nr: None,
            block_id: 0,
            stop: false,
        }
    }

    /// Resets the traver context to its "default" state.
    pub fn reset(&mut self) {
        self.copy_nr = None;
        self.block_id = 0;
        *self.file_pos_mut() = FilePosition::default();
        self.stop = false;
    }

    pub fn set_copy_nr(&mut self, copy_nr: usize) -> &mut Self {
        self.copy_nr = Some(copy_nr);
        self
    }

    pub fn clear_copy_nr(&mut self) -> &mut Self {
        self.copy_nr = None;
        self
    }

    pub fn file_pos(&self) -> FilePosition {
        self.ast_ctx.file_pos
    }

    pub fn file_pos_mut(&mut self) -> &mut FilePosition {
        &mut self.ast_ctx.file_pos
    }
}
