use crate::{file::FilePosition, BlockId};

use super::{ast_ctx::AstCtx, ty_ctx::TyCtx};

/// Will contain context used during traversing of the AST.
/// This will be given to all "visit_...()" functions so that the visitor can
/// access it during traversing.
#[derive(Debug)]
pub struct TraverseCtx<'a> {
    pub ast_ctx: &'a mut AstCtx,
    pub ty_ctx: &'a mut TyCtx,

    /// Indicates if any found shared references (ex. RefCount) should be "deep"
    /// copied before the traverser visits it. This comes in handy when working
    /// with generics for example, where you want to make transformations to
    /// different instances.
    pub deep_copy: bool,

    /// A number that is used to mark copies so that they can be uniquely identified.
    /// This will only be used if `deep_copy` is set to true.
    pub copy_nr: Option<usize>,

    pub block_id: BlockId,

    // TODO: Should this contains file information about the parent as well?
    //       Ex. if this is a type, should the information about what this type
    //       is assigned to also be here?
    pub file_pos: FilePosition,

    /// Flag to indicate if the traverse should stop traversing. This will be
    /// set from one of the visitors and this traverser will check this flag
    /// before every new visit.
    pub stop: bool,
}
