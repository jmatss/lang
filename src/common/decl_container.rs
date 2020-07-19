use super::decl_hashmap_tmp::DeclHashMap;
use crate::parse::token::BlockId;
use inkwell::values::PointerValue;

// TODO: Block id and scope are currently ignore -- Implement!

/// Used during parsing/code gen to keep track of all declarations of items.
#[derive(Debug, Clone, PartialEq)]
pub struct DeclContainer<'ctx, I> {
    /// Contains all the declarations mapped by ident and block id.
    declarations: DeclHashMap<I>,

    /// ~One-to-one mapping to the `declarations` map. Will contain the
    /// pointers produced during LLVM code gen that represents the items
    /// in the `declarations`.
    pointers: DeclHashMap<PointerValue<'ctx>>,
}

impl<'ctx, I> DeclContainer<'ctx, I> {
    pub fn new() -> Self {
        Self {
            declarations: DeclHashMap::new(),
            pointers: DeclHashMap::new(),
        }
    }

    pub fn insert(&mut self, ident: &str, id: BlockId, item: I) {
        self.declarations.insert(ident, id, item)
    }

    // TODO: Will be implemented when codegen is implemented.
    #[allow(dead_code)]
    pub fn insert_ptr(&mut self, ident: &str, id: BlockId, ptr: PointerValue<'ctx>) {
        self.pointers.insert(ident, id, ptr)
    }

    pub fn get(&self, ident: &str, id: BlockId) -> Option<&I> {
        self.declarations.get(ident, id)
    }

    // TODO: Will be implemented when codegen is implemented.
    #[allow(dead_code)]
    pub fn get_ptr(&self, ident: &str, id: BlockId) -> Option<&PointerValue<'ctx>> {
        self.pointers.get(ident, id)
    }

    pub fn get_mut(&mut self, ident: &str, id: BlockId) -> Option<&mut I> {
        self.declarations.get_mut(ident, id)
    }

    // TODO: Will be implemented when codegen is implemented.
    #[allow(dead_code)]
    pub fn get_ptr_mut(&mut self, ident: &str, id: BlockId) -> Option<&mut PointerValue<'ctx>> {
        self.pointers.get_mut(ident, id)
    }
}
