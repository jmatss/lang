use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use parking_lot::RwLock;

use common::{
    ctx::block_ctx::BlockCtx,
    error::{LangError, LangResult},
    file::FilePosition,
    hash::DerefType,
    hash_set::TyEnvHashSet,
    path::LangPath,
    token::{
        ast::AstToken,
        block::{Adt, AdtKind, Block, BlockHeader},
        expr::{Var, VarType},
    },
    traverse::{traverse_ctx::TraverseCtx, visitor::Visitor},
    ty::{inner_ty::InnerTy, to_string::to_string_type_id, ty::Ty, type_id::TypeId},
};

/// Creates new declarations of all tuples found in the given `tuple_types`.
/// The tuples will be represented as struct with the name `Tuple`.
/// The new tuple structs are inserted into both the AST and the look-ups.
pub struct TupleCreator {
    tuple_types: HashSet<TypeId>,
    errors: Vec<LangError>,
}

impl TupleCreator {
    pub fn new(tuple_types: HashSet<TypeId>) -> Self {
        Self {
            tuple_types,
            errors: Vec::default(),
        }
    }

    fn create_tuples(&self, ctx: &mut TraverseCtx, body: &mut Vec<AstToken>) -> LangResult<()> {
        // Will contain the string representation of all created tuples.
        // This will be used to ensure that no duplicates are created.
        let mut created_tuples = HashSet::new();

        let ty_env_guard = ctx.ty_env.lock();

        for type_id in &self.tuple_types {
            let tuple_name = to_string_type_id(&ty_env_guard, *type_id)?;
            if created_tuples.contains(&tuple_name) {
                continue;
            } else {
                created_tuples.insert(tuple_name);
            }

            let ty = ty_env_guard.ty(*type_id)?;
            let path = if let Ty::CompoundType(InnerTy::Tuple(path), ..) = ty {
                path
            } else {
                unreachable!(
                    "Not tuple type when creating tuple: {:#?} ({})",
                    ty, type_id
                )
            };

            let members = if let Some(gens) = path.gens() {
                let mut members = Vec::with_capacity(gens.len());
                for (i, member_type_id) in gens.iter_types().enumerate() {
                    let name = format!("tuple_member_{}", i);
                    let var = Var::new(
                        name,
                        Some(*member_type_id),
                        None,
                        None,
                        None,
                        None,
                        VarType::Unknown,
                    );
                    members.push(Arc::new(RwLock::new(var)));
                }
                members
            } else {
                Vec::with_capacity(0)
            };

            let struct_ = Arc::new(RwLock::new(Adt {
                name: "Tuple".into(),
                module: LangPath::empty(),
                modifiers: Vec::with_capacity(0),
                file_pos: FilePosition::new(0, 0, 0, 0),
                has_definition: true,
                methods: HashMap::with_capacity(0),
                members,
                kind: AdtKind::Tuple,
                generics: path.gens().cloned(),
                implements: None,
                implemented_traits: TyEnvHashSet::default(),
                enum_ty: None,
            }));

            // Use the default block ID. This is technically not correct to do,
            // but it should not cause any problems (might in the future).
            let block_id = BlockCtx::DEFAULT_BLOCK_ID;
            let ast_token = AstToken::Block(Block {
                header: BlockHeader::Struct(Arc::clone(&struct_)),
                body: Vec::with_capacity(0),
                id: block_id,
                file_pos: FilePosition::new(0, 0, 0, 0),
            });

            body.push(ast_token);

            let key = (path.clone(), block_id);
            ctx.ast_ctx
                .adts
                .insert(&ty_env_guard, DerefType::Deep, key, Arc::clone(&struct_))?;
        }
        Ok(())
    }
}

impl Visitor for TupleCreator {
    fn take_errors(&mut self, _ctx: &mut TraverseCtx) -> Option<Vec<LangError>> {
        if self.errors.is_empty() {
            None
        } else {
            Some(std::mem::take(&mut self.errors))
        }
    }

    fn visit_default_block(&mut self, block: &mut Block, ctx: &mut TraverseCtx) {
        let Block { body, .. } = block;
        if let Err(err) = self.create_tuples(ctx, body) {
            self.errors.push(err);
        }
    }
}
