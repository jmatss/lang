use common::{
    error::{LangError, LangErrorKind, LangResult},
    ty::ty::Ty,
};
use log::debug;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Debug,
};

type NodeId = usize;

#[derive(Debug, Clone)]
struct Node {
    /// The unique ID of the current node.
    id: NodeId,

    /// The unique ID of this current nodes parent. If this node doesn't have a
    /// parent (i.e. it is the root node), this will be set ty the same as `id`.
    parent_id: NodeId,

    /// The type that this node represents.
    ty: Ty,
}

/// Used to infer types. Every set represents types that in some way interact
/// with each other in the source code. The root in all sets will be the node
/// that contains the preferred type of all the nodes in the set.
///
/// At the end of the inference step, one can fetch the type that is to be used
/// from the root node of the sets.
#[derive(Clone)]
pub struct SubstitutionSets {
    ty_to_id: HashMap<Ty, NodeId>,
    id_to_node: HashMap<NodeId, Node>,

    /// Contains a unique ID that will be given to a newly created `Node`.
    /// It will be incremented after every new node to make sure that it is uniqe.
    node_id: NodeId,
}

impl SubstitutionSets {
    pub fn new() -> Self {
        Self {
            ty_to_id: HashMap::default(),
            id_to_node: HashMap::default(),
            node_id: 0,
        }
    }

    fn new_node(&mut self, ty: &Ty) -> NodeId {
        let id = self.node_id;
        self.node_id += 1;

        let new_node = Node {
            id,
            parent_id: id,
            ty: ty.clone(),
        };

        self.ty_to_id.insert(ty.clone(), id);
        self.id_to_node.insert(id, new_node);

        id
    }

    /// Returns the type contained in the root node of the set that contains the
    /// type `ty`. This will be the type with the highest precedence in the set.
    ///
    /// If the given `ty` doesn't exist in any set, returns the given `ty` itself.
    pub fn inferred_type(&self, ty: &Ty) -> LangResult<Ty> {
        if let Some(id) = self.ty_to_id.get(ty) {
            let root_id = self.find_root(*id).unwrap();
            let root_node = self.id_to_node.get(&root_id).unwrap();

            Ok(root_node.ty.clone())
        } else {
            Ok(ty.clone())
        }
    }

    /// Creates a union of the two sets containing the types `ty_a` and `ty_b`.
    /// If any of the given types doesn't exist, they will be created and inserted
    /// into the sets.
    ///
    /// The types contained in the roots for both sets will be compared to each
    /// other and the root containing the preferred type will be chosen as the
    /// new root for the union of the sets. If both types are in the same set
    /// already, this function will do nothing.
    ///
    /// Returns the type of the new root node of the unioned sets. This will be
    /// the type with the highest precedence in the set.
    pub fn union(&mut self, ty_a: &Ty, ty_b: &Ty) -> LangResult<Ty> {
        compatible(ty_a, ty_b)?;

        let id_a = if let Some(id) = self.ty_to_id.get(ty_a) {
            *id
        } else {
            self.new_node(ty_a)
        };

        let id_b = if let Some(id) = self.ty_to_id.get(ty_b) {
            *id
        } else {
            self.new_node(ty_b)
        };

        let root_id_a = self.find_root(id_a).unwrap();
        let root_id_b = self.find_root(id_b).unwrap();

        debug!(
            "Creating union of root_id_a: {}, root_id_b: {} -- ty_a: {:#?}, ty_b: {:#?}",
            root_id_a, root_id_b, &ty_a, &ty_b
        );

        // TODO: Balancing.
        let new_root_id = if root_id_a != root_id_b {
            let new_root_id_a = self.infer_root(root_id_a, id_a)?;
            let new_root_id_b = self.infer_root(root_id_b, id_b)?;

            self.infer_root_with_start(new_root_id_a, new_root_id_b, ty_a, ty_b)?
        } else {
            // Both types are in the same set with the same root. Changing the
            // root for `a` will then affect the root for `b`, so need to take
            // that into consideration.
            let new_root_id = self.infer_root(root_id_a, id_a)?;
            self.infer_root(new_root_id, id_b)?
        };

        Ok(self.id_to_node.get(&new_root_id).unwrap().ty.clone())
    }

    /// See `infer_root_with_start()`.
    fn infer_root(&mut self, id_a: NodeId, id_b: NodeId) -> LangResult<NodeId> {
        let ty_a = self.id_to_node.get(&id_a).unwrap().ty.clone();
        let ty_b = self.id_to_node.get(&id_b).unwrap().ty.clone();
        self.infer_root_with_start(id_a, id_b, &ty_a, &ty_b)
    }

    /// Given two node ids `id_a` and `id_b`, figures out which of the two nodes
    /// which have the preferred types and sets it as the new parent of the other
    /// node. The node id of the node that is chosen as the new root is returned.
    ///
    /// This function can called when either of these two requirements holds:
    ///   1. The two nodes are already a part of the same set.
    ///   2. The two nodes are roots of their respective sets.
    ///
    /// `start_ty_a` and `start_ty_a` are the "original" types that was used as
    /// arguments to the `union` function. They are used for error messages only,
    /// so that the error messages can contain some more information about how
    /// the types "relates" to each other.
    fn infer_root_with_start(
        &mut self,
        id_a: NodeId,
        id_b: NodeId,
        start_ty_a: &Ty,
        start_ty_b: &Ty,
    ) -> LangResult<NodeId> {
        let ty_a = self.id_to_node.get(&id_a).unwrap().ty.clone();
        let ty_b = self.id_to_node.get(&id_b).unwrap().ty.clone();

        compatible_with_start(&ty_a, &ty_b, start_ty_a, start_ty_b)?;

        let new_root_id = if ty_a.precedence(&ty_b) { id_a } else { id_b };

        if let Some(node_a) = self.id_to_node.get_mut(&id_a) {
            node_a.parent_id = new_root_id;
        }
        if let Some(node_b) = self.id_to_node.get_mut(&id_b) {
            node_b.parent_id = new_root_id;
        }

        Ok(new_root_id)
    }

    /// Returns the root ID of the node with the ID `id`.
    fn find_root(&self, id: NodeId) -> Option<NodeId> {
        if let Some(node) = self.id_to_node.get(&id) {
            if node.id == node.parent_id {
                Some(node.id)
            } else {
                self.find_root(node.parent_id)
            }
        } else {
            None
        }
    }

    pub fn iter_types(&self) -> Vec<Ty> {
        self.ty_to_id.keys().cloned().collect::<Vec<_>>()
    }
}

impl Debug for SubstitutionSets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut root_to_children: HashMap<NodeId, HashSet<NodeId>> = HashMap::new();

        for id in self.id_to_node.keys() {
            let root_id = self.find_root(*id).unwrap();

            if *id == root_id {
                continue;
            }

            match root_to_children.entry(root_id) {
                Entry::Occupied(mut o) => {
                    o.get_mut().insert(*id);
                }
                Entry::Vacant(v) => {
                    let mut s = HashSet::default();
                    s.insert(*id);
                    v.insert(s);
                }
            }
        }

        for (root_id, children_ids) in root_to_children {
            writeln!(f, "============")?;
            write!(f, "+ root: {}\nchildren:", root_id)?;
            for child_id in children_ids.iter() {
                write!(f, " {}", child_id)?;
            }

            let root_node = self.id_to_node.get(&root_id).unwrap();
            let root_parent = root_node.parent_id;
            let root_ty = &root_node.ty;
            writeln!(
                f,
                "\nroot id: {}, parent: {}, ty: {:#?}",
                root_id, root_parent, root_ty
            )?;

            for child_id in children_ids.iter() {
                let child_node = self.id_to_node.get(child_id).unwrap();
                let child_parent = child_node.parent_id;
                let child_ty = &child_node.ty;
                writeln!(
                    f,
                    "- id: {}, parent: {}, ty: {:#?}",
                    child_id, child_parent, child_ty
                )?;
            }
        }

        Ok(())
    }
}

/// See `compatible_with_start()`.
fn compatible(ty_a: &Ty, ty_b: &Ty) -> LangResult<()> {
    compatible_with_start(ty_a, ty_b, ty_a, ty_b)
}

/// Checks if the types `ty_a` and `ty_b` are compatible and tries to put together
/// a decent error message. `start_ty_a` and `start_ty_b` are only used in the
/// case where a error happens to add some context to the error.
fn compatible_with_start(ty_a: &Ty, ty_b: &Ty, start_ty_a: &Ty, start_ty_b: &Ty) -> LangResult<()> {
    if ty_a.is_compatible(ty_b) {
        Ok(())
    } else {
        let mut msg = format!(
            "Found unsolvable type constraint. Tried to map the types:\n  1. {:#?}\n  2. {:#?}",
            ty_a, ty_b,
        );

        if ty_a != start_ty_a {
            msg.push_str(&format!(
                "\nThe first type (1) was inferred from the type: {:#?}",
                start_ty_a
            ));
        }

        if ty_b != start_ty_b {
            msg.push_str(&format!(
                "\nThe second type (2) was inferred from the type: {:#?}",
                start_ty_b
            ));
        }

        Err(LangError::new(msg, LangErrorKind::AnalyzeError, None))
    }
}
