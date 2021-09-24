use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap, HashSet},
};

use crate::{
    error::{LangError, LangErrorKind, LangResult},
    ty::{get::get_file_pos, to_string::to_string_type_id},
    TypeId,
};

use super::{
    is::{is_compatible, is_solved},
    precedence::precedence,
    ty::SolveCond,
    ty_env::TyEnv,
};

type NodeId = usize;

#[derive(Debug, Clone)]
pub struct Node {
    /// The unique ID of the current node.
    pub id: NodeId,

    /// The unique ID of this current nodes parent. If this node doesn't have a
    /// parent (i.e. it is the root node), this will be set ty the same as `id`.
    pub parent_id: NodeId,

    /// The type that this node represents.
    pub type_id: TypeId,
}

/// Used to infer types. Every set represents types that in some way interact
/// with each other in the source code. The root in all sets will be the node
/// that contains the preferred type of all the nodes in the set.
///
/// At the end of the inference step, one can fetch the type that is to be used
/// from the root node of the sets.
#[derive(Clone, Debug)]
pub struct SubstitutionSets {
    pub ty_to_id: HashMap<TypeId, NodeId>,
    pub id_to_node: HashMap<NodeId, Node>,

    /// Contains a unique ID that will be given to a newly created `Node`.
    /// It will be incremented after every new node to make sure that it is uniqe.
    node_id: NodeId,
}

impl Default for SubstitutionSets {
    fn default() -> Self {
        Self {
            ty_to_id: HashMap::default(),
            id_to_node: HashMap::default(),
            node_id: 0,
        }
    }
}

impl SubstitutionSets {
    /// Returns the root ID of the node with the ID `id`.
    pub(super) fn find_root(&self, node_id: NodeId) -> Option<NodeId> {
        if let Some(node) = self.id_to_node.get(&node_id) {
            if node.id == node.parent_id {
                Some(node.id)
            } else {
                self.find_root(node.parent_id)
            }
        } else {
            None
        }
    }

    fn new_node(&mut self, type_id: TypeId) -> NodeId {
        let id = self.node_id;
        self.node_id += 1;

        let new_node = Node {
            id,
            parent_id: id,
            type_id,
        };

        self.ty_to_id.insert(type_id, id);
        self.id_to_node.insert(id, new_node);

        id
    }
}

// Warning incorrectly triggered in the `new_root_id` if-block. Can be removed
// once CLippy fixes the problem.
#[allow(clippy::branches_sharing_code)]
/// Creates a union of the two sets containing the types `type_id_a` and `type_id_b`.
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
pub fn union(ty_env: &mut TyEnv, type_id_a: TypeId, type_id_b: TypeId) -> LangResult<TypeId> {
    compatible(ty_env, type_id_a, type_id_b)?;

    let id_a = if let Some(id) = ty_env.sub_sets.ty_to_id.get(&type_id_a) {
        *id
    } else {
        ty_env.sub_sets.new_node(type_id_a)
    };

    let id_b = if let Some(id) = ty_env.sub_sets.ty_to_id.get(&type_id_b) {
        *id
    } else {
        ty_env.sub_sets.new_node(type_id_b)
    };

    let root_id_a = ty_env.sub_sets.find_root(id_a).unwrap();
    let root_id_b = ty_env.sub_sets.find_root(id_b).unwrap();

    debug!(
        "Creating union of root_id_a: {}, root_id_b: {} -- type_id_a: {}, type_id_b: {}",
        root_id_a, root_id_b, &type_id_a, &type_id_b
    );

    // TODO: Balancing.
    let new_root_id = if root_id_a != root_id_b {
        let new_root_id_a = infer_root(ty_env, root_id_a, id_a)?;
        let new_root_id_b = infer_root(ty_env, root_id_b, id_b)?;

        infer_root_with_start(ty_env, new_root_id_a, new_root_id_b, type_id_a, type_id_b)?
    } else {
        // Both types are in the same set with the same root. Changing the
        // root for `a` will then affect the root for `b`, so need to take
        // that into consideration.
        let new_root_id = infer_root(ty_env, root_id_a, id_a)?;
        infer_root(ty_env, new_root_id, id_b)?
    };

    Ok(ty_env
        .sub_sets
        .id_to_node
        .get(&new_root_id)
        .unwrap()
        .type_id)
}

/// Given a type with type ID `type_id`, sees if it should be "promoted" to
/// become the new root of its set. This will happen if this type is fully
/// solvable while the current root isn't.
/// Returns the type ID of the new root type.
pub fn promote(ty_env: &mut TyEnv, type_id: TypeId) -> LangResult<TypeId> {
    let check_inf = true;
    let solve_cond = SolveCond::new().excl_unknown();
    let is_solved = is_solved(ty_env, type_id, check_inf, solve_cond).unwrap_or(false);
    let is_in_ty_env = ty_env.sub_sets.ty_to_id.contains_key(&type_id);

    if is_solved && is_in_ty_env {
        let node_id = *ty_env.sub_sets.ty_to_id.get(&type_id).unwrap();
        let root_node_id = ty_env.sub_sets.find_root(node_id).unwrap();

        if node_id == root_node_id {
            return Ok(type_id);
        }

        let new_node_id = infer_root(ty_env, node_id, root_node_id)?;
        Ok(ty_env
            .sub_sets
            .id_to_node
            .get(&new_node_id)
            .unwrap()
            .type_id)
    } else {
        ty_env.inferred_type(type_id)
    }
}

/// See `infer_root_with_start()`.
fn infer_root(ty_env: &mut TyEnv, id_a: NodeId, id_b: NodeId) -> LangResult<NodeId> {
    let type_id_a = ty_env.sub_sets.id_to_node.get(&id_a).unwrap().type_id;
    let type_id_b = ty_env.sub_sets.id_to_node.get(&id_b).unwrap().type_id;
    infer_root_with_start(ty_env, id_a, id_b, type_id_a, type_id_b)
}

/// Given two node ids `id_a` and `id_b`, figures out which of the two nodes
/// which have the preferred type and sets it as the new parent of the other
/// node. The node id of the node that is chosen as the new root is returned.
///
/// This function can called when either of these two requirements holds:
///   1. The two nodes are already a part of the same set.
///   2. The two nodes are roots of their respective sets.
///
/// `start_type_id_a` and `start_type_id_b` are the "original" types that was
/// used as arguments to the `union` function. They are used for error messages
/// only, so that the error messages can contain some more information about
/// how the types "relates" to each other.
fn infer_root_with_start(
    ty_env: &mut TyEnv,
    id_a: NodeId,
    id_b: NodeId,
    start_type_id_a: TypeId,
    start_type_id_b: TypeId,
) -> LangResult<NodeId> {
    let type_id_a = ty_env.sub_sets.id_to_node.get(&id_a).unwrap().type_id;
    let type_id_b = ty_env.sub_sets.id_to_node.get(&id_b).unwrap().type_id;

    compatible_with_start(
        ty_env,
        type_id_a,
        type_id_b,
        start_type_id_a,
        start_type_id_b,
    )?;

    let new_root_id = match precedence(ty_env, type_id_a, type_id_b)? {
        Ordering::Less => id_a,
        Ordering::Greater => id_b,
        Ordering::Equal => {
            unreachable!("type_id_a: {}, type_id_b: {}", type_id_a, type_id_a)
        }
    };

    if let Some(node_a) = ty_env.sub_sets.id_to_node.get_mut(&id_a) {
        node_a.parent_id = new_root_id;
    }
    if let Some(node_b) = ty_env.sub_sets.id_to_node.get_mut(&id_b) {
        node_b.parent_id = new_root_id;
    }

    Ok(new_root_id)
}

/// See `compatible_with_start()`.
fn compatible(ty_env: &TyEnv, type_id_a: TypeId, type_id_b: TypeId) -> LangResult<()> {
    compatible_with_start(ty_env, type_id_a, type_id_b, type_id_a, type_id_b)
}

/// Checks if the types `type_id_a` and `type_id_b` are compatible and tries to
/// put together a decent error message. `start_type_id_a` and `start_type_id_b`
/// are only used in the case where a error happens to add some context to the error.
fn compatible_with_start(
    ty_env: &TyEnv,
    type_id_a: TypeId,
    type_id_b: TypeId,
    start_type_id_a: TypeId,
    start_type_id_b: TypeId,
) -> LangResult<()> {
    if is_compatible(ty_env, type_id_a, type_id_b)? {
        Ok(())
    } else {
        let mut msg = format!(
            "Found unsolvable type constraint. Tried to map the types:\n  \
            1. {} ({} -- {:?})\n  2. {} ({} -- {:?})",
            to_string_type_id(ty_env, type_id_a)?,
            type_id_a,
            get_file_pos(ty_env, type_id_a),
            to_string_type_id(ty_env, type_id_b)?,
            type_id_b,
            get_file_pos(ty_env, type_id_b),
        );

        if type_id_a != start_type_id_a {
            msg.push_str(&format!(
                "\nThe first type (1) was inferred from the type: {} ({} -- {:?})",
                to_string_type_id(ty_env, start_type_id_a)?,
                start_type_id_a,
                get_file_pos(ty_env, start_type_id_a),
            ));
        }

        if type_id_b != start_type_id_b {
            msg.push_str(&format!(
                "\nThe second type (2) was inferred from the type: {} ({} -- {:?})",
                to_string_type_id(ty_env, start_type_id_b)?,
                start_type_id_b,
                get_file_pos(ty_env, start_type_id_b),
            ));
        }

        Err(LangError::new(msg, LangErrorKind::AnalyzeError, None))
    }
}

pub fn sub_sets_debug_print(ty_env: &TyEnv) -> String {
    let mut root_to_children: HashMap<NodeId, HashSet<NodeId>> = HashMap::new();

    for id in ty_env.sub_sets.id_to_node.keys() {
        let root_id = ty_env.sub_sets.find_root(*id).unwrap();

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

    let mut s = String::new();

    s.push_str("sets:\n");
    for (root_id, children_ids) in root_to_children {
        s.push_str("============\n");
        s.push_str(&format!("+ root: {}\nchildren:\n", root_id));
        for child_id in children_ids.iter() {
            s.push_str(&format!(" {}\n", child_id));
        }

        let root_node = ty_env.sub_sets.id_to_node.get(&root_id).unwrap();
        let root_parent = root_node.parent_id;
        let root_type_id = &root_node.type_id;
        s.push_str(&format!(
            "\nroot node_id: {}, parent: {}, type_id: {}, ty: {:#?}\n",
            root_id,
            root_parent,
            root_type_id,
            ty_env.ty(*root_type_id)
        ));

        for child_id in children_ids.iter() {
            let child_node = ty_env.sub_sets.id_to_node.get(child_id).unwrap();
            let child_parent = child_node.parent_id;
            let child_type_id = &child_node.type_id;
            s.push_str(&format!(
                "- node_id: {}, parent: {}, type_id: {}, ty: {:#?}\n",
                child_id,
                child_parent,
                child_type_id,
                ty_env.ty(*child_type_id)
            ));
        }
    }

    s
}
