use common::{ty::ty::Ty, BlockId};
use log::debug;
use std::collections::{HashMap, HashSet};

/// Contains constraints between types. These sets will be used to check and
/// infer the types.
#[derive(Debug)]
pub struct ConstraintSets {
    /// The block root ID which these sets represents. This is used for debug
    /// printing only.
    root_id: BlockId,

    ty_to_id: HashMap<Ty, usize>,
    id_to_node: HashMap<usize, ConstraintNode>,

    /// A list of all current parents. Will make it easy to find all parent when
    /// it is time to iterate through all sets to solve the types.
    parent_ids: HashSet<usize>,

    /// Contains the ID that will be assigned to a newly created node. When the
    /// new node has been assigned this ID, it will be incremented so that the
    /// next node gets a new, unique, ID.
    cur_id: usize,
}

#[derive(Debug)]
struct ConstraintNode {
    id: usize,
    parent_id: usize,
    /// Keeps track of all the children for this node so that it is easy to
    /// iterate over the types when it is time to solve them.
    child_ids: HashSet<usize>,
    ty: Ty,
}

// TODO: Balancing of the tree.

impl ConstraintSets {
    /// Creates a new `ConstraintSets` for the root block with ID `root_id`.
    pub fn new(root_id: BlockId) -> Self {
        Self {
            root_id,
            ty_to_id: HashMap::default(),
            id_to_node: HashMap::default(),
            parent_ids: HashSet::default(),
            cur_id: 0,
        }
    }

    /// Creates a union of the two sets containing the types `ty_a` and `ty_b`.
    /// If any of the given types doesn't exist, they will be created and inserted
    /// into the sets. If both types are in the same set already, this function
    /// will do nothing.
    pub fn union(&mut self, ty_a: &Ty, ty_b: &Ty) {
        let parent_id_a = if let Some(parent_id_a) = self.find_parent(ty_a) {
            parent_id_a
        } else {
            self.new_node(ty_a)
        };

        let parent_id_b = if let Some(parent_id_b) = self.find_parent(ty_b) {
            parent_id_b
        } else {
            self.new_node(ty_b)
        };

        debug!(
            "Inserting constraint in scope ID {}. parent_a: {}, parent_b: {} -- lhs: {:?}, rhs: {:?}",
            self.root_id, parent_id_a, parent_id_b, &ty_a, &ty_b
        );

        // TODO: Balancing. Currently `b` is always inserted under `a`.
        // Update both children for `a` and parent for `b`.
        if parent_id_a != parent_id_b {
            if let Some(parent_node_a) = self.id_to_node.get_mut(&parent_id_a) {
                self.parent_ids.insert(parent_id_a);
                parent_node_a.child_ids.insert(parent_id_b);
            }
            if let Some(parent_node_b) = self.id_to_node.get_mut(&parent_id_b) {
                self.parent_ids.remove(&parent_id_b);
                parent_node_b.parent_id = parent_id_a;
            }
        }
    }

    /// Returns a nested list of all types stored in these constarint sets.
    pub fn constraint_sets(&self) -> Vec<Vec<Ty>> {
        let mut sets = Vec::default();

        debug!(
            "ty_to_id: {:#?}\nid_to_node: {:#?}\nparent_ids: {:#?}",
            self.ty_to_id, self.id_to_node, self.parent_ids
        );

        for parent_id in &self.parent_ids {
            sets.push(self.constraint_set(*parent_id));
        }

        sets
    }

    /// Gather all types, starting from the node with ID `id`, and going through
    /// all its children.
    fn constraint_set(&self, id: usize) -> Vec<Ty> {
        let mut types = Vec::default();

        if let Some(node) = self.id_to_node.get(&id) {
            types.push(node.ty.clone());

            for child_id in &node.child_ids {
                types.append(&mut self.constraint_set(*child_id));
            }
        }

        types
    }

    /// Creates a new node for the given type `ty`, insert it into the sets and
    /// returns the parent ID.
    fn new_node(&mut self, ty: &Ty) -> usize {
        let id = self.cur_id;
        self.cur_id += 1;

        let new_node = ConstraintNode {
            id,
            parent_id: id,
            child_ids: HashSet::default(),
            ty: ty.clone(),
        };

        self.ty_to_id.insert(ty.clone(), id);
        self.id_to_node.insert(id, new_node);

        id
    }

    /// Returns the parent ID of the given type `ty`.
    fn find_parent(&self, ty: &Ty) -> Option<usize> {
        let id = self.ty_to_id.get(ty)?;
        self.find_with_id(*id)
    }

    /// Returns the parent ID of the node with the ID `id`.
    fn find_with_id(&self, id: usize) -> Option<usize> {
        if let Some(node) = self.id_to_node.get(&id) {
            if node.id == node.parent_id {
                Some(node.id)
            } else {
                self.find_with_id(node.parent_id)
            }
        } else {
            None
        }
    }
}
