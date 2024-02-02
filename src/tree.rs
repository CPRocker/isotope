// arena allocated tree adapted from the following sources
//  https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6
//  https://sachanganesh.com/programming/graph-tree-traversals-in-rust/

pub type TreeIndex = usize;

#[derive(Debug, Default)]
pub struct Tree<T> {
    arena: Vec<Option<TreeNode<T>>>,
    root: Option<TreeIndex>,
}

#[derive(Debug)]
pub struct TreeNode<T> {
    value: T,
    parent: Option<TreeIndex>,
    children: Vec<TreeIndex>,
}

impl<T> Tree<T> {
    pub fn set_root(&mut self, root: Option<TreeIndex>) {
        self.root = root;
    }

    pub fn add_child(&mut self, child: T, parent: TreeIndex) -> TreeIndex {
        let child_index = self.new_node(child, Some(parent));
        self.arena[parent].unwrap().children.push(child_index);
        child_index
    }

    fn new_node(&mut self, value: T, parent: Option<TreeIndex>) -> TreeIndex {
        let index = self.arena.len();
        self.arena.push(Some(TreeNode::new(value, parent)));
        index
    }

    pub fn node_at(&self, index: TreeIndex) -> Option<&TreeNode<T>> {
        if let Some(node) = self.arena.get(index) {
            node.as_ref()
        } else {
            None
        }
    }

    pub fn node_at_mut(&mut self, index: TreeIndex) -> Option<&mut TreeNode<T>> {
        if let Some(node) = self.arena.get_mut(index) {
            node.as_mut()
        } else {
            None
        }
    }
}

impl<T> TreeNode<T> {
    fn new(value: T, parent: Option<TreeIndex>) -> Self {
        Self {
            value,
            parent,
            children: vec![],
        }
    }
}
