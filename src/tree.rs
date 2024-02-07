// arena allocated tree adapted from the following sources
//  https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6
//  https://sachanganesh.com/programming/graph-tree-traversals-in-rust/

pub type TreeIndex = usize;

#[derive(Debug)]
pub struct TreeNode<T> {
    value: T,
    parent: Option<TreeIndex>,
    children: Vec<TreeIndex>,
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

#[derive(Debug, Default)]
pub struct Tree<T> {
    arena: Vec<Option<TreeNode<T>>>,
    root: Option<TreeIndex>,
}

impl<T> Tree<T> {
    pub fn set_root(&mut self, root: Option<TreeIndex>) {
        self.root = root;
    }

    pub fn add_child(&mut self, child: T, parent: TreeIndex) -> TreeIndex {
        let child_index = self.new_node(child, Some(parent));
        self.arena[parent]
            .as_mut()
            .unwrap()
            .children
            .push(child_index);
        child_index
    }

    pub fn new_node(&mut self, value: T, parent: Option<TreeIndex>) -> TreeIndex {
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

    pub fn iter(&self) -> BreadthFirstTreeIter {
        BreadthFirstTreeIter::new(self.root)
    }
}

pub struct BreadthFirstTreeIter {
    stack: Vec<TreeIndex>,
}

impl BreadthFirstTreeIter {
    pub fn new(root: Option<TreeIndex>) -> Self {
        if let Some(index) = root {
            BreadthFirstTreeIter { stack: vec![index] }
        } else {
            BreadthFirstTreeIter { stack: vec![] }
        }
    }

    pub fn next<T>(&mut self, tree: &Tree<T>) -> Option<TreeIndex> {
        while let Some(node_index) = self.stack.pop() {
            if let Some(node) = tree.node_at(node_index) {
                self.stack.extend(node.children.iter());

                return Some(node_index);
            }
        }

        None
    }
}
