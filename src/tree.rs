// arena allocated tree adapted from the following sources
//  https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6
//  https://sachanganesh.com/programming/graph-tree-traversals-in-rust/

pub type TreeIndex = usize;

#[derive(Debug, Clone)]
pub struct TreeNode<T>
where
    T: Clone,
{
    value: T,
    parent: Option<TreeIndex>,
    children: Vec<TreeIndex>,
}

impl<T> TreeNode<T>
where
    T: Clone,
{
    fn new(value: T, parent: Option<TreeIndex>) -> Self {
        Self {
            value,
            parent,
            children: vec![],
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Tree<T>
where
    T: Clone,
{
    arena: Vec<Option<TreeNode<T>>>,
    root: Option<TreeIndex>,
}

impl<T> Tree<T>
where
    T: Clone,
{
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
}

impl<T> IntoIterator for Tree<T>
where
    T: Clone,
{
    type Item = TreeNode<T>;
    type IntoIter = TreeIntoIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self.root {
            Some(root) => TreeIntoIterator {
                tree: self,
                stack: vec![root],
            },
            _ => TreeIntoIterator {
                tree: self,
                stack: vec![],
            },
        }
    }
}

pub struct TreeIntoIterator<T>
where
    T: Clone,
{
    tree: Tree<T>,
    stack: Vec<TreeIndex>,
}

impl<T> Iterator for TreeIntoIterator<T>
where
    T: Clone,
{
    type Item = TreeNode<T>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(index) = self.stack.pop() {
            if let Some(node) = self.tree.node_at(index) {
                self.stack.extend(node.children.iter());

                return Some(node.clone());
            }
        }

        None
    }
}
