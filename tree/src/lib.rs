// arena allocated tree adapted from the following sources
//  https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6
//  https://sachanganesh.com/programming/graph-tree-traversals-in-rust/

use std::collections::VecDeque;

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

#[derive(Debug, Clone)]
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
    pub fn new(root_value: Option<T>) -> Self {
        match root_value {
            Some(value) => {
                let mut t = Self::new(None);
                let root_index = t.new_node(value, None);
                t.set_root(Some(root_index));
                t
            }
            None => Self {
                arena: vec![],
                root: None,
            },
        }
    }

    pub fn set_root(&mut self, root_index: Option<TreeIndex>) {
        self.root = root_index;
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

    pub fn bfs_iter(self) -> BreadthFirstPreorderTreeIterator<T> {
        match self.root {
            Some(root) => BreadthFirstPreorderTreeIterator {
                tree: self,
                queue: VecDeque::from([root]),
            },
            _ => BreadthFirstPreorderTreeIterator {
                tree: self,
                queue: VecDeque::new(),
            },
        }
    }

    pub fn dfs_iter(self) -> DepthFirstPreorderTreeIterator<T> {
        match self.root {
            Some(root) => DepthFirstPreorderTreeIterator {
                tree: self,
                stack: Vec::from([root]),
                visited: Vec::new(),
            },
            _ => DepthFirstPreorderTreeIterator {
                tree: self,
                stack: Vec::new(),
                visited: Vec::new(),
            },
        }
    }
}

pub struct BreadthFirstPreorderTreeIterator<T>
where
    T: Clone,
{
    tree: Tree<T>,
    queue: VecDeque<TreeIndex>,
}

impl<T> Iterator for BreadthFirstPreorderTreeIterator<T>
where
    T: Clone,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(index) = self.queue.pop_front() {
            if let Some(node) = self.tree.node_at(index) {
                self.queue.extend(node.children.iter());

                return Some(node.value.clone());
            }
        }

        None
    }
}

pub struct DepthFirstPreorderTreeIterator<T>
where
    T: Clone,
{
    tree: Tree<T>,
    stack: Vec<TreeIndex>,
    visited: Vec<TreeIndex>,
}

impl<T> Iterator for DepthFirstPreorderTreeIterator<T>
where
    T: Clone,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(index) = self.stack.pop() {
            if let Some(node) = self.tree.node_at(index) {
                if !self.visited.contains(&index) {
                    self.visited.push(index);
                    self.stack.extend(node.children.iter().rev());
                }

                return Some(node.value.clone());
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn bfs_iter_correct_order() {
        let mut t = super::Tree::new(Some(0));
        let root = t.root.unwrap();
        let b = t.add_child(1, root);
        let _c = t.add_child(2, root);
        let d = t.add_child(3, b);
        let _e = t.add_child(4, b);
        let _f = t.add_child(5, d);

        let mut iter = t.bfs_iter();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn dfs_iter_correct_order() {
        let mut t = super::Tree::new(Some(0));
        let root = t.root.unwrap();
        let b = t.add_child(1, root);
        let _c = t.add_child(2, root);
        let d = t.add_child(3, b);
        let _e = t.add_child(4, b);
        let _f = t.add_child(5, d);

        let mut iter = t.dfs_iter();
        assert_eq!(iter.next(), Some(0));
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), None);
    }
}
