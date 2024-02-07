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
