use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Id<T> {
    idx: u32,
    /// Makes `Id` act like it owns a `T`
    _phantom: PhantomData<fn() -> T>,
}

/// A simple arena to store IR nodes
pub struct Arena<T> {
    data: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn alloc(&mut self, item: T) -> Id<T> {
        let idx = self.data.len() as u32;
        self.data.push(item);
        Id {
            idx,
            _phantom: PhantomData,
        }
    }

    pub fn get(&self, id: Id<T>) -> &T {
        &self.data[id.idx as usize]
    }

    pub fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.data[id.idx as usize]
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
    fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}
