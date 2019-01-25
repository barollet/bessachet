pub mod pawn_hash_table;

use std::ops::{Index, IndexMut};
use std::sync::atomic::{AtomicUsize, Ordering};

pub use self::pawn_hash_table::*;

lazy_static! {
    pub static ref transposition_table: u64 = 5;
    pub static ref evaluation_table: u64 = 5;
}

// TODO write generic code for hash tables

/*
pub trait HashTableEntry {
    fn key(&self) -> AtomicUsize;
    fn data(&self) -> AtomicUsize;
}

pub trait HashTableType {
    type Entry: HashTableEntry;
    type ReadableEntry;

    fn new() -> Self::Entry;
    fn new_readable(key: usize, data: usize) -> Self::ReadableEntry;
}

const SIZE: usize = 10;
const INDEX_MASK: usize = 10;

pub struct SharedHashTable<T: HashTableType> {
    entries: [T::Entry; SIZE],
}

impl<T: HashTableType> SharedHashTable<T> {
    fn new() -> Self {
        let mut shared_table = Self {
            entries: unsafe { std::mem::uninitialized() },
        };

        for i in 0..SIZE {
            shared_table.entries[i] = <T as HashTableType>::new();
        }

        shared_table
    }

    // Gets a given entry as a readable entry
    pub fn get(&self, key: usize) -> Option<T::ReadableEntry> {
        let index = key & INDEX_MASK;
        let data = self[index].data().load(Ordering::Acquire);
        if self[index].key().load(Ordering::Acquire) ^ data == key {
            // Entry matches
            Some(<T as HashTableType>::new_readable(key, data))
        } else {
            None
        }
    }

    /*
    // It *may* insert if there is no current entry of if the replacing policy allows it
    // We don't mind the result of the insertion as it is for caching purpose
    pub fn try_insert(&mut self, key: usize, entry: PawnTableReadableEntry) {
        // TODO
    }
    */
}

// Indexation helpers
impl<T: HashTableType> Index<usize> for SharedHashTable<T> {
type Output = T::Entry;
fn index(&self, idx: usize) -> &Self::Output {
&self.entries[idx]
}
}

impl<T: HashTableType> IndexMut<usize> for SharedHashTable<T> {
fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
&mut self.entries[idx]
}
}
*/
