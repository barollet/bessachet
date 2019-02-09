use enum_primitive::FromPrimitive;
use move_generation::moves::Move;
use move_generation::moves::NULL_MOVE;
use std::sync::atomic::{AtomicUsize, Ordering};

const TTDEFAULT_SIZE: usize = 67_108_864; // The default size is 64MB of hash for the transposition table

pub type TranspositionTable = Vec<TTEntry>;

lazy_static! {
    pub static ref TRANSPOSITION_TABLE: TranspositionTable = init_tt();
    static ref INDEX_MASK: usize =
        usize::max_value() >> (64 - (read_tt_size() / 16).trailing_zeros());
}

// Reads the TT size from the command line or use the default 64MB value
fn read_tt_size() -> usize {
    // TODO read command line arguments and allocate the proper size
    TTDEFAULT_SIZE
}

fn init_tt() -> TranspositionTable {
    let size = read_tt_size();

    let n_entries = size / 16; // An entry is 16 bytes
    let mut tt: TranspositionTable = Vec::with_capacity(n_entries);

    for _ in 0..n_entries {
        tt.push(TTEntry::empty());
    }

    println!("Transposition table initialized");

    tt
}

// The data is organised from LSB to MSB:
// - 16 bits best move
// - 8 bits depth
// - 32 bits score
// - 2 bits node type
// - TODO 6 bits aging
pub struct TTEntry {
    key: AtomicUsize,
    data: AtomicUsize,
}

enum_from_primitive! {
#[derive(Debug, Copy, Clone)]
pub enum NodeType {
    PVNode = 0,
    AllNode,
    CutNode,
}
}

pub struct TTReadableEntry {
    pub key: usize,
    pub best_move: Move, // Or refutation move if kind is a beta cutoff
    pub depth: u8,
    pub score: f32,
    pub kind: NodeType,
}

impl TTEntry {
    const fn empty() -> TTEntry {
        TTEntry {
            key: AtomicUsize::new(0),
            data: AtomicUsize::new(0),
        }
    }
}

impl TTReadableEntry {
    fn deserialize(key: usize, data: usize) -> TTReadableEntry {
        TTReadableEntry {
            key,
            best_move: Move::from(data as u16),
            depth: (data >> 16) as u8,
            score: f32::from_bits((data >> 24) as u32),
            kind: NodeType::from_u8((data >> 58) as u8).unwrap(),
        }
    }

    pub fn new(key: usize, best_move: Move, depth: u8, score: f32, kind: NodeType) -> Self {
        TTReadableEntry {
            key,
            best_move,
            depth,
            score,
            kind,
        }
    }
}

// Public interface for the transposition table
pub trait TTExt {
    fn probe(&self, key: usize) -> Option<TTReadableEntry>;
    fn try_insert(&self, entry: &TTReadableEntry);
}

impl TTExt for TranspositionTable {
    // Gets a given entry as a readable entry
    fn probe(&self, key: usize) -> Option<TTReadableEntry> {
        let index = key & *INDEX_MASK;
        let data = self[index].data.load(Ordering::Acquire);
        if self[index].key.load(Ordering::Acquire) ^ data == key {
            // Entry matches
            Some(TTReadableEntry::deserialize(key, data))
        } else {
            None
        }
    }

    // It *may* insert if there is no current entry of if the replacing policy allows it
    // We don't mind the result of the insertion as it is for caching purpose
    fn try_insert(&self, entry: &TTReadableEntry) {
        let (key, data) = serialize(entry);
        let index = key & *INDEX_MASK;

        // Policy always replace TODO change it to a better policy
        self[index].key.store(key ^ data, Ordering::Release);
        self[index].data.store(data, Ordering::Release);
    }
}

// Compress the data into a key and a data of usize
fn serialize(entry: &TTReadableEntry) -> (usize, usize) {
    let key = entry.key;

    let raw_move: u16 = entry.best_move.into();
    let mut data = raw_move as usize;
    data |= usize::from(entry.depth) << 16;
    data |= (entry.score.to_bits() as usize) << 24;
    data |= (entry.kind as usize) << 56;

    (key, data)
}
