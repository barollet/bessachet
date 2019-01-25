// The pawn table is not synchronized, there is an additionnal checksum (xor of 64 bits words) to
// check if a data race occured. If so, we just record a table miss.

const PAWN_TABLE_SIZE: usize = 4096; // Has to be a power of 2

//const INDEX_MASK: usize = usize::max_value() >> (64 - PAWN_TABLE_SIZE.trailing_zeros()); // Works only on 64 bits processors
const INDEX_MASK: usize = 0xfff;

pub type PawnTable = [PawnTableEntry; PAWN_TABLE_SIZE];

pub static mut PAWN_TABLE: PawnTable = [PawnTableEntry {
    key: 0,
    evaluation: 0,
    checksum: 0,
}; PAWN_TABLE_SIZE];

#[derive(Copy, Clone)]
pub struct PawnTableEntry {
    key: usize, // The key is stored xored with the data
    pub evaluation: i8,
    // TODO extend this and maybe switch to an implementation with mutexes
    checksum: usize,
}

pub trait PawnTableInterface {
    fn probe(&self, key: usize) -> Option<PawnTableEntry>;
    fn try_insert(&mut self, data: PawnTableEntry);
}

impl PawnTableInterface for PawnTable {
    fn probe(&self, key: usize) -> Option<PawnTableEntry> {
        let table_entry = self[key & INDEX_MASK];

        if table_entry.key == key && table_entry.is_valid() {
            Some(table_entry)
        } else {
            None
        }
    }

    // Current policy, always replace
    // TODO better replacing policy
    fn try_insert(&mut self, data: PawnTableEntry) {
        self[data.key & INDEX_MASK] = data;
    }
}

impl PawnTableEntry {
    pub fn new(key: usize, eval: f32) -> Self {
        let mut entry = PawnTableEntry {
            key,
            evaluation: eval as i8,
            checksum: 0,
        };

        entry.checksum = entry.compute_checksum();

        entry
    }
    // Checks if the given entry is consistent with its checksum
    fn is_valid(&self) -> bool {
        self.compute_checksum() ^ self.checksum == 0
    }

    fn compute_checksum(&self) -> usize {
        self.key ^ (self.evaluation as usize)
    }
}
