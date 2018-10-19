

// unsafe attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// This table is computed at runtime not to make the executable table too big
// hence the mut keyword
static mut ATTACK_TABLE: [u64; 46909] = [0; 46909];

#[derive(Copy, Clone)]
pub struct MagicEntry {
    magic: u64,
    table: *mut u64, // Unsafe pointer not to use a safe bigger slice
    black_mask: u64,
    postmask: u64,
}

// The magic entries for rooks and bishops
static mut BISHOP_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];
static mut ROOK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];


impl MagicEntry {
    const fn empty_magic() -> Self {
        MagicEntry {
            magic: 0,
            table: std::ptr::null_mut(),
            black_mask: 0,
            postmask: 0,
        }
    }
}
