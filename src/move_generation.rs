mod init_magic;

use std::ptr;

use self::init_magic::get_fixed_offset;

// unsafe attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// This table is computed at runtime not to make the executable table too big
// hence the mut keyword
static mut ATTACK_TABLE: [u64; 50866] = [0; 50866]; // 406928 bytes

// See move_generation/init_magic.rs for impl block with initiatlization
#[derive(Debug, Copy, Clone)]
pub struct MagicEntry {
    magic: u64,
    table: *mut u64, // Unsafe pointer not to use a safe bigger slice
    black_mask: u64,
    postmask: u64,
}

// The magic entries for rooks and bishops (also mutable because pure functions cannot access
// static variable)
static mut BISHOP_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];
static mut ROOK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];

// Safe wrapper around the unsafe initialization (that have to be sequential)
pub fn init_magic_tables() {
    unsafe {
        for ((rook_entry, bishop_entry), square) in ROOK_TABLE.iter_mut().zip(BISHOP_TABLE.iter_mut()).zip(0u8..) {
            *rook_entry = MagicEntry::rook_magic(square);
            *bishop_entry = MagicEntry::bishop_magic(square);

            rook_entry.fill_attack_table(square, true);
            bishop_entry.fill_attack_table(square, false);
        }
    }
}

pub fn rook_attack(square: u8, mut occupancy: u64) -> u64 {
    let magic_entry = unsafe {
        ROOK_TABLE[usize::from(square)]
    };

    let table_pointer = magic_entry.table;

    occupancy |= magic_entry.black_mask;
    let table_offset = get_fixed_offset(occupancy, magic_entry.magic);
    unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    }
}

#[allow(dead_code)]
pub fn find_attack_table_holes() {
    unsafe {
        let mut hole_start: usize = 0;
        let mut holes_counter = 0;
        for (i, entry) in ATTACK_TABLE.iter().enumerate() {
            if *entry != 0 {
                if i - hole_start > 50 {
                    println!("hole at {} of size {}", hole_start, i - hole_start);
                }
                hole_start = i;
            } else {
                holes_counter += 1;
            }
        }
        println!("total holes {}", holes_counter);
    }
}

