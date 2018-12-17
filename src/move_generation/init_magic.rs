use std::ptr;

use move_generation::{SLIDING_ATTACK_TABLE, MagicEntry};

// Note: see the magic factors at the end of this file

// Magic factors initialization
impl MagicEntry {
    pub fn rook_magic(square: u8) -> Self {
        MagicEntry {
            magic: ROOK_MAGIC[usize::from(square)].0,
            table: &SLIDING_ATTACK_TABLE[ROOK_MAGIC[usize::from(square)].1],
            black_mask: rook_black_mask(square),
            postmask: rook_attack_empty_board(square),
        }
    }

    pub fn bishop_magic(square: u8) -> Self {
        MagicEntry {
            magic: BISHOP_MAGIC[usize::from(square)].0,
            table: &SLIDING_ATTACK_TABLE[BISHOP_MAGIC[usize::from(square)].1],
            black_mask: bishop_black_mask(square),
            postmask: bishop_attack_on_empty_board(square),
        }
    }
}

// Attack table initialization
// rook boolean is set if we are filling a rook entry
// Avoid using MagicEntry so we don't have deadlocks with lazy statics
pub fn fill_attack_table(attack_table: &mut[u64], square: u8) {
    let magic: u64 = ROOK_MAGIC[usize::from(square)].0;
    let table_pointer: *mut u64 = &mut attack_table[ROOK_MAGIC[usize::from(square)].1];
    fill_attack_table_helper(square, table_pointer, magic, rook_mask, rook_offset, rook_attack);

    let magic: u64 = BISHOP_MAGIC[usize::from(square)].0;
    let table_pointer: *mut u64 = &mut attack_table[BISHOP_MAGIC[usize::from(square)].1];
    fill_attack_table_helper(square, table_pointer, magic, bishop_mask, bishop_offset, bishop_attack);
}

fn fill_attack_table_helper(square: u8,
                            table_pointer: *mut u64,
                            magic: u64,
                            get_mask: fn (u8) -> u64,
                            get_offset: fn (u64, u64) -> usize,
                            get_attack: fn (u8, u64) -> u64) {
    let mask = get_mask(square);

    let n = mask.count_ones();

    for i in 0..(1 << n) {
        let key = index_to_key(i, n, mask) | !mask;
        let table_index = get_offset(key, magic);
        unsafe {
            let old_value: u64 = ptr::read(table_pointer.add(table_index));
            let new_attack_value = old_value | get_attack(square, key);
            ptr::write(table_pointer.add(table_index), new_attack_value);
        }
    }
}

/*
fn coord_from_square(square: u8) -> (u8, u8) {
    (square / 8, 7 - square % 8)
}

fn square_from_coord(coord: (u8, u8)) -> u8 {
    8 * coord.0 + 7 - coord.1
}
*/
fn coord_from_square(square: u8) -> (u8, u8) {
    (square / 8, square % 8)
}

fn square_from_coord(coord: (u8, u8)) -> u8 {
    8 * coord.0 + coord.1
}


// Goes through a line given a bitboard of blockers and a closure to get the next move
fn direction_blockers_mask<F, G>(result: &mut u64, blockers: u64, mut kl: (u8, u8), update: F, check_bounds: G)
    where F: Fn((u8, u8)) -> (u8, u8),
          G: Fn((u8, u8)) -> bool,
{
    while check_bounds(kl) {
        kl = update(kl);
        let square_bitboard = 1 << square_from_coord(kl);
        *result |= square_bitboard;
        if blockers & square_bitboard != 0 {
            break
        }
    }
}

// Get a bishop attack on an empty board
fn bishop_black_mask(square: u8) -> u64 {
    !bishop_key(square, 0)
}

pub fn bishop_mask(square: u8) -> u64 {
    bishop_key(square, 0)
}

fn bishop_attack_on_empty_board(square: u8) -> u64 {
    bishop_attack(square, 0)
}

// Get a bishop key for the given bitboard of blockers
pub fn bishop_key(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = coord_from_square(square);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l+1), |(k, l)| k < 6 && l < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l-1), |(k, l)| k < 6 && l > 1);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l+1), |(k, l)| k > 1 && l < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l-1), |(k, l)| k > 1 && l > 1);

    result
}

// Get a bishop attack for the given bitboard of blockers
pub fn bishop_attack(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = coord_from_square(square);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l+1), |(k, l)| k < 7 && l < 7);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l-1), |(k, l)| k < 7 && l > 0);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l+1), |(k, l)| k > 0 && l < 7);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l-1), |(k, l)| k > 0 && l > 0);

    result
}

fn rook_black_mask(square: u8) -> u64 {
    !rook_key(square, 0)
}

pub fn rook_mask(square: u8) -> u64 {
    rook_key(square, 0)
}

fn rook_attack_empty_board(square: u8) -> u64 {
    rook_attack(square, 0)
}

// Get a rook key for magic table for the given bitboard of blockers
pub fn rook_key(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = coord_from_square(square);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l), |(k, _l)| k < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l), |(k, _l)| k > 1);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k, l+1), |(_k, l)| l < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k, l-1), |(_k, l)| l > 1);

    result
}

pub fn rook_attack(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = coord_from_square(square);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l), |(k, _l)| k < 7);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l), |(k, _l)| k > 0);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k, l+1), |(_k, l)| l < 7);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k, l-1), |(_k, l)| l > 0);

    result
}

// Remove the first bit of a bitboard and return its position
fn pop_1st_bit(mask: &mut u64) -> u32 {
    let j = mask.trailing_zeros();
    *mask &= *mask -1;
    j
}

pub fn index_to_key(index: usize, bits: u32, mut mask: u64) -> u64 {
    let mut result = 0;
    for i in 0..bits {
        let j = pop_1st_bit(&mut mask);
        if index & (1 << i) != 0 {
            result |= 1u64 << j;
        }
    }
    result
}

// Computes the offset in the attack table from the relevant occupancy bits and a given magic factor
pub fn rook_offset(key: u64, magic: u64) -> usize {
    (key.overflowing_mul(magic).0 >> (64 - 12)) as usize
}

// Computes the offset in the attack table from the relevant occupancy bits and a given magic factor
pub fn bishop_offset(key: u64, magic: u64) -> usize {
    (key.overflowing_mul(magic).0 >> (64 - 9)) as usize
}

// Magic bitboards attack table initiatlization
// Everything is done unsafely at runtime
//
// The attack table for the given magic factors
// in this fancy black magic with fixed shift and shared attacks
// contains 46909 entries and represent ~375 kB
//
#[allow(clippy::unreadable_literal)]
const ROOK_MAGIC: [(u64, usize); 64] = [
    ( 0x80280013ff84ffff,  10890 ),
    ( 0x5ffbfefdfef67fff,  56054 ),
    ( 0xffeffaffeffdffff,  67495 ),
    ( 0x003000900300008a,  72797 ),
    ( 0x0030018003500030,  17179 ),
    ( 0x0020012120a00020,  63978 ),
    ( 0x0030006000c00030,  56650 ),
    ( 0xffa8008dff09fff8,  15929 ),
    ( 0x7fbff7fbfbeafffc,  55905 ),
    ( 0x0000140081050002,  26301 ),
    ( 0x0000180043800048,  78100 ),
    ( 0x7fffe800021fffb8,  86245 ),
    ( 0xffffcffe7fcfffaf,  75228 ),
    ( 0x00001800c0180060,  31661 ),
    ( 0xffffe7ff8fbfffe8,  38053 ),
    ( 0x0000180030620018,  37433 ),
    ( 0x00300018010c0003,  74747 ),
    ( 0x0003000c0085ffff,  53847 ),
    ( 0xfffdfff7fbfefff7,  70952 ),
    ( 0x7fc1ffdffc001fff,  49447 ),
    ( 0xfffeffdffdffdfff,  62629 ),
    ( 0x7c108007befff81f,  58996 ),
    ( 0x20408007bfe00810,  36009 ),
    ( 0x0400800558604100,  21230 ),
    ( 0x0040200010080008,  51882 ),
    ( 0x0010020008040004,  11841 ),
    ( 0xfffdfefff7fbfff7,  25794 ),
    ( 0xfebf7dfff8fefff9,  49689 ),
    ( 0xc00000ffe001ffe0,  63400 ),
    ( 0x2008208007004007,  33958 ),
    ( 0xbffbfafffb683f7f,  21991 ),
    ( 0x0807f67ffa102040,  45618 ),
    ( 0x200008e800300030,  70134 ),
    ( 0x0000008780180018,  75944 ),
    ( 0x0000010300180018,  68392 ),
    ( 0x4000008180180018,  66472 ),
    ( 0x008080310005fffa,  23236 ),
    ( 0x4000188100060006,  19067 ),
    ( 0xffffff7fffbfbfff,      0 ),
    ( 0x0000802000200040,  43566 ),
    ( 0x20000202ec002800,  29810 ),
    ( 0xfffff9ff7cfff3ff,  65558 ),
    ( 0x000000404b801800,  77684 ),
    ( 0x2000002fe03fd000,  73350 ),
    ( 0xffffff6ffe7fcffd,  61765 ),
    ( 0xbff7efffbfc00fff,  49282 ),
    ( 0x000000100800a804,  78840 ),
    ( 0xfffbffefa7ffa7fe,  82904 ),
    ( 0x0000052800140028,  24594 ),
    ( 0x00000085008a0014,   9513 ),
    ( 0x8000002b00408028,  29012 ),
    ( 0x4000002040790028,  27684 ),
    ( 0x7800002010288028,  27901 ),
    ( 0x0000001800e08018,  61477 ),
    ( 0x1890000810580050,  25719 ),
    ( 0x2003d80000500028,  50020 ),
    ( 0xfffff37eefefdfbe,  41547 ),
    ( 0x40000280090013c1,   4750 ),
    ( 0xbf7ffeffbffaf71f,   6014 ),
    ( 0xfffdffff777b7d6e,  41529 ),
    ( 0xeeffffeff0080bfe,  84192 ),
    ( 0xafe0000fff780402,  33433 ),
    ( 0xee73fffbffbb77fe,   8555 ),
    ( 0x0002000308482882,   1009 ),
    ]; // End of rook magic factors

#[allow(clippy::unreadable_literal)]
const BISHOP_MAGIC: [(u64, usize); 64] = [
    ( 0x107ac08050500bff,  66157 ),
    ( 0x7fffdfdfd823fffd,  71730 ),
    ( 0x0400c00fe8000200,  37781 ),
    ( 0x103f802004000000,  21015 ),
    ( 0xc03fe00100000000,  47590 ),
    ( 0x24c00bffff400000,    835 ),
    ( 0x0808101f40007f04,  23592 ),
    ( 0x100808201ec00080,  30599 ),
    ( 0xffa2feffbfefb7ff,  68776 ),
    ( 0x083e3ee040080801,  19959 ),
    ( 0x040180bff7e80080,  21783 ),
    ( 0x0440007fe0031000,  64836 ),
    ( 0x2010007ffc000000,  23417 ),
    ( 0x1079ffe000ff8000,  66724 ),
    ( 0x7f83ffdfc03fff80,  74542 ),
    ( 0x080614080fa00040,  67266 ),
    ( 0x7ffe7fff817fcff9,  26575 ),
    ( 0x7ffebfffa01027fd,  67543 ),
    ( 0x20018000c00f3c01,  24409 ),
    ( 0x407e0001000ffb8a,  30779 ),
    ( 0x201fe000fff80010,  17384 ),
    ( 0xffdfefffde39ffef,  18778 ),
    ( 0x7ffff800203fbfff,  65109 ),
    ( 0x7ff7fbfff8203fff,  20184 ),
    ( 0x000000fe04004070,  38240 ),
    ( 0x7fff7f9fffc0eff9,  16459 ),
    ( 0x7ffeff7f7f01f7fd,  17432 ),
    ( 0x3f6efbbf9efbffff,  81040 ),
    ( 0x0410008f01003ffd,  84946 ),
    ( 0x20002038001c8010,  18276 ),
    ( 0x087ff038000fc001,   8512 ),
    ( 0x00080c0c00083007,  78544 ),
    ( 0x00000080fc82c040,  19974 ),
    ( 0x000000407e416020,  23850 ),
    ( 0x00600203f8008020,  11056 ),
    ( 0xd003fefe04404080,  68019 ),
    ( 0x100020801800304a,  85965 ),
    ( 0x7fbffe700bffe800,  80524 ),
    ( 0x107ff00fe4000f90,  38221 ),
    ( 0x7f8fffcff1d007f8,  64647 ),
    ( 0x0000004100f88080,  61320 ),
    ( 0x00000020807c4040,  67281 ),
    ( 0x00000041018700c0,  79076 ),
    ( 0x0010000080fc4080,  17115 ),
    ( 0x1000003c80180030,  50718 ),
    ( 0x2006001cf00c0018,  24659 ),
    ( 0xffffffbfeff80fdc,  38291 ),
    ( 0x000000101003f812,  30605 ),
    ( 0x0800001f40808200,  37759 ),
    ( 0x084000101f3fd208,   4639 ),
    ( 0x080000000f808081,  21759 ),
    ( 0x0004000008003f80,  67799 ),
    ( 0x08000001001fe040,  22841 ),
    ( 0x085f7d8000200a00,  66689 ),
    ( 0xfffffeffbfeff81d,  62548 ),
    ( 0xffbfffefefdff70f,  66597 ),
    ( 0x100000101ec10082,  86749 ),
    ( 0x7fbaffffefe0c02f,  69558 ),
    ( 0x7f83fffffff07f7f,  61589 ),
    ( 0xfff1fffffff7ffc1,  62533 ),
    ( 0x0878040000ffe01f,  64387 ),
    ( 0x005d00000120200a,  26581 ),
    ( 0x0840800080200fda,  76355 ),
    ( 0x100000c05f582008,  11140 ),
    ]; // End of bishop magic factors
