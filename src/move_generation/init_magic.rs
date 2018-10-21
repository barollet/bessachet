use std::ptr;

use move_generation::{ATTACK_TABLE, MagicEntry};

// Note: see the magic factors at the end of this file

// Magic factors initialization
impl MagicEntry {
    pub fn rook_magic(square: u8) -> Self {
        MagicEntry {
            magic: ROOK_MAGIC[usize::from(square)].0,
            table: unsafe {
                &mut ATTACK_TABLE[ROOK_MAGIC[usize::from(square)].1]
            },
            black_mask: rook_black_mask(square),
            postmask: rook_attack_empty_board(square),
        }
    }

    pub fn bishop_magic(square: u8) -> Self {
        MagicEntry {
            magic: BISHOP_MAGIC[usize::from(square)].0,
            table: unsafe {
                &mut ATTACK_TABLE[BISHOP_MAGIC[usize::from(square)].1]
            },
            black_mask: bishop_black_mask(square),
            postmask: bishop_attack_on_empty_board(square),
        }
    }

    // Attack table initialization
    // rook boolean is set if we are filling a rook entry
    pub fn fill_attack_table(&self, square: u8, rook: bool) {
        let mask = if rook {
            rook_mask(square)
        } else {
            bishop_mask(square)
        };

        let n = mask.count_ones();

        for i in 0..(1 << n) {
            let key = index_to_key(i, n, mask) | !mask;
            let table_index = get_fixed_offset(key, self.magic);

            unsafe {
                let old_value: u64 = ptr::read(self.table.add(table_index));
                let new_attack_value = old_value | if rook {
                    rook_attack(square, key)
                } else {
                    bishop_attack(square, key)
                };
                ptr::write(self.table.add(table_index), new_attack_value);
            }
        }
    }

    pub const fn empty_magic() -> Self {
        MagicEntry {
            magic: 0,
            table: ptr::null_mut(),
            black_mask: 0,
            postmask: 0,
        }
    }
}

// Goes through a line given a bitboard of blockers and a closure to get the next move
fn direction_blockers_mask<F, G>(result: &mut u64, blockers: u64, mut kl: (u8, u8), update: F, check_bounds: G)
    where F: Fn((u8, u8)) -> (u8, u8),
          G: Fn((u8, u8)) -> bool,
{
    while check_bounds(kl) {
        kl = update(kl);
        *result |= 1 << (8*kl.0 + kl.1);
        if blockers & 1 << (8*kl.0 + kl.1) != 0 {
            break
        }
    }
}

// Get a bishop attack on an empty board
fn bishop_black_mask(square: u8) -> u64 {
    !bishop_key(square, 0)
}

fn bishop_mask(square: u8) -> u64 {
    bishop_key(square, 0)
}

fn bishop_attack_on_empty_board(square: u8) -> u64 {
    bishop_attack(square, 0)
}

// Get a bishop attack for the given bitboard of blockers
pub fn bishop_key(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = (square / 8, square % 8);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l+1), |(k, l)| k < 6 && l < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l-1), |(k, l)| k < 6 && l > 1);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l+1), |(k, l)| k > 1 && l < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l-1), |(k, l)| k > 1 && l > 1);

    result
}

// Get a bishop attack for the given bitboard of blockers
fn bishop_attack(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = (square / 8, square % 8);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l+1), |(k, l)| k < 7 && l < 7);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l-1), |(k, l)| k < 7 && l > 0);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l+1), |(k, l)| k > 0 && l < 7);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l-1), |(k, l)| k > 0 && l > 0);

    result
}

fn rook_black_mask(square: u8) -> u64 {
    !rook_key(square, 0)
}

fn rook_mask(square: u8) -> u64 {
    rook_key(square, 0)
}

fn rook_attack_empty_board(square: u8) -> u64 {
    rook_attack(square, 0)
}

// Get a rook key for magic table for the given bitboard of blockers
pub fn rook_key(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = (square / 8, square % 8);

    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k+1, l), |(k, _l)| k < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k-1, l), |(k, _l)| k > 1);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k, l+1), |(_k, l)| l < 6);
    direction_blockers_mask(&mut result, blockers, ij, |(k, l)| (k, l-1), |(_k, l)| l > 1);

    result
}

fn rook_attack(square: u8, blockers: u64) -> u64 {
    let mut result: u64 = 0;
    let ij = (square / 8, square % 8);

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

fn index_to_key(index: usize, bits: u32, mut mask: u64) -> u64 {
    let mut result = 0;
    for i in 0..bits {
        let j = pop_1st_bit(&mut mask);
        if index & (1 << i) != 0 {
            result |= 1u64 << j;
        }
    }
    result
}

#[inline]
// Computes the offset in the attack table from the relevant occupancy bits and a given magic factor
pub fn get_fixed_offset(key: u64, magic: u64) -> usize {
    (key.overflowing_mul(magic).0 >> (64 - 12)) as usize
}

// Magic bitboards attack table initiatlization
// Everything is done unsafely at runtime
//
// The attack table for the given magic factors
// in this fancy black magic with fixed shift and shared attacks
// contains 46909 entries and represent ~375 kB
//
#[allow(clippy::unreadable_literal)]
pub const ROOK_MAGIC: [(u64, usize); 64] = [
    (9227877501035347968, 2058),
    (4503668481327104, 2791),
    (9223935556830904320, 5332),
    (9009398345113600, 4821),
    (9008299303370752, 5844),
    (9232414422629154816, 8915),
    (6755708680568832, 7876),
    (1697784467062784, 9915),
    (3458782140380151808, 2791),
    (9223381395320111104, 2058),
    (2305845213538582528, 4821),
    (9223402823717355520, 5332),
    (11745405562337103872, 8915),
    (2305878230093465600, 5844),
    (4611721218502959104, 9915),
    (6631702142976, 7876),
    (9223372071430848512, 12361),
    (4611686035648708610, 12277),
    (4611686027029643266, 13449),
    (2377900742839959552, 13527),
    (2314850277441671168, 14426),
    (2305843079007406080, 15163),
    (9259400851384960000, 17238),
    (9520609629537968128, 17925),
    (2305844659152355328, 12277),
    (9255463482801913857, 12361),
    (2342215953405739008, 13527),
    (5845673416316706816, 13449),
    (2395915551786336256, 15163),
    (2305843559506843648, 14426),
    (9224497937835598849, 17925),
    (9367487225467623424, 17238),
    (2882304719295873024, 17376),
    (9223374751274369024, 19851),
    (9223372844308758592, 20857),
    (4611686142983569408, 20595),
    (1729382456628355072, 20734),
    (4611686222438860800, 22267),
    (9236886838651126784, 23860),
    (2378463561798240256, 23132),
    (9223374047033692160, 19851),
    (2882304897267532800, 17376),
    (4755802308028137984, 20595),
    (9241386574355111936, 20857),
    (5764608073041711104, 22267),
    (1197957775884292096, 20734),
    (2882303777153499136, 23132),
    (9227875652856717312, 23860),
    (576460889743425568, 25670),
    (2449960396421136392, 28642),
    (7007602119770898436, 32120),
    (11529215596296273924, 0),
    (9223372041150660640, 35216),
    (9520609749951873028, 37963),
    (9223372051887423560, 40391),
    (6917529027708977264, 44046),
    (21896429697, 28642),
    (288231477811088449, 25670),
    (4683743616907117825, 0),
    (720575946932880129, 32120),
    (2630102215672465412, 37963),
    (4611686020843806724, 35216),
    (11601272640419922945, 44046),
    (38280596902397441, 40391),
]; // end of rook magic factors

#[allow(clippy::unreadable_literal)]
pub const BISHOP_MAGIC: [(u64, usize); 64] = [
    (9227877501035347968, 2058),
    (4503668481327104, 2791),
    (9223935556830904320, 5332),
    (9009398345113600, 4821),
    (9008299303370752, 5844),
    (9232414422629154816, 8915),
    (6755708680568832, 7876),
    (1697784467062784, 9915),
    (3458782140380151808, 2791),
    (9223381395320111104, 2058),
    (2305845213538582528, 4821),
    (9223402823717355520, 5332),
    (11745405562337103872, 8915),
    (2305878230093465600, 5844),
    (4611721218502959104, 9915),
    (6631702142976, 7876),
    (9223372071430848512, 12361),
    (4611686035648708610, 12277),
    (4611686027029643266, 13449),
    (2377900742839959552, 13527),
    (2314850277441671168, 14426),
    (2305843079007406080, 15163),
    (9259400851384960000, 17238),
    (9520609629537968128, 17925),
    (2305844659152355328, 12277),
    (9255463482801913857, 12361),
    (2342215953405739008, 13527),
    (5845673416316706816, 13449),
    (2395915551786336256, 15163),
    (2305843559506843648, 14426),
    (9224497937835598849, 17925),
    (9367487225467623424, 17238),
    (2882304719295873024, 17376),
    (9223374751274369024, 19851),
    (9223372844308758592, 20857),
    (4611686142983569408, 20595),
    (1729382456628355072, 20734),
    (4611686222438860800, 22267),
    (9236886838651126784, 23860),
    (2378463561798240256, 23132),
    (9223374047033692160, 19851),
    (2882304897267532800, 17376),
    (4755802308028137984, 20595),
    (9241386574355111936, 20857),
    (5764608073041711104, 22267),
    (1197957775884292096, 20734),
    (2882303777153499136, 23132),
    (9227875652856717312, 23860),
    (576460889743425568, 25670),
    (2449960396421136392, 28642),
    (7007602119770898436, 32120),
    (11529215596296273924, 123),
    (9223372041150660640, 35216),
    (9520609749951873028, 37963),
    (9223372051887423560, 40391),
    (6917529027708977264, 44046),
    (21896429697, 28642),
    (288231477811088449, 25670),
    (4683743616907117825, 123),
    (720575946932880129, 32120),
    (2630102215672465412, 37963),
    (4611686020843806724, 35216),
    (11601272640419922945, 44046),
    (38280596902397441, 40391),
]; // End of bishop magic factors
