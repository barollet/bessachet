use std::ptr;

use move_generation::{SLIDING_ATTACK_TABLE, MagicEntry};

// Note: see the magic factors at the end of this file

// Magic factors initialization
impl MagicEntry {
    pub fn rook_magic(square: u8) -> Self {
        MagicEntry {
            magic: ROOK_MAGIC[usize::from(square)].0,
            table: unsafe {
                &mut SLIDING_ATTACK_TABLE[ROOK_MAGIC[usize::from(square)].1]
            },
            black_mask: rook_black_mask(square),
            postmask: rook_attack_empty_board(square),
        }
    }

    pub fn bishop_magic(square: u8) -> Self {
        MagicEntry {
            magic: BISHOP_MAGIC[usize::from(square)].0,
            table: unsafe {
                &mut SLIDING_ATTACK_TABLE[BISHOP_MAGIC[usize::from(square)].1]
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

// Get a bishop key for the given bitboard of blockers
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
pub fn bishop_attack(square: u8, blockers: u64) -> u64 {
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
    (22518084707288080, 2940), //a1
    (9007267974742048, 36389), //b1
    (18023195677294596, 4986), //c1
    (18018796690227202, 38440), //d1
    (18018796589563905, 7033), //e1
    (9232379786402268584, 41520), //f1
    (13511211202117824, 9573), //g1
    (58546984181563396, 44393), //h1
    (290288266383364, 36389), //a2
    (216190391480221708, 2940), //b2
    (576478353079664656, 38440), //c2
    (144150381038141473, 4986), //d2
    (9223407225521963040, 41520), //e2
    (4611721340246819072, 7033), //f2
    (216207968635453504, 44393), //g2
    (290271606736912, 9573), //h2
    (13510902047571976, 11576), //a3
    (1125968626843664, 46924), //b3
    (577023770976583696, 13236), //c3
    (9368613262309990404, 50028), //d3
    (563018689708048, 14131), //e3
    (2306124621637754881, 49483), //f3
    (9223372312806457345, 17261), //g3
    (9295430249502343520, 52264), //h3
    (285877452410881, 46924), //a4
    (576469548665143314, 11576), //b4
    (9368615323995013128, 50028), //c4
    (1152923704169071616, 13236), //d4
    (2305845208775917824, 49483), //e4
    (550294782208, 14131), //f4
    (4629770786754822145, 52264), //g4
    (9223407221763884392, 17261), //h4
    (18014467231055912, 18480), //a5
    (1125934267629588, 53386), //b5
    (4406637494288, 20011), //c5
    (9042392218862592, 54921), //d5
    (18049591473602816, 21038), //e5
    (35188669153408, 55944), //f5
    (432380750749237312, 23579), //g5
    (9223408801776926788, 59058), //h5
    (13195330721792, 53386), //a6
    (4785076803171518464, 18480), //b6
    (1130572965494800, 54921), //c6
    (2305847407293767712, 20011), //d6
    (1099545190432, 55944), //e6
    (18014673404182530, 21038), //f6
    (5818721089454358529, 59058), //g6
    (562987232430081, 23579), //h6
    (10376293653143683112, 26839), //a7
    (1125968626843664, 62227), //b7
    (577588851367804936, 30475), //c7
    (90074192107798560, 66171), //d7
    (2305878197880881184, 34317), //e7
    (9367488324979163168, 69882), //f7
    (76561228034474088, 0), //g7
    (8831107152, 72918), //h7
    (103633928326, 62227), //a8
    (1153202996932280385, 26839), //b8
    (824718674177, 66171), //c8
    (324329541982750690, 30475), //d8
    (2286985396880418, 69882), //e8
    (3530822225500835842, 34317), //f8
    (4398722877442, 72918), //g8
    (18577359879868482, 0), //h8
    ]; // end of rook magic factors

#[allow(clippy::unreadable_literal)]
pub const BISHOP_MAGIC: [(u64, usize); 64] = [
    (2305984296726808585, 73529), //a1
    (2305878260158824704, 74032), //b1
    (9511637357128845760, 76152), //c1
    (9223512825077432325, 76152), //d1
    (9511668384777928704, 76152), //e1
    (9295430732455870656, 76152), //f1
    (2305859914213367808, 78732), //g1
    (1441169538447048736, 79110), //h1
    (2323998411500560385, 73529), //a2
    (2350879418341638145, 74032), //b2
    (1806084325637349377, 74739), //c2
    (1157494923759484944, 74739), //d2
    (1299288750196523296, 74739), //e2
    (939000524462368784, 74739), //f2
    (2306133346318581792, 78732), //g2
    (1441151913507717153, 79110), //h2
    (2305983747742240772, 73529), //a3
    (2305852357747900420, 74032), //b3
    (9223407221497398248, 76930), //c3
    (9241391110302204032, 76930), //d3
    (9232381436139438080, 76930), //e3
    (9223374235894839812, 76930), //f3
    (2305847407799107590, 78732), //g3
    (1152921779754205192, 79110), //h3
    (2323998282658897730, 73529), //a4
    (2341940390420029441, 74032), //b4
    (4922293689588391968, 76575), //c4
    (4613656377632491521, 76575), //d4
    (4627448754562400512, 76575), //e4
    (4043687124791230496, 76575), //f4
    (2305851843962437634, 78732), //g4
    (1155182120109850628, 79110), //h4
    (9223440760626495505, 75370), //a5
    (9227911088769613842, 75801), //b5
    (1008823359229665312, 77054), //c5
    (2485994691024158728, 77054), //d5
    (2630665269827090440, 77054), //e5
    (1206120343941423136, 77054), //f5
    (4629717493793951753, 79568), //g5
    (4616092878219773968, 80105), //h5
    (9223442407679754256, 75370), //a6
    (9230074661886230592, 75801), //b6
    (9224506799428763664, 79228), //c6
    (9232950983213973520, 79228), //d6
    (9227875894717579268, 79228), //e6
    (9223438970699137040, 79228), //f6
    (4953993675236474912, 79568), //g6
    (4611721237696251860, 80105), //h6
    (9380998156973523456, 75370), //a7
    (9230135957519343616, 75801), //b7
    (1157425173213743248, 77887), //c7
    (1229553084200357908, 77887), //d7
    (1154408572610675201, 77887), //e7
    (1230609973092811008, 77887), //f7
    (4611686157201178769, 79568), //g7
    (4611826823025131776, 80105), //h7
    (9341591596308693184, 75370), //a8
    (9223389629579722761, 75801), //b8
    (9383249823628492864, 80012), //c8
    (9223394061447085968, 80012), //d8
    (9368622515687006210, 80012), //e8
    (9295431832065490950, 80012), //f8
    (4755801473873348610, 79568), //g8
    (4719913696988496899, 80105), //h8
    ]; // End of bishop magic factors
