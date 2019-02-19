// This module is testing the precomputed table for sliding attacks

use move_generation::{MagicEntry, BISHOP_ATTACK_TABLE, ROOK_ATTACK_TABLE};

use move_generation::init_magic::{
    bishop_attack, bishop_mask, bishop_offset, index_to_key, rook_attack, rook_mask, rook_offset,
};

use types::*;

fn key_iterator(mask: BitBoard) -> impl Iterator<Item = BitBoard> {
    let n = mask.count_ones();
    println!("{} {:?}", n, mask);
    (0..(1 << n)).map(move |index| index_to_key(index, n, mask) | !mask)
}

// mask is given as a white mask
fn test_key(mask: BitBoard) -> bool {
    let mut keys: [BitBoard; 4096] = [BBWrapper::empty(); 4096];

    for (i, key) in key_iterator(mask).enumerate() {
        // pass if different from all the precedent ones
        for j in 0..i {
            if key == keys[j] {
                println!("Collision between key {} and {} with value {:?}", i, j, key);
                return false;
            }
        }
        // store the key for next passes
        keys[i] = key;
    }

    true
}

// Test without sharing attack TODO add postmask
fn test_magic_entry(
    entry: &MagicEntry,
    attack_function: &Fn(BitBoard) -> BitBoard,
    bishop: bool,
) -> bool {
    let mut offsets: [usize; 4096] = [0; 4096];
    let mut keys: [BitBoard; 4096] = [BBWrapper::empty(); 4096];
    for (i, key) in key_iterator(!entry.black_mask).enumerate() {
        let offset = if bishop {
            bishop_offset(key, entry.magic)
        } else {
            rook_offset(key, entry.magic)
        };
        // pass if no unwanted collision without all the precedent ones
        for j in 0..i {
            if offset == offsets[j] && attack_function(key) != attack_function(keys[j]) {
                println!(
                    "Unwanted collision between offset {} and {} with value {:?}",
                    i, j, key
                );
                return false;
            }
        }
        // store the index for next passes
        offsets[i] = offset;
        keys[i] = key;
    }

    true
}

// /!\ This tests need the masking and attack generation to be correct

#[test]
// Tests that the key generation is distinct
fn magic_key_generation() {
    // bishop keys
    for square in 0..64 {
        test_key(bishop_mask(square));
    }
    // rook keys
    for square in 0..64 {
        test_key(rook_mask(square));
    }
}

#[test]
// Test that we have no overlapping between different attack sets
fn magic_no_bad_overlapping() {
    // bishop collisions
    for square in 0..64 {
        test_magic_entry(
            &BISHOP_ATTACK_TABLE[square],
            &|key| bishop_attack(square as u8, key),
            true,
        );
    }
    // rook collisions
    for square in 0..64 {
        test_magic_entry(
            &ROOK_ATTACK_TABLE[square],
            &|key| rook_attack(square as u8, key),
            false,
        );
    }
}
