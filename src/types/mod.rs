#![allow(clippy::unreadable_literal)]

use std::convert::From;

pub mod bitboard;
pub mod black_white_attribute;
pub mod color;
pub mod piece;
pub mod square;

pub use self::bitboard::*;
pub use self::black_white_attribute::*;
pub use self::color::*;
pub use self::piece::*;
pub use self::square::*;

/// All the squares of the board
pub const SQUARES: BitBoard = BBWraper::full();

/// Returns the nth file (numbered from 0 for A to 7 for H)
pub const fn file(number: u32) -> BitBoard {
    0x0101010101010101 << (7 - number)
}

// Conversions between Squares and BitBoards
impl From<SqWrapper> for BitBoard {
    fn from(square: SqWrapper) -> BitBoard {
        1u64 << square.0
    }
}

impl From<BBWraper> for Square {
    fn from(bitboard: BBWraper) -> Square {
        bitboard.0.trailing_zeros() as Square
    }
}

/// Returns a bitboard between the two given squares excluded.
/// Caution works only for straight lines
/*
 * 00000000 00000000    00000000
 * 00000000 00000000    00000000
 * 01000000 00000000    00000000
 * 00000000 00000000 -> 00100000
 * 00000000 00000000    00010000
 * 00000000 00000000    00001000
 * 00000000 00000100    00000000
 * 00000000 00000000    00000000
*/
// We use the partial sum of geometric sequences to compute this efficiently
pub fn square_mask_between(from: Square, to: Square) -> BitBoard {
    let raw_distance = distance(from, to);
    let offset_unit = masking_offset(from, to, raw_distance);
    let length = raw_distance / offset_unit - 1;

    let base = ((1 << (offset_unit * length)) - 1) / ((1 << offset_unit) - 1);
    base << (std::cmp::min(from, to) + offset_unit)
}

// Offsets for masking between two squares
// NOTE: specialized masking for rook and bishop ?
fn masking_offset(from_square: Square, target_square: Square, raw_distance: u8) -> u8 {
    if target_square % 8 == from_square % 8 {
        // Vertical
        8
    } else if target_square / 8 == from_square / 8 {
        // Horizontal
        1
    } else if raw_distance % 9 == 0 {
        // Antidiagonal
        9
    } else {
        debug_assert!(raw_distance % 7 == 0);
        // Diagonal
        7
    }
}
