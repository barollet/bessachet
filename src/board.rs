use std::fmt;
use std::iter::FusedIterator;

use utils::*;

// The board is represented as a set of bitboards
// See: https://www.chessprogramming.org/Bitboards

// LSB is the square lower-right and the MSB is the upper-left one.
// 63 62 61 60 59 58 57 56      black pawns
// 55 54 53 52 51 50 49 48      black pieces
// 47 46 45 44 43 42 41 40
// 39 38 37 36 35 34 33 32
// 31 30 29 28 27 26 25 24
// 23 22 21 20 19 18 17 16
// 15 14 13 12 11 10 9  8       white pawns
// 7  6  5  4  3  2  1  0       white pieces

// public attributes are for move generation
// they are not supposed to be publicly accessed somewhere else
pub struct Board {
    pub pawns: BitBoard,
    pub knights: BitBoard,
    pub bishops: BitBoard,
    pub rooks: BitBoard,
    pub queens: BitBoard,
    pub kings: BitBoard,

    pub occupancy: [BitBoard; 2], // black pieces first
}

impl Board {
    // Returns a board representing the initial position
    #![allow(clippy::unreadable_literal)]
    pub fn initial_position() -> Self {
        Board {
            pawns:   BitBoard::new(0x00ff00000000ff00),
            knights: BitBoard::new(0x4200000000000042),
            bishops: BitBoard::new(0x2400000000000024),
            rooks:   BitBoard::new(0x8100000000000081),
            queens:  BitBoard::new(0x1000000000000010),
            kings:   BitBoard::new(0x0800000000000008),

            occupancy: [BitBoard::new(0xffff000000000000),
                        BitBoard::new(0x000000000000ffff)],
        }
    }

    #[inline]
    pub fn occupancy(&self) -> BitBoard {
        self.occupancy[WHITE].union(self.occupancy[BLACK])
    }

    #[inline]
    pub fn empty_squares(&self) -> BitBoard {
        self.occupancy().not()
    }
}

#[derive(Copy, Clone)]
pub struct BitBoard(u64);

impl BitBoard {
    pub const fn new(bitboard: u64) -> Self {
        BitBoard(bitboard)
    }

    pub fn to_square(self) -> Square {
        Square(63u8 - self.0.leading_zeros() as u8)
    }

    // Returns the bitboard containing only the MSB and removes it
    #[inline]
    pub fn pop_msb_bitboard(&mut self) -> Self {
        self.0 ^= 1u64 << self.0.leading_zeros();
        BitBoard(1u64 << self.0.leading_zeros())
    }

    // Basic binary operations
    #[inline]
    pub const fn intersect(self, other_bitboard: Self) -> Self {
        BitBoard(self.0 & other_bitboard.0)
    }

    #[inline]
    pub const fn union(self, other_bitboard: Self) -> Self {
        BitBoard(self.0 | other_bitboard.0)
    }

    #[inline]
    pub const fn xor(self, other_bitboard: Self) -> Self {
        BitBoard(self.0 ^ other_bitboard.0)
    }

    #[inline]
    pub const fn shift_left(self, shift: u32) -> Self {
        BitBoard(self.0 << shift)
    }

    #[inline]
    pub const fn shift_right(self, shift: u32) -> Self {
        BitBoard(self.0 >> shift)
    }

    #[inline]
    pub const fn not(self) -> Self {
        BitBoard(!self.0)
    }
}

// Iterates over the singly populated bitboards of the given bitboard
// starting from the MSB
impl Iterator for BitBoard {
    type Item = BitBoard;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(self.pop_msb_bitboard())
        } else  {
            None
        }
    }
}

impl FusedIterator for BitBoard {}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.pawns)?;
        write!(f, "{:?}", self.knights)?;
        write!(f, "{:?}", self.bishops)?;
        write!(f, "{:?}", self.rooks)?;
        write!(f, "{:?}", self.queens)?;
        write!(f, "{:?}", self.kings)?;
        write!(f, "{:?}", self.occupancy[WHITE])?;
        write!(f, "{:?}", self.occupancy[BLACK])
    } 
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in (0..8).rev() {
            let line: u8 = ((self.0 >> (8*i)) & 0xff) as u8;
            for j in (0..8).rev() {
                write!(f, "{}", (line >> j) & 0x1)?;
            }
            writeln!(f)?;
        }
        writeln!(f)
    }
}
