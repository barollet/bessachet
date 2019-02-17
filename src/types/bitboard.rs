//! A boolean feature over the board
use std::iter::FusedIterator;

use types::black_white_attribute::*;
use types::color::*;
use types::square::*;

use types::file;

pub type BitBoard = u64;
/// A wrapper for conversion, iteration and display purpose
pub struct BBWrapper(pub BitBoard);

// Some constants declaration for rows and files
pub const ROW_1: BitBoard = 0xff;
pub const ROW_2: BitBoard = 0xff00;
pub const ROW_4: BitBoard = 0xff000000;
pub const ROW_5: BitBoard = 0xff00000000;
pub const ROW_7: BitBoard = 0xff000000000000;
pub const ROW_8: BitBoard = 0xff00000000000000;
/// The promotion line for both Black and White players
pub const PROMOTION_LINE: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(ROW_1, ROW_8);

pub const FILE_A: BitBoard = file(0);
pub const FILE_B: BitBoard = file(1);
pub const FILE_C: BitBoard = file(2);
pub const FILE_D: BitBoard = file(3);
pub const FILE_E: BitBoard = file(4);
pub const FILE_F: BitBoard = file(5);
pub const FILE_G: BitBoard = file(6);
pub const FILE_H: BitBoard = file(7);
/// A list of the 8 files of the board
pub const FILES: [BitBoard; 8] = [
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H,
];

impl BBWrapper {
    /// Returns a `BitBoard` with the feature set to false on all square.
    pub const fn empty() -> BitBoard {
        0
    }

    /// Returns a `BitBoard` with the feature set to true on all square.
    pub const fn full() -> BitBoard {
        u64::max_value()
    }
}

/// Public interface for `BitBoard`
pub trait BitBoardExt {
    fn pop_lsb_as_square(&mut self) -> Square;

    // Belonging
    fn has_square(self, square: Square) -> bool;
    fn intersects(self, squares: BitBoard) -> bool;

    // Manipulation
    fn remove_square(&mut self, square: Square);
    fn remove_squares(&mut self, squares: BitBoard);
    fn add_square(&mut self, square: Square) -> BitBoard;

    fn population(self) -> u32;

    // Pawn moves
    fn push(self, Color) -> BitBoard;
    fn left_capture(self, Color) -> BitBoard;
    fn right_capture(self, Color) -> BitBoard;
}

impl BitBoardExt for BitBoard {
    /// Removes the LSB from the `BitBoard` and returns the `Square` corresponding to it
    fn pop_lsb_as_square(&mut self) -> Square {
        let singly_populated_bitboard = *self & self.overflowing_neg().0;
        *self ^= singly_populated_bitboard;
        Square::from(BBWrapper(singly_populated_bitboard))
    }

    /// Returns true if the given `BitBoard` contains the given `Square`.
    fn has_square(self, square: Square) -> bool {
        self & BitBoard::from(SqWrapper(square)) != 0
    }
    /// Returns true if the intersection between the two given `BitBoard`s is not empty
    fn intersects(self, squares: BitBoard) -> bool {
        self & squares != 0
    }

    /// Removes in place the given square from the bitboard
    fn remove_square(&mut self, square: Square) {
        self.remove_squares(BitBoard::from(SqWrapper(square)));
    }
    /// Removes in place the given bitboard from the original bitboard
    fn remove_squares(&mut self, squares: Self) {
        *self &= !squares;
    }
    /// Add in place a square to the given bitboard
    fn add_square(&mut self, square: Square) -> BitBoard {
        *self |= BitBoard::from(SqWrapper(square));
        *self
    }

    /// Returns the number of square where the feature is true
    fn population(self) -> u32 {
        self.count_ones()
    }
    // Pawn moves features
    /// Returns a pawn push of the given color for each square of the given `BitBoard`.
    fn push(self, color: Color) -> BitBoard {
        if color == WHITE {
            self << 8
        } else {
            self >> 8
        }
    }
    /// Returns a pawn left capture of the given color for each square of the given `BitBoard`.
    fn left_capture(self, color: Color) -> BitBoard {
        if color == WHITE {
            self << 9
        } else {
            self >> 7
        }
    }
    /// Returns a pawn right capture of the given color for each square of the given `BitBoard`.
    fn right_capture(self, color: Color) -> BitBoard {
        if color == WHITE {
            self << 7
        } else {
            self >> 9
        }
    }
}

// Iterates over the bits of the given bitboard and returns the associated Square
// starting from the MSB
impl Iterator for BBWrapper {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(self.0.pop_lsb_as_square())
        } else {
            None
        }
    }
}

impl FusedIterator for BBWrapper {}

use std::fmt;

impl fmt::Debug for BBWrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in (0..8).rev() {
            let line: u8 = ((self.0 >> (8 * i)) & 0xff) as u8;
            for j in (0..8).rev() {
                write!(f, "{}", (line >> j) & 0x1)?;
            }
            writeln!(f)?;
        }
        writeln!(f)
    }
}
