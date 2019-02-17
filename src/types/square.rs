//! The square representation
use std::cmp::{max, min};

use types::color::*;

/// A single square of the board
pub type Square = u8;
/// A variant of `Square` used for en passant
pub type NonZeroSquare = std::num::NonZeroU8;
/// A wrapper for constant initialization, conversion and display
pub struct SqWrapper(pub Square);

// Some Square constant declaration
pub const A1_SQUARE: Square = SqWrapper::from_char_file_rank('a', '1');
pub const C1_SQUARE: Square = SqWrapper::from_char_file_rank('c', '1');
pub const C8_SQUARE: Square = SqWrapper::from_char_file_rank('c', '8');
pub const D1_SQUARE: Square = SqWrapper::from_char_file_rank('d', '1');
pub const D8_SQUARE: Square = SqWrapper::from_char_file_rank('d', '8');
pub const E1_SQUARE: Square = SqWrapper::from_char_file_rank('e', '1');
pub const E8_SQUARE: Square = SqWrapper::from_char_file_rank('e', '8');
pub const F1_SQUARE: Square = SqWrapper::from_char_file_rank('f', '1');
pub const F8_SQUARE: Square = SqWrapper::from_char_file_rank('f', '8');
pub const G1_SQUARE: Square = SqWrapper::from_char_file_rank('g', '1');
pub const G8_SQUARE: Square = SqWrapper::from_char_file_rank('g', '8');
pub const H1_SQUARE: Square = SqWrapper::from_char_file_rank('h', '1');
pub const A8_SQUARE: Square = SqWrapper::from_char_file_rank('a', '8');
pub const H8_SQUARE: Square = SqWrapper::from_char_file_rank('h', '8');

impl SqWrapper {
    /// Creates a square from file and rank between 0 and 7.
    /// See the board representation in `board/mod.rs`
    /// Caution: The inputs are unchecked!
    /// # Examples
    ///
    /// ```
    /// assert_eq!(SqWrapper::from_file_rank(0, 0), A8_SQUARE);
    /// ```
    pub const fn from_file_rank(file: u8, rank: u8) -> Square {
        8 * rank + (7 - file)
    }
    /// Creates a square from file and rank chars.
    /// Caution: The inputs are unchecked!
    /// # Examples
    ///
    /// ```
    /// assert_eq!(SqWrapper::from_file_rank('a', '8'), A8_SQUARE);
    /// ```
    pub const fn from_char_file_rank(file: char, rank: char) -> Square {
        Self::from_file_rank(file as u8 - b'a', rank as u8 - b'1')
    }
}

/// Returns the absolute difference between the 2 given squares.
/// This is as if the squares of the board were all put in line
/// # Examples
///
/// ```
/// assert_eq!(distance(A8_SQUARE, A1_SQUARE), 8);
/// assert_eq!(distance(A8_SQUARE, D8_SQUARE), 4);
/// ```
pub fn distance(from: Square, to: Square) -> u8 {
    max(from, to) - min(from, to)
}

/// Public `Square` interface
pub trait SquareExt {
    fn forward(self, color: Color) -> Square;
    fn white_behind(self) -> Square;
    fn white_forward(self) -> Square;
    fn forward_left(self) -> Square;
    fn forward_right(self) -> Square;
    fn behind_left(self) -> Square;
    fn behind_right(self) -> Square;
    fn left(self) -> Square;
    fn right(self) -> Square;
    fn rank(self) -> u8;
    fn file(self) -> u8;
    fn rank_file(self) -> (u8, u8);
}

impl SquareExt for Square {
    /// Returns the square in front of the given square from the given point of view
    fn forward(self, color: Color) -> Self {
        if color == WHITE {
            self.white_forward()
        } else {
            // Black going forward is White going backward
            self.white_behind()
        }
    }

    fn white_behind(self) -> Self {
        self - 8
    }

    fn white_forward(self) -> Self {
        self + 8
    }

    fn forward_left(self) -> Self {
        self + 9
    }

    fn forward_right(self) -> Self {
        self + 7
    }

    fn behind_left(self) -> Self {
        self - 7
    }

    fn behind_right(self) -> Self {
        self - 9
    }

    fn left(self) -> Self {
        self + 1
    }

    fn right(self) -> Self {
        self - 1
    }

    // Returns the rank between 0 and 7
    fn rank(self) -> u8 {
        self / 8
    }

    // Returns the file between 0 and 7
    fn file(self) -> u8 {
        7 - self % 8
    }

    fn rank_file(self) -> (u8, u8) {
        (self.rank(), self.file())
    }
}

// SqWrapper implements fmt::Display so we can use it to print a given square
/// Helper to pretty print `Square`s
#[macro_export]
macro_rules! print_square {
    ($square: expr) => {
        SqWrapper($square)
    };
}

use std::fmt;

impl fmt::Display for SqWrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}",
            char::from(b'a' + (7 - (self.0 % 8))),
            self.0 / 8 + 1
        )
    }
}
