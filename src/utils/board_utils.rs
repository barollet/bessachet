#![allow(clippy::unreadable_literal)]

use std::cmp::{max, min};
use std::convert::From;
use std::fmt;
use std::iter::FusedIterator;
use std::ops::{Index, IndexMut};

#[derive(Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum Color {
    BLACK = 0,
    WHITE,
}

// We declare knights to queen first to use the value directly in promotion code in move encoding
enum_from_primitive! {
#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(u8)]
pub enum Piece {
    KNIGHT = 0,
    BISHOP,
    ROOK,
    QUEEN,

    PAWN,
    KING,
}
}

pub type Square = u8;
pub type NonZeroSquare = std::num::NonZeroU8; // Used for en passant square
pub struct SqWrapper(pub Square);

pub type BitBoard = u64;
pub struct BBWraper(pub BitBoard);

impl Piece {
    pub fn to_char(self) -> char {
        match self {
            Piece::PAWN => 'p',
            Piece::KNIGHT => 'n',
            Piece::BISHOP => 'b',
            Piece::ROOK => 'r',
            Piece::QUEEN => 'q',
            Piece::KING => 'k',
        }
    }
}

impl Color {
    pub fn side_multiplier(self) -> f32 {
        [-1.0, 1.0][self as usize]
    }
}

pub const AVAILABLE_PROMOTION: [Piece; 4] =
    [Piece::KNIGHT, Piece::BISHOP, Piece::ROOK, Piece::QUEEN];
pub const PIECES_LIST: [Piece; 6] = [
    Piece::PAWN,
    Piece::KNIGHT,
    Piece::BISHOP,
    Piece::ROOK,
    Piece::QUEEN,
    Piece::KING,
];

// This is an array of size 2 indexable by Color to obtain an attribute that is color dependant
// such as castling squares or castling moves
#[derive(Clone)]
pub struct BlackWhiteAttribute<T>([T; 2]);

impl<T> BlackWhiteAttribute<T> {
    // Black value is declared first
    pub const fn new(black_value: T, white_value: T) -> Self {
        BlackWhiteAttribute([black_value, white_value])
    }
}

impl<T> Index<Color> for BlackWhiteAttribute<T> {
    type Output = T;

    fn index(&self, color: Color) -> &Self::Output {
        &self.0[color as usize]
    }
}

impl<T> IndexMut<Color> for BlackWhiteAttribute<T> {
    fn index_mut(&mut self, color: Color) -> &mut T {
        &mut self.0[color as usize]
    }
}

pub const fn file(number: u32) -> BitBoard {
    0x0101010101010101 << (7 - number)
}

// Some constants declaration
pub const ROW_1: BitBoard = 0xff;
pub const ROW_2: BitBoard = 0xff00;
pub const ROW_4: BitBoard = 0xff000000;
pub const ROW_5: BitBoard = 0xff00000000;
pub const ROW_7: BitBoard = 0xff000000000000;
pub const ROW_8: BitBoard = 0xff00000000000000;
pub const PROMOTION_LINE: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(ROW_1, ROW_8);

pub const FILE_A: BitBoard = file(0);
pub const FILE_B: BitBoard = file(1);
pub const FILE_C: BitBoard = file(2);
pub const FILE_D: BitBoard = file(3);
pub const FILE_E: BitBoard = file(4);
pub const FILE_F: BitBoard = file(5);
pub const FILE_G: BitBoard = file(6);
pub const FILE_H: BitBoard = file(7);
pub const FILES: [BitBoard; 8] = [
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H,
];

pub const SQUARES: BitBoard = BBWraper::full();

pub const A1_SQUARE: Square = SqWrapper::from_char_file_rank('a', '1');
pub const B1_SQUARE: Square = SqWrapper::from_char_file_rank('b', '1');
pub const C1_SQUARE: Square = SqWrapper::from_char_file_rank('c', '1');
pub const D1_SQUARE: Square = SqWrapper::from_char_file_rank('d', '1');
pub const D8_SQUARE: Square = SqWrapper::from_char_file_rank('d', '8');
pub const E1_SQUARE: Square = SqWrapper::from_char_file_rank('e', '1');
pub const F1_SQUARE: Square = SqWrapper::from_char_file_rank('f', '1');
pub const F8_SQUARE: Square = SqWrapper::from_char_file_rank('f', '8');
pub const G1_SQUARE: Square = SqWrapper::from_char_file_rank('g', '1');
pub const H1_SQUARE: Square = SqWrapper::from_char_file_rank('h', '1');
pub const A8_SQUARE: Square = SqWrapper::from_char_file_rank('a', '8');
pub const H8_SQUARE: Square = SqWrapper::from_char_file_rank('h', '8');

impl SqWrapper {
    // Creates a square from file and rank between 0 and 7
    pub const fn from_file_rank(file: u8, rank: u8) -> Square {
        8 * rank + (7 - file)
    }
    // Creates a square from file and rank between 0 and 7
    pub const fn from_char_file_rank(file: char, rank: char) -> Square {
        Self::from_file_rank(file as u8 - b'a', rank as u8 - b'1')
    }

    fn distance(from: Square, to: Square) -> u8 {
        max(from, to) - min(from, to)
    }
}

pub trait SquareExt {
    fn forward(self, Color) -> Square;
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
    fn forward(self, color: Color) -> Self {
        match color {
            Color::WHITE => self.white_forward(),
            Color::BLACK => self.white_behind(),
        }
    }
    // Returns the square behind (1 row below)
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

impl From<SqWrapper> for BitBoard {
    fn from(square: SqWrapper) -> BitBoard {
        1u64 << square.0
    }
}

// Returns a bitboard between the two given squares excluded
pub fn square_mask_between(from: Square, to: Square) -> BitBoard {
    let raw_distance = SqWrapper::distance(from, to);
    let offset_unit = masking_offset(from, to, raw_distance);
    let length = raw_distance / offset_unit - 1;

    let base = ((1 << (offset_unit * length)) - 1) / ((1 << offset_unit) - 1);
    base << (std::cmp::min(from, to) + offset_unit)
}

// Offsets for masking between two squares
// TODO specialized masking for rook and bishop
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

// Transpose trait is for objects that can be transposed into the other player pov.
// Actually the trait is never used as a trait but just for readability
pub trait Transpose {
    fn transpose(&self) -> Self;
}

impl Transpose for Square {
    fn transpose(&self) -> Self {
        63 - self
    }
}

impl Transpose for Color {
    fn transpose(&self) -> Self {
        match self {
            Color::BLACK => Color::WHITE,
            Color::WHITE => Color::BLACK,
        }
    }
}

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

impl BBWraper {
    pub const fn empty() -> BitBoard {
        0
    }

    pub const fn full() -> BitBoard {
        u64::max_value()
    }
}

impl From<BBWraper> for Square {
    fn from(bitboard: BBWraper) -> Square {
        bitboard.0.trailing_zeros() as Square
    }
}

pub trait BitBoardExt {
    fn pop_lsb_as_square(&mut self) -> Square;
    fn has_square(self, square: Square) -> bool;
    fn intersects(self, squares: BitBoard) -> bool;
    fn remove_square(self, square: Square) -> BitBoard;
    fn remove_squares(self, squares: BitBoard) -> BitBoard;
    fn add_square(self, square: Square) -> BitBoard;
    fn population(self) -> u32;
    fn push(self, Color) -> BitBoard;
    fn left_capture(self, Color) -> BitBoard;
    fn right_capture(self, Color) -> BitBoard;
}

impl BitBoardExt for BitBoard {
    // Returns the bitboard containing only the LSB and removes it
    fn pop_lsb_as_square(&mut self) -> Square {
        let singly_populated_bitboard = *self & self.overflowing_neg().0;
        *self ^= singly_populated_bitboard;
        Square::from(BBWraper(singly_populated_bitboard))
    }

    fn has_square(self, square: Square) -> bool {
        self & BitBoard::from(SqWrapper(square)) != 0
    }
    fn intersects(self, squares: BitBoard) -> bool {
        self & squares != 0
    }

    fn remove_square(self, square: Square) -> Self {
        self.remove_squares(BitBoard::from(SqWrapper(square)))
    }
    fn remove_squares(self, squares: Self) -> Self {
        self & !squares
    }
    fn add_square(self, square: Square) -> Self {
        self | BitBoard::from(SqWrapper(square))
    }

    fn population(self) -> u32 {
        self.count_ones()
    }
    fn push(self, color: Color) -> BitBoard {
        match color {
            Color::WHITE => self << 8,
            Color::BLACK => self >> 8,
        }
    }
    fn left_capture(self, color: Color) -> BitBoard {
        match color {
            Color::WHITE => self << 9,
            Color::BLACK => self >> 9,
        }
    }
    fn right_capture(self, color: Color) -> BitBoard {
        match color {
            Color::WHITE => self << 7,
            Color::BLACK => self >> 7,
        }
    }
}

impl Transpose for BitBoard {
    #[clippy::allow(clippy::unreadable_literal)]
    fn transpose(&self) -> Self {
        let mut x = *self;

        let h1 = 0x5555555555555555;
        let h2 = 0x3333333333333333;
        let h4 = 0x0F0F0F0F0F0F0F0F;
        let v1 = 0x00FF00FF00FF00FF;
        let v2 = 0x0000FFFF0000FFFF;

        x = ((x >> 1) & h1) | ((x & h1) << 1);
        x = ((x >> 2) & h2) | ((x & h2) << 2);
        x = ((x >> 4) & h4) | ((x & h4) << 4);
        x = ((x >> 8) & v1) | ((x & v1) << 8);
        x = ((x >> 16) & v2) | ((x & v2) << 16);
        x = (x >> 32) | (x << 32);

        x
    }
}

// Iterates over the bits of the given bitboard and returns the associated Square
// starting from the MSB
impl Iterator for BBWraper {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(self.0.pop_lsb_as_square())
        } else {
            None
        }
    }
}

impl FusedIterator for BBWraper {}

impl fmt::Debug for BBWraper {
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
