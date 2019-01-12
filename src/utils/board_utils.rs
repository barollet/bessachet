#![allow(clippy::unreadable_literal)]

use std::cmp::{max, min, Ordering};
use std::fmt;
use std::iter::FusedIterator;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Mul, Not, Shl, Shr};
use std::ops::{Div, Index, IndexMut, Rem, Sub};

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

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Square(pub u8);

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

pub const fn file(number: u32) -> u64 {
    0x0101010101010101 << (7 - number)
}

// Some constants declaration
pub const ROW_2: BitBoard = BitBoard::new(0xff00);
pub const ROW_7: BitBoard = BitBoard::new(0x00ff000000000000);
pub const ROW_8: BitBoard = BitBoard::new(0xff00000000000000);

pub const FILE_A: BitBoard = BitBoard::new(file(0));
pub const FILE_B: BitBoard = BitBoard::new(file(1));
pub const FILE_C: BitBoard = BitBoard::new(file(2));
pub const FILE_D: BitBoard = BitBoard::new(file(3));
pub const FILE_E: BitBoard = BitBoard::new(file(4));
pub const FILE_F: BitBoard = BitBoard::new(file(5));
pub const FILE_G: BitBoard = BitBoard::new(file(6));
pub const FILE_H: BitBoard = BitBoard::new(file(7));
pub const FILES: [BitBoard; 8] = [
    FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H,
];

pub const A1_SQUARE: Square = Square::from_char_file_rank('a', '1');
pub const B1_SQUARE: Square = Square::from_char_file_rank('b', '1');
pub const C1_SQUARE: Square = Square::from_char_file_rank('c', '1');
pub const D1_SQUARE: Square = Square::from_char_file_rank('d', '1');
pub const E1_SQUARE: Square = Square::from_char_file_rank('e', '1');
pub const F1_SQUARE: Square = Square::from_char_file_rank('f', '1');
pub const G1_SQUARE: Square = Square::from_char_file_rank('g', '1');
pub const H1_SQUARE: Square = Square::from_char_file_rank('h', '1');
pub const A8_SQUARE: Square = Square::from_char_file_rank('a', '8');
pub const H8_SQUARE: Square = Square::from_char_file_rank('h', '8');

// [Black masks, White masks]
pub const KING_CASTLE_EMPTY: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(
    BitBoard::new(0x0000000000000060),
    BitBoard::new(0x0000000000000006),
);
pub const KING_CASTLE_CHECK: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(
    BitBoard::new(0x0000000000000070),
    BitBoard::new(0x000000000000000e),
);
pub const QUEEN_CASTLE_EMPTY: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(
    BitBoard::new(0x000000000000000e),
    BitBoard::new(0x0000000000000070),
);
pub const QUEEN_CASTLE_CHECK: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(
    BitBoard::new(0x000000000000001c),
    BitBoard::new(0x0000000000000038),
);

// [Black square, White square]
pub const KING_CASTLE_ROOK_ORIGIN_SQUARES: BlackWhiteAttribute<Square> =
    BlackWhiteAttribute::new(A1_SQUARE, H1_SQUARE); // H8 transpose for black
pub const KING_CASTLE_ROOK_DEST_SQUARES: BlackWhiteAttribute<Square> =
    BlackWhiteAttribute::new(C1_SQUARE, F1_SQUARE);
pub const QUEEN_CASTLE_ROOK_ORIGIN_SQUARES: BlackWhiteAttribute<Square> =
    BlackWhiteAttribute::new(H1_SQUARE, A1_SQUARE);
pub const QUEEN_CASTLE_ROOK_DEST_SQUARES: BlackWhiteAttribute<Square> =
    BlackWhiteAttribute::new(E1_SQUARE, D1_SQUARE);

impl Square {
    pub const fn new(square: u8) -> Self {
        Square(square)
    }

    // Creates a square from file and rank between 0 and 7
    pub const fn from_file_rank(file: u8, rank: u8) -> Self {
        Square(8 * rank + (7 - file))
    }

    // Returns the square behind (1 row below)
    pub const fn behind(self) -> Self {
        Square(self.0 - 8)
    }

    pub const fn forward(self) -> Self {
        Square(self.0 + 8)
    }

    pub const fn forward_left(self) -> Self {
        Square(self.0 + 9)
    }

    pub const fn forward_right(self) -> Self {
        Square(self.0 + 7)
    }

    pub const fn behind_left(self) -> Self {
        Square(self.0 - 7)
    }

    pub const fn behind_right(self) -> Self {
        Square(self.0 - 9)
    }

    // Creates a square from file and rank between 0 and 7
    pub const fn from_char_file_rank(file: char, rank: char) -> Self {
        Self::from_file_rank(file as u8 - b'a', rank as u8 - b'1')
    }

    // Returns the rank between 0 and 7
    pub const fn rank(self) -> u8 {
        self.0 / 8
    }

    // Returns the file between 0 and 7
    pub const fn file(self) -> u8 {
        7 - self.0 % 8
    }

    pub const fn rank_file(self) -> (u8, u8) {
        (self.rank(), self.file())
    }

    pub const fn as_bitboard(self) -> BitBoard {
        BitBoard::new(1 << self.0)
    }

    pub const fn as_index(self) -> usize {
        self.0 as usize
    }
}

// Returns a bitboard between the two given squares included
/*
pub fn square_mask_included_with(from: Square, to: Square) -> BitBoard {
    let raw_distance = from - to;
    let offset_unit = masking_offset(from, to);
    let length = raw_distance / offset_unit + 1;

    let base = ((1 << (offset_unit*length)) - 1) / ((1 << offset_unit) - 1);
    BitBoard::new(base << std::cmp::min(from, to).0)
}
*/

// Returns a bitboard between the two given squares excluded
pub fn square_mask_between(from: Square, to: Square) -> BitBoard {
    let raw_distance = from - to;
    let offset_unit = masking_offset(from, to);
    let length = raw_distance / offset_unit - 1;

    let base = ((1 << (offset_unit * length)) - 1) / ((1 << offset_unit) - 1);
    BitBoard::new(base << (std::cmp::min(from, to).0 + offset_unit))
}
// Offsets for masking between two squares
// TODO specialized masking for rook and bishop
fn masking_offset(from_square: Square, target_square: Square) -> u8 {
    if target_square % 8 == from_square % 8 {
        // Vertical
        8
    } else if target_square / 8 == from_square / 8 {
        // Horizontal
        1
    } else if (target_square - from_square) % 9 == 0 {
        // Antidiagonal
        9
    } else {
        // Diagonal
        7
    }
}

// Sub returns the distance between two squares as a u8
impl Sub for Square {
    type Output = u8;
    fn sub(self, other: Square) -> Self::Output {
        max(self.0, other.0) - min(self.0, other.0)
    }
}

impl Ord for Square {
    fn cmp(&self, other: &Square) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl PartialOrd for Square {
    fn partial_cmp(&self, other: &Square) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// TODO maybe change this to row and file
impl Rem<u8> for Square {
    type Output = Square;
    fn rem(self, rhs: u8) -> Self::Output {
        Square(self.0 % rhs)
    }
}

impl Div<u8> for Square {
    type Output = Square;
    fn div(self, rhs: u8) -> Self::Output {
        Square(self.0 / rhs)
    }
}
// Transpose trait is for objects that can be transposed into the other player pov.
// Actually the trait is never used as a trait but just for readability
pub trait Transpose {
    fn transpose(&self) -> Self;
}

impl Transpose for Square {
    fn transpose(&self) -> Self {
        Square(63 - self.0)
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

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Square({}{:?} {:?})",
            char::from(b'a' + (7 - (self.0 % 8))),
            self.0 / 8 + 1,
            self.0
        )
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{:?}",
            char::from(b'a' + (7 - (self.0 % 8))),
            self.0 / 8 + 1
        )
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct BitBoard(pub u64);

impl BitBoard {
    pub const fn new(bitboard: u64) -> Self {
        BitBoard(bitboard)
    }

    pub const fn empty() -> Self {
        BitBoard(0)
    }

    pub const fn full() -> Self {
        BitBoard(u64::max_value())
    }

    // Has to be called on a non empty Bitboard
    pub fn as_square(self) -> Square {
        Square(self.0.trailing_zeros() as u8)
    }

    // Returns the bitboard containing only the LSB and removes it
    pub fn pop_lsb_as_square(&mut self) -> Square {
        let singly_populated_bitboard = self.0 & self.0.overflowing_neg().0;
        self.0 ^= singly_populated_bitboard;
        BitBoard(singly_populated_bitboard).as_square()
    }

    pub fn has_square(self, square: Square) -> bool {
        self & square.as_bitboard() != 0
    }
    pub fn has_squares(self, squares: BitBoard) -> bool {
        self & squares != 0
    }

    pub fn remove_square(self, square: Square) -> Self {
        self.remove_squares(square.as_bitboard())
    }
    pub fn remove_squares(self, squares: Self) -> Self {
        self & !squares
    }
    pub fn add_square(self, square: Square) -> Self {
        self | square.as_bitboard()
    }

    pub fn population(self) -> u32 {
        self.0.count_ones()
    }
}

impl Transpose for BitBoard {
    #[clippy::allow(clippy::unreadable_literal)]
    fn transpose(&self) -> Self {
        let mut x = self.0;

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

        BitBoard(x)
    }
}

// Iterates over the bits of the given bitboard and returns the associated Square
// starting from the MSB
impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(self.pop_lsb_as_square())
        } else {
            None
        }
    }
}

impl BitAnd for BitBoard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        BitBoard::new(self.0 & rhs.0)
    }
}

impl BitAndAssign for BitBoard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitOr for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        BitBoard::new(self.0 | rhs.0)
    }
}

impl BitOrAssign for BitBoard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self {
        BitBoard::new(!self.0)
    }
}

impl Shl<u32> for BitBoard {
    type Output = Self;
    fn shl(self, rhs: u32) -> Self {
        BitBoard::new(self.0 << rhs)
    }
}

impl Shr<u32> for BitBoard {
    type Output = Self;
    fn shr(self, rhs: u32) -> Self {
        BitBoard::new(self.0 >> rhs)
    }
}

impl Mul<u64> for BitBoard {
    type Output = Self;
    fn mul(self, rhs: u64) -> Self {
        BitBoard::new(self.0 * rhs)
    }
}

impl PartialEq<u64> for BitBoard {
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

impl FusedIterator for BitBoard {}

impl fmt::Debug for BitBoard {
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
