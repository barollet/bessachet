#![allow(clippy::unreadable_literal)]

use std::fmt;
use std::ops::{Index, IndexMut};

use board::BitBoard;

#[derive(Copy, Clone)]
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

pub static AVAILABLE_PROMOTION: [Piece; 4] = [ Piece::KNIGHT, Piece::BISHOP, Piece::ROOK, Piece::QUEEN ];
pub static PIECES_LIST: [Piece; 6] = [ Piece::PAWN, Piece::KNIGHT, Piece::BISHOP, Piece::ROOK, Piece::QUEEN, Piece::KING ];

#[derive(Copy, Clone, PartialEq)]
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

// Some constants declaration
pub const ROW_2: BitBoard = BitBoard::new(0xff00);
pub const ROW_5: BitBoard = BitBoard::new(0x000000ff00000000);
pub const ROW_7: BitBoard = BitBoard::new(0x00ff000000000000);
pub const FILE_A: BitBoard = BitBoard::new(0x8080808080808080);
pub const FILE_H: BitBoard = BitBoard::new(0x0101010101010101);

pub const EN_PASSANT_TARGET_LINE: BitBoard = ROW_5;

pub const A1_SQUARE: Square = Square::from_char_rank_file('a', '1');
pub const B1_SQUARE: Square = Square::from_char_rank_file('b', '1');
pub const C1_SQUARE: Square = Square::from_char_rank_file('c', '1');
pub const D1_SQUARE: Square = Square::from_char_rank_file('d', '1');
pub const E1_SQUARE: Square = Square::from_char_rank_file('e', '1');
pub const F1_SQUARE: Square = Square::from_char_rank_file('f', '1');
pub const G1_SQUARE: Square = Square::from_char_rank_file('g', '1');
pub const H1_SQUARE: Square = Square::from_char_rank_file('h', '1');
pub const A8_SQUARE: Square = Square::from_char_rank_file('a', '8');
pub const C8_SQUARE: Square = Square::from_char_rank_file('c', '8');
pub const D8_SQUARE: Square = Square::from_char_rank_file('d', '8');
pub const H8_SQUARE: Square = Square::from_char_rank_file('h', '8');

// [Black masks, White masks]
pub const KING_CASTLE_EMPTY: BlackWhiteAttribute<BitBoard>
    = BlackWhiteAttribute::new(BitBoard::new(0x0000000000000060), BitBoard::new(0x0000000000000006));
pub const KING_CASTLE_CHECK: BlackWhiteAttribute<BitBoard>
    = BlackWhiteAttribute::new(BitBoard::new(0x0000000000000070), BitBoard::new(0x000000000000000e));
pub const QUEEN_CASTLE_EMPTY: BlackWhiteAttribute<BitBoard>
    = BlackWhiteAttribute::new(BitBoard::new(0x000000000000000e), BitBoard::new(0x0000000000000070));
pub const QUEEN_CASTLE_CHECK: BlackWhiteAttribute<BitBoard>
    = BlackWhiteAttribute::new(BitBoard::new(0x000000000000001c), BitBoard::new(0x0000000000000030));

// [Black square, White square]
pub const KING_CASTLE_ROOK_ORIGIN_SQUARES: BlackWhiteAttribute<Square> = BlackWhiteAttribute::new(A1_SQUARE, H1_SQUARE); // H8 transpose for black
pub const KING_CASTLE_ROOK_DEST_SQUARES: BlackWhiteAttribute<Square> = BlackWhiteAttribute::new(F1_SQUARE, C1_SQUARE);
pub const QUEEN_CASTLE_ROOK_ORIGIN_SQUARES: BlackWhiteAttribute<Square> = BlackWhiteAttribute::new(H1_SQUARE, A1_SQUARE);
pub const QUEEN_CASTLE_ROOK_DEST_SQUARES: BlackWhiteAttribute<Square> = BlackWhiteAttribute::new(E1_SQUARE, D1_SQUARE);

impl Square {
    #[inline]
    pub const fn new(square: u8) -> Self {
        Square(square)
    }

    // Creates a square from file and rank between 0 and 7
    #[inline]
    pub const fn from_file_rank(file: u8, rank: u8) -> Self {
        Square(8*rank + (7-file))
    }

    // Returns the square behind (1 row below)
    #[inline]
    pub fn behind(self) -> Self {
        Square(self.0 - 8)
    }

    #[inline]
    pub fn forward(self) -> Self {
        Square(self.0 + 8)
    }

    #[inline]
    pub fn behind_left(self) -> Self {
        Square(self.0 - 7)
    }

    #[inline]
    pub fn behind_right(self) -> Self {
        Square(self.0 - 9)
    }

    // Creates a square from file and rank between 0 and 7
    #[inline]
    pub const fn from_char_rank_file(file: char, rank: char) -> Self {
        Self::from_file_rank(file as u8 - b'a', rank as u8 - b'1')
    }

    // Returns the rank between 0 and 7
    #[inline]
    pub fn rank(self) -> u8 {
        self.0 / 8
    }

    // Returns the file between 0 and 7
    #[inline]
    pub fn file(self) -> u8 {
        7 - self.0 % 8
    }

    #[inline]
    pub fn rank_file(self) -> (u8, u8) {
        (self.rank(), self.file())
    }

    #[inline]
    pub const fn as_bitboard(self) -> BitBoard {
        BitBoard::new(1 << self.0)
    }

    #[inline]
    pub fn as_index(self) -> usize {
        self.0 as usize
    }
}

// Transpose trait is for objects that can be transposed into the other player pov.
// Actually the trait is never used as a trait but just for readability
pub trait Transpose {
    fn transpose(&self) -> Self;
}

impl Transpose for Square {
    #[inline]
    fn transpose(&self) -> Self {
        Square(63 - self.0)
    }
}

impl Transpose for Color {
    #[inline]
    fn transpose(&self) -> Self {
        match self {
            Color::BLACK => Color::WHITE,
            Color::WHITE => Color::BLACK,
        }
    }
}

impl fmt::Debug for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Square({}{:?} {:?})", char::from(b'a' + (7 - (self.0 % 8))), self.0 / 8 + 1, self.0)
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{:?}", char::from(b'a' + (7 - (self.0 % 8))), self.0 / 8 + 1)
    }
}
