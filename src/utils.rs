#![allow(clippy::unreadable_literal)]

use std::fmt;
use std::ops::{Index, IndexMut};

use board::{BitBoard, Board};

/*
pub const WHITE: usize = 1;
pub const BLACK: usize = 0;
*/
#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Color {
    BLACK = 0,
    WHITE,
}

impl Index<Color> for Board {
    type Output = BitBoard;

    fn index(&self, color: Color) -> &Self::Output {
        &self.occupancy[color as usize]
    }
}

impl IndexMut<Color> for Board {
    fn index_mut(&mut self, color: Color) -> &mut BitBoard {
        &mut self.occupancy[color as usize]
    }
}

pub const ROW_2: BitBoard = BitBoard::new(0xff00);
pub const ROW_7: BitBoard = BitBoard::new(0x00ff000000000000);
pub const FILE_A: BitBoard = BitBoard::new(0x8080808080808080);
pub const FILE_H: BitBoard = BitBoard::new(0x0101010101010101);

pub const WHITE_KING_CASTLE_EMPTY: BitBoard = BitBoard::new(0x0000000000000006);
pub const WHITE_KING_CASTLE_CHECK: BitBoard = BitBoard::new(0x000000000000000e);
pub const WHITE_QUEEN_CASTLE_EMPTY: BitBoard = BitBoard::new(0x0000000000000070);
pub const WHITE_QUEEN_CASTLE_CHECK: BitBoard = BitBoard::new(0x0000000000000030);

// Those constants represent a single square but are used as bitboard so we convert them once and
// for all
pub const WHITE_KING_CASTLE_DEST_SQUARE: BitBoard = Square::from_char_rank_file('g', '1').as_bitboard();
pub const WHITE_QUEEN_CASTLE_DEST_SQUARE: BitBoard = Square::from_char_rank_file('c', '1').as_bitboard();

pub const WHITE_ROOK_KING_CASTLE_ORIGIN_SQUARE: Square = Square::from_char_rank_file('h', '1');
pub const WHITE_ROOK_KING_CASTLE_DEST_SQUARE: Square = Square::from_char_rank_file('f', '1');
pub const WHITE_ROOK_QUEEN_CASTLE_ORIGIN_SQUARE: Square = Square::from_char_rank_file('a', '1');
pub const WHITE_ROOK_QUEEN_CASTLE_DEST_SQUARE: Square = Square::from_char_rank_file('d', '1');

// We declare knights to queen first to use the value directly in promotion code in move encoding
enum_from_primitive! {
#[derive(Copy, Clone, PartialEq)]
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

pub static AVAILABLE_PROMOTION: [Piece; 4] = [ Piece::KNIGHT, Piece::BISHOP, Piece::ROOK, Piece::QUEEN ];
pub static PIECES_LIST: [Piece; 6] = [ Piece::PAWN, Piece::KNIGHT, Piece::BISHOP, Piece::ROOK, Piece::QUEEN, Piece::KING ];

impl Index<Piece> for Board {
    type Output = BitBoard;

    fn index(&self, piece: Piece) -> &Self::Output {
        &self.pieces[piece as usize]
    }
}

impl IndexMut<Piece> for Board {
    fn index_mut(&mut self, piece: Piece) -> &mut BitBoard {
        &mut self.pieces[piece as usize]
    }
}

#[derive(Copy, Clone, PartialEq)]
pub struct Square(pub u8);

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

    // Returns the square forward (1 row above)
    #[inline]
    pub fn forward(self) -> Self {
        Square(self.0 + 8)
    }

    #[inline]
    pub fn behind_left(self) -> Self {
        Square(self.0 - 9)
    }

    #[inline]
    pub fn behind_right(self) -> Self {
        Square(self.0 - 7)
    }

    #[inline]
    pub fn reverse(self) -> Self {
        Square(63 - self.0)
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

impl Index<Square> for Board {
    type Output = Option<Piece>;

    fn index(&self, square: Square) -> &Self::Output {
        &self.board_88[square.as_index()]
    }
}

impl IndexMut<Square> for Board {
    fn index_mut(&mut self, square: Square) -> &mut Option<Piece> {
        &mut self.board_88[square.as_index()]
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
