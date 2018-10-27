#![allow(clippy::unreadable_literal)]

use std::fmt;
use std::ops::{Index, IndexMut};

use board::{BitBoard, Board};

/*
pub const WHITE: usize = 1;
pub const BLACK: usize = 0;
*/
#[derive(Copy, Clone)]
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

// We declare knights to queen first to use the value directly in promotion code in move encoding
#[derive(Copy, Clone)]
pub enum Piece {
    KNIGHT = 0,
    BISHOP,
    ROOK,
    QUEEN,

    PAWN,
    KING,
}

pub static AVAILABLE_PROMOTION: [Piece; 4] = [ Piece::KNIGHT, Piece::BISHOP, Piece::ROOK, Piece::QUEEN ];

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

#[derive(Copy, Clone)]
pub struct Square(pub u8);

impl Square {
    #[inline]
    pub fn new(square: u8) -> Self {
        Square(square)
    }

    // Creates a square from file and rank between 0 and 7
    #[inline]
    pub fn from_file_rank(file: u8, rank: u8) -> Self {
        Square(8*rank + (7-file))
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
    pub fn as_bitboard(self) -> BitBoard {
        BitBoard::new(1 << self.0)
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
