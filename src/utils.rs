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

pub const LAST_LINE: BitBoard = BitBoard::new(0xff00000000000000);
pub const PAWN_FIRST_LINE: BitBoard = BitBoard::new(0xff00);

#[derive(Copy, Clone)]
pub enum Pieces {
    PAWN = 0,
    KNIGHT,
    BISHOP,
    ROOK,
    QUEEN,
    KING,
}

impl Index<Pieces> for Board {
    type Output = BitBoard;

    fn index(&self, piece: Pieces) -> &Self::Output {
        &self.pieces[piece as usize]
    }
}

impl IndexMut<Pieces> for Board {
    fn index_mut(&mut self, piece: Pieces) -> &mut BitBoard {
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
