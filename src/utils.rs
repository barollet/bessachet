#![allow(clippy::unreadable_literal)]

use std::fmt;

use board::BitBoard;

pub const WHITE: usize = 1;
pub const BLACK: usize = 0;

pub const LAST_LINE: BitBoard = BitBoard::new(0xff00000000000000);
pub const PAWN_FIRST_LINE: BitBoard = BitBoard::new(0xff00);

#[derive(Copy, Clone)]
pub struct Square(pub u8);

impl Square {
    pub fn new(square: u8) -> Self {
        Square(square)
    }

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
