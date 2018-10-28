use std::fmt;
use std::iter::FusedIterator;
use std::mem;
use std::ops::{BitAnd, BitOr, BitOrAssign, Not, Shl, Shr, Mul};

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
// We don't mind if the board is not super dense because we will only clone it a few times
#[derive(Clone)]
pub struct Board {
    pub pieces: [BitBoard; 6],
    pub occupancy: [BitBoard; 2], // black pieces first

    en_passant: BitBoard, // position of an en passant target (0 otherwise)
    castling_rights: u8, // We only use the 4 LSBs 0000 qkQK (same order than FEN notation when white plays)
}

impl Board {
    // Returns a board representing the initial position
    #![allow(clippy::unreadable_literal)]
    pub fn initial_position() -> Self {
        // See utils.rs for the piece order in the array
        Board {
            pieces:    [BitBoard::new(0x4200000000000042),  // Knights
                        BitBoard::new(0x2400000000000024),  // Bishops
                        BitBoard::new(0x8100000000000081),  // Rooks
                        BitBoard::new(0x1000000000000010),  // Queens

                        BitBoard::new(0x00ff00000000ff00),  // Pawns
                        BitBoard::new(0x0800000000000008)], // Kings

            occupancy: [BitBoard::new(0xffff000000000000),  // Black
                        BitBoard::new(0x000000000000ffff)], // White

            en_passant: BitBoard::empty(),
            castling_rights: 0xf,
        }
    }

    fn empty_board() -> Self {
        Board {
            pieces:    [BitBoard::new(0); 6],
            occupancy: [BitBoard::new(0); 2],

            en_passant: BitBoard::empty(),
            castling_rights: 0,
        }
    }

    // TODO clean parser
    pub fn from_fen(fen_string: &str) -> Result<Self, &'static str> {
        let mut board = Board::empty_board();

        let fen_parts: Vec<_> = fen_string.split_whitespace().collect();
        if fen_parts.len() < 4 || fen_parts.len() > 6 {
            return Err("Invalid FEN string");
        }

        // Filling the board
        let piece_lines: Vec<_> = fen_parts[0].split('/').collect();
        if piece_lines.len() != 8 {
            return Err("Invalid FEN string");
        }
        // Starting from the bottom line in white's perspective
        for (piece_line, i) in piece_lines.iter().rev().zip(0u32..) {
            let mut pos = 8;
            for c in piece_line.chars() {
                if let Some(offset) = c.to_digit(10) {
                    pos -= offset;
                } else {
                    pos -= 1;

                    let singly_populated_bitboard = BitBoard::new(1 << (8*i + pos));
                    // Piece bitboard
                    let new_piece = match c.to_ascii_lowercase() {
                        'p' => Piece::PAWN,
                        'n' => Piece::KNIGHT,
                        'b' => Piece::BISHOP,
                        'r' => Piece::ROOK,
                        'q' => Piece::QUEEN,
                        'k' => Piece::KING,
                        _ => return Err("Invalid FEN string"),
                    };
                    board[new_piece] |= singly_populated_bitboard;

                    // Occupancy
                    let color = if c.is_ascii_lowercase() { Color::BLACK } else { Color::WHITE };
                    board[color] |= singly_populated_bitboard;
                }
            }
        }

        // Setting metadata

        // En passant
        if fen_parts[3].len() == 2 {
            let chars: Vec<_> = fen_parts[3].chars().collect();
            board.en_passant = Square::from_char_rank_file(chars[0], chars[1]).as_bitboard();
        }
        // castling
        if fen_parts[2] != "-" {
            for c in fen_parts[2].chars() {
                board.castling_rights |= match c {
                    'K' => 0x1,
                    'Q' => 0x2,
                    'k' => 0x4,
                    'q' => 0x8,
                    _ => return Err("Invalid FEN string"),
                }
            }
        }

        Ok(board)
    }

    #[inline]
    pub fn occupied_squares(&self) -> BitBoard {
        self[Color::WHITE] | self[Color::BLACK]
    }

    #[inline]
    pub fn empty_squares(&self) -> BitBoard {
        self.occupied_squares().not()
    }

    fn switch_side(&mut self) {
        for bitboard in &mut self.pieces {
            *bitboard = bitboard.reverse()
        }
        let mut white_pieces = self[Color::WHITE]; // Happy borrow checker
        mem::swap(&mut white_pieces, &mut self[Color::BLACK]);

        self.en_passant = self.en_passant.reverse();
        self.castling_rights = ((self.castling_rights << 2) + (self.castling_rights >> 2)) & 0xf;
    }

    #[inline]
    pub fn en_passant_target_index(&self) -> usize {
        64usize - (self.en_passant >> 32).0.leading_zeros() as usize
    }

    #[inline]
    pub fn en_passant_square(&self) -> Square {
        self.en_passant.as_square()
    }

    #[inline]
    pub fn king_castling(&self) -> BitBoard {
        WHITE_KING_CASTLE_DEST_SQUARE * u64::from(self.castling_rights & 0x1)
    }

    #[inline]
    pub fn queen_castling(&self) -> BitBoard {
        WHITE_QUEEN_CASTLE_DEST_SQUARE * u64::from(self.castling_rights & 0x2)
    }
}

#[derive(Copy, Clone)]
pub struct BitBoard(pub u64);

impl BitBoard {
    #[inline]
    pub const fn new(bitboard: u64) -> Self {
        BitBoard(bitboard)
    }

    pub const fn empty() -> Self {
        BitBoard(0)
    }

    #[inline]
    pub fn as_square(self) -> Square {
        Square(63u8 - self.0.leading_zeros() as u8)
    }

    #[inline]
    pub fn as_index(self) -> usize {
        63usize - self.0.leading_zeros() as usize
    }

    // Returns the bitboard containing only the LSB and removes it
    #[inline]
    pub fn pop_lsb_bitboard(&mut self) -> Self {
        let singly_populated_bitboard = self.0 & self.0.overflowing_neg().0;
        self.0 ^= singly_populated_bitboard;
        BitBoard(singly_populated_bitboard)
    }

    #[inline]
    pub fn reverse(self) -> Self {
        BitBoard(self.0.reverse_bits())
    }
}

// Iterates over the singly populated bitboards of the given bitboard
// starting from the MSB
impl Iterator for BitBoard {
    type Item = BitBoard;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(self.pop_lsb_bitboard())
        } else  {
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

impl FusedIterator for BitBoard {}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self[Piece::PAWN])?;
        write!(f, "{:?}", self[Piece::KNIGHT])?;
        write!(f, "{:?}", self[Piece::BISHOP])?;
        write!(f, "{:?}", self[Piece::ROOK])?;
        write!(f, "{:?}", self[Piece::QUEEN])?;
        write!(f, "{:?}", self[Piece::KING])?;
        write!(f, "{:?}", self[Color::WHITE])?;
        write!(f, "{:?}", self[Color::BLACK])
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
