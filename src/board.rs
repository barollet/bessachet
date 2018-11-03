use std::fmt;
use std::iter::FusedIterator;
use std::mem;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, Shl, Shr, Mul};

use utils::*;

use move_generation::*;

// The board is represented as a set of bitboards
// See: https://www.chessprogramming.org/Bitboards

// LSB is the square lower-right and the MSB is the upper-left one.
// 63 62 61 60 59 58 57 56      black pieces
// 55 54 53 52 51 50 49 48      black pawns
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

    pub en_passant: Option<Square>, // position of an en passant target (0 otherwise)
    castling_rights: u8, // We only use the 4 LSBs 0000 qkQK (same order starting from the LSB than FEN notation when white plays)

    halfmove_clock: u8,

    pub board_88: [Option<Piece>; 64],
}

const REMOVE_ALL_WHITE_CASTLING_RIGHTS: u8 = 0b1100;
const REMOVE_QUEEN_SIDE_WHITE_CASTLING_RIGHTS: u8 = 0b1101;
const REMOVE_KING_SIDE_WHITE_CASTLING_RIGHTS: u8 = 0b1110;

impl Board {
    // Returns a board representing the initial position
    #![allow(clippy::unreadable_literal)]
    pub fn initial_position() -> Self {
        // See utils.rs for the piece order in the array
        let mut board = Board {
            pieces:    [BitBoard::new(0x4200000000000042),  // Knights
                        BitBoard::new(0x2400000000000024),  // Bishops
                        BitBoard::new(0x8100000000000081),  // Rooks
                        BitBoard::new(0x1000000000000010),  // Queens

                        BitBoard::new(0x00ff00000000ff00),  // Pawns
                        BitBoard::new(0x0800000000000008)], // Kings

            occupancy: [BitBoard::new(0xffff000000000000),  // Black
                        BitBoard::new(0x000000000000ffff)], // White

            en_passant: None,
            castling_rights: 0xf,

            halfmove_clock: 0,

            board_88: [None; 64],
        };

        board.fill_88();

        board
    }

    fn empty_board() -> Self {
        Board {
            pieces:    [BitBoard::new(0); 6],
            occupancy: [BitBoard::new(0); 2],

            en_passant: None,
            castling_rights: 0,

            halfmove_clock: 0,

            board_88: [None; 64],
        }
    }

    // For make and unmake, we assume that the provided move is valid
    // If not the program can panic! or remain in an unconsistent state
    pub fn make(&mut self, mov: Move) {
        // Move the piece
        let moved_piece = self[mov.origin_square()].unwrap();

        self.halfmove_clock = if moved_piece == Piece::PAWN {
            0
        } else {
            self.halfmove_clock+1
        };

        // Capture
        // Not triggered by en passant capture
        if let Some(captured_piece) = mov.get_captured_piece() {
            self.delete_piece(mov.destination_square(), captured_piece, Color::BLACK);

            self.halfmove_clock = 0;
        }

        // We move the piece after the capture
        self.move_piece(mov.origin_square(), mov.destination_square(), moved_piece);

        // En passant capture
        if let Some(en_passant_captured_square) = mov.get_en_passant_capture_square() {
            self.delete_piece(en_passant_captured_square, Piece::PAWN, Color::BLACK);

            self.halfmove_clock = 0;
        }

        // Castling, move the other rook
        if let Some((rook_from_square, rook_dest_square)) = mov.get_castling_rook() {
            self.move_piece(rook_from_square, rook_dest_square, Piece::ROOK);
        }
        // Double pawn push, set the en passant target
        self.en_passant = mov.get_en_passant_target_square();
        // Promotion
        if let Some(promotion_piece) = mov.get_promotion_piece() {
            self.delete_piece(mov.destination_square(), Piece::PAWN, Color::WHITE);
            self.create_piece(mov.destination_square(), promotion_piece, Color::WHITE);

            self.halfmove_clock = 0;
        }
        // Castling rights update
        if moved_piece == Piece::KING {
            self.castling_rights &= REMOVE_ALL_WHITE_CASTLING_RIGHTS;
        } else if moved_piece == Piece::ROOK {
            if mov.origin_square() == WHITE_ROOK_KING_CASTLE_ORIGIN_SQUARE {
                self.castling_rights &= REMOVE_KING_SIDE_WHITE_CASTLING_RIGHTS;
            } else if mov.origin_square() == WHITE_ROOK_QUEEN_CASTLE_ORIGIN_SQUARE {
                self.castling_rights &= REMOVE_QUEEN_SIDE_WHITE_CASTLING_RIGHTS;
            }
        }
    }

    pub fn unmake(&mut self, mov: Move) {
        // Move the piece back
        let moved_piece = self[mov.destination_square()].unwrap();

        self.move_piece(mov.destination_square(), mov.origin_square(), moved_piece);

        // Capture
        if let Some(captured_piece) = mov.get_captured_piece() {
            self.create_piece(mov.destination_square(), captured_piece, Color::BLACK);
        }
        // En passant capture
        if let Some(en_passant_captured_square) = mov.get_en_passant_capture_square() {
            self.create_piece(en_passant_captured_square, Piece::PAWN, Color::BLACK);
        }

        // Castling, move the other rook
        if let Some((rook_from_square, rook_dest_square)) = mov.get_castling_rook() {
            self.move_piece(rook_dest_square, rook_from_square, Piece::ROOK);
        }

        // Promotion
        if let Some(promotion_piece) = mov.get_promotion_piece() {
            self.delete_piece(mov.destination_square(), promotion_piece, Color::WHITE);
        }

        // restore en passant, caslting rights and halfmove clock from the move metadata
        let en_passant_square = mov.get_board_state(EN_PASSANT_SQUARE_BITS_OFFSET, EN_PASSANT_SQUARE_BITS_SIZE);
        self.en_passant = if en_passant_square != 0 {
            Some(Square::new(en_passant_square))
        } else {
            None
        };
        self.halfmove_clock = mov.get_board_state(HALFMOVE_CLOCK_BITS_OFFSET, HALFMOVE_CLOCK_BITS_SIZE);
        self.castling_rights = mov.get_board_state(CASTLING_RIGHTS_BITS_OFFSET, CASTLING_RIGHTS_BITS_SIZE);
    }

    #[inline]
    fn move_piece(&mut self, from: Square, to: Square, moved_piece: Piece) {
        self.delete_piece(from, moved_piece, Color::WHITE);
        self.create_piece(to, moved_piece, Color::WHITE);
    }

    #[inline]
    fn delete_piece(&mut self, square: Square, piece: Piece, color: Color) {
        let reset_mask = !square.as_bitboard();
        self[piece] &= reset_mask;
        self[color] &= reset_mask;
        self[square] = None;
    }

    #[inline]
    fn create_piece(&mut self, square: Square, piece: Piece, color: Color) {
        let set_mask = square.as_bitboard();
        self[piece] |= set_mask;
        self[color] |= set_mask;
        self[square] = Some(piece);
    }

    // Fills the redondant 88 representation from the bitboards representation
    fn fill_88(&mut self) {
        for piece in &PIECES_LIST {
            for square in self[*piece] {
                self[square] = Some(*piece);
            }
        }
    }

    // Decorate a move with the irreversible states of the board
    // s.a. en passant, castling rights and halfmove clock
    #[inline]
    pub fn decorate_move(&self, mov: Move) -> Move {
        mov.set_board_state(self.castling_rights, CASTLING_RIGHTS_BITS_OFFSET)
            .set_board_state(self.en_passant.map_or(0, |square| square.0), EN_PASSANT_SQUARE_BITS_OFFSET)
            .set_board_state(self.halfmove_clock, HALFMOVE_CLOCK_BITS_OFFSET)
    }

    #[inline]
    pub fn occupied_squares(&self) -> BitBoard {
        self[Color::WHITE] | self[Color::BLACK]
    }

    #[inline]
    pub fn empty_squares(&self) -> BitBoard {
        !self.occupied_squares()
    }

    fn switch_side(&mut self) {
        for bitboard in &mut self.pieces {
            *bitboard = bitboard.reverse()
        }
        let mut white_pieces = self[Color::WHITE]; // Happy borrow checker
        mem::swap(&mut white_pieces, &mut self[Color::BLACK]);

        self.en_passant = self.en_passant.map(|square| square.reverse());
        self.castling_rights = ((self.castling_rights << 2) + (self.castling_rights >> 2)) & 0xf;

        for (id_end, id_beg) in (32..64).rev().zip(0..) {
            let mut beg = self.board_88[id_beg];
            mem::swap(&mut beg, &mut self.board_88[id_end]);
        }
    }

    #[inline]
    pub fn can_king_castle(&self) -> bool {
        (self.castling_rights & 0x1 != 0) // right to castle kingside
        && (WHITE_KING_CASTLE_EMPTY & self.occupied_squares() == 0) // none of the squares on the way are occupied
        && (WHITE_KING_CASTLE_CHECK.all(|square| !self.is_in_check(square, Color::BLACK))) // squares crossed by the king are in check
    }

    #[inline]
    pub fn can_queen_castle(&self) -> bool {
        (self.castling_rights & 0x2 != 0) // right to castle queenside
        && (WHITE_QUEEN_CASTLE_EMPTY & self.occupied_squares() == 0) // none of the squares on the way are occupied
        && (WHITE_QUEEN_CASTLE_CHECK.all(|square| !self.is_in_check(square, Color::BLACK))) // squares crossed by the king are in check
    }

    // returns if the given square is checked by a piece of the given color
    // TODO do it again but at the double board level
    // TODO do it efficiently with only attacks and not captures
    pub fn is_in_check(&self, square: Square, color: Color) -> bool {
        let mut switch = self.clone();
        switch.switch_side();
        match color {
            Color::BLACK => switch.possible_moves() // TODO use attack map instead
                .any(|mov| mov.destination_square() == square),
            Color::WHITE => self.possible_moves()
                .any(|mov| mov.destination_square() == square),
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
            board.en_passant = Some(Square::from_char_rank_file(chars[0], chars[1]));
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

        board.fill_88();

        Ok(board)
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
        Square(self.0.trailing_zeros() as u8)
    }

    // Returns the bitboard containing only the LSB and removes it
    #[inline]
    pub fn pop_lsb_as_square(&mut self) -> Square {
        let singly_populated_bitboard = self.0 & self.0.overflowing_neg().0;
        self.0 ^= singly_populated_bitboard;
        BitBoard(singly_populated_bitboard).as_square()
    }

    #[inline]
    pub fn reverse(self) -> Self {
        BitBoard(self.0.reverse_bits())
    }
}

// Iterates over the bits of the given bitboard and returns the associated Square
// starting from the MSB
impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 != 0 {
            Some(self.pop_lsb_as_square())
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
