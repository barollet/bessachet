use std::fmt;
use std::iter::FusedIterator;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, Shl, Shr, Mul};
use std::ops::{Index, IndexMut};

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
    halfboards: BlackWhiteAttribute<HalfBoard>, // The same position from black and white pov

    castling_rights: u8, // We only use the 4 LSBs 0000 qkQK (same order starting from the LSB than FEN notation when white plays)
    halfmove_clock: u8,

    pub side_to_move: Color,

    capture_stack: [Piece; 32], // Pawns captured en passant are not but here (there is the code in the move encoding for en passant)
    capture_stack_index: usize,
}

// The board keeps two redundant representation so the move generation is white to move only
// Both representation are updated incrementaly together
#[derive(Clone)]
pub struct HalfBoard {
    pub pieces: [BitBoard; 6],
    pub occupancy: BlackWhiteAttribute<BitBoard>, // black pieces first

    pub en_passant: Option<Square>, // position of an en passant target (0 otherwise)

    pub board_88: [Option<Piece>; 64],
}

// [black mask, white mask]
const REMOVE_ALL_CASTLING_RIGHTS: BlackWhiteAttribute<u8> = BlackWhiteAttribute::new(0b0011, 0b1100);
const REMOVE_QUEEN_SIDE_CASTLING_RIGHTS: BlackWhiteAttribute<u8> = BlackWhiteAttribute::new(0b0111, 0b1101);
const REMOVE_KING_SIDE_CASTLING_RIGHTS: BlackWhiteAttribute<u8> = BlackWhiteAttribute::new(0b1011, 0b1110);

pub const KING_CASTLING_RIGHTS_MASKS: BlackWhiteAttribute<u8> = BlackWhiteAttribute::new(0b0100, 0b0001);
pub const QUEEN_CASTLING_RIGHTS_MASKS: BlackWhiteAttribute<u8> = BlackWhiteAttribute::new(0b1000, 0b0010);

// A HalfBoard holds the current position with the point of view of White or Black
// Both pov think they are white so the move generation is white to move only for
// both HalfBoard.
impl HalfBoard {
    // Returns a half-board representing the initial position
    // This is from white pov so it has to be transposed to obtain black pov
    #![allow(clippy::unreadable_literal)]
    pub fn initial_position() -> Self {
        // See utils.rs for the piece order in the array
        let mut board = Self {
            pieces:    [BitBoard::new(0x4200000000000042),  // Knights
                        BitBoard::new(0x2400000000000024),  // Bishops
                        BitBoard::new(0x8100000000000081),  // Rooks
                        BitBoard::new(0x1000000000000010),  // Queens

                        BitBoard::new(0x00ff00000000ff00),  // Pawns
                        BitBoard::new(0x0800000000000008)], // Kings

            occupancy: BlackWhiteAttribute::new(BitBoard::new(0xffff000000000000),  // Black
                                                BitBoard::new(0x000000000000ffff)), // White

            en_passant: None,

            board_88: [None; 64],
        };

        board.fill_88();

        board
    }

    pub fn empty_board() -> Self {
        Self {
            pieces:    [BitBoard::new(0); 6],
            occupancy: BlackWhiteAttribute::new(BitBoard::empty(), BitBoard::empty()),

            en_passant: None,

            board_88: [None; 64],
        }
    }

    // Fills the redondant 88 representation from the bitboards representation
    // Has to be used at initialization because the empty squares are not set back to None
    fn fill_88(&mut self) {
        for piece in &PIECES_LIST {
            for square in self[*piece] {
                self[square] = Some(*piece);
            }
        }
    }

    pub fn occupied_squares(&self) -> BitBoard {
        self[Color::WHITE] | self[Color::BLACK]
    }

    pub fn empty_squares(&self) -> BitBoard {
        !self.occupied_squares()
    }

    // Helper functions to make and unmake moves
    fn move_piece(&mut self, from: Square, to: Square, moved_piece: Piece, color: Color) {
        self.delete_piece(from, moved_piece, color);
        self.create_piece(to, moved_piece, color);
    }

    fn delete_piece(&mut self, square: Square, piece: Piece, color: Color) {
        let reset_mask = !square.as_bitboard();
        self[piece] &= reset_mask;
        self[color] &= reset_mask;
        self[square] = None;
    }

    fn create_piece(&mut self, square: Square, piece: Piece, color: Color) {
        let set_mask = square.as_bitboard();
        self[piece] |= set_mask;
        self[color] |= set_mask;
        self[square] = Some(piece);
    }
}

// This is supposed to be called only at initialization
impl Transpose for HalfBoard {
    fn transpose(&self) -> Self {
        let mut transposed_halfboard = Self::empty_board();

        // Transposing occupation
        for (bitboard, transposed_bitboard) in self.pieces.iter().zip(&mut transposed_halfboard.pieces) {
            *transposed_bitboard = bitboard.transpose();
        }

        // Transposing colors
        transposed_halfboard[Color::WHITE] = self[Color::BLACK].transpose();
        transposed_halfboard[Color::BLACK] = self[Color::WHITE].transpose();

        // Transposing en passant square
        transposed_halfboard.en_passant = self.en_passant.map(|square| square.transpose());

        // Transposing the 8x8 array representation
        for (square, transposed_square) in self.board_88.iter().zip(transposed_halfboard.board_88.iter_mut().rev()) {
            *transposed_square = *square;
        }

        transposed_halfboard
    }
}

impl Board {
    // Returns a board representing a given position
    // Two halfboards and initial parameters
    pub fn init_from_position(position: &HalfBoard, castling_rights: u8, halfmove_clock: u8, side_to_move: Color) -> Self {
        let white_pov = position.clone();
        let black_pov = position.clone().transpose();
        Board {
            halfboards: BlackWhiteAttribute::new(black_pov, white_pov),

            halfmove_clock,
            castling_rights,

            side_to_move,

            capture_stack: [Piece::PAWN; 32], // Initialized with pawns by default even if it should be empty
            capture_stack_index: 0,
        }
    }

    pub fn initial_position() -> Self {
        Self::init_from_position(&HalfBoard::initial_position(), 0b1111, 0, Color::WHITE)
    }

    fn empty_board() -> Self {
        Self::init_from_position(&HalfBoard::empty_board(), 0, 0, Color::WHITE)
    }

    // For make and unmake procedure, we assume that the provided move is valid
    // If not the program can panic! or remain in an unconsistent state
    pub fn make(&mut self, ext_mov: ExtendedMove) {
        let side_to_move = self.side_to_move;
        let mov = ext_mov.get_raw_move();
        let moved_piece = self[side_to_move][mov.origin_square()].unwrap();

        self.halfmove_clock = if moved_piece == Piece::PAWN {
            0
        } else {
            self.halfmove_clock+1
        };

        // Capture
        // Not triggered by en passant capture
        if mov.is_capture() {
            let captured_piece = self[side_to_move][mov.destination_square()].unwrap();
            self.delete_piece(mov.destination_square(), captured_piece, Color::BLACK, side_to_move);

            self.halfmove_clock = 0;

            // Remove caslting rights if it captures a rook on a castling square
            if captured_piece == Piece::ROOK {
                if side_to_move == Color::WHITE && mov.destination_square() == A8_SQUARE {
                    self.castling_rights &= REMOVE_QUEEN_SIDE_CASTLING_RIGHTS[Color::BLACK];
                } else if side_to_move == Color::WHITE && mov.destination_square() == H8_SQUARE {
                    self.castling_rights &= REMOVE_KING_SIDE_CASTLING_RIGHTS[Color::BLACK];
                } else if side_to_move == Color::BLACK && mov.destination_square() == A8_SQUARE {
                    self.castling_rights &= REMOVE_KING_SIDE_CASTLING_RIGHTS[Color::WHITE];
                } else if side_to_move == Color::BLACK && mov.destination_square() == H8_SQUARE {
                    self.castling_rights &= REMOVE_QUEEN_SIDE_CASTLING_RIGHTS[Color::WHITE];
                }
            }

            self.push_captured(captured_piece);
        }

        // We move the piece after the capture
        self.move_piece(mov.origin_square(), mov.destination_square(), moved_piece, Color::WHITE, side_to_move);

        // En passant capture
        if let Some(en_passant_captured_square) = mov.get_en_passant_capture_square() {
            self.delete_piece(en_passant_captured_square, Piece::PAWN, Color::BLACK, side_to_move);

            self.halfmove_clock = 0;
        }

        // Castling, move the other rook
        // Castling rights are removed when the king moves
        if let Some((rook_from_square, rook_dest_square)) = mov.get_castling_rook(self.side_to_move) {
            self.move_piece(rook_from_square, rook_dest_square, Piece::ROOK, Color::WHITE, side_to_move);
        }

        // Double pawn push, set the en passant target
        self[side_to_move].en_passant = mov.get_en_passant_target_square();
        self[side_to_move.transpose()].en_passant = mov.get_en_passant_target_square().map(|square| square.transpose());

        // Promotion
        if let Some(promotion_piece) = mov.get_promotion_piece() {
            self.delete_piece(mov.destination_square(), Piece::PAWN, Color::WHITE, side_to_move);
            self.create_piece(mov.destination_square(), promotion_piece, Color::WHITE, side_to_move);

            self.halfmove_clock = 0;
        }
        // Castling rights update
        if moved_piece == Piece::KING {
            self.castling_rights &= REMOVE_ALL_CASTLING_RIGHTS[self.side_to_move];
        } else if moved_piece == Piece::ROOK {
            if mov.origin_square() == KING_CASTLE_ROOK_ORIGIN_SQUARES[self.side_to_move] {
                self.castling_rights &= REMOVE_KING_SIDE_CASTLING_RIGHTS[self.side_to_move];
            } else if mov.origin_square() == QUEEN_CASTLE_ROOK_ORIGIN_SQUARES[self.side_to_move] {
                self.castling_rights &= REMOVE_QUEEN_SIDE_CASTLING_RIGHTS[self.side_to_move];
            }
        }

        self.side_to_move = self.side_to_move.transpose();
    }

    // Unmake the given move, if the move was played by White, the board must be in Black side to
    // play state (and vice versa)
    pub fn unmake(&mut self, ext_mov: ExtendedMove) {
        // Move the piece back
        let side_that_played = self.side_to_move.transpose();
        let mov = ext_mov.get_raw_move();
        let moved_piece = self[side_that_played][mov.destination_square()].unwrap();

        self.move_piece(mov.destination_square(), mov.origin_square(), moved_piece, Color::WHITE, side_that_played);

        // Capture
        if mov.is_capture() {
            let captured_piece = self.pop_captured();
            self.create_piece(mov.destination_square(), captured_piece, Color::BLACK, side_that_played);
        }
        // En passant capture
        if let Some(en_passant_captured_square) = mov.get_en_passant_capture_square() {
            self.create_piece(en_passant_captured_square, Piece::PAWN, Color::BLACK, side_that_played);
        }

        // Castling, move the other rook
        if let Some((rook_from_square, rook_dest_square)) = mov.get_castling_rook(side_that_played) {
            self.move_piece(rook_dest_square, rook_from_square, Piece::ROOK, Color::WHITE, side_that_played);
        }

        // Promotion
        if let Some(promotion_piece) = mov.get_promotion_piece() {
            self.delete_piece(mov.origin_square(), promotion_piece, Color::WHITE, side_that_played);
            self.create_piece(mov.origin_square(), Piece::PAWN, Color::WHITE, side_that_played);
        }

        // Restoring en passant, caslting rights and halfmove clock from the move metadata
        // En passant square is given from the side that played pov
        let en_passant_square = ext_mov.get_board_state(EN_PASSANT_SQUARE_BITS_OFFSET, EN_PASSANT_SQUARE_BITS_SIZE);
        self[side_that_played].en_passant = if en_passant_square != 0 {
            Some(Square::new(en_passant_square))
        } else {
            None
        };
        let side_to_move = self.side_to_move;
        self[side_to_move].en_passant = self[side_that_played].en_passant.map(|square| square.transpose());

        self.halfmove_clock = ext_mov.get_board_state(HALFMOVE_CLOCK_BITS_OFFSET, HALFMOVE_CLOCK_BITS_SIZE);
        self.castling_rights = ext_mov.get_board_state(CASTLING_RIGHTS_BITS_OFFSET, CASTLING_RIGHTS_BITS_SIZE);


        self.side_to_move = self.side_to_move.transpose();
    }

    fn push_captured(&mut self, piece: Piece) {
        self.capture_stack[self.capture_stack_index] = piece;
        self.capture_stack_index += 1;
    }

    fn pop_captured(&mut self) -> Piece {
        self.capture_stack_index -= 1;
        self.capture_stack[self.capture_stack_index]
    }

    // Creates a piece of the given color on the given player's board (on black board, black has
    // white pieces)
    fn create_piece(&mut self, square: Square, piece: Piece, piece_color: Color, player_color: Color) {
        self[player_color].create_piece(square, piece, piece_color);
        self[player_color.transpose()].create_piece(square.transpose(), piece, piece_color.transpose());
    }

    fn delete_piece(&mut self, square: Square, piece: Piece, piece_color: Color, player_color: Color) {
        self[player_color].delete_piece(square, piece, piece_color);
        self[player_color.transpose()].delete_piece(square.transpose(), piece, piece_color.transpose());
    }

    // This moves a piece of the given side color
    fn move_piece(&mut self, from: Square, to: Square, moved_piece: Piece, piece_color: Color, player_color: Color) {
        self[player_color].move_piece(from, to, moved_piece, piece_color);
        self[player_color.transpose()].move_piece(from.transpose(), to.transpose(), moved_piece, piece_color.transpose());
    }

    // Creates a LegalMoveGenerator for the side to move
    pub fn create_legal_move_generator(&self) -> LegalMoveGenerator {
        let side_to_move = self.side_to_move;
        LegalMoveGenerator::new(&self.halfboards[side_to_move], side_to_move, self.castling_rights, self.halfmove_clock)
    }

    // returns if the given square is checked by a piece of the given color
    // TODO do it "efficiently" with only attacks and not pawn pushs
    /*
    pub fn is_in_check(&self, square: Square, color: Color) -> bool {
        self.move_generators[color].attack_map().any(|attacked_square| attacked_square == square)
    }

    // temporary function while the move generation is pseudo legal
    // We check that the color that just played hasn't leave its king in check
    // TODO remove this because it is ugly
    pub fn is_king_checked(&self) -> bool {
        let side_that_played = self.side_to_move.transpose();
        let halfboard = &self[side_that_played];
        let king_square = (halfboard[Color::WHITE] & halfboard[Piece::KING]).as_square();
        self.is_in_check(king_square.transpose(), self.side_to_move)
    }
    */

}

// A Board indexed by Color returns the corresponding pov of the position
impl Index<Color> for Board {
    type Output = HalfBoard;

    fn index(&self, color: Color) -> &Self::Output {
        &self.halfboards[color]
    }
}

impl IndexMut<Color> for Board {
    fn index_mut(&mut self, color: Color) -> &mut HalfBoard {
        &mut self.halfboards[color]
    }
}

// An HalfBoard indexed by Color returns the corresponding BitBoard occupancy (the position of the pieces of
// the given color)
impl Index<Color> for HalfBoard {
    type Output = BitBoard;

    fn index(&self, color: Color) -> &Self::Output {
        &self.occupancy[color]
    }
}

impl IndexMut<Color> for HalfBoard {
    fn index_mut(&mut self, color: Color) -> &mut BitBoard {
        &mut self.occupancy[color]
    }
}

// An HalfBoard indexed by Piece returns the BitBoard occupancy of this type of piece whatever
// their color
impl Index<Piece> for HalfBoard {
    type Output = BitBoard;

    fn index(&self, piece: Piece) -> &Self::Output {
        &self.pieces[piece as usize]
    }
}

impl IndexMut<Piece> for HalfBoard {
    fn index_mut(&mut self, piece: Piece) -> &mut BitBoard {
        &mut self.pieces[piece as usize]
    }
}

// An HalfBoard indexed by square returns the type of the piece that is present on this square
// (None otherwise)
impl Index<Square> for HalfBoard {
    type Output = Option<Piece>;

    fn index(&self, square: Square) -> &Self::Output {
        &self.board_88[square.as_index()]
    }
}

impl IndexMut<Square> for HalfBoard {
    fn index_mut(&mut self, square: Square) -> &mut Option<Piece> {
        &mut self.board_88[square.as_index()]
    }
}


// An whole Board and HalfBoard from a FEN string
impl HalfBoard {
    // TODO clean parser
    pub fn from_fen(fen_parts: &[&str]) -> Result<Self, &'static str> {
        let mut board = Self::empty_board();

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

        // En passant
        if fen_parts[3].len() == 2 {
            let chars: Vec<_> = fen_parts[3].chars().collect();
            board.en_passant = Some(Square::from_char_file_rank(chars[0], chars[1]));
        }

        board.fill_88();

        Ok(board)
    }
}

impl Board {
    pub fn from_fen(fen_string: &str) -> Result<Self, &'static str> {
        let mut board = Self::empty_board();

        let fen_parts: Vec<_> = fen_string.split_whitespace().collect();
        if fen_parts.len() < 4 || fen_parts.len() > 6 {
            return Err("Invalid FEN string");
        }

        board[Color::WHITE] = HalfBoard::from_fen(&fen_parts)?;
        board[Color::BLACK] = board[Color::WHITE].transpose();

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

        // TODO side to move and halfmove_clock
        match fen_parts[1].chars().next() {
            Some('w') => board.side_to_move = Color::WHITE,
            Some('b') => board.side_to_move = Color::BLACK,
            _ => return Err("Invalid FEN string"),
        }

        Ok(board)
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
}

impl Transpose for BitBoard {
    fn transpose(&self) -> Self {
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


impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /*
        write!(f, "{}", self[Color::WHITE])?;
        write!(f, "{}", self[Color::BLACK])
        */
        for i in (0..8).rev() {
            let halfboard = &self[Color::WHITE];
            for j in (0..8).rev() {
                if let Some(piece) = halfboard.board_88[8*i + j] {
                    if halfboard[Color::WHITE].has_square(Square::new((8*i+j) as u8)) {
                        write!(f, "{}", piece.to_char().to_uppercase().next().unwrap())?;
                    } else {
                        write!(f, "{}", piece.to_char())?;
                    }
                } else {
                    write!(f, "*")?;
                }
            }
            write!(f, "   ")?;
            let halfboard = &self[Color::BLACK];
            for j in (0..8).rev() {
                if let Some(piece) = halfboard.board_88[8*i + j] {
                    if halfboard[Color::WHITE].has_square(Square::new((8*i+j) as u8)) {
                        write!(f, "{}", piece.to_char().to_uppercase().next().unwrap())?;
                    } else {
                        write!(f, "{}", piece.to_char())?;
                    }
                } else {
                    write!(f, "*")?;
                }
            }
            // Metadata info
            if i == 6 {
                write!(f, "   castling rights: ")?;
                if self.castling_rights & 0b1000  != 0 {
                    write!(f, "q")?;
                } else {
                    write!(f, ".")?;
                }
                if self.castling_rights & 0b0100  != 0 {
                    write!(f, "k")?;
                } else {
                    write!(f, ".")?;
                }
                if self.castling_rights & 0b0010  != 0 {
                    write!(f, "Q")?;
                } else {
                    write!(f, ".")?;
                }
                if self.castling_rights & 0b0001  != 0 {
                    write!(f, "K")?;
                } else {
                    write!(f, ".")?;
                }
            }

            if i == 4 {
                if let Some(square) = self[Color::WHITE].en_passant {
                    write!(f, "   en passant (white POV): {}", square)?;
                } else {
                    write!(f, "   en passant (white POV): None")?;
                }
            }
            if i == 2 {
                write!(f, "   halfmove clock: {}", self.halfmove_clock)?;
            }

            writeln!(f)?;
        }
        writeln!(f, "{}", match self.side_to_move {
            Color::WHITE => "   /\\",
            Color::BLACK => "               /\\",
        })?;
        writeln!(f)
    }
}

impl fmt::Display for HalfBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in (0..8).rev() {
            for j in (0..8).rev() {
                if let Some(piece) = self.board_88[8*i + j] {
                    if self[Color::WHITE].has_square(Square::new((8*i+j) as u8)) {
                        write!(f, "{}", piece.to_char().to_uppercase().next().unwrap())?;
                    } else {
                        write!(f, "{}", piece.to_char())?;
                    }
                } else {
                    write!(f, "*")?;
                }
            }

            writeln!(f)?;
        }
        writeln!(f)
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "WHITE")?;
        writeln!(f, "{:?}", self[Color::WHITE])?;
        writeln!(f, "BLACK")?;
        writeln!(f, "{:?}", self[Color::BLACK])
    }
}

impl fmt::Debug for HalfBoard {
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
