pub mod init_magic;

use std::ptr;
use std::fmt;

use self::init_magic::{rook_offset, bishop_offset, fill_attack_table};

use board::{Board, HalfBoard, BitBoard, KING_CASTLING_RIGHTS_MASKS, QUEEN_CASTLING_RIGHTS_MASKS};

use utils::*;

use enum_primitive::FromPrimitive;

// Perft tests for move generation, see move_generation/perft_tests.rs
#[cfg(test)]
mod perft_tests;

// Testing the magic factors
#[cfg(test)]
mod magic_factors_tests;

// This module provides a legal move generator to a board
// through the possible_moves interface
// TODO improve interface
// TODO make it legal (and not pseudo legal anymore)


// unsafe attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// See move_generation/init_magic.rs for impl block with initiatlization
pub struct MagicEntry {
    magic: u64,
    table: &'static u64, // static reference coerced to a pointer
    black_mask: u64,
    postmask: u64,
}

const SLIDING_ATTACK_TABLE_SIZE: usize = 88507; // 651KB

// NOTE: lazy statics uses an atomic check for each access, maybe at some point we will need to
// remove this and come back to classical static mut or something else to make it faster
lazy_static! {
    pub static ref BISHOP_ATTACK_TABLE: [MagicEntry; 64] = init_magic_entries(MagicEntry::bishop_magic);
    pub static ref ROOK_ATTACK_TABLE: [MagicEntry; 64] = init_magic_entries(MagicEntry::rook_magic);

    // TODO better magic table with none naive arrangement and better magic factors to reduce size
    pub static ref SLIDING_ATTACK_TABLE: [u64; SLIDING_ATTACK_TABLE_SIZE] = init_sliding_attack_tables();

    static ref KNIGHT_ATTACK_TABLE: [BitBoard; 64] = generate_knight_attacks(); // 512 bytes
    static ref KING_ATTACK_TABLE: [BitBoard; 64] = generate_king_attacks(); // 512 bytes
}

fn init_sliding_attack_tables() -> [u64; SLIDING_ATTACK_TABLE_SIZE] {
    let mut attack_table = [0; SLIDING_ATTACK_TABLE_SIZE];
    for square in 0u8..64 {
        //MagicEntry::fill_attack_table(&mut attack_table, square);
        fill_attack_table(&mut attack_table, square);
    }
    attack_table
}

fn init_magic_entries(magic_entry_init: fn (u8) -> MagicEntry) -> [MagicEntry; 64] {
    let mut attack_table: [MagicEntry; 64] = unsafe {std::mem::uninitialized()};
    for (magic_entry, square) in attack_table.iter_mut().zip(0u8..) {
        *magic_entry = magic_entry_init(square);
    }
    attack_table
}

// returns the bitboards of the pawns that can take the pawn in index (starting from LSB)
static EN_PASSANT_TABLE: [BitBoard; 8] = en_passant_table(); // 64 bytes 8*8 bitboards

fn sliding_attack(magic_entry: &MagicEntry, occupancy: BitBoard, offset_function: fn(u64, u64) -> usize) -> BitBoard {
    let table_pointer: *const u64 = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = offset_function(hash_key, magic_entry.magic);

    BitBoard::new(unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    })
}

pub fn rook_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square.0)];
    sliding_attack(magic_entry, occupancy, rook_offset)
}

pub fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square.0)];
    sliding_attack(magic_entry, occupancy, bishop_offset)
}

// A Move is a 64 bits word following more or the less the extended
// representation in https://www.chessprogramming.org/Encoding_Moves
// MSB -------------------------------------------------------- LSB
// latter   | ep5 .. ep0 | hf6 .. hf0 | csl3 .. csl0 | pc3 .. pc0
// 63 .. 39 |  38 .. 32 |x| 30 .. 24  |  23  ..  20  | 19  ..  16
// pc3 .. pc0 | prom | capt | sp1 | sp0 | dst5 .. dst0 | st5 .. st0
// 19  .. 16  |  15  |  14  |  13 | 12  |  11  ..  6   |  5  ..  0
// TODO change the Move structure to a short Move structure and an ExtendedMove with all the
// decoration
#[derive(Clone, Copy)]
pub struct Move(u64);

const SPECIAL0_FLAG: u64 = 1 << 12;
const SPECIAL1_FLAG: u64 = 1 << 13;
const CAPTURE_FLAG: u64 = 1 << 14;
const PROMOTION_FLAG: u64 = 1 << 15;

const FLAGS_RANGE: u64 = SPECIAL0_FLAG | SPECIAL1_FLAG | CAPTURE_FLAG | PROMOTION_FLAG;

const DOUBLE_PUSH_FLAG: u64 = SPECIAL0_FLAG;
const EN_PASSANT_CAPTURE_FLAG: u64 = CAPTURE_FLAG | DOUBLE_PUSH_FLAG;

const KING_CASTLE_FLAG: u64 = SPECIAL1_FLAG;
const QUEEN_CASTLE_FLAG: u64 = SPECIAL0_FLAG | SPECIAL1_FLAG;

// [black castle, white castle]
pub const KING_CASTLE_MOVES: BlackWhiteAttribute<Move> = BlackWhiteAttribute::new(
    Move::raw_new(D1_SQUARE, B1_SQUARE).set_flags(KING_CASTLE_FLAG),
    Move::raw_new(E1_SQUARE, G1_SQUARE).set_flags(KING_CASTLE_FLAG));

pub const QUEEN_CASTLE_MOVES: BlackWhiteAttribute<Move> = BlackWhiteAttribute::new(
    Move::raw_new(D1_SQUARE, F1_SQUARE).set_flags(QUEEN_CASTLE_FLAG),
    Move::raw_new(E1_SQUARE, C1_SQUARE).set_flags(QUEEN_CASTLE_FLAG));

pub const NULL_MOVE: Move = Move::raw_new(Square::new(0), Square::new(0));

pub const CASTLING_RIGHTS_BITS_OFFSET: u8 = 20;
pub const HALFMOVE_CLOCK_BITS_OFFSET: u8 = 24;
pub const EN_PASSANT_SQUARE_BITS_OFFSET: u8 = 32;

pub const CASTLING_RIGHTS_BITS_SIZE: u8 = 4;
pub const HALFMOVE_CLOCK_BITS_SIZE: u8 = 7;
pub const EN_PASSANT_SQUARE_BITS_SIZE: u8 = 6;

impl Move {
    // Creates a simple move with no side effect
    #[inline]
    pub fn quiet_move(from: Square, to: Square) -> Self {
        Move(u64::from(from.0) + (u64::from(to.0) << 6))
    }

    // Creates a move with all the specified flags
    #[inline]
    fn tactical_move(from: Square, to: Square, flags: u64) -> Self {
        Self::quiet_move(from, to).set_flags(flags)
    }

    // This is supposed to be called only for constant compilation and not an runtime
    #[inline]
    #[allow(clippy::cast_lossless)]
    const fn raw_new(from: Square, to: Square) -> Self {
        Move(from.0 as u64 + ((to.0 as u64) << 6))
    }

    // Helper to set some flags to the move
    #[inline]
    const fn set_flags(self, flags: u64) -> Self {
        Move(self.0 | flags)
    }

    // Set the captured piece for unmaking routine
    #[inline]
    fn set_captured_piece(self, piece: Piece) -> Self {
        Move(self.0 | ((piece as u64 & 0xf) << 16))
    }

    // Returns wether the move has the given flags set
    #[inline]
    fn has_flags(self, flags: u64) -> bool {
        self.0 & flags != 0
    }

    #[inline]
    fn has_exact_flags(self, flags: u64) -> bool {
        self.0 & FLAGS_RANGE == flags
    }

    #[inline]
    fn set_promoted_piece(self, piece: Piece) -> Self {
        Move(self.0 | (piece as u64 & 0b11) << 12)
    }

    #[inline]
    // value has to have trailing zeros not to overwrite some other states
    pub fn set_board_state(self, value: u8, state: u8) -> Self {
        Move(self.0 | u64::from(value) << state)
    }

    #[inline]
    pub fn get_board_state(self, state: u8, size: u8) -> u8 {
        ((self.0 >> state) & (u64::max_value() >> (64-size))) as u8
    }

    // Public interface for the board
    #[inline]
    pub fn origin_square(self) -> Square {
        Square::new((self.0 & 0x3f) as u8)
    }

    #[inline]
    pub fn destination_square(self) -> Square {
        Square::new((self.0 >> 6) as u8 & 0x3f)
    }

    #[inline]
    pub fn get_promotion_piece(self) -> Option<Piece> {
        if self.has_flags(PROMOTION_FLAG) {
            // from_u32 returns an Option, panic!s if the piece code is invalid
            Piece::from_u64(self.0 >> 12 & 0b11)
        } else {
            None
        }
    }

    // This is not triggered by en passant capture which has its own making and unmaking procedure
    #[inline]
    pub fn get_captured_piece(self) -> Option<Piece> {
        if self.has_flags(CAPTURE_FLAG) && !self.has_exact_flags(EN_PASSANT_CAPTURE_FLAG) {
            Piece::from_u64(self.0 >> 16 & 0xf)
        } else {
            None
        }
    }

    // if the move is a double pawn push, returns the associated en passant target square
    #[inline]
    pub fn get_en_passant_target_square(self) -> Option<Square> {
        if self.has_exact_flags(DOUBLE_PUSH_FLAG) {
            Some(self.destination_square())
        } else {
            None
        }
    }

    // If this is a castling move, returns the from and to squares of the associated tower
    #[inline]
    pub fn get_castling_rook(self, side_to_move: Color) -> Option<(Square, Square)> {
        if self.has_exact_flags(KING_CASTLE_FLAG) {
            Some((KING_CASTLE_ROOK_ORIGIN_SQUARES[side_to_move], KING_CASTLE_ROOK_DEST_SQUARES[side_to_move]))
        } else if self.has_exact_flags(QUEEN_CASTLE_FLAG) {
            Some((QUEEN_CASTLE_ROOK_ORIGIN_SQUARES[side_to_move], QUEEN_CASTLE_ROOK_DEST_SQUARES[side_to_move]))
        } else {
            None
        }
    }

    // If this is an en passant capture, returns the captured pawn square
    #[inline]
    pub fn get_en_passant_capture_square(self) -> Option<Square> {
        if self.has_exact_flags(EN_PASSANT_CAPTURE_FLAG) {
            Some(self.destination_square().behind())
        } else {
            None
        }
    }
}

impl PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        self.0 & 0xffff == other.0 & 0xffff
    }
}

// TODO clean transposition (for en passant)
impl Transpose for Move {
    fn transpose(&self) -> Self {
        let origin_square = self.origin_square().transpose();
        let dest_square = self.destination_square().transpose();
        let mut transposed_move = Move(self.0);
        transposed_move.0 &= !0xfff; // Clearing the old squares
        transposed_move.0 |= u64::from(origin_square.0) + (u64::from(dest_square.0) << 6);

        transposed_move
    }
}

// This struct holds a reference to a Board and generates the legal moves from this POV
// at the time the struct was instanciated (so we have to reinstantiate a LegalMoveGenerator after
// every move)
// LegalMoveGenerator are instanciated by the explorator logic, a Board can return a
// LegalMoveGenerator for the side to play.
// The LegalMoveGenerator also have an interface with evaluation (a Board is supposed to hold a
// reference to both of them to make them communicate)
// Pinned pieces are computed on the fly (Black slider's attack can be computed by the White
// move generation)
// TODO Evaluation interface
// Most information fetching is lazy so this creates branching but hopefully we gain some
// computation time
#[derive(Copy, Clone)]
pub struct LegalMoveGenerator<'board> {
    // Even if this is an HalfBoard, there will be another generator for the other HalfBoard
    // so we hide the distinction between a board and an halfboard.
    board: &'board HalfBoard,
    color: Color, // We keep the Color to know which castling masks to use

    move_stack: [Move; 128], // Allocated on the program stack with a bounded size
    // NOTE: The maximum size is 128 even if we can construct a position with 218 moves
    // maybe we have to change this to 218 in the future

    // Internals
    // next move on the stack
    last_move: usize,
    // iterator index
    next_iterator_move: usize,
    // copies of the caslting rights, previous en passant state and halfmove clock
    // NOTE: the en passant square is held in the HalfBoard, so we need to restore the state to use
    // it, maybe at some point 
    castling_rights: u8,
    halfmove_clock: u8,
}

impl<'board> LegalMoveGenerator<'board> {
    // Initialize a new LegalMoveGenerator by computing pinned pieces
    // It takes a reference to the current board and the color of the player we want to move
    pub fn new(halfboard: &'board HalfBoard, color: Color, castling_rights: u8, halfmove_clock: u8) -> Self {
        let mut generator = Self {
            board: &halfboard,
            color,

            move_stack: [NULL_MOVE; 128],

            last_move: 0,
            next_iterator_move: 0,
            castling_rights,
            halfmove_clock,
        };
        // compute pinned pieces
        //
        // fetch basic moves information
        generator.fetch_possible_moves();

        generator
    }

    // Pushs the given move in the move stack
    #[inline]
    fn push(&mut self, pushed_move: Move) {
        self.move_stack[self.last_move] = pushed_move;
        self.last_move += 1;
    }

    // Helper to push all the possible promotions
    #[inline]
    fn push_promotions(&mut self, promotion_move: Move) {
        for promoted_piece in &AVAILABLE_PROMOTION {
            self.push(promotion_move.set_promoted_piece(*promoted_piece));
        }
    }

    // Decorates the next move to be fetched for iteration with irreversible states
    fn decorate_move(&self, mov: Move) -> Move {
        mov
            .set_board_state(self.castling_rights, CASTLING_RIGHTS_BITS_OFFSET)
            // En passant square is given from the side to play pov
            .set_board_state(self.board.en_passant.map_or(0, |square| square.0), EN_PASSANT_SQUARE_BITS_OFFSET)
            .set_board_state(self.halfmove_clock, HALFMOVE_CLOCK_BITS_OFFSET)
    }

    // All the move generation logic
    // For captures, the captured piece is set when the move is iterated upon
    // TODO fetch only captures first and then quiet moves to make it lazier
    // ------------------------------------------------
    fn fetch_possible_moves(&mut self) {
        let board = self.board;

        // Simple pawn push ------------------------
        let pawns = board[Piece::PAWN] & board[Color::WHITE];
        let pushed_pawns = (pawns << 8) & board.empty_squares();

        // No promotion
        for dest_square in pushed_pawns & !ROW_8 {
            self.push(Move::quiet_move(dest_square.behind(), dest_square));
        }
        // Promotion
        for dest_square in pushed_pawns & ROW_8 {
            self.push_promotions(Move::tactical_move(dest_square.behind(), dest_square, PROMOTION_FLAG));
        }
        // -----------------------------------------

        // Double push, sets en passant flag -------
        let starting_pawns = board[Piece::PAWN] & board[Color::WHITE] & ROW_2;

        // To be double pushed, the pawns have to be able to move once forward
        let simple_pushed_pawns = (starting_pawns << 8) & board.empty_squares();
        // The pawns that can both be pushed for one and two lines forward
        let double_pushed_pawns = (simple_pushed_pawns << 8) & board.empty_squares();

        for dest_square in double_pushed_pawns {
            self.push(Move::quiet_move(dest_square.behind().behind(), dest_square)
                      .set_flags(DOUBLE_PUSH_FLAG));
        }
        // -----------------------------------------

        // Pawn captures ---------------------------
        let left_capture_moves = (pawns & !FILE_A) << 9 & board[Color::BLACK];
        let right_capture_moves = (pawns & !FILE_H) << 7 & board[Color::BLACK];
        // Capture without promotions
        for capture_square in left_capture_moves & !ROW_8 {
            self.push(Move::tactical_move(capture_square.behind_right(), capture_square, CAPTURE_FLAG));
        }
        for capture_square in right_capture_moves & !ROW_8 {
            self.push(Move::tactical_move(capture_square.behind_left(), capture_square, CAPTURE_FLAG));
        }
        // Capture with promotion
        for capture_square in left_capture_moves & ROW_8 {
            self.push_promotions(Move::tactical_move(capture_square.behind_right(), capture_square, CAPTURE_FLAG));
        }
        for capture_square in right_capture_moves & ROW_8 {
            self.push_promotions(Move::tactical_move(capture_square.behind_left(), capture_square, CAPTURE_FLAG));
        }
        // -----------------------------------------

        // En passant capture ----------------------
        for pawn_origin_square in en_passant_capture_start_square(board.en_passant) & pawns {
            self.push(Move::tactical_move(pawn_origin_square, board.en_passant.unwrap().forward(), EN_PASSANT_CAPTURE_FLAG));
        }
        // -----------------------------------------

        // Knights moves ---------------------------
        for knight_square in board[Piece::KNIGHT] & board[Color::WHITE] {
            let attack = KNIGHT_ATTACK_TABLE[knight_square.as_index()];
            self.push_captures_quiets(knight_square, attack);
        }
        // -----------------------------------------

        // Bishop moves ----------------------------
        for bishop_square in board[Piece::BISHOP] & board[Color::WHITE] {
            self.sliding_attack(bishop_square, bishop_attack);
        }
        // -----------------------------------------

        // Rook moves ------------------------------
        for rook_square in board[Piece::ROOK] & board[Color::WHITE] {
            self.sliding_attack(rook_square, rook_attack);
        }
        // -----------------------------------------

        // Queen moves -----------------------------
        for queen_square in board[Piece::QUEEN] & board[Color::WHITE] {
            self.sliding_attack(queen_square, bishop_attack);
            self.sliding_attack(queen_square, rook_attack);
        }
        // -----------------------------------------

        // King moves ------------------------------
        // Moves
        let king_square = (board[Piece::KING] & board[Color::WHITE]).as_square();
        let attack = KING_ATTACK_TABLE[king_square.as_index()];
        self.push_captures_quiets(king_square, attack);
        // Castle
        let color = self.color;
        if self.can_king_castle() {
            self.push(KING_CASTLE_MOVES[color]);
        }
        if self.can_queen_castle() {
            self.push(QUEEN_CASTLE_MOVES[color])
        }
        // -----------------------------------------
    }

    // Helper for pieces that can perform captures and quiet moves at the same time
    fn push_captures_quiets(&mut self, origin_square: Square, attack: BitBoard) {
        // Captures
        for capture_square in attack & self.board[Color::BLACK] {
            self.push(Move::tactical_move(origin_square, capture_square, CAPTURE_FLAG));
        }
        // Quiet moves
        for dest_square in attack & self.board.empty_squares() {
            self.push(Move::quiet_move(origin_square, dest_square));
        }
    }

    // Helper for sliders
    #[inline]
    fn sliding_attack(&mut self, origin_square: Square, piece_attack: fn (Square, BitBoard) -> BitBoard) {
        let attack = piece_attack(origin_square, self.board.occupied_squares());
        self.push_captures_quiets(origin_square, attack);
    }

    // TODO add the is in check function
    // Castling
    fn can_king_castle(&self) -> bool {
        (self.castling_rights & KING_CASTLING_RIGHTS_MASKS[self.color] != 0) // right to castle kingside
        && (KING_CASTLE_EMPTY[self.color] & self.board.occupied_squares() == 0) // none of the squares on the way are occupied
        && (KING_CASTLE_CHECK[self.color].all(|square| !self.is_in_check(square))) // squares crossed by the king are in check
    }

    fn can_queen_castle(&self) -> bool {
        (self.castling_rights & QUEEN_CASTLING_RIGHTS_MASKS[self.color] != 0) // right to castle queenside
        && (QUEEN_CASTLE_EMPTY[self.color] & self.board.occupied_squares() == 0) // none of the squares on the way are occupied
        && (QUEEN_CASTLE_CHECK[self.color].all(|square| !self.is_in_check(square))) // squares crossed by the king are in check
    }

    // Uses a super piece (not to rely on the other side move generator)
    fn is_in_check(&self, square: Square) -> bool {
        let board = self.board;
        // Rook
        rook_attack(square, board.occupied_squares()) & board[Color::BLACK] & (board[Piece::ROOK] | board[Piece::QUEEN]) != 0 ||
        // Bishop
        bishop_attack(square, board.occupied_squares()) & board[Color::BLACK] & (board[Piece::BISHOP] | board[Piece::QUEEN]) != 0 ||
        // Knight
        KNIGHT_ATTACK_TABLE[square.as_index()] & board[Color::BLACK] & board[Piece::KNIGHT] != 0 ||
        // Pawn
        ROW_8.has_square(square) & (
            FILE_A.has_square(square) & (board[Color::BLACK] & board[Piece::PAWN]).has_square(square.forward_left()) ||
            FILE_H.has_square(square) & (board[Color::BLACK] & board[Piece::PAWN]).has_square(square.forward_right())
        ) ||
        // King
        KING_ATTACK_TABLE[square.as_index()] & board[Color::BLACK] & board[Piece::KING] != 0
    }

    // TODO Remove this when move generation is legal
    fn is_king_checked(&self) -> bool {
        self.is_in_check((self.board[Piece::KING] & self.board[Color::WHITE]).as_square())
    }

    // Returns an attack map of the given position with White playing
    pub fn attack_map(&self) -> BitBoard {
        let board = self.board;
        let mut attack_map = BitBoard::empty();
        // Pawns
        attack_map |= (board[Piece::PAWN] & board[Color::WHITE] & !FILE_A) << 9;
        attack_map |= (board[Piece::PAWN] & board[Color::WHITE] & !FILE_H) << 7;
        // Knights
        for knight_square in board[Piece::KNIGHT] & board[Color::WHITE] {
            attack_map |= KNIGHT_ATTACK_TABLE[knight_square.as_index()];
        }
        // Bishops
        for bishop_square in board[Piece::BISHOP] & board[Color::WHITE] {
            attack_map |= bishop_attack(bishop_square, board.occupied_squares());
        }
        // Rooks
        for rook_square in board[Piece::ROOK] & board[Color::WHITE] {
            attack_map |= rook_attack(rook_square, board.occupied_squares());
        }
        // Queens
        for queen_square in board[Piece::QUEEN] & board[Color::WHITE] {
            attack_map |= bishop_attack(queen_square, board.occupied_squares());
            attack_map |= rook_attack(queen_square, board.occupied_squares());
        }
        // King
        attack_map |= KING_ATTACK_TABLE[(board[Piece::KING] & board[Color::WHITE]).as_square().as_index()];

        attack_map
    }
}

// The iterator function is straightforward and assumes that the moves have been sorted before
impl<'board> Iterator for LegalMoveGenerator<'board> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_iterator_move < self.last_move {
            let iter_move = self.move_stack[self.next_iterator_move];
            self.next_iterator_move += 1;

            // Fetch the captured piece
            if iter_move.has_flags(CAPTURE_FLAG) {
                iter_move.set_captured_piece(self.board[iter_move.destination_square()].unwrap());
            }
            // Decorate the move and returns it
            Some(self.decorate_move(iter_move))
        } else {
            None
        }
    }
}

/* Target interface for a move iterator
 * - tactical moves
 * - quiet moves
 */
impl Board {

    /*
    // TODO remove this once move generation is working
    pub fn get_move(&mut self, origin_file: char, origin_row: char, dest_file: char, dest_row: char) -> Move {
        let origin_square = Square::from_char_file_rank(origin_file, origin_row);
        let dest_square = Square::from_char_file_rank(dest_file, dest_row);
        for mov in self.possible_moves() {
            if mov.origin_square() == origin_square && mov.destination_square() == dest_square {
                return mov;
            }
        }
        panic!("Can't find move {}{}{}{}", origin_file, origin_row, dest_file, dest_row);
    }*/
}

fn en_passant_capture_start_square(target: Option<Square>) -> BitBoard {
    if let Some(square) = target {
        // TODO remove this condition when we will have a better test for check
        if EN_PASSANT_TARGET_LINE.has_square(square) {
            EN_PASSANT_TABLE[(square.0 - 32) as usize]
        } else {
            BitBoard::empty()
        }
    } else {
        BitBoard::empty()
    }
}


impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.origin_square(), self.destination_square(), match self.get_promotion_piece() {
            Some(Piece::KNIGHT) => "n",
            Some(Piece::BISHOP) => "b",
            Some(Piece::ROOK) => "r",
            Some(Piece::QUEEN) => "q",
            _ => "",
        })
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{} p:{} c:{} sp1:{} sp0:{}", self.origin_square(),
                                                  self.destination_square(),
                                                  self.has_flags(PROMOTION_FLAG),
                                                  self.has_flags(CAPTURE_FLAG),
                                                  self.has_flags(SPECIAL1_FLAG),
                                                  self.has_flags(SPECIAL0_FLAG))
    }
}

#[allow(clippy::unreadable_literal)]
const fn en_passant_table() -> [BitBoard; 8] {
    [
        BitBoard::new(0x0200000000),
        BitBoard::new(0x0500000000),
        BitBoard::new(0x0a00000000),
        BitBoard::new(0x1400000000),
        BitBoard::new(0x2800000000),
        BitBoard::new(0x5000000000),
        BitBoard::new(0xa000000000),
        BitBoard::new(0x4000000000),
    ]
}

fn generate_knight_attacks() -> [BitBoard; 64] {
    let mut knight_attacks = [BitBoard::empty(); 64];

    let knight_moves = [(1, 2), (1, -2), (-1, 2), (-1, -2),
                        (2, 1), (2, -1), (-2, 1), (-2, -1)];

    for (attack_bitboard, sq) in knight_attacks.iter_mut().zip(0u8..) {
        let (rank, file) = Square(sq).rank_file();
        let (rank, file) = (rank as i8, file as i8);

        for (i, j) in &knight_moves {
            if file + i >= 0 && file + i < 8 && rank + j >= 0 && rank + j < 8 {
                *attack_bitboard |= Square::from_file_rank((file + i) as u8, (rank + j) as u8).as_bitboard();
            }
        }
    }

    knight_attacks
}

fn generate_king_attacks() -> [BitBoard; 64] {
    let mut king_attacks = [BitBoard::empty(); 64];

    let king_moves = [(1, 1), (1, 0), (1, -1),
                      (0, 1), (0, -1),
                      (-1, 1), (-1, 0), (-1, -1)];

    for (attack_bitboard, sq) in king_attacks.iter_mut().zip(0u8..) {
        let (rank, file) = Square(sq).rank_file();
        let (rank, file) = (rank as i8, file as i8);

        for (i, j) in &king_moves {
            if file + i >= 0 && file + i < 8 && rank + j >= 0 && rank + j < 8 {
                *attack_bitboard |= Square::from_file_rank((file + i) as u8, (rank + j) as u8).as_bitboard();
            }
        }
    }

    king_attacks
}
