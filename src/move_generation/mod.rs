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
// remove this and come back to classical static mut to make it faster
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

pub fn rook_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square.0)];

    let table_pointer: *const u64 = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = rook_offset(hash_key, magic_entry.magic);

    BitBoard::new(unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    })
}

pub fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square.0)];

    let table_pointer: *const u64 = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = bishop_offset(hash_key, magic_entry.magic);

    BitBoard::new(unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    })
}

// A Move is a 64 bits word following more or the less the extended
// representation in https://www.chessprogramming.org/Encoding_Moves
// MSB -------------------------------------------------------- LSB
// latter   | ep5 .. ep0 | hf6 .. hf0 | csl3 .. csl0 | pc3 .. pc0
// 63 .. 39 |  38 .. 32 |x| 30 .. 24  |  23  ..  20  | 19  ..  16
// pc3 .. pc0 | prom | capt | sp1 | sp0 | dst5 .. dst0 | st5 .. st0
// 19  .. 16  |  15  |  14  |  13 | 12  |  11  ..  6   |  5  ..  0
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

    // Helper iterator over the possible promotions
    #[inline]
    fn promotion_move_iterator(self) -> impl Iterator<Item = Move> {
        AVAILABLE_PROMOTION.iter().map(move |promoted_piece| self.set_promoted_piece(*promoted_piece))
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

// White moves iterator
impl HalfBoard {

    // Returns the basic pawn pushs without promotion
    pub fn simple_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        // basic pawns are pawns that won't promote (ie not on the last line)
        let basic_pawns = self[Piece::PAWN] & self[Color::WHITE] & !ROW_7;
        let simple_pushed_pawns = (basic_pawns << 8) & self.empty_squares();

        simple_pushed_pawns.map(|dest_square| Move::quiet_move(dest_square.behind(), dest_square))
    }

    // Returns the pawns that can do the initial double pushs
    pub fn double_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        let starting_pawns = self[Piece::PAWN] & self[Color::WHITE] & ROW_2;

        // To be double pushed, the pawns have to be able to move once forward
        let simple_pushed_pawns = (starting_pawns << 8) & self.empty_squares();
        // The pawns that can both be pushed for one and two lines forward
        let double_pushed_pawns = (simple_pushed_pawns << 8) & self.empty_squares();

        double_pushed_pawns.map(|dest_square| Move::quiet_move(dest_square.behind().behind(), dest_square)
                                .set_flags(DOUBLE_PUSH_FLAG))
    }

    pub fn pawn_captures_without_promotion(&self) -> impl Iterator <Item = Move> + '_ {
        let basic_pawns = self[Piece::PAWN] & self[Color::WHITE] & !ROW_7;
        // left white capture
        let left_capture_iterator = ((basic_pawns & !FILE_A) << 9 & self[Color::BLACK])
            .map(move |dest_square| Move::tactical_move(dest_square.behind_right(), dest_square, CAPTURE_FLAG)
                 .set_captured_piece(self[dest_square].unwrap()));
        // right white capture
        let right_capture_iterator = ((basic_pawns & !FILE_H) << 7 & self[Color::BLACK])
            .map(move |dest_square| Move::tactical_move(dest_square.behind_left(), dest_square, CAPTURE_FLAG)
                 .set_captured_piece(self[dest_square].unwrap()));
        left_capture_iterator.chain(right_capture_iterator)
    }

    // TODO remove duplicates with the code without promotion
    pub fn pawn_promotion_moves(&self) -> impl Iterator <Item = Move> + '_ {
        let promoting_pawns = self[Piece::PAWN] & self[Color::WHITE] & ROW_7;
        // push
        let push_promotion_iterator = ((promoting_pawns << 8) & self.empty_squares())
            .map(|dest_square| Move::tactical_move(dest_square.behind(), dest_square, PROMOTION_FLAG));
        // catpure
        // left
        let left_capture_iterator = ((promoting_pawns & !FILE_A) << 9 & self[Color::BLACK])
            .map(move |dest_square|
                 Move::tactical_move(dest_square.behind_right(), dest_square, CAPTURE_FLAG | PROMOTION_FLAG)
                 .set_captured_piece(self[dest_square].unwrap()));
        // right
        let right_capture_iterator = ((promoting_pawns & !FILE_H) << 7 & self[Color::BLACK])
            .map(move |dest_square|
                 Move::tactical_move(dest_square.behind_left(), dest_square, CAPTURE_FLAG | PROMOTION_FLAG)
                 .set_captured_piece(self[dest_square].unwrap()));

        push_promotion_iterator.chain(left_capture_iterator).chain(right_capture_iterator).flat_map(|mov| mov.promotion_move_iterator())
    }

    pub fn en_passant_captures(&self) -> impl Iterator <Item = Move> + '_ {
        (en_passant_captures_start_square(self.en_passant) & self[Piece::PAWN] & self[Color::WHITE])
            .map(move |origin_square| Move::tactical_move(origin_square, self.en_passant.unwrap().forward(), EN_PASSANT_CAPTURE_FLAG)
                 .set_captured_piece(Piece::PAWN))
    }

    // TODO redo all below functions with quiet moves and capture moves distinction
    pub fn knight_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::KNIGHT] & self[Color::WHITE])
            .flat_map(move |knight_square| {
                let attack = KNIGHT_ATTACK_TABLE[knight_square.as_index()];
                (attack & self.empty_squares())
                    .map(move |dest_square| Move::quiet_move(knight_square, dest_square))
                .chain((attack & self[Color::BLACK])
                    .map(move |dest_square| Move::tactical_move(knight_square, dest_square, CAPTURE_FLAG)
                        .set_captured_piece(self[dest_square].unwrap())))
            })
    }

    fn sliding_attack(&self, origin_square: Square, piece_attack: fn (Square, BitBoard) -> BitBoard) -> impl Iterator <Item = Move> + '_ {
        let attack = piece_attack(origin_square, self.occupied_squares());
        (attack & self.empty_squares())
            .map(move |dest_square| Move::quiet_move(origin_square, dest_square))
        .chain((attack & self[Color::BLACK])
            .map(move |dest_square| Move::tactical_move(origin_square, dest_square, CAPTURE_FLAG)
                 .set_captured_piece(self[dest_square].unwrap()))
        )
    }

    pub fn bishop_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::BISHOP] & self[Color::WHITE]).flat_map(move |bishop_square| self.sliding_attack(bishop_square, bishop_attack))
    }

    pub fn rook_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::ROOK] & self[Color::WHITE]).flat_map(move |rook_square| self.sliding_attack(rook_square, rook_attack))
    }

    pub fn queen_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::QUEEN] & self[Color::WHITE]).flat_map(move |queen_square| self.sliding_attack(queen_square, bishop_attack)
                                                           .chain(self.sliding_attack(queen_square, rook_attack)))
    }

    pub fn king_moves(&self) -> impl Iterator <Item = Move> + '_ {
        let king_square = (self[Piece::KING] & self[Color::WHITE]).as_square();
        let attack = KING_ATTACK_TABLE[king_square.as_index()];
        (attack & self.empty_squares())
            .map(move |dest_square| Move::quiet_move(king_square, dest_square))
        .chain((attack & self[Color::BLACK])
            .map(move |dest_square| Move::tactical_move(king_square, dest_square, CAPTURE_FLAG)
                 .set_captured_piece(self[dest_square].unwrap()))
        )
    }

    // TODO change this with move ordering
    pub fn possible_moves(&self) -> impl Iterator <Item = Move> + '_ {
        self.simple_pawn_pushs()
            .chain(self.double_pawn_pushs())
            .chain(self.pawn_captures_without_promotion())
            .chain(self.pawn_promotion_moves())
            .chain(self.en_passant_captures())
            .chain(self.knight_moves())
            .chain(self.bishop_moves())
            .chain(self.rook_moves())
            .chain(self.queen_moves())
            .chain(self.king_moves())
    }

    // Returns an attack map of the given position with White playing
    // TODO refactor it with moves
    pub fn attack_map(&self) -> BitBoard {
        let mut attack_map = BitBoard::empty();
        // Pawns
        attack_map |= (self[Piece::PAWN] & self[Color::WHITE] & !FILE_A) << 9;
        attack_map |= (self[Piece::PAWN] & self[Color::WHITE] & !FILE_H) << 7;
        // Knights
        for knight_square in self[Piece::KNIGHT] & self[Color::WHITE] {
            attack_map |= KNIGHT_ATTACK_TABLE[knight_square.as_index()];
        }
        // Bishops
        for bishop_square in self[Piece::BISHOP] & self[Color::WHITE] {
            attack_map |= bishop_attack(bishop_square, self.occupied_squares());
        }
        // Rooks
        for rook_square in self[Piece::ROOK] & self[Color::WHITE] {
            attack_map |= rook_attack(rook_square, self.occupied_squares());
        }
        // Queens
        for queen_square in self[Piece::QUEEN] & self[Color::WHITE] {
            attack_map |= bishop_attack(queen_square, self.occupied_squares());
            attack_map |= rook_attack(queen_square, self.occupied_squares());
        }
        // King
        attack_map |= KING_ATTACK_TABLE[(self[Piece::KING] & self[Color::WHITE]).as_square().as_index()];

        attack_map
    }

    // TODO remove this once move generation is working
    pub fn debug_move_counts(&self) {
        println!("simple push {}", self.simple_pawn_pushs().count());
        println!("double push {}", self.double_pawn_pushs().count());
        println!("pawn capture without prom {}", self.pawn_captures_without_promotion().count());
        println!("pawn promotions {}", self.pawn_promotion_moves().count());
        println!("en passant cap {}", self.en_passant_captures().count());
        println!("knight {}", self.knight_moves().count());
        println!("bishop {}", self.bishop_moves().count());
        println!("rook {}", self.rook_moves().count());
        println!("queen {}", self.queen_moves().count());
        println!("king {}", self.king_moves().count());
    }
}

/* Target interface for a move iterator
 * - tactical moves
 * - quiet moves
 */
impl Board {

    #[inline]
    fn can_king_castle(&self) -> bool {
        (self.castling_rights & KING_CASTLING_RIGHTS_MASKS[self.side_to_move] != 0) // right to castle kingside
        && (KING_CASTLE_EMPTY[self.side_to_move] & self[self.side_to_move].occupied_squares() == 0) // none of the squares on the way are occupied
        && (KING_CASTLE_CHECK[self.side_to_move].all(|square| !self.is_in_check(square.transpose(), self.side_to_move.transpose()))) // squares crossed by the king are in check
    }

    #[inline]
    fn can_queen_castle(&self) -> bool {
        (self.castling_rights & QUEEN_CASTLING_RIGHTS_MASKS[self.side_to_move] != 0) // right to castle queenside
        && (QUEEN_CASTLE_EMPTY[self.side_to_move] & self[self.side_to_move].occupied_squares() == 0) // none of the squares on the way are occupied
        && (QUEEN_CASTLE_CHECK[self.side_to_move].all(|square| !self.is_in_check(square.transpose(), self.side_to_move.transpose()))) // squares crossed by the king are in check
    }

    // Castling moves are only encoding the king move
    fn castling(&self) -> impl Iterator <Item = Move> + '_ {
        if self.can_king_castle() {
            Some(KING_CASTLE_MOVES[self.side_to_move])
        } else {
            None
        }.into_iter().chain(if self.can_queen_castle() {
            Some(QUEEN_CASTLE_MOVES[self.side_to_move])
        } else {
            None
        }.into_iter())
    }

    pub fn possible_moves(&self) -> impl Iterator <Item = Move> + '_ {
        self[self.side_to_move].possible_moves().chain(self.castling()).map(move |mov| self.decorate_move(mov))
    }

    // TODO remove this once move generation is working
    pub fn debug_move_counts(&self) {
        self[self.side_to_move].debug_move_counts();
        println!("castling {}", self.castling().count());
    }

    // TODO remove this once move generation is working
    pub fn get_move(&self, origin_file: char, origin_row: char, dest_file: char, dest_row: char) -> Move {
        let origin_square = Square::from_char_file_rank(origin_file, origin_row);
        let dest_square = Square::from_char_file_rank(dest_file, dest_row);
        for mov in self.possible_moves() {
            if mov.origin_square() == origin_square && mov.destination_square() == dest_square {
                return mov;
            }
        }
        panic!("Can't find move {}{}{}{}", origin_file, origin_row, dest_file, dest_row);
    }
}

fn en_passant_captures_start_square(target: Option<Square>) -> BitBoard {
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
