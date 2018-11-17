pub mod init_magic;

use std::ptr;
use std::fmt;

use self::init_magic::{get_fixed_offset, get_fixed_offset_bishop};
use board::{Board, HalfBoard, BitBoard};

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
// This table is computed at runtime not to make the binary executable too big
// hence the mut keyword
// TODO better magic table with none naive arrangement and better magic factors to reduce size
// There is about 45000 holes right now
//static mut SLIDING_ATTACK_TABLE: [u64; 83352] = [0; 83352]; // 651kB
pub static mut SLIDING_ATTACK_TABLE: [u64; 88507] = [0; 88507]; // 651kB

// See move_generation/init_magic.rs for impl block with initiatlization
#[derive(Debug, Copy, Clone)]
pub struct MagicEntry {
    magic: u64,
    table: *mut u64, // Unsafe pointer not to use a safe bigger slice
    black_mask: u64,
    postmask: u64,
}

// The magic entries for rooks and bishops (also mutable because pure functions cannot access
// static variables)
pub static mut BISHOP_ATTACK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];
pub static mut ROOK_ATTACK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];

// Safe wrapper around the unsafe initialization (that have to be sequential)
pub fn init_magic_tables() {
    unsafe {
        for ((rook_entry, bishop_entry), square) in ROOK_ATTACK_TABLE.iter_mut().zip(BISHOP_ATTACK_TABLE.iter_mut()).zip(0u8..) {
            *rook_entry = MagicEntry::rook_magic(square);
            *bishop_entry = MagicEntry::bishop_magic(square);

            rook_entry.fill_attack_table(square, true);
            bishop_entry.fill_attack_table(square, false);
        }
    }
}

fn raw_sliding_attack(square: Square, occupancy: BitBoard, table: &[MagicEntry; 64]) -> BitBoard {
    let magic_entry = table[usize::from(square.0)];

    let table_pointer = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = get_fixed_offset(hash_key, magic_entry.magic);

    BitBoard::new(unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    })
}

pub fn rook_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    //raw_sliding_attack(square, occupancy, unsafe { &ROOK_ATTACK_TABLE })
    let magic_entry = unsafe {ROOK_ATTACK_TABLE[usize::from(square.0)]};

    let table_pointer = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = get_fixed_offset(hash_key, magic_entry.magic);

    BitBoard::new(unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    })
}

pub fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    //raw_sliding_attack(square, occupancy, unsafe { &BISHOP_ATTACK_TABLE })
    let magic_entry = unsafe {BISHOP_ATTACK_TABLE[usize::from(square.0)]};

    let table_pointer = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = get_fixed_offset_bishop(hash_key, magic_entry.magic);

    BitBoard::new(unsafe {
        ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask
    })
}

static KNIGHT_ATTACK_TABLE: [BitBoard; 64] = knight_attack_table(); // 512 bytes
static KING_ATTACK_TABLE: [BitBoard; 64] = king_attack_table(); // 512 bytes

// returns the bitboards of the pawns that can take the pawn in index (starting from LSB)
static EN_PASSANT_TABLE: [BitBoard; 8] = en_passant_table(); // 64 bytes 8*8 bitboards

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

//const NULL_MOVE: Move = Move::raw_new(Square::new(0), Square::new(0));

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

impl Board {
    pub fn possible_moves(&self) -> impl Iterator <Item = Move> + '_ {
        self[self.side_to_move].possible_moves().chain(self.castling()).map(move |mov| self.decorate_move(mov))
    }

    // TODO remove this once move generation is working
    pub fn debug_move_counts(&self) {
        self[self.side_to_move].debug_move_counts();
        println!("castling {}", self.castling().count());
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
        write!(f, "{}{}", self.origin_square(), self.destination_square())
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

#[allow(clippy::unreadable_literal)]
const fn knight_attack_table() -> [BitBoard; 64] {
    [
        BitBoard::new(0x20400),
        BitBoard::new(0x50800),
        BitBoard::new(0xa1100),
        BitBoard::new(0x142200),
        BitBoard::new(0x284400),
        BitBoard::new(0x508800),
        BitBoard::new(0xa01000),
        BitBoard::new(0x402000),
        BitBoard::new(0x2040004),
        BitBoard::new(0x5080008),
        BitBoard::new(0xa110011),
        BitBoard::new(0x14220022),
        BitBoard::new(0x28440044),
        BitBoard::new(0x50880088),
        BitBoard::new(0xa0100010),
        BitBoard::new(0x40200020),
        BitBoard::new(0x204000402),
        BitBoard::new(0x508000805),
        BitBoard::new(0xa1100110a),
        BitBoard::new(0x1422002214),
        BitBoard::new(0x2844004428),
        BitBoard::new(0x5088008850),
        BitBoard::new(0xa0100010a0),
        BitBoard::new(0x4020002040),
        BitBoard::new(0x20400040200),
        BitBoard::new(0x50800080500),
        BitBoard::new(0xa1100110a00),
        BitBoard::new(0x142200221400),
        BitBoard::new(0x284400442800),
        BitBoard::new(0x508800885000),
        BitBoard::new(0xa0100010a000),
        BitBoard::new(0x402000204000),
        BitBoard::new(0x2040004020000),
        BitBoard::new(0x5080008050000),
        BitBoard::new(0xa1100110a0000),
        BitBoard::new(0x14220022140000),
        BitBoard::new(0x28440044280000),
        BitBoard::new(0x50880088500000),
        BitBoard::new(0xa0100010a00000),
        BitBoard::new(0x40200020400000),
        BitBoard::new(0x204000402000000),
        BitBoard::new(0x508000805000000),
        BitBoard::new(0xa1100110a000000),
        BitBoard::new(0x1422002214000000),
        BitBoard::new(0x2844004428000000),
        BitBoard::new(0x5088008850000000),
        BitBoard::new(0xa0100010a0000000),
        BitBoard::new(0x4020002040000000),
        BitBoard::new(0x400040200000000),
        BitBoard::new(0x800080500000000),
        BitBoard::new(0x1100110a00000000),
        BitBoard::new(0x2200221400000000),
        BitBoard::new(0x4400442800000000),
        BitBoard::new(0x8800885000000000),
        BitBoard::new(0x100010a000000000),
        BitBoard::new(0x2000204000000000),
        BitBoard::new(0x4020000000000),
        BitBoard::new(0x8050000000000),
        BitBoard::new(0x110a0000000000),
        BitBoard::new(0x22140000000000),
        BitBoard::new(0x44280000000000),
        BitBoard::new(0x88500000000000),
        BitBoard::new(0x10a00000000000),
        BitBoard::new(0x20400000000000),
    ]
} // end of knight attack table


#[allow(clippy::unreadable_literal)]
const fn king_attack_table() -> [BitBoard; 64] {
    [
        BitBoard::new(0x302),
        BitBoard::new(0x705),
        BitBoard::new(0xe0a),
        BitBoard::new(0x1c14),
        BitBoard::new(0x3828),
        BitBoard::new(0x7050),
        BitBoard::new(0xe0a0),
        BitBoard::new(0xc040),
        BitBoard::new(0x30203),
        BitBoard::new(0x70507),
        BitBoard::new(0xe0a0e),
        BitBoard::new(0x1c141c),
        BitBoard::new(0x382838),
        BitBoard::new(0x705070),
        BitBoard::new(0xe0a0e0),
        BitBoard::new(0xc040c0),
        BitBoard::new(0x3020300),
        BitBoard::new(0x7050700),
        BitBoard::new(0xe0a0e00),
        BitBoard::new(0x1c141c00),
        BitBoard::new(0x38283800),
        BitBoard::new(0x70507000),
        BitBoard::new(0xe0a0e000),
        BitBoard::new(0xc040c000),
        BitBoard::new(0x302030000),
        BitBoard::new(0x705070000),
        BitBoard::new(0xe0a0e0000),
        BitBoard::new(0x1c141c0000),
        BitBoard::new(0x3828380000),
        BitBoard::new(0x7050700000),
        BitBoard::new(0xe0a0e00000),
        BitBoard::new(0xc040c00000),
        BitBoard::new(0x30203000000),
        BitBoard::new(0x70507000000),
        BitBoard::new(0xe0a0e000000),
        BitBoard::new(0x1c141c000000),
        BitBoard::new(0x382838000000),
        BitBoard::new(0x705070000000),
        BitBoard::new(0xe0a0e0000000),
        BitBoard::new(0xc040c0000000),
        BitBoard::new(0x3020300000000),
        BitBoard::new(0x7050700000000),
        BitBoard::new(0xe0a0e00000000),
        BitBoard::new(0x1c141c00000000),
        BitBoard::new(0x38283800000000),
        BitBoard::new(0x70507000000000),
        BitBoard::new(0xe0a0e000000000),
        BitBoard::new(0xc040c000000000),
        BitBoard::new(0x302030000000000),
        BitBoard::new(0x705070000000000),
        BitBoard::new(0xe0a0e0000000000),
        BitBoard::new(0x1c141c0000000000),
        BitBoard::new(0x3828380000000000),
        BitBoard::new(0x7050700000000000),
        BitBoard::new(0xe0a0e00000000000),
        BitBoard::new(0xc040c00000000000),
        BitBoard::new(0x203000000000000),
        BitBoard::new(0x507000000000000),
        BitBoard::new(0xa0e000000000000),
        BitBoard::new(0x141c000000000000),
        BitBoard::new(0x2838000000000000),
        BitBoard::new(0x5070000000000000),
        BitBoard::new(0xa0e0000000000000),
        BitBoard::new(0x40c0000000000000),
    ]
} // end of king attack table

// Prints the knight attack table
#[allow(dead_code)]
pub fn generate_knight_attacks() {
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

    for b in knight_attacks.iter() {
        println!("BitBoard::new(0x{:x}),", b.0);
    }
}

#[allow(dead_code)]
pub fn generate_king_attacks() {
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

    for b in king_attacks.iter() {
        println!("BitBoard::new(0x{:x}),", b.0);
    }
}

#[allow(dead_code)]
pub fn find_attack_table_holes() {
    unsafe {
        let mut hole_start: usize = 0;
        let mut holes_counter = 0;
        for (i, entry) in SLIDING_ATTACK_TABLE.iter().enumerate() {
            if *entry != 0 {
                if i - hole_start > 400 {
                    println!("hole at {} of size {}", hole_start, i - hole_start);
                }
                hole_start = i;
            } else {
                holes_counter += 1;
            }
        }
        println!("total holes {}", holes_counter);
    }
}

