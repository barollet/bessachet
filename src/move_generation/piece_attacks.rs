use std::ptr;

use move_generation::init_magic::*;
use std::convert::From;

use types::*;

// This module provides a legal move generator to a board
// through the possible_moves interface
// TODO improve interface

// Attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// See move_generation/init_magic.rs for impl block with initiatlization
pub struct MagicEntry {
    pub magic: u64,
    pub table: &'static BitBoard, // static reference coerced to a pointer
    pub black_mask: BitBoard,
    pub postmask: BitBoard,
}

pub type SlidingAttackTable = Vec<BitBoard>; // Heap allocated table
type AttackTable = [BitBoard; 64];

// NOTE: lazy statics uses an atomic check for each access, maybe at some point we will need to
// remove this and come back to classical static mut or something else to make it faster
lazy_static! {
    pub static ref BISHOP_ATTACK_TABLE: [MagicEntry; 64] = init_magic_entries(MagicEntry::bishop_magic);
    pub static ref ROOK_ATTACK_TABLE: [MagicEntry; 64] = init_magic_entries(MagicEntry::rook_magic);

    // TODO better magic table with none naive arrangement and better magic factors to reduce size
    pub static ref SLIDING_ATTACK_TABLE: SlidingAttackTable = init_sliding_attack_tables();

    static ref KNIGHT_ATTACK_TABLE: AttackTable = generate_knight_attacks(); // 512 bytes
    static ref KING_ATTACK_TABLE: AttackTable = generate_king_attacks(); // 512 bytes
    static ref PAWN_ATTACK_TABLE: BlackWhiteAttribute<AttackTable> = generate_pawn_attacks(); // 2*512 bytes

    // returns the BitBoard of candidates to capture and en passant target on the given file
    pub static ref EN_PASSANT_TABLE: BlackWhiteAttribute<[BitBoard; 8]> = generate_en_passant_table(); // 2*64 bytes 8*8 bitboards
}

pub const EN_PASSANT_LINE: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(ROW_5, ROW_4);
pub const STARTING_ROW: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(ROW_7, ROW_2);

pub fn sliding_attack(
    magic_entry: &MagicEntry,
    occupancy: BitBoard,
    offset_function: fn(BitBoard, u64) -> usize,
) -> BitBoard {
    let table_pointer: *const BitBoard = magic_entry.table;

    let hash_key = occupancy | magic_entry.black_mask;
    let table_offset = offset_function(hash_key, magic_entry.magic);

    unsafe { ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask }
}

pub fn rook_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square)];
    sliding_attack(magic_entry, occupancy, rook_offset)
}

pub fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square)];
    sliding_attack(magic_entry, occupancy, bishop_offset)
}

pub fn knight_attack(square: Square) -> BitBoard {
    KNIGHT_ATTACK_TABLE[square as usize]
}

pub fn king_attack(square: Square) -> BitBoard {
    KING_ATTACK_TABLE[square as usize]
}

pub fn pawn_attack(square: Square, color: Color) -> BitBoard {
    PAWN_ATTACK_TABLE[color][square as usize]
}

// Returns the xray attack of the given square for pinned pieces
// See: https://www.chessprogramming.org/X-ray_Attacks_(Bitboards)
pub fn xray_attack(
    magic_entry: &MagicEntry,
    occupancy: BitBoard,
    offset_function: fn(BitBoard, u64) -> usize,
) -> BitBoard {
    let attack = sliding_attack(magic_entry, occupancy, offset_function);
    let occupancy = occupancy & !attack;
    sliding_attack(magic_entry, occupancy, offset_function)
}

pub type XrayFunction = fn(Square, BitBoard) -> BitBoard;

pub fn rook_xray_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square)];
    xray_attack(magic_entry, occupancy, rook_offset)
}

pub fn bishop_xray_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square)];
    xray_attack(magic_entry, occupancy, bishop_offset)
}

fn generate_en_passant_table() -> BlackWhiteAttribute<[BitBoard; 8]> {
    let mut white_en_passant_candidates = [BBWrapper::empty(); 8];
    for square in BBWrapper(ROW_5) {
        if square.file() != 0 {
            white_en_passant_candidates[square.file() as usize].add_square(square.left());
        }
        if square.file() != 7 {
            white_en_passant_candidates[square.file() as usize].add_square(square.right());
        }
    }

    let black_en_passant_candidates =
        array_init::array_init(|i| white_en_passant_candidates[i] >> 8);

    BlackWhiteAttribute::new(black_en_passant_candidates, white_en_passant_candidates)
}

fn generate_pawn_attacks() -> BlackWhiteAttribute<AttackTable> {
    let mut white_pawn_attacks = [BBWrapper::empty(); 64]; // First and last row while remain empty

    for square in BBWrapper(SQUARES & !ROW_8) {
        if !FILE_A.has_square(square) {
            white_pawn_attacks[square as usize].add_square(square.forward_left());
        }
        if !FILE_H.has_square(square) {
            white_pawn_attacks[square as usize].add_square(square.forward_right());
        }
    }

    let mut black_pawn_attacks: [BitBoard; 64] =
        array_init::array_init(|i| white_pawn_attacks[i] >> 16);
    for square in BBWrapper(ROW_8) {
        black_pawn_attacks[square as usize] =
            black_pawn_attacks[square.forward(BLACK) as usize] << 8;
    }

    BlackWhiteAttribute::new(black_pawn_attacks, white_pawn_attacks)
}

fn generate_knight_attacks() -> AttackTable {
    let mut knight_attacks = [BBWrapper::empty(); 64];

    let knight_moves = [
        (1, 2),
        (1, -2),
        (-1, 2),
        (-1, -2),
        (2, 1),
        (2, -1),
        (-2, 1),
        (-2, -1),
    ];

    for (attack_bitboard, sq) in knight_attacks.iter_mut().zip(0u8..) {
        let (rank, file) = sq.rank_file();
        let (rank, file) = (rank as i8, file as i8);

        for (i, j) in &knight_moves {
            if file + i >= 0 && file + i < 8 && rank + j >= 0 && rank + j < 8 {
                attack_bitboard.add_square(SqWrapper::from_file_rank(
                    (file + i) as u8,
                    (rank + j) as u8,
                ));
            }
        }
    }

    knight_attacks
}

fn generate_king_attacks() -> AttackTable {
    let mut king_attacks = [BBWrapper::empty(); 64];

    let king_moves = [
        (1, 1),
        (1, 0),
        (1, -1),
        (0, 1),
        (0, -1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
    ];

    for (attack_bitboard, sq) in king_attacks.iter_mut().zip(0u8..) {
        let (rank, file) = sq.rank_file();
        let (rank, file) = (rank as i8, file as i8);

        for (i, j) in &king_moves {
            if file + i >= 0 && file + i < 8 && rank + j >= 0 && rank + j < 8 {
                attack_bitboard.add_square(SqWrapper::from_file_rank(
                    (file + i) as u8,
                    (rank + j) as u8,
                ));
            }
        }
    }

    king_attacks
}
