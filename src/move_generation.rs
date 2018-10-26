mod init_magic;

use std::ptr;
use std::fmt;

use self::init_magic::get_fixed_offset;
use board::{Board, BitBoard};

use utils::*;

// unsafe attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// This table is computed at runtime not to make the executable table too big
// hence the mut keyword
static mut SLIDING_ATTACK_TABLE: [u64; 50866] = [0; 50866]; // 406928 bytes

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
static mut BISHOP_ATTACK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];
static mut ROOK_ATTACK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];

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

fn rook_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    raw_sliding_attack(square, occupancy, unsafe { &ROOK_ATTACK_TABLE })
}

fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    raw_sliding_attack(square, occupancy, unsafe { &BISHOP_ATTACK_TABLE })
}

static KNIGHT_ATTACK_TABLE: [BitBoard; 64] = knight_attack_table(); // 512 bytes

// Moves iterator
pub struct Move(u16);

impl Move {
    // Returns a new move with the initial and destination squares
    fn new_from_to(from: Square, to: Square) -> Self {
        Move(u16::from(from.0) + (u16::from(to.0) << 6))
    }

    // Helper to get an iterator over promotions
    fn from_to_with_promotion(from: Square, to: Square) -> impl Iterator <Item = Move> {
        // TODO
    }

    fn initial_square(&self) -> Square {
        Square::new((self.0 & 0x3f) as u8)
    }

    fn destination_square(&self) -> Square {
        Square::new((self.0 >> 6) as u8 & 0x3f)
    }
}

impl Board {
    // Returns the basic pawn pushs without promotion
    pub fn simple_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        // basic pawns are pawns that won't promote (ie not on the last line)
        let basic_pawns = self[Pieces::PAWN] & self[Color::WHITE] & !ROW_7;
        let simple_pushed_pawns = (basic_pawns << 8) & self.empty_squares();

        simple_pushed_pawns.map(|bitboard|
                                Move::new_from_to((bitboard >> 8).as_square(),
                                                  bitboard.as_square()))
    }

    // Returns the pawns that can do the initial double pushs
    pub fn double_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        let starting_pawns = self[Pieces::PAWN] & self[Color::WHITE] & ROW_2;

        // To be double pushed, the pawns have to be able to move once forward
        let simple_pushed_pawns = (starting_pawns << 8) & self.empty_squares();
        // The pawns that can both be pushed for one and two lines forward
        let double_pushed_pawns = (simple_pushed_pawns << 8) & self.empty_squares();

        double_pushed_pawns.map(|bitboard|
                                Move::new_from_to((bitboard >> 16).as_square(),
                                                  bitboard.as_square()))
    }

    pub fn pawn_captures_without_promotion(&self) -> impl Iterator <Item = Move> {
        let basic_pawns = self[Pieces::PAWN] & self[Color::WHITE] & !ROW_7;
        // left white capture
        let left_capture_iterator = ((basic_pawns & !FILE_A) << 7 & self.empty_squares())
            .map(|bitboard| Move::new_from_to((bitboard >> 7).as_square(), bitboard.as_square()));
        // right white capture
        let right_capture_iterator = ((basic_pawns & !FILE_H) << 9 & self.empty_squares())
            .map(|bitboard| Move::new_from_to((bitboard >> 9).as_square(), bitboard.as_square()));
        left_capture_iterator.chain(right_capture_iterator)
    }
    // TODO Pawn push with promotion
    pub fn pawn_promotion_moves(&self) -> impl Iterator <Item = Move> {
        let promoting_pawns = self[Pieces::PAWN] & self[Color::WHITE] & ROW_7;
        // push
        let push_promotion_iterator = (promoting_pawns << 8) & self.empty_squares()
        // catpure
    }
    //
    // TODO Pawn en passant

    pub fn knight_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Pieces::KNIGHT] & self[Color::WHITE])
            .flat_map(move |knight| (KNIGHT_ATTACK_TABLE[knight.as_index()] & !self[Color::WHITE])
                      .map(move |bitboard| Move::new_from_to(knight.as_square(), bitboard.as_square())))
    }

    // TODO redo this with quiet moves and capture moves distinction
    fn sliding_attack(&self, piece: BitBoard, piece_attack: fn (Square, BitBoard) -> BitBoard) -> impl Iterator <Item = Move> {
        (piece_attack(piece.as_square(), self.occupied_squares()) & !self[Color::WHITE])
            .map(move |bitboard| Move::new_from_to(piece.as_square(), bitboard.as_square()))
    }

    pub fn bishop_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Pieces::BISHOP] & self[Color::WHITE]).flat_map(move |bishop| self.sliding_attack(bishop, bishop_attack))
    }

    pub fn rook_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Pieces::ROOK] & self[Color::WHITE]).flat_map(move |rook| self.sliding_attack(rook, rook_attack))
    }

    pub fn queen_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Pieces::QUEEN] & self[Color::WHITE]).flat_map(move |queen| self.sliding_attack(queen, bishop_attack)
                                                                                    .chain(self.sliding_attack(queen, rook_attack)))
    }

    // TODO king
    // TODO castle
}




impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Move {} {}", self.initial_square(), self.destination_square())
    }
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
}

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
pub fn find_attack_table_holes() {
    unsafe {
        let mut hole_start: usize = 0;
        let mut holes_counter = 0;
        for (i, entry) in SLIDING_ATTACK_TABLE.iter().enumerate() {
            if *entry != 0 {
                if i - hole_start > 50 {
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

