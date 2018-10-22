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
static mut ATTACK_TABLE: [u64; 50866] = [0; 50866]; // 406928 bytes

// See move_generation/init_magic.rs for impl block with initiatlization
#[derive(Debug, Copy, Clone)]
pub struct MagicEntry {
    magic: u64,
    table: *mut u64, // Unsafe pointer not to use a safe bigger slice
    black_mask: u64,
    postmask: u64,
}

// The magic entries for rooks and bishops (also mutable because pure functions cannot access
// static variable)
static mut BISHOP_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];
static mut ROOK_TABLE: [MagicEntry; 64] = [MagicEntry::empty_magic(); 64];

// Safe wrapper around the unsafe initialization (that have to be sequential)
pub fn init_magic_tables() {
    unsafe {
        for ((rook_entry, bishop_entry), square) in ROOK_TABLE.iter_mut().zip(BISHOP_TABLE.iter_mut()).zip(0u8..) {
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
    unsafe { raw_sliding_attack(square, occupancy, &ROOK_TABLE) }
}

fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    unsafe { raw_sliding_attack(square, occupancy, &BISHOP_TABLE) }
}


// Moves iterator
pub struct Move(u16);

impl Move {
    // Returns a new move with the initial and destination squares
    fn new_from_to(from: Square, to: Square) -> Self {
        Move(u16::from(from.0) + (u16::from(to.0) << 6))
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
        let basic_pawns = self.pawns.intersect(self.occupancy[WHITE]).intersect(LAST_LINE.not());
        let simple_pushed_pawns = basic_pawns.shift_left(8).intersect(self.empty_squares());

        simple_pushed_pawns.map(|bitboard|
                                Move::new_from_to(bitboard.shift_right(8).as_square(),
                                                  bitboard.as_square()))
    }

    // Returns the pawns that can do the initial double pushs
    pub fn double_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        let starting_pawns = self.pawns.intersect(self.occupancy[WHITE]).intersect(PAWN_FIRST_LINE);

        // To be double pushed, the pawns have to be able to move once forward
        let simple_pushed_pawns = starting_pawns.shift_left(8).intersect(self.empty_squares());
        // The pawns that can both be pushed for one and two lines forward
        let double_pushed_pawns = simple_pushed_pawns.shift_left(8).intersect(self.empty_squares());

        double_pushed_pawns.map(|bitboard|
                                Move::new_from_to(bitboard.shift_right(16).as_square(),
                                                  bitboard.as_square()))
    }

    // TODO Pawn captures
    //
    // TODO Pawn push with promotion
    //
    // TODO Pawn en passant

    // TODO Knight

    // TODO redo this with quiet moves and capture moves distinction
    fn sliding_attack(&self, piece: BitBoard, piece_attack: fn (Square, BitBoard) -> BitBoard) -> impl Iterator <Item = Move> {
        piece_attack(piece.as_square(), self.occupied_squares())
            .intersect(self.occupancy[WHITE].not())
            .map(move |bitboard| Move::new_from_to(piece.as_square(), bitboard.as_square()))
    }

    pub fn bishop_moves(&self) -> impl Iterator <Item = Move> + '_ {
        self.bishops.intersect(self.occupancy[WHITE]).flat_map(move |bishop| self.sliding_attack(bishop, bishop_attack))
    }

    pub fn rook_moves(&self) -> impl Iterator <Item = Move> + '_ {
        self.rooks.intersect(self.occupancy[WHITE]).flat_map(move |rook| self.sliding_attack(rook, rook_attack))
    }

    pub fn queen_moves(&self) -> impl Iterator <Item = Move> + '_ {
        self.bishops.intersect(self.occupancy[WHITE]).flat_map(move |bishop| self.sliding_attack(bishop, bishop_attack))
    }

    // TODO king
    // TODO castle
}




impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Move {} {}", self.initial_square(), self.destination_square())
    }
}

#[allow(dead_code)]
pub fn find_attack_table_holes() {
    unsafe {
        let mut hole_start: usize = 0;
        let mut holes_counter = 0;
        for (i, entry) in ATTACK_TABLE.iter().enumerate() {
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

