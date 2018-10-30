mod init_magic;

use std::ptr;
use std::fmt;

use self::init_magic::get_fixed_offset;
use board::{Board, BitBoard};

use utils::*;
use enum_primitive::FromPrimitive;

// Legal move generator


// unsafe attack table for rooks, bishops and queens
// this is a black magic fancy table with shared attacks
// See: https://www.chessprogramming.org/Magic_Bitboards
//
// This table is computed at runtime not to make the binary executable too big
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
static KING_ATTACK_TABLE: [BitBoard; 64] = king_attack_table(); // 512 bytes

// returns the bitboards of the pawns that can take the pawn in index (starting from LSB)
static EN_PASSANT_TABLE: [BitBoard; 9] = en_passant_table(); // 72 bytes 64 + 8 for no en passant target

// A Move is a 32 bits word following more or the less the extended
// representation in https://www.chessprogramming.org/Encoding_Moves
// MSB -------------------------------------------------------- LSB
// pc3 .. pc0 | prom | capt | sp1 | sp0 | dst5 .. dst0 | st5 .. st0
// 19  .. 16  |  15  |  14  |  13 | 12  |  11  ..  6   |  5  ..  0
#[derive(Clone, Copy)]
pub struct Move(u32);

impl Move {
    // Returns a new move with the initial and destination squares
    #[inline]
    fn new_quiet_move(from: Square, to: Square) -> Self {
        Move(u32::from(from.0) + (u32::from(to.0) << 6))
    }

    #[inline]
    fn new_capture_move(from: Square, to: Square, capture: Piece) -> Self {
        Self::new_quiet_move(from, to)
            .set_capture()
            .set_captured_piece(capture)
    }

    #[inline]
    fn en_passant(self) -> Self {
        Move(self.0).set_sp(1)
    }

    #[inline]
    fn double_push(self) -> Self {
        Move(self.0).set_sp(1)
    }

    #[inline]
    fn king_castle(self) -> Self {
        Move(self.0).set_sp(2)
    }

    #[inline]
    fn queen_castle(self) -> Self {
        Move(self.0).set_sp(3)
    }

    // Helper to get an iterator over promotions
    #[inline]
    fn promotion_move_iterator(self) -> impl Iterator <Item = Move> {
        AVAILABLE_PROMOTION.iter().map(move |piece| self.set_promotion(*piece))
    }

    #[inline]
    fn set_promotion(self, promotion: Piece) -> Self {
        Move(self.0 + 0x8000 + ((promotion as u32) << 12))
    }

    #[inline]
    fn set_capture(self) -> Self {
        Move(self.0 + 0x4000)
    }

    #[inline]
    fn set_captured_piece(self, piece: Piece) -> Self {
        Move(self.0 | ((piece as u32 & 0xf) << 16))
    }

    // Returns the captured piece if any (doesnt include en passant)
    #[inline]
    pub fn captured_piece(self) -> Option<Piece> {
        if self.0 & 0x4000 != 0 && self.0 & !0x5000 != 0 { // if the capture is not an en passant capture
            // from_u32 returns an Option<Piece> depending if the piece is valid or not
            Piece::from_u32(self.0 >> 16)
        } else {
            None
        }
    }

    #[inline]
    fn is_promotion(self) -> bool {
        self.0 & 0xa000 != 0
    }

    // Returns the rook moves involved in the castle move
    // TODO make it better
    #[inline]
    pub fn castle_rook(self) -> Option<(Square, Square)> {
        if !self.is_promotion() {
            let sp_val = self.get_sp();
            if sp_val == 2 {
                Some((WHITE_ROOK_KING_CASTLE_FROM_SQUARE, WHITE_ROOK_KING_CASTLE_DEST_SQUARE))
            } else if sp_val == 3 {
                Some((WHITE_ROOK_QUEEN_CASTLE_FROM_SQUARE, WHITE_ROOK_QUEEN_CASTLE_DEST_SQUARE))
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    fn set_sp(self, special_code: u32) -> Self {
        Move(self.0 | ((special_code & 0x3) << 12))
    }

    #[inline]
    fn get_sp(self) -> u32 {
        self.0 >> 12 & 0x3
    }

    #[inline]
    pub fn get_en_passant_target(self) -> BitBoard {
        self.destination_square().as_bitboard()
    }

    #[inline]
    pub fn initial_square(&self) -> Square {
        Square::new((self.0 & 0x3f) as u8)
    }

    #[inline]
    pub fn destination_square(&self) -> Square {
        Square::new((self.0 >> 6) as u8 & 0x3f)
    }
}

// Moves iterator
impl Board {

    // Returns the basic pawn pushs without promotion
    pub fn simple_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        // basic pawns are pawns that won't promote (ie not on the last line)
        let basic_pawns = self[Piece::PAWN] & self[Color::WHITE] & !ROW_7;
        let simple_pushed_pawns = (basic_pawns << 8) & self.empty_squares();

        simple_pushed_pawns.map(|bitboard|
                                Move::new_quiet_move((bitboard >> 8).as_square(),
                                                  bitboard.as_square()))
    }

    // Returns the pawns that can do the initial double pushs
    pub fn double_pawn_pushs(&self) -> impl Iterator <Item = Move> {
        let starting_pawns = self[Piece::PAWN] & self[Color::WHITE] & ROW_2;

        // To be double pushed, the pawns have to be able to move once forward
        let simple_pushed_pawns = (starting_pawns << 8) & self.empty_squares();
        // The pawns that can both be pushed for one and two lines forward
        let double_pushed_pawns = (simple_pushed_pawns << 8) & self.empty_squares();

        double_pushed_pawns.map(|bitboard|
                                Move::new_quiet_move((bitboard >> 16).as_square(),
                                                  bitboard.as_square())
                                .double_push())
    }

    pub fn pawn_captures_without_promotion(&self) -> impl Iterator <Item = Move> {
        let basic_pawns = self[Piece::PAWN] & self[Color::WHITE] & !ROW_7;
        // left white capture
        let left_capture_iterator = ((basic_pawns & !FILE_A) << 7 & self.empty_squares())
            .map(|bitboard| Move::new_quiet_move((bitboard >> 7).as_square(), bitboard.as_square()));
        // right white capture
        let right_capture_iterator = ((basic_pawns & !FILE_H) << 9 & self.empty_squares())
            .map(|bitboard| Move::new_quiet_move((bitboard >> 9).as_square(), bitboard.as_square()));
        left_capture_iterator.chain(right_capture_iterator)
    }

    // TODO remove duplicates with the code without promotion
    pub fn pawn_promotion_moves(&self) -> impl Iterator <Item = Move> + '_ {
        let promoting_pawns = self[Piece::PAWN] & self[Color::WHITE] & ROW_7;
        // push
        let push_promotion_iterator = ((promoting_pawns << 8) & self.empty_squares())
            .map(|bitboard| Move::new_quiet_move((bitboard >> 8).as_square(), bitboard.as_square()));
        // catpure
        // left
        let left_capture_iterator = ((promoting_pawns & !FILE_A) << 7 & self.empty_squares())
            .map(move |bitboard| Move::new_capture_move((bitboard >> 7).as_square(), bitboard.as_square(), self[bitboard.as_square()].unwrap()));
        // right
        let right_capture_iterator = ((promoting_pawns & !FILE_H) << 9 & self.empty_squares())
            .map(move |bitboard| Move::new_capture_move((bitboard >> 9).as_square(), bitboard.as_square(), self[bitboard.as_square()].unwrap()));

        push_promotion_iterator.chain(left_capture_iterator).chain(right_capture_iterator).flat_map(|mov| mov.promotion_move_iterator())
    }

    pub fn en_passant_captures(&self) -> impl Iterator <Item = Move> + '_ {
        (EN_PASSANT_TABLE[self.en_passant_target_index()] & self[Piece::PAWN])
            .map(move |bitboard| Move::new_quiet_move(bitboard.as_square(), self.en_passant_square()).set_capture().en_passant())
    }

    pub fn knight_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::KNIGHT] & self[Color::WHITE])
            .flat_map(move |knight| (KNIGHT_ATTACK_TABLE[knight.as_index()] & !self[Color::WHITE])
                      .map(move |bitboard| Move::new_quiet_move(knight.as_square(), bitboard.as_square())))
    }

    // TODO redo this with quiet moves and capture moves distinction
    fn sliding_attack(&self, piece: BitBoard, piece_attack: fn (Square, BitBoard) -> BitBoard) -> impl Iterator <Item = Move> {
        (piece_attack(piece.as_square(), self.occupied_squares()) & !self[Color::WHITE])
            .map(move |bitboard| Move::new_quiet_move(piece.as_square(), bitboard.as_square()))
    }

    pub fn bishop_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::BISHOP] & self[Color::WHITE]).flat_map(move |bishop| self.sliding_attack(bishop, bishop_attack))
    }

    pub fn rook_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::ROOK] & self[Color::WHITE]).flat_map(move |rook| self.sliding_attack(rook, rook_attack))
    }

    pub fn queen_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::QUEEN] & self[Color::WHITE]).flat_map(move |queen| self.sliding_attack(queen, bishop_attack)
                                                           .chain(self.sliding_attack(queen, rook_attack)))
    }

    pub fn king_moves(&self) -> impl Iterator <Item = Move> + '_ {
        (self[Piece::KING] & self[Color::WHITE])
            .flat_map(move |king| (KING_ATTACK_TABLE[king.as_index()] & !self[Color::WHITE])
                      .map(move |bitboard| Move::new_quiet_move(king.as_square(), bitboard.as_square())))
    }

    // Castling moves are only encoding the king move
    pub fn castling(&self) -> impl Iterator <Item = Move> + '_ {
        self.king_castling().map(|bitboard|
                                 Move::new_quiet_move(WHITE_KING_STARTING_SQUARE, bitboard.as_square()).king_castle()
        ).chain(self.queen_castling().map(|bitboard|
                                          Move::new_quiet_move(WHITE_KING_STARTING_SQUARE, bitboard.as_square()).queen_castle())
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
            .chain(self.castling())
    }
}




impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Move {} {}", self.initial_square(), self.destination_square())
    }
}

#[allow(clippy::unreadable_literal)]
const fn en_passant_table() -> [BitBoard; 9] {
    [
        BitBoard::empty(), // No en passant target
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
}


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

