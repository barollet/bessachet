pub mod init_magic;
pub mod moves;

use std::ptr;
use std::fmt;

use self::init_magic::{rook_offset, bishop_offset, fill_attack_table};

use board::{Board, HalfBoard, BitBoard, KING_CASTLING_RIGHTS_MASKS, QUEEN_CASTLING_RIGHTS_MASKS};

use utils::*;

pub use self::moves::*;

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


// Attack table for rooks, bishops and queens
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
        fill_attack_table(&mut attack_table, square);
    }
    println!("Magic table loaded");
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

// Returns the xray attack of the given square for pinned pieces
// See: https://www.chessprogramming.org/X-ray_Attacks_(Bitboards)
fn xray_attack(magic_entry: &MagicEntry, occupancy: BitBoard, offset_function: fn(u64, u64) -> usize) -> BitBoard {
    let attack = sliding_attack(magic_entry, occupancy, offset_function);
    let occupancy = occupancy & !attack;
    sliding_attack(magic_entry, occupancy, offset_function)
}

fn rook_xray_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square.0)];
    xray_attack(magic_entry, occupancy, rook_offset)
}

fn bishop_xray_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square.0)];
    xray_attack(magic_entry, occupancy, bishop_offset)
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
// TODO Escaping checks
// TODO Evaluation interface
// Most information fetching is lazy so this creates branching but hopefully we gain some
// computation time
#[derive(Copy, Clone)]
pub struct LegalMoveGenerator {
    // We don't use a permanent reference to a HalfBoard, we will borrow the HalfBoard each time we
    // need it
    move_stack: [Move; 128], // Allocated on the program stack with a bounded size
    // NOTE: The maximum size is 128 even if we can construct a position with 218 moves
    // maybe we have to change this to 218 in the future

    // Internals
    // TODO checkers to escape
    // We hold the squares that are pinned and no more than 2 pieces can be pinned on the same
    // direction, there is also the bitboard of the liberties of the pinned piece (to be
    // intersected with the actual moves of the piece)
    pinned_pieces: [(Square, BitBoard); 8],
    number_of_pinned_pieces: usize, // pinners stack indexes (basically a usize for 0 1 2)
    free_pieces: BitBoard, // A global pin mask to quickly get if a piece is pinned or free
    // next move on the stack
    last_move: usize,
    // iterator index
    next_iterator_move: usize,
    // copies of the caslting rights, previous en passant state and halfmove clock
    // NOTE: the en passant square is held in the HalfBoard, so we need to restore the state to use
    // it
    en_passant: Option<Square>,
    castling_rights: u8,
    halfmove_clock: u8,
}

/*
type Direction = usize;
const VERTICAL: Direction = 0;
const HORIZONTAL: Direction = 1;
const DIAGONAL: Direction = 2;
const ANTIDIAGONAL: Direction = 3;
*/

impl LegalMoveGenerator {
    // Initialize a new LegalMoveGenerator by computing pinned pieces
    // It takes a reference to the current board and the color of the player we want to move
    pub fn new(halfboard: &HalfBoard, color: Color, castling_rights: u8, halfmove_clock: u8) -> Self {
        let mut generator = Self {
            move_stack: [NULL_MOVE; 128], // Placeholder

            pinned_pieces: [(A1_SQUARE, BitBoard::empty()); 8], // Placeholder
            number_of_pinned_pieces: 0,
            free_pieces: BitBoard::full(),

            last_move: 0,
            next_iterator_move: 0,
            en_passant: halfboard.en_passant,
            castling_rights,
            halfmove_clock,
        };
        // we compute pinned pieces and store the result for evaluation
        generator.compute_pinners(halfboard);
        // fetch basic moves information
        generator.fetch_possible_moves(halfboard, color);

        generator
    }

    fn bishop_pin_offset(pinner_square: Square, target_square: Square) -> u8 {
        let distance = target_square - pinner_square;
        if distance % 9 == 0 {
            // Antidiagonal
            9
        } else {
            // Diagonal
            7
        }
    }

    fn rook_pin_offset(pinner_square: Square, target_square: Square) -> u8 {
        if target_square % 8 == pinner_square % 8 {
            // Vertical
            8
        } else {
            // Horizontal
            1
        }
    }

    fn push_pinned_piece(&mut self, board: &HalfBoard,
                         pinner_square: Square,
                         target_square: Square,
                         offset_function: fn(Square, Square) -> u8) {
        let distance = target_square - pinner_square;
        let offset = offset_function(pinner_square, target_square);
        // + 1 is for overlapping with both target and pinner square
        // the target square is removed afterward
        let size = distance / offset + 1;

        let base = ((1 << (offset*size)) - 1) / ((1 << offset) - 1);
        let pin_mask = BitBoard::new(base << std::cmp::min(target_square, pinner_square).0);

        // remove the target square
        let pin_mask = pin_mask & !target_square.as_bitboard();
        // get the pinned piece
        // empty if check and no pin TODO checks to escape
        let pinned_square = pin_mask & board[Color::WHITE];
        if pinned_square != BitBoard::empty() {
            let pinned_square = pinned_square.as_square();
            // updates the pin datastructure
            self.pinned_pieces[self.number_of_pinned_pieces] = (pinned_square, pin_mask);
            self.number_of_pinned_pieces += 1;
            self.free_pieces &= !pin_mask;
        } else {
            // TODO Check
        }
    }

    fn compute_pinners(&mut self, board: &HalfBoard) {
        let white_king_square = (board[Piece::KING] & board[Color::WHITE]).as_square();
        // Pinned by a bishop move
        for bishop_square in (board[Piece::BISHOP] | board[Piece::QUEEN]) & board[Color::BLACK] {
            let xray_attack = bishop_xray_attack(bishop_square, board.occupied_squares());
            if xray_attack.has_square(white_king_square) {
                self.push_pinned_piece(board, bishop_square, white_king_square, Self::bishop_pin_offset);
            }
        }
        // Pinned by a rook move
        for rook_square in (board[Piece::ROOK] | board[Piece::QUEEN]) & board[Color::BLACK] {
            let xray_attack = rook_xray_attack(rook_square, board.occupied_squares());
            if xray_attack.has_square(white_king_square) {
                self.push_pinned_piece(board, rook_square, white_king_square, Self::rook_pin_offset);
            }
        }
    }

    // All the move generation logic
    // For captures, the captured piece is set when the move is iterated upon
    // TODO fetch only captures first and then quiet moves to make it lazier
    // ------------------------------------------------
    fn fetch_possible_moves(&mut self, board: &HalfBoard, color: Color) {
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
            self.push_promotions(Move::tactical_move(capture_square.behind_right(), capture_square, CAPTURE_FLAG | PROMOTION_FLAG));
        }
        for capture_square in right_capture_moves & ROW_8 {
            self.push_promotions(Move::tactical_move(capture_square.behind_left(), capture_square, CAPTURE_FLAG | PROMOTION_FLAG));
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
            self.push_captures_quiets(board, knight_square, attack);
        }
        // -----------------------------------------

        // Bishop moves ----------------------------
        for bishop_square in board[Piece::BISHOP] & board[Color::WHITE] {
            self.sliding_attack(board, bishop_square, bishop_attack);
        }
        // -----------------------------------------

        // Rook moves ------------------------------
        for rook_square in board[Piece::ROOK] & board[Color::WHITE] {
            self.sliding_attack(board, rook_square, rook_attack);
        }
        // -----------------------------------------

        // Queen moves -----------------------------
        for queen_square in board[Piece::QUEEN] & board[Color::WHITE] {
            self.sliding_attack(board, queen_square, bishop_attack);
            self.sliding_attack(board, queen_square, rook_attack);
        }
        // -----------------------------------------

        // King moves ------------------------------
        // Moves
        let king_square = (board[Piece::KING] & board[Color::WHITE]).as_square();
        let attack = KING_ATTACK_TABLE[king_square.as_index()];
        self.push_captures_quiets(board, king_square, attack);
        // Castle
        if self.can_king_castle(board, color) {
            self.push(KING_CASTLE_MOVES[color]);
        }
        if self.can_queen_castle(board, color) {
            self.push(QUEEN_CASTLE_MOVES[color])
        }
        // -----------------------------------------
    }

    // Pushs the given move in the move stack
    fn push(&mut self, pushed_move: Move) {
        self.move_stack[self.last_move] = pushed_move;
        self.last_move += 1;
    }

    // Helper to push all the possible promotions
    fn push_promotions(&mut self, promotion_move: Move) {
        for promoted_piece in &AVAILABLE_PROMOTION {
            self.push(promotion_move.set_promoted_piece(*promoted_piece));
        }
    }

    // Decorates the next move to be fetched for iteration with irreversible states
    fn decorate_move(&self, mov: Move) -> ExtendedMove {
        ExtendedMove::from_raw_move(mov)
            .set_board_state(self.castling_rights, CASTLING_RIGHTS_BITS_OFFSET)
            // En passant square is given from the side to play pov
            .set_board_state(self.en_passant.map_or(0, |square| square.0), EN_PASSANT_SQUARE_BITS_OFFSET)
            .set_board_state(self.halfmove_clock, HALFMOVE_CLOCK_BITS_OFFSET)
    }

    // Helper for pieces that can perform captures and quiet moves at the same time
    fn push_captures_quiets(&mut self, board: &HalfBoard, origin_square: Square, attack: BitBoard) {
        // Captures
        for capture_square in attack & board[Color::BLACK] {
            self.push(Move::tactical_move(origin_square, capture_square, CAPTURE_FLAG));
        }
        // Quiet moves
        for dest_square in attack & board.empty_squares() {
            self.push(Move::quiet_move(origin_square, dest_square));
        }
    }

    // Helper for sliders
    fn sliding_attack(&mut self, board: &HalfBoard, origin_square: Square, piece_attack: fn (Square, BitBoard) -> BitBoard) {
        let attack = piece_attack(origin_square, board.occupied_squares());
        self.push_captures_quiets(board, origin_square, attack);
    }

    // Castling
    fn can_king_castle(&self, board: &HalfBoard, color: Color) -> bool {
        (self.castling_rights & KING_CASTLING_RIGHTS_MASKS[color] != 0) // right to castle kingside
        && (KING_CASTLE_EMPTY[color] & board.occupied_squares() == 0) // none of the squares on the way are occupied
        && (KING_CASTLE_CHECK[color].all(|square| !self.is_in_check(board, square))) // squares crossed by the king are in check
    }

    fn can_queen_castle(&self, board: &HalfBoard, color: Color) -> bool {
        (self.castling_rights & QUEEN_CASTLING_RIGHTS_MASKS[color] != 0) // right to castle queenside
        && (QUEEN_CASTLE_EMPTY[color] & board.occupied_squares() == 0) // none of the squares on the way are occupied
        && (QUEEN_CASTLE_CHECK[color].all(|square| !self.is_in_check(board, square))) // squares crossed by the king are in check
    }

    // Uses a super piece (not to rely on the other side move generator)
    fn is_in_check(&self, board: &HalfBoard, square: Square) -> bool {
        // Rook
        rook_attack(square, board.occupied_squares()) & board[Color::BLACK] & (board[Piece::ROOK] | board[Piece::QUEEN]) != 0 ||
        // Bishop
        bishop_attack(square, board.occupied_squares()) & board[Color::BLACK] & (board[Piece::BISHOP] | board[Piece::QUEEN]) != 0 ||
        // Knight
        KNIGHT_ATTACK_TABLE[square.as_index()] & board[Color::BLACK] & board[Piece::KNIGHT] != 0 ||
        // Pawn
        !ROW_8.has_square(square) & !ROW_7.has_square(square) & (
            !FILE_A.has_square(square) & (board[Color::BLACK] & board[Piece::PAWN]).has_square(square.forward_left()) ||
            !FILE_H.has_square(square) & (board[Color::BLACK] & board[Piece::PAWN]).has_square(square.forward_right())
        ) ||
        // King
        KING_ATTACK_TABLE[square.as_index()] & board[Color::BLACK] & board[Piece::KING] != 0
    }

    // TODO Remove this when move generation is legal
    pub fn is_king_checked(&self, board: &HalfBoard) -> bool {
        self.is_in_check(board, (board[Piece::KING] & board[Color::WHITE]).as_square())
    }

    // Returns an attack map of the given position with White playing
    pub fn attack_map(&self, board: &HalfBoard) -> BitBoard {
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
impl Iterator for LegalMoveGenerator {
    type Item = ExtendedMove;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_iterator_move < self.last_move {
            let iter_move = self.move_stack[self.next_iterator_move];
            self.next_iterator_move += 1;

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

    // TODO remove this once move generation is working
    pub fn get_move(&mut self, origin_file: char, origin_row: char, dest_file: char, dest_row: char) -> ExtendedMove {
        let origin_square = Square::from_char_file_rank(origin_file, origin_row);
        let dest_square = Square::from_char_file_rank(dest_file, dest_row);
        let generator = self.create_legal_move_generator();
        for ext_mov in generator {
            let mov = ext_mov.get_raw_move();
            if mov.origin_square() == origin_square && mov.destination_square() == dest_square {
                return ext_mov;
            }
        }
        panic!("Can't find move {}{}{}{}", origin_file, origin_row, dest_file, dest_row);
    }
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


impl fmt::Display for ExtendedMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mov = self.get_raw_move();
        write!(f, "{}{}{}", mov.origin_square(), mov.destination_square(), match mov.get_promotion_piece() {
            Some(Piece::KNIGHT) => "n",
            Some(Piece::BISHOP) => "b",
            Some(Piece::ROOK) => "r",
            Some(Piece::QUEEN) => "q",
            _ => "",
        })
    }
}

impl fmt::Debug for ExtendedMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mov = self.get_raw_move();
        write!(f, "{}{} p:{} c:{} sp1:{} sp0:{}", mov.origin_square(),
                                                  mov.destination_square(),
                                                  mov.has_flags(PROMOTION_FLAG),
                                                  mov.has_flags(CAPTURE_FLAG),
                                                  mov.has_flags(SPECIAL1_FLAG),
                                                  mov.has_flags(SPECIAL0_FLAG))
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
