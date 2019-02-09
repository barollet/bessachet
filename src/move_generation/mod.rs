pub mod init_magic;
pub mod moves;

use std::ptr;

use self::init_magic::{bishop_offset, fill_attack_table, rook_offset};

use board::{Board, HalfBoard, KING_CASTLING_RIGHTS_MASKS, QUEEN_CASTLING_RIGHTS_MASKS};

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
                                                //type SlidingAttackTable = Vec<BitBoard>;

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

fn init_magic_entries(magic_entry_init: fn(u8) -> MagicEntry) -> [MagicEntry; 64] {
    let mut attack_table: [MagicEntry; 64] = unsafe { std::mem::uninitialized() };
    for (magic_entry, square) in attack_table.iter_mut().zip(0u8..) {
        *magic_entry = magic_entry_init(square);
    }
    attack_table
}

// returns the bitboards of the pawns that can take the pawn in index (starting from LSB)
pub static EN_PASSANT_TABLE: [BitBoard; 8] = en_passant_table(); // 64 bytes 8*8 bitboards

fn sliding_attack(
    magic_entry: &MagicEntry,
    occupancy: BitBoard,
    offset_function: fn(u64, u64) -> usize,
) -> BitBoard {
    let table_pointer: *const u64 = magic_entry.table;

    let hash_key = occupancy.0 | magic_entry.black_mask;
    let table_offset = offset_function(hash_key, magic_entry.magic);

    BitBoard::new(unsafe { ptr::read(table_pointer.add(table_offset)) & magic_entry.postmask })
}

pub fn rook_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square.0)];
    sliding_attack(magic_entry, occupancy, rook_offset)
}

pub fn bishop_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square.0)];
    sliding_attack(magic_entry, occupancy, bishop_offset)
}

fn knight_attack(square: Square) -> BitBoard {
    KNIGHT_ATTACK_TABLE[square.as_index()]
}

fn king_attack(square: Square) -> BitBoard {
    KING_ATTACK_TABLE[square.as_index()]
}

// Returns the xray attack of the given square for pinned pieces
// See: https://www.chessprogramming.org/X-ray_Attacks_(Bitboards)
fn xray_attack(
    magic_entry: &MagicEntry,
    occupancy: BitBoard,
    offset_function: fn(u64, u64) -> usize,
) -> BitBoard {
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
// TODO Evaluation interface
// TODO Make the legal move generator lazier
// TODO some move ordering
// Most information fetching is lazy so this creates branching but hopefully we gain some
// computation time
#[derive(Copy, Clone)]
pub struct LegalMoveGenerator {
    // We don't use a permanent reference to a HalfBoard, we will borrow the HalfBoard each time we
    // need it
    color: Color,
    castling_rights: u8,
    en_passant_target: Option<Square>,
    move_stack: [Move; 128], // Allocated on the program stack with a bounded size
    // NOTE: The maximum size is 128 even if we can construct a position with 218 moves
    // maybe we have to change this to 218 in the future

    // Internals
    // We hold the squares that are pinned and no more than 2 pieces can be pinned on the same
    // direction, there is also the bitboard of the liberties of the pinned piece (to be
    // intersected with the actual moves of the piece)
    pinned_pieces: [(Square, BitBoard); 8],
    number_of_pinned_pieces: usize, // pinners stack indexes (basically a usize for 0 1 2)
    free_pieces: BitBoard,          // A global pin mask to quickly get if a piece is pinned or free
    // checkers
    checkers: [Square; 2],
    number_of_checkers: usize,
    // next move on the stack
    number_of_legal_moves: usize,
    // iterator index
    next_iterator_move: usize,
}

impl LegalMoveGenerator {
    // Initialize a new LegalMoveGenerator by computing pinned pieces
    // It takes a reference to the current board and the color of the player we want to move
    pub fn new(
        halfboard: &HalfBoard,
        color: Color,
        castling_rights: u8,
        en_passant_target: Option<Square>,
    ) -> Self {
        let mut generator = Self {
            color,
            castling_rights,
            en_passant_target,
            move_stack: [NULL_MOVE; 128], // Placeholders to initiatlize memory

            pinned_pieces: [(A1_SQUARE, BitBoard::empty()); 8], // Placeholders
            number_of_pinned_pieces: 0,
            free_pieces: BitBoard::full(),

            checkers: [A1_SQUARE; 2], // Placeholders
            number_of_checkers: 0,

            number_of_legal_moves: 0,
            next_iterator_move: 0,
        };

        generator.initialize(halfboard);

        generator
    }

    fn initialize(&mut self, board: &HalfBoard) {
        // we compute pinned pieces and checkers and store the result for evaluation
        self.compute_pinned_pieces(board);
        self.compute_checkers(board);

        // If there is no check we fetch the moves as usual
        if self.number_of_checkers == 0 {
            self.fetch_possible_moves(board);
        } else {
            // Otherwise we have to move the king
            self.escape_king(board);
            // If this is not a double check we can capture the checking piece or block a slider
            if self.number_of_checkers == 1 {
                let checking_square = self.checkers[0];
                let white_king_square = (board[Color::WHITE] & board[Piece::KING]).as_square();
                // Capture the checking piece
                self.capture_checker(board, checking_square);
                // Block the attack if it is a sliding one
                let checking_piece = board[self.checkers[0]].unwrap();
                if checking_piece == Piece::BISHOP
                    || checking_piece == Piece::ROOK
                    || checking_piece == Piece::QUEEN
                {
                    self.block_slider(board, checking_square, white_king_square);
                }
            }
        }

        self.order_moves();
    }
}

// Structure manipulation
impl LegalMoveGenerator {
    // Pushs the given move in the move stack
    fn push_move(&mut self, pushed_move: Move) {
        self.move_stack[self.number_of_legal_moves] = pushed_move;
        self.number_of_legal_moves += 1;
    }

    // Helper to push all the possible promotions
    fn push_promotions_from_move(&mut self, promotion_move: Move) {
        for promoted_piece in &AVAILABLE_PROMOTION {
            self.push_move(promotion_move.set_promoted_piece(*promoted_piece));
        }
    }

    // Push a new checker
    fn push_checker(&mut self, checker_square: Square) {
        self.checkers[self.number_of_checkers] = checker_square;
        self.number_of_checkers += 1;
    }

    // Helper for pieces that can perform captures and quiet moves at the same time
    // TODO remove this and change the fetching logic and add some basic ordering
    fn push_attack(&mut self, board: &HalfBoard, origin_square: Square, attack: BitBoard) {
        // Captures
        for capture_square in attack & board[Color::BLACK] {
            self.push_move(Move::tactical_move(
                origin_square,
                capture_square,
                CAPTURE_FLAG,
            ));
        }
        // Quiet moves
        for dest_square in attack & board.empty_squares() {
            self.push_move(Move::quiet_move(origin_square, dest_square));
        }
    }
}

// Logic helpers
impl LegalMoveGenerator {
    // Castling
    // the all iterator needs a mutable binding even if it doesn't modify
    fn can_castle(
        &self,
        board: &HalfBoard,
        caslting_rights: &BlackWhiteAttribute<u8>,
        empty_squares: &BlackWhiteAttribute<BitBoard>,
        check_squares: &mut BlackWhiteAttribute<BitBoard>,
    ) -> bool {
        (self.castling_rights & caslting_rights[self.color] != 0) // right to castle kingside
        && (empty_squares[self.color] & board.occupied_squares() == 0) // none of the squares on the way are occupied
        && (check_squares[self.color].all(|square| !self.is_in_check(board, square))) // squares crossed by the king are in check
    }
    fn can_king_castle(&self, board: &HalfBoard) -> bool {
        self.can_castle(
            board,
            &KING_CASTLING_RIGHTS_MASKS,
            &KING_CASTLE_EMPTY,
            &mut KING_CASTLE_CHECK,
        )
    }
    fn can_queen_castle(&self, board: &HalfBoard) -> bool {
        self.can_castle(
            board,
            &QUEEN_CASTLING_RIGHTS_MASKS,
            &QUEEN_CASTLE_EMPTY,
            &mut QUEEN_CASTLE_CHECK,
        )
    }

    // Returns if the given square is checked but only by sliding pieces
    // This faster (but not sufficiant) check is used for en passant capture
    fn is_in_sliding_check(&self, board: &HalfBoard, occupancy: BitBoard, square: Square) -> bool {
        rook_attack(square, occupancy)
            & board[Color::BLACK]
            & (board[Piece::ROOK] | board[Piece::QUEEN])
            != BitBoard::empty()
            || bishop_attack(square, occupancy)
                & board[Color::BLACK]
                & (board[Piece::BISHOP] | board[Piece::QUEEN])
                != BitBoard::empty()
    }

    // Checks that the en passant capture will not discover the king
    fn can_take_en_passant(&self, board: &HalfBoard, capturing_square: Square) -> bool {
        let captured_square = self.en_passant_target.unwrap();
        let after_en_passant_occupancy = board
            .occupied_squares()
            .remove_square(capturing_square)
            .remove_square(captured_square)
            .add_square(captured_square.forward());

        !self.is_in_sliding_check(board, after_en_passant_occupancy, board.white_king_square())
    }

    // Uses a super piece (not to rely on the other side move generator)
    // TODO use an attack map to go faster
    fn is_in_check(&self, board: &HalfBoard, square: Square) -> bool {
        // As we use a super piece for checks we can virtualy remove the king
        let occupancy_without_king = board
            .occupied_squares()
            .remove_square(board.white_king_square());

        // We can only be checked by pawns if we are below row 6 included
        let checked_by_pawn = !(ROW_8 | ROW_7).has_square(square)
            & (!FILE_A.has_square(square)
                & (board[Color::BLACK] & board[Piece::PAWN]).has_square(square.forward_left())
                || !FILE_H.has_square(square)
                    & (board[Color::BLACK] & board[Piece::PAWN])
                        .has_square(square.forward_right()));
        // Sliding pieces
        self.is_in_sliding_check(board, occupancy_without_king, square) ||
        // Knight
        knight_attack(square) & board[Color::BLACK] & board[Piece::KNIGHT] != BitBoard::empty() ||
        // Pawn
        checked_by_pawn ||
        // King
        king_attack(square) & board[Color::BLACK] & board[Piece::KING] != BitBoard::empty()
    }

    // Helpers for slider attacks
    fn push_slider_attack(
        &mut self,
        board: &HalfBoard,
        origin_square: Square,
        piece_attack: fn(Square, BitBoard) -> BitBoard,
    ) {
        self.push_pinned_slider_attack(board, origin_square, BitBoard::full(), piece_attack);
    }
    fn push_pinned_slider_attack(
        &mut self,
        board: &HalfBoard,
        origin_square: Square,
        pin_mask: BitBoard,
        piece_attack: fn(Square, BitBoard) -> BitBoard,
    ) {
        let attack = piece_attack(origin_square, board.occupied_squares()) & pin_mask;
        self.push_attack(board, origin_square, attack);
    }

    // Gets the pinned piece between the pinner and target squares
    // If this is empty, it means that this is a check and not a pin
    fn push_pinned_piece(
        &mut self,
        board: &HalfBoard,
        pinner_square: Square,
        target_square: Square,
    ) {
        // including is for overlapping with both target and pinner square
        // the target square is removed afterward
        let pin_mask = square_mask_between(pinner_square, target_square);

        // get the pinned piece
        let pinned_square = pin_mask & board[Color::WHITE];
        if pinned_square != BitBoard::empty() {
            let pinned_square = pinned_square.as_square();
            // updates the pin datastructure
            self.pinned_pieces[self.number_of_pinned_pieces] =
                (pinned_square, pin_mask.add_square(pinner_square));
            self.number_of_pinned_pieces += 1;
            self.free_pieces &= !pin_mask;
        } else if pin_mask & board.occupied_squares() == BitBoard::empty() {
            self.push_checker(pinner_square);
        }
    }

    // Helper to compute the pinned piece by a given slider type
    fn compute_pinned_pieces_for_type(
        &mut self,
        board: &HalfBoard,
        pieces: BitBoard,
        xray_attack_function: fn(Square, BitBoard) -> BitBoard,
    ) {
        let white_king_square = board.white_king_square();

        for piece_square in pieces & board[Color::BLACK] {
            let xray_attack = xray_attack_function(piece_square, board.occupied_squares());
            if xray_attack.has_square(white_king_square) {
                self.push_pinned_piece(board, piece_square, white_king_square);
            }
        }
    }
}

// Actual move generation logic
impl LegalMoveGenerator {
    fn compute_pinned_pieces(&mut self, board: &HalfBoard) {
        // Pinned by a bishop sliding
        self.compute_pinned_pieces_for_type(
            board,
            board[Piece::BISHOP] | board[Piece::QUEEN],
            bishop_xray_attack,
        );
        // Pinned by a rook sliding
        self.compute_pinned_pieces_for_type(
            board,
            board[Piece::ROOK] | board[Piece::QUEEN],
            rook_xray_attack,
        );
    }

    // Compute only checks by knights and pawns as bishop, rooks and queens are already done by
    // pinners computation
    fn compute_checkers(&mut self, board: &HalfBoard) {
        let white_king_square = board.white_king_square();
        let black_pawns = board[Color::BLACK] & board[Piece::PAWN];
        if !FILE_A.has_square(white_king_square)
            && black_pawns.has_square(white_king_square.forward_left())
        {
            self.push_checker(white_king_square.forward_left());
        }
        if !FILE_H.has_square(white_king_square)
            && black_pawns.has_square(white_king_square.forward_right())
        {
            self.push_checker(white_king_square.forward_right());
        }
        for knight_square in board[Color::BLACK] & board[Piece::KNIGHT] {
            if knight_attack(knight_square).has_square(white_king_square) {
                self.push_checker(knight_square);
            }
        }
    }
    // Make the king move in a safe place
    fn escape_king(&mut self, board: &HalfBoard) {
        let king_square = board.white_king_square();
        let mut attack = king_attack(king_square);
        for dest_square in attack {
            if self.is_in_check(board, dest_square) {
                attack = attack.remove_square(dest_square);
            }
        }
        self.push_attack(board, king_square, attack);
    }

    // Fetchs all the blocking slider moves at once
    fn block_slider(&mut self, board: &HalfBoard, checking_square: Square, target_square: Square) {
        // Capture is excluded from the blocker mask, this is handled in the capturing piece
        // routine
        let blocker_mask = square_mask_between(checking_square, target_square);

        // We check if any piece can go into the blocker mask
        // Pawns -------------------------------------------
        let pawns = board[Color::WHITE] & board[Piece::PAWN];
        let simple_pawns = pawns & !ROW_8 & self.free_pieces;
        // Simple push
        for pushed_pawn in (simple_pawns << 8) & blocker_mask {
            self.push_move(Move::quiet_move(pushed_pawn.behind(), pushed_pawn));
        }
        // Double push
        let double_pushed_pawns = pawns & ROW_2 & self.free_pieces;
        let double_pushed_pawns = (double_pushed_pawns << 8) & board.empty_squares();
        // if it can block the checking sliders, then the destination square is empty
        let double_pushed_pawns = double_pushed_pawns << 8 & blocker_mask;
        for double_pushed_pawn in double_pushed_pawns {
            self.push_move(Move::double_pawn_push_to(double_pushed_pawn));
        }
        // En passant
        for en_passant_capture in
            pawns & board.en_passant_capture_start_squares() & self.free_pieces
        {
            let dest_square = board.en_passant.unwrap().forward();
            if blocker_mask.has_square(dest_square) {
                self.push_move(Move::tactical_move(
                    en_passant_capture,
                    dest_square,
                    EN_PASSANT_CAPTURE_FLAG,
                ));
            }
        }
        // Pawns can promote while blocking only on an horizontal check on the last row
        if ROW_8.has_squares(blocker_mask) {
            let promoting_pawns = blocker_mask >> 8 & pawns & self.free_pieces;
            for pawn in promoting_pawns {
                self.push_promotions_from_move(Move::tactical_move(
                    pawn,
                    pawn.forward(),
                    PROMOTION_FLAG,
                ));
            }
        }

        // Other pieces -------------------------------------
        // Knight
        for knight_square in board[Color::WHITE] & board[Piece::KNIGHT] & self.free_pieces {
            let blocking_squares = knight_attack(knight_square) & blocker_mask;
            for blocking_square in blocking_squares {
                self.push_move(Move::quiet_move(knight_square, blocking_square));
            }
        }
        // Bishop or queen
        for bishop_square in
            board[Color::WHITE] & (board[Piece::BISHOP] | board[Piece::QUEEN]) & self.free_pieces
        {
            let blocking_squares =
                bishop_attack(bishop_square, board.occupied_squares()) & blocker_mask;
            for blocking_square in blocking_squares {
                self.push_move(Move::quiet_move(bishop_square, blocking_square));
            }
        }
        // Rook or queen
        for rook_square in
            board[Color::WHITE] & (board[Piece::ROOK] | board[Piece::QUEEN]) & self.free_pieces
        {
            let blocking_squares =
                rook_attack(rook_square, board.occupied_squares()) & blocker_mask;
            for blocking_square in blocking_squares {
                self.push_move(Move::quiet_move(rook_square, blocking_square));
            }
        }
    }

    // Pushes the moves that capture the given square
    // King captures are handled in the king escape
    fn capture_checker(&mut self, board: &HalfBoard, captured_square: Square) {
        // Rook
        let mut can_capture = rook_attack(captured_square, board.occupied_squares())
            & board[Color::WHITE]
            & (board[Piece::ROOK] | board[Piece::QUEEN]);
        // Bishop
        can_capture |= bishop_attack(captured_square, board.occupied_squares())
            & board[Color::WHITE]
            & (board[Piece::BISHOP] | board[Piece::QUEEN]);
        // Knight
        can_capture |= knight_attack(captured_square) & board[Color::WHITE] & board[Piece::KNIGHT];
        // Pawn simple capture or promotion
        if !(ROW_1 | ROW_2).has_square(captured_square) {
            let mut pawn_simple_captures = BitBoard::empty();
            if !FILE_A.has_square(captured_square) {
                pawn_simple_captures |= captured_square.behind_left().as_bitboard()
                    & board[Color::WHITE]
                    & board[Piece::PAWN]
            }
            if !FILE_H.has_square(captured_square) {
                pawn_simple_captures |= captured_square.behind_right().as_bitboard()
                    & board[Color::WHITE]
                    & board[Piece::PAWN]
            }
            if !ROW_8.has_square(captured_square) {
                can_capture |= pawn_simple_captures;
            } else {
                // Promotion
                let pawn_promotions = pawn_simple_captures.remove_squares(!self.free_pieces);
                for capturing_square in pawn_promotions {
                    self.push_promotions_from_move(Move::tactical_move(
                        capturing_square,
                        captured_square,
                        CAPTURE_FLAG | PROMOTION_FLAG,
                    ));
                }
            }
        }

        // En passant capture
        if self.en_passant_target.is_some() && self.en_passant_target.unwrap() == captured_square {
            for capturing_square in
                board.en_passant_capture_start_squares() & board[Color::WHITE] & board[Piece::PAWN]
            {
                if board[captured_square.forward()].is_none()
                    & self.can_take_en_passant(board, capturing_square)
                {
                    self.push_move(Move::tactical_move(
                        capturing_square,
                        captured_square.forward(),
                        EN_PASSANT_CAPTURE_FLAG,
                    ));
                }
            }
        }

        can_capture = can_capture.remove_squares(!self.free_pieces);

        // Push the basic moves
        for capturing_square in can_capture {
            self.push_move(Move::tactical_move(
                capturing_square,
                captured_square,
                CAPTURE_FLAG,
            ))
        }
    }

    // Move fetching when there is no checks
    // TODO fetch only captures first and then quiet moves to make it lazier
    // ------------------------------------------------
    fn fetch_possible_moves(&mut self, board: &HalfBoard) {
        let free_pieces = board[Color::WHITE] & self.free_pieces;
        // Simple pawn push ------------------------
        let pawns = board[Piece::PAWN] & free_pieces;
        let pushed_pawns = (pawns << 8) & board.empty_squares();

        // No promotion
        for dest_square in pushed_pawns & !ROW_8 {
            self.push_move(Move::quiet_move(dest_square.behind(), dest_square));
        }
        // Promotion
        for dest_square in pushed_pawns & ROW_8 {
            self.push_promotions_from_move(Move::tactical_move(
                dest_square.behind(),
                dest_square,
                PROMOTION_FLAG,
            ));
        }
        // -----------------------------------------

        // Double push, sets en passant flag -------
        let starting_pawns = board[Piece::PAWN] & free_pieces & ROW_2;

        // To be double pushed, the pawns have to be able to move once forward
        let simple_pushed_pawns = (starting_pawns << 8) & board.empty_squares();
        // The pawns that can both be pushed for one and two lines forward
        let double_pushed_pawns = (simple_pushed_pawns << 8) & board.empty_squares();

        for dest_square in double_pushed_pawns {
            self.push_move(Move::double_pawn_push_to(dest_square));
        }
        // -----------------------------------------

        // Pawn captures ---------------------------
        let left_capture_moves = (pawns & !FILE_A) << 9 & board[Color::BLACK];
        let right_capture_moves = (pawns & !FILE_H) << 7 & board[Color::BLACK];
        // Capture without promotions
        for capture_square in left_capture_moves & !ROW_8 {
            self.push_move(Move::tactical_move(
                capture_square.behind_right(),
                capture_square,
                CAPTURE_FLAG,
            ));
        }
        for capture_square in right_capture_moves & !ROW_8 {
            self.push_move(Move::tactical_move(
                capture_square.behind_left(),
                capture_square,
                CAPTURE_FLAG,
            ));
        }
        // Capture with promotion
        for capture_square in left_capture_moves & ROW_8 {
            self.push_promotions_from_move(Move::tactical_move(
                capture_square.behind_right(),
                capture_square,
                CAPTURE_FLAG | PROMOTION_FLAG,
            ));
        }
        for capture_square in right_capture_moves & ROW_8 {
            self.push_promotions_from_move(Move::tactical_move(
                capture_square.behind_left(),
                capture_square,
                CAPTURE_FLAG | PROMOTION_FLAG,
            ));
        }
        // -----------------------------------------

        // En passant capture ----------------------
        for pawn_origin_square in board.en_passant_capture_start_squares() & pawns {
            if board[board.en_passant.unwrap().forward()].is_none()
                && self.can_take_en_passant(board, pawn_origin_square)
            {
                self.push_move(Move::tactical_move(
                    pawn_origin_square,
                    board.en_passant.unwrap().forward(),
                    EN_PASSANT_CAPTURE_FLAG,
                ));
            }
        }
        // -----------------------------------------

        // Knights moves ---------------------------
        for knight_square in board[Piece::KNIGHT] & free_pieces {
            let attack = knight_attack(knight_square);
            self.push_attack(board, knight_square, attack);
        }
        // -----------------------------------------

        // Bishop moves ----------------------------
        for bishop_square in board[Piece::BISHOP] & free_pieces {
            self.push_slider_attack(board, bishop_square, bishop_attack);
        }
        // -----------------------------------------

        // Rook moves ------------------------------
        for rook_square in board[Piece::ROOK] & free_pieces {
            self.push_slider_attack(board, rook_square, rook_attack);
        }
        // -----------------------------------------

        // Queen moves -----------------------------
        for queen_square in board[Piece::QUEEN] & free_pieces {
            self.push_slider_attack(board, queen_square, bishop_attack);
            self.push_slider_attack(board, queen_square, rook_attack);
        }
        // -----------------------------------------

        // King moves ------------------------------
        // Moves
        self.escape_king(board);
        // Castle
        let color = self.color;
        if self.can_king_castle(board) {
            self.push_move(KING_CASTLE_MOVES[color]);
        }
        if self.can_queen_castle(board) {
            self.push_move(QUEEN_CASTLE_MOVES[color])
        }
        // -----------------------------------------

        // Moves of pinned pieces ------------------
        for i in 0..self.number_of_pinned_pieces {
            let (pinned_square, pin_mask) = self.pinned_pieces[i];
            let pinned_piece = board[pinned_square].unwrap();
            match pinned_piece {
                Piece::PAWN => {
                    // NOTE pinned pawns can never promote
                    // Simple push
                    let forward_square = pinned_square.forward();
                    if board[forward_square].is_none() && pin_mask.has_square(forward_square) {
                        self.push_move(Move::quiet_move(pinned_square, forward_square));
                        // Double push
                        if ROW_2.has_square(pinned_square) {
                            let double_push_square = forward_square.forward();
                            // Here we don't have to check the pin mask
                            if board[double_push_square].is_none() {
                                self.push_move(Move::double_pawn_push(
                                    pinned_square,
                                    double_push_square,
                                ));
                            }
                        }
                    }
                    // Left capture
                    let capture_left_square = pinned_square.forward_left();
                    if board[Color::BLACK].has_square(capture_left_square)
                        && pin_mask.has_square(capture_left_square)
                    {
                        self.push_move(Move::tactical_move(
                            pinned_square,
                            capture_left_square,
                            CAPTURE_FLAG,
                        ));
                    }
                    // Right capture
                    let capture_right_square = pinned_square.forward_right();
                    if board[Color::BLACK].has_square(capture_right_square)
                        && pin_mask.has_square(capture_right_square)
                    {
                        self.push_move(Move::tactical_move(
                            pinned_square,
                            capture_right_square,
                            CAPTURE_FLAG,
                        ));
                    }
                    // En passant
                    if board
                        .en_passant_capture_start_squares()
                        .has_square(pinned_square)
                    {
                        let en_passant_dest_square = board.en_passant.unwrap().forward();
                        if pin_mask.has_square(en_passant_dest_square) {
                            self.push_move(Move::tactical_move(
                                pinned_square,
                                en_passant_dest_square,
                                EN_PASSANT_CAPTURE_FLAG,
                            ));
                        }
                    }
                }
                Piece::BISHOP => {
                    self.push_pinned_slider_attack(board, pinned_square, pin_mask, bishop_attack)
                }
                Piece::ROOK => {
                    self.push_pinned_slider_attack(board, pinned_square, pin_mask, rook_attack)
                }
                Piece::QUEEN => {
                    self.push_pinned_slider_attack(board, pinned_square, pin_mask, bishop_attack);
                    self.push_pinned_slider_attack(board, pinned_square, pin_mask, rook_attack);
                }
                Piece::KNIGHT => (), // Knights cannot move if pinned
                Piece::KING => panic!("King shouldn't be pinned"),
            }
        }
        // -----------------------------------------
    }

    // Performs move ordering
    // Here the captures are set first
    fn order_moves(&mut self) {
        let mut last_capture_index = 0;

        for move_index in 0..self.number_of_legal_moves {
            if self.move_stack[move_index].has_flags(CAPTURE_FLAG) {
                if move_index != last_capture_index {
                    // push the move to the end of the capture moves
                    let capture_move = self.move_stack[move_index];
                    self.move_stack[move_index] = self.move_stack[last_capture_index];
                    self.move_stack[last_capture_index] = capture_move;
                }
                last_capture_index += 1;
            }
        }
    }

    pub fn is_king_checked(&self) -> bool {
        self.number_of_checkers != 0
    }

    // Returns an attack map of the given position with White playing
    pub fn attack_map(&self, board: &HalfBoard) -> BitBoard {
        let mut attack_map = BitBoard::empty();
        // Pawns
        attack_map |= (board[Piece::PAWN] & board[Color::WHITE] & !FILE_A) << 9;
        attack_map |= (board[Piece::PAWN] & board[Color::WHITE] & !FILE_H) << 7;
        // Knights
        for knight_square in board[Piece::KNIGHT] & board[Color::WHITE] {
            attack_map |= knight_attack(knight_square);
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
        attack_map |= king_attack((board[Piece::KING] & board[Color::WHITE]).as_square());

        attack_map
    }

    pub fn capture_iterator(&mut self) -> CaptureIterator {
        CaptureIterator::new(self)
    }
}

// The iterator function is straightforward and assumes that the moves have been sorted before
impl Iterator for LegalMoveGenerator {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next_iterator_move < self.number_of_legal_moves {
            let iter_move = self.move_stack[self.next_iterator_move];
            self.next_iterator_move += 1;

            // Decorate the move and returns it
            //Some(self.decorator.decorate_move(iter_move))
            Some(iter_move)
        } else {
            None
        }
    }
}

// An helper structure to iterate over only the capture moves
pub struct CaptureIterator<'a> {
    move_generator: &'a mut LegalMoveGenerator,
}

impl<'a> CaptureIterator<'a> {
    fn new(move_generator: &mut LegalMoveGenerator) -> CaptureIterator {
        CaptureIterator { move_generator }
    }
}

impl<'a> Iterator for CaptureIterator<'a> {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        match self.move_generator.next() {
            Some(next_move) => {
                if Move::from(next_move).has_flags(CAPTURE_FLAG) {
                    Some(next_move)
                } else {
                    None
                }
            }
            None => None,
        }
    }
}

/* Debugging interface */
impl Board {
    #[allow(dead_code)]
    // TODO redo interface to for example e2e3 instead of e 2 e 3
    pub fn play_move(&mut self, mov: &str) {
        let chars: Vec<_> = mov.chars().collect();
        assert_eq!(chars.len(), 4);
        let origin_file = chars[0];
        let origin_row = chars[1];
        let dest_file = chars[2];
        let dest_row = chars[3];
        let mut origin_square = Square::from_char_file_rank(origin_file, origin_row);
        let mut dest_square = Square::from_char_file_rank(dest_file, dest_row);
        if self.side_to_move == Color::BLACK {
            origin_square = origin_square.transpose();
            dest_square = dest_square.transpose();
        }
        let generator = self.create_legal_move_generator();
        for ext_mov in generator {
            let mov = Move::from(ext_mov);
            if mov.origin_square() == origin_square && mov.destination_square() == dest_square {
                self.make(ext_mov);
                return;
            }
        }
        panic!(
            "Can't find move {}{}{}{}",
            origin_file, origin_row, dest_file, dest_row
        );
    }

    #[allow(dead_code)]
    pub fn print_possible_moves(&self) {
        let generator = self.create_legal_move_generator();
        for mov in generator {
            if self.side_to_move == Color::BLACK {
                println!("{}", Move::from(mov).transpose());
            } else {
                println!("{}", Move::from(mov));
            }
        }
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
        let (rank, file) = Square(sq).rank_file();
        let (rank, file) = (rank as i8, file as i8);

        for (i, j) in &knight_moves {
            if file + i >= 0 && file + i < 8 && rank + j >= 0 && rank + j < 8 {
                *attack_bitboard |=
                    Square::from_file_rank((file + i) as u8, (rank + j) as u8).as_bitboard();
                *attack_bitboard |=
                    Square::from_file_rank((file + i) as u8, (rank + j) as u8).as_bitboard();
            }
        }
    }

    knight_attacks
}

fn generate_king_attacks() -> [BitBoard; 64] {
    let mut king_attacks = [BitBoard::empty(); 64];

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
        let (rank, file) = Square(sq).rank_file();
        let (rank, file) = (rank as i8, file as i8);

        for (i, j) in &king_moves {
            if file + i >= 0 && file + i < 8 && rank + j >= 0 && rank + j < 8 {
                *attack_bitboard |=
                    Square::from_file_rank((file + i) as u8, (rank + j) as u8).as_bitboard();
            }
        }
    }

    king_attacks
}
