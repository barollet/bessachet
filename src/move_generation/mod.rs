pub mod init_magic;
pub mod moves;

use std::ptr;

use self::init_magic::*;
pub use self::moves::*;
use board::prelude::*;
use std::convert::From;
use utils::*;

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
    table: &'static BitBoard, // static reference coerced to a pointer
    black_mask: BitBoard,
    postmask: BitBoard,
}

type SlidingAttackTable = Vec<BitBoard>; // Heap allocated table
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

const EN_PASSANT_LINE: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(ROW_5, ROW_4);
const STARTING_ROW: BlackWhiteAttribute<BitBoard> = BlackWhiteAttribute::new(ROW_7, ROW_2);

fn sliding_attack(
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

fn knight_attack(square: Square) -> BitBoard {
    KNIGHT_ATTACK_TABLE[square as usize]
}

fn king_attack(square: Square) -> BitBoard {
    KING_ATTACK_TABLE[square as usize]
}

fn pawn_attack(square: Square, color: Color) -> BitBoard {
    PAWN_ATTACK_TABLE[color][square as usize]
}

// Returns the xray attack of the given square for pinned pieces
// See: https://www.chessprogramming.org/X-ray_Attacks_(Bitboards)
fn xray_attack(
    magic_entry: &MagicEntry,
    occupancy: BitBoard,
    offset_function: fn(BitBoard, u64) -> usize,
) -> BitBoard {
    let attack = sliding_attack(magic_entry, occupancy, offset_function);
    let occupancy = occupancy & !attack;
    sliding_attack(magic_entry, occupancy, offset_function)
}

type XrayFunction = fn(Square, BitBoard) -> BitBoard;

fn rook_xray_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &ROOK_ATTACK_TABLE[usize::from(square)];
    xray_attack(magic_entry, occupancy, rook_offset)
}

fn bishop_xray_attack(square: Square, occupancy: BitBoard) -> BitBoard {
    let magic_entry = &BISHOP_ATTACK_TABLE[usize::from(square)];
    xray_attack(magic_entry, occupancy, bishop_offset)
}

#[derive(Clone)]
pub struct MoveGenHelper {
    // We hold the squares that are pinned and no more than 2 pieces can be pinned on the same
    // direction, there is also the bitboard of the liberties of the pinned piece (to be
    // intersected with the actual moves of the piece)
    pinned_pieces: [(Square, BitBoard); 8],
    number_of_pinned_pieces: usize, // pinners stack indexes

    free_pieces: BitBoard, // A global pin mask to quickly get if a piece is pinned or free

    // checkers
    checkers: [Square; 2],
    number_of_checkers: usize,
}

impl<'a> AuxiliaryStruct<'a> for MoveGenHelper {
    type Source = &'a Position;
    fn initialize(position: Self::Source) -> Self {
        let mut mgh = MoveGenHelper {
            pinned_pieces: [(0, 0); 8],
            number_of_pinned_pieces: 0,
            free_pieces: BBWraper::full(),
            checkers: [0; 2],
            number_of_checkers: 0,
        };

        // Pinned pieces and checks by sliders
        mgh.compute_pinned_pieces(&position, Piece::BISHOP, bishop_xray_attack);
        mgh.compute_pinned_pieces(&position, Piece::ROOK, rook_xray_attack);

        mgh.compute_pawn_knight_checkers(&position);

        mgh
    }
}

struct PinnedPiecesIterator<'a> {
    move_gen: &'a MoveGenHelper,
    iteration_index: usize,
}

// This iterates over the pinned pieces
impl<'a> Iterator for PinnedPiecesIterator<'a> {
    type Item = (Square, BitBoard);
    fn next(&mut self) -> Option<Self::Item> {
        if self.iteration_index < self.move_gen.number_of_pinned_pieces {
            let item = self.move_gen.pinned_pieces[self.iteration_index];
            self.iteration_index += 1;
            Some(item)
        } else {
            None
        }
    }
}

// Structure manipulation
impl MoveGenHelper {
    // Push a new pinned piece with its liberties
    fn push_pinned(&mut self, pinned_square: Square, liberties: BitBoard) {
        self.pinned_pieces[self.number_of_pinned_pieces] = (pinned_square, liberties);
        self.number_of_pinned_pieces += 1;
        self.free_pieces = self.free_pieces.remove_square(pinned_square);
    }
    // Push a new checker
    fn push_checker(&mut self, checker_square: Square) {
        self.checkers[self.number_of_checkers] = checker_square;
        self.number_of_checkers += 1;
    }
}

impl MoveGenHelper {
    // Gets the pinned piece between the pinner and target squares
    // If this is empty, it means that this is a check and not a pin
    fn decide_pin_check(&mut self, position: &Position, pinner: Square, target: Square) {
        // including is for overlapping with both target and pinner square
        // the target square is removed afterward
        let pin_liberties: BitBoard = square_mask_between(pinner, target);

        // get the pinned piece should be reduced to a single square or empty
        let pinned: BitBoard = pin_liberties & position[position.side_to_move];
        if pinned != BBWraper::empty() {
            let pinned = Square::from(BBWraper(pinned));
            self.push_pinned(pinned, pin_liberties.add_square(pinner));
        } else if pin_liberties & position.occupied_squares() == BBWraper::empty() {
            self.push_checker(pinner);
        }
    }

    fn compute_pinned_pieces(&mut self, pos: &Position, piece: Piece, xray_function: XrayFunction) {
        let king_square = pos.king_square(pos.side_to_move);
        let opponent_color = pos.side_to_move.transpose();

        for piece_square in BBWraper((pos[piece] | pos[Piece::QUEEN]) & pos[opponent_color]) {
            let xray_attack = xray_function(piece_square, pos.occupied_squares());
            if xray_attack.has_square(king_square) {
                self.decide_pin_check(pos, piece_square, king_square);
            }
        }
    }

    // Compute only checks by knights and pawns as bishop, rooks and queens are already done by
    // pinners computation
    fn compute_pawn_knight_checkers(&mut self, pos: &Position) {
        let opponent_color = pos.side_to_move.transpose();

        let king_square = pos.king_square(pos.side_to_move);

        let opponent_pawns = pos[opponent_color] & pos[Piece::PAWN];
        for pawn_square in BBWraper(pawn_attack(king_square, pos.side_to_move) & opponent_pawns) {
            self.push_checker(pawn_square)
        }
        let opponent_knights = pos[opponent_color] & pos[Piece::KNIGHT];
        for knight_square in BBWraper(knight_attack(king_square) & opponent_knights) {
            self.push_checker(knight_square);
        }
    }

    fn pinned_pieces_iterator(&self) -> PinnedPiecesIterator {
        PinnedPiecesIterator {
            move_gen: &self,
            iteration_index: 0,
        }
    }
}

// A pseudo legal move list, illegal moves from absolutely pinned pieces are already removed
// This does a legality check for en passant capture and king moves
pub struct PseudoLegalGenerator<'a> {
    board: &'a Board,
    // NOTE: The maximum size is 128 even if we can construct a position with 218 moves
    // maybe we have to change this to 218 or a dynamically sized struct
    moves_list: [Move; 128],
    number_of_moves: usize,
}

pub struct PseudoLegalMoveList {
    moves_list: [Move; 128],
    iterator_move: usize,
    number_of_moves: usize,
}

impl<'a> From<&mut PseudoLegalGenerator<'a>> for PseudoLegalMoveList {
    fn from(generator: &mut PseudoLegalGenerator) -> PseudoLegalMoveList {
        PseudoLegalMoveList {
            moves_list: generator.moves_list,
            number_of_moves: generator.number_of_moves,
            iterator_move: 0,
        }
    }
}

impl Iterator for PseudoLegalMoveList {
    type Item = Move;

    fn next(&mut self) -> Option<Move> {
        if self.iterator_move < self.number_of_moves {
            let output = self.moves_list[self.iterator_move];
            self.iterator_move += 1;

            Some(output)
        } else {
            None
        }
    }
}

pub struct LegalMoveList<'a> {
    board: &'a mut Board,
    move_gen: PseudoLegalMoveList,
}

impl<'a> LegalMoveList<'a> {
    fn new(board: &'a mut Board) -> LegalMoveList<'a> {
        LegalMoveList {
            move_gen: board.generate_pseudo_legal_moves(),
            board,
        }
    }
}

impl<'a> Iterator for LegalMoveList<'a> {
    type Item = Move;
    fn next(&mut self) -> Option<Self::Item> {
        self.move_gen.next().map_or(None, |mov| {
            let is_king_mov = self.board[mov.origin_square()].unwrap() == Piece::KING;
            // If the move can't be illegal
            if !mov.has_exact_flags(EN_PASSANT_CAPTURE_FLAG) && !is_king_mov {
                Some(mov)
            } else {
                // Otherwise we do a legality check and we unmake the move
                // NOTE we cannot use map or or map_or_else
                match self.board.legality_check(mov) {
                    Some(ext_mov) => {
                        self.board.unmake(ext_mov);
                        Some(mov)
                    }
                    None => self.next(),
                }
            }
        })
    }
}

pub struct LegalMoveMakerWithWork<'a, F, G>
where
    F: FnMut(&mut Board) -> G,
{
    board: &'a mut Board,
    move_gen: PseudoLegalMoveList,
    intermediate_work: F,
}

impl<'a, F, G> LegalMoveMakerWithWork<'a, F, G>
where
    F: FnMut(&mut Board) -> G,
{
    pub fn new(board: &'a mut Board, intermediate_work: F) -> LegalMoveMakerWithWork<'a, F, G> {
        LegalMoveMakerWithWork {
            move_gen: board.generate_pseudo_legal_moves(),
            board,
            intermediate_work,
        }
    }
}

impl<'a, F, G> LegalMoveMakerWithWork<'a, F, G>
where
    F: FnMut(&mut Board) -> G,
{
    fn work_and_unmake(&mut self, ext_mov: ExtendedMove) -> G {
        let intermediate_work = &mut self.intermediate_work;
        let score = intermediate_work(self.board);

        self.board.unmake(ext_mov);

        score
    }
}

impl<'a, F, G> Iterator for LegalMoveMakerWithWork<'a, F, G>
where
    F: FnMut(&mut Board) -> G,
{
    type Item = (Move, G);
    fn next(&mut self) -> Option<Self::Item> {
        self.move_gen.next().map_or(None, |mov| {
            // If the move can be illegal we test it and cancel it if it is indeed illegal
            let is_king_mov = self.board[mov.origin_square()].unwrap() == Piece::KING;
            if !mov.has_exact_flags(EN_PASSANT_CAPTURE_FLAG) && !is_king_mov {
                let ext_mov = self.board.make(mov);

                Some((mov, self.work_and_unmake(ext_mov)))
            } else {
                match self.board.legality_check(mov) {
                    Some(ext_mov) => Some((mov, self.work_and_unmake(ext_mov))),
                    None => self.next(),
                }
            }
        })
    }
}

// Helpers
impl Board {
    // Make a move and check if it is legal, if not unmake the move and returns None
    pub fn legality_check(&mut self, mov: Move) -> Option<ExtendedMove> {
        // Make the move
        let player_color = self.position.side_to_move;
        let ext_mov = self.make(mov);
        // Test legality
        let king_square = self.position.king_square(player_color);
        if self.is_in_check(king_square, player_color) {
            // Is in check, not legal
            self.unmake(ext_mov);
            None
        } else {
            // Legal
            Some(ext_mov)
        }
    }

    // A piece of the given color on the given square would be in check
    fn is_in_check(&self, square: Square, color: Color) -> bool {
        // We use the super piece method
        let opponent_color = color.transpose();
        let opponent_pieces = self.position[opponent_color];

        let occupancy = self.position.occupied_squares();

        // The pawn attack is indeed empty if we cannot be checked by pawns
        pawn_attack(square, color)
            .intersects(opponent_pieces & self.position[Piece::PAWN])
            || knight_attack(square).intersects(opponent_pieces & self.position[Piece::KNIGHT])
            || bishop_attack(square, occupancy).intersects(
                opponent_pieces & (self.position[Piece::BISHOP] | self.position[Piece::QUEEN]),
            )
            || rook_attack(square, occupancy).intersects(
                opponent_pieces & (self.position[Piece::ROOK] | self.position[Piece::QUEEN]),
            )
            // This can be avoided for en passant capture
            || king_attack(square).intersects(opponent_pieces & self.position[Piece::KING])
    }

    pub fn is_king_checked(&self) -> bool {
        let player_color = self.position.side_to_move;
        let king_square = self.position.king_square(player_color);
        self.is_in_check(king_square, player_color.transpose())
    }
}

// Move generation functions
impl Board {
    pub fn generate_pseudo_legal_moves(&mut self) -> PseudoLegalMoveList {
        // We update the checks and pins on demand
        self.move_gen = MoveGenHelper::initialize(&self.position);

        let mut pseudo_legal_mov_gen = PseudoLegalGenerator::new(&self);

        // If not in check, we fetch the move as usual
        if self.move_gen.number_of_checkers == 0 {
            pseudo_legal_mov_gen.all_moves()
        } else if self.move_gen.number_of_checkers == 1 {
            // If in simple check we can block the slider, capture the checker or escape the king
            pseudo_legal_mov_gen
                .capture_and_block_checker()
                .escape_king()
        } else {
            // If in double check we can only escape the king
            debug_assert_eq!(self.move_gen.number_of_checkers, 2);
            pseudo_legal_mov_gen.escape_king()
        }
    }

    // Need a mutable reference to test pseudo legal moves
    fn number_of_legal_moves(&mut self) -> usize {
        LegalMoveList::new(self).count()
    }
}

impl<'a> PseudoLegalGenerator<'a> {
    fn new(board: &Board) -> PseudoLegalGenerator {
        PseudoLegalGenerator {
            board,
            moves_list: [NULL_MOVE; 128],
            number_of_moves: 0,
        }
    }

    fn escape_king(&mut self) -> PseudoLegalMoveList {
        let player_color = self.board.position.side_to_move;
        let king_square = self.board.position.king_square(player_color);
        self.push_attack(king_square, king_attack(king_square));

        PseudoLegalMoveList::from(self)
    }

    fn capture_and_block_checker(&mut self) -> &mut PseudoLegalGenerator<'a> {
        let checker_square = self.board.move_gen.checkers[0];
        let checker_piece = self.board[checker_square].unwrap();

        let mut target_mask: BitBoard = BitBoard::from(SqWrapper(checker_square));
        // If the piece is a slider, try to block it, otherwise skip
        if checker_piece == Piece::BISHOP
            || checker_piece == Piece::ROOK
            || checker_piece == Piece::QUEEN
        {
            let player_color = self.board.position.side_to_move;
            let king_square = self.board.position.king_square(player_color);
            target_mask |= square_mask_between(checker_square, king_square);
        }

        self.all_moves_with_target(target_mask)
    }

    fn all_moves(&mut self) -> PseudoLegalMoveList {
        self.all_moves_with_target(BBWraper::full()).escape_king()
    }

    // Generate only moves that ends in the target mask
    fn all_moves_with_target(&mut self, target_mask: BitBoard) -> &mut PseudoLegalGenerator<'a> {
        let player_color = self.board.position.side_to_move;
        let player_pieces = self.board.position[player_color];
        let opponent_pieces = self.board.position[player_color.transpose()];

        let free_pieces = self.board.move_gen.free_pieces;
        let empty_squares = self.board.position.empty_squares();

        // Pawns
        let pawns = self.board.position[Piece::PAWN] & player_pieces & free_pieces;
        // Push

        let simple_pushed_pawns = pawns.push(player_color) & empty_squares & target_mask;
        let origin_pawns = simple_pushed_pawns.push(player_color.transpose()); // Push back
        self.push_pawn_attack(origin_pawns, simple_pushed_pawns, NO_FLAG, player_color);

        // Capture left
        let captures_dest =
            (pawns & !FILE_A).left_capture(player_color) & opponent_pieces & target_mask;
        let captures_origin = captures_dest.right_capture(player_color.transpose());
        self.push_pawn_attack(captures_origin, captures_dest, CAPTURE_FLAG, player_color);
        // Capture right
        let captures_dest =
            (pawns & !FILE_H).right_capture(player_color) & opponent_pieces & target_mask;
        let captures_origin = captures_dest.left_capture(player_color.transpose());
        self.push_pawn_attack(captures_origin, captures_dest, CAPTURE_FLAG, player_color);
        // Double push
        let double_pushed = (pawns.push(player_color) & empty_squares).push(player_color)
            & empty_squares
            & EN_PASSANT_LINE[player_color]
            & target_mask;
        let origin_pawns = double_pushed
            .push(player_color.transpose())
            .push(player_color.transpose());
        self.push_pawn_attack(origin_pawns, double_pushed, DOUBLE_PUSH_FLAG, player_color);
        // En passant (we don't need the target mask as en passant capture will be checked
        // afterward anyway)
        for pawn in BBWraper(self.board.position.en_passant_candidates() & pawns) {
            let en_passant_square = self.board.position.en_passant.unwrap();
            let en_passant_dest = en_passant_square.get().forward(player_color);
            if self.board[en_passant_dest].is_none() {
                self.push_move(pawn, en_passant_dest, EN_PASSANT_CAPTURE_FLAG);
            }
        }

        // Knights
        let knights = player_pieces & self.board.position[Piece::KNIGHT] & free_pieces;
        for knight in BBWraper(knights) {
            self.push_attack(knight, knight_attack(knight) & target_mask);
        }

        let occupied_squares = self.board.position.occupied_squares();
        macro_rules! sliding_attack {
            ($piece: expr, $attack_function: ident) => {
                let pieces = player_pieces
                    & (self.board.position[$piece] | self.board.position[Piece::QUEEN])
                    & self.board.move_gen.free_pieces;
                for piece in BBWraper(pieces) {
                    self.push_attack(
                        piece,
                        $attack_function(piece, occupied_squares) & target_mask,
                    );
                }
            };
        }
        // Bishop-like
        sliding_attack!(Piece::BISHOP, bishop_attack);

        // Rook-like
        sliding_attack!(Piece::ROOK, rook_attack);

        // Pinned pieces
        let pinned_pieces = self.board.move_gen.pinned_pieces_iterator();
        for (square, liberties) in pinned_pieces {
            let occupied_squares = self.board.position.occupied_squares();
            match self.board[square].unwrap() {
                // pinned pawn can push, double push, capture, capture en passant but not promote
                Piece::PAWN => {
                    // Push
                    let simple_push_dest = square.forward(player_color);
                    if liberties.has_square(simple_push_dest)
                        && target_mask.has_square(simple_push_dest)
                        && self.board[simple_push_dest].is_none()
                    {
                        self.push_move(square, simple_push_dest, NO_FLAG);
                    }
                    // Double push
                    if STARTING_ROW[player_color].has_square(square) {
                        let double_push_dest = simple_push_dest.forward(player_color);
                        if liberties.has_square(double_push_dest)
                            && target_mask.has_square(double_push_dest)
                            && self.board[simple_push_dest].is_none()
                            && self.board[double_push_dest].is_none()
                        {
                            self.push_move(square, double_push_dest, DOUBLE_PUSH_FLAG);
                        }
                    }
                    // Capture
                    for dest in BBWraper(
                        pawn_attack(square, player_color)
                            & opponent_pieces
                            & liberties
                            & target_mask,
                    ) {
                        self.push_move(square, dest, CAPTURE_FLAG);
                    }
                    // En passant capture
                    if let Some(en_passant_square) = self.board.position.en_passant {
                        let en_passant_square = en_passant_square.get();
                        let en_passant_dest = en_passant_square.forward(player_color);
                        // legality will be checked anyway
                        if self
                            .board
                            .position
                            .en_passant_candidates()
                            .has_square(square)
                            && liberties.has_square(en_passant_dest)
                        {
                            self.push_move(square, en_passant_dest, EN_PASSANT_CAPTURE_FLAG);
                        }
                    }
                }
                Piece::KNIGHT => (), // Knights cannot move if pinned
                Piece::BISHOP => self.push_attack(
                    square,
                    bishop_attack(square, occupied_squares) & target_mask & liberties,
                ),
                Piece::ROOK => self.push_attack(
                    square,
                    rook_attack(square, occupied_squares) & target_mask & liberties,
                ),
                Piece::QUEEN => self.push_attack(
                    square,
                    (bishop_attack(square, occupied_squares)
                        | rook_attack(square, occupied_squares))
                        & target_mask
                        & liberties,
                ),
                Piece::KING => panic!("King shouldn't be pinned"),
            }
        }

        // King
        // We generate king moves and castling if we are not engaged in check
        if target_mask != BBWraper::empty() {
            self.castling()
        } else {
            self
        }
    }

    fn castling(&mut self) -> &mut PseudoLegalGenerator<'a> {
        let player_color = self.board.position.side_to_move;
        // King side
        if self.can_castle(player_color, CastlingSide::KING) {
            self.push_move_internal(KING_CASTLE_MOVES[player_color]);
        }
        // Queen side
        if self.can_castle(player_color, CastlingSide::QUEEN) {
            self.push_move_internal(QUEEN_CASTLE_MOVES[player_color]);
        }
        self
    }

    // Castling, color is the player color
    fn can_castle(&self, color: Color, side: CastlingSide) -> bool {
        // right to castle on the given side
        (self.board.position.castling_rights & rights_mask(side, Rights::ALLOWED, color) != 0)
            // none of the squares on the way are occupied
        && (self.board.position.occupied_squares() & squares_mask(side, Mask::EMPTY, color) == 0)
        // squares crossed by the king are not in check
        && (BBWraper(squares_mask(side, Mask::CHECK, color)).all(|square| !self.board.is_in_check(square, color)))
    }
}

// Manipulation helpers
impl<'a> PseudoLegalGenerator<'a> {
    fn push_move(&mut self, origin: Square, dest: Square, flags: u16) {
        self.push_move_internal(Move::new_with_flags(origin, dest, flags));
    }
    // Pushs the given move in the move stack
    fn push_move_internal(&mut self, mov: Move) {
        self.moves_list[self.number_of_moves] = mov;
        self.number_of_moves += 1;
    }

    // Helper to push all the possible promotions
    fn push_promotions_from_move(&mut self, promotion_move: Move) {
        for promoted_piece in &AVAILABLE_PROMOTION {
            self.push_move_internal(promotion_move.set_promoted_piece(*promoted_piece));
        }
    }

    // Helper for pieces that can perform captures and quiet moves at the same time
    fn push_attack(&mut self, origin_square: Square, attack: BitBoard) {
        let opponent_color = self.board.position.side_to_move.transpose();
        let opponent_pieces = self.board.position[opponent_color];
        // Captures
        for square in BBWraper(attack & opponent_pieces) {
            self.push_move_internal(Move::new_with_flags(origin_square, square, CAPTURE_FLAG));
        }
        // Quiet moves
        for square in BBWraper(attack & self.board.position.empty_squares()) {
            self.push_move_internal(Move::new(origin_square, square));
        }
    }

    // Push a pawn attack of the given color with the given flag taking care of promotions
    fn push_pawn_attack(&mut self, origin: BitBoard, attack: BitBoard, flag: u16, color: Color) {
        for (origin, dest) in BBWraper(origin).zip(BBWraper(attack)) {
            if PROMOTION_LINE[color].has_square(dest) {
                self.push_promotions_from_move(Move::new_with_flags(origin, dest, flag))
            } else {
                self.push_move_internal(Move::new_with_flags(origin, dest, flag))
            }
        }
    }
}

/* Debugging interface */
impl Board {
    #[allow(dead_code)]
    pub fn play_move(&mut self, mov: &str) {
        let chars: Vec<_> = mov.chars().collect();
        assert_eq!(chars.len(), 4);
        let origin_file = chars[0];
        let origin_row = chars[1];
        let dest_file = chars[2];
        let dest_row = chars[3];
        let origin_square = SqWrapper::from_char_file_rank(origin_file, origin_row);
        let dest_square = SqWrapper::from_char_file_rank(dest_file, dest_row);

        LegalMoveList::new(self)
            .find(|mov| {
                mov.origin_square() == origin_square && mov.destination_square() == dest_square
            })
            .map(|mov| self.make(mov))
            .expect(&format!(
                "Move not found {}{}{}{}",
                origin_file, origin_row, dest_file, dest_row
            ));
    }

    #[allow(dead_code)]
    pub fn print_possible_moves(&mut self) {
        for mov in LegalMoveList::new(self) {
            println!("{}", mov);
        }
    }

    // Perft values
    fn internal_perft(&mut self, depth: u8, start_depth: u8) -> usize {
        if depth == 1 {
            return self.number_of_legal_moves();
        }

        let move_maker =
            LegalMoveMakerWithWork::new(self, |board| board.internal_perft(depth - 1, start_depth));

        move_maker.fold(0, |acc, (mov, partial_sum)| {
            if depth == start_depth {
                println!("{}: {}", Move::from(mov), partial_sum);
            }

            acc + partial_sum
        })
    }

    // Runs a perft test of the given depth on the given board
    pub fn perft(&mut self, depth: u8) -> usize {
        assert!(depth > 0);
        self.internal_perft(depth, depth)
    }
}

fn generate_en_passant_table() -> BlackWhiteAttribute<[BitBoard; 8]> {
    let mut white_en_passant_candidates = [BBWraper::empty(); 8];
    for square in BBWraper(ROW_5) {
        if square.file() != 0 {
            white_en_passant_candidates[square.file() as usize] =
                white_en_passant_candidates[square.file() as usize].add_square(square.left());
        }
        if square.file() != 7 {
            white_en_passant_candidates[square.file() as usize] =
                white_en_passant_candidates[square.file() as usize].add_square(square.right());
        }
    }

    let black_en_passant_candidates =
        array_init::array_init(|i| white_en_passant_candidates[i] >> 8);

    BlackWhiteAttribute::new(black_en_passant_candidates, white_en_passant_candidates)
}

fn generate_pawn_attacks() -> BlackWhiteAttribute<AttackTable> {
    let mut white_pawn_attacks = [BBWraper::empty(); 64]; // First and last row while remain empty

    for square in BBWraper(SQUARES & !ROW_8) {
        if !FILE_A.has_square(square) {
            white_pawn_attacks[square as usize] =
                white_pawn_attacks[square as usize].add_square(square.forward_left());
        }
        if !FILE_H.has_square(square) {
            white_pawn_attacks[square as usize] =
                white_pawn_attacks[square as usize].add_square(square.forward_right());
        }
    }

    let mut black_pawn_attacks: [BitBoard; 64] =
        array_init::array_init(|i| white_pawn_attacks[i] >> 16);
    for square in BBWraper(ROW_8) {
        black_pawn_attacks[square as usize] =
            black_pawn_attacks[square.forward(Color::BLACK) as usize] << 8;
    }

    BlackWhiteAttribute::new(black_pawn_attacks, white_pawn_attacks)
}

fn generate_knight_attacks() -> AttackTable {
    let mut knight_attacks = [BBWraper::empty(); 64];

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
                *attack_bitboard = attack_bitboard.add_square(SqWrapper::from_file_rank(
                    (file + i) as u8,
                    (rank + j) as u8,
                ));
            }
        }
    }

    knight_attacks
}

fn generate_king_attacks() -> AttackTable {
    let mut king_attacks = [BBWraper::empty(); 64];

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
                *attack_bitboard = attack_bitboard.add_square(SqWrapper::from_file_rank(
                    (file + i) as u8,
                    (rank + j) as u8,
                ));
            }
        }
    }

    king_attacks
}
