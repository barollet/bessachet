pub mod checks_pins;
pub mod init_magic;
pub mod moves;
mod piece_attacks;

pub use self::checks_pins::*;
pub use self::moves::*;
pub use self::piece_attacks::*;
use board::*;
use std::convert::From;
use std::ops::{Index, IndexMut};

use types::*;

// Perft tests for move generation, see move_generation/perft_tests.rs
#[cfg(test)]
mod perft_tests;

// Testing the magic factors
#[cfg(test)]
mod magic_factors_tests;

// This module provides a legal move generator to a board
// through the possible_moves interface
// TODO improve interface

// Structures definition
// A pseudo legal move generator, illegal moves from absolutely pinned pieces are already removed
// Move ordering is performed at this step
// This is a Builder object.
pub struct PseudoLegalGenerator<'a> {
    board: &'a Board,
    helper: MoveGenHelper,
    // NOTE: The maximum size is 128 even if we can construct a position with 218 moves
    // maybe we have to change this to 218 or a dynamically sized struct
    moves_list: [Move; 128],
    number_of_moves: usize,
}

// A list of pseudo legal moves on which we can iterate
pub struct PseudoLegalMoveList {
    pub moves_list: [Move; 128],
    iterator_move: usize,
    pub number_of_moves: usize,
}

impl std::default::Default for PseudoLegalMoveList {
    fn default() -> Self {
        PseudoLegalMoveList {
            moves_list: [NULL_MOVE; 128],
            iterator_move: 0,
            number_of_moves: 0,
        }
    }
}

impl<'a> From<PseudoLegalGenerator<'a>> for PseudoLegalMoveList {
    fn from(generator: PseudoLegalGenerator) -> PseudoLegalMoveList {
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

pub struct LegalMoveMaker<'a> {
    pseudo_legal_moves: PseudoLegalMoveList,
    board: &'a mut Board,
    ext_mov: Option<ExtendedMove>,
}

impl<'a> Board {
    pub fn move_maker(&'a mut self) -> LegalMoveMaker<'a> {
        LegalMoveMaker {
            pseudo_legal_moves: self.generate_pseudo_legal_moves(),
            board: self,
            ext_mov: None,
        }
    }
}

// A legal move iterator that doesn't make any moves on the board
struct LegalMoveList {}

impl LegalMoveList {
    fn iter(board: &mut Board) -> impl Iterator<Item = Move> + '_ {
        board.generate_pseudo_legal_moves().filter(move |mov| {
            let is_king_mov = board[mov.origin_square()].unwrap() == Piece::KING;
            // If the move can't be illegal
            if !mov.has_exact_flags(EN_PASSANT_CAPTURE_FLAG) && !is_king_mov {
                true
            } else {
                board.legality_check(*mov).map_or(false, |ext_mov| {
                    board.unmake(ext_mov);
                    true
                })
            }
        })
    }
}

impl Index<usize> for PseudoLegalMoveList {
    type Output = Move;
    fn index(&self, index: usize) -> &Move {
        &self.moves_list[index]
    }
}

impl IndexMut<usize> for PseudoLegalMoveList {
    fn index_mut(&mut self, index: usize) -> &mut Move {
        &mut self.moves_list[index]
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

    // Choose if the move needs a legality check or can be made without further check
    // If the given move is illagal, returns None
    pub fn legal_make(&mut self, pseudo_legal_move: Move) -> Option<ExtendedMove> {
        let is_king_mov = self[pseudo_legal_move.origin_square()].unwrap() == Piece::KING;
        // The move can be illegal
        if pseudo_legal_move.is_en_passant_capture() || is_king_mov {
            self.legality_check(pseudo_legal_move)
        // No further check needed
        } else {
            Some(self.make(pseudo_legal_move))
        }
    }

    // A piece of the given color on the given square would be in check
    fn is_in_check(&self, square: Square, color: Color) -> bool {
        // We use the super piece method
        let opponent_color = !color;
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
        self.is_in_check(king_square, player_color)
    }
}

// Move generation functions
impl Board {
    pub fn generate_pseudo_legal_moves(&self) -> PseudoLegalMoveList {
        // We update the checks and pins on demand
        let mut move_gen = PseudoLegalGenerator::new(&self);

        // If not in check, we fetch the move as usual
        if move_gen.helper.number_of_checkers == 0 {
            move_gen.all_moves();
        } else if move_gen.helper.number_of_checkers == 1 {
            // If in simple check we can block the slider, capture the checker or escape the king
            move_gen.capture_and_block_checker().escape_king();
        } else {
            // If in double check we can only escape the king
            debug_assert_eq!(move_gen.helper.number_of_checkers, 2);
            move_gen.escape_king();
        }
        PseudoLegalMoveList::from(move_gen)
    }

    // Need a mutable reference to test pseudo legal moves
    fn number_of_legal_moves(&mut self) -> usize {
        LegalMoveList::iter(self).count()
    }
}

impl<'a> PseudoLegalGenerator<'a> {
    fn new(board: &Board) -> PseudoLegalGenerator {
        PseudoLegalGenerator {
            board,
            helper: MoveGenHelper::new(&board.position),
            moves_list: [NULL_MOVE; 128],
            number_of_moves: 0,
        }
    }

    fn escape_king(&mut self) -> &mut PseudoLegalGenerator<'a> {
        let player_color = self.board.position.side_to_move;
        let king_square = self.board.position.king_square(player_color);
        self.push_attack(king_square, king_attack(king_square));

        self
    }

    fn capture_and_block_checker(&mut self) -> &mut PseudoLegalGenerator<'a> {
        let checker_square = self.helper.checkers[0];
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

    fn all_moves(&mut self) -> &mut PseudoLegalGenerator<'a> {
        self.all_moves_with_target(BBWrapper::full()).escape_king()
    }

    // Generate only moves that ends in the target mask
    fn all_moves_with_target(&mut self, target_mask: BitBoard) -> &mut PseudoLegalGenerator<'a> {
        let player_color = self.board.position.side_to_move;
        let player_pieces = self.board.position[player_color];
        let opponent_pieces = self.board.position[!player_color];

        let free_pieces = self.helper.free_pieces;
        let empty_squares = self.board.position.empty_squares();

        // Pawns
        let pawns = self.board.position[Piece::PAWN] & player_pieces & free_pieces;
        // Push

        let simple_pushed_pawns = pawns.push(player_color) & empty_squares & target_mask;
        let origin_pawns = simple_pushed_pawns.push(!player_color); // Push back
        self.push_pawn_attack(origin_pawns, simple_pushed_pawns, NO_FLAG, player_color);

        // Capture left
        let captures_dest =
            (pawns & !FILE_A).left_capture(player_color) & opponent_pieces & target_mask;
        let captures_origin = captures_dest.right_capture(!player_color);
        self.push_pawn_attack(captures_origin, captures_dest, CAPTURE_FLAG, player_color);
        // Capture right
        let captures_dest =
            (pawns & !FILE_H).right_capture(player_color) & opponent_pieces & target_mask;
        let captures_origin = captures_dest.left_capture(!player_color);
        self.push_pawn_attack(captures_origin, captures_dest, CAPTURE_FLAG, player_color);
        // Double push
        let double_pushed = (pawns.push(player_color) & empty_squares).push(player_color)
            & empty_squares
            & EN_PASSANT_LINE[player_color]
            & target_mask;
        let origin_pawns = double_pushed.push(!player_color).push(!player_color);
        self.push_pawn_attack(origin_pawns, double_pushed, DOUBLE_PUSH_FLAG, player_color);
        // En passant (we don't need the target mask as en passant capture will be checked
        // afterward anyway)
        for pawn in BBWrapper(self.board.position.en_passant_candidates() & pawns) {
            let en_passant_square = self.board.position.en_passant.unwrap();
            let en_passant_dest = en_passant_square.get().forward(player_color);
            if self.board[en_passant_dest].is_none() {
                self.push_move(pawn, en_passant_dest, EN_PASSANT_CAPTURE_FLAG);
            }
        }

        // Knights
        let knights = player_pieces & self.board.position[Piece::KNIGHT] & free_pieces;
        for knight in BBWrapper(knights) {
            self.push_attack(knight, knight_attack(knight) & target_mask);
        }

        let occupied_squares = self.board.position.occupied_squares();
        macro_rules! sliding_attack {
            ($piece: expr, $attack_function: ident) => {
                let pieces = player_pieces
                    & (self.board.position[$piece] | self.board.position[Piece::QUEEN])
                    & self.helper.free_pieces;
                for piece in BBWrapper(pieces) {
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
        let pinned_pieces = self.helper.pinned_pieces_iterator();
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
                    for dest in BBWrapper(
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
        if target_mask != BBWrapper::empty() {
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
        && (BBWrapper(squares_mask(side, Mask::CHECK, color)).all(|square| !self.board.is_in_check(square, color)))
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
        let opponent_color = !self.board.position.side_to_move;
        let opponent_pieces = self.board.position[opponent_color];
        // Captures
        for square in BBWrapper(attack & opponent_pieces) {
            self.push_move_internal(Move::new_with_flags(origin_square, square, CAPTURE_FLAG));
        }
        // Quiet moves
        for square in BBWrapper(attack & self.board.position.empty_squares()) {
            self.push_move_internal(Move::new(origin_square, square));
        }
    }

    // Push a pawn attack of the given color with the given flag taking care of promotions
    fn push_pawn_attack(&mut self, origin: BitBoard, attack: BitBoard, flag: u16, color: Color) {
        for (origin, dest) in BBWrapper(origin).zip(BBWrapper(attack)) {
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

        let played_move = LegalMoveList::iter(self).find(|mov| {
            mov.origin_square() == origin_square && mov.destination_square() == dest_square
        });
        played_move
            .map(|mov| self.make(mov))
            .unwrap_or_else(|| panic!("Move not found {}", Move::new(origin_square, dest_square),));
    }

    #[allow(dead_code)]
    pub fn print_possible_moves(&mut self) {
        for mov in LegalMoveList::iter(self) {
            println!("{}", mov);
        }
    }

    // Perft values
    fn internal_perft(&mut self, depth: u8, start_depth: u8) -> usize {
        if depth == 1 {
            return self.number_of_legal_moves();
        }

        self.generate_pseudo_legal_moves()
            .fold(0, |acc, pseudo_legal_move| {
                match self.legal_make(pseudo_legal_move) {
                    Some(ext_mov) => {
                        let partial_sum = self.internal_perft(depth - 1, start_depth);
                        self.unmake(ext_mov);
                        if depth == start_depth {
                            println!("{}: {}", Move::from(ext_mov), partial_sum);
                        }

                        acc + partial_sum
                    }
                    None => acc,
                }
            })
    }

    // Runs a perft test of the given depth on the given board
    pub fn perft(&mut self, depth: u8) -> usize {
        assert!(depth > 0);
        self.internal_perft(depth, depth)
    }
}
