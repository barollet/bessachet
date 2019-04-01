use board::*;
use move_generation::piece_attacks::*;
use types::*;
/*
 * The MoveGenHelper is updated before move generation and takes care of computing
 * pinned pieces with there liberties and checking pieces
 */
#[derive(Clone)]
pub struct MoveGenHelper {
    // We hold the squares that are pinned and no more than 2 pieces can be pinned on the same
    // direction, there is also the bitboard of the liberties of the pinned piece (to be
    // intersected with the actual moves of the piece)
    pub pinned_pieces: [(Square, BitBoard); 8],
    pub number_of_pinned_pieces: usize, // pinners stack indexes

    pub free_pieces: BitBoard, // A global pin mask to quickly get if a piece is pinned or free

    // checkers
    pub checkers: [Square; 2],
    pub number_of_checkers: usize,
}

impl MoveGenHelper {
    pub fn new(position: &Position) -> Self {
        let mut mgh = MoveGenHelper {
            pinned_pieces: [(0, 0); 8],
            number_of_pinned_pieces: 0,
            free_pieces: BBWrapper::full(),
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

// Structure manipulation
impl MoveGenHelper {
    // Push a new pinned piece with its liberties
    fn push_pinned(&mut self, pinned_square: Square, liberties: BitBoard) {
        self.pinned_pieces[self.number_of_pinned_pieces] = (pinned_square, liberties);
        self.number_of_pinned_pieces += 1;
        self.free_pieces.remove_square(pinned_square);
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
        let mut pin_liberties: BitBoard = square_mask_between(pinner, target);

        // get the pinned piece should be reduced to a single square or empty
        let pinned: BitBoard = pin_liberties & position[position.side_to_move];
        if pinned != BBWrapper::empty() {
            let pinned = Square::from(BBWrapper(pinned));
            self.push_pinned(pinned, pin_liberties.add_square(pinner));
        } else if pin_liberties & position.occupied_squares() == BBWrapper::empty() {
            self.push_checker(pinner);
        }
    }

    fn compute_pinned_pieces(&mut self, pos: &Position, piece: Piece, xray_function: XrayFunction) {
        let king_square = pos.king_square(pos.side_to_move);
        let opponent_color = !pos.side_to_move;

        for piece_square in BBWrapper((pos[piece] | pos[Piece::QUEEN]) & pos[opponent_color]) {
            let xray_attack = xray_function(piece_square, pos.occupied_squares());
            if xray_attack.has_square(king_square) {
                self.decide_pin_check(pos, piece_square, king_square);
            }
        }
    }

    // Compute only checks by knights and pawns as bishop, rooks and queens are already done by
    // pinners computation
    fn compute_pawn_knight_checkers(&mut self, pos: &Position) {
        let opponent_color = !pos.side_to_move;

        let king_square = pos.king_square(pos.side_to_move);

        let opponent_pawns = pos[opponent_color] & pos[Piece::PAWN];
        for pawn_square in BBWrapper(pawn_attack(king_square, pos.side_to_move) & opponent_pawns) {
            self.push_checker(pawn_square)
        }
        let opponent_knights = pos[opponent_color] & pos[Piece::KNIGHT];
        for knight_square in BBWrapper(knight_attack(king_square) & opponent_knights) {
            self.push_checker(knight_square);
        }
    }

    pub fn pinned_pieces_iterator(&self) -> PinnedPiecesIterator {
        PinnedPiecesIterator {
            pinned_pieces: self.pinned_pieces,
            number_of_pinned_pieces: self.number_of_pinned_pieces,
            iteration_index: 0,
        }
    }
}

pub struct PinnedPiecesIterator {
    pinned_pieces: [(Square, BitBoard); 8],
    number_of_pinned_pieces: usize,
    iteration_index: usize,
}

// This iterates over the pinned pieces
impl Iterator for PinnedPiecesIterator {
    type Item = (Square, BitBoard);
    fn next(&mut self) -> Option<Self::Item> {
        if self.iteration_index < self.number_of_pinned_pieces {
            let item = self.pinned_pieces[self.iteration_index];
            self.iteration_index += 1;
            Some(item)
        } else {
            None
        }
    }
}
