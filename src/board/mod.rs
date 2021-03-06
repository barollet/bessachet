use types::*;

use move_generation::*;

pub mod fen;
pub mod utils;

// Exports the prelude
pub use self::utils::*;

use search::GameNode;

use std::rc::Rc;

// The board is represented as a set of bitboards
// See: https://www.chessprogramming.org/Bitboards

// LSB is the square lower-right and the MSB is the upper-left one.
// 63 62 61 60 59 58 57 56      black pieces
// 55 54 53 52 51 50 49 48      black pawns
// 47 46 45 44 43 42 41 40
// 39 38 37 36 35 34 33 32
// 31 30 29 28 27 26 25 24
// 23 22 21 20 19 18 17 16
// 15 14 13 12 11 10 9  8       white pawns
// 7  6  5  4  3  2  1  0       white pieces

// Defining a unique position (Information used in hashing)
#[derive(Clone)]
pub struct Position {
    pub pieces: [BitBoard; 6],
    pub occupancy: BlackWhiteAttribute<BitBoard>,
    pub en_passant: Option<NonZeroSquare>,
    pub castling_rights: u8,
    pub side_to_move: Color,
}

#[derive(Clone)]
pub struct Board {
    pub position: Position,
    // Some redundancy with the piece bitboards to allow fast access to the content of a single
    // square
    pub mailbox_88: [Option<Piece>; 64],

    // Internal capture stack for making and unmaking moves
    // Pawns captured en passant are not here (there is the code in the move encoding for en passant)
    capture_stack: [Piece; 32],
    capture_stack_size: usize,

    // Game tree
    pub game_tree_node: Rc<GameNode>,

    // Misc
    halfmove_clock: u8,
    ply: u8,
}

// Board initialization
impl Board {
    // Returns a board representing a given position
    // Two halfboards and initial parameters
    pub fn init_from_position(position: Position, halfmove_clock: u8, ply: u8) -> Self {
        let mut board = Board {
            mailbox_88: initialize_mailbox_88(&position),

            position, // We move the position once we initialized everything

            capture_stack: [Piece::PAWN; 32], // We could let it uninitialized
            capture_stack_size: 0,

            // An uninitialized game node
            game_tree_node: Rc::new(GameNode::default()),

            halfmove_clock,
            ply,
        };
        board.game_tree_node = Rc::new(GameNode::new(&board));

        board
    }

    pub fn initial_position() -> Self {
        Self::init_from_position(Position::initial_position(), 0, 0)
    }
}

impl Position {
    pub fn occupied_squares(&self) -> BitBoard {
        self[WHITE] | self[BLACK]
    }
    pub fn empty_squares(&self) -> BitBoard {
        !self.occupied_squares()
    }
    pub fn king_square(&self, color: Color) -> Square {
        Square::from(BBWrapper(self[color] & self[Piece::KING]))
    }
    // Returns a bitboard of pawn candidates to capture en passant
    pub fn en_passant_candidates(&self) -> BitBoard {
        if let Some(square) = self.en_passant {
            EN_PASSANT_TABLE[self.side_to_move][square.get().file() as usize]
        } else {
            BBWrapper::empty()
        }
    }
}

// Board internal manipulation
impl Board {
    // Piece manipulation
    fn create_piece(&mut self, square: Square, piece: Piece, color: Color) {
        let set_mask = BitBoard::from(SqWrapper(square));
        self.position[piece] |= set_mask;
        self.position[color] |= set_mask;
        self[square] = Some(piece);
    }

    fn delete_piece(&mut self, square: Square, piece: Piece, color: Color) {
        let reset_mask = !BitBoard::from(SqWrapper(square));
        self.position[piece] &= reset_mask;
        self.position[color] &= reset_mask;
        self[square] = None;
    }

    fn move_piece(&mut self, from: Square, to: Square, piece: Piece, color: Color) {
        self.delete_piece(from, piece, color);
        self.create_piece(to, piece, color);
    }

    // Capture stack manipulation
    fn push_captured(&mut self, piece: Piece) {
        self.capture_stack[self.capture_stack_size] = piece;
        self.capture_stack_size += 1;
    }

    fn pop_captured(&mut self) -> Piece {
        self.capture_stack_size -= 1;
        self.capture_stack[self.capture_stack_size]
    }
}

// Make and Unmake procedure
impl Board {
    // For make and unmake procedure, we assume that the provided move is valid
    // If not the program can panic! or remain in an unconsistent state
    pub fn make(&mut self, mov: Move) -> ExtendedMove {
        let player_color = self.position.side_to_move;
        let opponent_color = !player_color;
        let moved_piece = self[mov.origin_square()].unwrap();

        let old_caslting_rights = self.position.castling_rights;
        let old_en_passant_square = self.position.en_passant;
        let old_halfmove_clock = self.halfmove_clock;

        self.halfmove_clock = if moved_piece == Piece::PAWN {
            0
        } else {
            self.halfmove_clock + 1
        };

        // Capture
        // Not triggered by en passant capture
        if mov.is_capture() {
            let captured_piece = self[mov.destination_square()].unwrap();
            self.delete_piece(mov.destination_square(), captured_piece, opponent_color);

            self.halfmove_clock = 0;

            // Remove caslting rights if it captures a rook on a castling square
            macro_rules! remove_castling_rights {
                ($color: expr, $side: expr) => {
                    if player_color == $color
                        && mov.destination_square()
                            == rook_square($side, RookSquare::ORIGIN, !$color)
                    {
                        self.position.castling_rights &=
                            rights_mask($side, Rights::REMOVE, !$color);
                    }
                };
            }
            if captured_piece == Piece::ROOK {
                remove_castling_rights!(WHITE, CastlingSide::QUEEN);
                remove_castling_rights!(WHITE, CastlingSide::KING);
                remove_castling_rights!(BLACK, CastlingSide::QUEEN);
                remove_castling_rights!(BLACK, CastlingSide::KING);
            }

            self.push_captured(captured_piece);
        }

        // We move the piece after the capture
        self.move_piece(
            mov.origin_square(),
            mov.destination_square(),
            moved_piece,
            player_color,
        );

        // En passant capture (before updating en passant target from double push)
        if mov.is_en_passant_capture() {
            let en_passant_square = self.position.en_passant.unwrap().get();
            self.delete_piece(en_passant_square, Piece::PAWN, opponent_color);

            self.halfmove_clock = 0;
        }

        // Castling, move the other rook
        // Castling rights are removed when the king moves
        if let Some((rook_from, rook_dest)) = mov.get_castling_rook(self.position.side_to_move) {
            self.move_piece(rook_from, rook_dest, Piece::ROOK, player_color);
        }

        // Double pawn push, set the en passant target
        self.position.en_passant = mov.get_en_passant_target_square();

        // Promotion
        if let Some(promotion_piece) = mov.get_promotion_piece() {
            self.delete_piece(mov.destination_square(), Piece::PAWN, player_color);
            self.create_piece(mov.destination_square(), promotion_piece, player_color);

            self.halfmove_clock = 0;
        }

        // Castling rights update
        if moved_piece == Piece::KING {
            self.position.castling_rights &=
                rights_mask(CastlingSide::BOTH, Rights::REMOVE, player_color);
        } else if moved_piece == Piece::ROOK {
            if mov.origin_square()
                == rook_square(CastlingSide::KING, RookSquare::ORIGIN, player_color)
            {
                self.position.castling_rights &=
                    rights_mask(CastlingSide::KING, Rights::REMOVE, player_color);
            } else if mov.origin_square()
                == rook_square(CastlingSide::QUEEN, RookSquare::ORIGIN, player_color)
            {
                self.position.castling_rights &=
                    rights_mask(CastlingSide::QUEEN, Rights::REMOVE, player_color);
            }
        }

        // Update the side to move
        self.position.side_to_move = opponent_color;

        //self.ply += 1;

        // Returns the decorated move
        mov.decorate(
            old_caslting_rights,
            old_en_passant_square.map_or(0, |u| u.get()),
            old_halfmove_clock,
        )
    }

    // Unmake the given move, if the move was played by White, the board must be in Black side to
    // play state (and vice versa)
    pub fn unmake(&mut self, ext_mov: ExtendedMove) {
        // Move the piece back
        let played_color = !self.position.side_to_move;
        let played_color_opponent = self.position.side_to_move;
        let mov = Move::from(ext_mov);
        let moved_piece = self[mov.destination_square()].unwrap();

        self.move_piece(
            mov.destination_square(),
            mov.origin_square(),
            moved_piece,
            played_color,
        );

        // Capture
        if mov.is_capture() {
            let captured_piece = self.pop_captured();
            self.create_piece(
                mov.destination_square(),
                captured_piece,
                played_color_opponent,
            );
        }

        // Castling, move the other rook
        if let Some((rook_from, rook_dest)) = mov.get_castling_rook(played_color) {
            self.move_piece(rook_dest, rook_from, Piece::ROOK, played_color);
        }

        // Promotion
        if let Some(promotion_piece) = mov.get_promotion_piece() {
            self.delete_piece(mov.origin_square(), promotion_piece, played_color);
            self.create_piece(mov.origin_square(), Piece::PAWN, played_color);
        }

        self.position.en_passant = ext_mov.get_en_passant_target();
        let new_en_passant_square = self.position.en_passant;

        // We restore en passant capture after en passant restoration
        if mov.is_en_passant_capture() {
            let en_passant_square = new_en_passant_square.unwrap().get();
            self.create_piece(en_passant_square, Piece::PAWN, played_color_opponent);
        }

        // Castling rights restoration
        let restored_castling_rights = ext_mov.get_castling_rights();
        self.position.castling_rights = restored_castling_rights;

        // Halfmove clock restoration
        self.halfmove_clock = ext_mov.get_halfmove_clock();

        self.position.side_to_move = played_color;

        //self.ply -= 1;
    }
}
