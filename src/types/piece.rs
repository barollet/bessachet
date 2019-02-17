//! The piece representation

use types::color::*;

// The piece type is represented as an enum but we need to be able to compute the enum from some
// given bits (due to promotion move encoding).
// NOTE: maybe change this to an u8 directly ?
enum_from_primitive! {
#[derive(Copy, Clone, PartialEq, Debug)]
#[repr(u8)]
// We declare knights to queen first to use the value directly in promotion code in move encoding
pub enum Piece {
    KNIGHT = 0,
    BISHOP,
    ROOK,
    QUEEN,

    PAWN,
    KING,
}
}

// Constant helpers to avoid repeating the Piece:: prefix
pub const PAWN: Piece = Piece::PAWN;
pub const KNIGHT: Piece = Piece::KNIGHT;
pub const BISHOP: Piece = Piece::BISHOP;
pub const ROOK: Piece = Piece::ROOK;
pub const QUEEN: Piece = Piece::QUEEN;
pub const KING: Piece = Piece::KING;

impl Piece {
    /// Returns the character associated to the piece
    pub fn to_raw_char(self) -> char {
        match self {
            Piece::PAWN => 'p',
            Piece::KNIGHT => 'n',
            Piece::BISHOP => 'b',
            Piece::ROOK => 'r',
            Piece::QUEEN => 'q',
            Piece::KING => 'k',
        }
    }
    /// Same as `to_raw_char` but with a color parameter
    pub fn to_char(self, color: Color) -> char {
        if color == WHITE {
            self.to_raw_char().to_ascii_uppercase()
        } else {
            self.to_raw_char()
        }
    }
}

/// A list of the pieces that we can promote to
pub const AVAILABLE_PROMOTION: [Piece; 4] = [KNIGHT, BISHOP, ROOK, QUEEN];
/// A list of all the pieces
pub const PIECES_LIST: [Piece; 6] = [PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING];
