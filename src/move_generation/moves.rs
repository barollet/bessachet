// Module holding move encoding

use utils::*;

use enum_primitive::FromPrimitive;

// An ExtendedMove is a 64 bits word following more or the less the extended
// representation in https://www.chessprogramming.org/Encoding_Moves
// MSB -------------------------------------------------------- LSB
// latter   | ep5 .. ep0 | hf6 .. hf0 | csl3 .. csl0
// 63 .. 39 |  34 .. 28  |  27 .. 20  |  19  ..  16
// Move structure
// prom | capt | sp1 | sp0 | dst5 .. dst0 | st5 .. st0
//  15  |  14  |  13 | 12  |  11  ..  6   |  5  ..  0
#[derive(Clone, Copy)]
pub struct Move(u16);
#[derive(Clone, Copy)]
pub struct ExtendedMove(u64);

pub const SPECIAL0_FLAG: u16 = 1 << 12;
pub const SPECIAL1_FLAG: u16 = 1 << 13;
pub const CAPTURE_FLAG: u16 = 1 << 14;
pub const PROMOTION_FLAG: u16 = 1 << 15;

const FLAGS_RANGE: u16 = SPECIAL0_FLAG | SPECIAL1_FLAG | CAPTURE_FLAG | PROMOTION_FLAG;

pub const DOUBLE_PUSH_FLAG: u16 = SPECIAL0_FLAG;
pub const EN_PASSANT_CAPTURE_FLAG: u16 = CAPTURE_FLAG | DOUBLE_PUSH_FLAG;

const KING_CASTLE_FLAG: u16 = SPECIAL1_FLAG;
const QUEEN_CASTLE_FLAG: u16 = SPECIAL0_FLAG | SPECIAL1_FLAG;

// [black castle, white castle]
pub const KING_CASTLE_MOVES: BlackWhiteAttribute<Move> = BlackWhiteAttribute::new(
    Move::raw_new(D1_SQUARE, B1_SQUARE).set_flags(KING_CASTLE_FLAG),
    Move::raw_new(E1_SQUARE, G1_SQUARE).set_flags(KING_CASTLE_FLAG),
);

pub const QUEEN_CASTLE_MOVES: BlackWhiteAttribute<Move> = BlackWhiteAttribute::new(
    Move::raw_new(D1_SQUARE, F1_SQUARE).set_flags(QUEEN_CASTLE_FLAG),
    Move::raw_new(E1_SQUARE, C1_SQUARE).set_flags(QUEEN_CASTLE_FLAG),
);

pub const NULL_MOVE: Move = Move::raw_new(Square::new(0), Square::new(0));
pub const NULL_EXTMOVE: ExtendedMove = ExtendedMove(0);

pub const CASTLING_RIGHTS_BITS_OFFSET: u8 = 16;
pub const HALFMOVE_CLOCK_BITS_OFFSET: u8 = 20;
pub const EN_PASSANT_SQUARE_BITS_OFFSET: u8 = 28;

pub const CASTLING_RIGHTS_BITS_SIZE: u8 = 4;
pub const HALFMOVE_CLOCK_BITS_SIZE: u8 = 7;
pub const EN_PASSANT_SQUARE_BITS_SIZE: u8 = 6;

impl Move {
    // Creates a simple move with no side effect
    pub fn quiet_move(from: Square, to: Square) -> Self {
        Move(u16::from(from.0) + (u16::from(to.0) << 6))
    }

    pub fn double_pawn_push_to(to: Square) -> Self {
        Self::tactical_move(to.behind().behind(), to, DOUBLE_PUSH_FLAG)
    }
    pub fn double_pawn_push(from: Square, to: Square) -> Self {
        Self::tactical_move(from, to, DOUBLE_PUSH_FLAG)
    }

    // Creates a move with all the specified flags
    pub fn tactical_move(from: Square, to: Square, flags: u16) -> Self {
        Self::quiet_move(from, to).set_flags(flags)
    }

    // This is supposed to be called only for constant compilation and not an runtime
    #[allow(clippy::cast_lossless)]
    const fn raw_new(from: Square, to: Square) -> Self {
        Move(from.0 as u16 + ((to.0 as u16) << 6))
    }

    // Helper to set some flags to the move
    const fn set_flags(self, flags: u16) -> Self {
        Move(self.0 | flags)
    }

    // Returns wether the move has the given flags set
    pub fn has_flags(self, flags: u16) -> bool {
        self.0 & flags != 0
    }

    fn has_exact_flags(self, flags: u16) -> bool {
        self.0 & FLAGS_RANGE == flags
    }

    pub fn set_promoted_piece(self, piece: Piece) -> Self {
        Move(self.0 | (piece as u16 & 0b11) << 12)
    }

    // Public interface for the board
    pub fn origin_square(self) -> Square {
        Square::new((self.0 & 0x3f) as u8)
    }

    pub fn destination_square(self) -> Square {
        Square::new((self.0 >> 6) as u8 & 0x3f)
    }

    pub fn get_promotion_piece(self) -> Option<Piece> {
        if self.has_flags(PROMOTION_FLAG) {
            // from_u32 returns an Option, panic!s if the piece code is invalid
            Piece::from_u16(self.0 >> 12 & 0b11)
        } else {
            None
        }
    }

    // if the move is a double pawn push, returns the associated en passant target square
    pub fn get_en_passant_target_square(self) -> Option<Square> {
        if self.has_exact_flags(DOUBLE_PUSH_FLAG) {
            Some(self.destination_square())
        } else {
            None
        }
    }

    // If this is a castling move, returns the from and to squares of the associated tower
    pub fn get_castling_rook(self, side_to_move: Color) -> Option<(Square, Square)> {
        if self.has_exact_flags(KING_CASTLE_FLAG) {
            Some((
                KING_CASTLE_ROOK_ORIGIN_SQUARES[side_to_move],
                KING_CASTLE_ROOK_DEST_SQUARES[side_to_move],
            ))
        } else if self.has_exact_flags(QUEEN_CASTLE_FLAG) {
            Some((
                QUEEN_CASTLE_ROOK_ORIGIN_SQUARES[side_to_move],
                QUEEN_CASTLE_ROOK_DEST_SQUARES[side_to_move],
            ))
        } else {
            None
        }
    }

    // If this is an en passant capture, returns the captured pawn square
    pub fn get_en_passant_capture_square(self) -> Option<Square> {
        if self.has_exact_flags(EN_PASSANT_CAPTURE_FLAG) {
            Some(self.destination_square().behind())
        } else {
            None
        }
    }

    // Not triggered by en passant capture
    pub fn is_capture(self) -> bool {
        self.has_flags(CAPTURE_FLAG) & !self.has_exact_flags(EN_PASSANT_CAPTURE_FLAG)
    }
}

impl ExtendedMove {
    pub fn from_raw_move(raw_move: Move) -> ExtendedMove {
        ExtendedMove(u64::from(raw_move.0))
    }

    pub fn get_raw_move(self) -> Move {
        #[allow(clippy::cast_lossless)]
        Move(self.0 as u16)
    }

    // value has to have trailing zeros not to overwrite some other states
    pub fn set_castling_rights(self, value: u8) -> Self {
        ExtendedMove(self.0 | u64::from(value) << CASTLING_RIGHTS_BITS_OFFSET)
    }
    pub fn set_halfmove_clock(self, value: u8) -> Self {
        ExtendedMove(self.0 | u64::from(value) << HALFMOVE_CLOCK_BITS_OFFSET)
    }
    pub fn set_en_passant_target(self, value: u8) -> Self {
        ExtendedMove(self.0 | u64::from(value) << EN_PASSANT_SQUARE_BITS_OFFSET)
    }

    pub fn get_castling_rights(self) -> u8 {
        ((self.0 >> CASTLING_RIGHTS_BITS_OFFSET)
            & (u64::max_value() >> (64 - CASTLING_RIGHTS_BITS_SIZE))) as u8
    }
    pub fn get_halfmove_clock(self) -> u8 {
        ((self.0 >> HALFMOVE_CLOCK_BITS_OFFSET)
            & (u64::max_value() >> (64 - HALFMOVE_CLOCK_BITS_SIZE))) as u8
    }
    pub fn get_en_passant_target(self) -> Option<Square> {
        let square = ((self.0 >> EN_PASSANT_SQUARE_BITS_OFFSET)
            & (u64::max_value() >> (64 - EN_PASSANT_SQUARE_BITS_SIZE))) as u8;
        if square != 0 {
            Some(Square::new(square))
        } else {
            None
        }
    }
}

impl PartialEq for Move {
    fn eq(&self, other: &Self) -> bool {
        self.0 & 0xffff == other.0 & 0xffff
    }
}

impl Transpose for Move {
    fn transpose(&self) -> Self {
        let origin_square = self.origin_square().transpose();
        let dest_square = self.destination_square().transpose();
        let mut transposed_move = Move(self.0);
        transposed_move.0 &= !0xfff; // Clearing the old squares
        transposed_move.0 |= u16::from(origin_square.0) + (u16::from(dest_square.0) << 6);

        transposed_move
    }
}
