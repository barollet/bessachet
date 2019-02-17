#![allow(clippy::unreadable_literal)]
use std::ops::{Index, IndexMut};

pub use board::fen::*;
pub use board::{Board, Position};
use types::*;

pub type MailBox88 = [Option<Piece>; 64];

// A trait for Board auxiliary structs like move generation, evaluator and hashers
pub trait AuxiliaryStruct<'a> {
    type Source;
    fn initialize(Self::Source) -> Self;
}

// Castling type enum for indexing
#[derive(Debug, Clone, Copy)]
pub enum CastlingSide {
    KING = 0,
    QUEEN,
    BOTH,
}

#[derive(Debug, Clone, Copy)]
pub enum Mask {
    EMPTY = 0,
    CHECK,
}

pub fn squares_mask(side: CastlingSide, kind: Mask, color: Color) -> BitBoard {
    CASTLING_MASKS[side as usize][kind as usize][color]
}

#[derive(Debug, Clone, Copy)]
pub enum Rights {
    REMOVE = 0,
    ALLOWED,
}

pub fn rights_mask(side: CastlingSide, kind: Rights, color: Color) -> u8 {
    CASTLING_RIGHTS[side as usize][kind as usize][color]
}

#[derive(Debug, Clone, Copy)]
pub enum RookSquare {
    ORIGIN = 0,
    DEST,
}

pub fn rook_square(side: CastlingSide, kind: RookSquare, color: Color) -> Square {
    ROOK_SQUARES[side as usize][kind as usize][color]
}

pub const CASTLING_MASKS: [[BlackWhiteAttribute<BitBoard>; 2]; 2] = [
    // King side
    [
        // Empty
        BlackWhiteAttribute::new(0x0600000000000000, 0x0000000000000006),
        // Not in check
        BlackWhiteAttribute::new(0x0e00000000000000, 0x000000000000000e),
    ],
    // Queen side
    [
        // Empty
        BlackWhiteAttribute::new(0x7000000000000000, 0x0000000000000070),
        // Not in check
        BlackWhiteAttribute::new(0x3800000000000000, 0x0000000000000038),
    ],
];

pub const CASTLING_RIGHTS: [[BlackWhiteAttribute<u8>; 2]; 3] = [
    // King side
    [
        // Remove
        BlackWhiteAttribute::new(0b1011, 0b1110),
        // Allowed
        BlackWhiteAttribute::new(0b0100, 0b0001),
    ],
    // Queen side
    [
        // Remove
        BlackWhiteAttribute::new(0b0111, 0b1101),
        // Allowed
        BlackWhiteAttribute::new(0b1000, 0b0010),
    ],
    // Both
    [
        // Remove
        BlackWhiteAttribute::new(0b0011, 0b1100),
        // /!\ We never look here
        BlackWhiteAttribute::new(0, 0),
    ],
];

pub const ROOK_SQUARES: [[BlackWhiteAttribute<Square>; 2]; 2] = [
    // King side
    [
        // Origin
        BlackWhiteAttribute::new(H8_SQUARE, H1_SQUARE),
        // Destination
        BlackWhiteAttribute::new(F8_SQUARE, F1_SQUARE),
    ],
    // Queen side
    [
        // Origin
        BlackWhiteAttribute::new(A8_SQUARE, A1_SQUARE),
        // Destination
        BlackWhiteAttribute::new(D8_SQUARE, D1_SQUARE),
    ],
];

impl<'a> AuxiliaryStruct<'a> for MailBox88 {
    type Source = &'a Position;

    fn initialize(position: Self::Source) -> Self {
        let mut mailbox = [None; 64];
        for piece in &PIECES_LIST {
            for square in BBWrapper(position.pieces[*piece as usize]) {
                mailbox[square as usize] = Some(*piece);
            }
        }

        mailbox
    }
}

impl Position {
    pub fn get_number_of(&self, piece: Piece, color: Color) -> i16 {
        (self.pieces[piece as usize] & self[color]).count_ones() as i16
    }
    pub fn initial_position() -> Position {
        // See utils.rs for the piece order in the array
        Position {
            pieces: [
                0x4200000000000042, // Knights
                0x2400000000000024, // Bishops
                0x8100000000000081, // Rooks
                0x1000000000000010, // Queens
                0x00ff00000000ff00, // Pawns
                0x0800000000000008,
            ], // Kings
            occupancy: BlackWhiteAttribute::new(
                0xffff000000000000, // Black
                0x000000000000ffff,
            ), // White
            en_passant: None,
            castling_rights: 0b1111,
            side_to_move: WHITE,
        }
    }
    pub fn empty_board() -> Position {
        Position {
            pieces: [0; 6],
            occupancy: BlackWhiteAttribute::new(BBWrapper::empty(), BBWrapper::empty()),
            en_passant: None,
            castling_rights: 0b1111,
            side_to_move: WHITE,
        }
    }
}

impl Index<Color> for Position {
    type Output = BitBoard;

    fn index(&self, color: Color) -> &Self::Output {
        &self.occupancy[color]
    }
}

impl IndexMut<Color> for Position {
    fn index_mut(&mut self, color: Color) -> &mut BitBoard {
        &mut self.occupancy[color]
    }
}

impl Index<Piece> for Position {
    type Output = BitBoard;

    fn index(&self, piece: Piece) -> &Self::Output {
        &self.pieces[piece as usize]
    }
}

impl IndexMut<Piece> for Position {
    fn index_mut(&mut self, piece: Piece) -> &mut BitBoard {
        &mut self.pieces[piece as usize]
    }
}

impl Index<Square> for Board {
    type Output = Option<Piece>;

    fn index(&self, square: Square) -> &Self::Output {
        &self.mailbox_88[square as usize]
    }
}

impl IndexMut<Square> for Board {
    fn index_mut(&mut self, square: Square) -> &mut Option<Piece> {
        &mut self.mailbox_88[square as usize]
    }
}

/*
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        /*
        write!(f, "{}", self[Color::WHITE])?;
        write!(f, "{}", self[Color::BLACK])
        */
for i in (0..8).rev() {
let halfboard = &self[Color::WHITE];
for j in (0..8).rev() {
if let Some(piece) = halfboard.mailbox_88[8 * i + j] {
if halfboard[Color::WHITE].has_square((8 * i + j) as u8) {
write!(f, "{}", piece.to_char().to_uppercase().next().unwrap())?;
} else {
write!(f, "{}", piece.to_char())?;
}
} else {
write!(f, "*")?;
}
}
write!(f, "   ")?;
let halfboard = &self[Color::BLACK];
for j in (0..8).rev() {
if let Some(piece) = halfboard.mailbox_88[8 * i + j] {
if halfboard[Color::WHITE].has_square((8 * i + j) as u8) {
write!(f, "{}", piece.to_char().to_uppercase().next().unwrap())?;
} else {
write!(f, "{}", piece.to_char())?;
}
} else {
write!(f, "*")?;
}
}
// Metadata info
if i == 6 {
write!(f, "   castling rights: ")?;
if self.castling_rights & 0b1000 != 0 {
write!(f, "q")?;
} else {
write!(f, ".")?;
}
if self.castling_rights & 0b0100 != 0 {
write!(f, "k")?;
} else {
write!(f, ".")?;
}
if self.castling_rights & 0b0010 != 0 {
write!(f, "Q")?;
} else {
write!(f, ".")?;
}
if self.castling_rights & 0b0001 != 0 {
write!(f, "K")?;
} else {
write!(f, ".")?;
}
}

if i == 4 {
if let Some(square) = self[Color::WHITE].en_passant {
write!(f, "   en passant (white POV): {}", square)?;
} else {
write!(f, "   en passant (white POV): None")?;
}
}
if i == 2 {
write!(f, "   halfmove clock: {}", self.halfmove_clock)?;
}

writeln!(f)?;
}
writeln!(
f,
"{}",
match self.side_to_move {
Color::WHITE => "   /\\",
Color::BLACK => "               /\\",
}
)?;
writeln!(f)
}
}

impl fmt::Display for HalfBoard {
fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
for i in (0..8).rev() {
for j in (0..8).rev() {
if let Some(piece) = self.mailbox_88[8 * i + j] {
if self[Color::WHITE].has_square((8 * i + j) as u8) {
write!(f, "{}", piece.to_char().to_uppercase().next().unwrap())?;
} else {
write!(f, "{}", piece.to_char())?;
}
} else {
write!(f, "*")?;
}
}

writeln!(f)?;
}
writeln!(f)
}
}
*/
