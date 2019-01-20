#![allow(clippy::unreadable_literal)]

use board::HalfBoard;
use move_generation::ExtendedMove;
use utils::*;

// A Xoroshiro Pseudo random generator
// See: https://en.wikipedia.org/wiki/Xoroshiro128%2B
struct PRNG {
    s: [u64; 2],
}

impl PRNG {
    pub fn new() -> Self {
        PRNG {
            s: [0xdf900294d8f554a5, 0x170865df4b3201fc],
        }
    }

    pub fn sseed(&mut self, s0: u64, s1: u64) {
        self.s[0] = s0;
        self.s[1] = s1;
    }

    // Generates a pseudo random 64 bits word
    pub fn next(&mut self) -> u64 {
        let s0 = self.s[0];
        let mut s1 = self.s[1];
        let result = s0.overflowing_add(s1).0;

        s1 ^= s0;
        self.s[0] = s0.rotate_left(24) ^ s1 ^ (s1 << 16);
        self.s[1] = s1.rotate_left(37);

        result
    }
}

// Zobrist hashing

// 12 possibles pieces per square, black to move, 16 different caslting rights and 8 possible file
//    for en passant
const ZOBRIST_ARRAY_SIZE: usize = 12 * 64 + 1 + 16 + 8;

const ZOBRIST_SIDE_TO_MOVE: usize = 12 * 64;
const ZOBRIST_CASTLING_BASE_OFFSET: usize = 12 * 64 + 1;
const ZOBRIST_EN_PASSANT_BASE_OFFSET: usize = 12 * 64 + 1 + 16;

// Static initialisation for the Zobrist hashing
lazy_static! {
    static ref zobrist_consts: [u64; ZOBRIST_ARRAY_SIZE] = generate_zobrist_consts();
}

fn generate_zobrist_consts() -> [u64; ZOBRIST_ARRAY_SIZE] {
    let mut consts = [0; ZOBRIST_ARRAY_SIZE];
    let mut prng = PRNG::new();
    for value in consts.iter_mut() {
        *value = prng.next();
    }

    consts
}

// Gets the zobrist constant for the given piece and color on the given square
fn zobrist_const_index(square: Square, piece: Piece, color: Color) -> usize {
    square.0 as usize * (6 * color as usize + piece as usize)
}

#[derive(Copy, Clone)]
pub struct ZobristHasher {
    zobrist_key: u64,
    zobrist_pawn_key: u64,
}

impl ZobristHasher {
    // Initialize the zobrist keys for the given position (from White POV)
    // The zobrist pawn key is updated only by pawns position (maybe change this in the future)
    pub fn new(position: &HalfBoard, side_to_move: Color, castling_rights: u8) -> Self {
        // We reset the zobrist key
        let mut zobrist_key = 0;
        let mut zobrist_pawn_key = 0;
        // For all squares we set the given piece
        // We use White POV
        for i in 0..64 {
            let square = Square::new(i);
            // If there is a piece on the given square
            if let Some(piece) = position[square] {
                // We look at its color and we update the key
                let color = if position[Color::WHITE].has_square(square) {
                    Color::WHITE
                } else {
                    Color::BLACK
                };
                zobrist_key ^= zobrist_consts[zobrist_const_index(square, piece, color)];
                if piece == Piece::PAWN {
                    zobrist_pawn_key ^=
                        zobrist_consts[zobrist_const_index(square, Piece::PAWN, color)];
                }
            }
        }

        // Side to move
        if side_to_move == Color::BLACK {
            zobrist_key ^= zobrist_consts[ZOBRIST_SIDE_TO_MOVE];
        }
        // Castling rights
        zobrist_key ^= zobrist_consts[ZOBRIST_CASTLING_BASE_OFFSET + castling_rights as usize];
        // En passant file
        if let Some(square) = position.en_passant {
            zobrist_key ^= zobrist_consts[ZOBRIST_EN_PASSANT_BASE_OFFSET + square.file() as usize];
        }

        ZobristHasher {
            zobrist_key,
            zobrist_pawn_key,
        }
    }

    // Updating helpers
    pub fn update_piece_capture_zobrist_key(
        &mut self,
        square: Square,
        piece: Piece,
        color: Color,
        player_color: Color,
    ) {
        // Depending on the player color POV we may have to transpose the square and color
        match player_color {
            Color::WHITE => {
                self.zobrist_key ^= zobrist_consts[zobrist_const_index(square, piece, color)]
            }
            Color::BLACK => {
                self.zobrist_key ^= zobrist_consts
                    [zobrist_const_index(square.transpose(), piece, color.transpose())]
            }
        }
    }

    pub fn update_piece_move_zobrist_key(
        &mut self,
        from: Square,
        to: Square,
        piece: Piece,
        color: Color,
        player_color: Color,
    ) {
        // Depending on the player color POV we may have to transpose the square and color
        match player_color {
            Color::WHITE => {
                self.zobrist_key ^= zobrist_consts[zobrist_const_index(from, piece, color)];
                self.zobrist_key ^= zobrist_consts[zobrist_const_index(to, piece, color)]
            }
            Color::BLACK => {
                self.zobrist_key ^=
                    zobrist_consts[zobrist_const_index(from.transpose(), piece, color.transpose())];
                self.zobrist_key ^=
                    zobrist_consts[zobrist_const_index(to.transpose(), piece, color.transpose())]
            }
        }
    }

    pub fn update_pawn_capture_zobrist_key(&mut self, square: Square, color: Color, pov: Color) {
        match pov {
            Color::WHITE => {
                self.zobrist_pawn_key ^=
                    zobrist_consts[zobrist_const_index(square, Piece::PAWN, color)]
            }
            Color::BLACK => {
                self.zobrist_pawn_key ^= zobrist_consts
                    [zobrist_const_index(square.transpose(), Piece::PAWN, color.transpose())]
            }
        }
    }

    pub fn update_pawn_move_zobrist_key(
        &mut self,
        from: Square,
        to: Square,
        color: Color,
        pov: Color,
    ) {
        match pov {
            Color::WHITE => {
                self.zobrist_pawn_key ^=
                    zobrist_consts[zobrist_const_index(from, Piece::PAWN, color)];
                self.zobrist_pawn_key ^= zobrist_consts[zobrist_const_index(to, Piece::PAWN, color)]
            }
            Color::BLACK => {
                self.zobrist_pawn_key ^= zobrist_consts
                    [zobrist_const_index(from.transpose(), Piece::PAWN, color.transpose())];
                self.zobrist_pawn_key ^= zobrist_consts
                    [zobrist_const_index(to.transpose(), Piece::PAWN, color.transpose())]
            }
        }
    }

    pub fn update_en_passant_zobrist_key(&mut self, ext_mov: ExtendedMove) {
        // Reset the old en passant file
        if let Some(old_en_passant_square) = ext_mov.get_en_passant_target() {
            self.zobrist_key ^= zobrist_consts
                [ZOBRIST_EN_PASSANT_BASE_OFFSET + old_en_passant_square.file() as usize];
        }
        if let Some(square) = ext_mov.get_raw_move().get_en_passant_target_square() {
            // Set the new en passant file
            self.zobrist_key ^=
                zobrist_consts[ZOBRIST_EN_PASSANT_BASE_OFFSET + square.file() as usize];
        }
    }

    pub fn update_castling_rights_zobrist_key(
        &mut self,
        old_caslting_rights: u8,
        new_castling_rights: u8,
    ) {
        if old_caslting_rights != new_castling_rights {
            self.zobrist_key ^=
                zobrist_consts[ZOBRIST_CASTLING_BASE_OFFSET + old_caslting_rights as usize];
            self.zobrist_key ^=
                zobrist_consts[ZOBRIST_CASTLING_BASE_OFFSET + new_castling_rights as usize];
        }
    }

    pub fn update_side_to_move_zobrist_key(&mut self) {
        self.zobrist_key ^= zobrist_consts[ZOBRIST_SIDE_TO_MOVE];
    }
}
