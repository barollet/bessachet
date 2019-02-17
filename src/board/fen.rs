use board::*;

impl Board {
    /// Parse the given fen string into a `Board`.
    /// The error handling is still very poor.
    // TODO proper parser with nice error handling
    pub fn from_fen(fen_string: &str) -> Result<Self, &'static str> {
        let fen_parts: Vec<_> = fen_string.split_whitespace().collect();
        if fen_parts.len() < 4 || fen_parts.len() > 6 {
            return Err("Too many FEN parts");
        }

        // Parsing position
        let piece_lines: Vec<_> = fen_parts[0].split('/').collect();
        if piece_lines.len() != 8 {
            return Err("Too many board lines");
        }
        // Starting from the bottom line in white's perspective
        let mut occupancy: BlackWhiteAttribute<BitBoard> =
            BlackWhiteAttribute::new(BBWrapper::empty(), BBWrapper::empty());
        let mut piece_array = [BBWrapper::empty(); 6];
        for (piece_line, i) in piece_lines.iter().rev().zip(0u32..) {
            let mut pos = 8;
            for c in piece_line.chars() {
                if let Some(offset) = c.to_digit(10) {
                    pos -= offset;
                } else {
                    pos -= 1;

                    let singly_populated_bitboard = 1 << (8 * i + pos);
                    // Piece bitboard
                    let new_piece = match c.to_ascii_lowercase() {
                        'p' => Piece::PAWN,
                        'n' => Piece::KNIGHT,
                        'b' => Piece::BISHOP,
                        'r' => Piece::ROOK,
                        'q' => Piece::QUEEN,
                        'k' => Piece::KING,
                        _ => return Err("Cannot parse piece"),
                    };
                    piece_array[new_piece as usize] |= singly_populated_bitboard;

                    // Occupancy
                    let color = if c.is_ascii_lowercase() { BLACK } else { WHITE };
                    occupancy[color] |= singly_populated_bitboard;
                }
            }
        }

        // En passant
        let en_passant = NonZeroSquare::new(if fen_parts[3].len() == 2 {
            let chars: Vec<_> = fen_parts[3].chars().collect();
            SqWrapper::from_char_file_rank(chars[0], chars[1])
        } else {
            0
        });

        // castling
        let mut castling_rights = 0;
        if fen_parts[2] != "-" {
            for c in fen_parts[2].chars() {
                castling_rights |= match c {
                    'K' => 0x1,
                    'Q' => 0x2,
                    'k' => 0x4,
                    'q' => 0x8,
                    _ => return Err("Cannot parse castling rights"),
                }
            }
        }

        let side_to_move = match fen_parts[1].chars().next() {
            Some('w') => WHITE,
            Some('b') => BLACK,
            _ => return Err("Cannot parse side to move"),
        };

        // TODO halfmove clock and ply

        let position = Position {
            pieces: piece_array,
            occupancy,
            en_passant,
            castling_rights,
            side_to_move,
        };
        Ok(Board::init_from_position(position, 0, 0))
    }
}
