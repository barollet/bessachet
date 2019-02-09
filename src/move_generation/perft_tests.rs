// This is a module for perft results testing
// All perft results and positions are taken from
// https://www.chessprogramming.org/Perft_Results

use board::Board;

use move_generation::Move;

use utils::*;

fn internal_perft(board: &mut Board, depth: u8, start_depth: u8) -> usize {
    let generator = board.create_legal_move_generator();
    if depth == 1 {
        return generator.number_of_legal_moves();
    }
    generator.fold(0, |acc, mov| {
        let ext_mov = board.make(mov);

        let partial_sum = internal_perft(board, depth - 1, start_depth);
        if depth == start_depth {
            println!(
                "{}: {}",
                if board.side_to_move == Color::WHITE {
                    Move::from(mov)
                } else {
                    Move::from(mov).transpose()
                },
                partial_sum
            );
        }

        board.unmake(ext_mov);
        acc + partial_sum
    })
}

// Runs a perft test of the given depth on the given board
fn perft(board: &mut Board, depth: u8) -> usize {
    assert!(depth > 0);
    internal_perft(board, depth, depth)
}

#[test]
fn perft_initial_position() {
    let mut board = Board::initial_position();

    let init_key = board.zobrist_hasher.zobrist_key;

    assert_eq!(perft(&mut board, 6), 119_060_324);
    //assert_eq!(perft(&mut board, 5), 4_865_609);
    //assert_eq!(perft(&mut board, 4), 197_281);
    //assert_eq!(perft(&mut board, 3), 8902);
    //assert_eq!(perft(&mut board, 2), 400);
    //assert_eq!(perft(&mut board, 1), 20);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);
}

#[test]
fn perft_kiwipete() {
    let mut board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ")
            .unwrap();

    let init_key = board.zobrist_hasher.zobrist_key;

    assert_eq!(perft(&mut board, 5), 193_690_690);
    //assert_eq!(perft(&mut board, 4), 4_085_603);
    //assert_eq!(perft(&mut board, 3), 97862);
    //assert_eq!(perft(&mut board, 2), 2039);
    //assert_eq!(perft(&mut board, 1), 48);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);
}

#[test]
fn perft_sparse_board() {
    let mut board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();

    let init_key = board.zobrist_hasher.zobrist_key;

    assert_eq!(perft(&mut board, 7), 178_633_661);
    //assert_eq!(perft(&mut board, 6), 11_030_083);
    //assert_eq!(perft(&mut board, 5), 674_624);
    //assert_eq!(perft(&mut board, 4), 43_238);
    //assert_eq!(perft(&mut board, 3), 2812);
    //assert_eq!(perft(&mut board, 2), 191);
    //assert_eq!(perft(&mut board, 1), 14);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);
}

#[test]
fn perft_mirror() {
    let mut board =
        Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
            .unwrap();

    let init_key = board.zobrist_hasher.zobrist_key;

    assert_eq!(perft(&mut board, 5), 15_833_292);
    //assert_eq!(perft(&mut board, 4), 422_333);
    //assert_eq!(perft(&mut board, 3), 9467);
    //assert_eq!(perft(&mut board, 2), 264);
    //assert_eq!(perft(&mut board, 1), 6);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);

    let mut board =
        Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
            .unwrap();

    let init_key = board.zobrist_hasher.zobrist_key;

    assert_eq!(perft(&mut board, 5), 15_833_292);
    //assert_eq!(perft(&mut board, 4), 422_333);
    //assert_eq!(perft(&mut board, 3), 9467);
    //assert_eq!(perft(&mut board, 2), 264);
    //assert_eq!(perft(&mut board, 1), 6);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);
}

#[test]
fn perft_talkchess() {
    let mut board =
        Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();

    let init_key = board.zobrist_hasher.zobrist_key;

    assert_eq!(perft(&mut board, 5), 89_941_194);
    //assert_eq!(perft(&mut board, 4), 2_103_487);
    //assert_eq!(perft(&mut board, 3), 62_379);
    //assert_eq!(perft(&mut board, 2), 1486);
    //assert_eq!(perft(&mut board, 1), 44);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);
}

#[test]
fn perft_edwards() {
    let mut board =
        Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")
            .unwrap();

    let init_key = board.zobrist_hasher.zobrist_key;

    //assert_eq!(perft(&mut board, 5), 164_075_551);
    //assert_eq!(perft(&mut board, 4), 3_894_594);
    assert_eq!(perft(&mut board, 3), 89_890);
    //assert_eq!(perft(&mut board, 2), 2079);
    //assert_eq!(perft(&mut board, 1), 46);

    assert_eq!(init_key, board.zobrist_hasher.zobrist_key);
}

// TODO do a real benchmark with a lot more perft files
#[test]
#[ignore]
fn perft_bench() {
    // Initial position
    let mut board = Board::initial_position();

    assert_eq!(perft(&mut board, 7), 3_195_901_860);

    // Kiwipite
    let mut board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ")
            .unwrap();

    assert_eq!(perft(&mut board, 6), 8_031_647_685);

    // Sparse board
    let mut board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();

    assert_eq!(perft(&mut board, 8), 3_009_794_393);

    // Mirror
    let mut board =
        Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
            .unwrap();

    assert_eq!(perft(&mut board, 6), 706_045_033);

    let mut board =
        Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
            .unwrap();

    assert_eq!(perft(&mut board, 6), 706_045_033);

    // Edwards
    let mut board =
        Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")
            .unwrap();

    assert_eq!(perft(&mut board, 6), 6_923_051_137);
}

// Some pin test before legal move generation works
// checks are not really tested though
#[test]
fn check_pin_test() {
    // Initial position
    let board = Board::initial_position();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 0);
    assert_eq!(generator.number_of_checkers, 0);

    // Kiwipite
    let board =
        Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ")
            .unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 0);
    assert_eq!(generator.number_of_checkers, 0);

    // Sparse
    let board = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 1);
    assert_eq!(generator.number_of_checkers, 0);

    // Mirror
    let board = Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
        .unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 0);
    assert_eq!(generator.number_of_checkers, 1);

    let board = Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
        .unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 0);
    assert_eq!(generator.number_of_checkers, 1);

    // Talkchess
    let board =
        Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 0);
    assert_eq!(generator.number_of_checkers, 0);

    // Edwards
    let board =
        Board::from_fen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")
            .unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 1);
    assert_eq!(generator.number_of_checkers, 0);

    // Impossible position overpinned
    let board = Board::from_fen("7k/1b5b/2Q3P1/8/2rBKRr1/5P2/6q1/8 w KQkq -").unwrap();
    let generator = board.create_legal_move_generator();
    assert_eq!(generator.number_of_pinned_pieces, 5);
    assert_eq!(generator.number_of_checkers, 0);
}
