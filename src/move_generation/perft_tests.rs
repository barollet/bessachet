// This is a module for perft results testing
// All perft results and positions are taken from
// https://www.chessprogramming.org/Perft_Results

use board::Board;

use move_generation::init_magic_tables;

use utils::*;

use move_generation::*;

// Runs a perft test of the given depth on the given board
fn perft(board: &mut Board, depth: u8) -> usize {
    // TODO Bulk gen when legal moves are ready
    if depth == 0 {
        return 1;
    }
    let moves: Vec<_> = board.possible_moves().collect();
    let mut sum = 0;
    for mov in moves {
        board.make(mov);

        if !board.is_king_checked() {
            sum += perft(board, depth-1);
            if depth == 6 {
                println!("{} {}: {}", mov, mov.transpose(), perft(board, depth-1));
                //println!("{}", board);
            }
        }

        board.unmake(mov);
    }

    sum
}

#[test]
#[ignore]
fn perft_initial_position() {
    init_magic_tables();

    let mut board = Board::initial_position();

    //board.make(Move::quiet_move(Square::from_char_rank_file('d', '2'), Square::from_char_rank_file('d', '3')));
    //board.make(Move::quiet_move(Square::from_char_rank_file('h', '2'), Square::from_char_rank_file('h', '3')));
    //board.make(Move::quiet_move(Square::from_char_rank_file('g', '2'), Square::from_char_rank_file('g', '3')));
    //board.make(Move::quiet_move(Square::from_char_rank_file('c', '2'), Square::from_char_rank_file('c', '3')));
    //board.make(Move::quiet_move(Square::from_char_rank_file('f', '2'), Square::from_char_rank_file('f', '3')));
    //board.make(Move::quiet_move(Square::from_char_rank_file('h', '4'), Square::from_char_rank_file('h', '5')));

    println!("{}", board);

    board.debug_move_counts();

    for mov in board.possible_moves() {
        println!("{} {}", mov, mov.transpose());
    }

    assert_eq!(perft(&mut board, 6), 119_060_324);
    //assert_eq!(perft(&mut board, 5), 4_865_609);
    //assert_eq!(perft(&mut board, 4), 197_281);
    //assert_eq!(perft(&mut board, 3), 8902);
    //assert_eq!(perft(&mut board, 2), 400);
    //assert_eq!(perft(&mut board, 1), 20);
}
