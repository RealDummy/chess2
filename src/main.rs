mod chess;
use chess::{
    board::{self, Board},
    piece::{Player, Piece},
    bit_set,
    bit_set::{
        Set
    },
    input,
    generator::{PossibleMoveGenerator, SliderMasks},


};
use std::env;
use log4rs;
fn main() {
    std::fs::remove_file("/Users/samwortzman/Documents/code/rust/chess2/logs/log.log");
    if let Err(e) =  log4rs::init_file("/Users/samwortzman/Documents/code/rust/chess2/log4rs.yml", Default::default()) {
        panic!("{}", e);
    }
    //let mut b = Board::new();
    //let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ");
    //let mut b = Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ");
    let args: Vec::<String> = env::args().collect();
    let fen = args.get(1);
    let mut b = match fen {
        Some(f) => Board::from_fen(f),
        None => Board::new(),
    };
    let depth = args.get(2);
    let depth = match depth {
        Some(d) => u32::from_str_radix(d, 10).ok().unwrap_or(6),
        None => 6,
    }; 
    println!("nodes searched: {:?}", b.perft(depth, &PossibleMoveGenerator::new(), &SliderMasks::new()));
    //b.shitty_play();
}
