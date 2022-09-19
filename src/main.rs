mod chess;
use chess::{
    board::{self, Board},
    piece::{Player, Piece},
    bit_set,
    bit_set::{
        Set
    },
    input,
    generator::PossibleMoveGenerator,

};
use log4rs;
fn main() {
    std::fs::remove_file("/Users/samwortzman/Documents/code/rust/chess2/logs/log.log");
    if let Err(e) =  log4rs::init_file("./log4rs.yml", Default::default()) {
        panic!("{}", e);
    }
    let mut b = Board::new();
    b.shitty_play();
}
