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
fn main() {
    let mut b = Board::new();
    b.shitty_play();
}
