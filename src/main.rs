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
    let gen = PossibleMoveGenerator::new();

    loop {
        let board::MoveSquares{
            to,
            from,
            promote,
        } = match input::read_uci(&input::get_input()) {
            Ok(n) => n,
            Err(msg) => {
                println!("invalid UCI \"{}\"", msg);
                continue;
            }
        };
        println!("{} {}", to, from);
        let a = gen.get_attacks(Player::White, Piece::Pawn, to);
        let b = gen.get_moves(Player::Black, Piece::Rook, from);
        bit_set::show(a);
        bit_set::show(b);
    }
}
