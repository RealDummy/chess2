mod moves;
mod bit_set;
mod board;
mod piece;
mod input;
mod generator;

use generator::{
    PossibleMoveGenerator,
    SliderMasks,
};
use board::Board;
use moves::Move;

use self::{board::{MoveType}, piece::Piece};

pub struct Game {
    gen: PossibleMoveGenerator,
    slide: SliderMasks,
    legal_moves: Option<Vec<Move>>,
    board: Board,
    last_move: Option<Move>
}

pub enum MoveResult {
    Win,
    Draw,
    NextTurn,
    InvalidMove,
    InvalidInput,
}
pub use piece::Player;
impl Game {
    pub fn new() -> Self {
        Self {
            gen: generator::PossibleMoveGenerator::new(),
            slide: SliderMasks::new(),
            legal_moves: None,
            board: Board::new(),
            last_move: None,
        }
    }
    pub fn from_fen(fen: &str) -> Self {
        Self {
            gen: generator::PossibleMoveGenerator::new(),
            slide: SliderMasks::new(),
            legal_moves: None,
            board: Board::from_fen(fen),
            last_move: None,
        }
    }
    pub fn fen(&self) -> String {
        self.board.to_fen()
    }
    pub fn perft(&self, n: u32) -> u64 {
        let res = self.board.perft(n, &self.gen, &self.slide);
        println!("Nodes searched: {res}");
        res
    }
    pub fn try_move(&mut self, try_move: &str) -> MoveResult {
        let input_move = match input::read_uci(try_move) {
            Ok(m) => m,
            Err(msg) => {
                eprintln!("{}", msg);
                return MoveResult::InvalidInput
            }
        };
        let valid_moves = match &self.legal_moves {
            None => {
                let lm = self.board.generate_legal(&self.gen, &self.slide);
                self.legal_moves = Some(lm);
                self.legal_moves.as_ref().unwrap()
            }
            Some(lm) => lm
        };
        let found = valid_moves.iter().find(|&m| {
            m.to == input_move.to && 
            m.from == input_move.from &&
            match m.move_type {
                MoveType::CaptureAndPromotion(_, promote) => {
                    promote == match input_move.promote {
                        None => Piece::Queen,
                        Some(p) => p,
                    }
                },
                MoveType::Promotion(promote) => {
                    promote == match input_move.promote {
                        None => Piece::Queen,
                        Some(p) => p,
                    }
                },
                _ => true
            }
        });
        let m = match found {
            Some(n) => n.clone(),
            None => {
                return MoveResult::InvalidMove;
            }
        };
        self.board.make_move(&m);
        self.last_move = Some(m);
        let next_moves = self.board.generate_legal(&self.gen, &self.slide);
        let res = match next_moves.len() {
            0 => match self.board.in_check(&self.gen, &self.slide) {
                false => MoveResult::Draw,
                true  => MoveResult::Win,
            }
            _ => MoveResult::NextTurn,
        };
        self.legal_moves = Some(next_moves);
        res
    }
    pub fn show(&self) {
        let square = match &self.last_move {
            Some(m) => Some(m.to),
            None => None
        };
        self.board.show(square);
    }
    pub fn active_player(&self) -> Player {
        self.board.active_player()
    }
}