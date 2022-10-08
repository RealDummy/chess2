mod moves;
mod bit_set;
mod board;
mod piece;
mod input;
mod generator;
mod hash;
mod transpositition;


use generator::{
    PossibleMoveGenerator,
    SliderMasks,
};
use board::Board;
use moves::Move;
use hash::{GameHasher};
use self::{board::{MoveType}, piece::Piece, hash::HashT};
use transpositition::TTable;

pub struct Game {
    gen: PossibleMoveGenerator,
    slide: SliderMasks,
    board: Board,
    last_move: Option<Move>,
    hasher: GameHasher,
    table: TTable,
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
        let hasher = GameHasher::new();
        Self {
            gen: generator::PossibleMoveGenerator::new(),
            slide: SliderMasks::new(),
            board: Board::new(&hasher),
            last_move: None,
            hasher,
            table: TTable::new(),
        }
    }
    pub fn from_fen(fen: &str) -> Self {
        let hasher = GameHasher::new();
        Self {
            gen: generator::PossibleMoveGenerator::new(),
            slide: SliderMasks::new(),
            board: Board::from_fen(fen, &hasher),
            last_move: None,
            hasher,
            table: TTable::new(),
        }
    }
    pub fn fen(&self) -> String {
        self.board.to_fen()
    }
    pub fn perft(&self, n: u32) -> u64 {
        let res = self.board.perft(n, &self.gen, &self.slide, &self.hasher);
        println!("Nodes searched: {res}");
        res
    }

    fn get_legal_moves<'a>(&'a mut self) -> Vec<Move> {
        self.board.generate_legal(&self.gen, &self.slide)
    }

    fn make_move(&mut self, m: &Move) {
        self.board.make_move(m, &self.hasher);
        self.last_move = Some(m.clone());   
    }

    pub fn try_move(&mut self, try_move: &str) -> MoveResult {
        let input_move = match input::read_uci(try_move) {
            Ok(m) => m,
            Err(msg) => {
                eprintln!("{}", msg);
                return MoveResult::InvalidInput
            }
        };
        let valid_moves = self.get_legal_moves();
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
        self.make_move(&m);
        //let next_moves = self.board.generate_legal(&self.gen, &self.slide);
        let res = match self.get_legal_moves().len() {
            0 => match self.board.in_check(&self.gen, &self.slide) {
                false => MoveResult::Draw,
                true  => MoveResult::Win,
            }
            _ => MoveResult::NextTurn,
        };
        res
    }

    fn quiesce(&self, board: &mut Board, mut alpha: f32, beta: f32) -> f32 {
        let standing_eval = board.eval();
        if standing_eval >= beta {
            return beta;
        }
        if alpha < standing_eval {
            alpha = standing_eval;
        }
        for capture in board.generate_legal_captures(&self.gen, &self.slide) {
            let mut b2 = board.clone();
            b2.make_move(&capture, &self.hasher);
            let new_eval = -self.quiesce(&mut b2, -beta, -alpha);
            if new_eval >= beta {
                return beta;
            }
            if new_eval > alpha {
                alpha = new_eval;
            }
        }
        alpha
    }

    fn alpha_beta(&mut self, board: &mut Board, mut alpha: f32, beta: f32, depth_left: i32) -> f32 {
        if depth_left == 0 {
            return self.quiesce(board, alpha, beta);
        }
        for m in board.generate_legal(&self.gen, &self.slide) {
            let mut b2 = board.clone();
            b2.make_move(&m, &self.hasher);
            let score = -self.alpha_beta(&mut b2, -beta, -alpha, depth_left - 1);
            if score >= beta {
                return beta;
            }
            if score > alpha {
                alpha = score;
            }
        }
        alpha
    }
    pub fn root_search(&mut self, depth: i32) -> (Move, f32) {
        assert!(depth >= 1);
        let mut alpha = f32::NEG_INFINITY;
        let mut best_move = None;
        for m in self.board.generate_legal(&self.gen, &self.slide) {
            let mut b2 = self.board.clone();
            b2.make_move(&m, &self.hasher);
            let score = -self.alpha_beta(&mut b2, f32::NEG_INFINITY, -alpha, depth - 1);
            if score > alpha {
                alpha = score;
                best_move = Some(m);
            }
        }
        (best_move.unwrap(), alpha)
    }
    pub fn make_best_move(&mut self) {
        let (m, a) = self.root_search(5);
        self.make_move(&m);
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