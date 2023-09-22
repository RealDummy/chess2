mod moves;
mod bit_set;
mod board;
mod piece;
mod input;
mod generator;
mod hash;
mod transpositition;
mod piece_square;


use generator::{
    PossibleMoveGenerator,
    SliderMasks,
};
use board::Board;
use moves::Move;
use hash::GameHasher;
use self::{board::{MoveType, EvalT, Evaluation, MoveSquares, HashT}, piece::Piece};
use transpositition::{TTable, TTableNode, Score};

use std::time::{Duration, Instant};

pub struct Game {
    gen: PossibleMoveGenerator,
    slide: SliderMasks,
    board: Board,
    last_move: Option<Move>,
    hasher: GameHasher,
    table: TTable,
    history: Vec<Board>,
    
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
            history: Vec::new(),
        }
    }
    pub fn from_fen(fen: &str) -> Result<Self, &'static str> {
        let hasher = GameHasher::new();
        Ok(Self {
            gen: generator::PossibleMoveGenerator::new(),
            slide: SliderMasks::new(),
            board: Board::from_fen(fen, &hasher)?,
            last_move: None,
            hasher,
            table: TTable::new(),
            history: Vec::new(),
        })
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
        self.history.push(self.board.clone());
        self.board.make_move(m, &self.hasher);
        self.last_move = Some(m.clone());   
    }
    fn is_probable_repetition(board: &Board, history: &Vec<HashT>) -> bool {
        let pos_count = history.iter().fold(0, |a,&e| {
            if e == board.get_hash() {
                a + 1
            }
            else {
                a
            }
        }); 
        pos_count >= 2
    }
    fn is_repetition(&self) -> bool {
        let pos_count = self.history.iter().fold(0, |a,e| {
            if e == &self.board {
                a + 1
            }
            else {
                a
            }
        });
        pos_count >= 2
    }
    pub fn read_uci(&self, uci: &str) -> Result<MoveSquares, String>{
        match input::read_uci(uci) {
            Ok(m) => Ok(m),
            Err(msg) => {
                Err(msg)
            }
        }
    }
    pub fn try_move(&mut self, try_move: &MoveSquares) -> MoveResult {
        
        let valid_moves = self.get_legal_moves();
        let found = valid_moves.iter().find(|&m| {
            m.to == try_move.to && 
            m.from == try_move.from &&
            match m.move_type {
                MoveType::CaptureAndPromotion(_, promote) => {
                    promote == match try_move.promote {
                        None => Piece::Queen,
                        Some(p) => p,
                    }
                },
                MoveType::Promotion(promote) => {
                    promote == match try_move.promote {
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
        match &m.move_type {
            MoveType::Capture(_) |
            MoveType::CaptureAndPromotion(_, _) |
            MoveType::PawnPush |
            MoveType::EnPassent => {
                self.history.clear();
            }
            _ => ()
        };
        self.make_move(&m);
        //let next_moves = self.board.generate_legal(&self.gen, &self.slide);
        let res = match self.get_legal_moves().len() {
            0 => match self.board.in_check(&self.gen, &self.slide) {
                false => MoveResult::Draw,
                true  => MoveResult::Win,
            }
            _ => match self.is_repetition() {
                true => MoveResult::Draw,
                false => MoveResult::NextTurn,
            }
        };
        res
    }

    fn quiesce(&self, board: &mut Board, mut alpha: EvalT, mut beta: EvalT, depth: u8) -> Evaluation {
        let node = self.table.get(board.get_hash());
        if let Some(data) = node {
            if depth <= data.ply {
                match data.eval {
                    Score::Exact(s) => {
                        return  s;
                    },
                    Score::LowerBound(s) => {
                        alpha = match data.is_odd {
                            true => -s,
                            false => s,
                        };                    },
                    Score::UpperBound(s) => {
                        beta = match data.is_odd {
                            true => -s,
                            false => s
                        }
                    }
                }
            }
        }
        let standing_eval = board.eval();
        if standing_eval >= beta {
            return Evaluation::Value(beta);
        }
        if depth > 3 {
            return Evaluation::Value(standing_eval);
        }
        if standing_eval > alpha {
            alpha = standing_eval;
        }
        for capture in board.generate_legal_captures(&self.gen, &self.slide) {
            let mut b2 = board.clone();
            b2.make_move(&capture, &self.hasher);
            let inverse_eval = self.quiesce(&mut b2, -beta, -alpha, depth + 1);
            let new_eval = match inverse_eval {
                Evaluation::MateWin(t) => Evaluation::MateLoss(t+1),
                Evaluation::MateLoss(t) => Evaluation::MateWin(t+1),
                Evaluation::Value(v) => Evaluation::Value(-v),
                Evaluation::Draw => Evaluation::Draw,
            };
            let new_eval= match new_eval {
                Evaluation::MateWin(_) => EvalT::MAX,
                Evaluation::MateLoss(_) => EvalT::MIN + 1,
                Evaluation::Draw => 0,
                Evaluation::Value(v) => v,
            };
            if new_eval >= beta {
                return Evaluation::Value(beta);
            }
            if new_eval > alpha {
                alpha = new_eval;
            }
        }
        Evaluation::Value(alpha)
    }

    fn search_best(&mut self, board: &mut Board, mut alpha: EvalT, mut beta: EvalT, depth: u8, max_depth: u8, history: &mut Vec<HashT>) -> (Option<Move>, Evaluation) {
        let node = self.table.get(board.get_hash());
        if let Some(data) = node {
            if max_depth - depth <= data.ply {
                match data.eval {
                    Score::Exact(s) => {
                        return (data.best.clone(), s);
                    },
                    Score::LowerBound(s) => {
                        alpha = match data.is_odd {
                            true => -s,
                            false => s,
                        };                    },
                    Score::UpperBound(s) => {
                        beta = match data.is_odd {
                            true => -s,
                            false => s
                        }
                    }
                }
            }
        }
        if depth == max_depth {
            let eval = self.quiesce(board, alpha, beta, 0);
            //self.table.insert(board.get_hash(), TTableNode { depth, eval: Score::Exact(eval), best: None});
            return (None, eval);
        }
        let is_odd = match depth % 2 {
            1 => true,
            _ => false,
        };
        let mut best_move = None;
        let legal_moves = board.generate_legal(&self.gen, &self.slide);
        if legal_moves.is_empty() {
            return match board.in_check(&self.gen, &self.slide) {
                true => (None, Evaluation::MateLoss(0)),
                false => (None, Evaluation::Draw)
            }
        }
        for m in legal_moves {
            let mut b2 = board.clone();
            b2.make_move(&m, &self.hasher);
            history.push(b2.get_hash());
            let (best, inverse_score) = self.search_best(&mut b2, -beta, -alpha, depth + 1, max_depth, history);
            history.pop();
            
            let score = (|| {
                    if Game::is_probable_repetition(&b2, history) {
                        return Evaluation::Draw;
                    }
                    match inverse_score {
                        Evaluation::MateWin(t) => Evaluation::MateLoss(t+1),
                        Evaluation::MateLoss(t) => Evaluation::MateWin(t+1),
                        Evaluation::Value(v) => Evaluation::Value(-v),
                        Evaluation::Draw => Evaluation::Draw,
                }
            })();

            let score = match score {
                Evaluation::MateWin(d) => {
                    EvalT::MAX - (d as i16)
                },
                Evaluation::MateLoss(_) => {
                    self.table.insert(  b2.get_hash(), TTableNode {ply: max_depth - depth - 1, eval: Score::UpperBound(beta), best, is_odd});
                    return (None, Evaluation::Value(beta));
                }
                Evaluation::Draw => 0,
                Evaluation::Value(v) => v,
            };
            if score >= beta {
                self.table.insert(  b2.get_hash(), TTableNode {ply: max_depth - depth - 1, eval: Score::UpperBound(beta), best, is_odd});
                return (None, Evaluation::Value(beta));
            }
            else if score > alpha {
                alpha = score;
                best_move = Some(m);
            }
            else {
                self.table.insert(b2.get_hash(), TTableNode {ply: max_depth - depth - 1, eval: Score::LowerBound(alpha), best, is_odd});
            }
        }
        self.table.insert(board.get_hash(), TTableNode { ply: max_depth - depth, eval: Score::Exact(Evaluation::Value(alpha)), best: best_move.clone(), is_odd});
        (best_move, Evaluation::Value(alpha))
    }
    
    pub fn find_best_move(&mut self, min_duration: Duration) -> MoveSquares {
        let start = Instant::now();
        let mut depth = 2;
        let mut best = None;
        let mut history = self.history.iter().map(|e| e.get_hash()).collect();
        loop  {
            let (m, a) = self.search_best(&mut self.board.clone(), EvalT::MIN + 1, EvalT::MAX, 0, depth, &mut history);
            best = match m {
                None => best,
                s => s,
            };
            depth += 1;
            if Instant::now() - start > min_duration {
                break;
            }
        }
        match best {
            Some(best) => MoveSquares {
                to: best.to,
                from: best.from,
                promote: match best.move_type {
                    MoveType::CaptureAndPromotion(_, p) => Some(p),
                    MoveType::Promotion(p) => Some(p),
                    _ => None,
                }
            },
            None => panic!("no moves found!"),
        }
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