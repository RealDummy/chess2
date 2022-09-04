use std::vec::Vec;

use super::input;
use crate::chess::piece::{Piece, BLACK_START, WHITE_START, Player};
use crate::chess::bit_set;
pub use super::moves::{Move, MoveType, MoveSquares};
use super::generator::{PossibleMoveGenerator, SliderMasks};
use bit_set::Set;
use termion::{color};

pub fn file<N: Into<usize> + Copy + std::ops::Div + TryFrom<usize>>(square: N) -> N {
    (square.into() % 8).try_into().ok().unwrap()
}
pub fn rank<N: Into<usize> + Copy + std::ops::Div + TryFrom<usize>>(square: N) -> N {
    (square.into() / 8).try_into().ok().unwrap()
}   

pub fn adjust_square<N: TryInto<i32> + Copy + std::ops::Add + TryFrom<i32>>(square: N, file: i32, rank: i32) -> N {
   (square.try_into().ok().unwrap() + file + rank * 8).try_into().ok().unwrap()
}

pub fn adjust_square_checked<N: TryInto<i32> + Into<usize> + TryFrom<usize> + std::ops::Div + Copy + std::ops::Add + TryFrom<i32>>(square:N, adj_file:i32, adj_rank: i32) -> Option<N> {
    let f = file(square).try_into().ok()?;
    let r = rank(square).try_into().ok()?;
    let nf = adj_file + f;
    let nr = adj_rank + r;
    if nf >= 8 || nf < 0 || nr >= 8 || nr < 0 {
        return None;
    }
    let new_square: i32 = nf + nr * 8;
    match new_square {
        0..=63 => new_square.try_into().ok(),
        _ => None,
    }
}

pub struct Board {
    active: Player,
    pieces: [[Set;6]; 2],
    mailbox: Mailbox,

}

struct Mailbox {
    squares: [Option<(Player, Piece)>; 65]
}

impl Board {
    pub fn new() -> Self{
        let pieces = [WHITE_START.clone(), BLACK_START.clone()];
        Board {
            pieces,
            active: Player::White,
            mailbox: Mailbox::from(pieces),
        }
    }
    fn get_set(&self, player: Player, piece: Piece) -> Set {
        self.pieces[player.index()][piece.index()]
    }
    fn get_set_mut<'a>(&'a mut self, player: Player, piece: Piece) -> &'a mut Set{
        &mut self.pieces[player.index()][piece.index()]
    }
    // pub fn iter_pieces<'a>(&'a self) -> MailboxIter<'a> {
    //     self.mailbox.iter()
    // }
    fn draw_board(buffer: &[char; 64], player: Player) {
        println!("{}  ┏━━━━━━━━━━━━━━━━━━━━━━━━┓", color::Fg(color::White));
        let square_iter  = match player {
            Player::White => [0,1,2,3,4,5,6,7],
            Player::Black => [7,6,5,4,3,2,1,0],
        }.into_iter();
        for i in square_iter.clone() {
            let rank = match i {
                0 => '8',
                1 => '7',
                2 => '6',
                3 => '5',
                4 => '4',
                5 => '3',
                6 => '2',
                7 => '1',
                _ => ' ',
            };
            print!("{} ┃", rank);
            for j in square_iter.clone() {
                let is_black = (i%2)^(j%2) == 1;
                let spot = buffer[(i*8 + j) as usize];
                if is_black {
                    print!("{}{} {} ", color::Bg(color::Black),color::Fg(color::White), spot)
                }
                else {
                    print!("{}{} {} ", color::Bg(color::LightBlack),color::Fg(color::White), spot)
                }
            }
            println!("{}┃",color::Bg(color::Reset));
        }
        println!("  ┗━━━━━━━━━━━━━━━━━━━━━━━━┛");
        println!("    A  B  C  D  E  F  G  H ");
    }
    pub fn show(&self) {
        let mut buffer = [' '; 64];
        for (player, piece, square) in self.mailbox.iter() {  
            buffer[square] = piece.get_char(player);
        }
        Board::draw_board(&buffer, self.active);
    }
    pub fn force_move(&mut self, m: MoveSquares, gen: &PossibleMoveGenerator) -> bool {
        let (player, piece) = match self.mailbox.get(m.from) {
            Some(n) => n,
            None => {return false},
        };
        let moves = bit_set::union(gen.get_moves(player, piece, m.from), gen.get_attacks(player, piece, m.from));
        if bit_set::get(moves, m.to) == 0 {
            return false;
        }
        let set = self.get_set_mut(player, piece);
        bit_set::unset(set, m.from);
        bit_set::set(set, m.to);
        let captured = self.mailbox.force_move(m);
        true
    }
    pub fn generate_psudolegal(&self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> Vec<Move> {
        let mut res: Vec<Move> =vec![];
        let friends = self.pieces[self.active.index()].iter().fold(0,|a,&x| bit_set::union(a, x));
        let enemies = self.pieces[self.active.invert().index()].iter().fold(0,|a,&x| bit_set::union(a, x));
        let all_pieces = bit_set::union(friends, enemies);
        for (_, piece, square) in self.mailbox.iter().filter(|(p,_,_)| *p == self.active) {
            let attacks = gen.get_attacks(self.active, piece, square);
            let moves = gen.get_moves(self.active, piece, square);
            let mut all_moves = bit_set::union(attacks, moves);
            while all_moves != 0 {
                let new_square = bit_set::lsb_pos(all_moves);
                bit_set::clear_lsb(&mut all_moves);
                let nss = 1u64 << new_square;
                if bit_set::intersect(friends, nss) != 0 {
                    continue;
                }
                let s = slide.get(square, new_square);
                //bit_set::show(s);
                if bit_set::intersect(s, all_pieces) != 0 {
                    continue;
                }
                let is_capture = bit_set::intersect(enemies, nss);
                res.push(
                    match self.mailbox.get(bit_set::lsb_pos(is_capture)) {
                        Some((_, piece)) => Move{from:square as u8, to: new_square as u8, move_type: MoveType::Capture(piece)},
                        None => Move { from: square as u8, to: new_square as u8, move_type: MoveType::Quiet }
                    }
                )
            }
        }
        res
    }
    pub fn shitty_play(&mut self) {
        let gen = PossibleMoveGenerator::new();
        let slide = SliderMasks::new();
        loop {
            self.show();
            let psudo_moves = self.generate_psudolegal(&gen, &slide);
            loop {
                let s = input::get_input();
                let m = match input::read_uci(&s) {
                    Err(msg) => {
                        eprintln!("{}", msg);
                        continue;
                    }
                    Ok(m) => m,
                };
                match psudo_moves.iter().find(|&target| {
                    target.to == m.to && target.from == m.from
                } ) {
                    Some(_) => {
                        if self.force_move(m, &gen) == false {
                            println!("Invalid Move (IDFK bro)");
                            continue;
                        }

                        println!("{:?}", psudo_moves);
                    }
                    None => {println!("Invalid Move (not in psuedolegal moves)"); continue;}
                }
    
                self.active = self.active.invert();
                break;
            }
            
        }
    }
}

impl Mailbox {
    pub fn new() -> Self {
        Self {
            squares: [None; 65]
        }
    }

    pub fn iter(&self) -> MailboxIter<'_> {
        MailboxIter { curr: 0, arr: &self.squares }
    }

    pub fn from(pieces: [[Set; 6]; 2]) -> Self {
        let mut mailbox = Self::new();
        for player in Player::iter() {
            for piece in Piece::iter() {
                let set = pieces[player.index()][piece.index()];
                for i in 0..64usize {
                    if bit_set::get(set,i) != 0{
                        mailbox.squares[i] = Some((player, piece));
                    }
                }
            }
        }
        mailbox
    }

    pub fn get<N: Into<usize>>(&self, square: N) -> Option<(Player, Piece)>{
        self.squares[square.into()]
    }

    pub fn force_move(&mut self, m: MoveSquares) -> Option<(Player, Piece)> {
        let t = self.get(m.from);
        self.squares[m.from as usize] = None;
        let res = self.squares[m.to as usize];
        self.squares[m.to as usize] = t;
        res
        
    }

}

pub struct MailboxIter<'a> {
    curr: usize,
    arr: &'a [Option<(Player,Piece)>; 65],
}

impl<'a> Iterator for MailboxIter<'a> {
    type Item = (Player, Piece, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.arr[self.curr..].iter().enumerate()
            .filter_map(|(i,o)| {
                match o {
                    Some((player, piece)) => {
                        let res = Some((*player, *piece, self.curr + i));
                        self.curr += i + 1;
                        res
                    },
                    None => None
                }
        }).next()
    }
}

#[cfg(test)]
mod test {
    use crate::chess::board::adjust_square;

    #[test]
    fn file_and_rank() {
        assert!( super::file(32u8) == 0);
        assert!( super::rank(32u8) == 4);
        assert!(adjust_square(28, 1, 1) == 37);
    }
}