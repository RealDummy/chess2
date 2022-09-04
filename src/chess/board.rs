

use super::input;
use crate::chess::piece::{Piece, BLACK_START, WHITE_START, Player};
use crate::chess::bit_set;
pub use super::moves::MoveSquares;
use super::generator::PossibleMoveGenerator;
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
    pub pieces: [[Set;6]; 2],
    pub mailbox: Mailbox,

}

pub struct Mailbox {
    squares: [Option<(Player, Piece)>; 64]
}

impl Board {
    pub fn new() -> Self{
        let pieces = [WHITE_START.clone(), BLACK_START.clone()];
        Board {
            pieces,
            mailbox: Mailbox::from(pieces),
        }
    }
    fn get_set(&self, player: Player, piece: Piece) -> Set {
        self.pieces[player.index()][piece.index()]
    }
    fn get_set_mut<'a>(&'a mut self, player: Player, piece: Piece) -> &'a mut Set{
        &mut self.pieces[player.index()][piece.index()]
    }
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
    pub fn show(&self, player: Player) {
        let mut buffer = [' '; 64];
        for (player, piece, square) in self.mailbox.iter() {  
            println!("{}", square); 
            buffer[square] = piece.get_char(player);
        }
        Board::draw_board(&buffer, player);
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
    pub fn shitty_play(&mut self) {
        let gen = PossibleMoveGenerator::new();
        let mut player = Player::White;
        loop {
            self.show(player);
            loop {
                let s = input::get_input();
                let m = match input::read_uci(&s) {
                    Err(msg) => {
                        eprintln!("{}", msg);
                        continue;
                    }
                    Ok(m) => m,
                };
                let friends = self.pieces[player.index()].iter().fold(0, |a,&x| bit_set::union(a, x));
                let enemies = self.pieces[player.invert().index()].iter().fold(0, |a,&x| bit_set::union(a, x));
                if bit_set::get(friends, m.to) != 0 {
                    println!("Invalid Move");
                    continue;
                }
                if self.force_move(m, &gen) == false {
                    println!("Invalid Move");
                    continue;
                }
                player = player.invert();
                break;
            }
            
        }
    }
}

impl Mailbox {
    pub fn new() -> Self {
        Self {
            squares: [None; 64]
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
    arr: &'a [Option<(Player,Piece)>; 64],
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