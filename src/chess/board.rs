use std::vec::Vec;

use super::bit_set::{union, intersect, count};
use super::input;
use crate::chess::piece::{Piece, BLACK_START, WHITE_START, Player, SpecefiedPiece};
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
#[derive(Clone, Copy, Debug)]
struct MailboxPiece {
    pub player: Player,
    pub piece: Piece,
}



struct Mailbox {
    squares: [Option<MailboxPiece>; 65] //not a typo, 0 has 64 trailing 0's, so squares[64] should be None always!
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
        if player == Player::White {
            println!("    A  B  C  D  E  F  G  H ");
        }
        else {
            println!("    H  G  F  E  D  C  B  A ")
        }
    }
    pub fn show(&self) {
        let mut buffer = [' '; 64];
        for p in self.mailbox.iter() {  
            buffer[p.square as usize] = p.piece.get_char(p.player);
        }
        Board::draw_board(&buffer, self.active);
    }
    pub fn force_move(&mut self, m: MoveSquares, gen: &PossibleMoveGenerator) -> bool {
        let p = match self.mailbox.get(m.from) {
            Some(n) => n,
            None => {return false},
        };
        let moves = bit_set::union(gen.get_moves(&p), gen.get_attacks(&p));
        if bit_set::get(moves, m.to) == 0 {
            return false;
        }
        let set = self.get_set_mut(p.player, p.piece);
        bit_set::unset(set, m.from);
        bit_set::set(set, m.to);
        match self.mailbox.force_move(m) {
            Some(p) => bit_set::unset(&mut self.get_set(self.active.invert(), p.piece), p.square),
            None => ()
        }
        true
    }
    pub fn generate_psudolegal(&self, gen: &PossibleMoveGenerator, slide: &SliderMasks) -> Vec<Move> {
        let mut res: Vec<Move> =vec![];
        let opp = self.active.invert(); 
        let friends = self.pieces[self.active.index()].iter()
            .fold(0,|a,&x| bit_set::union(a, x));
        let enemies = self.pieces[opp.index()].iter()
            .fold(0,|a,&x| bit_set::union(a, x));
        let all_pieces = bit_set::union(friends, enemies);
        let king_pos = bit_set::lsb_pos(self.get_set(self.active, Piece::King));

        let king_bishop_check = gen.get_attacks(
            &SpecefiedPiece { 
                player: self.active, 
                piece: Piece::Bishop, 
                square: king_pos 
            }
        );
        let king_rook_check = gen.get_attacks(
            &SpecefiedPiece { 
                player: self.active, 
                piece: Piece::Bishop, 
                square: king_pos 
            }
        );
        let king_queen_check = bit_set::union(king_bishop_check, king_rook_check);


        let bishop_pinners = bit_set::intersect(king_bishop_check, self.get_set(opp, Piece::Bishop));
        let rook_pinners = bit_set::intersect(king_rook_check, self.get_set(opp, Piece::Rook));
        let queen_pinners = bit_set::intersect(king_queen_check, self.get_set(opp, Piece::Queen));

        let pinners = bit_set::union(queen_pinners, bit_set::union(bishop_pinners, rook_pinners));

        let mut p = pinners;
        let mut pinned:Set = 0;
        let mut check:Set = 0;
        while p != 0 {
            let a = bit_set::lsb_pos(p);
            bit_set::clear_lsb(&mut p);
            let attack =  slide.get(a, king_pos);
            let blockers = bit_set::intersect(attack, bit_set::difference(pinners, all_pieces));
            if count(blockers) == 0 {
                bit_set::set(&mut check, a);
            }
            if bit_set::intersect(attack, bit_set::difference(pinners, all_pieces)) == 0 {
                bit_set::set(&mut check, a);
            }
            let pf = intersect(attack, friends);
            if bit_set::count(pf) == 1 {
                pinned = bit_set::union(pinned, pf);
            }
        }
        bit_set::show(pinners);
        bit_set::show(pinned);
        
        for piece in self.mailbox.iter().filter(|p| p.player == self.active) {
            let ss = 1u64 << piece.square;
            let attacks = gen.get_attacks(&piece);
            let moves = gen.get_moves(&piece);
            let mut all_moves = bit_set::union(attacks, moves);
            while all_moves != 0 {
                let new_square = bit_set::lsb_pos(all_moves);
                bit_set::clear_lsb(&mut all_moves);
                let nss = 1u64 << new_square;
                if bit_set::intersect(friends, nss) != 0 {
                    continue;
                }
                let s = slide.get(piece.square, new_square);
                if bit_set::intersect(s, all_pieces) != 0 {
                    continue;
                }
                let is_capture = bit_set::intersect(enemies, nss);
                if bit_set::intersect(ss, pinned) != 0 {
                    let resolves_pin = bit_set::difference(nss, pinners);
                    if resolves_pin == pinners {
                        continue;
                    }
                }
                res.push(
                    match self.mailbox.get(bit_set::lsb_pos(is_capture)) {
                        Some(SpecefiedPiece{piece: cap_piece, ..}) => Move{from: piece.square as u8, to: new_square as u8, move_type: MoveType::Capture(cap_piece)},
                        None => Move { from: piece.square as u8, to: new_square as u8, move_type: MoveType::Quiet }
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
            let psudo_moves = self.generate_psudolegal(&gen, &slide);
            self.show();
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
                            println!("Invalid Move (IDK bro)");
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
                        mailbox.squares[i] = Some(MailboxPiece{player, piece});
                    }
                }
            }
        }
        mailbox
    }

    pub fn get(&self, square: u8) -> Option<SpecefiedPiece> {
        let MailboxPiece {piece, player} = self.squares[square as usize]?;
        Some(SpecefiedPiece {piece, player, square})
    }

    pub fn force_move(&mut self, m: MoveSquares) -> Option<SpecefiedPiece> {
        let t = self.get(m.from)?;
        self.squares[m.from as usize] = None;
        let res = self.get(m.to);
        self.squares[m.to as usize] = Some(MailboxPiece {piece: t.piece, player: t.player});
        res   
    }

}

pub struct MailboxIter<'a> {
    curr: u8,
    arr: &'a [Option<MailboxPiece>; 65],
}

impl<'a> Iterator for MailboxIter<'a> {
    type Item = SpecefiedPiece;

    fn next(&mut self) -> Option<Self::Item> {
        self.arr[self.curr.into()..].iter().zip(0..64u8)
            .filter_map(|(&o, i)| {
                match o {
                    Some(MailboxPiece { player, piece }) => {
                        let res = Some(SpecefiedPiece{player, piece, square: self.curr + i});
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