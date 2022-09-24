pub type Set = u64;
use termion::{color};
pub fn union(a: Set, b: Set) -> Set {
    a | b
}

pub fn intersect(a: Set, b: Set) -> Set {
    a & b
}

pub fn from_idx(a: u8) -> Set {
    1 << a
}

pub fn difference(mask:Set, image:Set) -> Set {
    (!mask) & image
}

pub fn xor(a:Set, b:Set) -> Set {
    a ^ b
}

pub fn get<N: Into<usize>>(a: Set, idx: N) -> Set {
    a & (1 << (idx.into()))
}

pub fn set<N: Into<usize>>(a: &mut Set, idx: N) {
    *a = *a | (1 << idx.into())
}

pub fn unset<N: Into<u64>>( a: &mut Set, idx: N) {
    *a = *a & !(1 << idx.into())
}

pub fn clear_lsb(a: &mut Set) {
    *a = *a & (a.wrapping_sub(1))
}

pub fn lsb(a: Set) -> Set {
    a & (!a).wrapping_add(1)
}

pub fn lsb_pos(a: Set) -> u8 {
    a.trailing_zeros() as u8
}

pub fn count(a: Set) -> u8 {
    a.count_ones() as u8
}

pub fn iter(set: Set) -> SetIter {
    SetIter {
        set
    }
}

pub fn iter_pos(set: Set) -> SetPosIter {
    SetPosIter {
        set
    }
}

#[allow(unused)]
pub fn show(a: Set) {
    println!("{}", a);
    println!("{}  ┏━━━━━━━━━━━━━━━━━━━━━━━━┓", color::Fg(color::White));
    for i in 0..8u8 {
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
        for j in 0..8u8 {
            let spot = match get(a, i*8 + j) {
                0 => ' ',
                _ => 'x',
            };
            let is_black = (i%2)^(j%2) == 1;
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

pub struct SetIter {
    set: Set,
}
impl Iterator for SetIter {
    type Item = Set;
    fn next(&mut self) -> Option<Self::Item> {
        let res = lsb(self.set);
        clear_lsb(&mut self.set);
        match res {
            0 => None,
            _ => Some(res),
        }
    }
}

pub struct SetPosIter {
    set: Set,
}
impl Iterator for SetPosIter {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        let res = lsb_pos(self.set);
        clear_lsb(&mut self.set);
        match res {
            64 => None,
            _ => Some(res),
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn bit_twiddle() {
        assert!(super::lsb(15) == 1);
        let mut x = 15;
        super::clear_lsb(&mut x);
        assert!(x == 14);
    }
}