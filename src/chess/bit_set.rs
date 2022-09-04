pub type Set = u64;
use termion::{color};
pub fn union(a: Set, b: Set) -> Set {
    a | b
}

pub fn intersect(a: Set, b: Set) -> Set {
    a & b
}

pub fn inverse(a:Set) -> Set {
    !a
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

pub fn lsb_pos(a: Set) -> usize {
    a.trailing_zeros() as usize
}

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