use super::bit_set::{Set, self};

struct SliderMasks {
    masks: [[Set; 64]; 64]
}

impl SliderMasks {
    pub fn new() -> Self {
        let mut masks:[[Set; 64]; 64] = [[0;64];64];
        for (end_arr, start) in masks.iter_mut().zip(0..64i32) {
            for (set,end) in end_arr.iter_mut().zip(0..64i32) {
                let max = start.max(end);
                let min = start.min(end);
                let diff = max - min;
                if diff < 8 {
                    for i in min+1..max {
                        bit_set::set::<usize>(set, i.try_into().ok().unwrap())
                    }
                }
                for m in 7..=9 {
                    if diff % m == 0 {
                        let mut i = min + diff;
                        while i < max {
                            bit_set::set::<usize>(set, i.try_into().ok().unwrap());
                            i += diff;
                        }
                        break;
                    }
                }
                bit_set::show(*set);
            }
        }
        Self {
            masks
        }
    }
    pub fn get<N: Into<usize>>(&self, from: N, to: N) -> Set {
        self.masks[from.into()][to.into()]
    }
}