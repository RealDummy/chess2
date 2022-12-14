use std::usize;

#[derive(Clone)]
struct HashNode<P>
where P: Clone
{
    data: P,
    hash: usize,
}

pub struct HashTable <P, R, const S: usize> 
where 
    P: Clone,
    R: Fn(&P, P) -> Option<P>, 
{
    table: Vec<Option<[HashNode<P>; 2]>>,
    load: usize,
    mask: usize,
    replace_strat: R,
}

impl<P, R, const Size: usize> HashTable<P, R, Size> 
where 
    P: Clone,
    R: Fn(&P, P) -> Option<P>, 
{
    pub fn new(replace_strat: R) -> Self {
        let mut mask = 0;
        let table_size = if usize::is_power_of_two(Size) {
            Size
        } else {
            usize::next_power_of_two(Size)
        };
        for i in 0..table_size.trailing_zeros() {
            mask |= 1 << i;
        }
        let mut table = vec![None; table_size];
        Self {
            table,
            load: 0,
            mask,
            replace_strat
        }
    }
    fn idx(&self, hash: usize) -> usize {
        hash & self.mask
    }
    pub fn insert<H: Into<usize>>(&mut self, hash: H, data: P) {
        let hash: usize = hash.into();
        let index = self.idx(hash);
        match &mut self.table[index] {
            Some([a,b]) => {
                *a = HashNode {data: data.clone(), hash };
                if let Some(data) = (self.replace_strat)(&b.data, data) {
                    *b = HashNode{data, hash};
                }
            },
            n => {
                self.load += 1;
                *n = Some([
                    HashNode{ data: data.clone(), hash },
                    HashNode{ data, hash },
                ])
            }
        }
    }
    pub fn get<H: Into<usize>>(&self, hash: H) -> Option<&P>{
        let hash = hash.into();
        let index = self.idx(hash);
        let arr = self.table[index].as_ref()?;
        for entry in arr {
            if hash == entry.hash {
                return Some(&entry.data)
            }
        }
        None
    }
    pub fn load(&self) -> f64 {
        return self.load as f64 / self.table.len() as f64
    }
}

#[cfg(test)]
mod test {
    use super::HashTable;
    #[test]
    fn simple() {
        let mut table = HashTable::<i32,_,128>::new(|&a, b| None);
        for i in 0..128usize {
            table.insert(i, i as i32)
        }
        assert!(table.get(15usize) == Some(&15));
        assert!(table.get(128usize) == None);
        assert!(table.load() == 1.0);
    }
}