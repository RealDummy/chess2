use crate::chess::board::Move;

use super::hash_table::HashTable;
use super::super::board::{EvalT, Board};

#[derive(Clone)]
pub enum Score {
    Exact(EvalT),
    LowerBound(EvalT),
    UpperBound(EvalT),
}
#[derive(Clone)]
pub struct TTableNode {
    pub depth: u32,
    pub eval: Score,
    pub best: Option<Move>,
}

fn replace_shallow_strategy(a: &TTableNode, b: TTableNode) -> Option<TTableNode> {
    if a.depth >= b.depth {
        None
    }
    else {
        Some(b)
    }
}

pub struct TTable
{
    base: HashTable<TTableNode, for<'r> fn(&'r TTableNode, TTableNode) -> Option<TTableNode>, 1000000>,
}

impl TTable 
{
    pub fn new() -> Self {
        Self { 
            base: HashTable::new(replace_shallow_strategy), 
        }
    }
    pub fn insert(&mut self, hash: usize, data: TTableNode) {
        self.base.insert(hash, data)
    }
    pub fn get(&self, hash: usize) -> Option<&TTableNode> {
        self.base.get(hash)
    }
}