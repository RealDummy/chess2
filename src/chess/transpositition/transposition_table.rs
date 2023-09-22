use crate::chess::board::{Move, Evaluation, HashT};

use super::hash_table::HashTable;
use super::super::board::{EvalT, Board};

#[derive(Clone)]
pub enum Score {
    Exact(Evaluation),
    LowerBound(EvalT),
    UpperBound(EvalT),
}
#[derive(Clone)]
pub struct TTableNode {
    pub is_odd: bool,
    pub ply: u8,
    pub eval: Score,
    pub best: Option<Move>,
}

fn replace_shallow_strategy(a: &TTableNode, b: TTableNode) -> Option<TTableNode> {
    if a.ply >= b.ply {
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
    pub fn insert(&mut self, hash: HashT, data: TTableNode) {
        self.base.insert(hash, data)
    }
    pub fn get(&self, hash: HashT) -> Option<&TTableNode> {
        self.base.get(hash)
    }
}