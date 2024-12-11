use std::{
    cmp::Ordering,
    collections::HashMap,
    sync::{Arc, Mutex},
    usize,
};

use super::obj::LuzObj;

#[derive(Debug, Clone, Default)]
pub struct Table {
    table: HashMap<LuzObj, LuzObj>,
    arr: Vec<LuzObj>,
    metatable: Option<Arc<Mutex<Table>>>,
    tag_method_flags: u8,
}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        self.table == other.table
    }
}

impl Table {
    pub fn new(table: HashMap<LuzObj, LuzObj>, metatable: Option<Arc<Mutex<Table>>>) -> Self {
        Self {
            table,
            metatable,
            ..Default::default()
        }
    }

    pub fn table_eq(table1: Arc<Mutex<Table>>, table2: Arc<Mutex<Table>>) -> bool {
        let t1 = table1.lock().expect("Locking table1 for comparaison");
        let t2 = table2.lock().expect("Locking table2 for comparaison");

        *t1 == *t2
    }

    fn insert(&mut self, item: LuzObj) {
        self.arr.push(item);
    }

    pub fn raw_metatable(&self) -> Option<Arc<Mutex<Table>>> {
        self.metatable.as_ref().map(|m| Arc::clone(m))
    }

    pub fn metatable(&self) -> LuzObj {
        match &self.metatable {
            Some(metatable) => LuzObj::Table(Arc::clone(metatable)),
            None => LuzObj::Nil,
        }
    }

    //pub fn insert(&mut self, key: LuzObj, val: LuzObj) {}

    fn find_boundary_in_array(&self) -> usize {
        let mut left = 0;
        let mut right = self.arr.len();

        // We do a binary search to find the smallest `i`
        // such that arr[i] is not Nil and arr[i+1] is Nil
        while right - 1 > left {
            let mid = (right + left) / 2;
            if self.arr[mid - 1].is_nil() {
                right = mid;
            } else {
                left = mid;
            }
        }

        left
    }

    pub fn len(&self) -> usize {
        if self.arr.last().is_some_and(|el| el.is_nil()) {
            self.find_boundary_in_array()
        } else {
            0
        }
    }
}

#[cfg(test)]
mod test {
    use crate::luz::obj::LuzObj;

    #[test]
    fn test_find_boundary() {
        use crate::luz::numeral::Numeral;

        use super::Table;

        let mut table = Table::default();

        table.insert(Numeral::Int(1).into());
        table.insert(Numeral::Int(2).into());
        table.insert(Numeral::Int(3).into());
        table.insert(LuzObj::Nil);
        table.insert(Numeral::Int(4).into());
        table.insert(Numeral::Int(5).into());
        table.insert(Numeral::Int(6).into());
        table.insert(Numeral::Int(7).into());

        let b_loop = table.find_boundary_in_array();

        assert_eq!(b_loop, 3);
    }
}
