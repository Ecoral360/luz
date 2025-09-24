use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::luz::obj::{LuzType, Numeral};

use super::obj::LuzObj;

#[derive(Debug, Clone, Default)]
pub struct Table {
    table: HashMap<LuzObj, LuzObj>,
    arr: Vec<LuzObj>,
    keys: Vec<LuzObj>,
    metatable: Option<Rc<RefCell<Table>>>,
    tag_method_flags: u8,
}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        self.table == other.table
    }
}

impl Table {
    pub fn new(table: HashMap<LuzObj, LuzObj>, metatable: Option<Rc<RefCell<Table>>>) -> Self {
        Self {
            keys: table.keys().cloned().collect(),
            table,
            metatable,
            ..Default::default()
        }
    }

    pub fn table_eq(table1: Rc<RefCell<Table>>, table2: Rc<RefCell<Table>>) -> bool {
        let t1 = table1.borrow();
        let t2 = table2.borrow();

        *t1 == *t2
    }

    pub fn get(&self, key: &LuzObj) -> &LuzObj {
        match key.get_type() {
            LuzType::Nil => &LuzObj::Nil,
            LuzType::Number => {
                let LuzObj::Numeral(num) = key else {
                    unreachable!()
                };
                match num {
                    Numeral::Int(i) => {
                        if (1..=self.arr.len()).contains(&(*i as usize)) {
                            self.arr.get(*i as usize - 1).unwrap_or(&LuzObj::Nil)
                        } else {
                            self.table.get(key).unwrap_or(&LuzObj::Nil)
                        }
                    }
                    Numeral::Float(f) => {
                        if f.floor() == *f && (1..=self.arr.len()).contains(&(*f as usize)) {
                            self.arr.get(*f as usize - 1).unwrap_or(&LuzObj::Nil)
                        } else {
                            self.table.get(key).unwrap_or(&LuzObj::Nil)
                        }
                    }
                }
            }
            LuzType::Boolean
            | LuzType::String
            | LuzType::Function
            | LuzType::Userdata
            | LuzType::Thread => self.table.get(key).unwrap_or(&LuzObj::Nil),

            LuzType::Table => todo!(),

            _ => unreachable!(),
        }
    }

    pub fn push(&mut self, item: LuzObj) {
        self.arr.push(item);
    }

    pub fn insert(&mut self, key: LuzObj, item: LuzObj) {
        if key == LuzObj::Numeral(Numeral::Int(self.arr.len() as i64 + 1)) {
            self.arr.push(item);
        } else {
            if item == LuzObj::Nil {
                self.table.remove(&key);
                self.keys.retain(|k| *k != key);
            } else {
                self.table.insert(key.clone(), item);
                if !self.keys.contains(&key) {
                    self.keys.push(key);
                }
            }
        }
    }

    pub fn get_or_insert(&mut self, key: &LuzObj, item: LuzObj) -> LuzObj {
        let val = self.get(key).clone();
        if val.is_nil() {
            self.insert(key.clone(), item.clone());
            item
        } else {
            val
        }
    }

    pub fn raw_metatable(&self) -> Option<Rc<RefCell<Table>>> {
        self.metatable.as_ref().map(|m| Rc::clone(m))
    }

    pub fn metatable(&self) -> LuzObj {
        match &self.metatable {
            Some(metatable) => LuzObj::Table(Rc::clone(metatable)),
            None => LuzObj::Nil,
        }
    }

    //pub fn insert(&mut self, key: LuzObj, val: LuzObj) {}

    fn find_boundary_in_array(&self) -> usize {
        let mut left = 0;
        let mut right = self.arr.len();
        if right == 0 {
            return 0;
        }

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

        left + 1
    }

    pub fn len(&self) -> usize {
        self.find_boundary_in_array()
        // if self.arr.last().is_some_and(|el| el.is_nil()) {
        // } else {
        //     0
        // }
    }

    pub fn tag_method_flags(&self) -> u8 {
        self.tag_method_flags
    }

    pub fn first_idx(&self) -> LuzObj {
        if !self.arr.is_empty() {
            return LuzObj::int(1);
        }
        if !self.keys.is_empty() {
            return self.keys.first().unwrap().clone();
        }
        LuzObj::Nil
    }

    pub fn next_idx(&self, idx: &LuzObj) -> LuzObj {
        if let LuzObj::Numeral(num) = idx {
            if num.floor() == *num && (num.as_int() as usize) < self.arr.len() {
                return LuzObj::int(num.as_int() + 1);
            }
        }

        let res = self.keys.iter().skip_while(|k| *k != idx).nth(1);

        res.cloned().unwrap_or(LuzObj::Nil)
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

        table.push(Numeral::Int(1).into());
        table.push(Numeral::Int(2).into());
        table.push(Numeral::Int(3).into());
        table.push(LuzObj::Nil);
        table.push(Numeral::Int(4).into());
        table.push(Numeral::Int(5).into());
        table.push(Numeral::Int(6).into());
        table.push(Numeral::Int(7).into());

        let b_loop = table.find_boundary_in_array();

        assert_eq!(b_loop, 3);
    }
}
