use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::luz::obj::{LuzType, Numeral};

use super::obj::LuzObj;

#[derive(Debug, Clone, Default)]
pub struct Table {
    table: HashMap<LuzObj, LuzObj>,
    arr: Vec<LuzObj>,
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
            LuzType::Integer => {
                let LuzObj::Numeral(Numeral::Int(i)) = key else {
                    unreachable!()
                };
                if (0..self.arr.len()).contains(&(*i as usize)) {
                    self.arr.get(*i as usize).unwrap_or(&LuzObj::Nil)
                } else {
                    self.table.get(key).unwrap_or(&LuzObj::Nil)
                }
            }
            LuzType::Float => {
                let LuzObj::Numeral(Numeral::Float(f)) = key else {
                    unreachable!()
                };

                if f.floor() == *f {
                    self.arr.get(*f as usize).unwrap_or(&LuzObj::Nil)
                } else {
                    self.table.get(key).unwrap_or(&LuzObj::Nil)
                }
            }
            LuzType::Number => todo!(),
            LuzType::Boolean
            | LuzType::String
            | LuzType::Function
            | LuzType::Userdata
            | LuzType::Thread => self.table.get(key).unwrap_or(&LuzObj::Nil),

            LuzType::Table => todo!(),
        }
    }

    pub fn insert(&mut self, item: LuzObj) {
        self.arr.push(item);
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

    pub fn tag_method_flags(&self) -> u8 {
        self.tag_method_flags
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
