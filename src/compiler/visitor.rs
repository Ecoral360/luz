use crate::ast::{Exp, Stat};

pub trait Visitor {
    type Return;
    fn visit_exp(&mut self, exp: &Exp) -> Self::Return;
    fn visit_stat(&mut self, stat: &Stat) -> Self::Return;
}

pub trait Visitable<V: Visitor> {
    fn accept(&self, visitor: &mut V) -> V::Return;
}

impl<V: Visitor> Visitable<V> for Stat {
    fn accept(&self, visitor: &mut V) -> V::Return {
        visitor.visit_stat(self)
    }
}

impl<V: Visitor> Visitable<V> for Exp {
    fn accept(&self, visitor: &mut V) -> V::Return {
        visitor.visit_exp(self)
    }
}
