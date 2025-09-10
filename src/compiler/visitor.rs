use crate::ast::{Exp, Stat};

pub trait Visitor {
    type Return;
    type Ctx;

    fn visit_exp(&mut self, exp: &Exp, ctx: &mut Self::Ctx) -> Self::Return;
    fn visit_stat(&mut self, stat: &Stat, ctx: &mut Self::Ctx) -> Self::Return;
}

pub trait Visitable<V: Visitor> {
    fn accept(&self, visitor: &mut V, ctx: &mut V::Ctx) -> V::Return;
}

impl<V: Visitor> Visitable<V> for Stat {
    fn accept(&self, visitor: &mut V, ctx: &mut V::Ctx) -> V::Return {
        visitor.visit_stat(self, ctx)
    }
}

impl<V: Visitor> Visitable<V> for Exp {
    fn accept(&self, visitor: &mut V, ctx: &mut V::Ctx) -> V::Return {
        visitor.visit_exp(self, ctx)
    }
}
