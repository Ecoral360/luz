use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    compiler::{instructions::Instruction, Register, Scope, ScopeLink, Upvalue},
    luz::{env::get_builtin_scope, err::LuzError, obj::LuzObj},
};

#[derive(Debug, new, Builder, Clone)]
pub struct CompilerCtx {
    /// If Some(0) -> varargs
    /// else real nb of expected = nb_expected - 1
    nb_expected: u8,
    scope: ScopeLink,
}

impl CompilerCtx {
    pub fn new_main() -> Self {
        let mut scope = Scope::new(String::from("main"), Some(get_builtin_scope()));
        scope
            .upvalues
            .push(Upvalue::new("_ENV".to_owned(), 0, 0, true));
        Self {
            scope: Rc::new(RefCell::new(scope)),
            nb_expected: 1,
        }
    }

    pub fn new_with(&self, builder: &mut CompilerCtxBuilder) -> Self {
        CompilerCtx {
            nb_expected: builder.nb_expected.unwrap_or(self.nb_expected),
            scope: Rc::clone(builder.scope.as_ref().unwrap_or(&self.scope)),
        }
    }

    pub fn nb_expected(&self) -> u8 {
        self.nb_expected
    }
}

#[allow(unused)]
impl CompilerCtx {
    pub fn scope_clone(&self) -> ScopeLink {
        Rc::clone(&self.scope)
    }

    pub(crate) fn scope(&self) -> Ref<Scope> {
        self.scope.borrow()
    }

    pub(crate) fn scope_mut(&mut self) -> RefMut<Scope> {
        self.scope.borrow_mut()
    }

    pub(crate) fn push_scope(&mut self, scope_name: String) -> usize {
        let new_scope = Rc::new(RefCell::new(Scope::new(
            scope_name,
            Some(Rc::clone(&self.scope)),
        )));
        let idx = self.scope_mut().sub_scopes().len();
        self.scope_mut().sub_scopes.push(Rc::clone(&new_scope));
        self.scope = new_scope;
        idx
    }

    pub(crate) fn pop_scope(&mut self) -> Result<(), LuzError> {
        let parent = Rc::clone(&self.scope);
        let parent = parent.borrow();
        let parent = parent
            .parent
            .as_ref()
            .ok_or_else(|| LuzError::CompileError(format!("No parent scope")))?;

        self.scope = Rc::clone(parent);
        Ok(())
    }

    pub(crate) fn register_upvalue(&mut self) {}

    pub(crate) fn target_register(&self) -> Option<u8> {
        self.target_register_or_err().ok()
    }

    pub(crate) fn target_register_or_err(&self) -> Result<u8, LuzError> {
        self.scope()
            .regs
            .iter()
            .find_map(|reg| if reg.free { Some(reg.addr) } else { None })
            .ok_or_else(|| LuzError::CompileError(format!("No free target registers")))
    }

    pub(crate) fn get_or_push_free_register(&mut self) -> u8 {
        match self.target_register() {
            Some(v) => v,
            None => self.push_free_register(None),
        }
    }

    pub(crate) fn claim_next_free_register(&mut self) -> u8 {
        let next_free = self.get_or_push_free_register();
        self.scope_mut().regs[next_free as usize].free = false;
        next_free
    }

    pub(crate) fn find_reg(&self, register_name: &str) -> Option<u8> {
        self.scope()
            .regs
            .iter()
            .find(|reg| matches!(&reg.name, Some(x) if x == register_name))
            .map(|reg| reg.addr)
    }

    pub(crate) fn push_free_register(&mut self, register_name: Option<String>) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let reg = Register::new(register_name, addr);
        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn push_claimed_register(&mut self, register_name: Option<String>) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let mut reg = Register::new(register_name, addr);
        reg.free = false;
        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn unclaim_registers(&mut self, start: u8, nb: u8) {
        for i in start..start + nb {
            self.scope_mut().regs[i as usize].free = true;
        }
    }

    pub(crate) fn push_inst(&mut self, inst: Instruction) {
        self.scope_mut().instructions.push(inst);
    }

    pub(crate) fn get_or_add_const(&mut self, obj: LuzObj) -> u32 {
        let mut scope = self.scope_mut();
        let addr = scope
            .constants
            .iter()
            .enumerate()
            .find(|(i, con)| **con == obj)
            .map(|(i, con)| i);

        if let Some(addr) = addr {
            return addr as u32;
        }

        scope.constants.push(obj);
        (scope.constants.len() - 1) as u32
    }

    pub fn instructions(&self) -> Vec<Instruction> {
        self.scope().instructions.clone()
    }

    pub fn print_instructions(&self) {
        self.scope.borrow().print_instructions();
    }
}
