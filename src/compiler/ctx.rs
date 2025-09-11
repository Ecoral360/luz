use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    compiler::instructions::Instruction,
    luz::{env::get_builtin_scope, err::LuzError, obj::LuzObj},
};

#[derive(Debug, new, Builder, Clone)]
pub struct CompilerCtx {
    /// If Some(0) -> varargs
    /// else real nb of expected = nb_expected - 1
    nb_expected: u8,
    scope: ScopeRef,
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
    pub fn scope_clone(&self) -> ScopeRef {
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

    pub(crate) fn unclaim_register_range(&mut self, start: u8, nb: u8) {
        for i in start..start + nb {
            self.scope_mut().regs[i as usize].free = true;
        }
    }

    pub(crate) fn unclaim_registers(&mut self, regs: &[u8]) {
        for reg in regs {
            self.scope_mut().regs[*reg as usize].free = true;
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

#[derive(Debug, new, Clone)]
pub struct Scope {
    name: String,
    parent: Option<ScopeRef>,

    #[new(default)]
    instructions: Vec<Instruction>,
    #[new(default)]
    constants: Vec<LuzObj>,
    #[new(default)]
    regs: Vec<Register>,

    #[new(default)]
    upvalues: Vec<Upvalue>,

    #[new(default)]
    sub_scopes: Vec<ScopeRef>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;

pub enum RegOrUpvalue {
    Register(Register),
    Upvalue(Upvalue),
}

impl Scope {
    pub fn new_ref(name: String, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope::new(name, parent)))
    }

    pub fn instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    pub fn constants(&self) -> &Vec<LuzObj> {
        &self.constants
    }

    pub fn regs(&self) -> &Vec<Register> {
        &self.regs
    }

    pub fn upvalues(&self) -> &Vec<Upvalue> {
        &self.upvalues
    }

    pub fn set_reg_val(&mut self, addr: u8, val: LuzObj) {
        self.regs[addr as usize].val = Some(val);
    }

    pub fn push_reg(&mut self, reg: &mut RegisterBuilder) {
        reg.addr(self.regs().len() as u8);
        self.regs.push(reg.build().expect("Non valid register"));
    }

    /// Returns (is_intable, reg_or_upvalue)
    pub fn get_reg_or_upvalue(
        &mut self,
        register_name: &str,
    ) -> Result<(bool, RegOrUpvalue), LuzError> {
        let reg = self
            .regs
            .iter()
            .find(|reg| matches!(&reg.name, Some(x) if x == register_name));

        if let Some(v) = reg {
            return Ok((false, RegOrUpvalue::Register(v.clone())));
        }

        if let Some(up_addr) = self.get_upvalue(register_name) {
            return Ok((false, RegOrUpvalue::Upvalue(up_addr.clone())));
        }

        if let Some(p) = &self.parent {
            if p.borrow().name == "GLOBAL" {
                if let Some(env_addr) = self.get_upvalue("_ENV") {
                    return Ok((true, RegOrUpvalue::Upvalue(env_addr.clone())));
                } else {
                    return Err(LuzError::CompileError(format!(
                        "global name {:?} is not declared",
                        register_name
                    )));
                };
            }

            let (_, parent_addr) = p.borrow_mut().get_reg_or_upvalue(register_name)?;
            let addr = self.upvalues.len() as u8;

            let (in_table, upvalue) = match parent_addr {
                RegOrUpvalue::Register(register) => (
                    false,
                    Upvalue::new(register.name.unwrap(), addr, register.addr, true),
                ),
                RegOrUpvalue::Upvalue(upvalue) => {
                    let in_table = if upvalue.name == "_ENV" {
                        self.constants
                            .push(LuzObj::String(register_name.to_string()));
                        true
                    } else {
                        false
                    };
                    (
                        in_table,
                        Upvalue::new(upvalue.name, addr, upvalue.parent_addr, false),
                    )
                }
            };

            self.upvalues.push(upvalue.clone());

            Ok((in_table, RegOrUpvalue::Upvalue(upvalue)))
        } else {
            Err(LuzError::CompileError(format!(
                "name {:?} is not declared",
                register_name
            )))
        }
    }

    pub fn find_reg(&self, register_name: &str) -> Option<u8> {
        self.regs
            .iter()
            .find(|reg| matches!(&reg.name, Some(x) if x == register_name))
            .map(|reg| reg.addr)
    }

    pub fn set_reg_free(&self, register_name: &str) -> Option<u8> {
        self.regs
            .iter()
            .find(|reg| matches!(&reg.name, Some(x) if x == register_name))
            .map(|reg| reg.addr)
    }

    pub fn get_upvalue(&self, name: &str) -> Option<&Upvalue> {
        self.upvalues.iter().find(|con| con.name == name)
    }

    pub fn get_upvalue_addr(&self, name: &str) -> Option<u8> {
        self.upvalues.iter().enumerate().find_map(|(i, con)| {
            if con.name == name {
                Some(i as u8)
            } else {
                None
            }
        })
    }

    pub fn get_upvalue_value(&self, upvalue_addr: u8) -> Option<LuzObj> {
        let upvalue = &self.upvalues[upvalue_addr as usize];
        let p = self
            .parent
            .as_ref()
            .expect("Needs parent to have upvalue")
            .borrow();
        if upvalue.in_stack {
            p.regs[upvalue.parent_addr as usize].val.clone()
        } else {
            p.get_upvalue_value(upvalue.parent_addr)
        }
    }

    pub fn print_instructions(&self) {
        if self.name == "main" {
            println!("main");
        } else {
            println!("function {:?}", self.name);
        }
        for (i, inst) in self.instructions.iter().enumerate() {
            println!("[{}] {}", i + 1, inst);
        }
        println!("---- Constants:");
        for (i, inst) in self.constants.iter().enumerate() {
            println!("{} {:?}", i, inst);
        }

        println!("---- Locals:");
        for (i, inst) in self.regs.iter().enumerate() {
            if let Some(name) = &inst.name {
                println!("{} {}", i, name);
            }
        }

        println!("---- Upvalues:");
        for (i, inst) in self.upvalues.iter().enumerate() {
            println!("{} {} {} {}", i, inst.name, inst.in_stack, inst.parent_addr);
        }

        println!();
        for scope in &self.sub_scopes {
            scope.borrow().print_instructions();
        }
    }

    pub fn sub_scopes(&self) -> &[Rc<RefCell<Scope>>] {
        &self.sub_scopes
    }
}

#[derive(Debug, new, Clone, Builder)]
pub struct Register {
    pub name: Option<String>,
    pub addr: u8,
    #[new(default)]
    pub val: Option<LuzObj>,
    #[new(value = "true")]
    pub free: bool,
}

#[derive(Debug, new, Clone)]
pub struct Upvalue {
    pub name: String,
    pub addr: u8,
    pub parent_addr: u8,
    pub in_stack: bool,
}
