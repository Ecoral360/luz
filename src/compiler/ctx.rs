use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
    usize,
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
        let mut scope = Scope::new(Some(String::from("main")), Some(get_builtin_scope()));
        scope
            .upvalues
            .push(Upvalue::new("_ENV".to_owned(), 0, 0, true));
        Self {
            scope: Rc::new(RefCell::new(scope)),
            nb_expected: 1,
        }
    }
    pub fn new_chunk(name: String, parent_scope: Option<ScopeRef>, upvalues: Vec<Upvalue>) -> Self {
        let mut scope = Scope::new(Some(name), parent_scope);
        // scope
        //     .upvalues
        //     .push(Upvalue::new("_ENV".to_owned(), 0, 0, true));

        scope.upvalues = upvalues;

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

    pub(crate) fn push_scope(&mut self, scope_name: Option<String>) -> usize {
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
        self.scope()
            .regs
            .iter()
            .find_map(|reg| if reg.free { Some(reg.addr) } else { None })
    }

    pub(crate) fn target_register_or_err(&self) -> Result<u8, LuzError> {
        Ok(self
            .scope()
            .regs
            .iter()
            .find_map(|reg| if reg.free { Some(reg.addr) } else { None })
            .ok_or_else(|| LuzError::CompileError(format!("No free target registers")))
            .unwrap())
    }

    pub(crate) fn get_or_push_free_register(&mut self) -> u8 {
        match self.target_register() {
            Some(v) => v,
            None => self.push_free_register(None),
        }
    }

    pub(crate) fn rename_or_push_free_register(&mut self, name: String) -> u8 {
        match self.target_register() {
            Some(v) => {
                let start = self.scope().next_reg_start();
                self.scope_mut().regs[v as usize].name = Some(name);
                self.scope_mut().regs[v as usize].range = Some(RegisterRange::new(start, None));
                v
            }
            None => self.push_free_register(Some(name)),
        }
    }

    pub(crate) fn rename_register(&mut self, reg: u8, name: String) {
        self.scope_mut().rename_register(reg, name);
    }

    pub(crate) fn claim_next_free_register(&mut self) -> u8 {
        let next_free = self.get_or_push_free_register();
        self.scope_mut().regs[next_free as usize].free = false;
        next_free
    }

    pub(crate) fn find_reg(&self, register_name: &str) -> Option<u8> {
        self.scope().find_reg(register_name)
    }

    pub(crate) fn push_free_register(&mut self, register_name: Option<String>) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let range = register_name.as_ref().and(Some(RegisterRange::new(
            self.scope().next_reg_start(),
            None,
        )));

        let reg = Register::new(register_name, addr, range);
        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn set_end_of_register(&mut self, register_name: &str) {
        self.scope_mut().set_end_of_register(register_name);
    }

    pub(crate) fn push_claimed_register(&mut self, register_name: Option<String>) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let range = register_name.as_ref().and(Some(RegisterRange::new(
            self.scope().next_reg_start(),
            None,
        )));
        let mut reg = Register::new(register_name, addr, range);
        reg.free = false;
        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn push_claimed_register_with_start(
        &mut self,
        register_name: Option<String>,
        start: u8,
    ) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let range = register_name
            .as_ref()
            .and(Some(RegisterRange::new(start, None)));
        let mut reg = Register::new(register_name, addr, range);
        reg.free = false;
        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn unclaim_register_range(&mut self, start: u8, nb: u8) {
        for i in start..start + nb {
            self.scope_mut().regs[i as usize].free = true;
        }
    }

    pub(crate) fn claim_register(&mut self, reg: u8) {
        self.scope_mut().regs[reg as usize].free = false;
    }

    pub(crate) fn unclaim_registers(&mut self, regs: &[u8]) {
        for reg in regs {
            self.scope_mut().regs[*reg as usize].free = true;
        }
    }

    pub(crate) fn log_inst<S: ToString>(&mut self, s: S) {
        self.push_inst(Instruction::log(s));
    }

    pub(crate) fn push_inst(&mut self, inst: Instruction) {
        self.scope_mut().instructions.push(inst);
        let idx = self
            .scope_mut()
            .instructions
            .iter()
            .enumerate()
            .find_map(|(i, inst)| {
                if matches!(inst, Instruction::NOP) {
                    Some(i)
                } else {
                    None
                }
            });

        if let Some(idx) = idx {
            self.scope_mut().instructions.swap_remove(idx);
        }
    }

    pub fn get_or_add_const(&mut self, obj: LuzObj) -> u32 {
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

    pub fn instructions_to_string(&self) -> String {
        self.scope.borrow().instructions_to_string()
    }
}

#[derive(Debug, new, Clone)]
pub struct Scope {
    name: Option<String>,
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

    #[new(default)]
    nb_params: u32,

    #[new(default)]
    vararg: Vec<LuzObj>,
}

pub type ScopeRef = Rc<RefCell<Scope>>;

pub enum RegOrUpvalue {
    Register(Register),
    Upvalue(Upvalue),
}

impl Scope {
    pub fn new_ref(name: String, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope::new(Some(name), parent)))
    }

    pub fn new_global() -> Self {
        Self::new(Some(String::from("GLOBAL")), None)
    }

    pub fn is_global(&self) -> bool {
        matches!(self.name.as_deref(), Some("GLOBAL"))
    }

    pub fn next_reg_start(&self) -> u8 {
        self.instructions.len() as u8 + 2
    }

    pub fn instructions(&self) -> &Vec<Instruction> {
        &self.instructions
    }

    pub fn instructions_mut(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }

    pub fn constants(&self) -> &Vec<LuzObj> {
        &self.constants
    }

    pub fn get_or_add_const(&mut self, obj: LuzObj) -> u32 {
        let addr = self
            .constants
            .iter()
            .enumerate()
            .find(|(_, con)| **con == obj)
            .map(|(i, _)| i);

        if let Some(addr) = addr {
            return addr as u32;
        }

        self.constants.push(obj);
        (self.constants.len() - 1) as u32
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

    pub fn take_reg_val(&mut self, addr: u8) -> Option<LuzObj> {
        self.regs
            .get_mut(addr as usize)
            .map(|reg| reg.val.take())
            .flatten()
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
            .find_reg(register_name)
            .map(|addr| &self.regs[addr as usize]);

        if let Some(v) = reg {
            return Ok((false, RegOrUpvalue::Register(v.clone())));
        }

        if let Some(up_addr) = self.get_upvalue_with_name(register_name) {
            return Ok((false, RegOrUpvalue::Upvalue(up_addr.clone())));
        }

        if let Some(p) = &self.parent {
            if p.borrow().is_global() {
                if let Some(env_addr) = self.get_upvalue_with_name("_ENV") {
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
                        let obj = LuzObj::String(register_name.to_string());
                        if !self.constants.contains(&obj) {
                            self.constants.push(obj);
                        }
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

            let existing_upvalue = self.upvalues.iter().find(|up| up.name == upvalue.name);
            match existing_upvalue {
                Some(existing_upvalue) => {
                    Ok((in_table, RegOrUpvalue::Upvalue(existing_upvalue.clone())))
                }
                None => {
                    self.upvalues.push(upvalue.clone());
                    Ok((in_table, RegOrUpvalue::Upvalue(upvalue)))
                }
            }
        } else {
            Err(LuzError::CompileError(format!(
                "name {:?} is not declared",
                register_name
            )))
        }
    }

    pub fn find_reg(&self, register_name: &str) -> Option<u8> {
        let curr = self.next_reg_start();

        self.regs
            .iter()
            .rfind(|reg| matches!(&reg.name, Some(x) if x == register_name && reg.range.as_ref().is_none_or(|r| r.contains(curr - 1))))
            .map(|reg| reg.addr)
    }

    pub fn set_end_of_register(&mut self, register_name: &str) {
        let end = self.next_reg_start();

        let reg = self
            .find_reg(register_name)
            .map(|addr| &mut self.regs[addr as usize]);

        if let Some(reg) = reg {
            reg.range.as_mut().map(|range| range.end = Some(end));
        }
    }

    pub fn rename_register(&mut self, reg: u8, name: String) {
        let start = self.next_reg_start() - 2;
        let reg = &mut self.regs[reg as usize];
        reg.name = Some(name);
        reg.range = Some(RegisterRange::new(start, None));
    }

    pub fn get_const(&self, addr: u8) -> &LuzObj {
        &self.constants[addr as usize]
    }

    pub fn get_reg(&self, addr: u8) -> &Register {
        &self.regs[addr as usize]
    }

    pub fn get_upvalue(&self, addr: u8) -> &Upvalue {
        &self.upvalues[addr as usize]
    }

    pub fn get_upvalue_with_name(&self, name: &str) -> Option<&Upvalue> {
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

    pub fn set_upvalue_value(&mut self, upvalue_addr: u8, value: LuzObj) {
        let upvalue = &self.upvalues[upvalue_addr as usize];
        let mut p = self
            .parent
            .as_ref()
            .expect("Needs parent to have upvalue")
            .borrow_mut();
        if upvalue.in_stack {
            p.regs[upvalue.parent_addr as usize].val.replace(value);
        } else {
            p.set_upvalue_value(upvalue.parent_addr, value);
        }
    }

    pub fn instructions_to_string(&self) -> String {
        let mut result = if matches!(self.name.as_deref(), Some("main")) {
            String::from("main\n")
        } else {
            format!(
                "function {}\n",
                self.name.as_ref().unwrap_or(&String::new())
            )
        };

        for (i, inst) in self.instructions.iter().enumerate() {
            result += &format!("[{}] {}\n", i + 1, inst.debug(self));
        }

        result += "---- Constants:\n";
        for (i, inst) in self.constants.iter().enumerate() {
            result += &format!("{} {} {}\n", i, inst.get_type(), inst.repr());
        }

        result += "---- Locals:\n";
        for (i, inst) in self.regs.iter().enumerate() {
            if let Some(name) = &inst.name {
                let Some(RegisterRange { start, end }) = inst.range else {
                    unreachable!("There must be a range if there is a name")
                };
                result += &format!(
                    "{} {} {} {}\n",
                    i,
                    name,
                    start,
                    end.unwrap_or(self.instructions().len() as u8 + 1)
                );
            }
        }

        result += "---- Upvalues:\n";
        for (i, inst) in self.upvalues.iter().enumerate() {
            result += &format!(
                "{} {} {} {}\n",
                i, inst.name, inst.in_stack, inst.parent_addr
            );
        }

        result.push('\n');
        for scope in &self.sub_scopes {
            result += &scope.borrow().instructions_to_string();
        }

        result
    }

    pub fn sub_scopes(&self) -> &[Rc<RefCell<Scope>>] {
        &self.sub_scopes
    }

    pub fn set_nb_params(&mut self, nb_params: u32) {
        self.nb_params = nb_params;
    }

    pub fn nb_params(&self) -> u32 {
        self.nb_params
    }

    pub fn vararg(&self) -> &[LuzObj] {
        &self.vararg
    }

    pub fn set_vararg(&mut self, vararg: Vec<LuzObj>) {
        self.vararg = vararg;
    }

    pub fn parent(&self) -> Option<&Rc<RefCell<Scope>>> {
        self.parent.as_ref()
    }

    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }
}

#[derive(Debug, new, Clone, Builder)]
pub struct Register {
    #[builder(default)]
    pub name: Option<String>,
    pub addr: u8,

    #[builder(default)]
    #[new(default)]
    pub val: Option<LuzObj>,

    #[builder(default)]
    #[new(value = "true")]
    pub free: bool,

    #[builder(default)]
    range: Option<RegisterRange>,
}

#[derive(Debug, new, Clone)]
pub struct RegisterRange {
    pub start: u8,

    pub end: Option<u8>,
}

impl RegisterRange {
    pub fn contains(&self, addr: u8) -> bool {
        self.start <= addr && self.end.is_none_or(|end| addr <= end)
    }
}

#[derive(Debug, new, Clone)]
pub struct Upvalue {
    pub name: String,
    pub addr: u8,
    pub parent_addr: u8,
    pub in_stack: bool,
}
