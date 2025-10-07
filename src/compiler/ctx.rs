use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::Debug,
    rc::Rc,
    usize,
};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    ast::LineInfo,
    borrowed,
    compiler::instructions::{self, Instruction},
    luz::{err::LuzError, lib::env::get_builtin_scope, obj::LuzObj},
};

#[derive(Debug, new, Builder, Clone)]
pub struct CompilerCtx {
    filename: String,
    /// If Some(0) -> varargs
    /// else real nb of expected = nb_expected - 1
    nb_expected: u8,
    scope: ScopeRef,
    /// The expression is a child of a "not" expression
    in_not: bool,

    /// specify a destination register for the current expression
    dest_addr: Option<u8>,
}

impl CompilerCtx {
    pub fn new_main(filename: String) -> Self {
        let mut scope = Scope::new(Some(String::from("main")), Some(get_builtin_scope()));
        scope.instructions.push(Instruction::op_varargprep(0));
        scope
            .upvalues
            .push(Upvalue::new("_ENV".to_owned(), 0, 0, true));
        Self {
            filename,
            scope: Rc::new(RefCell::new(scope)),
            nb_expected: 2,
            dest_addr: None,
            in_not: false,
        }
    }
    pub fn new_chunk(
        filename: String,
        name: String,
        parent_scope: Option<ScopeRef>,
        mut upvalues: Vec<Upvalue>,
    ) -> Self {
        let mut scope = Scope::new(Some(name), parent_scope.or(Some(get_builtin_scope())));
        scope
            .upvalues
            .push(Upvalue::new("_ENV".to_owned(), 0, 0, true));

        scope.upvalues.append(&mut upvalues);

        scope.instructions.push(Instruction::op_varargprep(0));

        Self {
            scope: Rc::new(RefCell::new(scope)),
            nb_expected: 2,
            dest_addr: None,
            in_not: false,
            filename,
        }
    }

    pub fn new_with(&self, builder: &mut CompilerCtxBuilder) -> Self {
        CompilerCtx {
            nb_expected: builder.nb_expected.unwrap_or(self.nb_expected),
            scope: Rc::clone(builder.scope.as_ref().unwrap_or(&self.scope)),
            in_not: builder.in_not.unwrap_or(self.in_not),
            dest_addr: builder.dest_addr.unwrap_or(self.dest_addr),
            filename: builder.filename.as_ref().unwrap_or(&self.filename).clone(),
        }
    }

    pub fn nb_expected(&self) -> u8 {
        self.nb_expected
    }

    pub fn in_not(&self) -> bool {
        self.in_not
    }

    pub fn dest_addr(&self) -> Option<u8> {
        self.dest_addr
    }

    pub fn filename(&self) -> &str {
        &self.filename
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

    pub(crate) fn unamed_target_register(&self) -> Option<u8> {
        self.scope().regs.iter().find_map(|reg| {
            if reg.free && reg.name.is_none() {
                Some(reg.addr)
            } else {
                None
            }
        })
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
        match self.unamed_target_register() {
            Some(v) => {
                self.scope_mut().regs[v as usize].name = Some(name);
                self.scope_mut().regs[v as usize].range = Some(RegisterRange::new(None, None));

                let reg = self.scope().regs[v as usize].clone();
                self.scope_mut().locals.push(reg);
                v
            }
            None => self.push_free_register(Some(name)),
        }
    }

    pub(crate) fn rename_or_push_free_register_with_start(
        &mut self,
        name: String,
        start: usize,
    ) -> u8 {
        match self.unamed_target_register() {
            Some(v) => {
                self.scope_mut().regs[v as usize].name = Some(name);
                self.scope_mut().regs[v as usize].range =
                    Some(RegisterRange::new(Some(start), None));

                let reg = self.scope().regs[v as usize].clone();
                self.scope_mut().locals.push(reg);
                v
            }
            None => self.push_free_register_with_start(Some(name), start),
        }
    }

    pub(crate) fn set_register_start(&mut self, reg: u8, name: String) {
        self.scope_mut().set_register_start(reg, name);
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
        let range = register_name
            .as_ref()
            .and(Some(RegisterRange::new(None, None)));

        let reg = Register::new(register_name, addr, range);

        if let Some(ref name) = reg.name {
            self.scope_mut().locals.push(reg.clone());
        }

        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn push_free_register_with_start(
        &mut self,
        register_name: Option<String>,
        start: usize,
    ) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let range = register_name
            .as_ref()
            .and(Some(RegisterRange::new(Some(start), None)));

        let reg = Register::new(register_name, addr, range);

        if let Some(ref name) = reg.name {
            self.scope_mut().locals.push(reg.clone());
        }

        self.scope_mut().regs.push(reg);
        addr
    }

    pub(crate) fn set_end_of_register(&mut self, reg: u8) {
        self.scope_mut().set_end_of_register(reg);
    }

    pub(crate) fn push_claimed_register(&mut self, register_name: Option<String>) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let range = register_name.as_ref().and(Some(RegisterRange::new(
            Some(self.scope().next_reg_start()),
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
        start: usize,
    ) -> u8 {
        let addr = self.scope().regs.len() as u8;
        let range = register_name
            .as_ref()
            .and(Some(RegisterRange::new(Some(start), None)));
        let mut reg = Register::new(register_name, addr, range);
        reg.free = false;
        self.scope_mut().regs.push(reg.clone());
        self.scope_mut().locals.push(reg);
        addr
    }

    pub(crate) fn unclaim_register_range(&mut self, start: u8, nb: u8) {
        for i in start..start + nb {
            self.scope_mut().regs[i as usize].free = true;
        }
    }

    pub(crate) fn is_reg_named(&mut self, reg: u8) -> bool {
        self.scope_mut().regs[reg as usize].name.is_some()
    }

    pub(crate) fn is_reg_claimed(&mut self, reg: u8) -> bool {
        !self.is_reg_free(reg)
    }

    pub(crate) fn is_reg_free(&mut self, reg: u8) -> bool {
        self.scope_mut().regs[reg as usize].free
    }

    pub(crate) fn claim_register(&mut self, reg: u8) {
        self.scope_mut().regs[reg as usize].free = false;
    }

    pub(crate) fn unclaim_registers(&mut self, regs: &[u8]) {
        for reg in regs {
            self.scope_mut().regs[*reg as usize].free = true;
        }
    }

    pub(crate) fn log_inst<S: Debug>(&mut self, s: S) {
        self.push_inst(Instruction::log(format!("{:?}", s)));
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

    pub fn instructions_len(&self) -> usize {
        self.instructions().len()
    }

    pub fn pop_instruction(&mut self) -> Option<Instruction> {
        self.scope_mut().instructions.pop()
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
    line_infos: Vec<(usize, LineInfo)>,

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

    #[new(default)]
    locals: Vec<Register>,
}

impl Scope {
    pub fn make_closure(&self) -> Rc<RefCell<Self>> {
        let scope = Rc::new(RefCell::new(Self {
            name: self.name.clone(),
            parent: self.parent.as_ref().map(|p| Rc::clone(p)),
            instructions: self.instructions.clone(),
            line_infos: self.line_infos.clone(),
            constants: self.constants.clone(),
            regs: self.regs.clone(),
            upvalues: self.upvalues.clone(),
            sub_scopes: self.sub_scopes.clone(),
            nb_params: self.nb_params.clone(),
            vararg: self.vararg.clone(),
            locals: self.locals.clone(),
        }));

        // Put the new current scope as the parent scope for its subscopes
        for sub_scope in scope.borrow_mut().sub_scopes.iter_mut() {
            sub_scope.borrow_mut().parent = Some(Rc::clone(&scope));
        }

        scope
    }

    pub fn set_name(&mut self, name: Option<String>) {
        self.name = name;
    }

    pub fn set_constants(&mut self, constants: Vec<LuzObj>) {
        self.constants = constants;
    }

    pub fn set_instructions(&mut self, instructions: Vec<Instruction>) {
        self.instructions = instructions;
    }
}

pub type ScopeRef = Rc<RefCell<Scope>>;

#[derive(Debug)]
pub enum RegOrUpvalue {
    Register(Register),
    Upvalue(Upvalue),
}

impl Scope {
    pub fn get_global(mut scope: Rc<RefCell<Self>>) -> Option<ScopeRef> {
        loop {
            let s = Rc::clone(&scope);
            let s = s.borrow();
            let Some(ref p) = s.parent() else {
                return None;
            };
            let p = Rc::clone(p);
            if p.borrow().is_global() {
                return Some(p);
            } else {
                scope = p;
            }
        }
    }

    pub fn new_ref(name: String, parent: Option<ScopeRef>) -> ScopeRef {
        Rc::new(RefCell::new(Scope::new(Some(name), parent)))
    }

    pub fn new_global() -> Self {
        Self::new(Some(String::from("GLOBAL")), None)
    }

    pub fn is_global(&self) -> bool {
        matches!(self.name.as_deref(), Some("GLOBAL"))
    }

    pub fn next_reg_start(&self) -> usize {
        self.instructions.len() + 2
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

    pub fn named_regs(&self) -> Vec<&Register> {
        self.regs.iter().filter(|reg| reg.name.is_some()).collect()
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

    pub fn set_or_push_reg_val(&mut self, addr: u8, val: LuzObj) {
        if self.regs.len() == addr as usize {
            self.push_reg(RegisterBuilder::default().val(Some(val)));
        } else {
            self.regs[addr as usize].val = Some(val);
        }
    }

    pub fn take_reg_val(&mut self, addr: u8) -> Option<LuzObj> {
        self.regs
            .get_mut(addr as usize)
            .map(|reg| reg.val.take())
            .flatten()
    }

    pub fn push_upval(&mut self, upvalue: Upvalue) {
        self.upvalues.push(upvalue);
    }

    pub fn push_reg(&mut self, reg: &mut RegisterBuilder) {
        reg.addr(self.regs().len() as u8);
        self.regs.push(reg.build().expect("Non valid register"));
    }

    pub fn get_reg_with_name(&self, name: &str) -> Option<&Register> {
        self.regs
            .iter()
            .find(|con| con.name.as_deref() == Some(name))
    }

    pub fn set_reg_with_name(&mut self, name: &str, val: LuzObj) -> Option<()> {
        let reg = self
            .regs
            .iter_mut()
            .find(|con| con.name.as_deref() == Some(name))?;
        reg.val = Some(val);

        Some(())
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
                        let obj = LuzObj::str(register_name);
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

    pub fn get_reg_or_upvalue_value(&mut self, register_name: &str) -> Option<LuzObj> {
        let (in_table, reg_or_upvalue) = self.get_reg_or_upvalue(register_name).ok()?;
        let obj = match reg_or_upvalue {
            RegOrUpvalue::Register(register) => register.val,
            RegOrUpvalue::Upvalue(upvalue) => self.get_upvalue_value(upvalue.addr),
        }?;

        if in_table {
            let table = obj.as_table_or_err().unwrap();
            let table = table.borrow();
            Some(table.rawget(&LuzObj::str(register_name)).clone())
        } else {
            None
        }
    }

    pub fn find_reg(&self, register_name: &str) -> Option<u8> {
        let curr = self.next_reg_start();

        self.regs
            .iter()
            .rfind(|reg| matches!(&reg.name, Some(x) if x == register_name && reg.range.as_ref().is_none_or(|r| r.contains(curr))))
            .map(|reg| reg.addr)
    }

    pub fn set_end_of_register(&mut self, reg: u8) {
        let end = self.next_reg_start() - 1;

        let reg = &mut self.regs[reg as usize];

        let local_r = self.locals.iter_mut().rfind(|local| {
            reg.name.as_ref().is_some_and(|name| {
                name == local.name.as_ref().unwrap()
                    && local.range.as_ref().is_none_or(|range| range.end.is_none())
            })
        });

        if let Some(Some(range)) = local_r.map(|local| &mut local.range) {
            range.end = Some(end);
        }

        reg.free = true;
        reg.name = None;
    }

    pub fn set_register_start(&mut self, reg: u8, name: String) {
        let start = self.next_reg_start() - 1;
        let reg = &mut self.regs[reg as usize];
        reg.name = Some(name);

        let local_r = self.locals.iter_mut().rfind(|local| {
            reg.name
                .as_ref()
                .is_some_and(|reg_name| reg_name == local.name.as_ref().unwrap())
        });

        if let Some(Some(range)) = local_r.map(|local| &mut local.range) {
            range.start = Some(start);
        }

        reg.range = Some(RegisterRange::new(Some(start), None));
    }

    pub fn get_const(&self, addr: u8) -> &LuzObj {
        &self.constants[addr as usize]
    }

    pub fn get_reg(&self, addr: u8) -> &Register {
        &self.regs[addr as usize]
    }

    pub fn get_opt_upvalue(&self, addr: u8) -> Option<&Upvalue> {
        self.upvalues.get(addr as usize)
    }

    pub fn get_mut_opt_upvalue(&mut self, addr: u8) -> Option<&mut Upvalue> {
        self.upvalues.get_mut(addr as usize)
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
        if let Some(link) = &upvalue.link {
            borrowed!(link);
            let p = link
                .parent
                .as_ref()
                .expect("Needs parent to have upvalue")
                .borrow();
            if upvalue.in_stack {
                return p.regs[upvalue.parent_addr as usize].val.clone();
            } else {
                return p.get_upvalue_value(upvalue.parent_addr);
            }
        }
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
        if let Some(link) = &upvalue.link {
            borrowed!(link);
            let mut p = link
                .parent
                .as_ref()
                .expect("Needs parent to have upvalue")
                .borrow_mut();
            if upvalue.in_stack {
                p.regs[upvalue.parent_addr as usize].val.replace(value);
            } else {
                p.set_upvalue_value(upvalue.parent_addr, value);
            }
            return;
        }
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

        let insts_width = (self.instructions.len() + 1).to_string().len() + 4;

        for (i, inst) in self.instructions.iter().enumerate() {
            result += &format!(
                "{:<insts_width$} {}\n",
                format!("[{}]", i + 1),
                inst.debug(self, i + 1)
            );
        }

        result += "---- Constants:\n";
        for (i, inst) in self.constants.iter().enumerate() {
            result += &format!("{} {} {}\n", i, inst.get_type(), inst.repr());
        }

        result += "---- Locals:\n";
        for (i, inst) in self.locals.iter().enumerate() {
            let name = inst.name.clone().unwrap_or(String::from("WHAT"));
            let Some(RegisterRange { start, end }) = inst.range else {
                unreachable!("There must be a range if there is a name")
            };
            result += &format!(
                "{} {} {} {}\n",
                i,
                name,
                start.unwrap_or(0),
                end.unwrap_or(self.instructions().len() + 1)
            );
        }

        result += "---- Upvalues:\n";
        for (i, inst) in self.upvalues.iter().enumerate() {
            result += &format!(
                "{} {} {} {} {}\n",
                i,
                inst.name,
                inst.in_stack,
                inst.parent_addr,
                if inst.link.is_some() { "linked" } else { "" }
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

    pub fn push_sub_scope(&mut self, scope: ScopeRef) {
        self.sub_scopes.push(scope);
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

    pub fn get_line_info(&self, pc: usize) -> Option<&(usize, LineInfo)> {
        self.line_infos.iter().rfind(|(i, _)| *i < pc)
    }

    pub fn line_infos(&self) -> &[(usize, LineInfo)] {
        &self.line_infos
    }

    pub fn push_line_infos(&mut self, line_info: LineInfo) {
        self.line_infos.push((self.instructions().len(), line_info));
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

impl Register {
    // Returns a preview string (all field, but only only puts 'something' as val if val is Some(_))
    pub fn to_string_preview(&self) -> String {
        format!(
            "Register {{ name: {:?}, addr: {}, val: {}, free: {}, range: {:?} }}",
            self.name,
            self.addr,
            if self.val.is_some() {
                "Some(...)"
            } else {
                "None"
            },
            self.free,
            self.range
        )
    }
}

#[derive(Debug, new, Clone)]
pub struct RegisterRange {
    pub start: Option<usize>,

    pub end: Option<usize>,
}

impl RegisterRange {
    pub fn contains(&self, addr: usize) -> bool {
        self.start.is_some_and(|start| start <= addr) && self.end.is_none_or(|end| addr <= end)
    }
}

#[derive(Debug, new, Clone)]
pub struct Upvalue {
    pub name: String,
    pub addr: u8,
    pub parent_addr: u8,
    pub in_stack: bool,
    #[new(default)]
    pub link: Option<ScopeRef>,
}
