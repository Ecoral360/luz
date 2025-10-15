use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
    usize,
};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    ast::LineInfo,
    borrowed,
    compiler::{
        instructions::{self, Instruction},
        opcode::LuaOpCode,
    },
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
        scope.upvalues.push(Rc::new(RefCell::new(Upvalue::new(
            "_ENV".to_owned(),
            0,
            0,
            true,
        ))));
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
        mut upvalues: Vec<UpvalRef>,
    ) -> Self {
        let mut scope = Scope::new(Some(name), parent_scope.or(Some(get_builtin_scope())));
        scope.upvalues.push(Rc::new(RefCell::new(Upvalue::new(
            "_ENV".to_owned(),
            0,
            0,
            true,
        ))));

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
        new_scope.borrow_mut().start_pc = self.scope().instructions.len();
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

    pub fn needs_to_be_closed(&self) -> bool {
        self.scope
            .borrow()
            .instructions
            .iter()
            .any(|inst| inst.op() == LuaOpCode::OP_CLOSURE)
    }
}

#[derive(Debug, new, Clone)]
pub struct Scope {
    name: Option<String>,
    parent: Option<ScopeRef>,

    #[new(default)]
    start_pc: usize,

    #[new(default)]
    instructions: Vec<Instruction>,

    #[new(default)]
    line_infos: Vec<(usize, LineInfo)>,

    #[new(default)]
    constants: Vec<LuzObj>,

    #[new(default)]
    regs: Vec<Register>,

    #[new(default)]
    upvalues: Vec<Rc<RefCell<Upvalue>>>,

    #[new(default)]
    sub_scopes: Vec<ScopeRef>,

    #[new(default)]
    nb_params: u32,

    #[new(default)]
    vararg: Vec<LuzObj>,

    #[new(default)]
    locals: Vec<Register>,

    #[new(default)]
    labels: HashMap<String, usize>,

    #[new(default)]
    scopes_tbc: Vec<ScopeRef>,
}

impl Scope {
    pub fn make_tailcall_closure(&self) -> Rc<RefCell<Self>> {
        let scope = Rc::new(RefCell::new(Self {
            start_pc: self.start_pc.clone(),
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
            labels: self.labels.clone(),
            scopes_tbc: vec![],
        }));

        // Put the new current scope as the parent scope for its subscopes
        {
            for sub_scope in scope.borrow_mut().sub_scopes.iter_mut() {
                sub_scope.borrow_mut().parent = Some(Rc::clone(&scope));
            }
        }

        scope
    }

    /// Makes a new instance of the scope for a function call
    pub fn make_instance(&self) -> Rc<RefCell<Self>> {
        let mut scope = Self {
            start_pc: self.start_pc.clone(),
            name: self.name.clone(),
            parent: self.parent.as_ref().map(|p| Rc::clone(p)),
            instructions: self.instructions.clone(),
            line_infos: self.line_infos.clone(),
            constants: self.constants.clone(),
            regs: self.regs.clone(),
            upvalues: self.upvalues.iter().map(|up| Rc::clone(&up)).collect(),
            sub_scopes: self.sub_scopes.clone(),
            nb_params: self.nb_params.clone(),
            vararg: self.vararg.clone(),
            locals: self.locals.clone(),
            labels: self.labels.clone(),
            scopes_tbc: vec![],
        };

        // for upval in scope.upvalues.iter_mut() {
        //     if upval.in_stack && upval.val.is_none() {
        //         upval.val = Some(self.get_upvalue_value(upval.addr).unwrap_or(LuzObj::Nil));
        //     }
        // }

        let scope = Rc::new(RefCell::new(scope));

        // Put the new current scope as the parent scope for its subscopes
        {
            for sub_scope in scope.borrow_mut().sub_scopes.iter_mut() {
                sub_scope.borrow_mut().parent = Some(Rc::clone(&scope));
            }
        }

        scope
    }

    // pub fn update_upval_from_instance(&mut self, instance: ScopeRef) {
    //     borrowed!(instance);
    //     for upval in self.upvalues.iter() {
    //         let inst_upval = instance
    //             .upvalues
    //             .iter()
    //             .find(|inst_upval| inst_upval.borrow().is_same_as(&upval.borrow()))
    //             .expect("Instance and parent should have the same upvalues");
    //
    //         borrowed!(inst_upval);
    //         borrowed!(mut upval);
    //         upval.link = inst_upval.link.clone();
    //         upval.val = inst_upval.val.clone();
    //     }
    // }

    /// closes a subscope
    pub fn make_closure(&mut self, sub_scope_idx: usize) -> Rc<RefCell<Self>> {
        let scope = {
            let sub_scope = self.sub_scopes[sub_scope_idx].borrow();
            Self {
                start_pc: sub_scope.start_pc.clone(),
                name: sub_scope.name.clone(),
                parent: sub_scope.parent.as_ref().map(|p| Rc::clone(p)),
                instructions: sub_scope.instructions.clone(),
                line_infos: sub_scope.line_infos.clone(),
                constants: sub_scope.constants.clone(),
                regs: sub_scope.regs.clone(),
                upvalues: sub_scope.upvalues.iter().map(|up| Rc::clone(&up)).collect(),
                sub_scopes: sub_scope.sub_scopes.clone(),
                nb_params: sub_scope.nb_params.clone(),
                vararg: sub_scope.vararg.clone(),
                locals: sub_scope.locals.clone(),
                labels: sub_scope.labels.clone(),
                scopes_tbc: vec![],
            }
        };

        let scope = Rc::new(RefCell::new(scope));

        self.scopes_tbc.push(Rc::clone(&scope));

        // Put the new current scope as the parent scope for its subscopes
        {
            for sub_scope in scope.borrow_mut().sub_scopes.iter_mut() {
                sub_scope.borrow_mut().parent = Some(Rc::clone(&scope));
            }
        }

        scope
    }

    fn make_instance2(&self) -> Rc<RefCell<Self>> {
        let scope = Self {
            start_pc: self.start_pc.clone(),
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
            labels: self.labels.clone(),
            scopes_tbc: vec![],
        };

        Rc::new(RefCell::new(scope))
    }
    pub fn close(scope: ScopeRef, reg: Option<u8>) {
        let new_parent = scope.borrow().make_instance2();
        match reg {
            Some(reg) => {
                for sub_scope in &scope.borrow().scopes_tbc {
                    let mut sub_scope = sub_scope.borrow_mut();
                    let new_sub_scope = sub_scope.make_instance2();
                    // new_sub_scope.borrow_mut().parent = Some(Rc::clone(&scope));

                    for upval in sub_scope.upvalues.iter_mut() {
                        if upval.borrow().in_stack && upval.borrow().parent_addr >= reg {
                            // let val = new_parent
                            //     .borrow()
                            //     .get_reg(upval.borrow().parent_addr)
                            //     .val
                            //     .clone();
                            // borrowed!(mut upval);
                            // upval.val = val;
                        } else {
                            borrowed!(mut upval);
                            upval.link = Some(Rc::clone(&new_sub_scope));
                        }
                    }
                    sub_scope.parent = Some(Rc::clone(&new_parent));
                }
            }
            None => {
                for sub_scope in &scope.borrow().sub_scopes {
                    let mut sub_scope = sub_scope.borrow_mut();
                    let new_sub_scope = sub_scope.make_instance2();
                    // new_sub_scope.borrow_mut().parent = Some(Rc::clone(&scope));

                    for upval in sub_scope.upvalues.iter_mut() {
                        borrowed!(mut upval);
                        if upval.in_stack && upval.link.is_some() {
                            upval.link = Some(Rc::clone(&new_sub_scope));
                        }
                    }
                    sub_scope.parent = Some(Rc::clone(&new_parent));
                }
            }
        }
        // new_parent.borrow_mut().parent = Some(Rc::clone(&scope));
        scope.borrow_mut().scopes_tbc.clear()
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

    pub fn locals(&self) -> &[Register] {
        &self.locals
    }

    pub fn live_locals(&self, pc: usize) -> Vec<Register> {
        self.locals
            .iter()
            .filter(|local| {
                if let Some(range) = &local.range {
                    range.end.is_some() && range.contains(pc)
                } else {
                    false
                }
            })
            .cloned()
            .collect::<Vec<Register>>()
    }
}

pub type ScopeRef = Rc<RefCell<Scope>>;
pub type UpvalRef = Rc<RefCell<Upvalue>>;

#[derive(Debug)]
pub enum RegOrUpvalue {
    Register(Register),
    Upvalue(UpvalRef),
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

    pub fn upvalues(&self) -> &Vec<UpvalRef> {
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
        self.upvalues.push(Rc::new(RefCell::new(upvalue)));
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
            return Ok((false, RegOrUpvalue::Upvalue(up_addr)));
        }

        if let Some(env_addr) = self.get_reg_with_name("_ENV") {
            return Ok((true, RegOrUpvalue::Register(env_addr.clone())));
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
                    Rc::new(RefCell::new(Upvalue::new(
                        register.name.unwrap(),
                        addr,
                        register.addr,
                        true,
                    ))),
                ),
                RegOrUpvalue::Upvalue(upvalue) => {
                    borrowed!(upvalue);
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
                        Rc::new(RefCell::new(Upvalue::new(
                            upvalue.name.clone(),
                            addr,
                            upvalue.addr,
                            false,
                        ))),
                    )
                }
            };

            let existing_upvalue = self
                .upvalues
                .iter()
                .find(|up| up.borrow().name == upvalue.borrow().name);
            match existing_upvalue {
                Some(existing_upvalue) => {
                    Ok((in_table, RegOrUpvalue::Upvalue(Rc::clone(existing_upvalue))))
                }
                None => {
                    self.upvalues.push(Rc::clone(&upvalue));
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
            RegOrUpvalue::Upvalue(upvalue) => self.get_upvalue_value(upvalue.borrow().addr),
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

    pub fn get_opt_upvalue(&self, addr: u8) -> Option<UpvalRef> {
        self.upvalues.get(addr as usize).map(|u| Rc::clone(u))
    }

    pub fn get_upvalue(&self, addr: u8) -> UpvalRef {
        Rc::clone(&self.upvalues[addr as usize])
    }

    pub fn get_upvalue_with_name(&self, name: &str) -> Option<UpvalRef> {
        self.upvalues
            .iter()
            .find(|con| con.borrow().name == name)
            .map(|u| Rc::clone(u))
    }

    pub fn get_upvalue_addr(&self, name: &str) -> Option<u8> {
        self.upvalues.iter().enumerate().find_map(|(i, con)| {
            if con.borrow().name == name {
                Some(i as u8)
            } else {
                None
            }
        })
    }

    pub fn get_upvalue_value(&self, upvalue_addr: u8) -> Option<LuzObj> {
        let upvalue = &self.upvalues[upvalue_addr as usize];

        if let Some(value) = &upvalue.borrow().val {
            return Some(value.clone());
        }

        if let Some(link) = &upvalue.borrow().link {
            borrowed!(link);
            let p = link
                .parent
                .as_ref()
                .expect("Needs parent to have upvalue")
                .borrow();
            if upvalue.borrow().in_stack {
                return p.regs[upvalue.borrow().parent_addr as usize].val.clone();
            } else {
                return p.get_upvalue_value(upvalue.borrow().parent_addr);
            }
        }
        let p = self
            .parent
            .as_ref()
            .expect("Needs parent to have upvalue")
            .borrow();
        if upvalue.borrow().in_stack {
            p.regs[upvalue.borrow().parent_addr as usize].val.clone()
        } else {
            p.get_upvalue_value(upvalue.borrow().parent_addr)
        }
    }

    pub fn set_upvalue_value(&mut self, upvalue_addr: u8, value: LuzObj) {
        let upvalue = &self.upvalues[upvalue_addr as usize];

        borrowed!(mut upvalue);

        if upvalue.val.is_some() {
            upvalue.val.replace(value);
            return;
        }

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
            borrowed!(inst);
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

    pub fn insert_label(&mut self, label: String, pos: usize) -> bool {
        if self.labels.contains_key(&label) {
            false
        } else {
            self.labels.insert(label, pos);
            true
        }
    }

    pub fn get_label(&self, label: &str) -> Option<usize> {
        self.labels.get(label).cloned()
    }

    pub fn labels(&self) -> &HashMap<String, usize> {
        &self.labels
    }

    pub fn set_labels(&mut self, labels: HashMap<String, usize>) {
        self.labels = labels;
    }

    pub fn remove_new_labels(&mut self, old_labels: HashMap<String, usize>) {
        let mut to_remove = vec![];
        for key in self.labels.keys() {
            if !old_labels.contains_key(key) {
                to_remove.push(key.clone());
            }
        }
        for key in to_remove {
            self.labels.remove(&key);
        }
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
    /// Only use is to make debug.upvaluejoin possible
    /// when defined, it will use this to look up the value instead of the scope parent
    #[new(default)]
    pub link: Option<ScopeRef>,

    #[new(default)]
    pub val: Option<LuzObj>,
}

impl Upvalue {
    pub fn is_same_as(&self, other: &Upvalue) -> bool {
        self.name == other.name
            && self.addr == other.addr
            && self.in_stack == other.in_stack
            && self.parent_addr == other.parent_addr
    }
}
