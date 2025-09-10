use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use derive_new::new;

use crate::{
    ast::{AssignStat, Binop, Exp, FunctionDefStat, ReturnStat, Stat},
    compiler::{instructions::Instruction, visitor::Visitor},
    luz::{
        err::LuzError,
        obj::{LuzObj, Numeral},
    },
};

pub mod instructions;
pub mod opcode;
pub mod visitor;

type ScopeLink = Rc<RefCell<Scope>>;

#[allow(unused)]
#[derive(Debug)]
pub struct Compiler {
    scope: ScopeLink,
}
impl Default for Compiler {
    fn default() -> Self {
        Self {
            scope: Rc::new(RefCell::new(Scope::new(String::from("main"), None))),
        }
    }
}

#[derive(Debug, new)]
pub struct Scope {
    name: String,
    parent: Option<ScopeLink>,

    #[new(default)]
    instructions: Vec<Instruction>,
    #[new(default)]
    constants: Vec<LuzObj>,
    #[new(default)]
    regs: Vec<Register>,

    #[new(default)]
    upvalues: Vec<Upvalue>,

    #[new(default)]
    sub_scopes: Vec<ScopeLink>,
}

impl Scope {
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
            if let Some(name) = &inst.name {
                println!("{} {} {} {}", i, name, inst.in_stack, inst.parent_addr);
            }
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

#[derive(Debug, new, Clone)]
pub struct Register {
    pub name: Option<String>,
    pub addr: u8,
    #[new(default)]
    pub val: Option<LuzObj>,
}
#[derive(Debug, new, Clone)]
pub struct Upvalue {
    pub name: Option<String>,
    pub addr: u8,
    pub parent_addr: u8,
    pub in_stack: bool,
}

#[allow(unused)]
impl Compiler {
    pub fn scope_clone(&self) -> ScopeLink {
        Rc::clone(&self.scope)
    }

    fn scope(&self) -> Ref<Scope> {
        self.scope.borrow()
    }

    fn scope_mut(&mut self) -> RefMut<Scope> {
        self.scope.borrow_mut()
    }

    fn push_scope(&mut self, scope_name: String) -> usize {
        let new_scope = Rc::new(RefCell::new(Scope::new(
            scope_name,
            Some(Rc::clone(&self.scope)),
        )));
        let idx = self.scope_mut().sub_scopes().len();
        self.scope_mut().sub_scopes.push(Rc::clone(&new_scope));
        self.scope = new_scope;
        idx
    }

    fn pop_scope(&mut self) -> Result<(), LuzError> {
        let parent = Rc::clone(&self.scope);
        let parent = parent.borrow();
        let parent = parent
            .parent
            .as_ref()
            .ok_or_else(|| LuzError::CompileError(format!("No parent scope")))?;

        self.scope = Rc::clone(parent);
        Ok(())
    }

    fn target_register(&self) -> Option<u8> {
        self.scope().regs.last().map(|reg| reg.addr)
    }

    fn target_register_or_err(&self) -> Result<u8, LuzError> {
        self.scope()
            .regs
            .last()
            .map(|reg| reg.addr)
            .ok_or_else(|| LuzError::CompileError(format!("No target registers")))
    }

    fn find_reg(&self, register_name: &str) -> Option<u8> {
        self.scope()
            .regs
            .iter()
            .find(|reg| matches!(&reg.name, Some(x) if x == register_name))
            .map(|reg| reg.addr)
    }

    fn push_register(&mut self, register_name: Option<String>) {
        let reg = Register::new(register_name, self.scope().regs.len() as u8);
        self.scope_mut().regs.push(reg);
    }

    fn push_inst(&mut self, inst: Instruction) {
        self.scope_mut().instructions.push(inst);
    }

    fn get_or_add_const(&mut self, obj: &LuzObj) -> u32 {
        let mut scope = self.scope_mut();
        let addr = scope
            .constants
            .iter()
            .enumerate()
            .find(|(i, con)| *con == obj)
            .map(|(i, con)| i);

        if let Some(addr) = addr {
            return addr as u32;
        }

        scope.constants.push(obj.clone());
        (scope.constants.len() - 1) as u32
    }

    pub fn instructions(&self) -> Vec<Instruction> {
        self.scope().instructions.clone()
    }

    pub fn print_instructions(&self) {
        self.scope.borrow().print_instructions();
    }

    fn handle_exp(&mut self, exp: &Exp, supports_immidiate: bool) -> Result<ExpEval, LuzError> {
        match exp {
            Exp::Literal(lit) => match lit {
                LuzObj::Numeral(Numeral::Int(i)) if supports_immidiate && (0..128).contains(i) => {
                    Ok(ExpEval::InImmediate(*i as u8))
                }
                _ => Ok(ExpEval::InConstant(self.get_or_add_const(lit) as u8)),
            },
            Exp::Name(name) => {
                let src = self.find_reg(name).ok_or(LuzError::CompileError(format!(
                    "name {:?} is not declared",
                    name
                )))?;
                Ok(ExpEval::InRegister(src))
            }
            _ => {
                self.visit_exp(exp)?;
                Ok(ExpEval::InRegister(self.target_register_or_err()?))
            }
        }
    }
}

enum ExpEval {
    InImmediate(u8),
    InRegister(u8),
    InConstant(u8),
}

#[allow(unused)]
impl Compiler {
    fn visit_function_def(&mut self, func_def: &FunctionDefStat) -> Result<(), LuzError> {
        match func_def {
            FunctionDefStat::Normal {
                name,
                method,
                params,
                body,
            } => todo!(),
            FunctionDefStat::Local { name, params, body } => {
                self.push_register(Some(name.clone()));
                let idx = self.push_scope(name.clone());
                for param in &params.fixed {
                    self.push_register(Some(param.clone()));
                }
                for stat in body {
                    self.visit_stat(stat)?;
                }
                self.pop_scope()?;
                self.push_inst(Instruction::op_closure(
                    self.target_register_or_err()?,
                    idx as u32,
                ));
            }
        }
        Ok(())
    }

    fn visit_assign(&mut self, assign: &AssignStat) -> Result<(), LuzError> {
        match assign {
            AssignStat::Normal { varlist, explist } => todo!(),
            AssignStat::Local { varlist, explist } => {
                self.visit_local_assign(assign);
            }
        }
        Ok(())
    }

    fn visit_local_assign(&mut self, assign: &AssignStat) -> Result<(), LuzError> {
        let AssignStat::Local { varlist, explist } = assign else {
            unreachable!()
        };

        for (var, exp) in varlist.iter().zip(explist) {
            self.push_register(Some(var.0.clone()));
            self.visit_exp(exp);
        }

        if varlist.len() > explist.len() {
            for var in varlist[explist.len()..].iter() {
                self.push_register(Some(var.0.clone()));
                self.push_inst(Instruction::op_loadnil(self.target_register_or_err()?, 0));
            }
        }

        Ok(())
    }

    fn visit_return(&mut self, stat: &ReturnStat) -> Result<(), LuzError> {
        let start = self.scope().regs.len();
        let size = stat.explist.len();
        for exp in stat.explist.iter() {
            self.push_register(None);
            self.visit_exp(exp);
        }
        self.push_inst(Instruction::op_return(start as u8, false, size as u8 + 1));
        Ok(())
    }

    fn visit_binop(&mut self, exp: &Exp) -> Result<(), LuzError> {
        let Exp::Binop { op, lhs, rhs } = exp else {
            unreachable!()
        };

        let lhs_addr = self.handle_exp(lhs, matches!(op, Binop::Add))?;
        let rhs_addr = self.handle_exp(rhs, matches!(op, Binop::Add | Binop::Sub))?;

        let is_b_const = matches!(lhs_addr, ExpEval::InConstant(_));
        let is_b_imm = matches!(lhs_addr, ExpEval::InImmediate(_));

        let is_c_const = matches!(rhs_addr, ExpEval::InConstant(_));
        let is_c_imm = matches!(rhs_addr, ExpEval::InImmediate(_));

        if is_b_imm && is_c_imm {
            return Err(LuzError::CompileError(format!(
                "Both sides of {:?} cannot be immediate values, should have been optimized away before codegen",
                op
            )));
        }

        let mut lhs_addr = match lhs_addr {
            ExpEval::InImmediate(i) if *op == Binop::Add => i + 128,
            ExpEval::InImmediate(i) if *op == Binop::Sub => (-(i as i32) + 128) as u8,
            ExpEval::InImmediate(i) => i,
            ExpEval::InRegister(r) => r,
            ExpEval::InConstant(c) => c,
        };
        let mut rhs_addr = match rhs_addr {
            ExpEval::InImmediate(i) if *op == Binop::Add => i + 128,
            ExpEval::InImmediate(i) if *op == Binop::Sub => (-(i as i32) + 128) as u8,
            ExpEval::InImmediate(i) => i,
            ExpEval::InRegister(r) => r,
            ExpEval::InConstant(c) => c,
        };

        if is_b_imm {
            (lhs_addr, rhs_addr) = (rhs_addr, lhs_addr);
        }

        if is_c_imm || is_b_imm {
            self.push_inst(Instruction::op_addi(
                self.target_register_or_err()?,
                lhs_addr,
                false,
                rhs_addr,
            ));
        } else {
            self.push_inst(Instruction::op_arithmetic(
                *op,
                self.target_register_or_err()?,
                lhs_addr,
                is_b_const,
                rhs_addr,
                is_c_const,
            ));
        }
        Ok(())
    }

    fn visit_literal(&mut self, lit: &LuzObj) -> Result<(), LuzError> {
        match lit {
            LuzObj::Numeral(Numeral::Int(i)) if *i >= 0 && *i <= 255 => {
                self.push_inst(Instruction::op_loadi(
                    self.target_register().expect("Register to load literal"),
                    *i as u32,
                ));
                Ok(())
            }
            _ => {
                let addr = self.get_or_add_const(lit);

                self.push_inst(Instruction::op_loadk(
                    self.target_register().expect("Register to load literal"),
                    addr,
                ));
                Ok(())
            }
        }
    }

    fn visit_name(&mut self, name: &str) -> Result<(), LuzError> {
        let src = self.find_reg(name).ok_or(LuzError::CompileError(format!(
            "name {:?} is not declared",
            name
        )))?;

        self.push_inst(Instruction::op_move(self.target_register_or_err()?, src));
        Ok(())
    }
}

#[allow(unused)]
impl Visitor for Compiler {
    type Return = Result<(), LuzError>;

    fn visit_exp(&mut self, exp: &Exp) -> Self::Return {
        match exp {
            Exp::Literal(luz_obj) => self.visit_literal(luz_obj),
            Exp::Binop { .. } => self.visit_binop(exp),
            Exp::Ellipsis => todo!(),
            Exp::Name(name) => self.visit_name(name),
            Exp::Var(exp) => todo!(),
            Exp::Unop(unop, exp) => todo!(),
            Exp::CmpOp { op, lhs, rhs } => todo!(),
            Exp::LogicCmpOp { op, lhs, rhs } => todo!(),
            Exp::Access(exp_access) => todo!(),
            Exp::FuncDef(func_def) => todo!(),
            Exp::FuncCall(func_call) => todo!(),
            Exp::TableConstructor(exp_table_constructor) => todo!(),
        }
    }

    fn visit_stat(&mut self, stat: &Stat) -> Self::Return {
        match stat {
            Stat::Assign(assign_stat) => self.visit_assign(assign_stat),
            Stat::Return(return_stat) => self.visit_return(return_stat),
            Stat::FuncCall(func_call) => todo!(),
            Stat::FunctionDef(function_def_stat) => self.visit_function_def(function_def_stat),
            Stat::Do(do_stat) => todo!(),
            Stat::While(while_stat) => todo!(),
            Stat::Repeat(repea_stat) => todo!(),
            Stat::If(if_stat) => todo!(),
            Stat::ForRange(for_range_stat) => todo!(),
            Stat::ForIn(for_in_stat) => todo!(),
            Stat::Break => todo!(),
            Stat::Goto(goto_stat) => todo!(),
            Stat::Label(label_stat) => todo!(),
        }
    }
}
