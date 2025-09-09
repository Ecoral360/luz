use std::collections::HashMap;

use derive_new::new;

use crate::{
    ast::{AssignStat, Binop, Exp, ReturnStat, Stat},
    compiler::{instructions::Instruction, visitor::Visitor},
    luz::{err::LuzError, obj::LuzObj},
};

pub mod instructions;
pub mod opcode;
pub mod visitor;

#[allow(unused)]
#[derive(Debug)]
pub struct Compiler {
    scopes: HashMap<String, Scope>,
    scopes_stack: Vec<String>,
}
impl Default for Compiler {
    fn default() -> Self {
        let mut scopes = HashMap::new();
        scopes.insert("main".to_string(), Scope::default());

        Self {
            scopes,
            scopes_stack: vec![String::from("main")],
        }
    }
}

#[derive(Default, Debug)]
pub struct Scope {
    instructions: Vec<Instruction>,
    constants: Vec<LuzObj>,
    regs: Vec<Register>,
}

#[derive(Debug, new, Clone)]
pub struct Register {
    pub name: Option<String>,
    pub addr: u8,
}

#[allow(unused)]
impl Compiler {
    fn scope(&self) -> &Scope {
        self.scopes
            .get(self.scopes_stack.last().expect("Scope in stack"))
            .expect("Scope in scopes")
    }

    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .get_mut(self.scopes_stack.last().expect("Scope in stack"))
            .expect("Scope in scopes")
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
        for scope_name in self.scopes_stack.iter() {
            let scope = self.scopes.get(scope_name).expect("Scope exists");
            if scope_name == "main" {
                println!("main");
            } else {
                println!("function {:?}", scope_name);
            }
            for (i, inst) in scope.instructions.iter().enumerate() {
                println!("[{}] {}", i + 1, inst);
            }
        }
    }
}

#[allow(unused)]
impl Compiler {
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
        Ok(())
    }

    fn visit_return(&mut self, stat: &ReturnStat) -> Result<(), LuzError> {
        let start = self.scope().regs.len();
        let size = stat.explist.len();
        if size == 0 {
            self.push_inst(Instruction::op_return(0, false, 1));
            return Ok(());
        }
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

        match op {
            Binop::Concat => todo!(),
            Binop::Add => todo!(),
            Binop::Sub => todo!(),
            Binop::Mul => todo!(),
            Binop::FloatDiv => todo!(),
            Binop::FloorDiv => todo!(),
            Binop::Mod => todo!(),
            Binop::Exp => todo!(),
            Binop::BitAnd => todo!(),
            Binop::BitOr => todo!(),
            Binop::BitXor => todo!(),
            Binop::ShiftRight => todo!(),
            Binop::ShiftLeft => todo!(),
        }
        Ok(())
    }

    fn visit_literal(&mut self, lit: &LuzObj) -> Result<(), LuzError> {
        let addr = self.get_or_add_const(lit);

        self.push_inst(Instruction::op_loadk(
            self.target_register().expect("Register to load literal"),
            addr,
        ));
        Ok(())
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
            Stat::FunctionDef(function_def_stat) => todo!(),
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
