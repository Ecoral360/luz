use std::{cell::RefCell, env::var, rc::Rc};

use derive_builder::Builder;
use derive_new::new;

use crate::{
    ast::{AssignStat, Binop, Exp, FuncCall, FunctionDefStat, ReturnStat, Stat},
    compiler::{
        ctx::{CompilerCtx, CompilerCtxBuilder},
        instructions::{Instruction, MAX_HALF_sBx},
        visitor::Visitor,
    },
    luz::{
        err::LuzError,
        obj::{LuzObj, Numeral},
    },
};

pub mod ctx;
pub mod instructions;
pub mod opcode;
pub mod visitor;

type ScopeLink = Rc<RefCell<Scope>>;

#[derive(Debug)]
pub struct Compiler {}

#[derive(Debug, new, Clone)]
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

pub enum RegOrUpvalue {
    Register(Register),
    Upvalue(Upvalue),
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
                        "name {:?} is not declared",
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

    pub fn get_upvalue(&mut self, name: &str) -> Option<&Upvalue> {
        self.upvalues.iter().find(|con| con.name == name)
    }

    pub fn get_upvalue_addr(&mut self, name: &str) -> Option<u8> {
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

#[allow(unused)]
impl Compiler {
    fn handle_exp(
        &mut self,
        ctx: &mut CompilerCtx,
        exp: &Exp,
        supports_immidiate: bool,
    ) -> Result<ExpEval, LuzError> {
        match exp {
            Exp::Literal(lit) => match lit {
                LuzObj::Numeral(Numeral::Int(i))
                    if supports_immidiate && (-128..128).contains(i) =>
                {
                    Ok(ExpEval::InImmediate((*i + 128) as u8))
                }
                LuzObj::Numeral(Numeral::Int(i))
                    if (-(MAX_HALF_sBx as i64)..(MAX_HALF_sBx as i64)).contains(i) =>
                {
                    ctx.push_inst(Instruction::op_loadi(
                        ctx.target_register_or_err()?,
                        *i as u32,
                    ));
                    Ok(ExpEval::InRegister(ctx.target_register_or_err()?))
                }
                _ => Ok(ExpEval::InConstant(ctx.get_or_add_const(lit.clone()) as u8)),
            },
            Exp::Name(name) => {
                let src = ctx.find_reg(name).ok_or(LuzError::CompileError(format!(
                    "name {:?} is not declared",
                    name
                )))?;
                Ok(ExpEval::InRegister(src))
            }
            _ => {
                self.visit_exp(exp, ctx)?;
                Ok(ExpEval::InRegister(ctx.target_register_or_err()?))
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
    fn visit_function_def(
        &mut self,
        ctx: &mut CompilerCtx,
        func_def: &FunctionDefStat,
    ) -> Result<(), LuzError> {
        match func_def {
            FunctionDefStat::Normal {
                name,
                method,
                params,
                body,
            } => todo!(),
            FunctionDefStat::Local { name, params, body } => {
                let reg = ctx.push_claimed_register(Some(name.clone()));
                let idx = ctx.push_scope(name.clone());
                for param in &params.fixed {
                    ctx.push_claimed_register(Some(param.clone()));
                }
                for stat in body {
                    self.visit_stat(stat, ctx)?;
                }
                ctx.pop_scope()?;
                ctx.push_inst(Instruction::op_closure(reg, idx as u32));
            }
        }
        Ok(())
    }

    fn visit_assign(&mut self, ctx: &mut CompilerCtx, assign: &AssignStat) -> Result<(), LuzError> {
        match assign {
            AssignStat::Normal { varlist, explist } => todo!(),
            AssignStat::Local { varlist, explist } => {
                self.visit_local_assign(ctx, assign);
            }
        }
        Ok(())
    }

    fn visit_local_assign(
        &mut self,
        ctx: &mut CompilerCtx,
        assign: &AssignStat,
    ) -> Result<(), LuzError> {
        let AssignStat::Local { varlist, explist } = assign else {
            unreachable!()
        };

        let mut new_ctx =
            ctx.new_with(CompilerCtxBuilder::default().nb_expected((varlist.len() + 1) as u8));

        for var in varlist {
            ctx.push_free_register(Some(var.0.clone()));
        }
        for exp in explist {
            self.visit_exp(exp, &mut new_ctx)?;
        }

        if varlist.len() > explist.len() && !Exp::is_multires(explist) {
            for var in varlist[explist.len()..].iter() {
                let reg = ctx.push_claimed_register(Some(var.0.clone()));
                ctx.push_inst(Instruction::op_loadnil(reg, 0));
            }
        }

        Ok(())
    }

    fn visit_return(&mut self, ctx: &mut CompilerCtx, stat: &ReturnStat) -> Result<(), LuzError> {
        let start = ctx.get_or_push_free_register();
        let size = stat.explist.len();
        let mut new_ctx = if Exp::is_multires(&stat.explist) {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected(0))
        } else {
            ctx.new_with(CompilerCtxBuilder::default().nb_expected((size + 1) as u8))
        };
        for exp in stat.explist.iter() {
            self.visit_exp(exp, &mut new_ctx)?;
            ctx.claim_next_free_register();
        }
        ctx.push_inst(Instruction::op_return(start, false, size as u8 + 1));
        Ok(())
    }

    fn visit_binop(&mut self, ctx: &mut CompilerCtx, exp: &Exp) -> Result<(), LuzError> {
        let Exp::Binop { op, lhs, rhs } = exp else {
            unreachable!()
        };

        let lhs_addr = self.handle_exp(ctx, lhs, matches!(op, Binop::Add))?;
        let rhs_addr = self.handle_exp(ctx, rhs, matches!(op, Binop::Add | Binop::Sub))?;

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
            ExpEval::InImmediate(i) if *op == Binop::Sub => ((i as i32) - 256) as u8,
            ExpEval::InImmediate(i) => i,
            ExpEval::InRegister(r) => r,
            ExpEval::InConstant(c) => c,
        };
        let mut rhs_addr = match rhs_addr {
            ExpEval::InImmediate(i) if *op == Binop::Sub => ((i as i32) - 256) as u8,
            ExpEval::InImmediate(i) => i,
            ExpEval::InRegister(r) => r,
            ExpEval::InConstant(c) => c,
        };

        if is_b_imm {
            (lhs_addr, rhs_addr) = (rhs_addr, lhs_addr);
        }

        if is_c_imm || is_b_imm {
            ctx.push_inst(Instruction::op_addi(
                ctx.target_register_or_err()?,
                lhs_addr,
                false,
                rhs_addr,
            ));
        } else {
            ctx.push_inst(Instruction::op_arithmetic(
                *op,
                ctx.target_register_or_err()?,
                lhs_addr,
                is_b_const,
                rhs_addr,
                is_c_const,
            ));
        }
        Ok(())
    }

    fn visit_function_call(
        &mut self,
        ctx: &mut CompilerCtx,
        f_call: &FuncCall,
    ) -> Result<(), LuzError> {
        let FuncCall {
            func,
            method_name,
            args,
            variadic,
        } = f_call;
        self.visit_exp(func, ctx)?;
        let f_addr = ctx.claim_next_free_register();
        for arg in args {
            self.visit_exp(arg, ctx)?;
            ctx.claim_next_free_register();
        }
        let nb_expected = ctx.nb_expected();
        if args.len() as u8 > nb_expected && nb_expected != 0 {
            ctx.unclaim_registers(
                f_addr + nb_expected - 1,
                args.len() as u8 - (nb_expected - 1),
            );
        }

        ctx.push_inst(Instruction::op_call(
            f_addr,
            if *variadic { 0 } else { (args.len() + 1) as u8 },
            nb_expected,
        ));
        Ok(())
    }

    fn visit_literal(&mut self, ctx: &mut CompilerCtx, lit: &LuzObj) -> Result<(), LuzError> {
        match lit {
            LuzObj::Numeral(Numeral::Int(i)) if *i >= 0 && *i <= 255 => {
                let reg = ctx.get_or_push_free_register();
                ctx.push_inst(Instruction::op_loadi(reg, *i as u32));
                Ok(())
            }
            _ => {
                let addr = ctx.get_or_add_const(lit.clone());
                let reg = ctx.get_or_push_free_register();

                ctx.push_inst(Instruction::op_loadk(reg, addr));
                Ok(())
            }
        }
    }

    fn visit_name(&mut self, ctx: &mut CompilerCtx, name: &str) -> Result<(), LuzError> {
        let (is_intable, src) = ctx.scope_mut().get_reg_or_upvalue(name)?;
        let reg = ctx.get_or_push_free_register();
        match src {
            RegOrUpvalue::Register(src) => ctx.push_inst(Instruction::op_move(reg, src.addr)),
            RegOrUpvalue::Upvalue(src) => {
                if is_intable {
                    let name_k = ctx.get_or_add_const(LuzObj::String(name.to_owned()));
                    ctx.push_inst(Instruction::op_gettabup(reg, src.addr, name_k as u8));
                } else {
                    ctx.push_inst(Instruction::op_getupval(reg, src.addr));
                }
            }
        }

        Ok(())
    }
}

#[allow(unused)]
impl Visitor for Compiler {
    type Return = Result<(), LuzError>;
    type Ctx = CompilerCtx;

    #[must_use]
    fn visit_exp(&mut self, exp: &Exp, ctx: &mut Self::Ctx) -> Self::Return {
        match exp {
            Exp::Literal(luz_obj) => self.visit_literal(ctx, luz_obj),
            Exp::Binop { .. } => self.visit_binop(ctx, exp),
            Exp::Vararg => todo!(),
            Exp::Name(name) => self.visit_name(ctx, name),
            Exp::Var(exp) => todo!(),
            Exp::Unop(unop, exp) => todo!(),
            Exp::CmpOp { op, lhs, rhs } => todo!(),
            Exp::LogicCmpOp { op, lhs, rhs } => todo!(),
            Exp::Access(exp_access) => todo!(),
            Exp::FuncDef(func_def) => todo!(),
            Exp::FuncCall(func_call) => self.visit_function_call(ctx, func_call),
            Exp::TableConstructor(exp_table_constructor) => todo!(),
        }
    }

    #[must_use]
    fn visit_stat(&mut self, stat: &Stat, ctx: &mut Self::Ctx) -> Self::Return {
        match stat {
            Stat::Assign(assign_stat) => self.visit_assign(ctx, assign_stat),
            Stat::Return(return_stat) => self.visit_return(ctx, return_stat),
            Stat::FuncCall(func_call) => self.visit_function_call(ctx, func_call),
            Stat::FunctionDef(function_def_stat) => self.visit_function_def(ctx, function_def_stat),
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
