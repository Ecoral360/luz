use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::{
    ast::Binop,
    compiler::{
        instructions::{self, iABC, iABx, Instruction},
        opcode::LuaOpCode,
        Register, Scope,
    },
    luz::{err::LuzError, obj::{LuzObj, Numeral}},
};

#[allow(unused)]
pub struct Runner {
    scope: Rc<RefCell<Scope>>,
}

#[allow(unused)]
impl Runner {
    pub fn new(scope: Rc<RefCell<Scope>>) -> Self {
        Self { scope }
    }

    fn scope(&self) -> Ref<Scope> {
        self.scope.borrow()
    }

    fn scope_mut(&mut self) -> RefMut<Scope> {
        self.scope.borrow_mut()
    }

    pub fn run(&mut self) -> Result<Vec<LuzObj>, LuzError> {
        let instructions = self.scope().instructions().clone();
        let mut rets = vec![];
        for instruction in &instructions {
            if let Some(ret) = self.run_instruction(instruction)? {
                rets = ret;
                break;
            }
        }
        Ok(rets)
    }

    fn run_instruction(
        &mut self,
        instruction: &Instruction,
    ) -> Result<Option<Vec<LuzObj>>, LuzError> {
        match instruction {
            Instruction::iABC(i_abc) => match i_abc.op {
                LuaOpCode::OP_ADD => self.run_add(i_abc)?,
                LuaOpCode::OP_ADDI => {
                    let iABC { c, b, k, a, .. } = *i_abc;

                    let lhs = if k {
                        self.scope.borrow_mut().constants()[b as usize].clone()
                    } else {
                        self.scope.borrow_mut().regs()[b as usize]
                            .val
                            .as_ref()
                            .expect("A value")
                            .clone()
                    };

                    let c = from_excess_of_bx(c as u32);

                    let rhs = LuzObj::Numeral(Numeral::Int(c as i64));

                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, lhs.apply_binop(Binop::Add, rhs)?);
                }
                LuaOpCode::OP_ADDK => {
                    let iABC { c, b, k, a, .. } = *i_abc;

                    let lhs = if k {
                        self.scope.borrow_mut().constants()[b as usize].clone()
                    } else {
                        self.scope.borrow_mut().regs()[b as usize]
                            .val
                            .as_ref()
                            .expect("A value")
                            .clone()
                    };

                    let rhs = self.scope.borrow_mut().constants()[c as usize].clone();

                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, lhs.apply_binop(Binop::Add, rhs)?);
                }
                LuaOpCode::OP_MOVE => {
                    let iABC { a, b, .. } = *i_abc;
                    let val = self.scope.borrow_mut().regs()[b as usize]
                        .val
                        .as_ref()
                        .expect("A value")
                        .clone();
                    self.scope.borrow_mut().set_reg_val(a, val);
                }
                LuaOpCode::OP_RETURN => {
                    let iABC { a, b, .. } = *i_abc;
                    let mut rets = vec![];
                    for i in 0..b - 1 {
                        let val = self.scope.borrow_mut().regs()[(a + i) as usize]
                            .val
                            .as_ref()
                            .expect("A value")
                            .clone();
                        rets.push(val);
                    }
                    return Ok(Some(rets));
                }
                op => todo!("iABC {:?}", op),
            },
            Instruction::iABx(i_abx) => match i_abx.op {
                LuaOpCode::OP_LOADK => self.run_loadk(i_abx)?,
                LuaOpCode::OP_LOADI => {
                    let iABx { b, a, .. } = *i_abx;
                    let imm = from_excess_of_bx(b);
                    self.scope.borrow_mut().set_reg_val(a, LuzObj::Numeral(Numeral::Int(imm as i64)));
                }
                op => todo!("iABx {:?}", op),
            },
        }
        Ok(None)
    }

    fn run_loadk(&mut self, code: &iABx) -> Result<(), LuzError> {
        let iABx { b, a, op } = *code;

        let constant = self.scope.borrow().constants()[b as usize].clone();
        self.scope.borrow_mut().set_reg_val(a, constant);

        Ok(())
    }

    fn run_add(&mut self, code: &iABC) -> Result<(), LuzError> {
        let iABC { c, b, k, a, op } = *code;

        let lhs = if k {
            self.scope.borrow_mut().constants()[b as usize].clone()
        } else {
            self.scope.borrow_mut().regs()[b as usize]
                .val
                .as_ref()
                .expect("A value")
                .clone()
        };

        let rhs = self.scope.borrow_mut().regs()[c as usize]
            .val
            .as_ref()
            .expect("A value")
            .clone();

        self.scope
            .borrow_mut()
            .set_reg_val(a, lhs.apply_binop(Binop::Add, rhs)?);

        Ok(())
    }
}

fn from_excess_of_bx(val: u32) -> i32 {
    // (val as i32) - 131071
    val as i32
}

