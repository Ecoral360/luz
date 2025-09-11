use std::{
    cell::{Ref, RefCell, RefMut},
    rc::Rc,
};

use crate::{
    ast::{Binop, CmpOp},
    compiler::{
        instructions::{iABC, iABx, iAsBx, isJ, Instruction, MAX_HALF_sBx, MAX_HALF_sJ},
        opcode::LuaOpCode,
        Scope,
    },
    luz::{
        err::LuzError,
        obj::{LuzFunction, LuzObj, LuzType, Numeral},
    },
};

pub enum InstructionResult {
    Continue,
    Skip(u32),
    Return(Vec<LuzObj>),
}

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
        let mut instruction_iter = instructions.iter();
        while let Some(instruction) = instruction_iter.next() {
            rets = match self.run_instruction(instruction)? {
                InstructionResult::Continue => continue,
                InstructionResult::Skip(n) => {
                    for _ in 0..n {
                        instruction_iter.next();
                    }
                    continue;
                }
                InstructionResult::Return(rets) => rets,
            };
            break;
        }
        Ok(rets)
    }

    fn get_reg_val_or_const(&self, val: u8, is_const: bool) -> Result<LuzObj, LuzError> {
        if is_const {
            self.get_const_val(val)
        } else {
            self.get_reg_val(val)
        }
    }

    fn get_reg_val(&self, reg: u8) -> Result<LuzObj, LuzError> {
        let val = self.scope.borrow_mut().regs()[reg as usize]
            .val
            .as_ref()
            .expect("A value")
            .clone();
        Ok(val)
    }

    fn get_const_val(&self, idx: u8) -> Result<LuzObj, LuzError> {
        let val = self.scope.borrow_mut().constants()[idx as usize].clone();
        Ok(val)
    }

    fn run_instruction(
        &mut self,
        instruction: &Instruction,
    ) -> Result<InstructionResult, LuzError> {
        match instruction {
            Instruction::iABC(i_abc @ iABC { c, b, k, a, op }) => match op {
                LuaOpCode::OP_LT | LuaOpCode::OP_LE => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = self.get_reg_val(*b)?;

                    let res = lhs.apply_cmp(
                        if *op == LuaOpCode::OP_LT {
                            CmpOp::Lt
                        } else {
                            CmpOp::LtEq
                        },
                        rhs,
                    )?;

                    if res.is_truthy() != *k {
                        return Ok(InstructionResult::Skip(1));
                    }
                }
                LuaOpCode::OP_LOADTRUE => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(true));
                }
                LuaOpCode::OP_LOADFALSE => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(true));
                }
                LuaOpCode::OP_LFALSESKIP => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(false));
                    return Ok(InstructionResult::Skip(1));
                }
                LuaOpCode::OP_ADD
                | LuaOpCode::OP_ADDK
                | LuaOpCode::OP_SUB
                | LuaOpCode::OP_SUBK
                | LuaOpCode::OP_MUL
                | LuaOpCode::OP_MULK
                | LuaOpCode::OP_DIV
                | LuaOpCode::OP_DIVK
                | LuaOpCode::OP_IDIV
                | LuaOpCode::OP_IDIVK => self.run_arithmetic(i_abc)?,

                LuaOpCode::OP_ADDI => {
                    let iABC { c, b, k, a, .. } = *i_abc;

                    let lhs = self.get_reg_val_or_const(b, k)?;

                    let c = (c as i64) - 128; // from excess-128

                    let rhs = LuzObj::Numeral(Numeral::Int(c));

                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, lhs.apply_binop(Binop::Add, rhs)?);
                }
                LuaOpCode::OP_MOVE => {
                    let iABC { a, b, .. } = *i_abc;
                    let val = self.get_reg_val(b)?;
                    self.scope.borrow_mut().set_reg_val(a, val);
                }
                LuaOpCode::OP_RETURN => {
                    let iABC { a, b, .. } = *i_abc;
                    let mut rets = vec![];
                    for i in 0..b - 1 {
                        rets.push(self.get_reg_val(a + i)?);
                    }
                    return Ok(InstructionResult::Return(rets));
                }
                LuaOpCode::OP_CALL => {
                    let iABC {
                        c: nb_expected_results,
                        b: nb_args,
                        a: func_addr,
                        ..
                    } = *i_abc;

                    let func = self.get_reg_val(func_addr)?;
                    let LuzObj::Function(f) = func else {
                        return Err(LuzError::Type {
                            wrong: func.get_type(),
                            expected: vec![LuzType::Function],
                        });
                    };

                    let args = (func_addr + 1..func_addr + nb_args)
                        .map(|arg_addr| self.get_reg_val(arg_addr))
                        .collect::<Result<Vec<_>, _>>()?;

                    let f = f.borrow();
                    let results = match *f {
                        LuzFunction::User { ref scope } => {
                            let mut fc_scope = scope.borrow().clone();
                            for (i, arg) in args.into_iter().enumerate() {
                                fc_scope.set_reg_val(i as u8, arg);
                            }
                            let mut fc_runner = Runner::new(Rc::new(RefCell::new(fc_scope)));

                            fc_runner.run()?
                        }
                        LuzFunction::Native { ref fn_ptr } => {
                            let mut fn_ptr = fn_ptr.borrow_mut();
                            (fn_ptr)(self, args)?
                        }
                    };

                    let nb_results = results.len() as u8;
                    if nb_expected_results == 0 {
                        // TODO: check we have enough registers
                        for (i, result) in results.into_iter().enumerate() {
                            self.scope_mut().set_reg_val(func_addr + i as u8, result);
                        }
                    } else {
                        for (i, result) in results.into_iter().enumerate() {
                            if i == (nb_expected_results - 1) as usize {
                                break;
                            };
                            self.scope_mut().set_reg_val(func_addr + i as u8, result);
                        }
                        if (nb_expected_results - 1) > nb_results {
                            let diff = nb_expected_results - 1 - nb_results;
                            dbg!(diff);
                            for addr in 0..diff {
                                self.scope_mut()
                                    .set_reg_val(func_addr + diff + addr, LuzObj::Nil);
                            }
                        }
                    }
                }
                LuaOpCode::OP_GETTABUP => {
                    let iABC { a, b, c, .. } = *i_abc;
                    let table = self
                        .scope()
                        .get_upvalue_value(b)
                        .ok_or(LuzError::CompileError("Upvalue missing".to_owned()))?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_const_val(c)?;
                    let table = table.borrow();
                    let val = table.get(&key);
                    self.scope.borrow_mut().set_reg_val(a, val.clone());
                }
                op => todo!("iABC {:?}", op),
            },
            Instruction::iABx(i_abx) => match i_abx.op {
                LuaOpCode::OP_LOADK => self.run_loadk(i_abx)?,
                LuaOpCode::OP_LOADNIL => {
                    let iABx { b, a, .. } = *i_abx;
                    for i in 0..=b {
                        self.scope
                            .borrow_mut()
                            .set_reg_val(a + i as u8, LuzObj::Nil);
                    }
                }
                LuaOpCode::OP_CLOSURE => {
                    let iABx { b, a, .. } = *i_abx;
                    let sub_scope = self.scope().sub_scopes()[b as usize].clone();
                    self.scope.borrow_mut().set_reg_val(
                        a,
                        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_user(sub_scope)))),
                    );
                }
                op => todo!("iABx {:?}", op),
            },
            Instruction::iAsBx(i_asbx) => match i_asbx.op {
                LuaOpCode::OP_LOADI => {
                    let iAsBx { b, a, .. } = *i_asbx;
                    let imm = (b as i64) - MAX_HALF_sBx as i64;
                    self.scope
                        .borrow_mut()
                        .set_reg_val(a, LuzObj::Numeral(Numeral::Int(imm)));
                }
                op => todo!("iAsBx {:?}", op),
            },
            Instruction::isJ(is_j) => match is_j.op {
                LuaOpCode::OP_JMP => {
                    let isJ { b, .. } = *is_j;
                    let offset = b - MAX_HALF_sJ;
                    return Ok(InstructionResult::Skip(offset));
                }
                op => todo!("isJ {:?}", op),
            },
        }
        Ok(InstructionResult::Continue)
    }

    fn run_arithmetic(&mut self, code: &iABC) -> Result<(), LuzError> {
        let iABC { c, b, k, a, op } = *code;

        let lhs = self.get_reg_val_or_const(b, k)?;
        let rhs = self.get_reg_val_or_const(
            c,
            matches!(
                op,
                LuaOpCode::OP_ADDK
                    | LuaOpCode::OP_SUBK
                    | LuaOpCode::OP_MULK
                    | LuaOpCode::OP_DIVK
                    | LuaOpCode::OP_IDIVK
            ),
        )?;

        let result = match op {
            LuaOpCode::OP_ADD | LuaOpCode::OP_ADDK => lhs.apply_binop(Binop::Add, rhs)?,
            LuaOpCode::OP_SUB | LuaOpCode::OP_SUBK => lhs.apply_binop(Binop::Sub, rhs)?,
            LuaOpCode::OP_MUL | LuaOpCode::OP_MULK => lhs.apply_binop(Binop::Mul, rhs)?,
            LuaOpCode::OP_DIV | LuaOpCode::OP_DIVK => lhs.apply_binop(Binop::FloatDiv, rhs)?,
            LuaOpCode::OP_IDIV | LuaOpCode::OP_IDIVK => lhs.apply_binop(Binop::FloorDiv, rhs)?,
            _ => unreachable!(),
        };

        self.scope.borrow_mut().set_reg_val(a, result);

        Ok(())
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
