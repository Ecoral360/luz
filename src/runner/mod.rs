use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    ast::{Binop, CmpOp},
    compiler::{
        ctx::{Scope, ScopeRef},
        instructions::{iABC, iABx, iAsBx, isJ, Instruction, MAX_HALF_sBx, MAX_HALF_sJ},
        opcode::LuaOpCode,
    },
    luz::{
        err::LuzError,
        obj::{LuzFunction, LuzObj, LuzType, Numeral, Table},
    },
    runner::err::LuzRuntimeError,
};

pub mod err;

pub enum InstructionResult {
    Continue,
    Skip(u32),
    Return(Vec<LuzObj>),
}

#[allow(unused)]
pub struct Runner {
    scope: Rc<RefCell<Scope>>,
    vararg: Option<Vec<LuzObj>>,
}

#[allow(unused)]
impl Runner {
    pub fn new(scope: Rc<RefCell<Scope>>) -> Self {
        Self {
            scope,
            vararg: Some(vec![]),
        }
    }

    pub fn clone_scope(&self) -> Rc<RefCell<Scope>> {
        Rc::clone(&self.scope)
    }

    pub fn env_scope(&self) -> Option<ScopeRef> {
        let mut scope = Rc::clone(&self.scope);
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

    pub fn scope(&self) -> Ref<Scope> {
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
        let val = self
            .scope
            .borrow_mut()
            .regs()
            .get(reg as usize)
            .ok_or(LuzError::RuntimeError(format!("Register not found {reg}")))?
            .val
            .as_ref()
            .expect(&format!("Value not found for {reg}"))
            .clone();
        Ok(val)
    }

    fn get_reg_val_or_nil(&self, reg: u8) -> LuzObj {
        let val = self
            .scope
            .borrow_mut()
            .regs()
            .get(reg as usize)
            .map(|reg| reg.val.clone().unwrap_or(LuzObj::Nil))
            .unwrap_or(LuzObj::Nil);
        val
    }

    fn take_reg_val(&mut self, reg: u8) -> Result<Option<LuzObj>, LuzError> {
        Ok(self.scope.borrow_mut().take_reg_val(reg))
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

                    if res.is_truthy() == *k {
                        return Ok(InstructionResult::Skip(1));
                    }
                }
                LuaOpCode::OP_LTI | LuaOpCode::OP_LEI | LuaOpCode::OP_GEI | LuaOpCode::OP_GTI => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = (*b as i64) - 128;

                    let cmp_op = match op {
                        LuaOpCode::OP_LTI => CmpOp::Lt,
                        LuaOpCode::OP_LEI => CmpOp::LtEq,
                        LuaOpCode::OP_GEI => CmpOp::GtEq,
                        LuaOpCode::OP_GTI => CmpOp::Gt,
                        _ => unreachable!(),
                    };
                    let res = lhs.apply_cmp(cmp_op, LuzObj::Numeral(Numeral::Int(rhs)))?;

                    if res.is_truthy() == *k {
                        return Ok(InstructionResult::Skip(1));
                    }
                }
                LuaOpCode::OP_EQK => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = self.get_const_val(*b)?;

                    let LuzObj::Boolean(b) = lhs.apply_cmp(CmpOp::Eq, rhs)? else {
                        unreachable!()
                    };

                    if b != *k {
                        return Ok(InstructionResult::Skip(1));
                    }
                }

                LuaOpCode::OP_EQ => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = self.get_reg_val(*b)?;

                    let LuzObj::Boolean(b) = lhs.apply_cmp(CmpOp::Eq, rhs)? else {
                        unreachable!()
                    };

                    if b != *k {
                        return Ok(InstructionResult::Skip(1));
                    }
                }

                LuaOpCode::OP_EQI => {
                    let lhs = self.get_reg_val(*a)?;
                    let rhs = (*b as i64) - 128;

                    let LuzObj::Boolean(b) =
                        lhs.apply_cmp(CmpOp::Eq, LuzObj::Numeral(Numeral::Int(rhs)))?
                    else {
                        unreachable!()
                    };

                    if b != *k {
                        return Ok(InstructionResult::Skip(1));
                    }
                }

                LuaOpCode::OP_TEST => {
                    let val = self.get_reg_val(*a)?;

                    if !(val.is_truthy() == *k) {
                        return Ok(InstructionResult::Skip(1));
                    }
                }
                LuaOpCode::OP_TESTSET => {
                    let val = self.get_reg_val(*b)?;

                    if !(val.is_truthy() == *k) {
                        return Ok(InstructionResult::Skip(1));
                    } else {
                        self.scope_mut().set_reg_val(*a, val);
                    }
                }
                LuaOpCode::OP_CLOSE => {
                    let reg_len = self.scope().regs().len();
                    for addr in *a..reg_len as u8 {
                        self.scope_mut().set_reg_val(addr, LuzObj::Nil);
                    }
                }
                LuaOpCode::OP_LOADTRUE => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(true));
                }
                LuaOpCode::OP_LOADFALSE => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(false));
                }
                LuaOpCode::OP_LFALSESKIP => {
                    self.scope_mut().set_reg_val(*a, LuzObj::Boolean(false));
                    return Ok(InstructionResult::Skip(1));
                }
                LuaOpCode::OP_NOT => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.not());
                }
                LuaOpCode::OP_LEN => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.len());
                }
                LuaOpCode::OP_BNOT => {
                    let val = self.get_reg_val(*b)?;
                    self.scope_mut().set_reg_val(*a, val.bnot());
                }
                LuaOpCode::OP_UNM => {
                    let val = self.get_reg_val(*b)?;
                    let LuzObj::Numeral(num) = val else {
                        return Err(LuzError::LuzRuntimeError(LuzRuntimeError::message(
                            "Arithmetic operation not supported for the value",
                        )));
                    };
                    self.scope_mut()
                        .set_reg_val(*a, LuzObj::Numeral(num * Numeral::Int(-1)));
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

                LuaOpCode::OP_MMBINI => {}
                LuaOpCode::OP_MMBINK => {}
                LuaOpCode::OP_MMBIN => {}

                LuaOpCode::OP_CONCAT => {
                    let start = *a;
                    let end = start + *b;

                    let dest = *a;

                    let mut str = String::new();
                    for addr in start..end {
                        let obj = self.get_reg_val(addr)?;
                        str += &obj.to_string();
                    }

                    self.scope_mut().set_reg_val(start, LuzObj::String(str));
                }

                LuaOpCode::OP_MOVE => {
                    let iABC { a, b, .. } = *i_abc;
                    let val = self.get_reg_val(b)?;
                    self.scope.borrow_mut().set_reg_val(a, val);
                }

                LuaOpCode::OP_RETURN => {
                    let mut rets = vec![];
                    for i in 0..*b - 1 {
                        rets.push(self.get_reg_val(*a + i)?);
                    }
                    return Ok(InstructionResult::Return(rets));
                }
                LuaOpCode::OP_RETURN1 => {
                    return Ok(InstructionResult::Return(vec![self.get_reg_val(*a)?]));
                }
                LuaOpCode::OP_RETURN0 => {
                    return Ok(InstructionResult::Return(vec![]));
                }

                LuaOpCode::OP_VARARGPREP => {
                    let vararg = self.vararg.take().expect("Var arg");
                    self.scope_mut().set_vararg(vararg);
                }
                LuaOpCode::OP_SELF => {
                    let self_table = self.get_reg_val(*b)?;

                    self.scope_mut().set_reg_val(*a + 1, self_table.clone());

                    let LuzObj::Table(self_table) = self_table else {
                        return Err(LuzError::Type {
                            wrong: self_table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let table = self_table.borrow();
                    let val = table.get(&key);
                    self.scope.borrow_mut().set_reg_val(*a, val.clone());
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

                    let f = f.borrow();

                    let nb_params = f.nb_fixed_params();

                    let mut args = vec![];
                    let mut vararg = vec![];
                    let mut arg_addr = 0;
                    while let Ok(Some(arg_val)) = self.take_reg_val(func_addr + 1 + arg_addr as u8)
                    {
                        if nb_args != 0 && arg_addr as u8 == nb_args - 1 {
                            break;
                        }
                        if arg_addr >= nb_params {
                            vararg.push(arg_val);
                        } else {
                            args.push(arg_val);
                        }
                        arg_addr += 1;
                    }
                    if arg_addr < nb_params {
                        for _ in 0..nb_params {
                            args.push(LuzObj::Nil);
                        }
                    }
                    let results = match *f {
                        LuzFunction::User { ref scope, .. } => {
                            let mut fc_scope = scope.borrow().clone();
                            for (i, arg) in args.into_iter().enumerate() {
                                fc_scope.set_reg_val(i as u8, arg);
                            }
                            let mut fc_runner = Runner::new(Rc::new(RefCell::new(fc_scope)));
                            fc_runner.vararg = Some(vararg);

                            fc_runner.run()?
                        }
                        LuzFunction::Native { ref fn_ptr, .. } => {
                            let mut fn_ptr = fn_ptr.borrow_mut();
                            (fn_ptr)(self, args, vararg)?
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
                            if i == nb_expected_results as usize {
                                break;
                            };
                            self.scope_mut().set_reg_val(func_addr + i as u8, result);
                        }
                        if (nb_expected_results - 1) > nb_results {
                            let diff = nb_expected_results - 2 - nb_results;
                            for addr in 0..diff + 1 {
                                self.scope_mut()
                                    .set_reg_val(func_addr + diff + addr, LuzObj::Nil);
                            }
                        }
                    }
                }
                LuaOpCode::OP_GETUPVAL => {
                    let iABC { a, b, c, .. } = *i_abc;
                    let val = self
                        .scope()
                        .get_upvalue_value(b)
                        .ok_or(LuzError::CompileError("Upvalue missing".to_owned()))?;

                    self.scope.borrow_mut().set_reg_val(a, val);
                }
                LuaOpCode::OP_SETUPVAL => {
                    let val = self.get_reg_val(*a)?;
                    self.scope_mut().set_upvalue_value(*b, val);
                }
                LuaOpCode::OP_GETFIELD => {
                    let table = self.get_reg_val(*b)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_const_val(*c)?;
                    let table = table.borrow();
                    let val = table.get(&key);
                    self.scope.borrow_mut().set_reg_val(*a, val.clone());
                }
                LuaOpCode::OP_GETTABUP => {
                    let iABC { a, b, c, .. } = *i_abc;
                    let table = self
                        .scope()
                        .get_upvalue_value(b)
                        .ok_or(LuzError::CompileError("Upvalue missing".to_owned()))?;

                    if let LuzObj::Table(table) = table {
                        let key = self.get_const_val(c)?;
                        let table = table.borrow();
                        let val = table.get(&key);
                        self.scope.borrow_mut().set_reg_val(a, val.clone());
                    } else {
                        self.scope.borrow_mut().set_reg_val(a, LuzObj::Nil);
                    };
                }
                LuaOpCode::OP_GETTABLE => {
                    let table = self.get_reg_val(*b)?;

                    if let LuzObj::Table(table) = table {
                        let key = self.get_reg_val(*c)?;
                        let table = table.borrow();
                        let val = table.get(&key);
                        self.scope.borrow_mut().set_reg_val(*a, val.clone());
                    } else {
                        self.scope.borrow_mut().set_reg_val(*a, LuzObj::Nil);
                    };
                }
                LuaOpCode::OP_GETI => {
                    let table = self.get_reg_val(*b)?;

                    if let LuzObj::Table(table) = table {
                        let key = (*c as i64) - 128;
                        let table = table.borrow();
                        let val = table.get(&LuzObj::Numeral(Numeral::Int(key)));
                        self.scope.borrow_mut().set_reg_val(*a, val.clone());
                    } else {
                        self.scope.borrow_mut().set_reg_val(*a, LuzObj::Nil);
                    };
                }
                LuaOpCode::OP_SETTABUP => {
                    let table = self
                        .scope()
                        .get_upvalue_value(*a)
                        .ok_or(LuzError::CompileError("Upvalue missing".to_owned()))?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_const_val(*b)?;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.insert(key, val);
                }
                LuaOpCode::OP_SETTABLE => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_reg_val(*b)?;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.insert(key, val);
                }
                LuaOpCode::OP_SETI => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = (*b as i64) - 128;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.insert(LuzObj::Numeral(Numeral::Int(key)), val);
                }
                LuaOpCode::OP_SETFIELD => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };
                    let key = self.get_const_val(*b)?;
                    let val = if *k {
                        self.get_const_val(*c)?
                    } else {
                        self.get_reg_val(*c)?
                    };
                    let mut table = table.borrow_mut();
                    table.insert(key, val);
                }
                LuaOpCode::OP_NEWTABLE => {
                    let mut table = HashMap::new();

                    let table_obj = Table::new(table, None);

                    self.scope_mut()
                        .set_reg_val(*a, LuzObj::Table(Rc::new(RefCell::new(table_obj))));
                }
                LuaOpCode::OP_SETLIST => {
                    let table = self.get_reg_val(*a)?;

                    let LuzObj::Table(table) = table else {
                        return Err(LuzError::Type {
                            wrong: table.get_type(),
                            expected: vec![LuzType::Table],
                        });
                    };

                    for i in 1..=*b {
                        let val = self.get_reg_val_or_nil(*a + i);
                        table
                            .borrow_mut()
                            .insert(LuzObj::Numeral(Numeral::Int((*c + i) as i64)), val);
                    }
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
                    let nb_params = sub_scope.borrow().nb_params();
                    self.scope.borrow_mut().set_reg_val(
                        a,
                        LuzObj::Function(Rc::new(RefCell::new(LuzFunction::new_user(
                            nb_params, sub_scope,
                        )))),
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
            Instruction::NOP => unimplemented!(),
            // IGNORE those
            Instruction::LOG(_) => {}
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
            self.scope
                .borrow_mut()
                .get_reg(b)
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
