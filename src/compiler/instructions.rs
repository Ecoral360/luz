use std::fmt::Display;

use derive_new::new;

use crate::{ast::Binop, compiler::opcode::LuaOpCode};

#[allow(non_camel_case_types)]
#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    iABC(iABC),
    iABx(iABx),
    iAsBx(iAsBx),
}

#[allow(non_upper_case_globals)]
pub const MAX_HALF_sBx: u32 = 131072;

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::iABC(i_abc) => write!(f, "{}", i_abc),
            Instruction::iABx(i_abx) => write!(f, "{}", i_abx),
            Instruction::iAsBx(i_asbx) => write!(f, "{}", i_asbx),
        }
    }
}

impl Instruction {
    pub fn op_addi(a: u8, b: u8, is_b_const: bool, c: u8) -> Instruction {
        iABC::new(c, b, is_b_const, a, LuaOpCode::OP_ADDI).into()
    }

    pub fn op_loadnil(a: u8, b: u32) -> Instruction {
        iABx::new(b, a, LuaOpCode::OP_LOADNIL).into()
    }

    pub fn op_add(a: u8, b: u8, is_b_const: bool, c: u8, is_c_const: bool) -> Instruction {
        iABC::new(
            c,
            b,
            is_b_const,
            a,
            if is_c_const {
                LuaOpCode::OP_ADDK
            } else {
                LuaOpCode::OP_ADD
            },
        )
        .into()
    }

    /// Special values for n_args and n_expected
    /// n_args == 0 -> variadic call
    /// n_args == 1 -> no args
    /// n_args >= 2 -> n_args - 1 gives the number of args
    ///
    /// n_expected == 0 -> variadic return
    /// n_expected == 1 -> nothing returned
    /// n_expected >= 2 -> n_expected - 1 gives the number of expected values
    pub fn op_call(func_reg: u8, n_args: u8, n_expected: u8) -> Instruction {
        LuaOpCode::OP_CALL
            .to_iabc(func_reg, false, n_args, n_expected)
            .into()
    }

    pub fn op_arithmetic(
        op: Binop,
        dest_reg: u8,
        lhs: u8,
        is_lhs_const: bool,
        rhs: u8,
        is_rhs_const: bool,
    ) -> Instruction {
        let opcode: LuaOpCode = if is_rhs_const {
            match op {
                Binop::Concat => todo!(),
                Binop::Add => LuaOpCode::OP_ADDK,
                Binop::Sub => LuaOpCode::OP_SUBK,
                Binop::Mul => LuaOpCode::OP_MULK,
                Binop::FloatDiv => LuaOpCode::OP_DIVK,
                Binop::FloorDiv => LuaOpCode::OP_IDIVK,
                Binop::Mod => LuaOpCode::OP_MODK,
                Binop::Exp => LuaOpCode::OP_POWK,
                Binop::BitAnd => LuaOpCode::OP_BANDK,
                Binop::BitOr => LuaOpCode::OP_BORK,
                Binop::BitXor => LuaOpCode::OP_BXORK,
                Binop::ShiftLeft => LuaOpCode::OP_SHLI,
                Binop::ShiftRight => LuaOpCode::OP_SHRI,
            }
        } else {
            match op {
                Binop::Concat => todo!(),
                Binop::Add => LuaOpCode::OP_ADD,
                Binop::Sub => LuaOpCode::OP_SUB,
                Binop::Mul => LuaOpCode::OP_MUL,
                Binop::FloatDiv => LuaOpCode::OP_DIV,
                Binop::FloorDiv => LuaOpCode::OP_IDIV,
                Binop::Mod => LuaOpCode::OP_MOD,
                Binop::Exp => LuaOpCode::OP_POW,
                Binop::BitAnd => LuaOpCode::OP_BAND,
                Binop::BitOr => LuaOpCode::OP_BOR,
                Binop::BitXor => LuaOpCode::OP_BXOR,
                Binop::ShiftLeft => LuaOpCode::OP_SHL,
                Binop::ShiftRight => LuaOpCode::OP_SHR,
            }
        };
        opcode.to_iabc(dest_reg, is_lhs_const, lhs, rhs).into()
    }

    pub fn op_return(reg: u8, is_const: bool, nb_exps: u8) -> Instruction {
        LuaOpCode::OP_RETURN
            .to_iabc(reg, is_const, nb_exps, 1)
            .into()
    }

    pub fn op_loadk(reg: u8, addrk: u32) -> Instruction {
        LuaOpCode::OP_LOADK.to_iabx(reg, addrk).into()
    }

    pub fn op_loadi(reg: u8, imm: u32) -> Instruction {
        LuaOpCode::OP_LOADI.to_iasbx(reg, imm + MAX_HALF_sBx).into()
    }

    pub fn op_move(dest: u8, src: u8) -> Instruction {
        iABC::new(0, src, false, dest, LuaOpCode::OP_MOVE).into()
    }

    pub fn op_closure(reg: u8, sub_scope_idx: u32) -> Instruction {
        iABx::new(sub_scope_idx, reg, LuaOpCode::OP_CLOSURE).into()
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, new)]
pub struct iABC {
    pub c: u8,
    pub b: u8,
    pub k: bool,
    pub a: u8,
    pub op: LuaOpCode, // actually 7 bits
}

impl Into<Instruction> for iABC {
    fn into(self) -> Instruction {
        Instruction::iABC(self)
    }
}

impl Into<u32> for iABC {
    fn into(self) -> u32 {
        self.op as u32
            | (self.a as u32) << 7
            | (self.k as u32) << 15
            | (self.b as u32) << 16
            | (self.c as u32) << 24
    }
}

impl From<u32> for iABC {
    fn from(val: u32) -> Self {
        let op = get_arg(val, 7, 0);
        let a = get_arg(val, 8, 7);
        let k = get_arg(val, 1, 15);
        let b = get_arg(val, 8, 16);
        let c = get_arg(val, 8, 24);
        Self {
            c: c as u8,
            b: b as u8,
            k: k == 1,
            a: a as u8,
            op: (op as u8).try_into().unwrap(),
        }
    }
}

impl Display for iABC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b = match self.op {
            LuaOpCode::OP_ADD if self.k => -(self.b as i32 + 1),
            _ => self.b as i32,
        };
        let c = match self.op {
            LuaOpCode::OP_ADDI => self.c as i32 - 128,
            _ => self.c as i32,
        };
        write!(f, "{:?} {} {} {}", self.op, self.a, b, c)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, new)]
pub struct iABx {
    pub b: u32, // actually 17 bits
    pub a: u8,
    pub op: LuaOpCode, // actually 7 bits
}

impl Into<Instruction> for iABx {
    fn into(self) -> Instruction {
        Instruction::iABx(self)
    }
}

impl Into<u32> for iABx {
    fn into(self) -> u32 {
        self.op as u32 | (self.a as u32) << 7 | self.b << 16
    }
}

impl From<u32> for iABx {
    fn from(val: u32) -> Self {
        let op = get_arg(val, 7, 0);
        let a = get_arg(val, 8, 7);
        let b = get_arg(val, 17, 15);
        Self {
            b: b as u32,
            a: a as u8,
            op: (op as u8).try_into().unwrap(),
        }
    }
}

impl Display for iABx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b = match self.op {
            LuaOpCode::OP_LOADK => -(self.b as i32 + 1),
            _ => self.b as i32,
        };
        write!(f, "{:?} {} {}", self.op, self.a, b)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, new)]
pub struct iAsBx {
    pub b: u32, // actually 17 bits
    pub a: u8,
    pub op: LuaOpCode, // actually 7 bits
}

impl Into<Instruction> for iAsBx {
    fn into(self) -> Instruction {
        Instruction::iAsBx(self)
    }
}

impl Into<u32> for iAsBx {
    fn into(self) -> u32 {
        self.op as u32 | (self.a as u32) << 7 | self.b << 16
    }
}

impl From<u32> for iAsBx {
    fn from(val: u32) -> Self {
        let op = get_arg(val, 7, 0);
        let a = get_arg(val, 8, 7);
        let b = get_arg(val, 17, 15);
        Self {
            b: b as u32,
            a: a as u8,
            op: (op as u8).try_into().unwrap(),
        }
    }
}

impl Display for iAsBx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self.op, self.a, self.b - MAX_HALF_sBx)
    }
}

fn mask1(size: u32, pos: u32) -> u32 {
    (!((!0) << size)) << pos
}

fn get_arg(val: u32, size: u32, pos: u32) -> u32 {
    (val >> pos) & mask1(size, 0)
}

#[cfg(test)]
mod test {
    use crate::compiler::{instructions::iABC, opcode::LuaOpCode};

    #[test]
    fn test_luacode_instructions() {
        let instruction = iABC {
            c: 2,
            b: 0,
            k: true,
            a: 1,
            op: LuaOpCode::OP_ADD,
        };

        let i: u32 = instruction.into();
        println!("{:0b}", i);
        assert_eq!(iABC::from(i), instruction);
    }
}
