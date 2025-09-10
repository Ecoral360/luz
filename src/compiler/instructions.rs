use std::fmt::Display;

use derive_new::new;

use crate::{compiler::opcode::LuaOpCode, luz::err::LuzError};

#[allow(non_camel_case_types)]
#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    iABC(iABC),
    iABx(iABx),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::iABC(i_abc) => write!(f, "{}", i_abc),
            Instruction::iABx(i_abx) => write!(f, "{}", i_abx),
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

    pub fn op_sub(a: u8, b: u8, is_b_const: bool, c: u8, is_c_const: bool) -> Instruction {
        iABC::new(
            c,
            b,
            is_b_const,
            a,
            if is_c_const {
                LuaOpCode::OP_SUB
            } else {
                LuaOpCode::OP_SUBK
            },
        )
        .into()
    }

    pub fn op_mul(a: u8, b: u8, is_b_const: bool, c: u8, is_c_const: bool) -> Instruction {
        iABC::new(
            c,
            b,
            is_b_const,
            a,
            if is_c_const {
                LuaOpCode::OP_MULK
            } else {
                LuaOpCode::OP_MUL
            },
        )
        .into()
    }

    pub fn op_div(a: u8, b: u8, is_b_const: bool, c: u8, is_c_const: bool) -> Instruction {
        iABC::new(
            c,
            b,
            is_b_const,
            a,
            if is_c_const {
                LuaOpCode::OP_DIVK
            } else {
                LuaOpCode::OP_DIV
            },
        )
        .into()
    }

    pub fn op_idiv(a: u8, b: u8, is_b_const: bool, c: u8, is_c_const: bool) -> Instruction {
        iABC::new(
            c,
            b,
            is_b_const,
            a,
            if is_c_const {
                LuaOpCode::OP_IDIV
            } else {
                LuaOpCode::OP_IDIVK
            },
        )
        .into()
    }

    pub fn op_return(reg: u8, is_const: bool, b: u8) -> Instruction {
        iABC::new(1, b, is_const, reg, LuaOpCode::OP_RETURN).into()
    }

    pub fn op_loadk(reg: u8, addrk: u32) -> Instruction {
        iABx::new(addrk, reg, LuaOpCode::OP_LOADK).into()
    }

    pub fn op_loadi(reg: u8, imm: i32) -> Instruction {
        iABx::new(excess_of_bx(imm), reg, LuaOpCode::OP_LOADI).into()
    }

    pub fn op_move(dest: u8, src: u8) -> Instruction {
        iABC::new(0, src, false, dest, LuaOpCode::OP_MOVE).into()
    }

    pub fn op_closure(reg: u8, sub_scope_idx: u32) -> Instruction {
        iABx::new(sub_scope_idx, reg, LuaOpCode::OP_CLOSURE).into()
    }
}

// A signed argument is represented in excess K: the represented value is
// the written unsigned value minus K, where K is half the maximum for the
// corresponding unsigned argument.
fn excess_of_bx(val: i32) -> u32 {
    // (val + 131071) as u32
    val as u32
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
