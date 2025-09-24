use std::fmt::Display;

use derive_new::new;

use crate::{
    ast::{Binop, Unop},
    compiler::{ctx::Scope, opcode::LuaOpCode},
    luz::err::LuzError,
};

#[allow(non_camel_case_types)]
#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    NOP,
    LOG(String),
    iABC(iABC),
    iABx(iABx),
    iAsBx(iAsBx),
    isJ(isJ),
    iAx(iAx),
}

impl Instruction {
    pub fn find_inst(op: LuaOpCode, insts: &[Instruction]) -> Option<usize> {
        insts
            .iter()
            .enumerate()
            .find_map(|(i, inst)| if inst.op() == op { Some(i) } else { None })
    }

    pub fn rfind_inst(op: LuaOpCode, insts: &[Instruction]) -> Option<usize> {
        insts
            .iter()
            .enumerate()
            .rfind(|(_, inst)| inst.op() == op)
            .map(|(i, _)| i)
    }
}

impl Instruction {
    pub fn op(&self) -> LuaOpCode {
        match self {
            Instruction::iABC(i_abc) => i_abc.op,
            Instruction::iABx(i_abx) => i_abx.op,
            Instruction::iAsBx(i_as_bx) => i_as_bx.op,
            Instruction::isJ(is_j) => is_j.op,
            Instruction::iAx(i_ax) => i_ax.op,
            _ => LuaOpCode::OP_debug,
        }
    }
}

#[allow(non_upper_case_globals)]
pub const MAX_HALF_sBx: u32 = 131072;

#[allow(non_upper_case_globals)]
pub const MAX_HALF_sJ: u32 = 16777216;

impl Instruction {
    pub fn debug(&self, scope: &Scope, inst_idx: usize) -> String {
        match self {
            Instruction::iABC(i_abc) => i_abc.debug(scope),
            Instruction::iABx(i_abx) => i_abx.debug(scope),
            Instruction::iAsBx(i_asbx) => i_asbx.debug(scope),
            Instruction::isJ(i_sj) => i_sj.debug(scope, inst_idx),
            Instruction::iAx(i_ax) => i_ax.debug(scope),
            Instruction::LOG(s) => format!("LOG: {}", s),
            _ => String::from("NOP"),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::iABC(i_abc) => write!(f, "{}", i_abc),
            Instruction::iABx(i_abx) => write!(f, "{}", i_abx),
            Instruction::iAsBx(i_asbx) => write!(f, "{}", i_asbx),
            Instruction::isJ(i_sj) => write!(f, "{}", i_sj),
            Instruction::iAx(i_ax) => write!(f, "{}", i_ax),
            Instruction::LOG(s) => write!(f, "LOG: {}", s.clone()),
            _ => write!(f, "NOP"),
        }
    }
}

impl Instruction {
    pub fn log<S: ToString>(s: S) -> Instruction {
        Instruction::LOG(s.to_string())
    }

    pub fn op_close(start: u8) -> Instruction {
        LuaOpCode::OP_CLOSE.to_iabc(start, false, 0, 0).into()
    }

    pub fn op_addi(a: u8, b: u8, is_b_const: bool, c: u8) -> Instruction {
        iABC::new(c, b, is_b_const, a, LuaOpCode::OP_ADDI).into()
    }
    pub fn op_shli(dest: u8, lhs: u8, is_lhs_const: bool, rhs_imm: u8) -> Instruction {
        LuaOpCode::OP_SHLI
            .to_iabc(dest, is_lhs_const, lhs, rhs_imm)
            .into()
    }
    pub fn op_shri(dest: u8, lhs: u8, is_lhs_const: bool, rhs_imm: u8) -> Instruction {
        LuaOpCode::OP_SHRI
            .to_iabc(dest, is_lhs_const, lhs, rhs_imm)
            .into()
    }

    pub fn op_mmbini(obj_reg: u8, arg_i: u8, metamethod: u8, flip: bool) -> Instruction {
        LuaOpCode::OP_MMBINI
            .to_iabc(obj_reg, flip, arg_i, metamethod)
            .into()
    }
    pub fn op_mmbink(obj_reg: u8, arg_k: u8, metamethod: u8, flip: bool) -> Instruction {
        LuaOpCode::OP_MMBINK
            .to_iabc(obj_reg, flip, arg_k, metamethod)
            .into()
    }
    pub fn op_mmbin(obj_reg: u8, arg_reg: u8, metamethod: u8) -> Instruction {
        LuaOpCode::OP_MMBIN
            .to_iabc(obj_reg, false, arg_reg, metamethod)
            .into()
    }

    pub fn op_loadnil(start_addr: u8, nb: u32) -> Instruction {
        iABx::new(nb, start_addr, LuaOpCode::OP_LOADNIL).into()
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
    pub fn op_concat(start: u8, nb: u8) -> Instruction {
        LuaOpCode::OP_CONCAT.to_iabc(start, false, nb, 0).into()
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
                Binop::Concat => unreachable!(),
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
                Binop::Concat => unreachable!(),
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

    pub fn op_unop(unop: Unop, dest: u8, val_addr: u8) -> Instruction {
        let op = match unop {
            Unop::Neg => LuaOpCode::OP_UNM,
            Unop::Inv => LuaOpCode::OP_BNOT,
            Unop::Len => LuaOpCode::OP_LEN,
            Unop::Not => LuaOpCode::OP_NOT,
        };

        op.to_iabc(dest, false, val_addr, 0).into()
    }

    pub fn op_jmp(jmp_dist: i32) -> Instruction {
        LuaOpCode::OP_JMP
            .to_isj(0, (jmp_dist + MAX_HALF_sJ as i32) as u32) // a is unused
            .into()
    }

    pub fn op_lfalseskip(reg: u8) -> Instruction {
        LuaOpCode::OP_LFALSESKIP.to_iabc(reg, false, 0, 0).into()
    }

    pub fn op_loadtrue(reg: u8) -> Instruction {
        LuaOpCode::OP_LOADTRUE.to_iabc(reg, false, 0, 0).into()
    }
    pub fn op_loadfalse(reg: u8) -> Instruction {
        LuaOpCode::OP_LOADFALSE.to_iabc(reg, false, 0, 0).into()
    }

    pub fn op_test(reg: u8, apply_not: bool) -> Instruction {
        LuaOpCode::OP_TEST.to_iabc(reg, apply_not, 0, 0).into()
    }

    pub fn op_testset(reg: u8, val_reg: u8, apply_not: bool) -> Instruction {
        LuaOpCode::OP_TESTSET
            .to_iabc(reg, apply_not, val_reg, 0)
            .into()
    }

    pub fn op_eq(lhs: u8, rhs: u8, is_rhs_const: bool, apply_not: bool) -> Instruction {
        let opcode = if is_rhs_const {
            LuaOpCode::OP_EQK
        } else {
            LuaOpCode::OP_EQ
        };
        opcode.to_iabc(lhs, apply_not, rhs, 0).into()
    }

    pub fn op_eqi(lhs: u8, rhs_i: u8, apply_not: bool) -> Instruction {
        LuaOpCode::OP_EQI.to_iabc(lhs, apply_not, rhs_i, 0).into()
    }
    pub fn op_lt(lhs: u8, rhs: u8, is_rhs_immidiate: bool, apply_not: bool) -> Instruction {
        let opcode = if is_rhs_immidiate {
            LuaOpCode::OP_LTI
        } else {
            LuaOpCode::OP_LT
        };
        opcode.to_iabc(lhs, apply_not, rhs, 0).into()
    }

    pub fn op_le(lhs: u8, rhs: u8, is_rhs_immidiate: bool, apply_not: bool) -> Instruction {
        let opcode = if is_rhs_immidiate {
            LuaOpCode::OP_LEI
        } else {
            LuaOpCode::OP_LE
        };
        opcode.to_iabc(lhs, apply_not, rhs, 0).into()
    }

    pub fn op_gt(lhs: u8, rhs: u8, is_rhs_immidiate: bool, apply_not: bool) -> Instruction {
        if is_rhs_immidiate {
            LuaOpCode::OP_GTI.to_iabc(lhs, apply_not, rhs, 0).into()
        } else {
            Instruction::op_lt(rhs, lhs, is_rhs_immidiate, apply_not)
        }
    }

    pub fn op_ge(lhs: u8, rhs: u8, is_rhs_immidiate: bool, apply_not: bool) -> Instruction {
        if is_rhs_immidiate {
            LuaOpCode::OP_GEI.to_iabc(lhs, apply_not, rhs, 0).into()
        } else {
            Instruction::op_lt(rhs, lhs, is_rhs_immidiate, apply_not)
        }
    }

    pub fn op_return(reg: u8, is_const: bool, nb_exps: u8) -> Instruction {
        LuaOpCode::OP_RETURN
            .to_iabc(reg, is_const, nb_exps, 1)
            .into()
    }
    pub fn op_return0() -> Instruction {
        LuaOpCode::OP_RETURN0.to_iabc(0, false, 0, 0).into()
    }
    pub fn op_return1(reg: u8) -> Instruction {
        LuaOpCode::OP_RETURN1.to_iabc(reg, false, 0, 0).into()
    }

    pub fn op_loadk(reg: u8, addrk: u32) -> Instruction {
        LuaOpCode::OP_LOADK.to_iabx(reg, addrk).into()
    }

    pub fn op_loadi(reg: u8, imm: u32) -> Instruction {
        LuaOpCode::OP_LOADI
            .to_iasbx(reg, imm.wrapping_add(MAX_HALF_sBx))
            .into()
    }
    pub fn op_loadf(reg: u8, imm: f64) -> Instruction {
        LuaOpCode::OP_LOADF
            .to_iasbx(reg, (imm + MAX_HALF_sBx as f64) as u32)
            .into()
    }

    pub fn op_move(dest: u8, src: u8) -> Instruction {
        iABC::new(0, src, false, dest, LuaOpCode::OP_MOVE).into()
    }

    pub fn op_closure(reg: u8, sub_scope_idx: u32) -> Instruction {
        iABx::new(sub_scope_idx, reg, LuaOpCode::OP_CLOSURE).into()
    }
    pub fn op_self(
        func_addr: u8,
        self_addr: u8,
        func_attr: u8,
        is_func_attr_const: bool,
    ) -> Instruction {
        LuaOpCode::OP_SELF
            .to_iabc(func_addr, is_func_attr_const, self_addr, func_attr)
            .into()
    }

    pub fn op_getupval(dest: u8, upval_addr: u8) -> Instruction {
        LuaOpCode::OP_GETUPVAL
            .to_iabc(dest, false, upval_addr, 0)
            .into()
    }

    pub fn op_gettabup(dest: u8, upval_addr: u8, tabattr_addrk: u8) -> Instruction {
        LuaOpCode::OP_GETTABUP
            .to_iabc(dest, false, upval_addr, tabattr_addrk)
            .into()
    }
    pub fn op_getfield(dest: u8, tabaddr: u8, tabattr_addrk: u8) -> Instruction {
        LuaOpCode::OP_GETFIELD
            .to_iabc(dest, false, tabaddr, tabattr_addrk)
            .into()
    }
    pub fn op_geti(dest: u8, tabaddr: u8, tabattr_i: u8) -> Instruction {
        LuaOpCode::OP_GETI
            .to_iabc(dest, false, tabaddr, tabattr_i)
            .into()
    }
    pub fn op_gettable(dest: u8, tabaddr: u8, tabattr_addr: u8) -> Instruction {
        LuaOpCode::OP_GETTABLE
            .to_iabc(dest, false, tabaddr, tabattr_addr)
            .into()
    }

    pub fn op_setupval(upval_dest_addr: u8, val_reg: u8) -> Instruction {
        LuaOpCode::OP_SETUPVAL
            .to_iabc(val_reg, false, upval_dest_addr, 0)
            .into()
    }
    pub fn op_settabup(
        upval_dest_addr: u8,
        tabattr_addrk: u8,
        val: u8,
        is_val_const: bool,
    ) -> Instruction {
        LuaOpCode::OP_SETTABUP
            .to_iabc(upval_dest_addr, is_val_const, tabattr_addrk, val)
            .into()
    }
    pub fn op_setfield(
        dest_addr: u8,
        tabattr_addrk: u8,
        val: u8,
        is_val_const: bool,
    ) -> Instruction {
        LuaOpCode::OP_SETFIELD
            .to_iabc(dest_addr, is_val_const, tabattr_addrk, val)
            .into()
    }
    pub fn op_seti(dest_addr: u8, tabattr_i: u8, val: u8, is_val_const: bool) -> Instruction {
        LuaOpCode::OP_SETI
            .to_iabc(dest_addr, is_val_const, tabattr_i, val)
            .into()
    }
    pub fn op_settable(
        dest_addr: u8,
        tabattr_addr: u8,
        val: u8,
        is_val_const: bool,
    ) -> Instruction {
        LuaOpCode::OP_SETTABLE
            .to_iabc(dest_addr, is_val_const, tabattr_addr, val)
            .into()
    }

    pub fn op_varargprep(dest: u8) -> Instruction {
        LuaOpCode::OP_VARARGPREP.to_iabc(dest, false, 0, 0).into()
    }

    pub fn op_newtable(dest: u8, obj_fields_len: u8, arr_fields_len: u8) -> Instruction {
        LuaOpCode::OP_NEWTABLE
            .to_iabc(dest, false, obj_fields_len, arr_fields_len)
            .into()
    }
    pub fn op_extraarg(arg: u32) -> Instruction {
        LuaOpCode::OP_EXTRAARG.to_iax(arg).into()
    }
    pub fn op_setlist(dest: u8, nb_fields: u8, start_addr: u8) -> Instruction {
        LuaOpCode::OP_SETLIST
            .to_iabc(dest, false, nb_fields, start_addr)
            .into()
    }

    pub fn op_vararg(start: u8, nb: u8) -> Instruction {
        LuaOpCode::OP_VARARG.to_iabc(start, false, 0, nb).into()
    }

    pub fn op_forloop(forloop_test: u8, loop_len: u32) -> Instruction {
        LuaOpCode::OP_FORLOOP.to_iabx(forloop_test, loop_len).into()
    }
    pub fn op_forprep(forloop_test: u8, loop_len: u32) -> Instruction {
        LuaOpCode::OP_FORPREP.to_iabx(forloop_test, loop_len).into()
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

impl iABC {
    pub fn debug(&self, scope: &Scope) -> String {
        let s = match self.op {
            LuaOpCode::OP_EQK => {
                let local = scope.get_const(self.b);
                format!("{} ; {}", self, local.repr())
            }
            LuaOpCode::OP_BANDK
            | LuaOpCode::OP_BORK
            | LuaOpCode::OP_BXORK
            | LuaOpCode::OP_ADDK
            | LuaOpCode::OP_SUBK
            | LuaOpCode::OP_MULK
            | LuaOpCode::OP_DIVK
            | LuaOpCode::OP_MODK
            | LuaOpCode::OP_POWK
            | LuaOpCode::OP_IDIVK => {
                let local = scope.get_const(self.c);
                format!("{} ; {}", self, local.repr())
            }
            LuaOpCode::OP_LOADNIL => {
                format!("{} ; {} out", self, self.b + 1)
            }
            LuaOpCode::OP_VARARG => {
                format!(
                    "{} ; {} out",
                    self,
                    if self.c == 0 {
                        "all".to_string()
                    } else {
                        format!("{}", self.c - 1)
                    }
                )
            }
            LuaOpCode::OP_GETTABUP => {
                let upval = scope.get_upvalue(self.b);
                let local = scope.get_const(self.c);
                format!("{} ; {} {}", self, upval.name, local.repr())
            }
            LuaOpCode::OP_GETUPVAL => {
                let upval = scope.get_upvalue(self.b);
                format!("{} ; {}", self, upval.name)
            }
            LuaOpCode::OP_SETTABUP => {
                let upval = scope.get_upvalue(self.a);
                let local = scope.get_const(self.b);
                let val = if self.k {
                    scope.get_const(self.c).repr()
                } else {
                    String::new()
                };
                format!("{} ; {} {} {}", self, upval.name, local.repr(), val)
            }
            LuaOpCode::OP_GETFIELD => {
                let local = scope.get_const(self.c);
                format!("{} ; {}", self, local.repr())
            }
            LuaOpCode::OP_SETFIELD => {
                let local = scope.get_const(self.b);
                let val = if self.k {
                    scope.get_const(self.c).repr()
                } else {
                    String::new()
                };
                format!("{} ; {} {}", self, local.repr(), val)
            }
            LuaOpCode::OP_CALL => {
                let args = if self.b == 0 {
                    String::from("all")
                } else {
                    format!("{}", self.b - 1)
                };
                let output = if self.c == 0 {
                    String::from("all")
                } else {
                    format!("{}", self.c - 1)
                };
                format!("{} ; {} in {} out", self, args, output)
            }
            LuaOpCode::OP_SELF => {
                let val = if self.k {
                    scope.get_const(self.c).repr()
                } else {
                    String::new()
                };
                format!("{} ; {}", self, val)
            }
            LuaOpCode::OP_RETURN => {
                let nb = if self.b == 0 {
                    String::from("all")
                } else {
                    format!("{}", self.b - 1)
                };
                format!("{} ; {} out", self, nb)
            }
            _ => self.to_string(),
        };

        s
    }
}

const INSTS_COL_WIDTH: usize = 13;
const INST_ARG_COL_WIDTH: usize = 22;

impl Display for iABC {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let showk = matches!(
            self.op,
            LuaOpCode::OP_SETFIELD | LuaOpCode::OP_SELF | LuaOpCode::OP_SETTABUP
        );

        let b = match self.op {
            LuaOpCode::OP_ADD if self.k => -(self.b as i32 + 1),
            LuaOpCode::OP_GTI
            | LuaOpCode::OP_LTI
            | LuaOpCode::OP_GEI
            | LuaOpCode::OP_LEI
            | LuaOpCode::OP_EQI
            | LuaOpCode::OP_SETI => self.b as i32 - 128,
            _ => self.b as i32,
        };

        let c = match self.op {
            LuaOpCode::OP_ADDI | LuaOpCode::OP_SHLI | LuaOpCode::OP_SHRI | LuaOpCode::OP_GETI => {
                self.c as i32 - 128
            }
            LuaOpCode::OP_EQ
            | LuaOpCode::OP_LT
            | LuaOpCode::OP_LE
            | LuaOpCode::OP_EQK
            | LuaOpCode::OP_EQI
            | LuaOpCode::OP_LTI
            | LuaOpCode::OP_LEI
            | LuaOpCode::OP_GTI
            | LuaOpCode::OP_GEI
            | LuaOpCode::OP_TEST
            | LuaOpCode::OP_TESTSET => self.k as i32,

            _ => self.c as i32,
        };

        let s = match self.op {
            LuaOpCode::OP_RETURN0 => {
                format!("{}", self.op.to_string())
            }
            LuaOpCode::OP_MOVE
            | LuaOpCode::OP_BNOT
            | LuaOpCode::OP_LEN
            | LuaOpCode::OP_NOT
            | LuaOpCode::OP_UNM
            | LuaOpCode::OP_GETUPVAL
            | LuaOpCode::OP_VARARG => {
                format!(
                    "{:<INSTS_COL_WIDTH$} {} {}",
                    self.op.to_string(),
                    self.a,
                    self.b
                )
            }
            LuaOpCode::OP_LOADFALSE
            | LuaOpCode::OP_LOADTRUE
            | LuaOpCode::OP_LFALSESKIP
            | LuaOpCode::OP_RETURN1
            | LuaOpCode::OP_CLOSE => {
                format!("{:<INSTS_COL_WIDTH$} {}", self.op.to_string(), self.a)
            }
            LuaOpCode::OP_TEST => {
                format!(
                    "{:<INSTS_COL_WIDTH$} {} {}",
                    self.op.to_string(),
                    self.a,
                    self.k as u8
                )
            }
            LuaOpCode::OP_MMBINI => {
                format!(
                    "{:<INSTS_COL_WIDTH$} {} {} {} {}",
                    self.op.to_string(),
                    self.a,
                    (self.b as i32) - 128,
                    self.c,
                    self.k as u8
                )
            }
            _ => format!(
                "{:<INSTS_COL_WIDTH$} {} {} {}",
                self.op.to_string(),
                self.a,
                b,
                c
            ),
        };

        write!(
            f,
            "{:<INST_ARG_COL_WIDTH$}{}",
            s,
            if showk && self.k { "k" } else { "" }
        )
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

impl iABx {
    pub fn debug(&self, scope: &Scope) -> String {
        let s = match self.op {
            LuaOpCode::OP_LOADK => {
                let local = scope.get_const(self.b as u8);
                format!("{} ; {}", self, local.repr())
            }
            _ => self.to_string(),
        };

        s
    }
}

impl Display for iABx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let b = match self.op {
            // LuaOpCode::OP_LOADK => -(self.b as i32 + 1),
            _ => self.b as i32,
        };
        write!(
            f,
            "{:<INST_ARG_COL_WIDTH$}",
            format!("{:<INSTS_COL_WIDTH$} {} {}", self.op.to_string(), self.a, b)
        )
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

impl iAsBx {
    pub fn debug(&self, scope: &Scope) -> String {
        let s = match self.op {
            _ => self.to_string(),
        };

        s
    }
}

impl Display for iAsBx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:<INST_ARG_COL_WIDTH$}",
            format!(
                "{:<INSTS_COL_WIDTH$} {} {}",
                self.op.to_string(),
                self.a,
                (self.b as i32) - MAX_HALF_sBx as i32
            )
        )
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, new)]
pub struct isJ {
    pub b: u32, // actually 25 bits
    pub a: u8,
    pub op: LuaOpCode, // actually 7 bits
}

impl Into<Instruction> for isJ {
    fn into(self) -> Instruction {
        Instruction::isJ(self)
    }
}

impl Into<u32> for isJ {
    fn into(self) -> u32 {
        self.op as u32 | (self.a as u32) << 7 | self.b << 16
    }
}

impl From<u32> for isJ {
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

impl isJ {
    pub fn debug(&self, _scope: &Scope, inst_idx: usize) -> String {
        let s = match self.op {
            LuaOpCode::OP_JMP => {
                format!(
                    "{} ; to {}",
                    self,
                    inst_idx as i32 + self.b as i32 - MAX_HALF_sJ as i32 + 1
                )
            }
            _ => self.to_string(),
        };

        s
    }
}

impl Display for isJ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:<INST_ARG_COL_WIDTH$}",
            format!(
                "{:<INSTS_COL_WIDTH$} {}",
                self.op.to_string(),
                self.b as i32 - MAX_HALF_sJ as i32
            )
        )
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, new)]
pub struct iAx {
    pub a: u32,
    pub op: LuaOpCode, // actually 7 bits
}

impl Into<Instruction> for iAx {
    fn into(self) -> Instruction {
        Instruction::iAx(self)
    }
}

impl Into<u32> for iAx {
    fn into(self) -> u32 {
        self.op as u32 | (self.a as u32) << 7
    }
}

impl From<u32> for iAx {
    fn from(val: u32) -> Self {
        let op = get_arg(val, 7, 0);
        let a = get_arg(val, 8, 7);
        Self {
            a: a as u32,
            op: (op as u8).try_into().unwrap(),
        }
    }
}

impl iAx {
    pub fn debug(&self, scope: &Scope) -> String {
        let s = match self.op {
            _ => self.to_string(),
        };

        s
    }
}

impl Display for iAx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:<INST_ARG_COL_WIDTH$}",
            format!("{:<INSTS_COL_WIDTH$} {}", self.op.to_string(), self.a)
        )
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
