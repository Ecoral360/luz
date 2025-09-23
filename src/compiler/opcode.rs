use std::fmt::Display;

/// Link : <lua source code>/lopcodes.h
/*===========================================================================
  We assume that instructions are unsigned 32-bit integers.
  All instructions have an opcode in the first 7 bits.
  Instructions can have the following formats:

        3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
        1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
iABC          C(8)     |      B(8)     |k|     A(8)      |   Op(7)     |
iABx                Bx(17)               |     A(8)      |   Op(7)     |
iAsBx              sBx (signed)(17)      |     A(8)      |   Op(7)     |
iAx                           Ax(25)                     |   Op(7)     |
isJ                           sJ (signed)(25)            |   Op(7)     |

  A signed argument is represented in excess K: the represented value is
  the written unsigned value minus K, where K is half the maximum for the
  corresponding unsigned argument.
===========================================================================*/
use num_enum::TryFromPrimitive;

use crate::compiler::instructions::{iABC, iABx, iAsBx, iAx, isJ};

#[allow(non_camel_case_types)]
#[derive(TryFromPrimitive, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum LuaOpCode {
    /*----------------------------------------------------------------------
      name		args	description
    ------------------------------------------------------------------------*/
    OP_MOVE,       /*	A B	R[A] := R[B]					*/
    OP_LOADI,      /*	A sBx	R[A] := sBx					*/
    OP_LOADF,      /*	A sBx	R[A] := (lua_Number)sBx				*/
    OP_LOADK,      /*	A Bx	R[A] := K[Bx]					*/
    OP_LOADKX,     /*	A	R[A] := K[extra arg]				*/
    OP_LOADFALSE,  /*	A	R[A] := false					*/
    OP_LFALSESKIP, /*A	R[A] := false; pc++	(*)			*/
    OP_LOADTRUE,   /*	A	R[A] := true					*/
    OP_LOADNIL,    /*	A B	R[A], R[A+1], ..., R[A+B] := nil		*/
    OP_GETUPVAL,   /*	A B	R[A] := UpValue[B]				*/
    OP_SETUPVAL,   /*	A B	UpValue[B] := R[A]				*/

    OP_GETTABUP, /*	A B C	R[A] := UpValue[B][K[C]:shortstring]		*/
    OP_GETTABLE, /*	A B C	R[A] := R[B][R[C]]				*/
    OP_GETI,     /*	A B C	R[A] := R[B][C]					*/
    OP_GETFIELD, /*	A B C	R[A] := R[B][K[C]:shortstring]			*/

    OP_SETTABUP, /*	A B C	UpValue[A][K[B]:shortstring] := RK(C)		*/
    OP_SETTABLE, /*	A B C	R[A][R[B]] := RK(C)				*/
    OP_SETI,     /*	A B C	R[A][B] := RK(C)				*/
    OP_SETFIELD, /*	A B C	R[A][K[B]:shortstring] := RK(C)			*/

    OP_NEWTABLE, /*	A B C k	R[A] := {}					*/

    OP_SELF, /*	A B C	R[A+1] := R[B]; R[A] := R[B][RK(C):string]	*/

    OP_ADDI, /*	A B sC	R[A] := R[B] + sC				*/

    OP_ADDK,  /*	A B C	R[A] := R[B] + K[C]:number			*/
    OP_SUBK,  /*	A B C	R[A] := R[B] - K[C]:number			*/
    OP_MULK,  /*	A B C	R[A] := R[B] * K[C]:number			*/
    OP_MODK,  /*	A B C	R[A] := R[B] % K[C]:number			*/
    OP_POWK,  /*	A B C	R[A] := R[B] ^ K[C]:number			*/
    OP_DIVK,  /*	A B C	R[A] := R[B] / K[C]:number			*/
    OP_IDIVK, /*	A B C	R[A] := R[B] // K[C]:number			*/

    OP_BANDK, /*	A B C	R[A] := R[B] & K[C]:integer			*/
    OP_BORK,  /*	A B C	R[A] := R[B] | K[C]:integer			*/
    OP_BXORK, /*	A B C	R[A] := R[B] ~ K[C]:integer			*/

    OP_SHRI, /*	A B sC	R[A] := R[B] >> sC				*/
    OP_SHLI, /*	A B sC	R[A] := sC << R[B]				*/

    OP_ADD,  /*	A B C	R[A] := R[B] + R[C]				*/
    OP_SUB,  /*	A B C	R[A] := R[B] - R[C]				*/
    OP_MUL,  /*	A B C	R[A] := R[B] * R[C]				*/
    OP_MOD,  /*	A B C	R[A] := R[B] % R[C]				*/
    OP_POW,  /*	A B C	R[A] := R[B] ^ R[C]				*/
    OP_DIV,  /*	A B C	R[A] := R[B] / R[C]				*/
    OP_IDIV, /*	A B C	R[A] := R[B] // R[C]				*/

    OP_BAND, /*	A B C	R[A] := R[B] & R[C]				*/
    OP_BOR,  /*	A B C	R[A] := R[B] | R[C]				*/
    OP_BXOR, /*	A B C	R[A] := R[B] ~ R[C]				*/
    OP_SHL,  /*	A B C	R[A] := R[B] << R[C]				*/
    OP_SHR,  /*	A B C	R[A] := R[B] >> R[C]				*/

    OP_MMBIN,  /*	A B C	call C metamethod over R[A] and R[B]	(*)	*/
    OP_MMBINI, /*	A sB C k	call C metamethod over R[A] and sB	*/
    OP_MMBINK, /*	A B C k		call C metamethod over R[A] and K[B]	*/

    OP_UNM,  /*	A B	R[A] := -R[B]					*/
    OP_BNOT, /*	A B	R[A] := ~R[B]					*/
    OP_NOT,  /*	A B	R[A] := not R[B]				*/
    OP_LEN,  /*	A B	R[A] := #R[B] (length operator)			*/

    OP_CONCAT, /*	A B	R[A] := R[A].. ... ..R[A + B - 1]		*/

    OP_CLOSE, /*	A	close all upvalues >= R[A]			*/
    OP_TBC,   /*	A	mark variable A "to be closed"			*/
    OP_JMP,   /*	sJ	pc += sJ					*/
    OP_EQ,    /*	A B k	if ((R[A] == R[B]) ~= k) then pc++		*/
    OP_LT,    /*	A B k	if ((R[A] <  R[B]) ~= k) then pc++		*/
    OP_LE,    /*	A B k	if ((R[A] <= R[B]) ~= k) then pc++		*/

    OP_EQK, /*	A B k	if ((R[A] == K[B]) ~= k) then pc++		*/
    OP_EQI, /*	A sB k	if ((R[A] == sB) ~= k) then pc++		*/
    OP_LTI, /*	A sB k	if ((R[A] < sB) ~= k) then pc++			*/
    OP_LEI, /*	A sB k	if ((R[A] <= sB) ~= k) then pc++		*/
    OP_GTI, /*	A sB k	if ((R[A] > sB) ~= k) then pc++			*/
    OP_GEI, /*	A sB k	if ((R[A] >= sB) ~= k) then pc++		*/

    OP_TEST,    /*	A k	if (not R[A] == k) then pc++			*/
    OP_TESTSET, /*	A B k	if (not R[B] == k) then pc++ else R[A] := R[B] (*) */

    OP_CALL,     /*	A B C	R[A], ... ,R[A+C-2] := R[A](R[A+1], ... ,R[A+B-1]) */
    OP_TAILCALL, /*	A B C k	return R[A](R[A+1], ... ,R[A+B-1])		*/

    OP_RETURN,  /*	A B C k	return R[A], ... ,R[A+B-2]	(see note)	*/
    OP_RETURN0, /*		return						*/
    OP_RETURN1, /*	A	return R[A]					*/

    OP_FORLOOP, /*	A Bx	update counters; if loop continues then pc-=Bx; */
    OP_FORPREP, /*	A Bx	<check values and prepare counters>;
                if not to run then pc+=Bx+1;			*/

    OP_TFORPREP, /*	A Bx	create upvalue for R[A + 3]; pc+=Bx		*/
    OP_TFORCALL, /*	A C	R[A+4], ... ,R[A+3+C] := R[A](R[A+1], R[A+2]);	*/
    OP_TFORLOOP, /*	A Bx	if R[A+2] ~= nil then { R[A]=R[A+2]; pc -= Bx }	*/

    OP_SETLIST, /*	A B C k	R[A][C+i] := R[A+i], 1 <= i <= B		*/

    OP_CLOSURE, /*	A Bx	R[A] := closure(KPROTO[Bx])			*/

    OP_VARARG, /*	A C	R[A], R[A+1], ..., R[A+C-2] = vararg		*/

    OP_VARARGPREP, /*A	(adjust vararg parameters)			*/

    OP_EXTRAARG, /*	Ax	extra (larger) argument for previous opcode	*/

    OP_debug,
}

impl Display for LuaOpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &format!("{:?}", self)[3..])
    }
}

impl LuaOpCode {
    pub fn to_iabc(self, a: u8, k: bool, b: u8, c: u8) -> iABC {
        iABC::new(c, b, k, a, self)
    }
    pub fn to_iabx(self, a: u8, b: u32) -> iABx {
        iABx::new(b, a, self)
    }
    pub fn to_iax(self, a: u32) -> iAx {
        iAx::new(a, self)
    }
    pub fn to_iasbx(self, a: u8, b: u32) -> iAsBx {
        iAsBx::new(b, a, self)
    }
    pub fn to_isj(self, a: u8, b: u32) -> isJ {
        isJ::new(b, a, self)
    }
}

// #[allow(non_camel_case_types)]
// enum RibbitOpCode {
//     JUMP_SYM_S,
//     JUMP_INT_L,
//     JUMP_SYM_L,
//     JUMP_INT_S,
//
//     CALL_SYM_S,
//     CALL_INT_L,
//     CALL_SYM_L,
//     CALL_INT_S,
//
//     SET_INT_L,
//     SET_SYM_L,
//     SET_SYM_S,
//     SET_INT_S,
//
//     GET_INT_S,
//     GET_INT_L,
//     GET_SYM_L,
//     GET_SYM_S,
//
//     CONST_INT_S,
//     CONST_INT_L,
//     CONST_SYM_L,
//     CONST_PROC_S,
//     CONST_PROC_L,
//     CONST_SYM_S,
//
//     SKIP_INT_S,
//     SKIP_LONG_L,
//
//     IF,
// }
