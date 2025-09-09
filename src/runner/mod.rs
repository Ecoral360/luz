use crate::{
    compiler::{instructions::Instruction, Register},
    luz::obj::LuzObj,
};

pub struct Runner {
    instructions: Vec<Instruction>,
    constants: Vec<LuzObj>,
    regs: Vec<Register>,
}
