mod exp;
pub mod parser;
mod stat;

use derive_new::new;
pub use exp::*;
pub use stat::*;

#[derive(Debug, Clone, new)]
pub struct LineInfo {
    pub content: String,
    pub line_no: usize,
    pub col_no: usize,
}
