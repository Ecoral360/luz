mod exp;
pub mod parser;
mod stat;

use derive_new::new;
pub use exp::*;
pub use stat::*;

#[derive(Debug, Clone, new, Copy, Default)]
pub struct LineInfo {
    pub start_pos: usize,
    pub end_pos: usize,
    pub start_line_col: (usize, usize),
    pub end_line_col: (usize, usize),
}

impl LineInfo {
    pub fn format_with_filename(&self, filename: &str) -> String {
        format!("<{}:{},{}>", filename, self.start_line_col.0, self.end_line_col.0)
    }
}
