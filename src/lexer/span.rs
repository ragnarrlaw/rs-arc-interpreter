use std::usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start_byte_pos: usize,
    pub end_byte_pos: usize,
    pub line_num: usize,
    pub col_num: usize,
}

impl Span {
    pub fn default() -> Self {
        Self {
            start_byte_pos: 0,
            end_byte_pos: 0,
            line_num: 1,
            col_num: 1,
        }
    }
}
