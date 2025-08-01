#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub start_byte_pos: usize,
    pub end_byte_pos: usize,
    pub line_num: usize,
    pub col_num: usize,
}
