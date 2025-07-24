#[derive(Debug)]
pub struct LineMap {
    pub source_len: usize,
    pub line_starts: Vec<usize>,
}

impl LineMap {
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];
        for (pos, ch) in source.char_indices() {
            if ch == '\n' {
                line_starts.push(pos + 1);
            }
        }
        LineMap {
            source_len: source.len(),
            line_starts,
        }
    }

    // given a byte position, returns the column number and the line number (zero based)
    pub fn get_position(&self, byte_pos: usize) -> Option<(usize, usize)> {
        if byte_pos >= self.source_len {
            None
        } else {
            let idx = self.line_starts.partition_point(|pos| *pos < byte_pos);
            if idx == 0 {
                Some((self.line_starts[idx], byte_pos))
            } else {
                Some((
                    self.line_starts[idx - 1],
                    byte_pos - self.line_starts[idx - 1],
                ))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::line_map::LineMap;

    #[test]
    fn test_line_map() {
        let line_map = LineMap::new("Hello\nJames\nThis\nis\nfun.");
        assert_eq!(line_map.source_len, 24);
        assert_eq!(line_map.line_starts, vec![0, 6, 12, 17, 20]);
        assert_eq!(line_map.get_position(0), Some((0, 0)));
        assert_eq!(line_map.get_position(2), Some((0, 2)));
        assert_eq!(line_map.get_position(7), Some((6, 1)));
        assert_eq!(line_map.get_position(10), Some((6, 4)));
        assert_eq!(line_map.get_position(19), Some((17, 2)));
        assert_eq!(line_map.get_position(21), Some((20, 1)));
        assert_eq!(line_map.get_position(25), None);
    }
}
