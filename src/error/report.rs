use std::fmt::Display;

use crate::{error::diagnostic::Diagnostic, line_map::LineMap};

pub struct Report<'a> {
    source: &'a str,
    line_map: LineMap,
    diagnostic: &'a dyn Diagnostic,
}

impl<'a> Report<'a> {
    pub fn new(source: &'a str, line_map: LineMap, diagnostic: &'a dyn Diagnostic) -> Self {
        Self {
            source,
            line_map,
            diagnostic,
        }
    }
}

impl<'a> Display for Report<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let span = self.diagnostic.span();
        let (line_start, col_num) = self.line_map.get_position(span.start_byte_pos).unwrap();

        let padding = " ".repeat(span.line_num.to_string().len());
        let pointer_len = (span.end_byte_pos - span.start_byte_pos).max(1);
        let pointer = format!("{}{}", " ".repeat(col_num), "^".repeat(pointer_len));
        let line_end = self.source[line_start..]
            .find('\n')
            .map(|i| line_start + i)
            .unwrap_or(self.source.len());
        let line_str = &self.source[line_start..line_end].trim_end_matches('\n');

        writeln!(f, "error: {}", self.diagnostic.message())?;

        writeln!(f, "{}--> repl:{}:{}", padding, span.line_num, span.col_num)?;
        writeln!(f, "{} |", padding)?;
        writeln!(f, "{} | {}", span.line_num, line_str)?;
        writeln!(f, "{} | {}", padding, pointer)?;

        if let Some(help) = self.diagnostic.help() {
            writeln!(f, "{} |", padding)?;
            writeln!(f, "{}= help: {}", padding, help)?;
        }

        Ok(())
    }
}
