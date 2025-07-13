//! A [`winnow`]-based RFC 5545 parser implementation.

use std::io::{BufRead, Cursor};

use line::{LineError, Lines};

pub mod escaped;
pub mod line;
pub mod parameter;
pub mod primitive;
pub mod property;

#[derive(Debug, Clone, Copy)]
pub struct Parser<B> {
    lines: Lines<B>,
    line_index: usize,
}

pub enum ParseError {
    LineError(LineError),
    UnexpectedEof,
}

impl From<LineError> for ParseError {
    fn from(v: LineError) -> Self {
        Self::LineError(v)
    }
}

impl<'a> Parser<Cursor<&'a str>> {
    pub fn new_from_str(source: &'a str) -> Self {
        Parser::new(Cursor::new(source))
    }
}

impl<B: BufRead> Parser<B> {
    pub fn new(source: B) -> Self {
        Self {
            lines: Lines::new(source),
            line_index: 0,
        }
    }
}
