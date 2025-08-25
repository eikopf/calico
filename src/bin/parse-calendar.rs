use std::time::Instant;

use calico::parser::{component::calendar, escaped::AsEscaped};
use winnow::Parser;

/// Takes a file path as an argument, parses it, and then prints some data about
/// how long the parsing took, and whether it parsed the entire file or not.
pub fn main() {
    let mut args = std::env::args();
    let _ = args.next();
    let path = args.next().unwrap();

    let load_start = Instant::now();
    let input = std::fs::read_to_string(path).unwrap();

    let parse_start = Instant::now();
    let (tail, cal) = calendar::<_, ()>.parse_peek(input.as_escaped()).unwrap();

    let end = Instant::now();
    dbg![cal.components().len()];
    dbg![tail.len()];
    eprintln!("load time: {:?}", end - load_start);
    eprintln!("parse time: {:?}", end - parse_start);
}
