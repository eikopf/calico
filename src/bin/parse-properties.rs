use std::time::Instant;

use calico::parser::{escaped::AsEscaped, property::property};
use winnow::{
    Parser,
    ascii::crlf,
    combinator::{repeat, terminated},
};

/// Takes a file path as an argument, parses it, and then prints some data about
/// how long the parsing took, and whether it parsed the entire file or not.
pub fn main() {
    let mut args = std::env::args();
    let _ = args.next();
    let path = args.next().unwrap();

    let load_start = Instant::now();
    let input = std::fs::read_to_string(path).unwrap();

    let parse_start = Instant::now();
    let (tail, props) = repeat(0.., terminated(property::<_, ()>, crlf))
        .map(Vec::into_boxed_slice)
        .parse_peek(input.as_escaped())
        .unwrap();

    let end = Instant::now();
    eprintln!("parsed {} properties (incl. BEGIN/END)", props.len());
    eprintln!("parsed bytes: {}", input.len() - tail.len());
    eprintln!("remaining bytes: {}", tail.len());
    eprintln!("total time: {:?}", end - load_start);
    eprintln!("parse time: {:?}", end - parse_start);
}
