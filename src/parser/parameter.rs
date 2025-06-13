//! Property parameters.

use winnow::{
    ModalResult, Parser,
    combinator::{alt, delimited, preceded, repeat},
    error::ParserError,
    token::none_of,
};

use crate::model::primitive::{CalendarUserType as CalendarUserTypeValue, Uri};

use super::primitive::{calendar_user_type, uri};

pub fn parameter<'a, O, E>(
    name: &'static str,
    value: impl Parser<&'a str, O, E>,
) -> impl Parser<&'a str, O, E>
where
    E: ParserError<&'a str>,
{
    preceded((name, '='), value)
}

/// Parses a parameter value string, stripping double quotes if they occur.
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::param_value;
/// use winnow::Parser;
///
/// assert!(param_value.parse_peek("hello world").is_ok());
/// assert!(param_value.parse_peek("\"hello, world\"").is_ok());
/// assert!(param_value.parse_peek(",hello world").is_err());
/// ```
pub fn param_value<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
    fn param_text<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        repeat(1.., none_of((..' ', '"', ',', ':', ';', '\u{007F}')))
            .map(|()| ())
            .take()
            .parse_next(input)
    }

    fn quoted_string<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        delimited(
            '"',
            repeat(1.., none_of((..' ', '"', '\u{007F}')))
                .map(|()| ())
                .take(),
            '"',
        )
        .parse_next(input)
    }

    alt((quoted_string, param_text)).parse_next(input)
}

/// RFC 5545 §3.2.1
#[derive(Debug, Clone)]
pub struct AltRep(Uri);

/// Parses the [`AltRep`] parameter.
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::altrep;
/// use winnow::Parser;
///
/// assert!(altrep.parse_peek("ALTREP=\"CID:part3.msg.970415T083000@example.com\"").is_ok());
/// ```
pub fn altrep(input: &mut &str) -> ModalResult<AltRep> {
    parameter("ALTREP", delimited('"', uri.map(AltRep), '"')).parse_next(input)
}

/// RFC 5545 §3.2.2
#[derive(Debug, Clone)]
pub struct CommonName(Box<str>);

/// Parses the [`CommonName`] parameter.
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::common_name;
/// use winnow::Parser;
///
/// assert!(common_name.parse_peek("CN=\"John Smith\"").is_ok());
/// assert!(common_name.parse_peek("CN=John Smith").is_ok());
/// ```
pub fn common_name(input: &mut &str) -> ModalResult<CommonName> {
    parameter("CN", param_value.map(Box::<str>::from).map(CommonName)).parse_next(input)
}

/// RFC 5545 §3.2.3
#[derive(Debug, Clone)]
pub struct CalendarUserType(CalendarUserTypeValue);

/// Parses the [`CalendarUserType`] parameter.
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::cu_type;
/// use winnow::Parser;
///
/// assert!(cu_type.parse_peek("CUTYPE=GROUP").is_ok());
/// assert!(cu_type.parse_peek("CUTYPE=\"GROUP\"").is_err());
/// ```
pub fn cu_type(input: &mut &str) -> ModalResult<CalendarUserType> {
    parameter("CUTYPE", calendar_user_type.map(CalendarUserType)).parse_next(input)
}

// NOTE: okay, so here's my current plan for implementing out-of-order parsing.
// the basic idea is to tag each parser with its allowed multiplicity (either a
// number or a range) and then use something like Stateful to track how many times
// it's successfully parsed a value. then we can just wrap them into a tuple and
// use some combinator to try all of them repeatedly until either the input ends,
// and we can check thereafter whether the success state was acheived.
// ------------------------------------------------------------------------------
// basically, we're bundling three things together here: the parsers themselves,
// multiplicity tracking, and intermediate state tracking. that is, i want an
// impl Parser that combines multiple subparsers together, tracks how many times
// they've each succeeded, accumulates their outputs, and then emits a single
// tuple afterwards (which i can then map over to construct the output type).
// ------------------------------------------------------------------------------
// each multiplicity corresponds to a certain way of tracking intermediate state.
// obviously you could get extremely complicated with this, but i think the best
// solution is just to have a bipartite distinction between mandatory and optional
// values (stored in an Option) and all other multiplicities (stored in a Vec).
