//! Parsers for the components of an iCalendar object.

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, preceded, terminated},
    error::{FromExternalError, ParserError},
    stream::{AsChar, Compare, Stream, StreamIsPartial},
    token::literal,
};

use crate::{
    model::component::Component,
    parser::primitive::{ascii_lower, iana_token, x_name},
};

use super::error::CalendarParseError;

// WARN: it is possible for the name of a component (say a long x-name) to be
// folded over multiple lines. since END is shorter than BEGIN, it is possible
// that the name will be folded differently in the beginning and in the end of
// the component. therefore a simple equality check on the names DOES NOT
// suffice to parse input correctly

pub fn component<I, E>(input: &mut I) -> Result<Component<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<char>
        + Compare<Caseless<&'static str>>
        + Compare<Caseless<I::Slice>>,
    I::Slice: std::fmt::Debug + Clone,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let kind = terminated(begin(comp_kind), crlf).parse_next(input)?;
    // TODO: parse the contents of the component
    let () = terminated(end(kind.parser()), crlf).parse_next(input)?;
    todo!()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CalCompKind<S> {
    Event,
    Todo,
    Journal,
    FreeBusy,
    TimeZone,
    Iana(S),
    X(S),
}

impl<S> CalCompKind<S> {
    fn parser<I, E>(&self) -> impl Parser<I, (), E>
    where
        I: StreamIsPartial
            + Stream
            + Compare<Caseless<S>>
            + Compare<Caseless<&'static str>>,
        I::Token: AsChar + Clone,
        S: std::fmt::Debug + Clone,
        E: ParserError<I>,
    {
        move |input: &mut I| match self {
            CalCompKind::Event => Caseless("VEVENT").void().parse_next(input),
            CalCompKind::Todo => Caseless("VTODO").void().parse_next(input),
            CalCompKind::Journal => {
                Caseless("VJOURNAL").void().parse_next(input)
            }
            CalCompKind::FreeBusy => {
                Caseless("VFREEBUSY").void().parse_next(input)
            }
            CalCompKind::TimeZone => {
                Caseless("VTIMEZONE").void().parse_next(input)
            }
            CalCompKind::Iana(name) | CalCompKind::X(name) => {
                literal(Caseless(name.clone())).void().parse_next(input)
            }
        }
    }
}

/// Parses a [`CalCompKind`].
fn comp_kind<I, E>(input: &mut I) -> Result<CalCompKind<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<char>
        + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    /// Parses a static variant of [`CalCompKind`] (i.e. not including iana tokens
    /// or x-names).
    fn static_comp_kind<I>(input: &mut I) -> Result<CalCompKind<I::Slice>, ()>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
        I::Token: AsChar + Clone,
    {
        match ascii_lower.parse_next(input)? {
            'v' => match ascii_lower.parse_next(input)? {
                'e' => {
                    let _tail = Caseless("vent").parse_next(input)?;
                    Ok(CalCompKind::Event)
                }
                'f' => {
                    let _tail = Caseless("reebusy").parse_next(input)?;
                    Ok(CalCompKind::FreeBusy)
                }
                'j' => {
                    let _tail = Caseless("ournal").parse_next(input)?;
                    Ok(CalCompKind::Journal)
                }
                // VTODO | VTIMEZONE
                't' => match ascii_lower.parse_next(input)? {
                    'i' => {
                        let _tail = Caseless("mezone").parse_next(input)?;
                        Ok(CalCompKind::TimeZone)
                    }
                    'o' => {
                        let _tail = Caseless("do").parse_next(input)?;
                        Ok(CalCompKind::Todo)
                    }
                    _ => Err(()),
                },
                _ => Err(()),
            },
            _ => Err(()),
        }
    }

    let checkpoint = input.checkpoint();
    match static_comp_kind.parse_next(input) {
        Ok(res) => Ok(res),
        Err(()) => {
            input.reset(&checkpoint);

            alt((
                x_name.map(CalCompKind::X),
                iana_token.map(CalCompKind::Iana),
            ))
            .parse_next(input)
        }
    }
}

/// Parses the `BGEIN:<name>` sequence at the start of a component.
fn begin<I, O, E>(name: impl Parser<I, O, E>) -> impl Parser<I, O, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    preceded(Caseless("BEGIN:"), name)
}

/// Parses the `END:<name>` sequence at the end of a component.
fn end<I, O, E>(name: impl Parser<I, O, E>) -> impl Parser<I, O, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    preceded(Caseless("END:"), name)
}

/// A version of [`winnow::ascii::crlf`] bounded by `Compare<char>` instead
/// of `Compare<&'static str>`.
fn crlf<I, E>(input: &mut I) -> Result<I::Slice, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    E: ParserError<I>,
{
    ('\r', '\n').take().parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comp_kind_parser() {
        assert_eq!(
            comp_kind::<_, ()>.parse_peek("VEVENT"),
            Ok(("", CalCompKind::Event))
        );

        assert_eq!(
            comp_kind::<_, ()>.parse_peek("vtodo"),
            Ok(("", CalCompKind::Todo))
        );

        assert_eq!(
            comp_kind::<_, ()>.parse_peek("VJournal"),
            Ok(("", CalCompKind::Journal))
        );

        assert_eq!(
            comp_kind::<_, ()>.parse_peek("vFreeBusy"),
            Ok(("", CalCompKind::FreeBusy))
        );

        assert_eq!(
            comp_kind::<_, ()>.parse_peek("vtimezone"),
            Ok(("", CalCompKind::TimeZone))
        );

        assert_eq!(
            comp_kind::<_, ()>.parse_peek("vtimezane"),
            Ok(("", CalCompKind::Iana("vtimezane")))
        );

        assert_eq!(
            comp_kind::<_, ()>.parse_peek("x-something"),
            Ok(("", CalCompKind::X("x-something")))
        );
    }

    #[test]
    fn begin_parser() {
        assert_eq!(
            begin::<_, _, ()>(Caseless("vtodo").take())
                .parse_peek("BEGIN:VTODO"),
            Ok(("", "VTODO"))
        );

        assert_eq!(
            begin::<_, _, ()>(Caseless("VEVENT").take())
                .parse_peek("begin:vevent"),
            Ok(("", "vevent"))
        );
    }

    #[test]
    fn end_parser() {
        assert_eq!(
            end::<_, _, ()>(Caseless("valarm").take()).parse_peek("END:VALARM"),
            Ok(("", "VALARM"))
        );

        assert_eq!(
            end::<_, _, ()>(Caseless("VFREEBUSY").take())
                .parse_peek("end:vfreebusy"),
            Ok(("", "vfreebusy"))
        );
    }
}
