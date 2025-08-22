//! Parsers for the components of an iCalendar object.

use std::{fmt::Debug, hash::Hash};

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, empty, fail, preceded, repeat, terminated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::literal,
};

use crate::{
    model::{
        component::{
            Component, Entry, Event, Key, OffsetProp, OffsetPropName,
            OffsetTable, OtherComponent, TimeZone, TimeZoneProp,
            TimeZonePropName, TimeZoneTable, Todo, TzRule, TzRuleKind,
        },
        property::Prop,
    },
    parser::{
        escaped::{Equiv, LineFoldCaseless},
        primitive::{ascii_lower, iana_token, x_name},
        property::{
            KnownProp, Prop as ParserProp, PropName, Rfc5545PropName,
            UnknownProp,
        },
    },
};

use super::{
    error::CalendarParseError,
    property::{ParsedProp, property},
};

// WARN: it is possible for the name of a component (say a long x-name) to be
// folded over multiple lines. since END is shorter than BEGIN, it is possible
// that the name will be folded differently in the beginning and in the end of
// the component. therefore a simple equality check on the names DOES NOT
// suffice to parse input correctly

pub fn component<I, E>(input: &mut I) -> Result<Component<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<Caseless<I::Slice>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr
        + Clone
        + PartialEq
        + Eq
        + SliceLen
        + Stream
        + Hash
        + Equiv<LineFoldCaseless>
        + AsRef<[u8]>,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let kind = terminated(begin(comp_kind), crlf).parse_next(input)?;

    let result = match kind {
        CalCompKind::Event => event.map(Component::Event).parse_next(input),
        CalCompKind::Todo => todo.map(Component::Todo).parse_next(input),
        CalCompKind::Journal => todo!(),
        CalCompKind::FreeBusy => todo!(),
        CalCompKind::TimeZone => {
            timezone.map(Component::TimeZone).parse_next(input)
        }
        CalCompKind::Iana(_) => other.map(Component::Iana).parse_next(input),
        CalCompKind::X(_) => other.map(Component::X).parse_next(input),
    }?;

    let () = terminated(end(kind.parser()), crlf).parse_next(input)?;

    Ok(result)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StateMachine<S, F> {
    state: S,
    step: F,
}

impl<S, F> StateMachine<S, F> {
    fn new(step: F) -> Self
    where
        S: Default,
    {
        Self {
            state: Default::default(),
            step,
        }
    }

    fn parse_next<I, E>(mut self, input: &mut I) -> Result<S, E>
    where
        I: StreamIsPartial
            + Stream
            + Compare<Caseless<&'static str>>
            + Compare<char>,
        I::Token: AsChar + Clone,
        I::Slice: AsBStr + Clone + Eq + SliceLen + Stream,
        <<I as Stream>::Slice as Stream>::Token: AsChar,
        E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
        F: FnMut(
            ParsedProp<I::Slice>,
            &mut S,
        ) -> Result<(), CalendarParseError<I::Slice>>,
    {
        loop {
            let checkpoint = input.checkpoint();

            // if we run into a BEGIN or END line, we're done
            if let Ok(()) = alt((begin(empty::<I, E>), end(empty::<I, E>)))
                .parse_next(input)
            {
                input.reset(&checkpoint);
                return Ok(self.state);
            // otherwise reset the input
            } else {
                input.reset(&checkpoint);
            }

            // parse a property and apply the step function
            let parsed_prop = terminated(property, crlf).parse_next(input)?;
            match (self.step)(parsed_prop, &mut self.state) {
                Ok(()) => (),
                Err(err) => return Err(E::from_external_error(input, err)),
            }
        }
    }
}

macro_rules! step_inner {
    (
        $state:ident, $comp_kind:ident, $prop:ident, $unknown_params:ident;
        $($p:pat => $body:expr),* $(,)?
    ) => {
        match $prop {
            $(
                $p => $body,
            )*
            ParserProp::Known(prop) => {
                Err(CalendarParseError::UnexpectedProp {
                    prop: prop.name(),
                    component: super::error::ComponentKind::$comp_kind,
                })
            }
            ParserProp::Unknown(UnknownProp::Iana {
                name,
                value,
                params,
            }) => {
                $state.insert_iana(
                    name,
                    Prop {
                        value: Box::new(value),
                        params,
                        unknown_params: $unknown_params,
                    },
                );

                Ok(())
            }
            ParserProp::Unknown(UnknownProp::X {
                name,
                value,
                params,
            }) => {
                $state.insert_x(
                    name,
                    Prop {
                        value: Box::new(value),
                        params,
                        unknown_params: $unknown_params,
                    },
                );

                Ok(())
            }
        }
    };
}

macro_rules! try_insert_once {
    ($state:ident, $component:ident, $key:expr, $name:expr, $ret:expr $(,)?) => {
        match $state.get($key) {
            Some(_) => Err(CalendarParseError::UnexpectedProp {
                prop: $name,
                component: super::error::ComponentKind::$component,
            }),
            None => {
                let _prev = $state.insert($ret);
                debug_assert!(_prev.is_none());
                Ok(())
            }
        }
    };
}

macro_rules! insert_seq {
    ($state:ident, $component:ident, $name:ident, $key:expr, $ret:expr $(,)?) => {
        match $state.get_mut($key) {
            Some(Entry::Known($component::$name(props))) => {
                props.push($ret);
                Ok(())
            }
            Some(_) => unreachable!(),
            None => {
                let props = vec![$ret];
                let entry = Entry::Known($component::$name(props));
                $state.insert(entry);
                Ok(())
            }
        }
    };
}

/// Parses an [`Event`].
fn event<I, E>(input: &mut I) -> Result<Event<I::Slice>, E>
where
    I: StreamIsPartial + Stream,
    E: ParserError<I>,
{
    todo!()
}

/// Parses a [`Todo`].
fn todo<I, E>(input: &mut I) -> Result<Todo<I::Slice>, E>
where
    I: StreamIsPartial + Stream,
    E: ParserError<I>,
{
    todo!()
}

/// Parses a [`TimeZone`].
fn timezone<I, E>(input: &mut I) -> Result<TimeZone<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr
        + Clone
        + PartialEq
        + Eq
        + SliceLen
        + Stream
        + Equiv<LineFoldCaseless>
        + AsRef<[u8]>
        + Hash,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    fn tz_step<S>(
        (prop, unknown_params): ParsedProp<S>,
        state: &mut TimeZoneTable<S>,
    ) -> Result<(), CalendarParseError<S>>
    where
        S: Hash + PartialEq + Debug + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        step_inner! {state, TimeZone, prop, unknown_params;
            ParserProp::Known(KnownProp::TzId(tz_id)) => {
                try_insert_once!(state, TimeZone, Key::Known(TimeZonePropName::TzId),
                    PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier),
                    Entry::Known(TimeZoneProp::TzId(Prop {
                        value: tz_id,
                        params: (),
                        unknown_params
                    })),
                )
            },
            ParserProp::Known(KnownProp::LastModified(dt)) => {
                try_insert_once!(state, TimeZone, Key::Known(TimeZonePropName::LastModified),
                    PropName::Rfc5545(Rfc5545PropName::LastModified),
                    Entry::Known(TimeZoneProp::LastModified(Prop {
                        value: dt,
                        params: (),
                        unknown_params
                    })),
                )
            },
            ParserProp::Known(KnownProp::TzUrl(tz_url)) => {
                try_insert_once!(state, TimeZone, Key::Known(TimeZonePropName::TzUrl),
                    PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl),
                    Entry::Known(TimeZoneProp::TzUrl(Prop {
                        value: tz_url,
                        params: (),
                        unknown_params
                    })),
                )
            }
        }
    }

    fn rule_step<S>(
        (prop, unknown_params): ParsedProp<S>,
        state: &mut OffsetTable<S>,
    ) -> Result<(), CalendarParseError<S>>
    where
        S: Hash + PartialEq + Debug + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        step_inner! {state, StandardOrDaylight, prop, unknown_params;
            ParserProp::Known(KnownProp::DtStart(value, params)) => {
                try_insert_once!(state, StandardOrDaylight, Key::Known(OffsetPropName::DtStart),
                    PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
                    Entry::Known(OffsetProp::DtStart(Prop {
                        value,
                        params,
                        unknown_params
                    })),
                )
            },
            ParserProp::Known(KnownProp::TzOffsetTo(value)) => {
                try_insert_once!(state, StandardOrDaylight, Key::Known(OffsetPropName::TzOffsetTo),
                    PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo),
                    Entry::Known(OffsetProp::TzOffsetTo(Prop {
                        value,
                        params: (),
                        unknown_params
                    })),
                )
            },
            ParserProp::Known(KnownProp::TzOffsetFrom(value)) => {
                try_insert_once!(state, StandardOrDaylight, Key::Known(OffsetPropName::TzOffsetFrom),
                    PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom),
                    Entry::Known(OffsetProp::TzOffsetFrom(Prop {
                        value,
                        params: (),
                        unknown_params
                    })),
                )
            },
            ParserProp::Known(KnownProp::RRule(value)) => {
                try_insert_once!(state, StandardOrDaylight, Key::Known(OffsetPropName::RRule),
                    PropName::Rfc5545(Rfc5545PropName::RecurrenceRule),
                    Entry::Known(OffsetProp::RRule(Prop {
                        value: Box::new(value),
                        params: (),
                        unknown_params
                    })),
                )
            },
            ParserProp::Known(KnownProp::Comment(value, params)) => {
                insert_seq!(state, OffsetProp, Comment, Key::Known(OffsetPropName::Comment), Prop {
                    value,
                    params,
                    unknown_params,
                })
            },
            ParserProp::Known(KnownProp::RDate(value, params)) => {
                insert_seq!(state, OffsetProp, RDate, Key::Known(OffsetPropName::RDate), Prop {
                    value,
                    params,
                    unknown_params,
                })
            },
            ParserProp::Known(KnownProp::TzName(value, params)) => {
                insert_seq!(state, OffsetProp, TzName, Key::Known(OffsetPropName::TzName), Prop {
                    value,
                    params,
                    unknown_params,
                })
            },
        }
    }

    fn rule_kind<I, E>(input: &mut I) -> Result<TzRuleKind, E>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
        E: ParserError<I>,
    {
        alt((
            Caseless("STANDARD").value(TzRuleKind::Standard),
            Caseless("DAYLIGHT").value(TzRuleKind::Daylight),
        ))
        .parse_next(input)
    }

    fn rule<I, E>(input: &mut I) -> Result<TzRule<I::Slice>, E>
    where
        I: StreamIsPartial
            + Stream
            + Compare<Caseless<&'static str>>
            + Compare<char>,
        I::Token: AsChar + Clone,
        I::Slice: AsBStr
            + Clone
            + Stream
            + SliceLen
            + Hash
            + PartialEq
            + Eq
            + Debug
            + Equiv<LineFoldCaseless>
            + AsRef<[u8]>,
        <<I as Stream>::Slice as Stream>::Token: AsChar,
        E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
    {
        let kind = terminated(begin(rule_kind), crlf).parse_next(input)?;
        let props = StateMachine::new(rule_step).parse_next(input)?;

        match terminated(end(rule_kind), crlf).parse_next(input)? == kind {
            true => Ok(TzRule { props, kind }),
            false => fail.parse_next(input),
        }
    }

    terminated(begin(CalCompKind::TimeZone.parser()), crlf)
        .parse_next(input)?;
    let props = StateMachine::new(tz_step).parse_next(input)?;
    let subcomponents = repeat(1.., rule).parse_next(input)?;
    terminated(end(CalCompKind::TimeZone.parser()), crlf).parse_next(input)?;

    Ok(TimeZone {
        props,
        subcomponents,
    })
}

/// Parses an [`OtherComponent`].
fn other<I, E>(input: &mut I) -> Result<OtherComponent<I::Slice>, E>
where
    I: StreamIsPartial + Stream,
    E: ParserError<I>,
{
    // NOTE: the grammar for the "other" components (iana/x-name) just says
    // that they shall have at least one content line between the beginning
    // and the end of the component. obviously we can parse property lines,
    // but what about subcomponents? are they legal? what does libical do
    // here?

    todo!()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalCompKind<S> {
    Event,
    Todo,
    Journal,
    FreeBusy,
    TimeZone,
    Iana(S),
    X(S),
}

impl<S> CalCompKind<S> {
    /// Creates a parser corresponding to `self`, which accepts itself. For the
    /// static variants (i.e. the variants other than Iana and X), this amounts
    /// to a comparison to a Caseless<&'static str>. However, for the dynamic
    /// variants we must be careful to ignore line folds in both the input and
    /// the stored value of S, so we require some additional trait bounds.
    ///
    /// Take as an example the BEGIN and END lines around an X-component. If the
    /// name of the component is sufficiently long, it may be folded differently
    /// in both cases. Hence, a simple equality comparison does not suffice to
    /// parse the input correctly, and we must use the captured slice from the
    /// BEGIN line as a parser when encountering the END line.
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
    use crate::{
        date,
        model::primitive::{DateTime, Local, Sign, Text, TzId, Utc, UtcOffset},
        time,
    };

    use super::*;

    #[test]
    fn timezone_parser() {
        let input = concat!(
            "BEGIN:VTIMEZONE",
            "\r\n",
            "TZID:America/New_York",
            "\r\n",
            "LAST-MODIFIED:20050809T020000Z",
            "\r\n",
            "BEGIN:DAYLIGHT",
            "\r\n",
            "DTSTART:19670430T020000",
            "\r\n",
            "RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z",
            "\r\n",
            "TZOFFSETFROM:-0500",
            "\r\n",
            "TZOFFSETTO:-0400",
            "\r\n",
            "TZNAME:EDT",
            "\r\n",
            "END:DAYLIGHT",
            "\r\n",
            "BEGIN:STANDARD",
            "\r\n",
            "DTSTART:19671029T020000",
            "\r\n",
            "RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z",
            "\r\n",
            "TZOFFSETFROM:-0400",
            "\r\n",
            "TZOFFSETTO:-0500",
            "\r\n",
            "TZNAME:EST",
            "\r\n",
            "END:STANDARD",
            "\r\n",
            "END:VTIMEZONE",
            "\r\n",
        );

        let (tail, tz) = timezone::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        dbg![&tz];

        // properties

        let tz_id = tz.id().unwrap();
        let last_modified = tz.last_modified().unwrap();
        let tz_url = tz.url();

        assert!(tz_url.is_none());

        assert_eq!(tz_id.value, TzId("America/New_York"));
        assert!(tz_id.unknown_params.is_empty());

        assert_eq!(
            last_modified.value,
            DateTime {
                date: date!(2005;8;9),
                time: time!(2;00;00, Utc)
            }
        );
        assert!(last_modified.unknown_params.is_empty());

        // subcomponents
        let rules = tz.rules();

        assert_eq!(rules.len(), 2);

        let daylight = &rules[0];
        assert_eq!(daylight.kind(), TzRuleKind::Daylight);
        assert_eq!(
            daylight.start(),
            &Prop::from_value(
                DateTime {
                    date: date!(1967;4;30),
                    time: time!(2;00;00, Local)
                }
                .into()
            ),
        );

        assert!(daylight.rrule().is_some());

        assert_eq!(
            daylight.offset_to(),
            &Prop::from_value(UtcOffset {
                sign: Sign::Negative,
                hours: 4,
                minutes: 0,
                seconds: None,
            })
        );

        assert_eq!(
            daylight.offset_from(),
            &Prop::from_value(UtcOffset {
                sign: Sign::Negative,
                hours: 5,
                minutes: 0,
                seconds: None,
            })
        );

        assert_eq!(
            daylight.names(),
            Some([Prop::from_value(Text("EDT"))].as_slice())
        );

        let standard = &rules[1];
        assert_eq!(standard.kind(), TzRuleKind::Standard);

        assert_eq!(
            standard.start(),
            &Prop::from_value(
                DateTime {
                    date: date!(1967;10;29),
                    time: time!(2;00;00, Local)
                }
                .into()
            ),
        );

        assert!(standard.rrule().is_some());

        assert_eq!(
            standard.offset_to(),
            &Prop::from_value(UtcOffset {
                sign: Sign::Negative,
                hours: 5,
                minutes: 0,
                seconds: None,
            })
        );

        assert_eq!(
            standard.offset_from(),
            &Prop::from_value(UtcOffset {
                sign: Sign::Negative,
                hours: 4,
                minutes: 0,
                seconds: None,
            })
        );

        assert_eq!(
            standard.names(),
            Some([Prop::from_value(Text("EST"))].as_slice())
        );
    }

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
