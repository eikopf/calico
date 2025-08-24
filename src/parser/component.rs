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
            Alarm, Component, Entry, Event, FreeAlarmProp, FreeAlarmPropName, FreeAlarmTable,
            FreeBusy, FreeBusyProp, FreeBusyPropName, FreeBusyTable, Journal, JournalProp,
            JournalPropName, JournalTable, Key, OffsetProp, OffsetPropName, OffsetTable,
            OtherComponent, TimeZone, TimeZoneProp, TimeZonePropName, TimeZoneTable, Todo, TzRule,
            TzRuleKind,
        },
        primitive::{JournalStatus, Status},
        property::{Prop, TriggerProp},
    },
    parser::{
        error::ComponentKind,
        escaped::{Equiv, LineFoldCaseless},
        primitive::{ascii_lower, iana_token, x_name},
        property::{KnownProp, Prop as ParserProp, PropName, Rfc5545PropName, UnknownProp},
    },
};

use super::{
    error::CalendarParseError,
    property::{ParsedProp, property},
};

/// Parses a [`Component`].
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
        CalCompKind::Event => event.map(Into::into).parse_next(input),
        CalCompKind::Todo => todo.map(Into::into).parse_next(input),
        CalCompKind::Journal => journal.map(Into::into).parse_next(input),
        CalCompKind::FreeBusy => free_busy.map(Into::into).parse_next(input),
        CalCompKind::TimeZone => timezone.map(Into::into).parse_next(input),
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
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
        I::Token: AsChar + Clone,
        I::Slice: AsBStr + Clone + Eq + SliceLen + Stream,
        <<I as Stream>::Slice as Stream>::Token: AsChar,
        E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
        F: FnMut(ParsedProp<I::Slice>, &mut S) -> Result<(), CalendarParseError<I::Slice>>,
    {
        loop {
            let checkpoint = input.checkpoint();

            // if we run into a BEGIN or END line, we're done
            if let Ok(()) = alt((begin(empty::<I, E>), end(empty::<I, E>))).parse_next(input) {
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
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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
    todo!()
}

/// Parses a [`Todo`].
fn todo<I, E>(input: &mut I) -> Result<Todo<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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
    todo!()
}

/// Parses a [`Journal`].
fn journal<I, E>(input: &mut I) -> Result<Journal<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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
    fn step<S>(
        (prop, unknown_params): ParsedProp<S>,
        state: &mut JournalTable<S>,
    ) -> Result<(), CalendarParseError<S>>
    where
        S: Hash + PartialEq + Debug + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        macro_rules! once {
            ($name:ident, $long_name:expr, $value:expr, $params:expr) => {
                try_insert_once!(
                    state,
                    Journal,
                    Key::Known(JournalPropName::$name),
                    $long_name,
                    Entry::Known(JournalProp::$name(Prop {
                        value: $value,
                        params: $params,
                        unknown_params
                    })),
                )
            };
        }

        macro_rules! seq {
            ($name:ident, $value:expr, $params:expr) => {
                insert_seq!(
                    state,
                    JournalProp,
                    $name,
                    Key::Known(JournalPropName::$name),
                    Prop {
                        value: $value,
                        params: $params,
                        unknown_params,
                    }
                )
            };
        }

        step_inner! {state, Journal, prop, unknown_params;
            ParserProp::Known(KnownProp::DtStamp(value)) => {
                once!(DtStamp, PropName::Rfc5545(Rfc5545PropName::DateTimeStamp), value, ())
            },
            ParserProp::Known(KnownProp::Uid(value)) => {
                once!(Uid, PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier), value, ())
            },
            ParserProp::Known(KnownProp::Class(value)) => {
                once!(Class, PropName::Rfc5545(Rfc5545PropName::Classification), value, ())
            },
            ParserProp::Known(KnownProp::Created(value)) => {
                once!(Created, PropName::Rfc5545(Rfc5545PropName::DateTimeCreated), value, ())
            },
            ParserProp::Known(KnownProp::DtStart(value, params)) => {
                once!(DtStart, PropName::Rfc5545(Rfc5545PropName::DateTimeStart), value, params)
            },
            ParserProp::Known(KnownProp::LastModified(value)) => {
                once!(LastModified, PropName::Rfc5545(Rfc5545PropName::LastModified), value, ())
            },
            ParserProp::Known(KnownProp::Organizer(value, params)) => {
                once!(Organizer, PropName::Rfc5545(Rfc5545PropName::Organizer), value, Box::new(params))
            },
            ParserProp::Known(KnownProp::RecurrenceId(value, params)) => {
                once!(RecurId, PropName::Rfc5545(Rfc5545PropName::RecurrenceId), value, params)
            },
            ParserProp::Known(KnownProp::Sequence(value)) => {
                once!(Sequence, PropName::Rfc5545(Rfc5545PropName::SequenceNumber), value, ())
            },
            ParserProp::Known(KnownProp::Status(value)) => {
                let value = match value {
                    Status::Cancelled => JournalStatus::Cancelled,
                    Status::Draft => JournalStatus::Draft,
                    Status::Final => JournalStatus::Final,
                    status => return Err(CalendarParseError::InvalidJournalStatus(status)),
                };

                once!(Status, PropName::Rfc5545(Rfc5545PropName::Status), value, ())
            },
            ParserProp::Known(KnownProp::Summary(value, params)) => {
                once!(Summary, PropName::Rfc5545(Rfc5545PropName::Summary), value, params)
            },
            ParserProp::Known(KnownProp::Url(value)) => {
                once!(Url, PropName::Rfc5545(Rfc5545PropName::UniformResourceLocator), value, ())
            },
            ParserProp::Known(KnownProp::RRule(value)) => {
                once!(RRule, PropName::Rfc5545(Rfc5545PropName::RecurrenceRule), Box::new(value), ())
            },
            ParserProp::Known(KnownProp::Attach(value, params)) => {
                seq!(Attach, value, Box::new(params))
            },
            ParserProp::Known(KnownProp::Attendee(value, params)) => {
                seq!(Attendee, value, Box::new(params))
            },
            ParserProp::Known(KnownProp::Categories(value, params)) => {
                seq!(Categories, value, params)
            },
            ParserProp::Known(KnownProp::Comment(value, params)) => {
                seq!(Comment, value, params)
            },
            ParserProp::Known(KnownProp::Contact(value, params)) => {
                seq!(Contact, value, params)
            },
            ParserProp::Known(KnownProp::Description(value, params)) => {
                seq!(Description, value, params)
            },
            ParserProp::Known(KnownProp::ExDate(value, params)) => {
                seq!(ExDate, value, params)
            },
            ParserProp::Known(KnownProp::RelatedTo(value, params)) => {
                seq!(RelatedTo, value, params)
            },
            ParserProp::Known(KnownProp::RDate(value, params)) => {
                seq!(RDate, value, params)
            },
            ParserProp::Known(KnownProp::RequestStatus(value, params)) => {
                seq!(RequestStatus, value, params)
            },
        }
    }

    fn name<I, E>(input: &mut I) -> Result<(), E>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
        E: ParserError<I>,
    {
        Caseless("VJOURNAL").void().parse_next(input)
    }

    terminated(begin(name), crlf).parse_next(input)?;
    let props = StateMachine::new(step).parse_next(input)?;
    terminated(end(name), crlf).parse_next(input)?;

    // check mandatory fields
    if props.get(Key::Known(JournalPropName::DtStamp)).is_none() {
        return Err(E::from_external_error(
            input,
            CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
                component: ComponentKind::Journal,
            },
        ));
    }

    if props.get(Key::Known(JournalPropName::Uid)).is_none() {
        return Err(E::from_external_error(
            input,
            CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier),
                component: ComponentKind::Journal,
            },
        ));
    }

    Ok(Journal { props })
}

/// Parses a [`FreeBusy`].
fn free_busy<I, E>(input: &mut I) -> Result<FreeBusy<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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
    fn step<S>(
        (prop, unknown_params): ParsedProp<S>,
        state: &mut FreeBusyTable<S>,
    ) -> Result<(), CalendarParseError<S>>
    where
        S: Hash + PartialEq + Debug + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        macro_rules! once {
            ($name:ident, $long_name:expr, $value:expr, $params:expr) => {
                try_insert_once!(
                    state,
                    FreeBusy,
                    Key::Known(FreeBusyPropName::$name),
                    $long_name,
                    Entry::Known(FreeBusyProp::$name(Prop {
                        value: $value,
                        params: $params,
                        unknown_params
                    })),
                )
            };
        }

        macro_rules! seq {
            ($name:ident, $value:expr, $params:expr) => {
                insert_seq!(
                    state,
                    FreeBusyProp,
                    $name,
                    Key::Known(FreeBusyPropName::$name),
                    Prop {
                        value: $value,
                        params: $params,
                        unknown_params,
                    }
                )
            };
        }

        step_inner! {state, FreeBusy, prop, unknown_params;
            ParserProp::Known(KnownProp::DtStamp(value)) => {
                once!(DtStamp, PropName::Rfc5545(Rfc5545PropName::DateTimeStamp), value, ())
            },
            ParserProp::Known(KnownProp::Uid(value)) => {
                once!(Uid, PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier), value, ())
            },
            ParserProp::Known(KnownProp::Contact(value, params)) => {
                once!(Contact, PropName::Rfc5545(Rfc5545PropName::Contact), value, params)
            },
            ParserProp::Known(KnownProp::DtStart(value, params)) => {
                once!(DtStart, PropName::Rfc5545(Rfc5545PropName::DateTimeStart), value, params)
            },
            ParserProp::Known(KnownProp::DtEnd(value, params)) => {
                once!(DtEnd, PropName::Rfc5545(Rfc5545PropName::DateTimeEnd), value, params)
            },
            ParserProp::Known(KnownProp::Organizer(value, params)) => {
                once!(Organizer, PropName::Rfc5545(Rfc5545PropName::Organizer), value, Box::new(params))
            },
            ParserProp::Known(KnownProp::Url(value)) => {
                once!(Url, PropName::Rfc5545(Rfc5545PropName::UniformResourceLocator), value, ())
            },
            ParserProp::Known(KnownProp::Attendee(value, params)) => {
                seq!(Attendee, value, Box::new(params))
            },
            ParserProp::Known(KnownProp::Comment(value, params)) => {
                seq!(Comment, value, params)
            },
            ParserProp::Known(KnownProp::FreeBusy(value, params)) => {
                seq!(FreeBusy, value, params)
            },
            ParserProp::Known(KnownProp::RequestStatus(value, params)) => {
                seq!(RequestStatus, value, params)
            },
        }
    }

    fn name<I, E>(input: &mut I) -> Result<(), E>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
        E: ParserError<I>,
    {
        Caseless("VFREEBUSY").void().parse_next(input)
    }

    terminated(begin(name), crlf).parse_next(input)?;
    let props = StateMachine::new(step).parse_next(input)?;
    terminated(end(name), crlf).parse_next(input)?;

    // check mandatory fields
    if props.get(Key::Known(FreeBusyPropName::DtStamp)).is_none() {
        return Err(E::from_external_error(
            input,
            CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
                component: ComponentKind::FreeBusy,
            },
        ));
    }

    if props.get(Key::Known(FreeBusyPropName::Uid)).is_none() {
        return Err(E::from_external_error(
            input,
            CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier),
                component: ComponentKind::FreeBusy,
            },
        ));
    }

    Ok(FreeBusy { props })
}

/// Parses a [`TimeZone`].
fn timezone<I, E>(input: &mut I) -> Result<TimeZone<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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
        macro_rules! once {
            ($name:ident, $long_name:expr, $value:expr) => {
                try_insert_once!(
                    state,
                    TimeZone,
                    Key::Known(TimeZonePropName::$name),
                    $long_name,
                    Entry::Known(TimeZoneProp::$name(Prop {
                        value: $value,
                        params: (),
                        unknown_params
                    })),
                )
            };
        }

        step_inner! {state, TimeZone, prop, unknown_params;
            ParserProp::Known(KnownProp::TzId(value)) => {
                once!(TzId, PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier), value)
            },
            ParserProp::Known(KnownProp::LastModified(value)) => {
                once!(LastModified, PropName::Rfc5545(Rfc5545PropName::LastModified), value)
            },
            ParserProp::Known(KnownProp::TzUrl(value)) => {
                once!(TzUrl, PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl), value)
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
        macro_rules! once {
            ($name:ident, $long_name:expr, $value:expr, $params:expr) => {
                try_insert_once!(
                    state,
                    StandardOrDaylight,
                    Key::Known(OffsetPropName::$name),
                    $long_name,
                    Entry::Known(OffsetProp::$name(Prop {
                        value: $value,
                        params: $params,
                        unknown_params
                    })),
                )
            };
        }

        macro_rules! seq {
            ($name:ident, $value:expr, $params:expr) => {
                insert_seq!(
                    state,
                    OffsetProp,
                    $name,
                    Key::Known(OffsetPropName::$name),
                    Prop {
                        value: $value,
                        params: $params,
                        unknown_params,
                    }
                )
            };
        }

        step_inner! {state, StandardOrDaylight, prop, unknown_params;
            ParserProp::Known(KnownProp::DtStart(value, params)) => {
                once!(DtStart, PropName::Rfc5545(Rfc5545PropName::DateTimeStart), value, params)
            },
            ParserProp::Known(KnownProp::TzOffsetTo(value)) => {
                once!(TzOffsetTo, PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo), value, ())
            },
            ParserProp::Known(KnownProp::TzOffsetFrom(value)) => {
                once!(TzOffsetFrom, PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom), value, ())
            },
            ParserProp::Known(KnownProp::RRule(value)) => {
                once!(RRule, PropName::Rfc5545(Rfc5545PropName::RecurrenceRule), Box::new(value), ())
            },
            ParserProp::Known(KnownProp::Comment(value, params)) => {
                seq!(Comment, value, params)
            },
            ParserProp::Known(KnownProp::RDate(value, params)) => {
                seq!(RDate, value, params)
            },
            ParserProp::Known(KnownProp::TzName(value, params)) => {
                seq!(TzName, value, params)
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
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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

        // check for mandatory fields
        if props.get(Key::Known(OffsetPropName::DtStart)).is_none() {
            return Err(E::from_external_error(
                input,
                CalendarParseError::MissingProp {
                    prop: PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
                    component: kind.into(),
                },
            ));
        }

        if props.get(Key::Known(OffsetPropName::TzOffsetTo)).is_none() {
            return Err(E::from_external_error(
                input,
                CalendarParseError::MissingProp {
                    prop: PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo),
                    component: kind.into(),
                },
            ));
        }

        if props
            .get(Key::Known(OffsetPropName::TzOffsetFrom))
            .is_none()
        {
            return Err(E::from_external_error(
                input,
                CalendarParseError::MissingProp {
                    prop: PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom),
                    component: kind.into(),
                },
            ));
        }

        match terminated(end(rule_kind), crlf).parse_next(input)? == kind {
            true => Ok(TzRule { props, kind }),
            false => fail.parse_next(input),
        }
    }

    terminated(begin(CalCompKind::TimeZone.parser()), crlf).parse_next(input)?;
    let props = StateMachine::new(tz_step).parse_next(input)?;
    let subcomponents = repeat(1.., rule).parse_next(input)?;
    terminated(end(CalCompKind::TimeZone.parser()), crlf).parse_next(input)?;

    Ok(TimeZone {
        props,
        subcomponents,
    })
}

fn alarm<I, E>(input: &mut I) -> Result<Alarm<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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
    fn step<S>(
        (prop, unknown_params): ParsedProp<S>,
        state: &mut FreeAlarmTable<S>,
    ) -> Result<(), CalendarParseError<S>>
    where
        S: Hash + PartialEq + Debug + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        macro_rules! once {
            ($name:ident, $long_name:expr, $value:expr, $params:expr) => {
                try_insert_once!(
                    state,
                    Alarm,
                    Key::Known(FreeAlarmPropName::$name),
                    $long_name,
                    Entry::Known(FreeAlarmProp::$name(Prop {
                        value: $value,
                        params: $params,
                        unknown_params
                    })),
                )
            };
        }

        macro_rules! seq {
            ($name:ident, $value:expr, $params:expr) => {
                insert_seq!(
                    state,
                    FreeAlarmProp,
                    $name,
                    Key::Known(FreeAlarmPropName::$name),
                    Prop {
                        value: $value,
                        params: $params,
                        unknown_params,
                    }
                )
            };
        }

        step_inner! {state, Alarm, prop, unknown_params;
            ParserProp::Known(KnownProp::Action(value)) => {
                once!(Action, PropName::Rfc5545(Rfc5545PropName::Action), value, ())
            },
            ParserProp::Known(KnownProp::Description(value, params)) => {
                once!(Description, PropName::Rfc5545(Rfc5545PropName::Description), value, params)
            },
            ParserProp::Known(KnownProp::TriggerRelative(value, params)) => {
                try_insert_once!(state, Alarm, Key::Known(FreeAlarmPropName::Trigger),
                    PropName::Rfc5545(Rfc5545PropName::Trigger),
                    Entry::Known(FreeAlarmProp::Trigger(TriggerProp::Relative(Prop {
                        value,
                        params,
                        unknown_params
                    }))),
                )
            },
            ParserProp::Known(KnownProp::TriggerAbsolute(value)) => {
                try_insert_once!(state, Alarm, Key::Known(FreeAlarmPropName::Trigger),
                    PropName::Rfc5545(Rfc5545PropName::Trigger),
                    Entry::Known(FreeAlarmProp::Trigger(TriggerProp::Absolute(Prop {
                        value,
                        params: (),
                        unknown_params
                    }))),
                )
            },
            ParserProp::Known(KnownProp::Summary(value, params)) => {
                once!(Summary, PropName::Rfc5545(Rfc5545PropName::Summary), value, params)
            },
            ParserProp::Known(KnownProp::Duration(value)) => {
                once!(Duration, PropName::Rfc5545(Rfc5545PropName::Duration), value, ())
            },
            ParserProp::Known(KnownProp::Repeat(value)) => {
                once!(Repeat, PropName::Rfc5545(Rfc5545PropName::RepeatCount), value, ())
            },
            ParserProp::Known(KnownProp::Attendee(value, params)) => {
                seq!(Attendee, value, Box::new(params))
            },
            ParserProp::Known(KnownProp::Attach(value, params)) => {
                seq!(Attach, value, params)
            },
        }
    }

    fn name<I, E>(input: &mut I) -> Result<(), E>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
        E: ParserError<I>,
    {
        Caseless("VALARM").void().parse_next(input)
    }

    terminated(begin(name), crlf).parse_next(input)?;
    let raw_table = StateMachine::new(step).parse_next(input)?;
    terminated(end(name), crlf).parse_next(input)?;

    match raw_table.try_into_alarm() {
        Ok(alarm) => Ok(alarm),
        Err(err) => Err(E::from_external_error(input, err)),
    }
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
        I: StreamIsPartial + Stream + Compare<Caseless<S>> + Compare<Caseless<&'static str>>,
        I::Token: AsChar + Clone,
        S: std::fmt::Debug + Clone,
        E: ParserError<I>,
    {
        move |input: &mut I| match self {
            CalCompKind::Event => Caseless("VEVENT").void().parse_next(input),
            CalCompKind::Todo => Caseless("VTODO").void().parse_next(input),
            CalCompKind::Journal => Caseless("VJOURNAL").void().parse_next(input),
            CalCompKind::FreeBusy => Caseless("VFREEBUSY").void().parse_next(input),
            CalCompKind::TimeZone => Caseless("VTIMEZONE").void().parse_next(input),
            CalCompKind::Iana(name) | CalCompKind::X(name) => {
                literal(Caseless(name.clone())).void().parse_next(input)
            }
        }
    }
}

/// Parses a [`CalCompKind`].
fn comp_kind<I, E>(input: &mut I) -> Result<CalCompKind<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char> + Compare<Caseless<&'static str>>,
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
        model::{
            primitive::{
                AttachValue, AudioAction, CalAddress, DateTime, DisplayAction, Duration,
                DurationKind, DurationTime, EmailAction, FormatType, Local, Period, Sign, Text,
                TriggerRelation, TzId, Uid, Uri, Utc,
            },
            property::{AttachParams, TriggerParams},
        },
        parser::escaped::AsEscaped,
        time, utc_offset,
    };

    use super::*;

    macro_rules! concat_crlf {
        ($($l:literal),* $(,)?) => {
            concat! (
                $(
                    $l, "\r\n",
                )*
            )
        };
    }

    #[test]
    fn rfc_5545_example_journal() {
        let input = concat_crlf!(
            "BEGIN:VJOURNAL",
            "UID:19970901T130000Z-123405@example.com",
            "DTSTAMP:19970901T130000Z",
            "DTSTART;VALUE=DATE:19970317",
            "SUMMARY:Staff meeting minutes",
            "DESCRIPTION:1. Staff meeting: Participants include Joe\\,",
            "  Lisa\\, and Bob. Aurora project plans were reviewed.",
            "  There is currently no budget reserves for this project.",
            "  Lisa will escalate to management. Next meeting on Tuesday.\\n",
            " 2. Telephone Conference: ABC Corp. sales representative",
            "  called to discuss new printer. Promised to get us a demo by",
            "  Friday.\\n3. Henry Miller (Handsoff Insurance): Car was",
            "  totaled by tree. Is looking into a loaner car. 555-2323",
            "  (tel).",
            "END:VJOURNAL",
        );

        let (tail, journal) = journal::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        assert_eq!(
            journal.uid(),
            &Prop::from_value(Uid("19970901T130000Z-123405@example.com".as_escaped()))
        );

        assert_eq!(
            journal.timestamp(),
            &Prop::from_value(DateTime {
                date: date!(1997;9;1),
                time: time!(13;00;00, Utc)
            })
        );

        assert_eq!(
            journal.start(),
            Some(&Prop::from_value(date!(1997;3;17).into()))
        );

        assert!(journal.summary().is_some());
        assert!(journal.descriptions().is_some());
    }

    #[test]
    fn rfc_5545_example_free_busy_1() {
        let input = concat_crlf!(
            "BEGIN:VFREEBUSY",
            "UID:19970901T082949Z-FA43EF@example.com",
            "ORGANIZER:mailto:jane_doe@example.com",
            "ATTENDEE:mailto:john_public@example.com",
            "DTSTART:19971015T050000Z",
            "DTEND:19971016T050000Z",
            "DTSTAMP:19970901T083000Z",
            "END:VFREEBUSY",
        );

        let (tail, fb) = free_busy::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        assert_eq!(
            fb.uid(),
            &Prop::from_value(Uid("19970901T082949Z-FA43EF@example.com"))
        );

        assert_eq!(
            fb.organizer(),
            Some(&Prop::from_value(CalAddress("mailto:jane_doe@example.com")))
        );

        assert_eq!(
            fb.attendees(),
            Some(
                [Prop::from_value(CalAddress(
                    "mailto:john_public@example.com"
                ))]
                .as_slice()
            )
        );

        assert_eq!(
            fb.start(),
            Some(&Prop::from_value(
                DateTime {
                    date: date!(1997;10;15),
                    time: time!(5;00;00, Utc)
                }
                .into()
            ))
        );

        assert_eq!(
            fb.end(),
            Some(&Prop::from_value(
                DateTime {
                    date: date!(1997;10;16),
                    time: time!(5;00;00, Utc)
                }
                .into()
            ))
        );

        assert_eq!(
            fb.timestamp(),
            &Prop::from_value(DateTime {
                date: date!(1997;9;1),
                time: time!(8;30;00, Utc)
            })
        );

        assert!(fb.contact().is_none());
        assert!(fb.url().is_none());
        assert!(fb.comments().is_none());
        assert!(fb.request_statuses().is_none());
    }

    #[test]
    fn rfc_5545_example_free_busy_2() {
        let input = concat_crlf!(
            "BEGIN:VFREEBUSY",
            "UID:19970901T095957Z-76A912@example.com",
            "ORGANIZER:mailto:jane_doe@example.com",
            "ATTENDEE:mailto:john_public@example.com",
            "DTSTAMP:19970901T100000Z",
            "FREEBUSY:19971015T050000Z/PT8H30M,",
            " 19971015T160000Z/PT5H30M,19971015T223000Z/PT6H30M",
            "URL:http://example.com/pub/busy/jpublic-01.ifb",
            "COMMENT:This iCalendar file contains busy time information for",
            " the next three months.",
            "END:VFREEBUSY",
        );

        let (tail, fb) = free_busy::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        assert_eq!(
            fb.uid(),
            &Prop::from_value(Uid("19970901T095957Z-76A912@example.com".as_escaped()))
        );

        assert_eq!(
            fb.organizer(),
            Some(&Prop::from_value(CalAddress(
                "mailto:jane_doe@example.com".as_escaped()
            )))
        );

        assert_eq!(
            fb.attendees(),
            Some(
                [Prop::from_value(CalAddress(
                    "mailto:john_public@example.com".as_escaped()
                ))]
                .as_slice()
            )
        );

        assert_eq!(
            fb.timestamp(),
            &Prop::from_value(DateTime {
                date: date!(1997;9;1),
                time: time!(10;00;00, Utc)
            })
        );

        assert_eq!(
            fb.free_busy_periods(),
            Some(
                [Prop::from_value(
                    vec![
                        Period::Start {
                            start: DateTime {
                                date: date!(1997;10;15),
                                time: time!(5;00;00, Utc)
                            },
                            duration: Duration {
                                sign: None,
                                kind: DurationKind::Time {
                                    time: DurationTime::HM {
                                        hours: 8,
                                        minutes: 30
                                    }
                                }
                            }
                        },
                        Period::Start {
                            start: DateTime {
                                date: date!(1997;10;15),
                                time: time!(16;00;00, Utc)
                            },
                            duration: Duration {
                                sign: None,
                                kind: DurationKind::Time {
                                    time: DurationTime::HM {
                                        hours: 5,
                                        minutes: 30
                                    }
                                }
                            }
                        },
                        Period::Start {
                            start: DateTime {
                                date: date!(1997;10;15),
                                time: time!(22;30;00, Utc)
                            },
                            duration: Duration {
                                sign: None,
                                kind: DurationKind::Time {
                                    time: DurationTime::HM {
                                        hours: 6,
                                        minutes: 30
                                    }
                                }
                            }
                        },
                    ]
                    .into()
                )]
                .as_slice()
            )
        );

        assert_eq!(
            fb.url(),
            Some(&Prop::from_value(Uri(
                "http://example.com/pub/busy/jpublic-01.ifb".as_escaped()
            )))
        );

        assert_eq!(
            fb.comments(),
            Some([Prop::from_value(Text("This iCalendar file contains busy time information for\r\n the next three months.".as_escaped()))].as_slice())
        );

        assert!(fb.start().is_none());
        assert!(fb.end().is_none());
        assert!(fb.contact().is_none());
        assert!(fb.request_statuses().is_none());
    }

    #[test]
    fn timezone_parser() {
        // this input is an abbreviated section of the example given in RFC 5545
        let input = concat_crlf!(
            "BEGIN:VTIMEZONE",
            "TZID:America/New_York",
            "LAST-MODIFIED:20050809T020000Z",
            "BEGIN:DAYLIGHT",
            "DTSTART:19670430T020000",
            "RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z",
            "TZOFFSETFROM:-0500",
            "TZOFFSETTO:-0400",
            "TZNAME:EDT",
            "END:DAYLIGHT",
            "BEGIN:STANDARD",
            "DTSTART:19671029T020000",
            "RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z",
            "TZOFFSETFROM:-0400",
            "TZOFFSETTO:-0500",
            "TZNAME:EST",
            "END:STANDARD",
            "END:VTIMEZONE",
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

        assert_eq!(daylight.offset_to(), &Prop::from_value(utc_offset!(-4;00)));
        assert_eq!(
            daylight.offset_from(),
            &Prop::from_value(utc_offset!(-5;00))
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

        assert_eq!(standard.offset_to(), &Prop::from_value(utc_offset!(-5;00)));
        assert_eq!(
            standard.offset_from(),
            &Prop::from_value(utc_offset!(-4;00))
        );

        assert_eq!(
            standard.names(),
            Some([Prop::from_value(Text("EST"))].as_slice())
        );
    }

    #[test]
    fn rfc_5545_example_audio_alarm() {
        let input = concat_crlf!(
            "BEGIN:VALARM",
            "TRIGGER;VALUE=DATE-TIME:19970317T133000Z",
            "REPEAT:4",
            "DURATION:PT15M",
            "ACTION:AUDIO",
            "ATTACH;FMTTYPE=audio/basic:ftp://example.com/pub/",
            " sounds/bell-01.aud",
            "END:VALARM",
        );

        let (tail, alarm) = alarm::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let Alarm::Audio(alarm) = alarm else { panic!() };
        assert_eq!(alarm.action(), &Prop::from_value(AudioAction));

        assert_eq!(
            alarm.trigger(),
            &TriggerProp::Absolute(Prop::from_value(DateTime {
                date: date!(1997;3;17),
                time: time!(13;30;00, Utc)
            }))
        );

        assert_eq!(
            alarm.attachment(),
            Some(&Prop {
                value: AttachValue::Uri(Uri(
                    "ftp://example.com/pub/\r\n sounds/bell-01.aud".as_escaped()
                )),
                params: AttachParams {
                    format_type: Some(FormatType {
                        source: "audio/basic".as_escaped(),
                        separator_index: 5
                    }),
                },
                unknown_params: Default::default(),
            })
        );

        let (duration, repeat) = alarm.duration_and_repeat().unwrap();
        assert_eq!(repeat, &Prop::from_value(4));
        assert_eq!(
            duration,
            &Prop::from_value(Duration {
                sign: None,
                kind: DurationKind::Time {
                    time: DurationTime::M { minutes: 15 },
                },
            })
        );
    }

    #[test]
    fn rfc_5545_example_display_alarm() {
        let input = concat_crlf!(
            "BEGIN:VALARM",
            "TRIGGER:-PT30M",
            "REPEAT:2",
            "DURATION:PT15M",
            "ACTION:DISPLAY",
            "DESCRIPTION:Breakfast meeting with executive\\n",
            " team at 8:30 AM EST.",
            "END:VALARM",
        );

        let (tail, alarm) = alarm::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let Alarm::Display(alarm) = alarm else {
            panic!()
        };

        assert_eq!(alarm.action(), &Prop::from_value(DisplayAction));

        assert_eq!(
            alarm.trigger(),
            &TriggerProp::Relative(Prop::from_value(Duration {
                sign: Some(Sign::Negative),
                kind: DurationKind::Time {
                    time: DurationTime::M { minutes: 30 },
                },
            }))
        );

        assert_eq!(
            alarm.description(),
            &Prop::from_value(Text(
                "Breakfast meeting with executive\\n\r\n team at 8:30 AM EST.".as_escaped()
            ))
        );

        let (duration, repeat) = alarm.duration_and_repeat().unwrap();
        assert_eq!(repeat, &Prop::from_value(2));
        assert_eq!(
            duration,
            &Prop::from_value(Duration {
                sign: None,
                kind: DurationKind::Time {
                    time: DurationTime::M { minutes: 15 },
                },
            })
        );
    }

    #[test]
    fn rfc_5545_example_email_alarm() {
        let input = concat_crlf!(
            "BEGIN:VALARM",
            "TRIGGER;RELATED=END:-P2D",
            "ACTION:EMAIL",
            "ATTENDEE:mailto:john_doe@example.com",
            "SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***",
            "DESCRIPTION:A draft agenda needs to be sent out to the attendees",
            " to the weekly managers meeting (MGR-LIST). Attached is a",
            " pointer the document template for the agenda file.",
            "ATTACH;FMTTYPE=application/msword:http://example.com/",
            " templates/agenda.doc",
            "END:VALARM",
        );

        let (tail, alarm) = alarm::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let Alarm::Email(alarm) = alarm else { panic!() };

        assert_eq!(alarm.action(), &Prop::from_value(EmailAction));

        assert_eq!(
            alarm.trigger(),
            &TriggerProp::Relative(Prop {
                value: Duration {
                    sign: Some(Sign::Negative),
                    kind: DurationKind::Date {
                        days: 2,
                        time: None,
                    },
                },
                params: TriggerParams {
                    trigger_relation: Some(TriggerRelation::End)
                },
                unknown_params: Default::default(),
            })
        );

        assert_eq!(
            alarm.attendees(),
            Some(
                [Prop::from_value(CalAddress(
                    "mailto:john_doe@example.com".as_escaped()
                ))]
                .as_slice()
            ),
        );

        assert_eq!(
            alarm.summary(),
            &Prop::from_value(Text(
                "*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***".as_escaped()
            ))
        );

        assert_eq!(
            alarm.description(),
            &Prop::from_value(Text("A draft agenda needs to be sent out to the attendees\r\n to the weekly managers meeting (MGR-LIST). Attached is a\r\n pointer the document template for the agenda file.".as_escaped())),
        );

        assert_eq!(
            alarm.attachments(),
            Some(
                [Prop {
                    value: AttachValue::Uri(Uri(
                        "http://example.com/\r\n templates/agenda.doc".as_escaped()
                    )),
                    params: AttachParams {
                        format_type: Some(FormatType {
                            source: "application/msword".as_escaped(),
                            separator_index: 11
                        })
                    },
                    unknown_params: Default::default(),
                }]
                .as_slice()
            ),
        );

        assert!(alarm.duration_and_repeat().is_none());
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
            begin::<_, _, ()>(Caseless("vtodo").take()).parse_peek("BEGIN:VTODO"),
            Ok(("", "VTODO"))
        );

        assert_eq!(
            begin::<_, _, ()>(Caseless("VEVENT").take()).parse_peek("begin:vevent"),
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
            end::<_, _, ()>(Caseless("VFREEBUSY").take()).parse_peek("end:vfreebusy"),
            Ok(("", "vfreebusy"))
        );
    }
}
