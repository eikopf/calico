//! Parsers for properties.

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, fail, preceded, repeat, terminated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::none_of,
};

use crate::{
    model::{
        css::Css3Color,
        primitive::{
            AlarmAction, AttachValue, ClassValue, DateTime, DateTimeOrDate,
            Duration, Geo, ImageData, Method, Period, RDate, Status,
            Transparency, Uid, Uri, Utc, UtcOffset, Value, ValueType,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams,
            ImageParams, LangParams, OrganizerParams, RDateParams,
            RecurrenceIdParams, RelTypeParams, TextParams, TriggerParams,
        },
    },
    parser::{
        parameter::{KnownParam, Param, parameter},
        primitive::{duration, float, iana_token, integer, period, x_name},
    },
};

use super::{
    parameter::UnknownParam,
    primitive::{
        InvalidDateError, InvalidDurationTimeError, InvalidIntegerError,
        InvalidRawTimeError, binary, bool_caseless, date, datetime, time, uri,
    },
};

/// The type of "unbounded" unsigned integers.
type UInt = usize;

// NOTE: the IANA iCalendar property registry lists several registered properties
// from RFC 6321 §4.2, RFC 7808 §7, RFC 7953 §3.2, RFC 9073 §6, and RFC 9253 § 8
// that have not been included here (they would fall under the Other catch-all).
// perhaps they should be included as static variants at some later point?
// registry: (https://www.iana.org/assignments/icalendar/icalendar.xhtml#properties)

#[derive(Debug, Clone)]
pub enum Prop<S = Box<str>> {
    Known(KnownProp<S>),
    Unknown(UnknownProp<S>),
}

impl<S> Prop<S> {
    pub fn try_into_known(self) -> Result<KnownProp<S>, Self> {
        if let Self::Known(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_unknown(self) -> Result<UnknownProp<S>, Self> {
        if let Self::Unknown(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone)]
pub enum KnownProp<S> {
    // CALENDAR PROPERTIES
    CalScale,
    Method(Method<S>),
    ProdId(S),
    Version,
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(AttachValue<S>, AttachParams),
    Categories(Box<[S]>, LangParams<S>),
    Class(ClassValue<S>),
    Comment(S, TextParams<S>),
    Description(S, TextParams<S>),
    Geo(Geo),
    Location(S, TextParams<S>),
    PercentComplete(u8), // 0..=100
    Priority(u8),        // 0..=9
    Resources(Box<[S]>, TextParams<S>),
    Status(Status),
    Summary(S, TextParams<S>),
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted(DateTime),
    DtEnd(DateTimeOrDate, DtParams<S>),
    DtDue(DateTimeOrDate, DtParams<S>),
    DtStart(DateTimeOrDate, DtParams<S>),
    Duration(Duration),
    FreeBusy(Box<[Period]>, FBTypeParams<S>),
    Transparency(Transparency),
    // TIME ZONE COMPONENT PROPERTIES
    TzId(S),
    TzName(S, LangParams<S>),
    TzOffsetFrom(UtcOffset),
    TzOffsetTo(UtcOffset),
    TzUrl(Uri<S>),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(Uri<S>, AttendeeParams<S>),
    Contact(S, TextParams<S>),
    Organizer(Uri<S>, OrganizerParams<S>),
    RecurrenceId(DateTimeOrDate, RecurrenceIdParams<S>),
    RelatedTo(S, RelTypeParams<S>),
    Url(Uri<S>),
    Uid(Uid<S>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(DateTimeOrDate, DtParams<S>),
    RDate(RDate, RDateParams<S>),
    // TODO: finish recurrence rule model
    RRule(()),
    // ALARM COMPONENT PROPERTIES
    Action(AlarmAction<S>),
    Repeat(UInt),
    TriggerRelative(Duration, TriggerParams),
    TriggerAbsolute(DateTime<Utc>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(DateTime),
    DtStamp(DateTime),
    LastModified(DateTime),
    Sequence(UInt),
    // MISCELLANEOUS COMPONENT PROPERTIES
    // TODO: the value of this property has a more precise grammar (page 143)
    RequestStatus(S, LangParams<S>),
    // RFC 7986 PROPERTIES
    Name(S, TextParams<S>),
    RefreshInterval(Duration),
    Source(Uri<S>),
    Color(Css3Color),
    Image(ImageData<S>, ImageParams<S>),
    Conference(Uri<S>, ConfParams<S>),
}

#[derive(Debug, Clone)]
pub enum UnknownProp<S> {
    Iana {
        name: S,
        value: Value<S>,
    },
    X {
        name: S,
        value: Value<S>,
        params: Box<[KnownParam<S>]>,
    },
}

fn parse_value<I, E>(
    value_type: ValueType<I::Slice>,
    input: &mut I,
) -> Result<Value<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr + Clone,
    E: ParserError<I>
        + FromExternalError<I, InvalidDateError>
        + FromExternalError<I, InvalidRawTimeError>
        + FromExternalError<I, InvalidDurationTimeError>
        + FromExternalError<I, InvalidIntegerError>,
{
    /// Parses the raw text value of a property.
    fn text<I, E>(input: &mut I) -> Result<I::Slice, E>
    where
        I: StreamIsPartial + Stream,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        repeat::<_, _, (), _, _>(0.., none_of((..' ', '\u{007F}')))
            .take()
            .parse_next(input)
    }

    match value_type {
        ValueType::Binary => binary.map(Value::Binary).parse_next(input),
        ValueType::Boolean => {
            bool_caseless.map(Value::Boolean).parse_next(input)
        }
        ValueType::CalAddress => uri.map(Value::CalAddress).parse_next(input),
        ValueType::Date => date.map(Value::Date).parse_next(input),
        ValueType::DateTime => datetime.map(Value::DateTime).parse_next(input),
        ValueType::Duration => duration.map(Value::Duration).parse_next(input),
        ValueType::Float => float.map(Value::Float).parse_next(input),
        ValueType::Integer => integer.map(Value::Integer).parse_next(input),
        ValueType::Period => period.map(Value::Period).parse_next(input),
        ValueType::Recur => todo!(),
        ValueType::Text => text.map(Value::Text).parse_next(input),
        ValueType::Time => time.map(Value::Time).parse_next(input),
        ValueType::Uri => uri.map(Value::Uri).parse_next(input),
        ValueType::UtcOffset => todo!(),
        ValueType::Iana(name) => text
            .map(|value| Value::Iana {
                name: name.clone(),
                value,
            })
            .parse_next(input),
        ValueType::X(name) => text
            .map(|value| Value::X {
                name: name.clone(),
                value,
            })
            .parse_next(input),
    }
}

/// Finds and removes a value from a vector by matching it against a pattern.
///
/// See rust-lang/rust#118682 for variable attribute bug.
macro_rules! find_and_remove {
    (let $name:ident = $p:pat => $binding:tt in $v:expr) => {
        let $name = {
            let index = ($v).iter().position(|x| matches!(x, $p));

            match index {
                Some(index) => {
                    let item = ($v).swap_remove(index);
                    #[allow(unused_variables)]
                    match item {
                        $p => Some($binding),
                        _ => unreachable!(),
                    }
                }
                None => None,
            }
        };
    };
}

type ParsedProp<S> = (Prop<S>, Box<[UnknownParam<S>]>);

/// Parses a [`Prop`].
pub fn property<I, E>(input: &mut I) -> Result<ParsedProp<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr + Clone + SliceLen,
    E: ParserError<I>
        + FromExternalError<I, InvalidDateError>
        + FromExternalError<I, InvalidRawTimeError>
        + FromExternalError<I, InvalidDurationTimeError>
        + FromExternalError<I, InvalidIntegerError>,
{
    let name = property_name.parse_next(input)?;
    let mut params: Vec<_> =
        terminated(repeat(0.., preceded(';', parameter)), ':')
            .parse_next(input)?;

    Ok(match name {
        PropName::Rfc5545(name) => todo!(),
        PropName::Rfc7986(name) => todo!(),
        PropName::Iana(name) => {
            find_and_remove! {
                let value_type = Param::Known(KnownParam::Value(_x)) => _x in params
            };

            let value =
                parse_value(value_type.unwrap_or(ValueType::Text), input)?;

            // TODO: check that the VALUE parameter does not occur more than
            // once in iana_params

            // assume all remaining parameters must be unknown IANA or X-names
            let unknown_params = params
                .into_iter()
                .map(Param::try_into_unknown)
                .collect::<Result<Box<_>, _>>()
                .map_err(|_| todo!())?;

            (
                Prop::Unknown(UnknownProp::Iana { name, value }),
                unknown_params,
            )
        }
        PropName::X(name) => {
            find_and_remove! {
                let value_type = Param::Known(KnownParam::Value(_x)) => _x in params
            };

            let already_found = value_type.is_some();

            let value =
                parse_value(value_type.unwrap_or(ValueType::Text), input)?;

            let (known_params, unknown_params) = {
                let mut known_params = Vec::with_capacity(params.len() / 2);
                let mut unknown_params = Vec::with_capacity(params.len() / 2);

                for param in params.drain(..) {
                    match param {
                        // if we run into another VALUE parameter having already found one, that's
                        // an error
                        Param::Known(KnownParam::Value(_)) if already_found => {
                            todo!()
                        }
                        Param::Known(param) => known_params.push(param),
                        Param::Unknown(param) => unknown_params.push(param),
                    }
                }

                (
                    known_params.into_boxed_slice(),
                    unknown_params.into_boxed_slice(),
                )
            };

            (
                Prop::Unknown(UnknownProp::X {
                    name,
                    value,
                    params: known_params,
                }),
                unknown_params,
            )
        }
    })
}

/// A property name, which may be statically known from RFC 5545 or RFC 7986, or
/// otherwise may be some arbitrary [`iana_token`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropName<S = Box<str>> {
    Rfc5545(Rfc5545PropName),
    Rfc7986(Rfc7986PropName),
    Iana(S),
    X(S),
}

/// Parses a [`PropName`].
pub fn property_name<I, E>(input: &mut I) -> Result<PropName<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
    PropName<I::Slice>: Clone,
{
    fn other<I, E>(input: &mut I) -> Result<PropName<I::Slice>, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        alt((x_name.map(PropName::X), iana_token.map(PropName::Iana)))
            .parse_next(input)
    }

    macro_rules! keywords {
        ($name:ident; $($kw:literal => $val:expr),*) => {
            fn $name<I, E>(input: &mut I) -> Result<PropName<I::Slice>, E>
            where
                I: StreamIsPartial
                    + Stream
                    + Compare<Caseless<&'static str>>
                    + Compare<char>,
                I::Token: AsChar + Clone,
                E: ParserError<I>,
                PropName<I::Slice>: Clone,
            {
                alt(($(Caseless($kw).value($val),)* other)).parse_next(input)
            }
        };
    }

    keywords! {a_names;
        "ATTENDEE" => PropName::Rfc5545(Rfc5545PropName::Attendee),
        "ATTACH"   => PropName::Rfc5545(Rfc5545PropName::Attachment),
        "ACTION"   => PropName::Rfc5545(Rfc5545PropName::Action)
    }

    keywords! {c_names;
        "CONFERENCE" => PropName::Rfc7986(Rfc7986PropName::Conference),
        "CATEGORIES" => PropName::Rfc5545(Rfc5545PropName::Categories),
        "COMPLETED"  => PropName::Rfc5545(Rfc5545PropName::DateTimeCompleted),
        "CALSCALE"   => PropName::Rfc5545(Rfc5545PropName::CalendarScale),
        "CONTACT"    => PropName::Rfc5545(Rfc5545PropName::Contact),
        "CREATED"    => PropName::Rfc5545(Rfc5545PropName::DateTimeCreated),
        "COMMENT"    => PropName::Rfc5545(Rfc5545PropName::Comment),
        "COLOR"      => PropName::Rfc7986(Rfc7986PropName::Color),
        "CLASS"      => PropName::Rfc5545(Rfc5545PropName::Classification)
    }

    keywords! {d_names;
        "DESCRIPTION" => PropName::Rfc5545(Rfc5545PropName::Description),
        "DURATION"    => PropName::Rfc5545(Rfc5545PropName::Duration),
        "DTSTART"     => PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        "DTSTAMP"     => PropName::Rfc5545(Rfc5545PropName::DateTimeStamp),
        "DTEND"       => PropName::Rfc5545(Rfc5545PropName::DateTimeEnd),
        "DUE"         => PropName::Rfc5545(Rfc5545PropName::DateTimeDue)
    }

    keywords! {e_names;
        "EXDATE" => PropName::Rfc5545(Rfc5545PropName::ExceptionDateTimes)
    }

    keywords! {f_names;
        "FREEBUSY" => PropName::Rfc5545(Rfc5545PropName::FreeBusyTime)
    }

    keywords! {g_names;
        "GEO" => PropName::Rfc5545(Rfc5545PropName::GeographicPosition)
    }

    keywords! {i_names;
        "IMAGE" => PropName::Rfc7986(Rfc7986PropName::Image)
    }

    keywords! {l_names;
        "LAST-MODIFIED" => PropName::Rfc5545(Rfc5545PropName::LastModified),
        "LOCATION"      => PropName::Rfc5545(Rfc5545PropName::Location)
    }

    keywords! {m_names;
        "METHOD" => PropName::Rfc5545(Rfc5545PropName::Method)
    }

    keywords! {n_names;
        "NAME" => PropName::Rfc7986(Rfc7986PropName::Name)
    }

    keywords! {o_names;
        "ORGANIZER" => PropName::Rfc5545(Rfc5545PropName::Organizer)
    }

    keywords! {p_names;
        "PERCENT-COMPLETE" => PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        "PRIORITY"         => PropName::Rfc5545(Rfc5545PropName::Priority),
        "PRODID"           => PropName::Rfc5545(Rfc5545PropName::ProductIdentifier)
    }

    keywords! {r_names;
        "REFRESH-INTERVAL" => PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        "REQUEST-STATUS"   => PropName::Rfc5545(Rfc5545PropName::RequestStatus),
        "RECURRENCE-ID"    => PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
        "RELATED-TO"       => PropName::Rfc5545(Rfc5545PropName::RelatedTo),
        "RESOURCES"        => PropName::Rfc5545(Rfc5545PropName::Resources),
        "REPEAT"           => PropName::Rfc5545(Rfc5545PropName::RepeatCount),
        "RDATE"            => PropName::Rfc5545(Rfc5545PropName::RecurrenceDateTimes),
        "RRULE"            => PropName::Rfc5545(Rfc5545PropName::RecurrenceRule)
    }

    keywords! {s_names;
        "SEQUENCE" => PropName::Rfc5545(Rfc5545PropName::SequenceNumber),
        "SUMMARY"  => PropName::Rfc5545(Rfc5545PropName::Summary),
        "STATUS"   => PropName::Rfc5545(Rfc5545PropName::Status),
        "SOURCE"   => PropName::Rfc7986(Rfc7986PropName::Source)
    }

    keywords! {t_names;
        "TZOFFSETFROM" => PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom),
        "TZOFFSETTO"   => PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo),
        "TRIGGER"      => PropName::Rfc5545(Rfc5545PropName::Trigger),
        "TZNAME"       => PropName::Rfc5545(Rfc5545PropName::TimeZoneName),
        "TRANSP"       => PropName::Rfc5545(Rfc5545PropName::TimeTransparency),
        "TZURL"        => PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl),
        "TZID"         => PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier)
    }

    keywords! {u_names;
        "URL" => PropName::Rfc5545(Rfc5545PropName::UniformResourceLocator),
        "UID" => PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier)
    }

    keywords! {v_names;
        "VERSION" => PropName::Rfc5545(Rfc5545PropName::Version)
    }

    match input.peek_token().map(AsChar::as_char) {
        Some('A' | 'a') => a_names.parse_next(input),
        Some('C' | 'c') => c_names.parse_next(input),
        Some('D' | 'd') => d_names.parse_next(input),
        Some('E' | 'e') => e_names.parse_next(input),
        Some('F' | 'f') => f_names.parse_next(input),
        Some('G' | 'g') => g_names.parse_next(input),
        Some('I' | 'i') => i_names.parse_next(input),
        Some('L' | 'l') => l_names.parse_next(input),
        Some('M' | 'm') => m_names.parse_next(input),
        Some('N' | 'n') => n_names.parse_next(input),
        Some('O' | 'o') => o_names.parse_next(input),
        Some('P' | 'p') => p_names.parse_next(input),
        Some('R' | 'r') => r_names.parse_next(input),
        Some('S' | 's') => s_names.parse_next(input),
        Some('T' | 't') => t_names.parse_next(input),
        Some('U' | 'u') => u_names.parse_next(input),
        Some('V' | 'v') => v_names.parse_next(input),
        Some(_) => other.parse_next(input),
        None => fail.parse_next(input),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc5545PropName {
    // CALENDAR PROPERTIES (RFC 5545 §3.7)
    /// RFC 5545 §3.7.1 (CALSCALE)
    CalendarScale,
    /// RFC 5545 §3.7.2 (METHOD)
    Method,
    /// RFC 5545 §3.7.3 (PRODID)
    ProductIdentifier,
    /// RFC 5545 §3.7.4 (VERSION)
    Version,

    // DESCRIPTIVE PROPERTIES (RFC 5545 §3.8.1)
    /// RFC 5545 §3.8.1.1 (ATTACH)
    Attachment,
    /// RFC 5545 §3.8.1.2 (CATEGORIES)
    Categories,
    /// RFC 5545 §3.8.1.3 (CLASS)
    Classification,
    /// RFC 5545 §3.8.1.4 (COMMENT)
    Comment,
    /// RFC 5545 §3.8.1.5 (DESCRIPTION)
    Description,
    /// RFC 5545 §3.8.1.6 (GEO)
    GeographicPosition,
    /// RFC 5545 §3.8.1.7 (LOCATION)
    Location,
    /// RFC 5545 §3.8.1.8 (PERCENT-COMPLETE)
    PercentComplete,
    /// RFC 5545 §3.8.1.9 (PRIORITY)
    Priority,
    /// RFC 5545 §3.8.1.10 (RESOURCES)
    Resources,
    /// RFC 5545 §3.8.1.11 (STATUS)
    Status,
    /// RFC 5545 §3.8.1.12 (SUMMARY)
    Summary,

    // DATE AND TIME PROPERTIES (RFC 5545 §3.8.2)
    /// RFC 5545 §3.8.2.1 (COMPLETED)
    DateTimeCompleted,
    /// RFC 5545 §3.8.2.2 (DTEND)
    DateTimeEnd,
    /// RFC 5545 §3.8.2.3 (DUE)
    DateTimeDue,
    /// RFC 5545 §3.8.2.4 (DTSTART)
    DateTimeStart,
    /// RFC 5545 §3.8.2.5 (DURATION)
    Duration,
    /// RFC 5545 §3.8.2.6 (FREEBUSY)
    FreeBusyTime,
    /// RFC 5545 §3.8.2.7 (TRANSP)
    TimeTransparency,

    // TIME ZONE PROPERTIES (RFC 5545 §3.8.3)
    /// RFC 5545 §3.8.3.1 (TZID)
    TimeZoneIdentifier,
    /// RFC 5545 §3.8.3.2 (TZNAME)
    TimeZoneName,
    /// RFC 5545 §3.8.3.3 (TZOFFSETFROM)
    TimeZoneOffsetFrom,
    /// RFC 5545 §3.8.3.4 (TZOFFSETTO)
    TimeZoneOffsetTo,
    /// RFC 5545 §3.8.3.5 (TZURL)
    TimeZoneUrl,

    // RELATIONSHIP PROPERTIES (RFC 5545 §3.8.4)
    /// RFC 5545 §3.8.4.1 (ATTENDEE)
    Attendee,
    /// RFC 5545 §3.8.4.2 (CONTACT)
    Contact,
    /// RFC 5545 §3.8.4.3 (ORGANIZER)
    Organizer,
    /// RFC 5545 §3.8.4.4 (RECURRENCE-ID)
    RecurrenceId,
    /// RFC 5545 §3.8.4.5 (RELATED-TO)
    RelatedTo,
    /// RFC 5545 §3.8.4.6 (URL)
    UniformResourceLocator,
    /// RFC 5545 §3.8.4.7 (UID)
    UniqueIdentifier,

    // RECURRENCE PROPERTIES (RFC 5545 §3.8.5)
    /// RFC 5545 §3.8.5.1 (EXDATE)
    ExceptionDateTimes,
    /// RFC 5545 §3.8.5.2 (RDATE)
    RecurrenceDateTimes,
    /// RFC 5545 §3.8.5.3 (RRULE)
    RecurrenceRule,

    // ALARM PROPERTIES (RFC 5545 §3.8.6)
    /// RFC 5545 §3.8.6.1 (ACTION)
    Action,
    /// RFC 5545 §3.8.6.2 (REPEAT)
    RepeatCount,
    /// RFC 5545 §3.8.6.3 (TRIGGER)
    Trigger,

    // CHANGE MANAGEMENT PROPERTIES (RFC 5545 §3.8.7)
    /// RFC 5545 §3.8.7.1 (CREATED)
    DateTimeCreated,
    /// RFC 5545 §3.8.7.2 (DTSTAMP)
    DateTimeStamp,
    /// RFC 5545 §3.8.7.3 (LAST-MODIFIED)
    LastModified,
    /// RFC 5545 §3.8.7.4 (SEQUENCE)
    SequenceNumber,

    // MISCELLANEOUS PROPERTIES (RFC 5545 §3.8.8)
    /// RFC 5545 §3.8.8.3 (REQUEST-STATUS)
    RequestStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc7986PropName {
    /// RFC 7986 §5.1 (NAME)
    Name,
    /// RFC 7986 §5.7 (REFRESH-INTERVAL)
    RefreshInterval,
    /// RFC 7986 §5.8 (SOURCE)
    Source,
    /// RFC 7986 §5.9 (COLOR)
    Color,
    /// RFC 7986 §5.10 (IMAGE)
    Image,
    /// RFC 7986 §5.11 (CONFERENCE)
    Conference,
}

#[cfg(test)]
mod tests {
    use super::*;
    use winnow::Parser;

    // PROPERTY PARSING TESTS

    #[test]
    fn rfc_5545_example_iana_property() {
        let mut input = "NON-SMOKING;VALUE=BOOLEAN:TRUE";
        let prop = property::<_, ()>(&mut input);
        assert!(matches!(
            prop,
            Ok((
                Prop::Unknown(UnknownProp::Iana {
                    name: "NON-SMOKING",
                    value: Value::Boolean(true),
                }),
                extras,
            )) if extras.is_empty(),
        ));

        let mut input = "NON-SMOKING:TRUE";
        let prop = property::<_, ()>(&mut input);
        assert!(matches!(
            prop,
            Ok((
                Prop::Unknown(UnknownProp::Iana {
                    name: "NON-SMOKING",
                    value: Value::Text("TRUE"),
                }),
                extras,
            )) if extras.is_empty(),
        ));
    }

    #[test]
    fn rfc_5545_example_x_property() {
        let input = "X-ABC-MMSUBJ;VALUE=URI;FMTTYPE=audio/basic:http://www.example.org/mysubj.au";
        let prop = property::<_, ()>.parse_peek(input);
        let expected_uri = Uri("http://www.example.org/mysubj.au");

        assert!(matches!(
            prop, Ok(("", (Prop::Unknown(UnknownProp::X {
                name: "X-ABC-MMSUBJ",
                value: Value::Uri(uri),
                params
            }), extras))) if params.len() == 1 && extras.is_empty() && uri == expected_uri
        ))
    }

    #[test]
    fn integer_value_parsing() {
        for mut i in ["0", "-2147483648", "2147483647"] {
            assert!(
                parse_value::<_, ()>(ValueType::<&str>::Integer, &mut i)
                    .is_ok()
            );
        }

        for mut i in ["-2147483649", "2147483648"] {
            assert!(
                parse_value::<_, ()>(ValueType::<&str>::Integer, &mut i)
                    .is_err()
            );
        }
    }

    // PROPERTY NAME TESTS

    /// Asserts that the inputs are equal under [`property_name`].
    fn assert_prop_name_eq<'i>(input: &'i str, expected: PropName<&'i str>) {
        let mut input_ref = input;
        let result = property_name::<_, ()>.parse_next(&mut input_ref);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected);
        assert!(input_ref.is_empty(),);
    }

    // Helper function to test parsing failures
    fn assert_prop_name_parse_failure(input: &str) {
        let mut input_ref = input;
        let result = property_name::<_, ()>.parse_next(&mut input_ref);
        assert!(result.is_err());
    }

    #[test]
    fn rfc5545_calendar_property_names() {
        assert_prop_name_eq(
            "CALSCALE",
            PropName::Rfc5545(Rfc5545PropName::CalendarScale),
        );
        assert_prop_name_eq(
            "METHOD",
            PropName::Rfc5545(Rfc5545PropName::Method),
        );
        assert_prop_name_eq(
            "PRODID",
            PropName::Rfc5545(Rfc5545PropName::ProductIdentifier),
        );
        assert_prop_name_eq(
            "VERSION",
            PropName::Rfc5545(Rfc5545PropName::Version),
        );
    }

    #[test]
    fn rfc5545_descriptive_property_names() {
        assert_prop_name_eq(
            "ATTACH",
            PropName::Rfc5545(Rfc5545PropName::Attachment),
        );
        assert_prop_name_eq(
            "CATEGORIES",
            PropName::Rfc5545(Rfc5545PropName::Categories),
        );
        assert_prop_name_eq(
            "CLASS",
            PropName::Rfc5545(Rfc5545PropName::Classification),
        );
        assert_prop_name_eq(
            "COMMENT",
            PropName::Rfc5545(Rfc5545PropName::Comment),
        );
        assert_prop_name_eq(
            "DESCRIPTION",
            PropName::Rfc5545(Rfc5545PropName::Description),
        );
        assert_prop_name_eq(
            "GEO",
            PropName::Rfc5545(Rfc5545PropName::GeographicPosition),
        );
        assert_prop_name_eq(
            "LOCATION",
            PropName::Rfc5545(Rfc5545PropName::Location),
        );
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
        assert_prop_name_eq(
            "PRIORITY",
            PropName::Rfc5545(Rfc5545PropName::Priority),
        );
        assert_prop_name_eq(
            "RESOURCES",
            PropName::Rfc5545(Rfc5545PropName::Resources),
        );
        assert_prop_name_eq(
            "STATUS",
            PropName::Rfc5545(Rfc5545PropName::Status),
        );
        assert_prop_name_eq(
            "SUMMARY",
            PropName::Rfc5545(Rfc5545PropName::Summary),
        );
    }

    #[test]
    fn rfc5545_datetime_property_names() {
        assert_prop_name_eq(
            "COMPLETED",
            PropName::Rfc5545(Rfc5545PropName::DateTimeCompleted),
        );
        assert_prop_name_eq(
            "DTEND",
            PropName::Rfc5545(Rfc5545PropName::DateTimeEnd),
        );
        assert_prop_name_eq(
            "DUE",
            PropName::Rfc5545(Rfc5545PropName::DateTimeDue),
        );
        assert_prop_name_eq(
            "DTSTART",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "DURATION",
            PropName::Rfc5545(Rfc5545PropName::Duration),
        );
        assert_prop_name_eq(
            "FREEBUSY",
            PropName::Rfc5545(Rfc5545PropName::FreeBusyTime),
        );
        assert_prop_name_eq(
            "TRANSP",
            PropName::Rfc5545(Rfc5545PropName::TimeTransparency),
        );
        assert_prop_name_eq(
            "DTSTAMP",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStamp),
        );
    }

    #[test]
    fn rfc5545_timezone_property_names() {
        assert_prop_name_eq(
            "TZID",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier),
        );
        assert_prop_name_eq(
            "TZNAME",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneName),
        );
        assert_prop_name_eq(
            "TZOFFSETFROM",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom),
        );
        assert_prop_name_eq(
            "TZOFFSETTO",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo),
        );
        assert_prop_name_eq(
            "TZURL",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl),
        );
    }

    #[test]
    fn rfc5545_relationship_property_names() {
        assert_prop_name_eq(
            "ATTENDEE",
            PropName::Rfc5545(Rfc5545PropName::Attendee),
        );
        assert_prop_name_eq(
            "CONTACT",
            PropName::Rfc5545(Rfc5545PropName::Contact),
        );
        assert_prop_name_eq(
            "ORGANIZER",
            PropName::Rfc5545(Rfc5545PropName::Organizer),
        );
        assert_prop_name_eq(
            "RECURRENCE-ID",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
        );
        assert_prop_name_eq(
            "RELATED-TO",
            PropName::Rfc5545(Rfc5545PropName::RelatedTo),
        );
        assert_prop_name_eq(
            "URL",
            PropName::Rfc5545(Rfc5545PropName::UniformResourceLocator),
        );
        assert_prop_name_eq(
            "UID",
            PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier),
        );
    }

    #[test]
    fn rfc5545_recurrence_property_names() {
        assert_prop_name_eq(
            "EXDATE",
            PropName::Rfc5545(Rfc5545PropName::ExceptionDateTimes),
        );
        assert_prop_name_eq(
            "RDATE",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceDateTimes),
        );
        assert_prop_name_eq(
            "RRULE",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceRule),
        );
    }

    #[test]
    fn rfc5545_alarm_property_names() {
        assert_prop_name_eq(
            "ACTION",
            PropName::Rfc5545(Rfc5545PropName::Action),
        );
        assert_prop_name_eq(
            "REPEAT",
            PropName::Rfc5545(Rfc5545PropName::RepeatCount),
        );
        assert_prop_name_eq(
            "TRIGGER",
            PropName::Rfc5545(Rfc5545PropName::Trigger),
        );
    }

    #[test]
    fn rfc5545_change_management_property_names() {
        assert_prop_name_eq(
            "CREATED",
            PropName::Rfc5545(Rfc5545PropName::DateTimeCreated),
        );
        assert_prop_name_eq(
            "LAST-MODIFIED",
            PropName::Rfc5545(Rfc5545PropName::LastModified),
        );
        assert_prop_name_eq(
            "SEQUENCE",
            PropName::Rfc5545(Rfc5545PropName::SequenceNumber),
        );
    }

    #[test]
    fn rfc5545_miscellaneous_property_names() {
        assert_prop_name_eq(
            "REQUEST-STATUS",
            PropName::Rfc5545(Rfc5545PropName::RequestStatus),
        );
    }

    #[test]
    fn rfc7986_property_names() {
        assert_prop_name_eq("NAME", PropName::Rfc7986(Rfc7986PropName::Name));
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        );
        assert_prop_name_eq(
            "SOURCE",
            PropName::Rfc7986(Rfc7986PropName::Source),
        );
        assert_prop_name_eq("COLOR", PropName::Rfc7986(Rfc7986PropName::Color));
        assert_prop_name_eq("IMAGE", PropName::Rfc7986(Rfc7986PropName::Image));
        assert_prop_name_eq(
            "CONFERENCE",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
    }

    #[test]
    fn property_name_case_insensitivity() {
        assert_prop_name_eq(
            "dtstart",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "DTSTART",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "DtStArT",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );
        assert_prop_name_eq(
            "dtSTART",
            PropName::Rfc5545(Rfc5545PropName::DateTimeStart),
        );

        assert_prop_name_eq(
            "conference",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
        assert_prop_name_eq(
            "Conference",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
        assert_prop_name_eq(
            "CONFERENCE",
            PropName::Rfc7986(Rfc7986PropName::Conference),
        );
    }

    #[test]
    fn iana_property_names() {
        assert_prop_name_eq("UNKNOWN-PROP", PropName::Iana("UNKNOWN-PROP"));
        assert_prop_name_eq("CUSTOM", PropName::Iana("CUSTOM"));
        assert_prop_name_eq("NEW-FEATURE", PropName::Iana("NEW-FEATURE"));
    }

    #[test]
    fn x_property_names() {
        assert_prop_name_eq("X-CUSTOM", PropName::X("X-CUSTOM"));
        assert_prop_name_eq("X-VENDOR-PROP", PropName::X("X-VENDOR-PROP"));
        assert_prop_name_eq("X-custom", PropName::X("X-custom"));
    }

    #[test]
    fn property_name_longest_match_precedence() {
        // Ensure longer properties are matched correctly when they share prefixes
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        );
        assert_prop_name_eq(
            "REQUEST-STATUS",
            PropName::Rfc5545(Rfc5545PropName::RequestStatus),
        );
        assert_prop_name_eq(
            "RECURRENCE-ID",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
        );

        // Make sure we don't match shorter prefixes
        assert_prop_name_eq("REFRESH", PropName::Iana("REFRESH"));
        assert_prop_name_eq("REQUEST", PropName::Iana("REQUEST"));
        assert_prop_name_eq("RECURRENCE", PropName::Iana("RECURRENCE"));
    }

    #[test]
    fn property_name_edge_cases() {
        // Empty input
        assert_prop_name_parse_failure("");

        // Single characters
        assert_prop_name_eq("A", PropName::Iana("A"));
        assert_prop_name_eq("Z", PropName::Iana("Z"));

        // Properties with hyphens
        assert_prop_name_eq(
            "LAST-MODIFIED",
            PropName::Rfc5545(Rfc5545PropName::LastModified),
        );
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Rfc7986(Rfc7986PropName::RefreshInterval),
        );

        // Mixed case with hyphens
        assert_prop_name_eq(
            "last-modified",
            PropName::Rfc5545(Rfc5545PropName::LastModified),
        );
        assert_prop_name_eq(
            "Percent-Complete",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
    }
}
