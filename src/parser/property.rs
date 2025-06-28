//! Parsers for properties.

use iri_string::types::{UriStr, UriString};
use winnow::{
    ModalResult, Parser,
    ascii::Caseless,
    combinator::{alt, fail, opt, preceded, repeat, terminated},
    stream::Stream,
    token::none_of,
};

use crate::{
    model::{
        css::Css3Color,
        primitive::{
            AlarmAction, AttachValue, ClassValue, DateTime, DateTimeOrDate,
            Duration, Geo, ImageData, Method, Period, RDate, Status,
            Transparency, Uid, Utc, UtcOffset, Value, ValueType,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams,
            ImageParams, LangParams, OrganizerParams, RDateParams,
            RecurrenceIdParams, RelTypeParams, TextParams, TriggerParams,
        },
    },
    parser::{
        parameter::{KnownParam, Param, parameter},
        primitive::{
            duration, float, iana_token, lz_dec_uint, period, sign, x_name,
        },
    },
};

use super::{
    parameter::UnknownParam,
    primitive::{binary, bool_caseless, date, datetime, time, uri},
};

/// The type of "unbounded" unsigned integers.
type UInt = usize;

// NOTE: the IANA iCalendar property registry lists several registered properties
// from RFC 6321 §4.2, RFC 7808 §7, RFC 7953 §3.2, RFC 9073 §6, and RFC 9253 § 8
// that have not been included here (they would fall under the Other catch-all).
// perhaps they should be included as static variants at some later point?
// registry: (https://www.iana.org/assignments/icalendar/icalendar.xhtml#properties)

#[derive(Debug, Clone)]
pub enum Prop<S = Box<str>, U = UriString> {
    Known(KnownProp<S, U>),
    Unknown(UnknownProp<S, U>),
}

impl<S, U> Prop<S, U> {
    pub fn try_into_known(self) -> Result<KnownProp<S, U>, Self> {
        if let Self::Known(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_unknown(self) -> Result<UnknownProp<S, U>, Self> {
        if let Self::Unknown(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone)]
pub enum KnownProp<S, U> {
    // CALENDAR PROPERTIES
    CalScale,
    Method(Method),
    ProdId(S),
    Version,
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(AttachValue<U>, AttachParams),
    Categories(Box<[S]>, LangParams<S>),
    Class(ClassValue<S>),
    Comment(S, TextParams<S, U>),
    Description(S, TextParams<S, U>),
    Geo(Geo),
    Location(S, TextParams<S, U>),
    PercentComplete(u8), // 0..=100
    Priority(u8),        // 0..=9
    Resources(Box<[S]>, TextParams<S, U>),
    Status(Status),
    Summary(S, TextParams<S, U>),
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
    TzUrl(U),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(U, AttendeeParams<S, U>),
    Contact(S, TextParams<S, U>),
    Organizer(U, OrganizerParams<S, U>),
    RecurrenceId(DateTimeOrDate, RecurrenceIdParams<S>),
    RelatedTo(S, RelTypeParams<S>),
    Url(U),
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
    Name(S, TextParams<S, U>),
    RefreshInterval(Duration),
    Source(U),
    Color(Css3Color),
    Image(ImageData<U>, ImageParams<S, U>),
    Conference(U, ConfParams<S>),
}

#[derive(Debug, Clone)]
pub enum UnknownProp<S, U> {
    Iana {
        name: S,
        value: Value<S, U>,
    },
    X {
        name: S,
        value: Value<S, U>,
        params: Box<[KnownParam<S, U>]>,
    },
}

fn parse_value<'i, S: AsRef<str> + ?Sized>(
    value_type: ValueType<&'i S>,
    input: &mut &'i str,
) -> ModalResult<Value<&'i str, &'i UriStr>> {
    /// Parses the raw text value of a property.
    fn text<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
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
        ValueType::Integer => (opt(sign), lz_dec_uint::<_, u64, _>)
            .try_map(|(s, d)| i64::try_from(d).map(|d| (s, d)))
            .verify_map(|(s, d)| d.checked_mul(s.unwrap_or_default() as i64))
            .try_map(i32::try_from)
            .map(Value::Integer)
            .parse_next(input),
        ValueType::Period => period.map(Value::Period).parse_next(input),
        ValueType::Recur => todo!(),
        ValueType::Text => text.map(Value::Text).parse_next(input),
        ValueType::Time => time.map(Value::Time).parse_next(input),
        ValueType::Uri => uri.map(Value::Uri).parse_next(input),
        ValueType::UtcOffset => todo!(),
        ValueType::Iana(name) => text
            .map(|value| Value::Iana {
                name: name.as_ref(),
                value,
            })
            .parse_next(input),
        ValueType::X(name) => text
            .map(|value| Value::X {
                name: name.as_ref(),
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

type ParsedProp<'a> = (Prop<&'a str, &'a UriStr>, Box<[UnknownParam<&'a str>]>);

/// Parses a [`Prop`].
pub fn property<'i>(input: &mut &'i str) -> ModalResult<ParsedProp<'i>> {
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

            let value =
                parse_value(value_type.unwrap_or(ValueType::Text), input)?;

            let (known_params, unknown_params) = {
                let mut known_params = Vec::with_capacity(params.len() / 2);
                let mut unknown_params = Vec::with_capacity(params.len() / 2);

                for param in params.drain(..) {
                    match param {
                        // if we run into another VALUE parameter having already found one, that's
                        // an error
                        Param::Known(KnownParam::Value(_))
                            if value_type.is_some() =>
                        {
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
pub enum PropName<'a> {
    Rfc5545(Rfc5545PropName),
    Rfc7986(Rfc7986PropName),
    Iana(&'a str),
    X(&'a str),
}

/// Parses a [`PropName`].
///
/// # Examples
///
/// ```
/// use calico::parser::property::{
///     property_name,
///     PropName,
///     Rfc5545PropName
/// };
/// use winnow::Parser;
///
/// assert_eq!(
///     property_name.parse_peek("ACTION").unwrap().1,
///     PropName::Rfc5545(Rfc5545PropName::Action),
/// );
///
/// assert_eq!(
///     property_name.parse_peek("cOnFeReNcE"),
///     property_name.parse_peek("CONFERENCE"),
/// );
/// ```
pub fn property_name<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
    // NOTE: this internal implementation is a little gross, and arguably i
    // could do better with something like aho-corasick. maybe look at how
    // logos does token parser generation?

    fn other<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((x_name.map(PropName::X), iana_token.map(PropName::Iana)))
            .parse_next(input)
    }

    fn a_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("ATTENDEE")
                .value(PropName::Rfc5545(Rfc5545PropName::Attendee)),
            Caseless("ATTACH")
                .value(PropName::Rfc5545(Rfc5545PropName::Attachment)),
            Caseless("ACTION")
                .value(PropName::Rfc5545(Rfc5545PropName::Action)),
            other,
        ))
        .parse_next(input)
    }

    fn c_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("CONFERENCE")
                .value(PropName::Rfc7986(Rfc7986PropName::Conference)),
            Caseless("CATEGORIES")
                .value(PropName::Rfc5545(Rfc5545PropName::Categories)),
            Caseless("COMPLETED")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeCompleted)),
            Caseless("CALSCALE")
                .value(PropName::Rfc5545(Rfc5545PropName::CalendarScale)),
            Caseless("CONTACT")
                .value(PropName::Rfc5545(Rfc5545PropName::Contact)),
            Caseless("CREATED")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeCreated)),
            Caseless("COMMENT")
                .value(PropName::Rfc5545(Rfc5545PropName::Comment)),
            Caseless("COLOR").value(PropName::Rfc7986(Rfc7986PropName::Color)),
            Caseless("CLASS")
                .value(PropName::Rfc5545(Rfc5545PropName::Classification)),
            other,
        ))
        .parse_next(input)
    }

    fn d_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("DESCRIPTION")
                .value(PropName::Rfc5545(Rfc5545PropName::Description)),
            Caseless("DTSTART")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeStart)),
            Caseless("DTSTAMP")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeStamp)),
            Caseless("DTEND")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeEnd)),
            Caseless("DURATION")
                .value(PropName::Rfc5545(Rfc5545PropName::Duration)),
            Caseless("DUE")
                .value(PropName::Rfc5545(Rfc5545PropName::DateTimeDue)),
            other,
        ))
        .parse_next(input)
    }

    fn e_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("EXDATE")
                .value(PropName::Rfc5545(Rfc5545PropName::ExceptionDateTimes)),
            other,
        ))
        .parse_next(input)
    }

    fn f_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("FREEBUSY")
                .value(PropName::Rfc5545(Rfc5545PropName::FreeBusyTime)),
            other,
        ))
        .parse_next(input)
    }

    fn g_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("GEO")
                .value(PropName::Rfc5545(Rfc5545PropName::GeographicPosition)),
            other,
        ))
        .parse_next(input)
    }

    fn i_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("IMAGE").value(PropName::Rfc7986(Rfc7986PropName::Image)),
            other,
        ))
        .parse_next(input)
    }

    fn l_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("LAST-MODIFIED")
                .value(PropName::Rfc5545(Rfc5545PropName::LastModified)),
            Caseless("LOCATION")
                .value(PropName::Rfc5545(Rfc5545PropName::Location)),
            other,
        ))
        .parse_next(input)
    }

    fn m_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("METHOD")
                .value(PropName::Rfc5545(Rfc5545PropName::Method)),
            other,
        ))
        .parse_next(input)
    }

    fn n_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("NAME").value(PropName::Rfc7986(Rfc7986PropName::Name)),
            other,
        ))
        .parse_next(input)
    }

    fn o_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("ORGANIZER")
                .value(PropName::Rfc5545(Rfc5545PropName::Organizer)),
            other,
        ))
        .parse_next(input)
    }

    fn p_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("PERCENT-COMPLETE")
                .value(PropName::Rfc5545(Rfc5545PropName::PercentComplete)),
            Caseless("PRIORITY")
                .value(PropName::Rfc5545(Rfc5545PropName::Priority)),
            Caseless("PRODID")
                .value(PropName::Rfc5545(Rfc5545PropName::ProductIdentifier)),
            other,
        ))
        .parse_next(input)
    }

    fn r_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("REFRESH-INTERVAL")
                .value(PropName::Rfc7986(Rfc7986PropName::RefreshInterval)),
            Caseless("REQUEST-STATUS")
                .value(PropName::Rfc5545(Rfc5545PropName::RequestStatus)),
            Caseless("RECURRENCE-ID")
                .value(PropName::Rfc5545(Rfc5545PropName::RecurrenceId)),
            Caseless("RELATED-TO")
                .value(PropName::Rfc5545(Rfc5545PropName::RelatedTo)),
            Caseless("RESOURCES")
                .value(PropName::Rfc5545(Rfc5545PropName::Resources)),
            Caseless("RDATE")
                .value(PropName::Rfc5545(Rfc5545PropName::RecurrenceDateTimes)),
            Caseless("RRULE")
                .value(PropName::Rfc5545(Rfc5545PropName::RecurrenceRule)),
            Caseless("REPEAT")
                .value(PropName::Rfc5545(Rfc5545PropName::RepeatCount)),
            other,
        ))
        .parse_next(input)
    }

    fn s_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("SEQUENCE")
                .value(PropName::Rfc5545(Rfc5545PropName::SequenceNumber)),
            Caseless("SUMMARY")
                .value(PropName::Rfc5545(Rfc5545PropName::Summary)),
            Caseless("STATUS")
                .value(PropName::Rfc5545(Rfc5545PropName::Status)),
            Caseless("SOURCE")
                .value(PropName::Rfc7986(Rfc7986PropName::Source)),
            other,
        ))
        .parse_next(input)
    }

    fn t_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("TZOFFSETFROM")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom)),
            Caseless("TZOFFSETTO")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo)),
            Caseless("TZNAME")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneName)),
            Caseless("TZURL")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl)),
            Caseless("TZID")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier)),
            Caseless("TRIGGER")
                .value(PropName::Rfc5545(Rfc5545PropName::Trigger)),
            Caseless("TRANSP")
                .value(PropName::Rfc5545(Rfc5545PropName::TimeTransparency)),
            other,
        ))
        .parse_next(input)
    }

    fn u_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("URL").value(PropName::Rfc5545(
                Rfc5545PropName::UniformResourceLocator,
            )),
            Caseless("UID")
                .value(PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier)),
            other,
        ))
        .parse_next(input)
    }

    fn v_names<'i>(input: &mut &'i str) -> ModalResult<PropName<'i>> {
        alt((
            Caseless("VERSION")
                .value(PropName::Rfc5545(Rfc5545PropName::Version)),
            other,
        ))
        .parse_next(input)
    }

    match input.peek_token() {
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
        let prop = property(&mut input);
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
        let prop = property(&mut input);
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
        let prop = property.parse_peek(input);
        let expected_uri =
            UriStr::new("http://www.example.org/mysubj.au").unwrap();

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
            assert!(parse_value(ValueType::<&str>::Integer, &mut i).is_ok());
        }

        for mut i in ["-2147483649", "2147483648"] {
            assert!(parse_value(ValueType::<&str>::Integer, &mut i).is_err());
        }
    }

    // PROPERTY NAME TESTS

    /// Asserts that the inputs are equal under [`property_name`].
    fn assert_prop_name_eq<'i>(input: &'i str, expected: PropName<'i>) {
        let mut input_ref = input;
        let result = property_name.parse_next(&mut input_ref);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected);
        assert!(input_ref.is_empty(),);
    }

    // Helper function to test parsing failures
    fn assert_prop_name_parse_failure(input: &str) {
        let mut input_ref = input;
        let result = property_name.parse_next(&mut input_ref);
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
