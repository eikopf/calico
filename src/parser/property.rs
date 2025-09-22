//! Parsers for properties.

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, preceded, repeat, separated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::none_of,
};

use crate::{
    model::{
        parameter::{Param, Params, StaticParam, UnknownParam, UpcastParamValue},
        primitive::{DateTimeOrDate, UnknownKind, Value, ValueType},
        property::{Prop, RawPropValue, StaticProp},
        table::HashCaseless,
    },
    parser::{
        config::{Config, DefaultConfig},
        error::CalendarParseError,
        escaped::Equiv,
        parameter::parameter,
        primitive::{
            alarm_action, ascii_lower, binary, bool_caseless, cal_address, class_value, color,
            completion_percentage, date, datetime, datetime_utc, duration, float, geo, gregorian,
            iana_token, integer, method, participant_type, period, priority, proximity_value,
            request_status, resource_type, status, text, text_seq, time, time_transparency, tz_id,
            uid, uri, utc_offset, version, x_name,
        },
        rrule::rrule,
    },
};

#[derive(Debug, Clone)]
pub enum ParsedProp<S> {
    Known(KnownProp<S>),
    Unknown(UnknownProp<S>),
}

impl<S: PartialEq + HashCaseless + Equiv> PartialEq for ParsedProp<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Known(l0), Self::Known(r0)) => l0 == r0,
            (Self::Unknown(l0), Self::Unknown(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<S> ParsedProp<S> {
    #[allow(clippy::result_large_err)]
    pub fn try_into_known(self) -> Result<KnownProp<S>, Self> {
        if let Self::Known(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    #[allow(clippy::result_large_err)]
    pub fn try_into_unknown(self) -> Result<UnknownProp<S>, Self> {
        if let Self::Unknown(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone)]
pub struct KnownProp<S> {
    pub name: StaticProp,
    pub value: RawPropValue<S>,
}

impl<S: PartialEq + HashCaseless + Equiv> PartialEq for KnownProp<S> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.value == other.value
    }
}

#[derive(Debug, Clone)]
pub struct UnknownProp<S> {
    pub name: S,
    pub kind: UnknownKind,
    pub params: Params<S>,
    pub value: Value<S>,
}

impl<S: PartialEq + HashCaseless + Equiv> PartialEq for UnknownProp<S> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.kind == other.kind
            && self.value == other.value
            && self.params == other.params
    }
}

fn parse_value<I, E>(value_type: ValueType<I::Slice>, input: &mut I) -> Result<Value<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
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
        ValueType::Boolean => bool_caseless.map(Value::Boolean).parse_next(input),
        ValueType::CalAddress => cal_address::<_, _, false>
            .map(Value::CalAddress)
            .parse_next(input),
        ValueType::Date => date.map(Value::Date).parse_next(input),
        ValueType::DateTime => datetime.map(Value::DateTime).parse_next(input),
        ValueType::Duration => duration.map(Value::Duration).parse_next(input),
        ValueType::Float => float.map(Value::Float).parse_next(input),
        ValueType::Integer => integer.map(Value::Integer).parse_next(input),
        ValueType::Period => period.map(Value::Period).parse_next(input),
        ValueType::Recur => rrule.map(Value::Recur).parse_next(input),
        ValueType::Text => text.map(Value::Text).parse_next(input),
        ValueType::Time => time.map(Value::Time).parse_next(input),
        ValueType::Uri => uri::<_, _, false>.map(Value::Uri).parse_next(input),
        ValueType::UtcOffset => utc_offset.map(Value::UtcOffset).parse_next(input),
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

/// Parses a property.
pub fn property<I, E>(input: &mut I) -> Result<ParsedProp<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr + Clone + Eq + SliceLen + Stream + HashCaseless + Equiv,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    (|input: &mut I| {
        let mut config = DefaultConfig;
        property_with_config(input, &mut config)
    })
    .parse_next(input)
}

pub fn property_with_config<I, E>(
    input: &mut I,
    config: &mut impl Config,
) -> Result<ParsedProp<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr + Clone + Eq + SliceLen + Stream + HashCaseless + Equiv,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    // parse name
    let name = property_name.parse_next(input)?;

    // parse parameters
    let (params, value_type) = {
        let mut table: Params<I::Slice> = Default::default();
        let mut value_type: Option<ValueType<I::Slice>> = None;

        let params: Vec<Param<_>> = repeat(0.., preceded(';', parameter)).parse_next(input)?;
        for param in params {
            match param {
                Param::Known(param) => {
                    let key = param.name();

                    match param.upcast() {
                        UpcastParamValue::ValueType(vt) => match value_type {
                            Some(_) => {
                                return Err(E::from_external_error(
                                    input,
                                    CalendarParseError::DuplicateParam(StaticParam::Value),
                                ));
                            }
                            None => {
                                value_type = Some(vt);
                            }
                        },
                        UpcastParamValue::RawValue(value) => match table.contains_known(key) {
                            true => {
                                return Err(E::from_external_error(
                                    input,
                                    CalendarParseError::DuplicateParam(key),
                                ));
                            }
                            false => {
                                let _prev = table.insert_known(key, value);
                                debug_assert!(_prev.is_none());
                            }
                        },
                    }
                }
                Param::Unknown(UnknownParam { name, value }) => {
                    if table.contains_unknown(&name) {
                        let previous_value = &mut table.get_unknown_mut(name).unwrap().values;
                        let new_value = value.values;

                        config
                            .handle_duplicate_param(previous_value, new_value)
                            .map_err(|err| E::from_external_error(input, err.into()))?;
                    } else {
                        let _prev = table.insert_unknown(name, value);
                        debug_assert!(_prev.is_none());
                    }
                }
            }
        }

        (table, value_type)
    };

    // parse the colon separator (we use Caseless to avoid introducing a new bound)
    let _ = Caseless(":").parse_next(input)?;

    match name {
        PropName::Unknown { name, kind } => {
            let value = parse_value(value_type.unwrap_or(ValueType::Text), input)?;

            Ok(ParsedProp::Unknown(UnknownProp {
                name,
                kind,
                params,
                value,
            }))
        }
        PropName::Known(name) => {
            // in the grammar for a property, we're here:
            //
            //     name *(";" param) ":" value
            //                          ↑
            // so in order, we need to:
            // 1. parse the value (depending on the name and value type)
            // 2. check and possibly transform the parameters
            // 3. construct a KnownProp by upcasting to a RawPropValue

            macro_rules! trivial {
                ($parser:expr, $value_type:ident) => {{
                    if value_type
                        .as_ref()
                        .is_some_and(|x| x != &ValueType::$value_type)
                    {
                        return Err(E::from_external_error(
                            input,
                            CalendarParseError::InvalidValueType(value_type.unwrap()),
                        ));
                    }
                    Prop {
                        value: $parser.parse_next(input)?,
                        params,
                    }
                    .into()
                }};
                ($parser:expr, !$value_type:ident) => {{
                    if let Some(value_type) = value_type {
                        if !matches!(value_type, ValueType::$value_type) {
                            return Err(E::from_external_error(
                                input,
                                CalendarParseError::InvalidValueType(value_type),
                            ));
                        }
                    } else {
                        return Err(E::from_external_error(
                            input,
                            CalendarParseError::MissingValueType,
                        ));
                    }

                    Prop {
                        value: $parser.parse_next(input)?,
                        params,
                    }
                    .into()
                }};
            }

            macro_rules! dt_or_date {
                () => {{
                    let value = match value_type {
                        None | Some(ValueType::DateTime) => {
                            datetime.map(DateTimeOrDate::DateTime).parse_next(input)?
                        }
                        Some(ValueType::Date) => {
                            date.map(DateTimeOrDate::Date).parse_next(input)?
                        }
                        Some(value_type) => {
                            return Err(E::from_external_error(
                                input,
                                CalendarParseError::InvalidValueType(value_type),
                            ));
                        }
                    };
                    Prop { value, params }.into()
                }};
            }

            let value: RawPropValue<_> = match name {
                StaticProp::Attach => {
                    todo!("URI (default) or BINARY, ENCODING=BASE64 iff VALUE=BINARY")
                }
                StaticProp::ExDate => todo!("DATETIME (default) or DATE + sequential"),
                StaticProp::RDate => todo!("DATETIME (default) or DATE or PERIOD + sequential"),
                StaticProp::Trigger => todo!("DURATION (default) or DATETIME<UTC>"),
                StaticProp::Image => {
                    todo!("URI or BINARY (no default), ENCODING=BASE64 iff VALUE=BINARY")
                }
                StaticProp::StyledDescription => todo!("URI or TEXT (no default)"),
                StaticProp::StructuredData => {
                    todo!("TEXT or BINARY or URI (no default) + requirements depending on VALUE")
                }
                StaticProp::CalScale => trivial!(gregorian, Text),
                StaticProp::Method => trivial!(method, Text),
                StaticProp::ProdId
                | StaticProp::Comment
                | StaticProp::Description
                | StaticProp::Location
                | StaticProp::Summary
                | StaticProp::TzName
                | StaticProp::Contact
                | StaticProp::Name => trivial!(text, Text),
                StaticProp::Version => trivial!(version, Text),
                StaticProp::Categories | StaticProp::Resources => trivial!(text_seq, Text),
                StaticProp::Class => trivial!(class_value, Text),
                StaticProp::Geo => trivial!(geo, Float),
                StaticProp::PercentComplete => trivial!(completion_percentage, Integer),
                StaticProp::Priority => trivial!(priority, Integer),
                StaticProp::Status => trivial!(status, Text),
                StaticProp::DtCompleted
                | StaticProp::Created
                | StaticProp::DtStamp
                | StaticProp::LastModified => trivial!(datetime_utc, DateTime),
                StaticProp::DtEnd
                | StaticProp::DtDue
                | StaticProp::DtStart
                | StaticProp::RecurId => dt_or_date!(),
                StaticProp::Duration => trivial!(duration, Duration),
                StaticProp::FreeBusy => {
                    trivial!(separated(1.., period, ',').map(|v: Vec<_>| v), Period)
                }
                StaticProp::Transp => trivial!(time_transparency, Text),
                StaticProp::TzId => trivial!(tz_id, Text),
                StaticProp::TzOffsetFrom | StaticProp::TzOffsetTo => {
                    trivial!(utc_offset, UtcOffset)
                }
                StaticProp::TzUrl | StaticProp::Url => trivial!(uri::<_, _, false>, Uri),
                StaticProp::Attendee | StaticProp::Organizer | StaticProp::CalendarAddress => {
                    trivial!(cal_address::<_, _, false>, CalAddress)
                }
                StaticProp::RelatedTo | StaticProp::Uid => trivial!(uid, Text),
                StaticProp::RRule => trivial!(rrule.map(Box::new), Recur),
                StaticProp::Action => trivial!(alarm_action, Text),
                StaticProp::Repeat | StaticProp::Sequence => trivial!(integer, Integer),
                StaticProp::RequestStatus => trivial!(request_status, Text),
                StaticProp::RefreshInterval => trivial!(duration, !Duration),
                StaticProp::Source | StaticProp::Conference => trivial!(uri::<_, _, false>, !Uri),
                StaticProp::Color => trivial!(color, Text),
                StaticProp::LocationType => trivial!(text_seq, Text),
                StaticProp::ParticipantType => trivial!(participant_type, Text),
                StaticProp::ResourceType => trivial!(resource_type, Text),
                StaticProp::Acknowledged => trivial!(datetime_utc, DateTime),
                StaticProp::Proximity => trivial!(proximity_value, Text),
            };

            Ok(ParsedProp::Known(KnownProp { name, value }))
        }
    }
}

/// A property name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropName<S> {
    Known(StaticProp),
    Unknown { name: S, kind: UnknownKind },
}

impl<S> PropName<S> {
    #[inline(always)]
    pub const fn iana(name: S) -> Self {
        Self::Unknown {
            name,
            kind: UnknownKind::Iana,
        }
    }

    #[inline(always)]
    pub const fn x(name: S) -> Self {
        Self::Unknown {
            name,
            kind: UnknownKind::X,
        }
    }
}

/// Parses a [`PropName`].
pub fn property_name<I, E>(input: &mut I) -> Result<PropName<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
    PropName<I::Slice>: Clone,
{
    fn static_name<I>(input: &mut I) -> Result<StaticProp, ()>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
        I::Token: AsChar + Clone,
    {
        macro_rules! tail {
            ($s:literal, $c:expr) => {
                Caseless($s).value($c).parse_next(input)
            };
        }

        match ascii_lower.parse_next(input)? {
            'a' => match ascii_lower.parse_next(input)? {
                //'c' => tail!("tion", Rfc5545(PN5545::Action)),
                'c' => match ascii_lower.parse_next(input)? {
                    't' => tail!("ion", StaticProp::Action),
                    'k' => tail!("nowledged", StaticProp::Acknowledged),
                    _ => Err(()),
                },
                't' => match preceded(Caseless("t"), ascii_lower).parse_next(input)? {
                    'a' => tail!("ch", StaticProp::Attach),
                    'e' => tail!("ndee", StaticProp::Attendee),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'c' => match ascii_lower.parse_next(input)? {
                'a' => match ascii_lower.parse_next(input)? {
                    'l' => tail!("scale", StaticProp::CalScale),
                    't' => tail!("egories", StaticProp::Categories),
                    _ => Err(()),
                },
                'l' => tail!("ass", StaticProp::Class),
                'o' => match ascii_lower.parse_next(input)? {
                    'l' => tail!("or", StaticProp::Color),
                    'm' => match ascii_lower.parse_next(input)? {
                        'm' => tail!("ent", StaticProp::Comment),
                        'p' => tail!("leted", StaticProp::DtCompleted),
                        _ => Err(()),
                    },
                    'n' => match ascii_lower.parse_next(input)? {
                        'f' => tail!("erence", StaticProp::Conference),
                        't' => tail!("act", StaticProp::Contact),
                        _ => Err(()),
                    },
                    _ => Err(()),
                },
                'r' => tail!("eated", StaticProp::Created),
                _ => Err(()),
            },
            'd' => match ascii_lower.parse_next(input)? {
                'e' => tail!("scription", StaticProp::Description),
                't' => match ascii_lower.parse_next(input)? {
                    'e' => tail!("nd", StaticProp::DtEnd),
                    's' => match preceded(Caseless("ta"), ascii_lower).parse_next(input)? {
                        'm' => tail!("p", StaticProp::DtStamp),
                        'r' => tail!("t", StaticProp::DtStart),
                        _ => Err(()),
                    },
                    _ => Err(()),
                },
                'u' => match ascii_lower.parse_next(input)? {
                    'e' => tail!("", StaticProp::DtDue),
                    'r' => tail!("ation", StaticProp::Duration),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'e' => tail!("xdate", StaticProp::ExDate),
            'f' => tail!("reebusy", StaticProp::FreeBusy),
            'g' => tail!("eo", StaticProp::Geo),
            'i' => tail!("mage", StaticProp::Image),
            'l' => match ascii_lower.parse_next(input)? {
                'a' => tail!("st-modified", StaticProp::LastModified),
                'o' => tail!("cation", StaticProp::Location),
                _ => Err(()),
            },
            'm' => tail!("ethod", StaticProp::Method),
            'n' => tail!("ame", StaticProp::Name),
            'o' => tail!("rganizer", StaticProp::Organizer),
            'p' => match ascii_lower.parse_next(input)? {
                'e' => tail!("rcent-complete", StaticProp::PercentComplete),
                // PRIORITY | PRODID | PROXIMITY
                'r' => match ascii_lower.parse_next(input)? {
                    'i' => tail!("ority", StaticProp::Priority),
                    'o' => match ascii_lower.parse_next(input)? {
                        'd' => tail!("id", StaticProp::ProdId),
                        'x' => tail!("imity", StaticProp::Proximity),
                        _ => Err(()),
                    },
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'r' => match ascii_lower.parse_next(input)? {
                'e' => match ascii_lower.parse_next(input)? {
                    'c' => tail!("urrence-id", StaticProp::RecurId),
                    'f' => {
                        tail!("resh-interval", StaticProp::RefreshInterval)
                    }
                    'l' => tail!("ated-to", StaticProp::RelatedTo),
                    'p' => tail!("eat", StaticProp::Repeat),
                    'q' => tail!("uest-status", StaticProp::RequestStatus),
                    's' => tail!("ources", StaticProp::Resources),
                    _ => Err(()),
                },
                'd' => tail!("ate", StaticProp::RDate),
                'r' => tail!("ule", StaticProp::RRule),
                _ => Err(()),
            },
            's' => match ascii_lower.parse_next(input)? {
                'e' => tail!("quence", StaticProp::Sequence),
                'o' => tail!("urce", StaticProp::Source),
                't' => tail!("atus", StaticProp::Status),
                'u' => tail!("mmary", StaticProp::Summary),
                _ => Err(()),
            },
            't' => match ascii_lower.parse_next(input)? {
                // TRIGGER | TRANSP
                'r' => match ascii_lower.parse_next(input)? {
                    'a' => tail!("nsp", StaticProp::Transp),
                    'i' => tail!("gger", StaticProp::Trigger),
                    _ => Err(()),
                },
                // TZOFFSETFROM | TZOFFSETTO | TZNAME | TZURL | TZID
                'z' => match ascii_lower.parse_next(input)? {
                    'i' => tail!("d", StaticProp::TzId),
                    'n' => tail!("ame", StaticProp::TzName),
                    // TZOFFSETFROM | TZOFFSETTO
                    'o' => match preceded(Caseless("ffset"), ascii_lower).parse_next(input)? {
                        'f' => tail!("rom", StaticProp::TzOffsetFrom),
                        't' => tail!("o", StaticProp::TzOffsetTo),
                        _ => Err(()),
                    },
                    'u' => tail!("rl", StaticProp::TzUrl),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'u' => match ascii_lower.parse_next(input)? {
                'i' => tail!("d", StaticProp::Uid),
                'r' => tail!("l", StaticProp::Url),
                _ => Err(()),
            },
            'v' => tail!("ersion", StaticProp::Version),
            _ => Err(()),
        }
    }

    let checkpoint = input.checkpoint();
    match static_name.parse_next(input) {
        Ok(res) => Ok(PropName::Known(res)),
        Err(()) => {
            input.reset(&checkpoint);
            alt((x_name.map(PropName::x), iana_token.map(PropName::iana))).parse_next(input)
        }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc9073PropName {
    /// RFC 9073 §6.1
    LocationType,
    /// RFC 9073 §6.2
    ParticipantType,
    /// RFC 9073 §6.3
    ResourceType,
    /// RFC 9073 §6.4
    CalendarAddress,
    /// RFC 9073 §6.5
    StyledDescription,
    /// RFC 9073 §6.6
    StructuredData,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc9074PropName {
    /// RFC 9074 §6 (ACKNOWLEDGED)
    Acknowledged,
    /// RFC 9074 §8.1 (PROXIMITY)
    Proximity,
}

#[cfg(test)]
mod tests {
    use crate::{
        date,
        model::{
            parameter::{ParamValue, Params},
            primitive::{
                AttachValue, CalAddress, ClassValue, CompletionPercentage, DateTime,
                DateTimeOrDate, Duration, DurationKind, DurationTime, FormatType, FreeBusyType,
                Geo, GeoComponent, Gregorian, Language, Method, ParticipationRole,
                ParticipationStatus, Period, Priority, RawTime, RequestStatus, Status, Text,
                ThisAndFuture, Time, TimeFormat, TimeTransparency, TzId, Uid, Uri, Utc, Version,
            },
            property::Prop,
        },
        parser::escaped::AsEscaped,
        time, utc_offset,
    };

    use super::*;
    use winnow::{Parser, ascii::crlf, combinator::terminated};

    // PROPERTY PARSING TESTS

    macro_rules! known_prop {
        ($name:ident, $value:expr $(,)?) => {
            ParsedProp::Known(KnownProp {
                name: StaticProp::$name,
                value: Prop::<_, Params<_>>::from_value($value).into(),
            })
        };
    }

    #[test]
    fn apple_calendar_attendee_edge_case() {
        let input = r#"ATTENDEE;CN="John Smith";CUTYPE=INDIVIDUAL;EMAIL="john.smith@icloud.com";PARTSTAT=ACCEPTED;ROLE=CHAIR:/aMTg2ODQAyMzEjg0NX9m3Gyi2XcPHS8HXCT7Y3j1oq6U7hokvhVwdffK5c/principal/"#;
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        dbg![tail];
        dbg![prop];
    }

    #[test]
    fn apple_calendar_empty_url_line() {
        let input = "URL;VALUE=URI:";
        let (tail, _prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());
    }

    #[test]
    fn rfc_5545_section_4_example_1() {
        let input = include_bytes!("../../examples/rfc5545-section-4-example-1.ics").as_escaped();

        let (tail, props) = repeat(1.., terminated(property::<_, ()>, crlf))
            .map(Vec::into_boxed_slice)
            .parse_peek(input)
            .unwrap();

        assert!(tail.is_empty());
        assert_eq!(props.len(), 15);

        let mut iter = props.into_iter();
        let _ = iter.next(); // skip the first line, it's not a real property

        assert_eq!(
            iter.next(),
            Some(known_prop!(
                ProdId,
                Text("-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN".as_escaped(),)
            ))
        );

        assert_eq!(iter.next(), Some(known_prop!(Version, Version::V2_0)),);

        let _ = iter.next(); // next line is also not a real property

        assert_eq!(
            iter.next(),
            Some(known_prop!(
                DtStamp,
                DateTime {
                    date: date!(1996;7;4),
                    time: time!(12;0;0, Utc),
                }
            ))
        );

        assert_eq!(
            iter.next(),
            Some(known_prop!(Uid, Uid("uid1@example.com".as_escaped())))
        );

        assert_eq!(
            iter.next(),
            Some(known_prop!(
                Organizer,
                CalAddress("mailto:jsmith@example.com".as_escaped())
            ))
        );

        // omitted tests for remaining properties...
    }

    #[test]
    fn rfc_5545_example_calendar_scale_property() {
        let input = "CALSCALE:GREGORIAN";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(CalScale, Gregorian));
    }

    #[test]
    fn rfc_5545_example_method_property() {
        let input = "METHOD:REQUEST";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Method, Method::Request));
    }

    #[test]
    fn rfc_5545_example_product_identifier_property() {
        let input = "PRODID:-//ABC Corporation//NONSGML My Product//EN";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(ProdId, Text("-//ABC Corporation//NONSGML My Product//EN"))
        );
    }

    #[test]
    fn rfc_5545_example_version_property() {
        let input = "VERSION:2.0";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Version, Version::V2_0));
    }

    #[test]
    fn rfc_5545_example_attachment_property_1() {
        let input = "ATTACH:CID:jsmith.part3.960817T083000.xyzMail@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                Attach,
                AttachValue::Uri(Uri("CID:jsmith.part3.960817T083000.xyzMail@example.com"))
            )
        );
    }

    #[test]
    fn rfc_5545_example_attachment_property_2() {
        let input =
            "ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/\r\n reports/r-960812.ps"
                .as_escaped();
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Attach);

        let prop: Prop<AttachValue<_>, Params<_>> = value.downcast().unwrap();
        assert_eq!(
            prop.value,
            AttachValue::Uri(Uri(
                "ftp://example.com/pub/\r\n reports/r-960812.ps".as_escaped()
            ))
        );

        assert_eq!(
            prop.params.format_type(),
            Some(&FormatType {
                source: "application/postscript".as_escaped(),
                separator_index: 11
            })
        );
    }

    #[test]
    fn rfc_5545_example_categories_property_1() {
        let input = "CATEGORIES:APPOINTMENT,EDUCATION";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(Categories, vec![Text("APPOINTMENT"), Text("EDUCATION")])
        );
    }

    #[test]
    fn rfc_5545_example_categories_property_2() {
        let input = "CATEGORIES:MEETING";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Categories, vec![Text("MEETING")]));
    }

    #[test]
    fn rfc_5545_example_classification_property() {
        let input = "CLASS:PUBLIC";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Class, ClassValue::Public));
    }

    #[test]
    fn rfc_5545_example_comment_property() {
        let input = "COMMENT:The meeting really needs to include both ourselves \r\n and the customer. We can't hold this meeting without them. \r\n As a matter of fact\\, the venue for the meeting ought to be at \r\n their site. - - John";

        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Comment, Text(input[8..].as_escaped())));
    }

    #[test]
    fn rfc_5545_example_description_property() {
        let input = "DESCRIPTION:Meeting to provide technical review for \"Phoenix\" \r\n design.\\nHappy Face Conference Room. Phoenix design team \r\n MUST attend this meeting.\\nRSVP to team leader.";

        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(Description, Text(input[12..].as_escaped()))
        );
    }

    #[test]
    fn rfc_5545_example_geographic_position_property() {
        let input = "GEO:37.386013;-122.082932";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        let expected_geo = Geo {
            lat: GeoComponent(37386013),
            lon: GeoComponent(-122082932),
        };

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Geo, expected_geo));
    }

    #[test]
    fn rfc_5545_example_location_property_1() {
        let input = "LOCATION:Conference Room - F123\\, Bldg. 002";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(Location, Text("Conference Room - F123\\, Bldg. 002"))
        );
    }

    #[test]
    fn rfc_5545_example_location_property_2() {
        let input = "LOCATION;ALTREP=\"http://xyzcorp.com/conf-rooms/f123.vcf\":\r\n Conference Room - F123\\, Bldg. 002";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let alt_rep_uri = Uri("http://xyzcorp.com/conf-rooms/f123.vcf".as_escaped());
        let text = Text(input[57..].as_escaped());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Location);
        let prop: Prop<Text<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(prop.params.alternate_representation(), Some(&alt_rep_uri));
        assert_eq!(prop.value, text);
    }

    #[test]
    fn rfc_5545_example_percent_complete_property() {
        let input = "PERCENT-COMPLETE:39";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(PercentComplete, CompletionPercentage(39)));
    }

    #[test]
    fn rfc_5545_example_priority_property_1() {
        let input = "PRIORITY:1";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Priority, Priority::A1));
    }

    #[test]
    fn rfc_5545_example_priority_property_2() {
        let input = "PRIORITY:2";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Priority, Priority::A2));
    }

    #[test]
    fn rfc_5545_example_priority_property_3() {
        let input = "PRIORITY:0";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Priority, Priority::Zero));
    }

    #[test]
    fn rfc_5545_example_resources_property_1() {
        let input = "RESOURCES:EASEL,PROJECTOR,VCR";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                Resources,
                vec![Text("EASEL"), Text("PROJECTOR"), Text("VCR")]
            )
        );
    }

    #[test]
    fn rfc_5545_example_resources_property_2() {
        let input = "RESOURCES;LANGUAGE=fr:Nettoyeur haute pression";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Resources);
        let prop: Prop<Vec<Text<_>>, Params<_>> = value.downcast().unwrap();

        assert_eq!(prop.params.language(), Some(&Language("fr")));
        assert_eq!(prop.value, vec![Text("Nettoyeur haute pression")]);
    }

    #[test]
    fn rfc_5545_example_status_property_1() {
        let input = "STATUS:TENTATIVE";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Status, Status::Tentative));
    }

    #[test]
    fn rfc_5545_example_status_property_2() {
        let input = "STATUS:NEEDS-ACTION";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Status, Status::NeedsAction));
    }

    #[test]
    fn rfc_5545_example_status_property_3() {
        let input = "STATUS:DRAFT";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Status, Status::Draft));
    }

    #[test]
    fn rfc_5545_example_summary_property() {
        let input = "SUMMARY:Department Party";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Summary, Text("Department Party")));
    }

    #[test]
    fn rfc_5545_example_date_time_completed_property() {
        let input = "COMPLETED:19960401T150000Z";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                DtCompleted,
                DateTime {
                    date: date!(1996;4;1),
                    time: time!(15;0;0, Utc),
                }
            )
        );
    }

    #[test]
    fn rfc_5545_example_date_time_end_property_1() {
        let input = "DTEND:19960401T150000Z";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                DtEnd,
                DateTimeOrDate::DateTime(DateTime {
                    date: date!(1996;4;1),
                    time: time!(15;0;0, Utc),
                })
            )
        );
    }

    #[test]
    fn rfc_5545_example_date_time_end_property_2() {
        let input = "DTEND;VALUE=DATE:19980704";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(DtEnd, DateTimeOrDate::Date(date!(1998;7;4)))
        );
    }

    #[test]
    fn rfc_5545_example_date_time_due_property() {
        let input = "DUE:19980430T000000Z";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                DtDue,
                DateTimeOrDate::DateTime(DateTime {
                    date: date!(1998;4;30),
                    time: time!(0;0;0, Utc),
                })
            )
        );
    }

    #[test]
    fn rfc_5545_example_date_time_start_property() {
        let input = "DTSTART:19980118T073000Z";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                DtStart,
                DateTimeOrDate::DateTime(DateTime {
                    date: date!(1998;1;18),
                    time: time!(7;30;0, Utc),
                })
            )
        );
    }

    #[test]
    fn rfc_5545_example_duration_property_1() {
        let input = "DURATION:PT1H0M0S";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                Duration,
                Duration {
                    sign: None,
                    kind: DurationKind::Time {
                        time: DurationTime::HMS {
                            hours: 1,
                            minutes: 0,
                            seconds: 0,
                        }
                    }
                }
            )
        );
    }

    #[test]
    fn rfc_5545_example_duration_property_2() {
        let input = "DURATION:PT15M";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                Duration,
                Duration {
                    sign: None,
                    kind: DurationKind::Time {
                        time: DurationTime::M { minutes: 15 }
                    }
                }
            )
        );
    }

    #[test]
    fn rfc_5545_example_free_busy_time_property_1() {
        let input = "FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:19970308T160000Z/PT8H30M";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::FreeBusy);
        let prop: Prop<Vec<Period>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.free_busy_type(),
            Some(&FreeBusyType::BusyUnavailable)
        );

        assert_eq!(
            prop.value,
            vec![Period::Start {
                start: DateTime {
                    date: date!(1997;3;8),
                    time: Time {
                        raw: RawTime {
                            hours: 16,
                            minutes: 0,
                            seconds: 0
                        },
                        format: TimeFormat::Utc
                    },
                },
                duration: Duration {
                    sign: None,
                    kind: DurationKind::Time {
                        time: DurationTime::HM {
                            hours: 8,
                            minutes: 30
                        },
                    }
                }
            }]
        );
    }

    #[test]
    fn rfc_5545_example_free_busy_time_property_2() {
        let input = "FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::FreeBusy);
        let prop: Prop<Vec<Period>, Params<_>> = value.downcast().unwrap();

        assert_eq!(prop.params.free_busy_type(), Some(&FreeBusyType::Free));

        assert_eq!(
            prop.value,
            vec![
                Period::Start {
                    start: DateTime {
                        date: date!(1997;3;8),
                        time: Time {
                            raw: RawTime {
                                hours: 16,
                                minutes: 0,
                                seconds: 0
                            },
                            format: TimeFormat::Utc
                        },
                    },
                    duration: Duration {
                        sign: None,
                        kind: DurationKind::Time {
                            time: DurationTime::H { hours: 3 },
                        }
                    }
                },
                Period::Start {
                    start: DateTime {
                        date: date!(1997;3;8),
                        time: Time {
                            raw: RawTime {
                                hours: 20,
                                minutes: 0,
                                seconds: 0
                            },
                            format: TimeFormat::Utc
                        },
                    },
                    duration: Duration {
                        sign: None,
                        kind: DurationKind::Time {
                            time: DurationTime::H { hours: 1 },
                        }
                    }
                },
            ]
        );
    }

    #[test]
    fn rfc_5545_example_free_busy_time_property_3() {
        let input = "FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H\r\n\t,19970308T230000Z/19970309T000000Z";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::FreeBusy);
        let prop: Prop<Vec<Period>, Params<_>> = value.downcast().unwrap();

        assert_eq!(prop.params.free_busy_type(), Some(&FreeBusyType::Free));

        assert_eq!(
            prop.value,
            vec![
                Period::Start {
                    start: DateTime {
                        date: date!(1997;3;8),
                        time: Time {
                            raw: RawTime {
                                hours: 16,
                                minutes: 0,
                                seconds: 0
                            },
                            format: TimeFormat::Utc
                        },
                    },
                    duration: Duration {
                        sign: None,
                        kind: DurationKind::Time {
                            time: DurationTime::H { hours: 3 },
                        }
                    }
                },
                Period::Start {
                    start: DateTime {
                        date: date!(1997;3;8),
                        time: Time {
                            raw: RawTime {
                                hours: 20,
                                minutes: 0,
                                seconds: 0
                            },
                            format: TimeFormat::Utc
                        },
                    },
                    duration: Duration {
                        sign: None,
                        kind: DurationKind::Time {
                            time: DurationTime::H { hours: 1 },
                        }
                    }
                },
                Period::Explicit {
                    start: DateTime {
                        date: date!(1997;3;8),
                        time: Time {
                            raw: RawTime {
                                hours: 23,
                                minutes: 0,
                                seconds: 0
                            },
                            format: TimeFormat::Utc
                        },
                    },
                    end: DateTime {
                        date: date!(1997;3;9),
                        time: Time {
                            raw: RawTime {
                                hours: 0,
                                minutes: 0,
                                seconds: 0
                            },
                            format: TimeFormat::Utc
                        },
                    },
                }
            ]
        );
    }

    #[test]
    fn rfc_5545_example_time_transparency_property_1() {
        let input = "TRANSP:TRANSPARENT";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Transp, TimeTransparency::Transparent));
    }

    #[test]
    fn rfc_5545_example_time_transparency_property_2() {
        let input = "TRANSP:OPAQUE";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(Transp, TimeTransparency::Opaque));
    }

    #[test]
    fn rfc_5545_example_time_zone_identifier_property_1() {
        let input = "TZID:America/New_York";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzId, TzId("America/New_York")));
    }

    #[test]
    fn rfc_5545_example_time_zone_identifier_property_2() {
        let input = "TZID:America/Los_Angeles";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzId, TzId("America/Los_Angeles")));
    }

    #[test]
    fn rfc_5545_example_time_zone_identifier_property_3() {
        let input = "TZID:/example.org/America/New_York";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(TzId, TzId("/example.org/America/New_York"))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_name_property_1() {
        let input = "TZNAME:EST";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzName, Text("EST")));
    }

    #[test]
    fn rfc_5545_example_time_zone_name_property_2() {
        let input = "TZNAME;LANGUAGE=fr-CA:HNE";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::TzName);
        let prop: Prop<Text<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(prop.params.language(), Some(&Language("fr-CA")));
        assert_eq!(prop.value, Text("HNE"));
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_from_property_1() {
        let input = "TZOFFSETFROM:-0500";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzOffsetFrom, utc_offset!(-5;00)));
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_from_property_2() {
        let input = "TZOFFSETFROM:+1345";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzOffsetFrom, utc_offset!(+13;45)));
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_to_property_1() {
        let input = "TZOFFSETTO:-0400";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzOffsetTo, utc_offset!(-4;00)));
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_to_property_2() {
        let input = "TZOFFSETTO:+1245";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(prop, known_prop!(TzOffsetTo, utc_offset!(+12;45)));
    }

    #[test]
    fn rfc_5545_example_time_zone_url_property() {
        let input = "TZURL:http://timezones.example.org/tz/America-Los_Angeles.ics";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                TzUrl,
                Uri("http://timezones.example.org/tz/America-Los_Angeles.ics")
            )
        );
    }

    #[test]
    fn rfc_5545_example_attendee_property_1() {
        let input =
            "ATTENDEE;MEMBER=\"mailto:DEV-GROUP@example.com\":\r\n mailto:joecool@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Attendee);
        let prop: Prop<CalAddress<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.membership(),
            Some(&vec![CalAddress(
                "mailto:DEV-GROUP@example.com".as_escaped()
            )])
        );

        assert_eq!(
            prop.value,
            CalAddress("\r\n mailto:joecool@example.com".as_escaped())
        );
    }

    #[test]
    fn rfc_5545_example_attendee_property_2() {
        let input =
            "ATTENDEE;DELEGATED-FROM=\"mailto:immud@example.com\":\r\n mailto:ildoit@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Attendee);
        let prop: Prop<CalAddress<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.delegated_from(),
            Some(&vec![CalAddress("mailto:immud@example.com".as_escaped())])
        );

        assert_eq!(
            prop.value,
            CalAddress("\r\n mailto:ildoit@example.com".as_escaped())
        );
    }

    #[test]
    fn rfc_5545_example_attendee_property_3() {
        let input = "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry\r\n Cabot:mailto:hcabot@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Attendee);
        let prop: Prop<CalAddress<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.participation_role(),
            Some(&ParticipationRole::ReqParticipant)
        );
        assert_eq!(
            prop.params.participation_status(),
            Some(&ParticipationStatus::Tentative)
        );
        assert_eq!(
            prop.params.common_name(),
            Some(&ParamValue::Safe("Henry\r\n Cabot".as_escaped()))
        );

        assert_eq!(
            prop.value,
            CalAddress("mailto:hcabot@example.com".as_escaped())
        );
    }

    #[test]
    fn rfc_5545_example_contact_property_1() {
        let input = "CONTACT:Jim Dolittle\\, ABC Industries\\, +1-919-555-1234";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                Contact,
                Text("Jim Dolittle\\, ABC Industries\\, +1-919-555-1234")
            )
        );
    }

    #[test]
    fn rfc_5545_example_contact_property_2() {
        let input = "CONTACT;ALTREP=\"ldap://example.com:6666/o=ABC%20Industries\\,\r\n c=US???(cn=Jim%20Dolittle)\":Jim Dolittle\\, ABC Industries\\,\r\n +1-919-555-1234";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Contact);
        let prop: Prop<Text<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.alternate_representation(),
            Some(&Uri(input[16..89].as_escaped()))
        );

        assert_eq!(prop.value, Text(input[91..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_organizer_property_1() {
        let input = "ORGANIZER;CN=John Smith:mailto:jsmith@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Organizer);
        let prop: Prop<CalAddress<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.common_name(),
            Some(&ParamValue::Safe("John Smith"))
        );
        assert_eq!(prop.value, CalAddress("mailto:jsmith@example.com"));
    }

    #[test]
    fn rfc_5545_example_organizer_property_2() {
        let input = "ORGANIZER;CN=JohnSmith;DIR=\"ldap://example.com:6666/o=DC%20Ass\r\n ociates,c=US???(cn=John%20Smith)\":mailto:jsmith@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input.as_escaped()).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::Organizer);
        let prop: Prop<CalAddress<_>, Params<_>> = value.downcast().unwrap();

        assert_eq!(
            prop.params.common_name(),
            Some(&ParamValue::Safe("JohnSmith".as_escaped()))
        );

        assert_eq!(
            prop.params.directory_reference(),
            Some(&Uri(input[28..97].as_escaped()))
        );

        assert_eq!(prop.value, CalAddress(input[99..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_recurrence_identifier_property_1() {
        let input = "RECURRENCE-ID;VALUE=DATE:19960401";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(RecurId, DateTimeOrDate::Date(date!(1996;4;1)))
        );
    }

    #[test]
    fn rfc_5545_example_recurrence_identifier_property_2() {
        let input = "RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();
        assert!(tail.is_empty());

        let KnownProp { name, value } = prop.try_into_known().unwrap();
        assert_eq!(name, StaticProp::RecurId);
        let prop: Prop<DateTimeOrDate, Params<_>> = value.downcast().unwrap();

        assert_eq!(prop.params.recurrence_range(), Some(&ThisAndFuture));

        assert_eq!(
            prop.value,
            DateTime {
                date: date!(1996;1;20),
                time: time!(12;00;00, Utc),
            }
            .into()
        );
    }

    #[test]
    fn rfc_5545_example_uid_property() {
        let input = "UID:19960401T080045Z-4000F192713-0052@example.com";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(Uid, Uid("19960401T080045Z-4000F192713-0052@example.com"))
        );
    }

    #[test]
    fn rfc_5545_example_request_status_property_1() {
        let input = "REQUEST-STATUS:2.0;Success";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                RequestStatus,
                RequestStatus {
                    code: (2, 0).into(),
                    description: Text("Success"),
                    exception_data: None,
                }
            )
        );
    }

    #[test]
    fn rfc_5545_example_request_status_property_2() {
        let input = "REQUEST-STATUS:3.1;Invalid property value;DTSTART:96-Apr-01";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                RequestStatus,
                RequestStatus {
                    code: (3, 1).into(),
                    description: Text("Invalid property value"),
                    exception_data: Some(Text("DTSTART:96-Apr-01")),
                }
            )
        );
    }

    // NOTE: skipped the third example

    #[test]
    fn rfc_5545_example_request_status_property_4() {
        let input = "REQUEST-STATUS:4.1;Event conflict.  Date-time is busy.";
        let (tail, prop) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert_eq!(
            prop,
            known_prop!(
                RequestStatus,
                RequestStatus {
                    code: (4, 1).into(),
                    description: Text("Event conflict.  Date-time is busy."),
                    exception_data: None,
                }
            )
        );
    }

    #[test]
    fn rfc_5545_example_iana_property() {
        let mut input = "NON-SMOKING;VALUE=BOOLEAN:TRUE";
        let prop = property::<_, ()>(&mut input);
        assert!(matches!(
            prop,
            Ok(
                ParsedProp::Unknown(UnknownProp {
                    name: "NON-SMOKING",
                    kind: UnknownKind::Iana,
                    value: Value::Boolean(true),
                    params,
                }),
            ) if params.is_empty(),
        ));

        let mut input = "NON-SMOKING:TRUE";
        let prop = property::<_, ()>(&mut input);
        assert!(matches!(
            prop,
            Ok(
                ParsedProp::Unknown(UnknownProp {
                    name: "NON-SMOKING",
                    kind: UnknownKind::Iana,
                    value: Value::Text("TRUE"),
                    params
                }),
            ) if params.is_empty(),
        ));
    }

    #[test]
    fn rfc_5545_example_x_property() {
        let input = "X-ABC-MMSUBJ;VALUE=URI;FMTTYPE=audio/basic:http://www.example.org/mysubj.au";
        let prop = property::<_, ()>.parse_peek(input);
        let expected_uri = Uri("http://www.example.org/mysubj.au");

        dbg![&prop];

        assert!(matches!(
            prop, Ok(("", ParsedProp::Unknown(UnknownProp {
                name: "X-ABC-MMSUBJ",
                kind: UnknownKind::X,
                value: Value::Uri(uri),
                params: _
            }))) if uri == expected_uri
        ))
    }

    #[test]
    fn integer_value_parsing() {
        for mut i in ["0", "-2147483648", "2147483647"] {
            assert!(parse_value::<_, ()>(ValueType::<&str>::Integer, &mut i).is_ok());
        }

        for mut i in ["-2147483649", "2147483648"] {
            assert!(parse_value::<_, ()>(ValueType::<&str>::Integer, &mut i).is_err());
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
        assert_prop_name_eq("CALSCALE", PropName::Known(StaticProp::CalScale));
        assert_prop_name_eq("METHOD", PropName::Known(StaticProp::Method));
        assert_prop_name_eq("PRODID", PropName::Known(StaticProp::ProdId));
        assert_prop_name_eq("VERSION", PropName::Known(StaticProp::Version));
    }

    #[test]
    fn rfc5545_descriptive_property_names() {
        assert_prop_name_eq("ATTACH", PropName::Known(StaticProp::Attach));
        assert_prop_name_eq("CATEGORIES", PropName::Known(StaticProp::Categories));
        assert_prop_name_eq("CLASS", PropName::Known(StaticProp::Class));
        assert_prop_name_eq("COMMENT", PropName::Known(StaticProp::Comment));
        assert_prop_name_eq("DESCRIPTION", PropName::Known(StaticProp::Description));
        assert_prop_name_eq("GEO", PropName::Known(StaticProp::Geo));
        assert_prop_name_eq("LOCATION", PropName::Known(StaticProp::Location));
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Known(StaticProp::PercentComplete),
        );
        assert_prop_name_eq("PRIORITY", PropName::Known(StaticProp::Priority));
        assert_prop_name_eq("RESOURCES", PropName::Known(StaticProp::Resources));
        assert_prop_name_eq("STATUS", PropName::Known(StaticProp::Status));
        assert_prop_name_eq("SUMMARY", PropName::Known(StaticProp::Summary));
    }

    #[test]
    fn rfc5545_datetime_property_names() {
        assert_prop_name_eq("COMPLETED", PropName::Known(StaticProp::DtCompleted));
        assert_prop_name_eq("DTEND", PropName::Known(StaticProp::DtEnd));
        assert_prop_name_eq("DUE", PropName::Known(StaticProp::DtDue));
        assert_prop_name_eq("DTSTART", PropName::Known(StaticProp::DtStart));
        assert_prop_name_eq("DURATION", PropName::Known(StaticProp::Duration));
        assert_prop_name_eq("FREEBUSY", PropName::Known(StaticProp::FreeBusy));
        assert_prop_name_eq("TRANSP", PropName::Known(StaticProp::Transp));
        assert_prop_name_eq("DTSTAMP", PropName::Known(StaticProp::DtStamp));
    }

    #[test]
    fn rfc5545_timezone_property_names() {
        assert_prop_name_eq("TZID", PropName::Known(StaticProp::TzId));
        assert_prop_name_eq("TZNAME", PropName::Known(StaticProp::TzName));
        assert_prop_name_eq("TZOFFSETFROM", PropName::Known(StaticProp::TzOffsetFrom));
        assert_prop_name_eq("TZOFFSETTO", PropName::Known(StaticProp::TzOffsetTo));
        assert_prop_name_eq("TZURL", PropName::Known(StaticProp::TzUrl));
    }

    #[test]
    fn rfc5545_relationship_property_names() {
        assert_prop_name_eq("ATTENDEE", PropName::Known(StaticProp::Attendee));
        assert_prop_name_eq("CONTACT", PropName::Known(StaticProp::Contact));
        assert_prop_name_eq("ORGANIZER", PropName::Known(StaticProp::Organizer));
        assert_prop_name_eq("RECURRENCE-ID", PropName::Known(StaticProp::RecurId));
        assert_prop_name_eq("RELATED-TO", PropName::Known(StaticProp::RelatedTo));
        assert_prop_name_eq("URL", PropName::Known(StaticProp::Url));
        assert_prop_name_eq("UID", PropName::Known(StaticProp::Uid));
    }

    #[test]
    fn rfc5545_recurrence_property_names() {
        assert_prop_name_eq("EXDATE", PropName::Known(StaticProp::ExDate));
        assert_prop_name_eq("RDATE", PropName::Known(StaticProp::RDate));
        assert_prop_name_eq("RRULE", PropName::Known(StaticProp::RRule));
    }

    #[test]
    fn rfc5545_alarm_property_names() {
        assert_prop_name_eq("ACTION", PropName::Known(StaticProp::Action));
        assert_prop_name_eq("REPEAT", PropName::Known(StaticProp::Repeat));
        assert_prop_name_eq("TRIGGER", PropName::Known(StaticProp::Trigger));
    }

    #[test]
    fn rfc5545_change_management_property_names() {
        assert_prop_name_eq("CREATED", PropName::Known(StaticProp::Created));
        assert_prop_name_eq("LAST-MODIFIED", PropName::Known(StaticProp::LastModified));
        assert_prop_name_eq("SEQUENCE", PropName::Known(StaticProp::Sequence));
    }

    #[test]
    fn rfc5545_miscellaneous_property_names() {
        assert_prop_name_eq("REQUEST-STATUS", PropName::Known(StaticProp::RequestStatus));
    }

    #[test]
    fn rfc7986_property_names() {
        assert_prop_name_eq("NAME", PropName::Known(StaticProp::Name));
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Known(StaticProp::RefreshInterval),
        );
        assert_prop_name_eq("SOURCE", PropName::Known(StaticProp::Source));
        assert_prop_name_eq("COLOR", PropName::Known(StaticProp::Color));
        assert_prop_name_eq("IMAGE", PropName::Known(StaticProp::Image));
        assert_prop_name_eq("CONFERENCE", PropName::Known(StaticProp::Conference));
    }

    #[test]
    fn property_name_case_insensitivity() {
        assert_prop_name_eq("dtstart", PropName::Known(StaticProp::DtStart));
        assert_prop_name_eq("DTSTART", PropName::Known(StaticProp::DtStart));
        assert_prop_name_eq("DtStArT", PropName::Known(StaticProp::DtStart));
        assert_prop_name_eq("dtSTART", PropName::Known(StaticProp::DtStart));

        assert_prop_name_eq("conference", PropName::Known(StaticProp::Conference));
        assert_prop_name_eq("Conference", PropName::Known(StaticProp::Conference));
        assert_prop_name_eq("CONFERENCE", PropName::Known(StaticProp::Conference));
    }

    #[test]
    fn iana_property_names() {
        assert_prop_name_eq("UNKNOWN-PROP", PropName::iana("UNKNOWN-PROP"));
        assert_prop_name_eq("CUSTOM", PropName::iana("CUSTOM"));
        assert_prop_name_eq("NEW-FEATURE", PropName::iana("NEW-FEATURE"));
    }

    #[test]
    fn x_property_names() {
        assert_prop_name_eq("X-CUSTOM", PropName::x("X-CUSTOM"));
        assert_prop_name_eq("X-VENDOR-PROP", PropName::x("X-VENDOR-PROP"));
        assert_prop_name_eq("X-custom", PropName::x("X-custom"));
    }

    #[test]
    fn property_name_longest_match_precedence() {
        // Ensure longer properties are matched correctly when they share prefixes
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Known(StaticProp::RefreshInterval),
        );
        assert_prop_name_eq("REQUEST-STATUS", PropName::Known(StaticProp::RequestStatus));
        assert_prop_name_eq("RECURRENCE-ID", PropName::Known(StaticProp::RecurId));

        // Make sure we don't match shorter prefixes
        assert_prop_name_eq("REFRESH", PropName::iana("REFRESH"));
        assert_prop_name_eq("REQUEST", PropName::iana("REQUEST"));
        assert_prop_name_eq("RECURRENCE", PropName::iana("RECURRENCE"));
    }

    #[test]
    fn property_name_edge_cases() {
        // Empty input
        assert_prop_name_parse_failure("");

        // Single characters
        assert_prop_name_eq("A", PropName::iana("A"));
        assert_prop_name_eq("Z", PropName::iana("Z"));

        // Properties with hyphens
        assert_prop_name_eq("LAST-MODIFIED", PropName::Known(StaticProp::LastModified));
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Known(StaticProp::PercentComplete),
        );
        assert_prop_name_eq(
            "REFRESH-INTERVAL",
            PropName::Known(StaticProp::RefreshInterval),
        );

        // Mixed case with hyphens
        assert_prop_name_eq("last-modified", PropName::Known(StaticProp::LastModified));
        assert_prop_name_eq(
            "Percent-Complete",
            PropName::Known(StaticProp::PercentComplete),
        );
    }
}
