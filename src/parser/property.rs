//! Parsers for properties.

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, fail, preceded, repeat, separated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::none_of,
};

use crate::{
    model::{
        css::Css3Color,
        primitive::{
            AlarmAction, AttachValue, ClassValue, CompletionPercentage,
            DateTime, DateTimeOrDate, Duration, Encoding, FormatType,
            FreeBusyType, Geo, ImageData, Language, Method,
            ParticipationStatus, Period, Priority, RDate, RelationshipType,
            Text, ThisAndFuture, TimeTransparency, TzId, Uid, Uri, Utc,
            UtcOffset, Value, ValueType,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams,
            ImageParams, LangParams, OrganizerParams, RecurrenceIdParams,
            RelTypeParams, TextParams, TriggerParams,
        },
    },
    parser::{
        parameter::{KnownParam, Param, Rfc5545ParamName, parameter},
        primitive::{
            class_value, completion_percentage, datetime_utc, duration, float,
            geo, gregorian, iana_token, integer, method, participation_status,
            period, priority, text, time_transparency, tz_id, uid, utc_offset,
            v2_0, x_name,
        },
    },
};

use super::{
    parameter::{StaticParamName, UnknownParam},
    primitive::{
        InvalidCompletionPercentageError, InvalidDateError,
        InvalidDurationTimeError, InvalidGeoError, InvalidIntegerError,
        InvalidPriorityError, InvalidRawTimeError, InvalidUtcOffsetError,
        binary, bool_caseless, date, datetime, time, uri,
    },
};

// NOTE: the IANA iCalendar property registry lists several registered properties
// from RFC 6321 §4.2, RFC 7808 §7, RFC 7953 §3.2, RFC 9073 §6, and RFC 9253 §8
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

// TODO: refactor value types to make Uri and CalAddress distinct types. this is
// required for at least the Attendee and Organizer variants of KnownProp

#[derive(Debug, Clone)]
pub enum KnownProp<S> {
    // CALENDAR PROPERTIES
    CalScale,
    Method(Method<S>),
    ProdId(Text<S>),
    Version,
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(AttachValue<S>, AttachParams<S>),
    Categories(Box<[Text<S>]>, LangParams<S>),
    Class(ClassValue<S>),
    Comment(Text<S>, TextParams<S>),
    Description(Text<S>, TextParams<S>),
    Geo(Geo),
    Location(Text<S>, TextParams<S>),
    PercentComplete(CompletionPercentage),
    Priority(Priority),
    Resources(Box<[Text<S>]>, TextParams<S>),
    Status(ParticipationStatus<S>),
    Summary(Text<S>, TextParams<S>),
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted(DateTime<Utc>),
    DtEnd(DateTimeOrDate, DtParams<S>),
    DtDue(DateTimeOrDate, DtParams<S>),
    DtStart(DateTimeOrDate, DtParams<S>),
    Duration(Duration),
    FreeBusy(Box<[Period]>, FBTypeParams<S>),
    Transparency(TimeTransparency),
    // TIME ZONE COMPONENT PROPERTIES
    TzId(TzId<S>),
    TzName(Text<S>, LangParams<S>),
    TzOffsetFrom(UtcOffset),
    TzOffsetTo(UtcOffset),
    TzUrl(Uri<S>),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(Uri<S>, AttendeeParams<S>),
    Contact(Text<S>, TextParams<S>),
    Organizer(Uri<S>, OrganizerParams<S>),
    RecurrenceId(DateTimeOrDate, RecurrenceIdParams<S>),
    RelatedTo(Text<S>, RelTypeParams<S>),
    Url(Uri<S>),
    Uid(Uid<S>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(DateTimeOrDate, DtParams<S>),
    RDate(Box<[RDate]>, DtParams<S>),
    // TODO: finish recurrence rule model
    RRule(()),
    // ALARM COMPONENT PROPERTIES
    Action(AlarmAction<S>),
    Repeat(usize),
    TriggerRelative(Duration, TriggerParams),
    TriggerAbsolute(DateTime<Utc>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(DateTime),
    DtStamp(DateTime),
    LastModified(DateTime),
    Sequence(usize),
    // MISCELLANEOUS COMPONENT PROPERTIES
    // TODO: the value of this property has a more precise grammar (page 143)
    RequestStatus(Text<S>, LangParams<S>),
    // RFC 7986 PROPERTIES
    Name(Text<S>, TextParams<S>),
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
        params: Box<[KnownParam<S>]>,
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
        + FromExternalError<I, InvalidIntegerError>
        + FromExternalError<I, InvalidUtcOffsetError>,
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
        ValueType::UtcOffset => {
            utc_offset.map(Value::UtcOffset).parse_next(input)
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
/// Either [`DuplicateParamError`] or [`UnexpectedKnownParamError`].
pub enum DuplicateOrUnexpectedError<S> {
    Duplicate(DuplicateParamError),
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// A parameter with a multiplicity less than 2 occurred more than once.
pub struct DuplicateParamError(StaticParamName);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedKnownParamError<S> {
    current_property: PropName<S>,
    unexpected_param: KnownParam<S>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttachParamError<S> {
    /// Value type was a URI and the ENCODING parameter was present.
    EncodingOnUri,
    /// Value type was BINARY and the ENCODING parameter was not present.
    BinaryWithoutEncoding,
    /// ENCODING parameter was 8BIT; the only allowed value is BASE64.
    Bit8Encoding,
    /// The VALUE parameter occurred and was not BINARY.
    NonBinaryValueType,
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParamName),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DtParamError<S> {
    /// The VALUE parameter occurred and was not DATETIME or DATE.
    InvalidValueType(ValueType<S>),
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParamName),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RDateParamError<S> {
    /// The VALUE parameter occurred and was not DATETIME, DATE, or PERIOD.
    InvalidValueType(ValueType<S>),
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParamName),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
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
    I::Slice: AsBStr + Clone + SliceLen + Stream,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I>
        + FromExternalError<I, InvalidDateError>
        + FromExternalError<I, InvalidRawTimeError>
        + FromExternalError<I, InvalidDurationTimeError>
        + FromExternalError<I, InvalidIntegerError>
        + FromExternalError<I, InvalidUtcOffsetError>
        + FromExternalError<I, InvalidGeoError>
        + FromExternalError<I, InvalidCompletionPercentageError>
        + FromExternalError<I, InvalidPriorityError>
        + FromExternalError<I, DuplicateParamError>
        + FromExternalError<I, DuplicateOrUnexpectedError<I::Slice>>
        + FromExternalError<I, UnexpectedKnownParamError<I::Slice>>
        + FromExternalError<I, AttachParamError<I::Slice>>
        + FromExternalError<I, DtParamError<I::Slice>>
        + FromExternalError<I, RDateParamError<I::Slice>>,
{
    struct StateMachine<Src, S, F> {
        state: S,
        step: F,
        unknown_params: Vec<UnknownParam<Src>>,
    }

    impl<Src, S, F> StateMachine<Src, S, F> {
        pub fn new(state: S, step: F) -> Self {
            StateMachine {
                state,
                step,
                unknown_params: Vec::new(),
            }
        }

        pub fn advance<E>(&mut self, param: KnownParam<Src>) -> Result<(), E>
        where
            F: FnMut(KnownParam<Src>, &mut S) -> Result<(), E>,
        {
            (self.step)(param, &mut self.state)
        }
    }

    #[allow(clippy::type_complexity)]
    fn sm_parse_next<I, S, F, E1, E2>(
        mut sm: StateMachine<I::Slice, S, F>,
        input: &mut I,
    ) -> Result<(S, Box<[UnknownParam<I::Slice>]>), E1>
    where
        I: StreamIsPartial
            + Stream
            + Compare<Caseless<&'static str>>
            + Compare<char>,
        I::Token: AsChar + Clone,
        I::Slice: Clone + SliceLen,
        E1: ParserError<I> + FromExternalError<I, E2>,
        F: FnMut(KnownParam<I::Slice>, &mut S) -> Result<(), E2>,
    {
        while input.peek_token().is_some_and(|t| t.as_char() != ':') {
            let param = preceded(';', parameter).parse_next(input)?;

            match param {
                Param::Known(known_param) => {
                    let () = sm
                        .advance(known_param)
                        .map_err(|err| E1::from_external_error(input, err))?;
                }
                Param::Unknown(unknown_param) => {
                    sm.unknown_params.push(unknown_param);
                }
            }
        }

        Ok((sm.state, sm.unknown_params.into_boxed_slice()))
    }

    /// The step function for unknown (IANA and X) properties.
    fn unknown_step<Src>(
        param: KnownParam<Src>,
        state: &mut (Option<ValueType<Src>>, Vec<KnownParam<Src>>),
    ) -> Result<(), DuplicateParamError> {
        match param {
            KnownParam::Value(value_type) => match state.0 {
                Some(_) => Err(DuplicateParamError(StaticParamName::Rfc5545(
                    Rfc5545ParamName::ValueDataType,
                ))),
                None => {
                    state.0 = Some(value_type);
                    Ok(())
                }
            },
            known_param => {
                state.1.push(known_param);
                Ok(())
            }
        }
    }

    /// Returns the step function for properties with no known parameters.
    fn trivial_step<Src: Clone>(
        current_property: PropName<Src>,
    ) -> impl FnMut(
        KnownParam<Src>,
        &mut (),
    ) -> Result<(), UnexpectedKnownParamError<Src>> {
        move |param, &mut ()| {
            Err(UnexpectedKnownParamError {
                current_property: current_property.clone(),
                unexpected_param: param,
            })
        }
    }

    struct LangParamState<S> {
        language: Option<Language<S>>,
    }

    impl<S> Default for LangParamState<S> {
        fn default() -> Self {
            Self {
                language: Default::default(),
            }
        }
    }

    fn lang_param_step<S: Clone>(
        current_property: PropName<S>,
    ) -> impl FnMut(
        KnownParam<S>,
        &mut LangParamState<S>,
    ) -> Result<(), DuplicateOrUnexpectedError<S>> {
        move |param, state| match param {
            KnownParam::Language(language) => match state.language {
                Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                    DuplicateParamError(StaticParamName::Rfc5545(
                        Rfc5545ParamName::Language,
                    )),
                )),
                None => {
                    state.language = Some(language);
                    Ok(())
                }
            },
            unexpected_param => Err(DuplicateOrUnexpectedError::Unexpected(
                UnexpectedKnownParamError {
                    current_property: current_property.clone(),
                    unexpected_param,
                },
            )),
        }
    }

    struct TextParamState<S> {
        alt_rep: Option<Uri<S>>,
        language: Option<Language<S>>,
    }

    impl<S> Default for TextParamState<S> {
        fn default() -> Self {
            Self {
                alt_rep: Default::default(),
                language: Default::default(),
            }
        }
    }

    impl<S> TextParamState<S> {
        fn into_params(self) -> TextParams<S> {
            TextParams {
                alternate_representation: self.alt_rep,
                language: self.language,
            }
        }
    }

    /// Returns the step function for properties with [`TextParams`].
    fn text_param_step<S: Clone>(
        current_property: PropName<S>,
    ) -> impl FnMut(
        KnownParam<S>,
        &mut TextParamState<S>,
    ) -> Result<(), DuplicateOrUnexpectedError<S>> {
        move |param, state| match param {
            KnownParam::AltRep(uri) => match state.alt_rep {
                Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                    DuplicateParamError(StaticParamName::Rfc5545(
                        Rfc5545ParamName::AlternateTextRepresentation,
                    )),
                )),
                None => {
                    state.alt_rep = Some(uri);
                    Ok(())
                }
            },
            KnownParam::Language(language) => match state.language {
                Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                    DuplicateParamError(StaticParamName::Rfc5545(
                        Rfc5545ParamName::Language,
                    )),
                )),
                None => {
                    state.language = Some(language);
                    Ok(())
                }
            },
            unexpected_param => Err(DuplicateOrUnexpectedError::Unexpected(
                UnexpectedKnownParamError {
                    current_property: current_property.clone(),
                    unexpected_param,
                },
            )),
        }
    }

    #[derive(Default)]
    enum DateTimeOrDateType {
        #[default]
        DateTime,
        Date,
    }

    impl DateTimeOrDateType {
        fn try_from_value_type<S>(
            value_type: ValueType<S>,
        ) -> Result<Self, ValueType<S>> {
            match value_type {
                ValueType::Date => Ok(Self::Date),
                ValueType::DateTime => Ok(Self::DateTime),
                value_type => Err(value_type),
            }
        }
    }

    struct DtParamState<S> {
        value_type: Option<DateTimeOrDateType>,
        tz_id: Option<TzId<S>>,
    }

    impl<S> Default for DtParamState<S> {
        fn default() -> Self {
            Self {
                value_type: Default::default(),
                tz_id: Default::default(),
            }
        }
    }

    fn dt_param_step<S: Clone>(
        current_property: PropName<S>,
    ) -> impl FnMut(KnownParam<S>, &mut DtParamState<S>) -> Result<(), DtParamError<S>>
    {
        move |param, state| match param {
            KnownParam::Value(value_type) => match state.value_type {
                Some(_) => Err(DtParamError::DuplicateParam(
                    StaticParamName::Rfc5545(Rfc5545ParamName::ValueDataType),
                )),
                None => {
                    let value_type =
                        DateTimeOrDateType::try_from_value_type(value_type)
                            .map_err(DtParamError::InvalidValueType)?;

                    state.value_type = Some(value_type);
                    Ok(())
                }
            },
            KnownParam::TzId(tz_id) => match state.tz_id {
                Some(_) => {
                    Err(DtParamError::DuplicateParam(StaticParamName::Rfc5545(
                        Rfc5545ParamName::TimeZoneIdentifier,
                    )))
                }
                None => {
                    state.tz_id = Some(tz_id);
                    Ok(())
                }
            },
            unexpected_param => {
                Err(DtParamError::Unexpected(UnexpectedKnownParamError {
                    current_property: current_property.clone(),
                    unexpected_param,
                }))
            }
        }
    }

    let name = property_name.parse_next(input)?;

    Ok(match name {
        PropName::Rfc5545(prop @ Rfc5545PropName::CalendarScale) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let () = gregorian.parse_next(input)?;

            (Prop::Known(KnownProp::CalScale), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Method) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let method = method.parse_next(input)?;

            (Prop::Known(KnownProp::Method(method)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::ProductIdentifier) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let prod_id = text.parse_next(input)?;

            (Prop::Known(KnownProp::ProdId(prod_id)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Version) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let () = v2_0.parse_next(input)?;

            (Prop::Known(KnownProp::Version), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::Attachment) => {
            struct Base64;

            struct State<Src> {
                /// URI by default, otherwise URI or BINARY
                value_type: Option<ValueType<Src>>,
                /// Permitted iff value_type is BINARY
                encoding: Option<Base64>,
                /// OPTIONAL for URI and RECOMMENDED for BINARY
                format_type: Option<FormatType<Src>>,
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), AttachParamError<S>> {
                match param {
                    KnownParam::Value(value_type) => match value_type {
                        ValueType::Binary => match state.value_type {
                            Some(_) => Err(AttachParamError::DuplicateParam(
                                StaticParamName::Rfc5545(
                                    Rfc5545ParamName::ValueDataType,
                                ),
                            )),
                            None => {
                                state.value_type = Some(ValueType::Binary);
                                Ok(())
                            }
                        },
                        _ => Err(AttachParamError::NonBinaryValueType),
                    },
                    KnownParam::Encoding(Encoding::Base64) => {
                        match state.encoding {
                            Some(_) => Err(AttachParamError::DuplicateParam(
                                StaticParamName::Rfc5545(
                                    Rfc5545ParamName::InlineEncoding,
                                ),
                            )),
                            None => {
                                state.encoding = Some(Base64);
                                Ok(())
                            }
                        }
                    }
                    KnownParam::Encoding(Encoding::Bit8) => {
                        Err(AttachParamError::Bit8Encoding)
                    }
                    KnownParam::FormatType(format_type) => {
                        match state.format_type {
                            Some(_) => Err(AttachParamError::DuplicateParam(
                                StaticParamName::Rfc5545(
                                    Rfc5545ParamName::FormatType,
                                ),
                            )),
                            None => {
                                state.format_type = Some(format_type);
                                Ok(())
                            }
                        }
                    }
                    known_param => Err(AttachParamError::Unexpected(
                        UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(
                                Rfc5545PropName::Attachment,
                            ),
                            unexpected_param: known_param,
                        },
                    )),
                }
            }

            let (
                State {
                    value_type,
                    encoding,
                    format_type,
                },
                unknown_params,
            ) = sm_parse_next(
                StateMachine::new(
                    State {
                        value_type: None,
                        encoding: None,
                        format_type: None,
                    },
                    step,
                ),
                input,
            )?;

            if matches!(value_type, Some(ValueType::Binary)) {
                match encoding {
                    Some(Base64) => (),
                    None => {
                        return Err(E::from_external_error(
                            input,
                            AttachParamError::BinaryWithoutEncoding,
                        ));
                    }
                }
            }

            let _ = ':'.parse_next(input)?;

            let value =
                match parse_value(value_type.unwrap_or(ValueType::Uri), input)?
                {
                    Value::Binary(binary) => AttachValue::Binary(binary),
                    Value::Uri(uri) => AttachValue::Uri(uri),
                    _ => unreachable!(),
                };

            let params = AttachParams { format_type };

            (
                Prop::Known(KnownProp::Attach(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Categories) => {
            let step = lang_param_step(PropName::Rfc5545(prop));

            let (LangParamState { language }, unknown_params) = sm_parse_next(
                StateMachine::new(LangParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let categories = separated(1.., text, ',')
                .map(|v: Vec<_>| v.into_boxed_slice())
                .parse_next(input)?;

            let params = LangParams { language };

            (
                Prop::Known(KnownProp::Categories(categories, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Classification) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = class_value.parse_next(input)?;

            (Prop::Known(KnownProp::Class(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Comment) => {
            let step = text_param_step(PropName::Rfc5545(prop));

            let (state, unknown_params) = sm_parse_next(
                StateMachine::new(TextParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = state.into_params();

            (
                Prop::Known(KnownProp::Comment(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Description) => {
            let step = text_param_step(PropName::Rfc5545(prop));

            let (state, unknown_params) = sm_parse_next(
                StateMachine::new(TextParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = state.into_params();

            (
                Prop::Known(KnownProp::Description(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::GeographicPosition) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = geo.parse_next(input)?;

            (Prop::Known(KnownProp::Geo(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Location) => {
            let step = text_param_step(PropName::Rfc5545(prop));

            let (state, unknown_params) = sm_parse_next(
                StateMachine::new(TextParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = state.into_params();

            (
                Prop::Known(KnownProp::Location(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::PercentComplete) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = completion_percentage.parse_next(input)?;

            (
                Prop::Known(KnownProp::PercentComplete(value)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Priority) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = priority.parse_next(input)?;

            (Prop::Known(KnownProp::Priority(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Resources) => {
            let step = text_param_step(PropName::Rfc5545(prop));

            let (state, unknown_params) = sm_parse_next(
                StateMachine::new(TextParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = separated(1.., text, ',')
                .map(|v: Vec<_>| v.into_boxed_slice())
                .parse_next(input)?;

            let params = state.into_params();

            (
                Prop::Known(KnownProp::Resources(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Status) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let status = participation_status.parse_next(input)?;

            (Prop::Known(KnownProp::Status(status)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Summary) => {
            let step = text_param_step(PropName::Rfc5545(prop));

            let (state, unknown_params) = sm_parse_next(
                StateMachine::new(TextParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = state.into_params();

            (
                Prop::Known(KnownProp::Summary(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeCompleted) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let dt = datetime_utc.parse_next(input)?;

            (Prop::Known(KnownProp::DtCompleted(dt)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeEnd) => {
            let step = dt_param_step(PropName::Rfc5545(prop));

            let (DtParamState { value_type, tz_id }, unknown_params) =
                sm_parse_next(
                    StateMachine::new(DtParamState::default(), step),
                    input,
                )?;

            let _ = ':'.parse_next(input)?;

            let value = match value_type.unwrap_or_default() {
                DateTimeOrDateType::DateTime => {
                    datetime.parse_next(input).map(DateTimeOrDate::DateTime)
                }
                DateTimeOrDateType::Date => {
                    date.parse_next(input).map(DateTimeOrDate::Date)
                }
            }?;

            let params = DtParams { tz_id };

            (Prop::Known(KnownProp::DtEnd(value, params)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeDue) => {
            let step = dt_param_step(PropName::Rfc5545(prop));

            let (DtParamState { value_type, tz_id }, unknown_params) =
                sm_parse_next(
                    StateMachine::new(DtParamState::default(), step),
                    input,
                )?;

            let _ = ':'.parse_next(input)?;

            let value = match value_type.unwrap_or_default() {
                DateTimeOrDateType::DateTime => {
                    datetime.parse_next(input).map(DateTimeOrDate::DateTime)
                }
                DateTimeOrDateType::Date => {
                    date.parse_next(input).map(DateTimeOrDate::Date)
                }
            }?;

            let params = DtParams { tz_id };

            (Prop::Known(KnownProp::DtDue(value, params)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeStart) => {
            let step = dt_param_step(PropName::Rfc5545(prop));

            let (DtParamState { value_type, tz_id }, unknown_params) =
                sm_parse_next(
                    StateMachine::new(DtParamState::default(), step),
                    input,
                )?;

            let _ = ':'.parse_next(input)?;

            let value = match value_type.unwrap_or_default() {
                DateTimeOrDateType::DateTime => {
                    datetime.parse_next(input).map(DateTimeOrDate::DateTime)
                }
                DateTimeOrDateType::Date => {
                    date.parse_next(input).map(DateTimeOrDate::Date)
                }
            }?;

            let params = DtParams { tz_id };

            (
                Prop::Known(KnownProp::DtStart(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Duration) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = duration.parse_next(input)?;

            (Prop::Known(KnownProp::Duration(value)), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::FreeBusyTime) => {
            type State<S> = Option<FreeBusyType<S>>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), DuplicateOrUnexpectedError<S>> {
                match param {
                    KnownParam::FBType(free_busy_type) => match state {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(StaticParamName::Rfc5545(
                                Rfc5545ParamName::FreeBusyTimeType,
                            )),
                        )),
                        None => {
                            *state = Some(free_busy_type);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(DuplicateOrUnexpectedError::Unexpected(
                            UnexpectedKnownParamError {
                                current_property: PropName::Rfc5545(
                                    Rfc5545PropName::FreeBusyTime,
                                ),
                                unexpected_param,
                            },
                        ))
                    }
                }
            }

            let (free_busy_type, unknown_params) =
                sm_parse_next(StateMachine::new(None, step), input)?;

            let _ = ':'.parse_next(input)?;

            let value = separated(1.., period, ',')
                .map(Vec::into_boxed_slice)
                .parse_next(input)?;

            let params = FBTypeParams { free_busy_type };

            (
                Prop::Known(KnownProp::FreeBusy(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeTransparency) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = time_transparency.parse_next(input)?;

            (Prop::Known(KnownProp::Transparency(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneIdentifier) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = tz_id.parse_next(input)?;

            (Prop::Known(KnownProp::TzId(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneName) => {
            let step = lang_param_step(PropName::Rfc5545(prop));

            let (LangParamState { language }, unknown_params) = sm_parse_next(
                StateMachine::new(LangParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = LangParams { language };

            (
                Prop::Known(KnownProp::TzName(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneOffsetFrom) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = utc_offset.parse_next(input)?;

            (Prop::Known(KnownProp::TzOffsetFrom(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneOffsetTo) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = utc_offset.parse_next(input)?;

            (Prop::Known(KnownProp::TzOffsetTo(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneUrl) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;

            let _ = ':'.parse_next(input)?;
            let value = uri.parse_next(input)?;

            (Prop::Known(KnownProp::TzUrl(value)), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::Attendee) => {
            type State<S> = AttendeeParams<S>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), DuplicateOrUnexpectedError<S>> {
                let name = param.name();
                match param {
                    KnownParam::CommonName(common_name) => match state
                        .common_name
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.common_name = Some(common_name);
                            Ok(())
                        }
                    },
                    KnownParam::CUType(calendar_user_type) => {
                        match state.calendar_user_type {
                            Some(_) => {
                                Err(DuplicateOrUnexpectedError::Duplicate(
                                    DuplicateParamError(name),
                                ))
                            }
                            None => {
                                state.calendar_user_type =
                                    Some(calendar_user_type);
                                Ok(())
                            }
                        }
                    }
                    KnownParam::DelFrom(delegators) => match state.delegators {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.delegators = Some(delegators);
                            Ok(())
                        }
                    },
                    KnownParam::DelTo(delegatees) => match state.delegatees {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.delegatees = Some(delegatees);
                            Ok(())
                        }
                    },
                    KnownParam::Dir(dir) => match state
                        .directory_entry_reference
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.directory_entry_reference = Some(dir);
                            Ok(())
                        }
                    },
                    KnownParam::Language(language) => match state.language {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.language = Some(language);
                            Ok(())
                        }
                    },
                    KnownParam::Member(uris) => match state
                        .group_or_list_membership
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.group_or_list_membership = Some(uris);
                            Ok(())
                        }
                    },
                    KnownParam::PartStatus(participation_status) => match state
                        .participation_status
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.participation_status =
                                Some(participation_status);
                            Ok(())
                        }
                    },
                    KnownParam::Role(participation_role) => match state
                        .participation_role
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.participation_role = Some(participation_role);
                            Ok(())
                        }
                    },
                    KnownParam::Rsvp(rsvp_expectation) => match state
                        .rsvp_expectation
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.rsvp_expectation = Some(rsvp_expectation);
                            Ok(())
                        }
                    },
                    KnownParam::SentBy(sent_by) => match state.sent_by {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.sent_by = Some(sent_by);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(DuplicateOrUnexpectedError::Unexpected(
                            UnexpectedKnownParamError {
                                current_property: PropName::Rfc5545(
                                    Rfc5545PropName::Attendee,
                                ),
                                unexpected_param,
                            },
                        ))
                    }
                }
            }

            let (params, unknown_params) = sm_parse_next(
                StateMachine::new(
                    AttendeeParams {
                        language: None,
                        calendar_user_type: None,
                        group_or_list_membership: None,
                        participation_role: None,
                        participation_status: None,
                        rsvp_expectation: None,
                        delegatees: None,
                        delegators: None,
                        sent_by: None,
                        common_name: None,
                        directory_entry_reference: None,
                    },
                    step,
                ),
                input,
            )?;

            let _ = ':'.parse_next(input)?;

            // TODO: this value is properly a CAL-ADDRESS (RFC 5545 §3.3.3) and
            // so the type should probably be distinguished from ordinary uris
            let value = uri.parse_next(input)?;

            (
                Prop::Known(KnownProp::Attendee(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Contact) => {
            let step = text_param_step(PropName::Rfc5545(prop));

            let (state, unknown_params) = sm_parse_next(
                StateMachine::new(TextParamState::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = state.into_params();

            (
                Prop::Known(KnownProp::Contact(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(Rfc5545PropName::Organizer) => {
            type State<S> = OrganizerParams<S>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), DuplicateOrUnexpectedError<S>> {
                let name = param.name();
                match param {
                    KnownParam::CommonName(common_name) => match state
                        .common_name
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.common_name = Some(common_name);
                            Ok(())
                        }
                    },
                    KnownParam::Dir(dir) => match state
                        .directory_entry_reference
                    {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.directory_entry_reference = Some(dir);
                            Ok(())
                        }
                    },
                    KnownParam::Language(language) => match state.language {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.language = Some(language);
                            Ok(())
                        }
                    },
                    KnownParam::SentBy(sent_by) => match state.sent_by {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            state.sent_by = Some(sent_by);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(DuplicateOrUnexpectedError::Unexpected(
                            UnexpectedKnownParamError {
                                current_property: PropName::Rfc5545(
                                    Rfc5545PropName::Organizer,
                                ),
                                unexpected_param,
                            },
                        ))
                    }
                }
            }

            let (params, unknown_params) = sm_parse_next(
                StateMachine::new(
                    OrganizerParams {
                        language: None,
                        sent_by: None,
                        common_name: None,
                        directory_entry_reference: None,
                    },
                    step,
                ),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = uri.parse_next(input)?;

            (
                Prop::Known(KnownProp::Organizer(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(Rfc5545PropName::RecurrenceId) => {
            struct State<S> {
                tz_id: Option<TzId<S>>,
                recurrence_identifier_range: Option<ThisAndFuture>,
                value_type: Option<DateTimeOrDateType>,
            }

            impl<S> Default for State<S> {
                fn default() -> Self {
                    Self {
                        tz_id: Default::default(),
                        recurrence_identifier_range: Default::default(),
                        value_type: Default::default(),
                    }
                }
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), DtParamError<S>> {
                let name = param.name();
                match param {
                    KnownParam::RecurrenceIdentifierRange => {
                        match state.recurrence_identifier_range {
                            Some(ThisAndFuture) => {
                                Err(DtParamError::DuplicateParam(name))
                            }
                            None => {
                                state.recurrence_identifier_range =
                                    Some(ThisAndFuture);
                                Ok(())
                            }
                        }
                    }
                    KnownParam::TzId(tz_id) => match state.tz_id {
                        Some(_) => Err(DtParamError::DuplicateParam(name)),
                        None => {
                            state.tz_id = Some(tz_id);
                            Ok(())
                        }
                    },
                    KnownParam::Value(value_type) => match state.value_type {
                        Some(_) => Err(DtParamError::DuplicateParam(name)),
                        None => {
                            let value_type =
                                DateTimeOrDateType::try_from_value_type(
                                    value_type,
                                )
                                .map_err(DtParamError::InvalidValueType)?;

                            state.value_type = Some(value_type);
                            Ok(())
                        }
                    },
                    unexpected_param => Err(DtParamError::Unexpected(
                        UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(
                                Rfc5545PropName::RecurrenceId,
                            ),
                            unexpected_param,
                        },
                    )),
                }
            }

            let (
                State {
                    tz_id,
                    recurrence_identifier_range,
                    value_type,
                },
                unknown_params,
            ) = sm_parse_next(
                StateMachine::new(State::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = match value_type.unwrap_or_default() {
                DateTimeOrDateType::DateTime => {
                    datetime.parse_next(input).map(DateTimeOrDate::DateTime)
                }
                DateTimeOrDateType::Date => {
                    date.parse_next(input).map(DateTimeOrDate::Date)
                }
            }?;

            let params = RecurrenceIdParams {
                tz_id,
                recurrence_identifier_range,
            };

            (
                Prop::Known(KnownProp::RecurrenceId(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(Rfc5545PropName::RelatedTo) => {
            type State<S> = Option<RelationshipType<S>>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), DuplicateOrUnexpectedError<S>> {
                let name = param.name();

                match param {
                    KnownParam::RelType(relationship_type) => match state {
                        Some(_) => Err(DuplicateOrUnexpectedError::Duplicate(
                            DuplicateParamError(name),
                        )),
                        None => {
                            *state = Some(relationship_type);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(DuplicateOrUnexpectedError::Unexpected(
                            UnexpectedKnownParamError {
                                current_property: PropName::Rfc5545(
                                    Rfc5545PropName::RelatedTo,
                                ),
                                unexpected_param,
                            },
                        ))
                    }
                }
            }

            let (relationship_type, unknown_params) = sm_parse_next(
                StateMachine::new(State::default(), step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value = text.parse_next(input)?;
            let params = RelTypeParams { relationship_type };

            (
                Prop::Known(KnownProp::RelatedTo(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::UniformResourceLocator) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;
            let _ = ':'.parse_next(input)?;
            let value = uri.parse_next(input)?;

            (Prop::Known(KnownProp::Url(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::UniqueIdentifier) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;
            let _ = ':'.parse_next(input)?;
            let value = uid.parse_next(input)?;

            (Prop::Known(KnownProp::Uid(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::ExceptionDateTimes) => {
            let step = dt_param_step(PropName::Rfc5545(prop));

            let (DtParamState { value_type, tz_id }, unknown_params) =
                sm_parse_next(
                    StateMachine::new(DtParamState::default(), step),
                    input,
                )?;

            let _ = ':'.parse_next(input)?;
            let value = match value_type.unwrap_or_default() {
                DateTimeOrDateType::DateTime => {
                    datetime.parse_next(input).map(DateTimeOrDate::DateTime)
                }
                DateTimeOrDateType::Date => {
                    date.parse_next(input).map(DateTimeOrDate::Date)
                }
            }?;

            let params = DtParams { tz_id };

            (
                Prop::Known(KnownProp::ExDate(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(Rfc5545PropName::RecurrenceDateTimes) => {
            #[derive(Default)]
            enum RDateType {
                #[default]
                DateTime,
                Date,
                Period,
            }

            impl<S> TryFrom<ValueType<S>> for RDateType {
                type Error = ValueType<S>;

                fn try_from(value: ValueType<S>) -> Result<Self, Self::Error> {
                    match value {
                        ValueType::Date => Ok(Self::Date),
                        ValueType::DateTime => Ok(Self::DateTime),
                        ValueType::Period => Ok(Self::Period),
                        value_type => Err(value_type),
                    }
                }
            }

            struct State<S> {
                value_type: Option<RDateType>,
                tz_id: Option<TzId<S>>,
            }

            impl<S> Default for State<S> {
                fn default() -> Self {
                    Self {
                        value_type: Default::default(),
                        tz_id: Default::default(),
                    }
                }
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), RDateParamError<S>> {
                let name = param.name();

                match param {
                    KnownParam::TzId(tz_id) => match state.tz_id {
                        Some(_) => Err(RDateParamError::DuplicateParam(name)),
                        None => {
                            state.tz_id = Some(tz_id);
                            Ok(())
                        }
                    },
                    KnownParam::Value(value_type) => match state.value_type {
                        Some(_) => Err(RDateParamError::DuplicateParam(name)),
                        None => {
                            let value_type = RDateType::try_from(value_type)
                                .map_err(RDateParamError::InvalidValueType)?;

                            state.value_type = Some(value_type);
                            Ok(())
                        }
                    },
                    unexpected_param => Err(RDateParamError::Unexpected(
                        UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(
                                Rfc5545PropName::RecurrenceDateTimes,
                            ),
                            unexpected_param,
                        },
                    )),
                }
            }

            let (State { value_type, tz_id }, unknown_params) = sm_parse_next(
                StateMachine::new(State::default(), step),
                input,
            )?;

            let params = DtParams { tz_id };
            let _ = ':'.parse_next(input)?;
            let value = match value_type.unwrap_or_default() {
                RDateType::DateTime => {
                    separated(1.., datetime.map(RDate::DateTime), ',')
                        .map(Vec::into_boxed_slice)
                        .parse_next(input)
                }
                RDateType::Date => separated(1.., date.map(RDate::Date), ',')
                    .map(Vec::into_boxed_slice)
                    .parse_next(input),
                RDateType::Period => {
                    separated(1.., period.map(RDate::Period), ',')
                        .map(Vec::into_boxed_slice)
                        .parse_next(input)
                }
            }?;

            (Prop::Known(KnownProp::RDate(value, params)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::RecurrenceRule) => {
            let step = trivial_step(PropName::Rfc5545(prop));

            let ((), unknown_params) =
                sm_parse_next(StateMachine::new((), step), input)?;
            let _ = ':'.parse_next(input)?;

            todo!()
        }
        PropName::Rfc5545(name) => todo!(),
        PropName::Rfc7986(name) => todo!(),
        PropName::Iana(name) => {
            let ((value_type, params), unknown_params) = sm_parse_next(
                StateMachine::new((None, vec![]), unknown_step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value =
                parse_value(value_type.unwrap_or(ValueType::Text), input)?;

            (
                Prop::Unknown(UnknownProp::Iana {
                    name: name.clone(),
                    value,
                    params: params.into_boxed_slice(),
                }),
                unknown_params,
            )
        }
        PropName::X(name) => {
            let ((value_type, params), unknown_params) = sm_parse_next(
                StateMachine::new((None, vec![]), unknown_step),
                input,
            )?;

            let _ = ':'.parse_next(input)?;
            let value =
                parse_value(value_type.unwrap_or(ValueType::Text), input)?;

            (
                Prop::Unknown(UnknownProp::X {
                    name: name.clone(),
                    value,
                    params: params.into_boxed_slice(),
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
    fn rfc_5545_example_uid_property() {
        let input = "UID:19960401T080045Z-4000F192713-0052@example.com";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Uid(uid)) = prop else {
            panic!()
        };

        assert_eq!(uid, Uid("19960401T080045Z-4000F192713-0052@example.com"));
    }

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
                    params,
                }),
                extras,
            )) if params.is_empty() && extras.is_empty(),
        ));

        let mut input = "NON-SMOKING:TRUE";
        let prop = property::<_, ()>(&mut input);
        assert!(matches!(
            prop,
            Ok((
                Prop::Unknown(UnknownProp::Iana {
                    name: "NON-SMOKING",
                    value: Value::Text("TRUE"),
                    params
                }),
                extras,
            )) if params.is_empty() && extras.is_empty(),
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
