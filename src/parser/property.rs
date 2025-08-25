//! Parsers for properties.

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, empty, opt, preceded, repeat, separated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::none_of,
};

use crate::{
    model::{
        css::Css3Color,
        parameter::{KnownParam, Param, Rfc5545ParamName, StaticParamName, UnknownParam},
        primitive::{
            AlarmAction, AttachValue, CalAddress, ClassValue, CompletionPercentage, DateTime,
            DateTimeOrDate, DateTimeOrDateSeq, Duration, Encoding, FormatType, FreeBusyType, Geo,
            ImageData, Integer, Language, Method, Period, Priority, ProximityValue, RDateSeq,
            RelationshipType, RequestStatus, Status, Text, ThisAndFuture, TimeTransparency,
            TriggerRelation, TzId, Uid, Uri, Utc, UtcOffset, Value, ValueType,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams, ImageParams,
            LangParams, OrganizerParams, RecurrenceIdParams, RelTypeParams, TextParams,
            TriggerParams,
        },
        rrule::RRule,
    },
    parser::{
        error::{AttachParamError, TriggerParamError, UnexpectedKnownParamError},
        parameter::parameter,
        primitive::{
            alarm_action, ascii_lower, cal_address, class_value, color, completion_percentage,
            datetime_utc, duration, float, geo, gregorian, iana_token, integer, method, period,
            priority, proximity_value, status, status_code, text, time_transparency, tz_id, uid,
            utc_offset, v2_0, x_name,
        },
        rrule::rrule,
    },
};

use super::{
    error::CalendarParseError,
    primitive::{binary, bool_caseless, date, datetime, time, uri},
};

// NOTE: the IANA iCalendar property registry lists several registered properties
// from RFC 6321 §4.2, RFC 7808 §7, RFC 7953 §3.2, RFC 9073 §6, and RFC 9253 §8
// that have not been included here (they would fall under the Other catch-all).
// perhaps they should be included as static variants at some later point?
// registry: (https://www.iana.org/assignments/icalendar/icalendar.xhtml#properties)

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prop<S> {
    Known(KnownProp<S>),
    Unknown(UnknownProp<S>),
}

impl<S> Prop<S> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Status(Status),
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
    Attendee(CalAddress<S>, AttendeeParams<S>),
    Contact(Text<S>, TextParams<S>),
    Organizer(CalAddress<S>, OrganizerParams<S>),
    RecurrenceId(DateTimeOrDate, RecurrenceIdParams<S>),
    RelatedTo(Text<S>, RelTypeParams<S>),
    Url(Uri<S>),
    Uid(Uid<S>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(DateTimeOrDateSeq, DtParams<S>),
    RDate(RDateSeq, DtParams<S>),
    RRule(RRule),
    // ALARM COMPONENT PROPERTIES
    Action(AlarmAction<S>),
    Repeat(Integer),
    TriggerRelative(Duration, TriggerParams),
    TriggerAbsolute(DateTime<Utc>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(DateTime<Utc>),
    DtStamp(DateTime<Utc>),
    LastModified(DateTime<Utc>),
    Sequence(Integer),
    // MISCELLANEOUS COMPONENT PROPERTIES
    RequestStatus(RequestStatus<S>, LangParams<S>),
    // RFC 7986 PROPERTIES
    Name(Text<S>, TextParams<S>),
    RefreshInterval(Duration),
    Source(Uri<S>),
    Color(Css3Color),
    Image(ImageData<S>, ImageParams<S>),
    Conference(Uri<S>, ConfParams<S>),
    // RFC 9074 PROPERTIES
    Acknowledged(DateTime<Utc>),
    Proximity(ProximityValue<S>),
}

impl<S> KnownProp<S> {
    pub const fn name(&self) -> PropName<S> {
        use PropName::*;
        use Rfc5545PropName as PN5545;
        use Rfc7986PropName as PN7986;
        use Rfc9074PropName as PN9074;

        match self {
            KnownProp::CalScale => Rfc5545(PN5545::CalendarScale),
            KnownProp::Method(..) => Rfc5545(PN5545::Method),
            KnownProp::ProdId(..) => Rfc5545(PN5545::ProductIdentifier),
            KnownProp::Version => Rfc5545(PN5545::Version),
            KnownProp::Attach(..) => Rfc5545(PN5545::Attachment),
            KnownProp::Categories(..) => Rfc5545(PN5545::Categories),
            KnownProp::Class(..) => Rfc5545(PN5545::Classification),
            KnownProp::Comment(..) => Rfc5545(PN5545::Comment),
            KnownProp::Description(..) => Rfc5545(PN5545::Description),
            KnownProp::Geo(..) => Rfc5545(PN5545::GeographicPosition),
            KnownProp::Location(..) => Rfc5545(PN5545::Location),
            KnownProp::PercentComplete(..) => Rfc5545(PN5545::PercentComplete),
            KnownProp::Priority(..) => Rfc5545(PN5545::Priority),
            KnownProp::Resources(..) => Rfc5545(PN5545::Resources),
            KnownProp::Status(..) => Rfc5545(PN5545::Status),
            KnownProp::Summary(..) => Rfc5545(PN5545::Summary),
            KnownProp::DtCompleted(..) => Rfc5545(PN5545::DateTimeCompleted),
            KnownProp::DtEnd(..) => Rfc5545(PN5545::DateTimeEnd),
            KnownProp::DtDue(..) => Rfc5545(PN5545::DateTimeDue),
            KnownProp::DtStart(..) => Rfc5545(PN5545::DateTimeStart),
            KnownProp::Duration(..) => Rfc5545(PN5545::Duration),
            KnownProp::FreeBusy(..) => Rfc5545(PN5545::FreeBusyTime),
            KnownProp::Transparency(..) => Rfc5545(PN5545::TimeTransparency),
            KnownProp::TzId(..) => Rfc5545(PN5545::TimeZoneIdentifier),
            KnownProp::TzName(..) => Rfc5545(PN5545::TimeZoneName),
            KnownProp::TzOffsetFrom(..) => Rfc5545(PN5545::TimeZoneOffsetFrom),
            KnownProp::TzOffsetTo(..) => Rfc5545(PN5545::TimeZoneOffsetTo),
            KnownProp::TzUrl(..) => Rfc5545(PN5545::TimeZoneUrl),
            KnownProp::Attendee(..) => Rfc5545(PN5545::Attendee),
            KnownProp::Contact(..) => Rfc5545(PN5545::Contact),
            KnownProp::Organizer(..) => Rfc5545(PN5545::Organizer),
            KnownProp::RecurrenceId(..) => Rfc5545(PN5545::RecurrenceId),
            KnownProp::RelatedTo(..) => Rfc5545(PN5545::RelatedTo),
            KnownProp::Url(..) => Rfc5545(PN5545::UniformResourceLocator),
            KnownProp::Uid(..) => Rfc5545(PN5545::UniqueIdentifier),
            KnownProp::ExDate(..) => Rfc5545(PN5545::ExceptionDateTimes),
            KnownProp::RDate(..) => Rfc5545(PN5545::RecurrenceDateTimes),
            KnownProp::RRule(..) => Rfc5545(PN5545::RecurrenceRule),
            KnownProp::Action(..) => Rfc5545(PN5545::Action),
            KnownProp::Repeat(..) => Rfc5545(PN5545::RepeatCount),
            KnownProp::TriggerRelative(..) => Rfc5545(PN5545::Trigger),
            KnownProp::TriggerAbsolute(..) => Rfc5545(PN5545::Trigger),
            KnownProp::Created(..) => Rfc5545(PN5545::DateTimeCreated),
            KnownProp::DtStamp(..) => Rfc5545(PN5545::DateTimeStamp),
            KnownProp::LastModified(..) => Rfc5545(PN5545::LastModified),
            KnownProp::Sequence(..) => Rfc5545(PN5545::SequenceNumber),
            KnownProp::RequestStatus(..) => Rfc5545(PN5545::RequestStatus),
            KnownProp::Name(..) => Rfc7986(PN7986::Name),
            KnownProp::RefreshInterval(..) => Rfc7986(PN7986::RefreshInterval),
            KnownProp::Source(..) => Rfc7986(PN7986::Source),
            KnownProp::Color(..) => Rfc7986(PN7986::Color),
            KnownProp::Image(..) => Rfc7986(PN7986::Image),
            KnownProp::Conference(..) => Rfc7986(PN7986::Conference),
            KnownProp::Acknowledged(..) => Rfc9074(PN9074::Acknowledged),
            KnownProp::Proximity(..) => Rfc9074(PN9074::Proximity),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

pub type ParsedProp<S> = (Prop<S>, Box<[UnknownParam<S>]>);

/// Parses a [`Prop`].
pub fn property<I, E>(input: &mut I) -> Result<ParsedProp<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr + Clone + Eq + SliceLen + Stream,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    struct StateMachine<Src, S, F> {
        state: S,
        step: F,
        unknown_params: Vec<UnknownParam<Src>>,
        value_type: Option<ValueType<Src>>,
    }

    impl<Src, S, F> StateMachine<Src, S, F> {
        pub fn new(state: S, step: F) -> Self {
            StateMachine {
                state,
                step,
                unknown_params: Vec::new(),
                value_type: None,
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
    fn sm_parse_next<I, S, F, E>(
        mut sm: StateMachine<I::Slice, S, F>,
        input: &mut I,
    ) -> Result<
        (
            S,
            Option<ValueType<I::Slice>>,
            Box<[UnknownParam<I::Slice>]>,
        ),
        E,
    >
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
        I::Token: AsChar + Clone,
        I::Slice: Clone + SliceLen,
        E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
        F: FnMut(KnownParam<I::Slice>, &mut S) -> Result<(), CalendarParseError<I::Slice>>,
    {
        while input.peek_token().is_some_and(|t| t.as_char() != ':') {
            let param = preceded(';', parameter).parse_next(input)?;

            match param {
                Param::Known(KnownParam::Value(value_type)) => match sm.value_type {
                    Some(_) => {
                        return Err(E::from_external_error(
                            input,
                            CalendarParseError::DuplicateParam(StaticParamName::Rfc5545(
                                Rfc5545ParamName::ValueDataType,
                            )),
                        ));
                    }
                    None => {
                        sm.value_type = Some(value_type);
                    }
                },
                Param::Known(known_param) => {
                    let () = sm
                        .advance(known_param)
                        .map_err(|err| E::from_external_error(input, err))?;
                }
                Param::Unknown(unknown_param) => {
                    sm.unknown_params.push(unknown_param);
                }
            }
        }

        Ok((
            sm.state,
            sm.value_type,
            sm.unknown_params.into_boxed_slice(),
        ))
    }

    /// The step function for unknown (IANA and X) properties.
    fn unknown_step<S>(
        param: KnownParam<S>,
        state: &mut Vec<KnownParam<S>>,
    ) -> Result<(), CalendarParseError<S>> {
        state.push(param);
        Ok(())
    }

    /// Returns the step function for properties with no known parameters.
    fn trivial_step<S: Clone>(
        current_property: PropName<S>,
    ) -> impl FnMut(KnownParam<S>, &mut ()) -> Result<(), CalendarParseError<S>> {
        move |param, &mut ()| {
            Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                current_property: current_property.clone(),
                unexpected_param: param,
            }))
        }
    }

    struct LangParamState<S> {
        language: Option<Language<S>>,
    }

    impl<S> LangParamState<S> {
        fn into_params(self) -> LangParams<S> {
            LangParams {
                language: self.language,
            }
        }
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
    ) -> impl FnMut(KnownParam<S>, &mut LangParamState<S>) -> Result<(), CalendarParseError<S>>
    {
        move |param, state| match param {
            KnownParam::Language(language) => match state.language {
                Some(_) => Err(CalendarParseError::DuplicateParam(
                    StaticParamName::Rfc5545(Rfc5545ParamName::Language),
                )),
                None => {
                    state.language = Some(language);
                    Ok(())
                }
            },
            unexpected_param => Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                current_property: current_property.clone(),
                unexpected_param,
            })),
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
    ) -> impl FnMut(KnownParam<S>, &mut TextParamState<S>) -> Result<(), CalendarParseError<S>>
    {
        move |param, state| match param {
            KnownParam::AltRep(uri) => match state.alt_rep {
                Some(_) => Err(CalendarParseError::DuplicateParam(
                    StaticParamName::Rfc5545(Rfc5545ParamName::AlternateTextRepresentation),
                )),
                None => {
                    state.alt_rep = Some(uri);
                    Ok(())
                }
            },
            KnownParam::Language(language) => match state.language {
                Some(_) => Err(CalendarParseError::DuplicateParam(
                    StaticParamName::Rfc5545(Rfc5545ParamName::Language),
                )),
                None => {
                    state.language = Some(language);
                    Ok(())
                }
            },
            unexpected_param => Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                current_property: current_property.clone(),
                unexpected_param,
            })),
        }
    }

    #[derive(Clone, Copy, Default)]
    enum DateTimeOrDateType {
        #[default]
        DateTime,
        Date,
    }

    impl DateTimeOrDateType {
        fn into_parser<I, E>(self) -> impl Parser<I, DateTimeOrDate, E> + Copy
        where
            I: StreamIsPartial + Stream + Compare<char>,
            I::Token: AsChar + Clone,
            E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
        {
            move |input: &mut I| match self {
                DateTimeOrDateType::DateTime => {
                    datetime.map(DateTimeOrDate::DateTime).parse_next(input)
                }
                DateTimeOrDateType::Date => date.map(DateTimeOrDate::Date).parse_next(input),
            }
        }

        fn into_seq_parser<I, E>(self) -> impl Parser<I, DateTimeOrDateSeq, E> + Copy
        where
            I: StreamIsPartial + Stream + Compare<char>,
            I::Token: AsChar + Clone,
            E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
        {
            move |input: &mut I| match self {
                DateTimeOrDateType::DateTime => separated(1.., datetime, ',')
                    .map(Vec::into_boxed_slice)
                    .map(DateTimeOrDateSeq::DateTime)
                    .parse_next(input),
                DateTimeOrDateType::Date => separated(1.., date, ',')
                    .map(Vec::into_boxed_slice)
                    .map(DateTimeOrDateSeq::Date)
                    .parse_next(input),
            }
        }
    }

    struct DtParamState<S> {
        tz_id: Option<TzId<S>>,
    }

    impl<S> Default for DtParamState<S> {
        fn default() -> Self {
            Self {
                tz_id: Default::default(),
            }
        }
    }

    impl<S> DtParamState<S> {
        fn into_params(self) -> DtParams<S> {
            DtParams { tz_id: self.tz_id }
        }
    }

    fn dt_param_step<S: Clone>(
        current_property: PropName<S>,
    ) -> impl FnMut(KnownParam<S>, &mut DtParamState<S>) -> Result<(), CalendarParseError<S>> {
        move |param, state| match param {
            KnownParam::TzId(tz_id) => match state.tz_id {
                Some(_) => Err(CalendarParseError::DuplicateParam(
                    StaticParamName::Rfc5545(Rfc5545ParamName::TimeZoneIdentifier),
                )),
                None => {
                    state.tz_id = Some(tz_id);
                    Ok(())
                }
            },
            unexpected_param => Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                current_property: current_property.clone(),
                unexpected_param,
            })),
        }
    }

    #[derive(Default, PartialEq, Eq)]
    enum UriOrBinary {
        #[default]
        Uri,
        Binary,
    }

    #[allow(clippy::type_complexity)]
    fn parse_property<I, E, T, S, V>(
        input: &mut I,
        step_fn: impl FnMut(KnownParam<I::Slice>, &mut S) -> Result<(), CalendarParseError<I::Slice>>,
        value_type_check: impl FnOnce(
            Option<ValueType<I::Slice>>,
        ) -> Result<V, CalendarParseError<I::Slice>>,
        value_parser: impl FnOnce(&mut I) -> Result<T, E>,
    ) -> Result<(T, V, S, Box<[UnknownParam<I::Slice>]>), E>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
        I::Token: AsChar + Clone,
        I::Slice: Clone + SliceLen,
        E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
        S: Default,
    {
        let (params, value_type, unknown_params) =
            sm_parse_next(StateMachine::new(S::default(), step_fn), input)?;
        let _ = ':'.parse_next(input)?;

        let value_type =
            value_type_check(value_type).map_err(|err| E::from_external_error(input, err))?;
        let value = value_parser(input)?;
        Ok((value, value_type, params, unknown_params))
    }

    #[allow(clippy::type_complexity)]
    fn only<I>(
        value_type: ValueType<I::Slice>,
    ) -> impl FnOnce(Option<ValueType<I::Slice>>) -> Result<(), CalendarParseError<I::Slice>>
    where
        I: Stream,
        I::Slice: Eq,
    {
        move |v| {
            if v.is_none_or(|v| v == value_type) {
                Ok(())
            } else {
                Err(CalendarParseError::UnexpectedValueType)
            }
        }
    }

    #[allow(clippy::type_complexity)]
    fn once<I>(
        value_type: ValueType<I::Slice>,
    ) -> impl FnOnce(Option<ValueType<I::Slice>>) -> Result<(), CalendarParseError<I::Slice>>
    where
        I: Stream,
        I::Slice: Eq,
    {
        move |v| {
            if v.is_some_and(|v| v == value_type) {
                Ok(())
            } else {
                Err(CalendarParseError::UnexpectedValueType)
            }
        }
    }

    fn datetime_or_date_type<I>(
        value_type: Option<ValueType<I::Slice>>,
    ) -> Result<DateTimeOrDateType, CalendarParseError<I::Slice>>
    where
        I: Stream,
    {
        match value_type {
            None | Some(ValueType::DateTime) => Ok(DateTimeOrDateType::DateTime),
            Some(ValueType::Date) => Ok(DateTimeOrDateType::Date),
            _ => Err(CalendarParseError::UnexpectedValueType),
        }
    }

    let name = property_name.parse_next(input)?;

    Ok(match name {
        PropName::Rfc5545(prop @ Rfc5545PropName::CalendarScale) => {
            let ((), (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                gregorian,
            )?;

            (Prop::Known(KnownProp::CalScale), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Method) => {
            let (method, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                method,
            )?;

            (Prop::Known(KnownProp::Method(method)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::ProductIdentifier) => {
            let (prod_id, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (Prop::Known(KnownProp::ProdId(prod_id)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Version) => {
            let ((), (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                v2_0,
            )?;

            (Prop::Known(KnownProp::Version), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::Attachment) => {
            struct Base64;

            struct State<S> {
                /// Permitted iff value_type is BINARY
                encoding: Option<Base64>,
                /// OPTIONAL for URI and RECOMMENDED for BINARY
                format_type: Option<FormatType<S>>,
            }

            impl<S> Default for State<S> {
                fn default() -> Self {
                    Self {
                        encoding: Default::default(),
                        format_type: Default::default(),
                    }
                }
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                match param {
                    KnownParam::Encoding(Encoding::Base64) => match state.encoding {
                        Some(_) => Err(CalendarParseError::DuplicateParam(
                            StaticParamName::Rfc5545(Rfc5545ParamName::InlineEncoding),
                        )),
                        None => {
                            state.encoding = Some(Base64);
                            Ok(())
                        }
                    },
                    KnownParam::Encoding(Encoding::Bit8) => Err(CalendarParseError::AttachParam(
                        AttachParamError::Bit8Encoding,
                    )),
                    KnownParam::FormatType(format_type) => match state.format_type {
                        Some(_) => Err(CalendarParseError::DuplicateParam(
                            StaticParamName::Rfc5545(Rfc5545ParamName::FormatType),
                        )),
                        None => {
                            state.format_type = Some(format_type);
                            Ok(())
                        }
                    },
                    known_param => Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                        current_property: PropName::Rfc5545(Rfc5545PropName::Attachment),
                        unexpected_param: known_param,
                    })),
                }
            }

            let (
                (),
                value_type,
                State {
                    encoding,
                    format_type,
                },
                unknown_params,
            ) = parse_property(
                input,
                step,
                |v| match v {
                    None | Some(ValueType::Uri) => Ok(UriOrBinary::Uri),
                    Some(ValueType::Binary) => Ok(UriOrBinary::Binary),
                    _ => Err(CalendarParseError::UnexpectedValueType),
                },
                empty,
            )?;

            if value_type == UriOrBinary::Binary {
                match encoding {
                    Some(Base64) => (),
                    None => {
                        return Err(E::from_external_error(
                            input,
                            CalendarParseError::AttachParam(
                                AttachParamError::BinaryWithoutEncoding,
                            ),
                        ));
                    }
                }
            }

            let params = AttachParams { format_type };
            let value = match value_type {
                UriOrBinary::Uri => uri::<_, _, false>.map(AttachValue::Uri).parse_next(input)?,
                UriOrBinary::Binary => binary.map(AttachValue::Binary).parse_next(input)?,
            };

            (
                Prop::Known(KnownProp::Attach(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Categories) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                lang_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                |i| {
                    separated(1.., text, ',')
                        .map(|v: Vec<_>| v.into_boxed_slice())
                        .parse_next(i)
                },
            )?;

            (
                Prop::Known(KnownProp::Categories(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Classification) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                class_value,
            )?;

            (Prop::Known(KnownProp::Class(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Comment) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::Comment(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Description) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::Description(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::GeographicPosition) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Float),
                geo,
            )?;

            (Prop::Known(KnownProp::Geo(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Location) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::Location(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::PercentComplete) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Integer),
                completion_percentage,
            )?;

            (
                Prop::Known(KnownProp::PercentComplete(value)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Priority) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Integer),
                priority,
            )?;

            (Prop::Known(KnownProp::Priority(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Resources) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                |i| {
                    separated(1.., text, ',')
                        .map(|v: Vec<_>| v.into_boxed_slice())
                        .parse_next(i)
                },
            )?;

            (
                Prop::Known(KnownProp::Resources(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Status) => {
            let (status, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                status,
            )?;

            (Prop::Known(KnownProp::Status(status)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Summary) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::Summary(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeCompleted) => {
            let (dt, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::DateTime),
                datetime_utc,
            )?;

            (Prop::Known(KnownProp::DtCompleted(dt)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeEnd) => {
            let ((), value_type, state, unknown_params) = parse_property(
                input,
                dt_param_step(PropName::Rfc5545(prop)),
                datetime_or_date_type::<I>,
                empty,
            )?;

            let value = value_type.into_parser().parse_next(input)?;
            let params = state.into_params();
            (Prop::Known(KnownProp::DtEnd(value, params)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeDue) => {
            let ((), value_type, state, unknown_params) = parse_property(
                input,
                dt_param_step(PropName::Rfc5545(prop)),
                datetime_or_date_type::<I>,
                empty,
            )?;

            let value = value_type.into_parser().parse_next(input)?;
            let params = state.into_params();
            (Prop::Known(KnownProp::DtDue(value, params)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeStart) => {
            let ((), value_type, state, unknown_params) = parse_property(
                input,
                dt_param_step(PropName::Rfc5545(prop)),
                datetime_or_date_type::<I>,
                empty,
            )?;

            let value = value_type.into_parser().parse_next(input)?;
            let params = state.into_params();

            (
                Prop::Known(KnownProp::DtStart(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Duration) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Duration),
                duration,
            )?;

            (Prop::Known(KnownProp::Duration(value)), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::FreeBusyTime) => {
            type State<S> = Option<FreeBusyType<S>>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                match param {
                    KnownParam::FBType(free_busy_type) => match state {
                        Some(_) => Err(CalendarParseError::DuplicateParam(
                            StaticParamName::Rfc5545(Rfc5545ParamName::FreeBusyTimeType),
                        )),
                        None => {
                            *state = Some(free_busy_type);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(Rfc5545PropName::FreeBusyTime),
                            unexpected_param,
                        }))
                    }
                }
            }

            let (value, (), free_busy_type, unknown_params) =
                parse_property(input, step, only::<I>(ValueType::Period), |i| {
                    separated(1.., period, ',')
                        .map(Vec::into_boxed_slice)
                        .parse_next(i)
                })?;

            let params = FBTypeParams { free_busy_type };

            (
                Prop::Known(KnownProp::FreeBusy(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeTransparency) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                time_transparency,
            )?;

            (Prop::Known(KnownProp::Transparency(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneIdentifier) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                tz_id,
            )?;

            (Prop::Known(KnownProp::TzId(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneName) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                lang_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::TzName(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneOffsetFrom) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::UtcOffset),
                utc_offset,
            )?;

            (Prop::Known(KnownProp::TzOffsetFrom(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneOffsetTo) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::UtcOffset),
                utc_offset,
            )?;

            (Prop::Known(KnownProp::TzOffsetTo(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::TimeZoneUrl) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Uri),
                uri::<_, _, false>,
            )?;

            (Prop::Known(KnownProp::TzUrl(value)), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::Attendee) => {
            type State<S> = AttendeeParams<S>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();
                match param {
                    KnownParam::CommonName(common_name) => match state.common_name {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.common_name = Some(common_name);
                            Ok(())
                        }
                    },
                    KnownParam::CUType(calendar_user_type) => match state.calendar_user_type {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.calendar_user_type = Some(calendar_user_type);
                            Ok(())
                        }
                    },
                    KnownParam::DelFrom(delegators) => match state.delegators {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.delegators = Some(delegators);
                            Ok(())
                        }
                    },
                    KnownParam::DelTo(delegatees) => match state.delegatees {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.delegatees = Some(delegatees);
                            Ok(())
                        }
                    },
                    KnownParam::Dir(dir) => match state.directory_entry_reference {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.directory_entry_reference = Some(dir);
                            Ok(())
                        }
                    },
                    KnownParam::Language(language) => match state.language {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.language = Some(language);
                            Ok(())
                        }
                    },
                    KnownParam::Member(uris) => match state.group_or_list_membership {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.group_or_list_membership = Some(uris);
                            Ok(())
                        }
                    },
                    KnownParam::PartStatus(participation_status) => {
                        match state.participation_status {
                            Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                            None => {
                                state.participation_status = Some(participation_status);
                                Ok(())
                            }
                        }
                    }
                    KnownParam::Role(participation_role) => match state.participation_role {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.participation_role = Some(participation_role);
                            Ok(())
                        }
                    },
                    KnownParam::Rsvp(rsvp_expectation) => match state.rsvp_expectation {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.rsvp_expectation = Some(rsvp_expectation);
                            Ok(())
                        }
                    },
                    KnownParam::SentBy(sent_by) => match state.sent_by {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.sent_by = Some(sent_by);
                            Ok(())
                        }
                    },
                    KnownParam::Email(email) => match state.email {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.email = Some(email);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(Rfc5545PropName::Attendee),
                            unexpected_param,
                        }))
                    }
                }
            }

            let (value, (), params, unknown_params) = parse_property(
                input,
                step,
                only::<I>(ValueType::CalAddress),
                cal_address::<_, _, false>,
            )?;

            (
                Prop::Known(KnownProp::Attendee(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Contact) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::Contact(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(Rfc5545PropName::Organizer) => {
            type State<S> = OrganizerParams<S>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();
                match param {
                    KnownParam::CommonName(common_name) => match state.common_name {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.common_name = Some(common_name);
                            Ok(())
                        }
                    },
                    KnownParam::Dir(dir) => match state.directory_entry_reference {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.directory_entry_reference = Some(dir);
                            Ok(())
                        }
                    },
                    KnownParam::Language(language) => match state.language {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.language = Some(language);
                            Ok(())
                        }
                    },
                    KnownParam::SentBy(sent_by) => match state.sent_by {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.sent_by = Some(sent_by);
                            Ok(())
                        }
                    },
                    KnownParam::Email(email) => match state.email {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.email = Some(email);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(Rfc5545PropName::Organizer),
                            unexpected_param,
                        }))
                    }
                }
            }

            let (value, (), params, unknown_params) = parse_property(
                input,
                step,
                only::<I>(ValueType::CalAddress),
                cal_address::<_, _, false>,
            )?;

            (
                Prop::Known(KnownProp::Organizer(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(Rfc5545PropName::RecurrenceId) => {
            struct State<S> {
                tz_id: Option<TzId<S>>,
                recurrence_identifier_range: Option<ThisAndFuture>,
            }

            impl<S> Default for State<S> {
                fn default() -> Self {
                    Self {
                        tz_id: Default::default(),
                        recurrence_identifier_range: Default::default(),
                    }
                }
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();
                match param {
                    KnownParam::RecurrenceIdentifierRange => {
                        match state.recurrence_identifier_range {
                            Some(ThisAndFuture) => Err(CalendarParseError::DuplicateParam(name)),
                            None => {
                                state.recurrence_identifier_range = Some(ThisAndFuture);
                                Ok(())
                            }
                        }
                    }
                    KnownParam::TzId(tz_id) => match state.tz_id {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.tz_id = Some(tz_id);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
                            unexpected_param,
                        }))
                    }
                }
            }

            let ((), value_type, state, unknown_params) =
                parse_property(input, step, datetime_or_date_type::<I>, empty)?;

            let value = value_type.into_parser().parse_next(input)?;
            let params = RecurrenceIdParams {
                tz_id: state.tz_id,
                recurrence_identifier_range: state.recurrence_identifier_range,
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
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();

                match param {
                    KnownParam::RelType(relationship_type) => match state {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            *state = Some(relationship_type);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(Rfc5545PropName::RelatedTo),
                            unexpected_param,
                        }))
                    }
                }
            }

            let (value, (), relationship_type, unknown_params) =
                parse_property(input, step, only::<I>(ValueType::Text), text)?;
            let params = RelTypeParams { relationship_type };

            (
                Prop::Known(KnownProp::RelatedTo(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::UniformResourceLocator) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Uri),
                uri::<_, _, false>,
            )?;

            (Prop::Known(KnownProp::Url(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::UniqueIdentifier) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                uid,
            )?;

            (Prop::Known(KnownProp::Uid(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::ExceptionDateTimes) => {
            let ((), value_type, state, unknown_params) = parse_property(
                input,
                dt_param_step(PropName::Rfc5545(prop)),
                datetime_or_date_type::<I>,
                empty,
            )?;

            let value = value_type.into_seq_parser().parse_next(input)?;

            (
                Prop::Known(KnownProp::ExDate(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::RecurrenceDateTimes) => {
            enum RDateType {
                DateTime,
                Date,
                Period,
            }

            let ((), value_type, state, unknown_params) = parse_property(
                input,
                dt_param_step(PropName::Rfc5545(prop)),
                |v| match v {
                    None | Some(ValueType::DateTime) => Ok(RDateType::DateTime),
                    Some(ValueType::Date) => Ok(RDateType::Date),
                    Some(ValueType::Period) => Ok(RDateType::Period),
                    _ => Err(CalendarParseError::UnexpectedValueType),
                },
                empty,
            )?;

            let value = match value_type {
                RDateType::DateTime => separated(1.., datetime, ',')
                    .map(Vec::into_boxed_slice)
                    .map(RDateSeq::DateTime)
                    .parse_next(input),
                RDateType::Date => separated(1.., date, ',')
                    .map(Vec::into_boxed_slice)
                    .map(RDateSeq::Date)
                    .parse_next(input),
                RDateType::Period => separated(1.., period, ',')
                    .map(Vec::into_boxed_slice)
                    .map(RDateSeq::Period)
                    .parse_next(input),
            }?;

            (
                Prop::Known(KnownProp::RDate(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::RecurrenceRule) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Recur),
                rrule,
            )?;

            (Prop::Known(KnownProp::RRule(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::Action) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                alarm_action,
            )?;

            (Prop::Known(KnownProp::Action(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::RepeatCount) => {
            // NOTE: should we restrict this to being positive? the standard
            // (RFC 5545 §3.8.6.2) doesn't explicitly provide for this, but it
            // seems semantically incoherent to have a negative number of
            // repetitions.

            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Integer),
                integer,
            )?;

            (Prop::Known(KnownProp::Repeat(value)), unknown_params)
        }
        PropName::Rfc5545(Rfc5545PropName::Trigger) => {
            enum TriggerType {
                Duration,
                DateTime,
            }

            #[derive(Default)]
            struct State {
                trigger_relation: Option<TriggerRelation>,
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State,
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();

                match param {
                    KnownParam::AlarmTrigger(trigger_relation) => match state.trigger_relation {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.trigger_relation = Some(trigger_relation);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc5545(Rfc5545PropName::Trigger),
                            unexpected_param,
                        }))
                    }
                }
            }

            let ((), value_type, State { trigger_relation }, unknown_params) = parse_property(
                input,
                step,
                |v| match v {
                    None | Some(ValueType::Duration) => Ok(TriggerType::Duration),
                    Some(ValueType::DateTime) => Ok(TriggerType::DateTime),
                    _ => Err(CalendarParseError::UnexpectedValueType),
                },
                empty,
            )?;

            let prop = match value_type {
                TriggerType::DateTime if trigger_relation.is_some() => Err(E::from_external_error(
                    input,
                    CalendarParseError::TriggerParam(TriggerParamError::DateTimeWithRelation),
                )),
                TriggerType::DateTime => {
                    let value = datetime_utc.parse_next(input)?;
                    Ok(Prop::Known(KnownProp::TriggerAbsolute(value)))
                }
                TriggerType::Duration => {
                    let value = duration.parse_next(input)?;
                    let params = TriggerParams { trigger_relation };
                    Ok(Prop::Known(KnownProp::TriggerRelative(value, params)))
                }
            }?;

            (prop, unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeCreated) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::DateTime),
                datetime_utc,
            )?;

            (Prop::Known(KnownProp::Created(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::DateTimeStamp) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::DateTime),
                datetime_utc,
            )?;

            (Prop::Known(KnownProp::DtStamp(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::LastModified) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::DateTime),
                datetime_utc,
            )?;

            (Prop::Known(KnownProp::LastModified(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::SequenceNumber) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Integer),
                integer,
            )?;

            (Prop::Known(KnownProp::Sequence(value)), unknown_params)
        }
        PropName::Rfc5545(prop @ Rfc5545PropName::RequestStatus) => {
            let ((), (), state, unknown_params) = parse_property(
                input,
                lang_param_step(PropName::Rfc5545(prop)),
                only::<I>(ValueType::Text),
                empty,
            )?;

            let status = RequestStatus {
                code: status_code.parse_next(input)?,
                description: preceded(';', text).parse_next(input)?,
                exception_data: opt(preceded(';', text)).parse_next(input)?,
            };

            let params = state.into_params();

            (
                Prop::Known(KnownProp::RequestStatus(status, params)),
                unknown_params,
            )
        }
        PropName::Rfc7986(prop @ Rfc7986PropName::Name) => {
            let (value, (), state, unknown_params) = parse_property(
                input,
                text_param_step(PropName::Rfc7986(prop)),
                only::<I>(ValueType::Text),
                text,
            )?;

            (
                Prop::Known(KnownProp::Name(value, state.into_params())),
                unknown_params,
            )
        }
        PropName::Rfc7986(prop @ Rfc7986PropName::RefreshInterval) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc7986(prop)),
                once::<I>(ValueType::Duration),
                duration,
            )?;

            (
                Prop::Known(KnownProp::RefreshInterval(value)),
                unknown_params,
            )
        }
        PropName::Rfc7986(prop @ Rfc7986PropName::Source) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc7986(prop)),
                once::<I>(ValueType::Uri),
                uri::<_, _, false>,
            )?;

            (Prop::Known(KnownProp::Source(value)), unknown_params)
        }
        PropName::Rfc7986(prop @ Rfc7986PropName::Color) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc7986(prop)),
                only::<I>(ValueType::Text),
                color,
            )?;

            (Prop::Known(KnownProp::Color(value)), unknown_params)
        }
        PropName::Rfc7986(Rfc7986PropName::Image) => {
            struct State<S> {
                params: ImageParams<S>,
                encoding_found: bool,
            }

            impl<S> Default for State<S> {
                fn default() -> Self {
                    Self {
                        params: Default::default(),
                        encoding_found: Default::default(),
                    }
                }
            }

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();

                match param {
                    KnownParam::FormatType(format_type) => match state.params.format_type {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.params.format_type = Some(format_type);
                            Ok(())
                        }
                    },
                    KnownParam::AltRep(alternate_representation) => {
                        match state.params.alternate_representation {
                            Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                            None => {
                                state.params.alternate_representation =
                                    Some(alternate_representation);
                                Ok(())
                            }
                        }
                    }
                    KnownParam::Display(display) => match state.params.display {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.params.display = Some(display);
                            Ok(())
                        }
                    },
                    KnownParam::Encoding(Encoding::Base64) => match state.encoding_found {
                        true => Err(CalendarParseError::DuplicateParam(name)),
                        false => {
                            state.encoding_found = true;
                            Ok(())
                        }
                    },
                    KnownParam::Encoding(Encoding::Bit8) => Err(CalendarParseError::AttachParam(
                        AttachParamError::Bit8Encoding,
                    )),
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc7986(Rfc7986PropName::Image),
                            unexpected_param,
                        }))
                    }
                }
            }

            let ((), value_type, state, unknown_params) = parse_property(
                input,
                step,
                |v| match v {
                    Some(ValueType::Uri) => Ok(UriOrBinary::Uri),
                    Some(ValueType::Binary) => Ok(UriOrBinary::Binary),
                    Some(_) | None => Err(CalendarParseError::UnexpectedValueType),
                },
                empty,
            )?;

            let value = match (state.encoding_found, value_type) {
                (false, UriOrBinary::Uri) => {
                    uri::<_, _, false>.map(ImageData::Uri).parse_next(input)
                }
                (true, UriOrBinary::Binary) => binary.map(ImageData::Binary).parse_next(input),
                (true, UriOrBinary::Uri) => Err(E::from_external_error(
                    input,
                    CalendarParseError::AttachParam(AttachParamError::EncodingOnUri),
                )),
                (false, UriOrBinary::Binary) => Err(E::from_external_error(
                    input,
                    CalendarParseError::AttachParam(AttachParamError::BinaryWithoutEncoding),
                )),
            }?;

            (
                Prop::Known(KnownProp::Image(value, state.params)),
                unknown_params,
            )
        }
        PropName::Rfc7986(Rfc7986PropName::Conference) => {
            type State<S> = ConfParams<S>;

            fn step<S>(
                param: KnownParam<S>,
                state: &mut State<S>,
            ) -> Result<(), CalendarParseError<S>> {
                let name = param.name();

                match param {
                    KnownParam::Feature(feature_type) => match state.feature_type {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.feature_type = Some(feature_type);
                            Ok(())
                        }
                    },
                    KnownParam::Language(language) => match state.language {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.language = Some(language);
                            Ok(())
                        }
                    },
                    KnownParam::Label(label) => match state.label {
                        Some(_) => Err(CalendarParseError::DuplicateParam(name)),
                        None => {
                            state.label = Some(label);
                            Ok(())
                        }
                    },
                    unexpected_param => {
                        Err(CalendarParseError::Unexpected(UnexpectedKnownParamError {
                            current_property: PropName::Rfc7986(Rfc7986PropName::Conference),
                            unexpected_param,
                        }))
                    }
                }
            }

            let (value, (), params, unknown_params) =
                parse_property(input, step, once::<I>(ValueType::Uri), uri::<_, _, false>)?;

            (
                Prop::Known(KnownProp::Conference(value, params)),
                unknown_params,
            )
        }
        PropName::Rfc9074(prop @ Rfc9074PropName::Acknowledged) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc9074(prop)),
                only::<I>(ValueType::DateTime),
                datetime_utc,
            )?;

            (Prop::Known(KnownProp::Acknowledged(value)), unknown_params)
        }
        PropName::Rfc9074(prop @ Rfc9074PropName::Proximity) => {
            let (value, (), (), unknown_params) = parse_property(
                input,
                trivial_step(PropName::Rfc9074(prop)),
                only::<I>(ValueType::Text),
                proximity_value,
            )?;

            (Prop::Known(KnownProp::Proximity(value)), unknown_params)
        }
        PropName::Iana(name) => {
            let ((), value_type, params, unknown_params) = parse_property(
                input,
                unknown_step,
                |v| Ok(v.unwrap_or(ValueType::Text)),
                empty,
            )?;

            (
                Prop::Unknown(UnknownProp::Iana {
                    value: parse_value(value_type, input)?,
                    name: name.clone(),
                    params: params.into_boxed_slice(),
                }),
                unknown_params,
            )
        }
        PropName::X(name) => {
            let ((), value_type, params, unknown_params) = parse_property(
                input,
                unknown_step,
                |v| Ok(v.unwrap_or(ValueType::Text)),
                empty,
            )?;

            (
                Prop::Unknown(UnknownProp::X {
                    value: parse_value(value_type, input)?,
                    name: name.clone(),
                    params: params.into_boxed_slice(),
                }),
                unknown_params,
            )
        }
    })
}

/// A property name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropName<S> {
    Rfc5545(Rfc5545PropName),
    Rfc7986(Rfc7986PropName),
    Rfc9074(Rfc9074PropName),
    Iana(S),
    X(S),
}

/// Parses a [`PropName`].
pub fn property_name<I, E>(input: &mut I) -> Result<PropName<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
    PropName<I::Slice>: Clone,
{
    use PropName::*;
    use Rfc5545PropName as PN5545;
    use Rfc7986PropName as PN7986;
    use Rfc9074PropName as PN9074;

    let checkpoint = input.checkpoint();

    let abandon = |input: &mut I| {
        input.reset(&checkpoint);
        alt((x_name.map(X), iana_token.map(Iana))).parse_next(input)
    };

    fn static_name<I>(input: &mut I) -> Result<PropName<I::Slice>, ()>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
        I::Token: AsChar + Clone,
        PropName<I::Slice>: Clone,
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
                    't' => tail!("ion", Rfc5545(PN5545::Action)),
                    'k' => tail!("nowledged", Rfc9074(PN9074::Acknowledged)),
                    _ => Err(()),
                },
                't' => match preceded(Caseless("t"), ascii_lower).parse_next(input)? {
                    'a' => tail!("ch", Rfc5545(PN5545::Attachment)),
                    'e' => tail!("ndee", Rfc5545(PN5545::Attendee)),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'c' => match ascii_lower.parse_next(input)? {
                'a' => match ascii_lower.parse_next(input)? {
                    'l' => tail!("scale", Rfc5545(PN5545::CalendarScale)),
                    't' => tail!("egories", Rfc5545(PN5545::Categories)),
                    _ => Err(()),
                },
                'l' => tail!("ass", Rfc5545(PN5545::Classification)),
                'o' => match ascii_lower.parse_next(input)? {
                    'l' => tail!("or", Rfc7986(PN7986::Color)),
                    'm' => match ascii_lower.parse_next(input)? {
                        'm' => tail!("ent", Rfc5545(PN5545::Comment)),
                        'p' => {
                            tail!("leted", Rfc5545(PN5545::DateTimeCompleted))
                        }
                        _ => Err(()),
                    },
                    'n' => match ascii_lower.parse_next(input)? {
                        'f' => tail!("erence", Rfc7986(PN7986::Conference)),
                        't' => tail!("act", Rfc5545(PN5545::Contact)),
                        _ => Err(()),
                    },
                    _ => Err(()),
                },
                'r' => tail!("eated", Rfc5545(PN5545::DateTimeCreated)),
                _ => Err(()),
            },
            'd' => match ascii_lower.parse_next(input)? {
                'e' => tail!("scription", Rfc5545(PN5545::Description)),
                't' => match ascii_lower.parse_next(input)? {
                    'e' => tail!("nd", Rfc5545(PN5545::DateTimeEnd)),
                    's' => match preceded(Caseless("ta"), ascii_lower).parse_next(input)? {
                        'm' => tail!("p", Rfc5545(PN5545::DateTimeStamp)),
                        'r' => tail!("t", Rfc5545(PN5545::DateTimeStart)),
                        _ => Err(()),
                    },
                    _ => Err(()),
                },
                'u' => match ascii_lower.parse_next(input)? {
                    'e' => tail!("", Rfc5545(PN5545::DateTimeDue)),
                    'r' => tail!("ation", Rfc5545(PN5545::Duration)),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'e' => tail!("xdate", Rfc5545(PN5545::ExceptionDateTimes)),
            'f' => tail!("reebusy", Rfc5545(PN5545::FreeBusyTime)),
            'g' => tail!("eo", Rfc5545(PN5545::GeographicPosition)),
            'i' => tail!("mage", Rfc7986(PN7986::Image)),
            'l' => match ascii_lower.parse_next(input)? {
                'a' => tail!("st-modified", Rfc5545(PN5545::LastModified)),
                'o' => tail!("cation", Rfc5545(PN5545::Location)),
                _ => Err(()),
            },
            'm' => tail!("ethod", Rfc5545(PN5545::Method)),
            'n' => tail!("ame", Rfc7986(PN7986::Name)),
            'o' => tail!("rganizer", Rfc5545(PN5545::Organizer)),
            'p' => match ascii_lower.parse_next(input)? {
                'e' => {
                    tail!("rcent-complete", Rfc5545(PN5545::PercentComplete))
                }
                // PRIORITY | PRODID | PROXIMITY
                'r' => match ascii_lower.parse_next(input)? {
                    'i' => tail!("ority", Rfc5545(PN5545::Priority)),
                    'o' => match ascii_lower.parse_next(input)? {
                        'd' => tail!("id", Rfc5545(PN5545::ProductIdentifier)),
                        'x' => tail!("imity", Rfc9074(PN9074::Proximity)),
                        _ => Err(()),
                    },
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'r' => match ascii_lower.parse_next(input)? {
                'e' => match ascii_lower.parse_next(input)? {
                    'c' => tail!("urrence-id", Rfc5545(PN5545::RecurrenceId)),
                    'f' => {
                        tail!("resh-interval", Rfc7986(PN7986::RefreshInterval))
                    }
                    'l' => tail!("ated-to", Rfc5545(PN5545::RelatedTo)),
                    'p' => tail!("eat", Rfc5545(PN5545::RepeatCount)),
                    'q' => tail!("uest-status", Rfc5545(PN5545::RequestStatus)),
                    's' => tail!("ources", Rfc5545(PN5545::Resources)),
                    _ => Err(()),
                },
                'd' => tail!("ate", Rfc5545(PN5545::RecurrenceDateTimes)),
                'r' => tail!("ule", Rfc5545(PN5545::RecurrenceRule)),
                _ => Err(()),
            },
            's' => match ascii_lower.parse_next(input)? {
                'e' => tail!("quence", Rfc5545(PN5545::SequenceNumber)),
                'o' => tail!("urce", Rfc7986(PN7986::Source)),
                't' => tail!("atus", Rfc5545(PN5545::Status)),
                'u' => tail!("mmary", Rfc5545(PN5545::Summary)),
                _ => Err(()),
            },
            't' => match ascii_lower.parse_next(input)? {
                // TRIGGER | TRANSP
                'r' => match ascii_lower.parse_next(input)? {
                    'a' => tail!("nsp", Rfc5545(PN5545::TimeTransparency)),
                    'i' => tail!("gger", Rfc5545(PN5545::Trigger)),
                    _ => Err(()),
                },
                // TZOFFSETFROM | TZOFFSETTO | TZNAME | TZURL | TZID
                'z' => match ascii_lower.parse_next(input)? {
                    'i' => tail!("d", Rfc5545(PN5545::TimeZoneIdentifier)),
                    'n' => tail!("ame", Rfc5545(PN5545::TimeZoneName)),
                    // TZOFFSETFROM | TZOFFSETTO
                    'o' => match preceded(Caseless("ffset"), ascii_lower).parse_next(input)? {
                        'f' => {
                            tail!("rom", Rfc5545(PN5545::TimeZoneOffsetFrom))
                        }
                        't' => tail!("o", Rfc5545(PN5545::TimeZoneOffsetTo)),
                        _ => Err(()),
                    },
                    'u' => tail!("rl", Rfc5545(PN5545::TimeZoneUrl)),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            'u' => match ascii_lower.parse_next(input)? {
                'i' => tail!("d", Rfc5545(PN5545::UniqueIdentifier)),
                'r' => tail!("l", Rfc5545(PN5545::UniformResourceLocator)),
                _ => Err(()),
            },
            'v' => tail!("ersion", Rfc5545(PN5545::Version)),
            _ => Err(()),
        }
    }

    match static_name.parse_next(input) {
        Ok(res) => Ok(res),
        Err(()) => abandon(input),
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
        model::primitive::{
            DurationKind, DurationTime, GeoComponent, ParticipationRole, ParticipationStatus,
            RawTime, Sign, Time, TimeFormat,
        },
        parser::{escaped::AsEscaped, parameter::ParamValue},
    };

    use super::*;
    use winnow::{Parser, ascii::crlf, combinator::terminated};

    // PROPERTY PARSING TESTS

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

        let mut iter = props.into_iter().map(|(prop, _)| prop);
        let _ = iter.next(); // skip the first line, it's not a real property

        assert_eq!(
            iter.next(),
            Some(Prop::Known(KnownProp::ProdId(Text(
                "-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN".as_escaped()
            )))),
        );

        assert_eq!(iter.next(), Some(Prop::Known(KnownProp::Version)));

        let _ = iter.next(); // next line is also not a real property

        assert_eq!(
            iter.next(),
            Some(Prop::Known(KnownProp::DtStamp(DateTime {
                date: date!(1996;7;4),
                time: Time {
                    raw: RawTime {
                        hours: 12,
                        minutes: 0,
                        seconds: 0,
                    },
                    format: Utc,
                }
            })))
        );

        assert_eq!(
            iter.next(),
            Some(Prop::Known(KnownProp::Uid(Uid(
                "uid1@example.com".as_escaped()
            )))),
        );

        assert_eq!(
            iter.next(),
            Some(Prop::Known(KnownProp::Organizer(
                CalAddress("mailto:jsmith@example.com".as_escaped(),),
                OrganizerParams::default()
            ))),
        );

        // omitted tests for remaining properties...
    }

    #[test]
    fn rfc_5545_example_calendar_scale_property() {
        let input = "CALSCALE:GREGORIAN";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::CalScale));
    }

    #[test]
    fn rfc_5545_example_method_property() {
        let input = "METHOD:REQUEST";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Method(Method::Request)));
    }

    #[test]
    fn rfc_5545_example_product_identifier_property() {
        let input = "PRODID:-//ABC Corporation//NONSGML My Product//EN";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::ProdId(Text(
                "-//ABC Corporation//NONSGML My Product//EN"
            )))
        )
    }

    #[test]
    fn rfc_5545_example_version_property() {
        let input = "VERSION:2.0";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Version));
    }

    #[test]
    fn rfc_5545_example_attachment_property_1() {
        let input = "ATTACH:CID:jsmith.part3.960817T083000.xyzMail@example.com";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Attach(value, params)) = prop else {
            panic!()
        };

        assert!(params.format_type.is_none());
        assert_eq!(
            value,
            AttachValue::Uri(Uri("CID:jsmith.part3.960817T083000.xyzMail@example.com"))
        );
    }

    #[test]
    fn rfc_5545_example_attachment_property_2() {
        let input =
            "ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/\r\n reports/r-960812.ps"
                .as_escaped();
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Attach(value, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params.format_type,
            Some(FormatType {
                source: "application/postscript".as_escaped(),
                separator_index: 11
            })
        );
        assert_eq!(
            value,
            AttachValue::Uri(Uri(
                "ftp://example.com/pub/\r\n reports/r-960812.ps".as_escaped()
            ))
        );
    }

    #[test]
    fn rfc_5545_example_categories_property_1() {
        let input = "CATEGORIES:APPOINTMENT,EDUCATION";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Categories(values, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(
            values.as_ref(),
            [Text("APPOINTMENT"), Text("EDUCATION")].as_slice()
        );
    }

    #[test]
    fn rfc_5545_example_categories_property_2() {
        let input = "CATEGORIES:MEETING";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Categories(values, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(values.as_ref(), [Text("MEETING")].as_slice());
    }

    #[test]
    fn rfc_5545_example_classification_property() {
        let input = "CLASS:PUBLIC";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Class(ClassValue::Public)));
    }

    #[test]
    fn rfc_5545_example_comment_property() {
        let input = "COMMENT:The meeting really needs to include both ourselves \r\n and the customer. We can't hold this meeting without them. \r\n As a matter of fact\\, the venue for the meeting ought to be at \r\n their site. - - John";

        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Comment(text, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert!(params.alternate_representation.is_none());
        assert_eq!(text, Text(input[8..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_description_property() {
        let input = "DESCRIPTION:Meeting to provide technical review for \"Phoenix\" \r\n design.\\nHappy Face Conference Room. Phoenix design team \r\n MUST attend this meeting.\\nRSVP to team leader.";

        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Description(text, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert!(params.alternate_representation.is_none());
        assert_eq!(text, Text(input[12..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_geographic_position_property() {
        let input = "GEO:37.386013;-122.082932";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Geo(geo)) = prop else {
            panic!()
        };

        assert_eq!(geo.lat, GeoComponent(37386013));
        assert_eq!(geo.lon, GeoComponent(-122082932));
    }

    #[test]
    fn rfc_5545_example_location_property_1() {
        let input = "LOCATION:Conference Room - F123\\, Bldg. 002";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Location(text, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert!(params.alternate_representation.is_none());
        assert_eq!(text, Text("Conference Room - F123\\, Bldg. 002"));
    }

    #[test]
    fn rfc_5545_example_location_property_2() {
        let input = "LOCATION;ALTREP=\"http://xyzcorp.com/conf-rooms/f123.vcf\":\r\n Conference Room - F123\\, Bldg. 002";

        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Location(text, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(
            params.alternate_representation,
            Some(Uri("http://xyzcorp.com/conf-rooms/f123.vcf".as_escaped()))
        );

        assert_eq!(text, Text(input[57..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_percent_complete_property() {
        let input = "PERCENT-COMPLETE:39";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::PercentComplete(pct)) = prop else {
            panic!()
        };

        assert_eq!(pct, CompletionPercentage(39));
    }

    #[test]
    fn rfc_5545_example_priority_property_1() {
        let input = "PRIORITY:1";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Priority(Priority::A1)));
    }

    #[test]
    fn rfc_5545_example_priority_property_2() {
        let input = "PRIORITY:2";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Priority(Priority::A2)));
    }

    #[test]
    fn rfc_5545_example_priority_property_3() {
        let input = "PRIORITY:0";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Priority(Priority::Zero)));
    }

    #[test]
    fn rfc_5545_example_resources_property_1() {
        let input = "RESOURCES:EASEL,PROJECTOR,VCR";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Resources(values, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert!(params.alternate_representation.is_none());
        assert_eq!(
            values.as_ref(),
            [Text("EASEL"), Text("PROJECTOR"), Text("VCR")].as_slice()
        );
    }

    #[test]
    fn rfc_5545_example_resources_property_2() {
        let input = "RESOURCES;LANGUAGE=fr:Nettoyeur haute pression";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Resources(values, params)) = prop else {
            panic!()
        };

        assert!(params.alternate_representation.is_none());
        assert_eq!(params.language, Some(Language("fr")));
        assert_eq!(
            values.as_ref(),
            [Text("Nettoyeur haute pression")].as_slice()
        );
    }

    #[test]
    fn rfc_5545_example_status_property_1() {
        let input = "STATUS:TENTATIVE";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Status(Status::Tentative)));
    }

    #[test]
    fn rfc_5545_example_status_property_2() {
        let input = "STATUS:NEEDS-ACTION";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Status(Status::NeedsAction)));
    }

    #[test]
    fn rfc_5545_example_status_property_3() {
        let input = "STATUS:DRAFT";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::Status(Status::Draft)));
    }

    #[test]
    fn rfc_5545_example_summary_property() {
        let input = "SUMMARY:Department Party";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Summary(text, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert!(params.alternate_representation.is_none());
        assert_eq!(text, Text("Department Party"));
    }

    #[test]
    fn rfc_5545_example_date_time_completed_property() {
        let input = "COMPLETED:19960401T150000Z";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::DtCompleted(datetime)) = prop else {
            panic!()
        };

        assert_eq!(datetime.date, date!(1996;4;1));
        assert_eq!(
            datetime.time.raw,
            RawTime {
                hours: 15,
                minutes: 0,
                seconds: 0
            }
        );
        assert_eq!(datetime.time.format, Utc);
    }

    #[test]
    fn rfc_5545_example_date_time_end_property_1() {
        let input = "DTEND:19960401T150000Z";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::DtEnd(value, params)) = prop else {
            panic!()
        };

        assert!(params.tz_id.is_none());
        assert_eq!(
            value,
            DateTimeOrDate::DateTime(DateTime {
                date: date!(1996;4;1),
                time: Time {
                    raw: RawTime {
                        hours: 15,
                        minutes: 0,
                        seconds: 0
                    },
                    format: TimeFormat::Utc
                },
            })
        );
    }

    #[test]
    fn rfc_5545_example_date_time_end_property_2() {
        let input = "DTEND;VALUE=DATE:19980704";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::DtEnd(value, params)) = prop else {
            panic!()
        };

        assert!(params.tz_id.is_none());
        assert_eq!(value, date!(1998;7;4).into());
    }

    #[test]
    fn rfc_5545_example_date_time_due_property() {
        let input = "DUE:19980430T000000Z";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::DtDue(value, params)) = prop else {
            panic!()
        };

        assert!(params.tz_id.is_none());
        assert_eq!(
            value,
            DateTimeOrDate::DateTime(DateTime {
                date: date!(1998;4;30),
                time: Time {
                    raw: RawTime {
                        hours: 0,
                        minutes: 0,
                        seconds: 0
                    },
                    format: TimeFormat::Utc,
                }
            })
        );
    }

    #[test]
    fn rfc_5545_example_date_time_start_property() {
        let input = "DTSTART:19980118T073000Z";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::DtStart(value, params)) = prop else {
            panic!()
        };

        assert!(params.tz_id.is_none());
        assert_eq!(
            value,
            DateTimeOrDate::DateTime(DateTime {
                date: date!(1998;1;18),
                time: Time {
                    raw: RawTime {
                        hours: 7,
                        minutes: 30,
                        seconds: 0
                    },
                    format: TimeFormat::Utc,
                }
            })
        );
    }

    #[test]
    fn rfc_5545_example_duration_property_1() {
        let input = "DURATION:PT1H0M0S";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Duration(duration)) = prop else {
            panic!()
        };

        assert_eq!(
            duration,
            Duration {
                sign: None,
                kind: DurationKind::Time {
                    time: DurationTime::HMS {
                        hours: 1,
                        minutes: 0,
                        seconds: 0
                    }
                },
            }
        );
    }

    #[test]
    fn rfc_5545_example_duration_property_2() {
        let input = "DURATION:PT15M";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Duration(duration)) = prop else {
            panic!()
        };

        assert_eq!(
            duration,
            Duration {
                sign: None,
                kind: DurationKind::Time {
                    time: DurationTime::M { minutes: 15 }
                },
            }
        );
    }

    #[test]
    fn rfc_5545_example_free_busy_time_property_1() {
        let input = "FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:19970308T160000Z/PT8H30M";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::FreeBusy(periods, params)) = prop else {
            panic!()
        };

        assert_eq!(params.free_busy_type, Some(FreeBusyType::BusyUnavailable));
        assert_eq!(
            periods.as_ref(),
            [Period::Start {
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
            .as_slice()
        );
    }

    #[test]
    fn rfc_5545_example_free_busy_time_property_2() {
        let input = "FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::FreeBusy(periods, params)) = prop else {
            panic!()
        };

        assert_eq!(params.free_busy_type, Some(FreeBusyType::Free));
        assert_eq!(
            periods.as_ref(),
            [
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
            .as_slice()
        );
    }

    #[test]
    fn rfc_5545_example_free_busy_time_property_3() {
        let input = "FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H\r\n\t,19970308T230000Z/19970309T000000Z";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::FreeBusy(periods, params)) = prop else {
            panic!()
        };

        assert_eq!(params.free_busy_type, Some(FreeBusyType::Free));
        assert_eq!(
            periods.as_ref(),
            [
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
            .as_slice()
        );
    }

    #[test]
    fn rfc_5545_example_time_transparency_property_1() {
        let input = "TRANSP:TRANSPARENT";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::Transparency(TimeTransparency::Transparent))
        );
    }

    #[test]
    fn rfc_5545_example_time_transparency_property_2() {
        let input = "TRANSP:OPAQUE";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::Transparency(TimeTransparency::Opaque))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_identifier_property_1() {
        let input = "TZID:America/New_York";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(prop, Prop::Known(KnownProp::TzId(TzId("America/New_York"))));
    }

    #[test]
    fn rfc_5545_example_time_zone_identifier_property_2() {
        let input = "TZID:America/Los_Angeles";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzId(TzId("America/Los_Angeles")))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_identifier_property_3() {
        let input = "TZID:/example.org/America/New_York";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzId(TzId("/example.org/America/New_York")))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_name_property_1() {
        let input = "TZNAME:EST";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::TzName(name, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(name, Text("EST"));
    }

    #[test]
    fn rfc_5545_example_time_zone_name_property_2() {
        let input = "TZNAME;LANGUAGE=fr-CA:HNE";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::TzName(name, params)) = prop else {
            panic!()
        };

        assert_eq!(params.language, Some(Language("fr-CA")));
        assert_eq!(name, Text("HNE"));
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_from_property_1() {
        let input = "TZOFFSETFROM:-0500";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzOffsetFrom(UtcOffset {
                sign: Sign::Negative,
                hours: 5,
                minutes: 0,
                seconds: None,
            }))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_from_property_2() {
        let input = "TZOFFSETFROM:+1345";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzOffsetFrom(UtcOffset {
                sign: Sign::Positive,
                hours: 13,
                minutes: 45,
                seconds: None,
            }))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_to_property_1() {
        let input = "TZOFFSETTO:-0400";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzOffsetTo(UtcOffset {
                sign: Sign::Negative,
                hours: 4,
                minutes: 0,
                seconds: None,
            }))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_offset_to_property_2() {
        let input = "TZOFFSETTO:+1245";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzOffsetTo(UtcOffset {
                sign: Sign::Positive,
                hours: 12,
                minutes: 45,
                seconds: None,
            }))
        );
    }

    #[test]
    fn rfc_5545_example_time_zone_url_property() {
        let input = "TZURL:http://timezones.example.org/tz/America-Los_Angeles.ics";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());
        assert_eq!(
            prop,
            Prop::Known(KnownProp::TzUrl(Uri(
                "http://timezones.example.org/tz/America-Los_Angeles.ics"
            ))),
        );
    }

    #[test]
    fn rfc_5545_example_attendee_property_1() {
        let input =
            "ATTENDEE;MEMBER=\"mailto:DEV-GROUP@example.com\":\r\n mailto:joecool@example.com";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Attendee(address, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params,
            AttendeeParams {
                group_or_list_membership: Some(
                    vec![CalAddress("mailto:DEV-GROUP@example.com".as_escaped())].into()
                ),
                ..Default::default()
            }
        );

        assert_eq!(
            address,
            CalAddress("\r\n mailto:joecool@example.com".as_escaped())
        );
    }

    #[test]
    fn rfc_5545_example_attendee_property_2() {
        let input =
            "ATTENDEE;DELEGATED-FROM=\"mailto:immud@example.com\":\r\n mailto:ildoit@example.com";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Attendee(address, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params,
            AttendeeParams {
                delegators: Some(vec![CalAddress("mailto:immud@example.com".as_escaped())].into()),
                ..Default::default()
            }
        );

        assert_eq!(
            address,
            CalAddress("\r\n mailto:ildoit@example.com".as_escaped())
        );
    }

    #[test]
    fn rfc_5545_example_attendee_property_3() {
        let input = "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry\r\n Cabot:mailto:hcabot@example.com";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Attendee(address, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params,
            AttendeeParams {
                participation_role: Some(ParticipationRole::ReqParticipant),
                participation_status: Some(ParticipationStatus::Tentative),
                common_name: Some(ParamValue::Safe("Henry\r\n Cabot".as_escaped())),
                ..Default::default()
            }
        );

        assert_eq!(
            address,
            CalAddress("mailto:hcabot@example.com".as_escaped())
        );
    }

    #[test]
    fn rfc_5545_example_contact_property_1() {
        let input = "CONTACT:Jim Dolittle\\, ABC Industries\\, +1-919-555-1234";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Contact(contact, params)) = prop else {
            panic!()
        };

        assert_eq!(params, TextParams::default());
        assert_eq!(
            contact,
            Text("Jim Dolittle\\, ABC Industries\\, +1-919-555-1234")
        );
    }

    #[test]
    fn rfc_5545_example_contact_property_2() {
        let input = "CONTACT;ALTREP=\"ldap://example.com:6666/o=ABC%20Industries\\,\r\n c=US???(cn=Jim%20Dolittle)\":Jim Dolittle\\, ABC Industries\\,\r\n +1-919-555-1234";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Contact(contact, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params,
            TextParams {
                language: None,
                alternate_representation: Some(Uri(input[16..89].as_escaped(),))
            }
        );

        assert_eq!(contact, Text(input[91..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_organizer_property_1() {
        let input = "ORGANIZER;CN=John Smith:mailto:jsmith@example.com";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Organizer(organizer, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params,
            OrganizerParams {
                common_name: Some(ParamValue::Safe("John Smith")),
                ..Default::default()
            }
        );

        assert_eq!(organizer, CalAddress("mailto:jsmith@example.com"));
    }

    #[test]
    fn rfc_5545_example_organizer_property_2() {
        let input = "ORGANIZER;CN=JohnSmith;DIR=\"ldap://example.com:6666/o=DC%20Ass\r\n ociates,c=US???(cn=John%20Smith)\":mailto:jsmith@example.com";
        let (tail, (prop, unknown_params)) =
            property::<_, ()>.parse_peek(input.as_escaped()).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Organizer(organizer, params)) = prop else {
            panic!()
        };

        assert_eq!(
            params,
            OrganizerParams {
                common_name: Some(ParamValue::Safe("JohnSmith".as_escaped())),
                directory_entry_reference: Some(Uri(input[28..97].as_escaped())),
                ..Default::default()
            }
        );

        assert_eq!(organizer, CalAddress(input[99..].as_escaped()));
    }

    #[test]
    fn rfc_5545_example_recurrence_identifier_property_1() {
        let input = "RECURRENCE-ID;VALUE=DATE:19960401";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::RecurrenceId(DateTimeOrDate::Date(date), params)) = prop else {
            panic!()
        };

        assert!(params.tz_id.is_none());
        assert!(params.recurrence_identifier_range.is_none());
        assert_eq!(date, date!(1996;4;1));
    }

    #[test]
    fn rfc_5545_example_recurrence_identifier_property_2() {
        let input = "RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::RecurrenceId(DateTimeOrDate::DateTime(dt), params)) = prop
        else {
            panic!()
        };

        assert!(params.tz_id.is_none());
        assert_eq!(params.recurrence_identifier_range, Some(ThisAndFuture));
        assert_eq!(
            dt,
            DateTime {
                date: date!(1996;1;20),
                time: Time {
                    raw: RawTime {
                        hours: 12,
                        minutes: 0,
                        seconds: 0
                    },
                    format: TimeFormat::Utc
                },
            }
        );
    }

    #[test]
    fn rfc_5545_example_uid_property() {
        let input = "UID:19960401T080045Z-4000F192713-0052@example.com";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::Uid(uid)) = prop else {
            panic!()
        };

        assert_eq!(uid, Uid("19960401T080045Z-4000F192713-0052@example.com"));
    }

    #[test]
    fn rfc_5545_example_request_status_property_1() {
        let input = "REQUEST-STATUS:2.0;Success";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::RequestStatus(status, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(
            status,
            RequestStatus {
                code: (2, 0).into(),
                description: Text("Success"),
                exception_data: None,
            }
        );
    }

    #[test]
    fn rfc_5545_example_request_status_property_2() {
        let input = "REQUEST-STATUS:3.1;Invalid property value;DTSTART:96-Apr-01";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::RequestStatus(status, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(
            status,
            RequestStatus {
                code: (3, 1).into(),
                description: Text("Invalid property value"),
                exception_data: Some(Text("DTSTART:96-Apr-01")),
            }
        );
    }

    // NOTE: skipped the third example

    #[test]
    fn rfc_5545_example_request_status_property_4() {
        let input = "REQUEST-STATUS:4.1;Event conflict.  Date-time is busy.";
        let (tail, (prop, unknown_params)) = property::<_, ()>.parse_peek(input).unwrap();

        assert!(tail.is_empty());
        assert!(unknown_params.is_empty());

        let Prop::Known(KnownProp::RequestStatus(status, params)) = prop else {
            panic!()
        };

        assert!(params.language.is_none());
        assert_eq!(
            status,
            RequestStatus {
                code: (4, 1).into(),
                description: Text("Event conflict.  Date-time is busy."),
                exception_data: None,
            }
        );
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
        assert_prop_name_eq(
            "CALSCALE",
            PropName::Rfc5545(Rfc5545PropName::CalendarScale),
        );
        assert_prop_name_eq("METHOD", PropName::Rfc5545(Rfc5545PropName::Method));
        assert_prop_name_eq(
            "PRODID",
            PropName::Rfc5545(Rfc5545PropName::ProductIdentifier),
        );
        assert_prop_name_eq("VERSION", PropName::Rfc5545(Rfc5545PropName::Version));
    }

    #[test]
    fn rfc5545_descriptive_property_names() {
        assert_prop_name_eq("ATTACH", PropName::Rfc5545(Rfc5545PropName::Attachment));
        assert_prop_name_eq("CATEGORIES", PropName::Rfc5545(Rfc5545PropName::Categories));
        assert_prop_name_eq("CLASS", PropName::Rfc5545(Rfc5545PropName::Classification));
        assert_prop_name_eq("COMMENT", PropName::Rfc5545(Rfc5545PropName::Comment));
        assert_prop_name_eq(
            "DESCRIPTION",
            PropName::Rfc5545(Rfc5545PropName::Description),
        );
        assert_prop_name_eq(
            "GEO",
            PropName::Rfc5545(Rfc5545PropName::GeographicPosition),
        );
        assert_prop_name_eq("LOCATION", PropName::Rfc5545(Rfc5545PropName::Location));
        assert_prop_name_eq(
            "PERCENT-COMPLETE",
            PropName::Rfc5545(Rfc5545PropName::PercentComplete),
        );
        assert_prop_name_eq("PRIORITY", PropName::Rfc5545(Rfc5545PropName::Priority));
        assert_prop_name_eq("RESOURCES", PropName::Rfc5545(Rfc5545PropName::Resources));
        assert_prop_name_eq("STATUS", PropName::Rfc5545(Rfc5545PropName::Status));
        assert_prop_name_eq("SUMMARY", PropName::Rfc5545(Rfc5545PropName::Summary));
    }

    #[test]
    fn rfc5545_datetime_property_names() {
        assert_prop_name_eq(
            "COMPLETED",
            PropName::Rfc5545(Rfc5545PropName::DateTimeCompleted),
        );
        assert_prop_name_eq("DTEND", PropName::Rfc5545(Rfc5545PropName::DateTimeEnd));
        assert_prop_name_eq("DUE", PropName::Rfc5545(Rfc5545PropName::DateTimeDue));
        assert_prop_name_eq("DTSTART", PropName::Rfc5545(Rfc5545PropName::DateTimeStart));
        assert_prop_name_eq("DURATION", PropName::Rfc5545(Rfc5545PropName::Duration));
        assert_prop_name_eq("FREEBUSY", PropName::Rfc5545(Rfc5545PropName::FreeBusyTime));
        assert_prop_name_eq(
            "TRANSP",
            PropName::Rfc5545(Rfc5545PropName::TimeTransparency),
        );
        assert_prop_name_eq("DTSTAMP", PropName::Rfc5545(Rfc5545PropName::DateTimeStamp));
    }

    #[test]
    fn rfc5545_timezone_property_names() {
        assert_prop_name_eq(
            "TZID",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneIdentifier),
        );
        assert_prop_name_eq("TZNAME", PropName::Rfc5545(Rfc5545PropName::TimeZoneName));
        assert_prop_name_eq(
            "TZOFFSETFROM",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetFrom),
        );
        assert_prop_name_eq(
            "TZOFFSETTO",
            PropName::Rfc5545(Rfc5545PropName::TimeZoneOffsetTo),
        );
        assert_prop_name_eq("TZURL", PropName::Rfc5545(Rfc5545PropName::TimeZoneUrl));
    }

    #[test]
    fn rfc5545_relationship_property_names() {
        assert_prop_name_eq("ATTENDEE", PropName::Rfc5545(Rfc5545PropName::Attendee));
        assert_prop_name_eq("CONTACT", PropName::Rfc5545(Rfc5545PropName::Contact));
        assert_prop_name_eq("ORGANIZER", PropName::Rfc5545(Rfc5545PropName::Organizer));
        assert_prop_name_eq(
            "RECURRENCE-ID",
            PropName::Rfc5545(Rfc5545PropName::RecurrenceId),
        );
        assert_prop_name_eq("RELATED-TO", PropName::Rfc5545(Rfc5545PropName::RelatedTo));
        assert_prop_name_eq(
            "URL",
            PropName::Rfc5545(Rfc5545PropName::UniformResourceLocator),
        );
        assert_prop_name_eq("UID", PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier));
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
        assert_prop_name_eq("RRULE", PropName::Rfc5545(Rfc5545PropName::RecurrenceRule));
    }

    #[test]
    fn rfc5545_alarm_property_names() {
        assert_prop_name_eq("ACTION", PropName::Rfc5545(Rfc5545PropName::Action));
        assert_prop_name_eq("REPEAT", PropName::Rfc5545(Rfc5545PropName::RepeatCount));
        assert_prop_name_eq("TRIGGER", PropName::Rfc5545(Rfc5545PropName::Trigger));
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
        assert_prop_name_eq("SOURCE", PropName::Rfc7986(Rfc7986PropName::Source));
        assert_prop_name_eq("COLOR", PropName::Rfc7986(Rfc7986PropName::Color));
        assert_prop_name_eq("IMAGE", PropName::Rfc7986(Rfc7986PropName::Image));
        assert_prop_name_eq("CONFERENCE", PropName::Rfc7986(Rfc7986PropName::Conference));
    }

    #[test]
    fn property_name_case_insensitivity() {
        assert_prop_name_eq("dtstart", PropName::Rfc5545(Rfc5545PropName::DateTimeStart));
        assert_prop_name_eq("DTSTART", PropName::Rfc5545(Rfc5545PropName::DateTimeStart));
        assert_prop_name_eq("DtStArT", PropName::Rfc5545(Rfc5545PropName::DateTimeStart));
        assert_prop_name_eq("dtSTART", PropName::Rfc5545(Rfc5545PropName::DateTimeStart));

        assert_prop_name_eq("conference", PropName::Rfc7986(Rfc7986PropName::Conference));
        assert_prop_name_eq("Conference", PropName::Rfc7986(Rfc7986PropName::Conference));
        assert_prop_name_eq("CONFERENCE", PropName::Rfc7986(Rfc7986PropName::Conference));
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
