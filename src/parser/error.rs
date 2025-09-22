//! Error types for parsing iCalendar.

use std::convert::Infallible;

use crate::model::{
    component::TzRuleKind,
    parameter::{KnownParam, StaticParam},
    primitive::{GeoComponent, Integer, Sign, Status, ValueType},
    rrule,
};

use super::property::PropName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CalendarParseError<S> {
    InvalidPositiveInteger(Integer),
    InvalidRawTime(InvalidRawTimeError),
    InvalidUtcOffset(InvalidUtcOffsetError),
    InvalidDate(InvalidDateError),
    InvalidInteger(InvalidIntegerError),
    InvalidGeo(InvalidGeoError),
    InvalidCompletionPercentage(InvalidCompletionPercentageError),
    InvalidPriority(InvalidPriorityError),
    InvalidDurationTime(InvalidDurationTimeError),
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParam),
    MissingValueType,
    InvalidValueType(ValueType<S>),
    AttachParam(AttachParamError<S>),
    DtParam(DtParamError<S>),
    RDateParam(RDateParamError<S>),
    SDParam(SDParamError<S>),
    TriggerParam(TriggerParamError<S>),
    /// Received the interval 0 in a recurrence rule, which must be a
    /// positive integer.
    ZeroInterval,
    /// Expected an ISO week index, got a value outside the range `1..=53`.
    InvalidIsoWeekIndex(u8),
    /// Expected a month day index, got a value outside the range `1..=31`.
    InvalidMonthDayIndex(u8),
    /// Expected a month number, got a value outside the range `1..=12`.
    InvalidMonthNumber(u8),
    /// Expected an hour index, got a value outside the range `0..=23`.
    InvalidHourIndex(u8),
    /// Expected a minute index, got a value outside the range `0..=59`.
    InvalidMinuteIndex(u8),
    /// Expected a second index, got a value outside the range `0..=60`.
    InvalidSecondIndex(u8),
    /// Received a part in a recurrence rule more than once.
    DuplicateRRulePart(rrule::PartName),
    /// Both the COUNT and UNTIL parts occurred in the same RRULE.
    CountAndUntilInRRule,
    /// The FREQ part did not occur in an RRULE.
    MissingFreqPart,
    /// A BYxxx rule occurred that was inadmissible for the current FREQ value.
    UnexpectedByRule {
        freq: rrule::Freq,
        by_rule: rrule::ByRuleName,
    },
    UnexpectedProp {
        prop: PropName<S>,
        component: ComponentKind<S>,
    },
    MissingProp {
        prop: PropName<S>,
        component: ComponentKind<S>,
    },
    DurationWithoutRepeat,
    RepeatWithoutDuration,
    TooManyAttachmentsOnAudioAlarm,
    InvalidEventStatus(Status),
    InvalidTodoStatus(Status),
    InvalidJournalStatus(Status),
    MoreThanOneProp {
        prop: PropName<S>,
        component: ComponentKind<S>,
    },
    /// Both DTEND and DURATION occurred in the same VEVENT.
    EventTerminationCollision,
    /// Both DTDUE and DURATION occurred in the same VTODO.
    TodoTerminationCollision,
    /// The ORDER parameter occurred on a property that cannot occur more than once.
    OrderOnNonRepeatableProp,
}

impl<S> From<Infallible> for CalendarParseError<S> {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

/// A component kind, including the static subcomponents.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentKind<S> {
    Calendar,
    Event,
    Todo,
    Journal,
    FreeBusy,
    TimeZone,
    Alarm,
    AudioAlarm,
    DisplayAlarm,
    EmailAlarm,
    Standard,
    Daylight,
    StandardOrDaylight,
    Iana(S),
    X(S),
    /// Iana or X without a specific name.
    Unknown,
}

impl<S> From<TzRuleKind> for ComponentKind<S> {
    fn from(value: TzRuleKind) -> Self {
        match value {
            TzRuleKind::Standard => Self::Standard,
            TzRuleKind::Daylight => Self::Daylight,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidRawTimeError {
    pub(crate) hours: u8,
    pub(crate) minutes: u8,
    pub(crate) seconds: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidUtcOffsetError {
    NegativeZero,
    BadHours(u8),
    BadMinutes(u8),
    BadSeconds(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidDateError {
    pub(crate) year: u16,
    pub(crate) month: u8,
    pub(crate) day: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidIntegerError {
    pub(crate) sign: Option<Sign>,
    pub(crate) digits: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidGeoError {
    IntegralTooLarge(u8),
    LatOutOfBounds(GeoComponent),
    LonOutOfBounds(GeoComponent),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidCompletionPercentageError(pub(crate) Integer);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidPriorityError(pub(crate) Integer);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidDurationTimeError<T = usize> {
    pub(crate) hours: Option<T>,
    pub(crate) seconds: Option<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedKnownParamError<S> {
    pub(crate) current_property: PropName<S>,
    pub(crate) unexpected_param: KnownParam<S>,
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
    DuplicateParam(StaticParam),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DtParamError<S> {
    /// The VALUE parameter occurred and was not DATETIME or DATE.
    InvalidValueType(ValueType<S>),
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParam),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RDateParamError<S> {
    /// The VALUE parameter occurred and was not DATETIME, DATE, or PERIOD.
    InvalidValueType(ValueType<S>),
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParam),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriggerParamError<S> {
    /// The VALUE parameter occurred and was not DURATION or DATETIME.
    InvalidValueType(ValueType<S>),
    /// The VALUE was DATETIME and the relation parameter was present.
    DateTimeWithRelation,
    /// A parameter with a multiplicity less than 2 occurred more than once.
    DuplicateParam(StaticParam),
    /// Received an unexpected known parameter.
    Unexpected(UnexpectedKnownParamError<S>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SDParamError<S> {
    InvalidValueType(ValueType<S>),
    MissingFormatTypeOnNonUri,
    MissingSchemaOnNonUri,
    MissingEncodingOnBinary,
    EncodingOnNonBinary,
    MissingValueType,
    Bit8Encoding,
}
