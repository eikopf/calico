//! Primitive types for the object model.

use chrono::NaiveDate;
pub use iri_string::types::{UriStr, UriString};
use uuid::Uuid;

/// A method as defined in RFC 5546 §1.4
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Method<S = Box<str>> {
    Publish,
    Request,
    Reply,
    Add,
    Cancel,
    Refresh,
    Counter,
    DeclineCounter,
    Iana(S),
}

/// A unique identifier (RFC 5545 §3.8.4.7).
///
/// Following the advice given in RFC 7986 §5.3, UUIDs (RFC 9562) should be
/// preferred over arbitrary string UIDs whenever possible, and especially
/// when generating new UIDs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Uid<S = Box<str>> {
    Uuid(Uuid),
    String(S),
}

impl<S> Uid<S> {
    /// Returns `true` if the uid is [`Uuid`].
    ///
    /// [`Uuid`]: Uid::Uuid
    #[must_use]
    pub fn is_uuid(&self) -> bool {
        matches!(self, Self::Uuid(..))
    }
}

/// An RFC 5646 language tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Language<S = Box<str>>(pub(crate) S);

/// The data of a BINARY property.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Binary {
    pub(crate) bytes: Vec<u8>,
}

/// Date-time or date value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateTimeOrDate<F = TimeFormat> {
    DateTime(DateTime<F>),
    Date(Date),
}

/// The product of a [`Date`] and a [`Time`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DateTime<F = TimeFormat> {
    pub date: Date,
    pub time: Time<F>,
}

/// A DATE value (RFC 5545, §3.3.4).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Date(pub(crate) NaiveDate);

/// A TIME value (RFC 5545, §3.3.12).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Time<F = TimeFormat> {
    pub raw: RawTime,
    pub format: F,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RawTime {
    pub hours: u8,
    pub minutes: u8,
    pub seconds: u8,
}

/// A marker struct for absolute UTC time.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Utc;
/// A marker struct for local time.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Local;

/// The format of a [`Time`], which may be local or absolute.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TimeFormat {
    #[default]
    Local,
    Utc,
}

/// One of the seven weekdays.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Weekday {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday,
}

/// The possible values of the ENCODING parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    /// The `8bit` text encoding defined in RFC 2045.
    Bit8,
    /// The `BASE64` binary encoding defined in RFC 4648.
    Base64,
}

/// The data of an RFC 7986 IMAGE property.
#[derive(Debug, Clone)]
pub enum ImageData<U = UriString> {
    Uri(U),
    Binary(Binary),
}

/// RFC 5545 §3.2.8
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatType<S = Box<str>> {
    pub(crate) source: S,
    pub(crate) separator_index: usize,
}

/// DISPLAY parameter values (RFC 7986)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayType<S = Box<str>> {
    Badge,
    Graphic,
    Fullsize,
    Thumbnail,
    Other(S),
}

/// FEATURE parameter values (RFC 7986)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FeatureType<S = Box<str>> {
    Audio,
    Chat,
    Feed,
    Moderator,
    Phone,
    Screen,
    Video,
    Other(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    Event(EventStatus),
    Todo(TodoStatus),
    Journal(JournalStatus),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventStatus {
    Tentative,
    Confirmed,
    Cancelled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TodoStatus {
    NeedsAction,
    Completed,
    InProcess,
    Cancelled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JournalStatus {
    Draft,
    Final,
    Cancelled,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Transparency {
    #[default]
    Opaque,
    Transparent,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CalendarUserType<S = Box<str>> {
    Individual,
    Group,
    Resource,
    Room,
    Unknown,
    Other(S),
}

impl<S> Default for CalendarUserType<S> {
    fn default() -> Self {
        Self::Individual
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParticipationRole<S = Box<str>> {
    Chair,
    ReqParticipant,
    OptParticipant,
    NonParticipant,
    Other(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParticipationStatus<S = Box<str>> {
    NeedsAction,
    Accepted,
    Declined,
    Tentative,
    Delegated,
    Completed,
    InProcess,
    Other(S),
}

impl<S> Default for ParticipationStatus<S> {
    fn default() -> Self {
        Self::NeedsAction
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FreeBusyType<S = Box<str>> {
    Free,
    Busy,
    BusyUnavailable,
    BusyTentative,
    Other(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlarmAction<S = Box<str>> {
    Audio,
    Display,
    Email,
    Other(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriggerRelation {
    Start,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelationshipType<S = Box<str>> {
    Parent,
    Child,
    Sibling,
    Other(S),
}

/// The type of a [`Value`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType<S = Box<str>> {
    Binary,
    Boolean,
    CalAddress,
    Date,
    DateTime,
    Duration,
    Float,
    Integer,
    Period,
    Recur,
    Text,
    Time,
    Uri,
    UtcOffset,
    Iana(S),
    X(S),
}

///A runtime-discriminated property value.
#[derive(Debug, Clone)]
pub enum Value<S = Box<str>, U = UriString> {
    Binary(Binary),
    Boolean(bool),
    CalAddress(U),
    Date(Date),
    DateTime(DateTime),
    Duration(Duration),
    Float(Float<S>),
    Integer(i32),
    Period(Period),
    Recur(()),
    Text(S),
    Time(Time),
    Uri(U),
    UtcOffset(UtcOffset),
    Iana { name: S, value: S },
    X { name: S, value: S },
}

impl<S, U> Value<S, U> {
    pub fn as_value_type(&self) -> ValueType<&S> {
        match self {
            Value::Binary(_) => ValueType::Binary,
            Value::Boolean(_) => ValueType::Boolean,
            Value::CalAddress(_) => ValueType::CalAddress,
            Value::Date(_) => ValueType::Date,
            Value::DateTime(_) => ValueType::DateTime,
            Value::Duration(_) => ValueType::Duration,
            Value::Float(_) => ValueType::Float,
            Value::Integer(_) => ValueType::Integer,
            Value::Period(_) => ValueType::Period,
            Value::Recur(_) => ValueType::Recur,
            Value::Text(_) => ValueType::Text,
            Value::Time(_) => ValueType::Time,
            Value::Uri(_) => ValueType::Uri,
            Value::UtcOffset(_) => ValueType::UtcOffset,
            Value::Iana { name, .. } => ValueType::Iana(name),
            Value::X { name, .. } => ValueType::X(name),
        }
    }
}

/// A string matching the regex `[\+\-]?[0-9]+\.[0-9]*` (RFC 5545 §3.3.7). Since
/// the standard imposes no precision requirements for floats, representing them
/// as strings after validation is the best option.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Float<S = Box<str>>(pub S);

/// The possible values of the `ATTACH` property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttachValue<U = UriString> {
    Uri(U),
    Binary(Binary),
}

/// The value type of the `CLASS` property.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassValue<S = Box<str>> {
    Public,
    Private,
    Confidential,
    Other(S),
}

impl<S> Default for ClassValue<S> {
    fn default() -> Self {
        Self::Public
    }
}

/// The only possible value of the `RANGE` parameter.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ThisAndFuture;

/// Period of time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Period<F = TimeFormat> {
    Explicit {
        start: DateTime<F>,
        end: DateTime<F>,
    },
    Start {
        start: DateTime<F>,
        duration: Duration,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RDate<F = TimeFormat> {
    DateTime(DateTime<F>),
    Date(Date),
    Period(Period),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[repr(i8)]
pub enum Sign {
    #[default]
    Positive = 1,
    Negative = -1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Duration {
    pub sign: Option<Sign>,
    pub kind: DurationKind,
}

/// The kind of a [`Duration`]. The type parameter `T` is the underlying integer type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DurationKind<T = usize> {
    /// Some number of days with an optional time duration.
    Date {
        days: T,
        time: Option<DurationTime<T>>,
    },
    /// An exact time duration.
    Time { time: DurationTime<T> },
    /// Some number of weeks.
    Week { weeks: T },
}

/// The time portion of a [`Duration`], measured in hours, minutes, and seconds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DurationTime<T = usize> {
    HMS { hours: T, minutes: T, seconds: T },
    HM { hours: T, minutes: T },
    MS { minutes: T, seconds: T },
    H { hours: T },
    M { minutes: T },
    S { seconds: T },
}

// TODO: it's unclear whether Geo is correct here, since the FLOAT type in RFC
// 5545 is not explicitly defined with reference to IEEE 754.

/// Geographic coordinates.
#[derive(Debug, Clone)]
pub struct Geo {
    pub latitude: f64,
    pub longitude: f64,
}

/// UTC offset.
#[derive(Debug, Clone)]
pub struct UtcOffset {
    pub hours: i8,
    pub minutes: u8,
    pub seconds: Option<u8>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn raw_time_ord_impl() {
        assert!(
            RawTime {
                hours: 12,
                minutes: 0,
                seconds: 0
            } < RawTime {
                hours: 12,
                minutes: 30,
                seconds: 0
            }
        );
    }
}
