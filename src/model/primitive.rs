//! Primitive types for the object model.
//!
//! # Type Parameters
//!
//! There are two primary groups of type parameters on the types in this module;
//! they are called `S` and `F` by convention.
//!
//! The `S` parameter denotes the _source_ of a type, i.e. the underlying data
//! over which the type is providing a view. Typically this is a slice of the
//! parsed input, e.g. `&str`, `&[u8]`, or
//! [`Escaped`](crate::parser::escaped::Escaped).
//!
//! The `F` parameter is the _time format_ of a type. This is used to distinguish
//! between temporal values which are strictly [`Local`], strictly in reference
//! to [`Utc`], or which may have either a local or absolute [`TimeFormat`].

use chrono::NaiveDate;

/// The INTEGER type as defined in RFC 5545 §3.3.8.
pub type Integer = i32;

/// A method as defined in RFC 5546 §1.4
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Method<S> {
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

/// A timezone identifier.
#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq)]
pub struct TzId<S>(pub(crate) S);

/// An unescaped text value (RFC 5545 §3.3.11).
#[derive(Debug, Default, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Text<S>(pub(crate) S);

/// A unique identifier (RFC 5545 §3.8.4.7).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Uid<S>(pub(crate) S);

/// A URI (RFC 3986 §3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Uri<S>(pub(crate) S);

/// A calendar address (RFC 5545 §3.3.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CalAddress<S>(pub(crate) S);

/// An RFC 5646 language tag.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Language<S>(pub(crate) S);

/// The data of a BINARY property.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Binary {
    pub(crate) bytes: Vec<u8>,
}

/// The text of a BINARY value, which may be converted into a [`Binary`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct BinaryText<S>(pub(crate) S);

/// Date-time or date value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateTimeOrDate<F = TimeFormat> {
    DateTime(DateTime<F>),
    Date(Date),
}

impl<F> DateTimeOrDate<F> {
    /// Returns `true` if the date time or date is [`Date`].
    ///
    /// [`Date`]: DateTimeOrDate::Date
    #[must_use]
    pub fn is_date(&self) -> bool {
        matches!(self, Self::Date(..))
    }

    /// Returns `true` if the date time or date is [`DateTime`].
    ///
    /// [`DateTime`]: DateTimeOrDate::DateTime
    #[must_use]
    pub fn is_date_time(&self) -> bool {
        matches!(self, Self::DateTime(..))
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImageData<S> {
    Uri(Uri<S>),
    Binary(BinaryText<S>),
}

/// The value of the TRANSP property (RFC 5545 §3.8.2.7).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TimeTransparency {
    #[default]
    Opaque,
    Transparent,
}

/// RFC 5545 §3.2.8
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatType<S> {
    pub(crate) source: S,
    pub(crate) separator_index: usize,
}

/// DISPLAY parameter values (RFC 7986)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayType<S> {
    Badge,
    Graphic,
    Fullsize,
    Thumbnail,
    Iana(S),
    X(S),
}

/// FEATURE parameter values (RFC 7986)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FeatureType<S> {
    Audio,
    Chat,
    Feed,
    Moderator,
    Phone,
    Screen,
    Video,
    Iana(S),
    X(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CalendarUserType<S> {
    Individual,
    Group,
    Resource,
    Room,
    Unknown,
    Iana(S),
    X(S),
}

impl<S> Default for CalendarUserType<S> {
    fn default() -> Self {
        Self::Individual
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParticipationRole<S> {
    Chair,
    ReqParticipant,
    OptParticipant,
    NonParticipant,
    Iana(S),
    X(S),
}

impl<S> Default for ParticipationRole<S> {
    fn default() -> Self {
        Self::ReqParticipant
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    Tentative,
    Confirmed,
    Cancelled,
    NeedsAction,
    Completed,
    InProcess,
    Draft,
    Final,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParticipationStatus<S> {
    NeedsAction,
    Accepted,
    Declined,
    Tentative,
    Delegated,
    Completed,
    InProcess,
    Iana(S),
    X(S),
}

impl<S> Default for ParticipationStatus<S> {
    fn default() -> Self {
        Self::NeedsAction
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FreeBusyType<S> {
    Free,
    Busy,
    BusyUnavailable,
    BusyTentative,
    Iana(S),
    X(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlarmAction<S> {
    Audio,
    Display,
    Email,
    Iana(S),
    X(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriggerRelation {
    Start,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelationshipType<S> {
    Parent,
    Child,
    Sibling,
    Iana(S),
    X(S),
}

/// The type of a [`Value`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType<S> {
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<S> {
    Binary(BinaryText<S>),
    Boolean(bool),
    CalAddress(CalAddress<S>),
    Date(Date),
    DateTime(DateTime),
    Duration(Duration),
    Float(Float<S>),
    Integer(i32),
    Period(Period),
    Recur(()),
    Text(S),
    Time(Time),
    Uri(Uri<S>),
    UtcOffset(UtcOffset),
    Iana { name: S, value: S },
    X { name: S, value: S },
}

impl<S> Value<S> {
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
pub struct Float<S>(pub S);

/// The possible values of the `ATTACH` property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttachValue<S> {
    Uri(Uri<S>),
    Binary(BinaryText<S>),
}

/// The value type of the `CLASS` property.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClassValue<S> {
    Public,
    Private,
    Confidential,
    Iana(S),
    X(S),
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

/// Geographic coordinates (RFC 5545 §3.8.1.6).
///
/// # Precision
/// To quote directly from RFC 5545 §3.8.1.6 (_Description_):
/// > The longitude and latitude values MAY be specified up to six decimal
/// > places, which will allow for accuracy to within one meter of geographical
/// > position. Receiving applications MUST accept values of this precision and
/// > MAY truncate values of greater position.
///
/// And again from the next paragraph:
/// > Whole degrees of latitude shall be represented by a two-digit decimal
/// > number ranging from 0 through 90. Whole degrees of longitude shall be
/// > represented by a decimal number ranging from 0 through 180.
///
/// So as an upper bound, we need to preserve decimal precision up to 6 digits
/// in the range `-180..=180`. An estimate of `log2(360) + log2(1E6) ≈ 28.4`
/// (rounding up to the nearest byte) indicates that we will need at least 32
/// bits to represent this. Unfortunately, `f32` will not suffice.
///
/// The solution is to literally represent the value with fixed precision: we
/// do this by multiplying the value by 10^6 so that it has no fractional
/// component, and storing it as an `i32`. This has the advantage of preserving
/// most of the algebraic properties of the original domain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Geo {
    pub lat: GeoComponent,
    pub lon: GeoComponent,
}

/// A component of a [`Geo`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct GeoComponent(pub(crate) i32);

impl GeoComponent {
    pub const SCALING_FACTOR: i32 = 10i32.pow(6);
}

/// An integer in the range `0..=100`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CompletionPercentage(pub(crate) u8);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Priority {
    #[default]
    Zero,
    A1,
    A2,
    A3,
    B1,
    B2,
    B3,
    C1,
    C2,
    C3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PriorityClass {
    Low,
    Medium,
    High,
}

impl PartialOrd for Priority {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if matches!(self, Self::Zero) || matches!(other, Self::Zero) {
            None
        } else {
            let a = (*self) as u8;
            let b = (*other) as u8;

            match a.cmp(&b) {
                std::cmp::Ordering::Less => Some(std::cmp::Ordering::Greater),
                std::cmp::Ordering::Equal => Some(std::cmp::Ordering::Equal),
                std::cmp::Ordering::Greater => Some(std::cmp::Ordering::Less),
            }
        }
    }
}

impl Priority {
    pub const fn is_low(self) -> bool {
        matches!(self.into_class(), Some(PriorityClass::Low))
    }

    pub const fn is_medium(self) -> bool {
        matches!(self.into_class(), Some(PriorityClass::Medium))
    }

    pub const fn is_high(self) -> bool {
        matches!(self.into_class(), Some(PriorityClass::High))
    }

    pub const fn into_class(self) -> Option<PriorityClass> {
        match self {
            Self::Zero => None,
            Self::A1 | Self::A2 | Self::A3 | Self::B1 => {
                Some(PriorityClass::High)
            }
            Self::B2 => Some(PriorityClass::Medium),
            Self::B3 | Self::C1 | Self::C2 | Self::C3 => {
                Some(PriorityClass::Low)
            }
        }
    }
}

/// A UTC offset (RFC 5545 §3.3.14).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UtcOffset {
    pub sign: Sign,
    pub hours: u8,
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

    #[test]
    #[allow(clippy::neg_cmp_op_on_partial_ord)]
    fn priority_partial_ord() {
        assert_eq!(Priority::Zero.partial_cmp(&Priority::Zero), None);
        assert_eq!(Priority::Zero.partial_cmp(&Priority::A1), None);
        assert_eq!(Priority::A1.partial_cmp(&Priority::Zero), None);

        assert!(Priority::A1 > Priority::A2);
        assert!(Priority::A2 > Priority::A3);
        assert!(Priority::A1 > Priority::A3);

        assert!(Priority::A1 > Priority::B1);
        assert!(Priority::B1 > Priority::C1);
        assert!(Priority::A1 > Priority::C1);

        assert!(!(Priority::Zero > Priority::C2));
        assert!(!(Priority::Zero < Priority::C2));
        assert!(Priority::Zero != Priority::C2);
    }

    #[test]
    fn priority_class_predicates() {
        assert!(!Priority::Zero.is_low());
        assert!(!Priority::Zero.is_medium());
        assert!(!Priority::Zero.is_high());

        assert!(Priority::A1.is_high());
        assert!(Priority::A2.is_high());
        assert!(Priority::A3.is_high());
        assert!(Priority::B1.is_high());

        assert!(!Priority::B2.is_high());
        assert!(Priority::B2.is_medium());
        assert!(!Priority::B2.is_low());

        assert!(Priority::B3.is_low());
        assert!(Priority::C1.is_low());
        assert!(Priority::C2.is_low());
        assert!(Priority::C3.is_low());
    }

    #[test]
    fn priority_class_projection() {
        assert_eq!(Priority::Zero.into_class(), None);

        assert_eq!(Priority::A1.into_class(), Some(PriorityClass::High));
        assert_eq!(Priority::A2.into_class(), Some(PriorityClass::High));
        assert_eq!(Priority::A3.into_class(), Some(PriorityClass::High));
        assert_eq!(Priority::B1.into_class(), Some(PriorityClass::High));

        assert_eq!(Priority::B2.into_class(), Some(PriorityClass::Medium));

        assert_eq!(Priority::B3.into_class(), Some(PriorityClass::Low));
        assert_eq!(Priority::C1.into_class(), Some(PriorityClass::Low));
        assert_eq!(Priority::C2.into_class(), Some(PriorityClass::Low));
        assert_eq!(Priority::C3.into_class(), Some(PriorityClass::Low));
    }
}
