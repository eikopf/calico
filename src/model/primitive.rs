//! Primitive types for the object model.

use chrono::NaiveDate;
use oxilangtag::LanguageTag;
use uuid::Uuid;

/// An HTTP method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method(pub(crate) http::Method);

/// A unique identifier (RFC 5545 §3.8.4.7).
///
/// Following the advice given in RFC 7986 §5.3, UUIDs (RFC 9562) should be
/// preferred over arbitrary string UIDs whenever possible, and especially
/// when generating new UIDs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Uid {
    Uuid(Uuid),
    String(Box<str>),
}

impl Uid {
    /// Returns `true` if the uid is [`Uuid`].
    ///
    /// [`Uuid`]: Uid::Uuid
    #[must_use]
    pub fn is_uuid(&self) -> bool {
        matches!(self, Self::Uuid(..))
    }
}

/// An RFC 5646 language tag.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Language(pub(crate) LanguageTag<String>);

/// An RFC 3986 uniform resource identifier (URI).
///
/// # Compatibility
///
/// This type is _not_ equivalent to [`http::Uri`], and instead strictly
/// represents a URI as defined in RFC 3986. Most other URI types can be
/// converted into this type, but the converse is not true: not all RFC 3986
/// URIs are representable by all URI types!
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Uri(pub(crate) iri_string::types::UriString);

/// The data of a BINARY property.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Binary {
    pub(crate) bytes: Vec<u8>,
}

/// Date-time or date value.
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Date(pub(crate) NaiveDate);

/// A TIME value (RFC 5545, §3.3.12).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Time<F = TimeFormat> {
    pub raw: RawTime,
    pub format: F,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawTime {
    pub hours: u8,
    pub minutes: u8,
    pub seconds: u8,
}

/// The format of a [`Time`], which may be local or absolute.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimeFormat {
    #[default]
    Local,
    Utc,
}

#[derive(Debug, Clone, Copy)]
pub enum Classification {
    Public,
    Private,
    Confidential,
}

#[derive(Debug, Clone, Copy)]
pub enum RelationshipType {
    Parent,
    Child,
    Sibling,
}

/// The data of an RFC 7986 IMAGE property.
#[derive(Debug, Clone)]
pub enum ImageData {
    Uri(Uri),
    Binary(Binary),
}

/// RFC 5545 §3.2.8
#[derive(Debug, Clone)]
pub struct FormatType;

/// DISPLAY parameter values (RFC 7986)
#[derive(Debug, Clone, Copy)]
pub enum DisplayType {
    Badge,
    Graphic,
    Fullsize,
    Thumbnail,
}

/// FEATURE parameter values (RFC 7986)
#[derive(Debug, Clone)]
pub enum FeatureType {
    Audio,
    Chat,
    Feed,
    Moderator,
    Phone,
    Screen,
    Video,
    Other(Box<str>),
}

#[derive(Debug, Clone, Copy)]
pub enum Status {
    Event(EventStatus),
    Todo(TodoStatus),
    Journal(JournalStatus),
}

#[derive(Debug, Clone, Copy)]
pub enum EventStatus {
    Tentative,
    Confirmed,
    Cancelled,
}

#[derive(Debug, Clone, Copy)]
pub enum TodoStatus {
    NeedsAction,
    Completed,
    InProcess,
    Cancelled,
}

#[derive(Debug, Clone, Copy)]
pub enum JournalStatus {
    Draft,
    Final,
    Cancelled,
}

#[derive(Debug, Clone, Copy)]
pub enum Transparency {
    Opaque,
    Transparent,
}

#[derive(Debug, Clone, Copy)]
pub enum CalendarUserType {
    Individual,
    Group,
    Resource,
    Room,
    Unknown,
}

#[derive(Debug, Clone, Copy)]
pub enum ParticipationRole {
    Chair,
    ReqParticipant,
    OptParticipant,
    NonParticipant,
}

#[derive(Debug, Clone, Copy)]
pub enum ParticipationStatus {
    NeedsAction,
    Accepted,
    Declined,
    Tentative,
    Delegated,
    Completed,
    InProcess,
}

#[derive(Debug, Clone, Copy)]
pub enum FreeBusyType {
    Free,
    Busy,
    BusyUnavailable,
    BusyTentative,
}

#[derive(Debug, Clone, Copy)]
pub enum AlarmAction {
    Audio,
    Display,
    Email,
}

#[derive(Debug, Clone, Copy)]
pub enum TriggerRelation {
    Start,
    End,
}

/// Period of time.
#[derive(Debug, Clone)]
pub enum Period {
    Explicit {
        start: DateTimeOrDate,
        end: DateTimeOrDate,
    },
    Start {
        start: DateTimeOrDate,
        duration: Duration,
    },
}

/// RDATE values can be DATE-TIME, DATE, or PERIOD.
#[derive(Debug, Clone)]
pub enum RDateValue {
    DateTime(DateTimeOrDate),
    Period(Period),
}

/// Duration type.
#[derive(Debug, Clone, Default)]
pub struct Duration {
    pub weeks: Option<u32>,
    pub days: Option<u32>,
    pub hours: Option<u32>,
    pub minutes: Option<u32>,
    pub seconds: Option<u32>,
    pub negative: bool,
}

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
