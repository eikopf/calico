//! Primitive types for the object model.

use iri_string::types::UriString;
use oxilangtag::LanguageTag;

/// The calendar scale, which is almost always Gregorian (RFC 5545 §3.7.1).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum CalendarScale {
    #[default]
    Gregorian,
    Other(String),
}

/// An HTTP method.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method(http::Method);

/// The iCalendar version requirements on a [`Calendar`] (RFC 5545, §3.7.4).
#[derive(Debug, Clone)]
pub enum VersionReq {
    Latest(Version),
    Bounded { min: Version, max: Version },
}

/// A version value, which is almost always 2.0 (RFC 5545 §3.7.4).
#[derive(Debug, Clone, Default)]
pub enum Version {
    /// Version 2.0.
    #[default]
    Rfc5545,
    /// Any version other than 2.0.
    Other(String),
}

/// A unique identifier (RFC 5545 §3.8.4.7).
///
/// Following the advice given in RFC 7986 §5.3, UUIDs (RFC 9562) should be
/// preferred over arbitrary string UIDs whenever possible, and especially
/// when generating new UIDs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Uid {
    Uuid(uuid::Uuid),
    String(Box<str>),
}

/// An RFC 5646 language tag.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Language(LanguageTag<String>);

/// An RFC 3986 uniform resource identifier (URI).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Uri(UriString);

/// The data of a BINARY property.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Binary {
    bytes: Vec<u8>,
}

/// The frequency of a recurrence rule.
#[derive(Debug, Clone, Copy)]
pub enum Frequency {
    Secondly,
    Minutely,
    Hourly,
    Daily,
    Weekly,
    Monthly,
    Yearly,
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
