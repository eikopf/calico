//! The iCalendar object model.
//!
//! # Specification
//!
//! The primary document specifying the iCalendar object model is
//! [RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545), with some later alterations
//! specified in [RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986). Other relevant
//! documents include [RFC 6868](https://www.rfc-editor.org/rfc/rfc6868) and [RFC 7529](https://www.rfc-editor.org/rfc/rfc7529).
//!
//! ## Vestigial Features
//!
//! Some elements of these documents were intended to be variable, but in practice are almost never
//! changed from a single fixed value. Most notably, the CALSCALE (calendar scale) property is
//! essentially always GREGORIAN, and the VERSION (iCalendar version requirement) property is
//! essentially always 2.0.

use chrono::{DateTime, NaiveDate, NaiveTime, Utc};
use css::Css3Color;
use primitive::{
    AlarmAction, Binary, CalendarScale, CalendarUserType, Classification, DisplayType, EventStatus,
    FeatureType, FreeBusyType, Frequency, ImageData, JournalStatus, Language, Method,
    ParticipationRole, ParticipationStatus, RelationshipType, Status, TodoStatus, Transparency,
    TriggerRelation, Uid, Uri, VersionReq,
};
use property::{ConfProp, ImageProp, Prop, TextProp};
use std::collections::HashMap;

pub mod css;
pub mod primitive;
pub mod property;

/// Top-level iCalendar object.
#[derive(Debug, Clone)]
pub struct Calendar {
    /// RFC 5545 §3.7.1
    pub calendar_scale: Prop<CalendarScale>,
    /// RFC 5545 §3.7.2
    pub method: Option<Prop<Method>>,
    /// RFC 5545 §3.7.3
    pub product_id: Prop<Box<str>>,
    /// RFC 5545 §3.7.4
    pub version: Prop<VersionReq>,

    /// RFC 7986 §5.1
    pub name: Vec<TextProp>,
    /// RFC 7986 §5.2
    pub description: Vec<TextProp>,
    /// RFC 7986 §5.3
    pub uid: Option<Prop<Uid>>,
    /// RFC 7986 §5.4
    pub last_modified: Option<Prop<DateTime<Utc>>>,
    /// RFC 7986 §5.5
    pub url: Option<Prop<Uri>>,
    /// RFC 7986 §5.6
    pub categories: Vec<TextProp>,
    /// RFC 7986 §5.7
    pub refresh_interval: Option<Prop<Duration>>,
    /// RFC 7986 §5.8
    pub source: Option<Prop<Uri>>,
    /// RFC 7986 §5.9
    pub color: Option<Prop<Css3Color>>,
    /// RFC 7986 §5.10
    pub image: Vec<ImageProp>,
    /// RFC 7986 §5.11
    pub conference: Option<ConfProp>,

    pub components: Vec<Component>,
    pub extra_properties: HashMap<Box<str>, Prop<Box<str>>>,
}

/// Calendar component types
#[derive(Debug, Clone)]
pub enum Component {
    Event(Event),
    Todo(Todo),
    Journal(Journal),
    FreeBusy(FreeBusy),
    TimeZone(TimeZone),
    Alarm(Alarm),
}

/// Common fields shared by all components except [`TimeZone`] and [`Alarm`].
#[derive(Debug, Clone)]
pub struct ComponentCore {
    pub uid: Prop<Uid>,
    pub datetime_stamp: Prop<DateTime<Utc>>,
    pub sequence: Option<u32>,
    pub created: Option<DateTime<Utc>>,
    pub last_modified: Option<DateTime<Utc>>,
    pub x_properties: HashMap<String, XProperty>,
    pub iana_properties: HashMap<String, Property>,
}

/// Descriptive properties.
#[derive(Debug, Clone, Default)]
pub struct DescriptiveProperties {
    pub summary: Option<Text>,
    pub description: Option<Text>,
    pub categories: Vec<String>,
    pub class: Option<Classification>,
    pub url: Option<Uri>,
    pub attachments: Vec<Attachment>,
    pub contact: Option<Text>,
    pub comments: Vec<Text>,
}

/// Scheduling properties.
#[derive(Debug, Clone, Default)]
pub struct SchedulingProperties {
    pub organizer: Option<CalendarUser>,
    pub attendees: Vec<Attendee>,
}

/// Recurrence properties.
#[derive(Debug, Clone, Default)]
pub struct RecurrenceProperties {
    pub recurrence_rule: Option<RecurrenceRule>,
    pub recurrence_dates: Vec<DateTimeOrDate>,
    pub exception_dates: Vec<DateTimeOrDate>,
    pub recurrence_id: Option<RecurrenceId>,
}

/// Geographic and location properties.
#[derive(Debug, Clone, Default)]
pub struct LocationProperties {
    pub location: Option<Text>,
    pub geo: Option<Geo>,
    pub resources: Vec<String>,
}

/// Time-based properties.
#[derive(Debug, Clone, Default)]
pub struct TimeProperties {
    pub dtstart: Option<DateTimeOrDate>,
    pub duration: Option<Duration>,
}

/// RFC 7986 component extension properties.
#[derive(Debug, Clone, Default)]
pub struct ExtensionProperties {
    pub color: Option<Css3Color>,
    pub image: Vec<Image>,
    pub conference: Vec<Conference>,
}

/// Event component (VEVENT).
#[derive(Debug, Clone)]
pub struct Event {
    pub core: ComponentCore,
    pub descriptive: DescriptiveProperties,
    pub scheduling: SchedulingProperties,
    pub recurrence: RecurrenceProperties,
    pub location: LocationProperties,
    pub time: TimeProperties,
    pub extensions: ExtensionProperties,

    // Event-specific properties
    pub dtend: Option<DateTimeOrDate>,
    pub status: Option<EventStatus>,
    pub priority: Option<u8>,
    pub transparency: Option<Transparency>,
    pub related_to: Vec<RelatedTo>,
    pub alarms: Vec<Alarm>,
}

/// Todo component (VTODO).
#[derive(Debug, Clone)]
pub struct Todo {
    pub core: ComponentCore,
    pub descriptive: DescriptiveProperties,
    pub scheduling: SchedulingProperties,
    pub recurrence: RecurrenceProperties,
    pub location: LocationProperties,
    pub time: TimeProperties,
    pub extensions: ExtensionProperties,

    // Todo-specific properties
    pub due: Option<DateTimeOrDate>,
    pub status: Option<TodoStatus>,
    pub priority: Option<u8>,
    pub percent_complete: Option<u8>,
    pub completed: Option<DateTime<Utc>>,
    pub related_to: Vec<RelatedTo>,
    pub alarms: Vec<Alarm>,
}

/// Journal component (VJOURNAL).
#[derive(Debug, Clone)]
pub struct Journal {
    pub core: ComponentCore,
    pub descriptive: DescriptiveProperties,
    pub scheduling: SchedulingProperties,
    pub recurrence: RecurrenceProperties,

    // RFC 7986 extension properties.
    pub color: Option<String>,
    pub image: Vec<Image>,

    // Journal-specific properties.
    pub dtstart: Option<DateTimeOrDate>,
    pub status: Option<JournalStatus>,
    pub related_to: Vec<RelatedTo>,
}

/// Free/Busy component (VFREEBUSY).
#[derive(Debug, Clone)]
pub struct FreeBusy {
    pub core: ComponentCore,

    // FreeBusy-specific properties.
    pub dtstart: Option<DateTime<Utc>>,
    pub dtend: Option<DateTime<Utc>>,
    pub organizer: Option<CalendarUser>,
    pub attendee: Option<CalendarUser>,
    pub url: Option<Uri>,
    pub freebusy_periods: Vec<FreeBusyPeriod>,
    pub contact: Option<Text>,
    pub comments: Vec<Text>,
}

/// Time Zone component (VTIMEZONE).
#[derive(Debug, Clone)]
pub struct TimeZone {
    pub tzid: String,
    pub last_modified: Option<DateTime<Utc>>,
    pub tzurl: Option<Uri>,
    pub standard_time: Vec<TimeZoneRule>,
    pub daylight_time: Vec<TimeZoneRule>,
    pub x_properties: HashMap<String, XProperty>,
    pub iana_properties: HashMap<String, Property>,
}

/// Time zone rule (STANDARD/DAYLIGHT).
#[derive(Debug, Clone)]
pub struct TimeZoneRule {
    pub dtstart: DateTime<Utc>,
    pub tzoffsetfrom: UtcOffset,
    pub tzoffsetto: UtcOffset,
    pub tzname: Option<Text>,
    pub recurrence_rule: Option<RecurrenceRule>,
    pub recurrence_dates: Vec<DateTime<Utc>>,
    pub comments: Vec<Text>,
    pub x_properties: HashMap<String, XProperty>,
    pub iana_properties: HashMap<String, Property>,
}

/// Alarm component (VALARM).
#[derive(Debug, Clone)]
pub struct Alarm {
    pub action: AlarmAction,
    pub trigger: AlarmTrigger,
    pub duration: Option<Duration>,
    pub repeat: Option<u32>,
    pub description: Option<Text>,
    pub summary: Option<Text>,
    pub attendees: Vec<CalendarUser>,
    pub attachments: Vec<Attachment>,
    pub x_properties: HashMap<String, XProperty>,
    pub iana_properties: HashMap<String, Property>,
}

/// Properties.
#[derive(Debug, Clone)]
pub enum Property {
    // Descriptive properties
    Attach(Attachment),
    Categories(Vec<String>),
    Class(Classification),
    Comment(Text),
    Description(Text),
    Geo(Geo),
    Location(Text),
    PercentComplete(u8),
    Priority(u8),
    Resources(Vec<String>),
    Status(Status),
    Summary(Text),

    // Date/Time properties
    Completed(DateTime<Utc>),
    DtEnd(DateTimeOrDate),
    Due(DateTimeOrDate),
    DtStart(DateTimeOrDate),
    Duration(Duration),
    FreeBusy(Vec<FreeBusyPeriod>),
    Transp(Transparency),

    // Time Zone properties
    TzId(String),
    TzName(Text),
    TzOffsetFrom(UtcOffset),
    TzOffsetTo(UtcOffset),
    TzUrl(Uri),

    // Relationship properties
    Attendee(Attendee),
    Contact(Text),
    Organizer(CalendarUser),
    RecurrenceId(RecurrenceId),
    RelatedTo(RelatedTo),
    Url(Uri),
    Uid(Uid),

    // Recurrence properties
    ExDate(Vec<DateTimeOrDate>),
    RDate(Vec<RDateValue>),
    RRule(RecurrenceRule),

    // Alarm properties
    Action(AlarmAction),
    Repeat(u32),
    Trigger(AlarmTrigger),

    // Change management properties
    Created(DateTime<Utc>),
    DtStamp(DateTime<Utc>),
    LastModified(DateTime<Utc>),
    Sequence(u32),

    // RFC 7986 new properties
    Name(Text),
    RefreshInterval(Duration),
    Source(Uri),
    Color(String),
    Image(Image),
    Conference(Conference),

    // Miscellaneous properties
    RequestStatus(RequestStatus),
}

/// RFC 7986 IMAGE property.
#[derive(Debug, Clone)]
pub struct Image {
    pub data: ImageData,
    pub fmttype: Option<String>,
    pub display: Vec<DisplayType>,
    pub alternate_representation: Option<Uri>,
}

/// RFC 7986 CONFERENCE property.
#[derive(Debug, Clone)]
pub struct Conference {
    pub uri: Uri,
    pub feature: Vec<FeatureType>,
    pub label: Option<String>,
    pub language: Option<Language>,
}

/// A text value.
#[derive(Debug, Clone)]
pub struct Text {
    pub contents: String,
    pub language: Option<Language>,
    pub alternate_representation: Option<Uri>,
}

/// X-Property (non-standard property).
#[derive(Debug, Clone)]
pub struct XProperty {
    pub value: PropertyValueType,
    pub parameters: HashMap<String, ParameterValue>,
}

/// Property value types as defined in RFC 5545.
#[derive(Debug, Clone)]
pub enum PropertyValueType {
    Binary(Binary),
    Boolean(bool),
    CalAddress(String),
    Date(NaiveDate),
    DateTime(DateTimeOrDate),
    Duration(Duration),
    Float(f64),
    Integer(i64),
    Period(Period),
    Recur(RecurrenceRule),
    Text(String),
    Time(NaiveTime),
    Uri(String),
    UtcOffset(UtcOffset),
}

/// Parameter values.
#[derive(Debug, Clone)]
pub enum ParameterValue {
    Text(String),
    Uri(String),
    CalAddress(String),
    Boolean(bool),
    Integer(i64),
}

/// Date-time or date value.
#[derive(Debug, Clone)]
pub enum DateTimeOrDate {
    DateTime(DateTime<Utc>),
    Date(NaiveDate),
    FloatingDateTime(chrono::NaiveDateTime),
    LocalDateTime {
        datetime: chrono::NaiveDateTime,
        tzid: String,
    },
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

/// Calendar user.
#[derive(Debug, Clone)]
pub struct CalendarUser {
    pub email: String,
    pub common_name: Option<String>,
    pub dir: Option<Uri>,
    pub sent_by: Option<String>,
    pub language: Option<String>,
    pub email_param: Option<String>, // RFC 7986 extension
}

/// Attendee with participation info.
#[derive(Debug, Clone)]
pub struct Attendee {
    pub user: CalendarUser,
    pub cutype: Option<CalendarUserType>,
    pub member: Vec<String>,
    pub role: Option<ParticipationRole>,
    pub partstat: Option<ParticipationStatus>,
    pub rsvp: Option<bool>,
    pub delegated_to: Vec<String>,
    pub delegated_from: Vec<String>,
}

/// Attachment.
#[derive(Debug, Clone)]
pub enum Attachment {
    Uri {
        uri: Uri,
        fmttype: Option<String>,
    },
    Binary {
        data: Vec<u8>,
        fmttype: Option<String>,
    },
}

/// Free/busy period.
#[derive(Debug, Clone)]
pub struct FreeBusyPeriod {
    pub period: Period,
    pub fbtype: FreeBusyType,
}

/// Recurrence ID with optional range.
#[derive(Debug, Clone)]
pub struct RecurrenceId {
    pub datetime: DateTimeOrDate,
    pub this_and_future: bool,
}

/// Related-to property.
#[derive(Debug, Clone)]
pub struct RelatedTo {
    pub uid: String,
    pub reltype: Option<RelationshipType>,
}

/// Request status.
#[derive(Debug, Clone)]
pub struct RequestStatus {
    pub status_code: String,
    pub status_description: String,
    pub exception_data: Option<String>,
    pub language: Option<String>,
}

/// Recurrence rule.
#[derive(Debug, Clone)]
pub struct RecurrenceRule {
    pub freq: Frequency,
    pub until: Option<DateTimeOrDate>,
    pub count: Option<u32>,
    pub interval: Option<u32>,
    pub by_second: Vec<u8>,
    pub by_minute: Vec<u8>,
    pub by_hour: Vec<u8>,
    pub by_day: Vec<WeekdayNum>,
    pub by_month_day: Vec<i8>,
    pub by_year_day: Vec<i16>,
    pub by_week_no: Vec<i8>,
    pub by_month: Vec<u8>,
    pub by_set_pos: Vec<i16>,
    pub week_start: Option<chrono::Weekday>,
}

/// Weekday with optional occurrence number.
#[derive(Debug, Clone)]
pub struct WeekdayNum {
    pub weekday: chrono::Weekday,
    pub occurrence: Option<i8>,
}

/// Alarm trigger.
#[derive(Debug, Clone)]
pub enum AlarmTrigger {
    Duration {
        duration: Duration,
        related: Option<TriggerRelation>,
    },
    DateTime(DateTime<Utc>),
}
