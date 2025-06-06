//! The iCalendar data model.

use chrono::{DateTime, NaiveDate, NaiveTime, Utc};
use css::Css3Color;
use iri_string::types::UriString;
use std::collections::HashMap;

pub mod css;

/// Top-level iCalendar object.
#[derive(Debug, Clone)]
pub struct Calendar {
    pub version: String,
    pub prodid: String,
    pub calscale: Option<String>,
    pub method: Option<String>,

    // RFC 7986 extensions
    pub name: Vec<Text>,
    pub description: Vec<Text>,
    pub uid: Option<String>,
    pub last_modified: Option<DateTime<Utc>>,
    pub url: Option<Uri>,
    pub categories: Vec<String>,
    pub refresh_interval: Option<Duration>,
    pub source: Option<Uri>,
    pub color: Option<Css3Color>,
    pub image: Vec<Image>,

    pub components: Vec<Component>,
    pub x_properties: HashMap<String, XProperty>,
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

/// Common fields shared by all calendar components except [`TimeZone`] and [`Alarm`].
#[derive(Debug, Clone)]
pub struct ComponentCore {
    pub uid: String,
    pub dtstamp: DateTime<Utc>,
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
    Uid(String),

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

/// The data of an RFC 7986 IMAGE property.
#[derive(Debug, Clone)]
pub enum ImageData {
    Uri(Uri),
    Binary {
        data: BinaryData,
        encoding: BinaryEncoding,
    },
}

/// RFC 7986 CONFERENCE property.
#[derive(Debug, Clone)]
pub struct Conference {
    pub uri: Uri,
    pub feature: Vec<FeatureType>,
    pub label: Option<Text>,
    pub language: Option<String>,
}

/// Text value with optional parameters.
#[derive(Debug, Clone)]
pub struct Text {
    pub value: String,
    pub language: Option<String>,
    pub alternate_representation: Option<Uri>,
}

/// An RFC 3986 URI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Uri(UriString);

/// X-Property (non-standard property).
#[derive(Debug, Clone)]
pub struct XProperty {
    pub value: PropertyValueType,
    pub parameters: HashMap<String, ParameterValue>,
}

/// Property value types as defined in RFC 5545.
#[derive(Debug, Clone)]
pub enum PropertyValueType {
    Binary(BinaryData),
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

/// The data of a BINARY property.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct BinaryData {
    bytes: Vec<u8>,
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
        encoding: BinaryEncoding,
    },
}

/// Binary encoding type.
#[derive(Debug, Clone, Copy)]
pub enum BinaryEncoding {
    Base64,
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
    pub range: Option<RecurrenceRange>,
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

#[derive(Debug, Clone, Copy)]
pub enum RecurrenceRange {
    ThisAndFuture,
}

#[derive(Debug, Clone, Copy)]
pub enum RelationshipType {
    Parent,
    Child,
    Sibling,
}

/// DISPLAY parameter values (RFC 7986)
#[derive(Debug, Clone, Copy)]
pub enum DisplayType {
    Badge,
    Graphic,
    Fullsize,
    Thumbnail,
}

/// FEATURE parameter values (RFC 7986)
#[derive(Debug, Clone, Copy)]
pub enum FeatureType {
    Audio,
    Chat,
    Feed,
    Moderator,
    Phone,
    Screen,
    Video,
}

// Display trait impls

impl std::fmt::Display for DisplayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DisplayType::Badge => write!(f, "BADGE"),
            DisplayType::Graphic => write!(f, "GRAPHIC"),
            DisplayType::Fullsize => write!(f, "FULLSIZE"),
            DisplayType::Thumbnail => write!(f, "THUMBNAIL"),
        }
    }
}

impl std::fmt::Display for FeatureType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FeatureType::Audio => write!(f, "AUDIO"),
            FeatureType::Chat => write!(f, "CHAT"),
            FeatureType::Feed => write!(f, "FEED"),
            FeatureType::Moderator => write!(f, "MODERATOR"),
            FeatureType::Phone => write!(f, "PHONE"),
            FeatureType::Screen => write!(f, "SCREEN"),
            FeatureType::Video => write!(f, "VIDEO"),
        }
    }
}

impl std::fmt::Display for Frequency {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Frequency::Secondly => write!(f, "SECONDLY"),
            Frequency::Minutely => write!(f, "MINUTELY"),
            Frequency::Hourly => write!(f, "HOURLY"),
            Frequency::Daily => write!(f, "DAILY"),
            Frequency::Weekly => write!(f, "WEEKLY"),
            Frequency::Monthly => write!(f, "MONTHLY"),
            Frequency::Yearly => write!(f, "YEARLY"),
        }
    }
}
