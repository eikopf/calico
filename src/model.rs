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

use css::Css3Color;
use primitive::{
    AlarmAction, Binary, CalendarUserType, ClassValue, Date, DateTime,
    DateTimeOrDate, DisplayType, Duration, EventStatus, FeatureType,
    FreeBusyType, Geo, ImageData, JournalStatus, Language, Method,
    ParticipationRole, ParticipationStatus, Period, RDate, RelationshipType,
    Status, Text, Time, TimeTransparency, TodoStatus, TriggerRelation, Uid,
    Uri, Utc, UtcOffset,
};
use property::{ConfProp, ImageProp, Prop, SeqLangProp, TextProp};
use rrule::RecurrenceRule;
use std::collections::HashMap;

pub mod css;
pub mod primitive;
pub mod property;
pub mod rrule;

// TODO: this top-level model requires a significant refactor. in particular,
// the idea of "sharing property groups" (as in e.g. DescriptiveProperties) is
// inadequate, since the same property may have a different multiplicity based
// on the component it occurs in. for example, the description property may
// occur 0 or 1 times in VEVENT, VTODO, and VALARM, but may occur any number of
// times in the VJOURNAL component.

// NOTE: some properties are (), but not omitted, because it's still legal to
// add arbitrary extra parameters to them. if this were not the case, the
// calendar_scale and version fields on Calendar could be removed.

/// Top-level iCalendar object.
#[derive(Debug, Clone)]
pub struct Calendar<S> {
    /// RFC 5545 §3.7.1 (always GREGORIAN in practice)
    pub calendar_scale: Prop<()>,
    /// RFC 5545 §3.7.2
    pub method: Option<Prop<Method<S>>>,
    /// RFC 5545 §3.7.3
    pub product_id: Prop<Text<S>>,
    /// RFC 5545 §3.7.4 (always 2.0 in practice)
    pub version: Prop<()>,

    /// RFC 7986 §5.1
    pub name: Vec<TextProp<S>>,
    /// RFC 7986 §5.2
    pub description: Vec<TextProp<S>>,
    /// RFC 7986 §5.3
    pub uid: Option<Prop<Uid<S>>>,
    /// RFC 7986 §5.4
    pub last_modified: Option<Prop<DateTime<Utc>>>,
    /// RFC 7986 §5.5
    pub url: Option<Prop<Uri<S>>>,
    /// RFC 7986 §5.6
    pub categories: Vec<TextProp<S>>,
    /// RFC 7986 §5.7
    pub refresh_interval: Option<Prop<Duration>>,
    /// RFC 7986 §5.8
    pub source: Option<Prop<Uri<S>>>,
    /// RFC 7986 §5.9
    pub color: Option<Prop<Css3Color>>,
    /// RFC 7986 §5.10
    pub image: Vec<ImageProp<S>>,
    /// RFC 7986 §5.11
    pub conference: Option<ConfProp<S>>,

    pub components: Vec<Component<S>>,
    pub extra_properties: HashMap<S, Prop<S>>,
}

/// Calendar component types
#[derive(Debug, Clone)]
pub enum Component<S> {
    Event(Event<S>),
    Todo(Todo<S>),
    Journal(Journal<S>),
    FreeBusy(FreeBusy<S>),
    TimeZone(TimeZone),
    Alarm(Alarm),
}

/// Common fields shared by all components except [`TimeZone`] and [`Alarm`].
#[derive(Debug, Clone)]
pub struct ComponentCore<S> {
    pub uid: Prop<Uid<S>>,
    pub datetime_stamp: Prop<DateTime<Utc>>,
    pub sequence: Prop<Option<u64>>,
    pub created: Prop<Option<DateTime<Utc>>>,
    pub last_modified: Prop<Option<DateTime<Utc>>>,
    pub x_properties: HashMap<S, XProperty>,
    pub iana_properties: HashMap<S, Property>,
}

/// Descriptive properties.
#[derive(Debug, Clone, Default)]
pub struct DescriptiveProperties<S> {
    pub summary: Option<TextProp<S>>,
    pub description: Option<TextProp<S>>,
    pub categories: Option<SeqLangProp<S>>,
    pub class: Option<Prop<ClassValue<S>>>,
    pub url: Option<Uri<S>>,
    pub attachments: Vec<Attachment>,
    pub contact: Option<Text<S>>,
    pub comments: Box<[TextProp<S>]>,
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
pub struct LocationProperties<S> {
    pub location: Option<Text<S>>,
    pub geo: Option<Geo>,
    pub resources: Vec<S>,
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
pub struct Event<S> {
    pub core: ComponentCore<S>,
    pub descriptive: DescriptiveProperties<S>,
    pub scheduling: SchedulingProperties,
    pub recurrence: RecurrenceProperties,
    pub location: LocationProperties<S>,
    pub time: TimeProperties,
    pub extensions: ExtensionProperties,

    // Event-specific properties
    pub dtend: Option<DateTimeOrDate>,
    pub status: Option<EventStatus>,
    pub priority: Option<u8>,
    pub transparency: Option<TimeTransparency>,
    pub related_to: Vec<RelatedTo>,
    pub alarms: Vec<Alarm>,
}

/// Todo component (VTODO).
#[derive(Debug, Clone)]
pub struct Todo<S> {
    pub core: ComponentCore<S>,
    pub descriptive: DescriptiveProperties<S>,
    pub scheduling: SchedulingProperties,
    pub recurrence: RecurrenceProperties,
    pub location: LocationProperties<S>,
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
pub struct Journal<S> {
    pub core: ComponentCore<S>,
    pub descriptive: DescriptiveProperties<S>,
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
pub struct FreeBusy<S> {
    pub core: ComponentCore<S>,

    // FreeBusy-specific properties.
    pub dtstart: Option<DateTime<Utc>>,
    pub dtend: Option<DateTime<Utc>>,
    pub organizer: Option<CalendarUser>,
    pub attendee: Option<CalendarUser>,
    pub url: Option<Uri<Box<str>>>,
    pub freebusy_periods: Vec<FreeBusyPeriod>,
    pub contact: Option<Text<S>>,
    pub comments: Vec<Text<S>>,
}

/// Time Zone component (VTIMEZONE).
#[derive(Debug, Clone)]
pub struct TimeZone {
    pub tzid: String,
    pub last_modified: Option<DateTime<Utc>>,
    pub tzurl: Option<Uri<Box<str>>>,
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
    pub tzname: Option<Text<Box<str>>>,
    pub recurrence_rule: Option<RecurrenceRule>,
    pub recurrence_dates: Vec<DateTime<Utc>>,
    pub comments: Vec<Text<Box<str>>>,
    pub x_properties: HashMap<String, XProperty>,
    pub iana_properties: HashMap<String, Property>,
}

/// Alarm component (VALARM).
#[derive(Debug, Clone)]
pub struct Alarm {
    pub action: AlarmAction<Box<str>>,
    pub trigger: AlarmTrigger,
    pub duration: Option<Duration>,
    pub repeat: Option<u32>,
    pub description: Option<Text<Box<str>>>,
    pub summary: Option<Text<Box<str>>>,
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
    Class(ClassValue<Box<str>>),
    Comment(Text<Box<str>>),
    Description(Text<Box<str>>),
    Geo(Geo),
    Location(Text<Box<str>>),
    PercentComplete(u8),
    Priority(u8),
    Resources(Vec<String>),
    Status(Status),
    Summary(Text<Box<str>>),

    // Date/Time properties
    Completed(DateTime<Utc>),
    DtEnd(DateTimeOrDate),
    Due(DateTimeOrDate),
    DtStart(DateTimeOrDate),
    Duration(Duration),
    FreeBusy(Vec<FreeBusyPeriod>),
    Transp(TimeTransparency),

    // Time Zone properties
    TzId(String),
    TzName(Text<Box<str>>),
    TzOffsetFrom(UtcOffset),
    TzOffsetTo(UtcOffset),
    TzUrl(Uri<Box<str>>),

    // Relationship properties
    Attendee(Attendee),
    Contact(Text<Box<str>>),
    Organizer(CalendarUser),
    RecurrenceId(RecurrenceId),
    RelatedTo(RelatedTo),
    Url(Uri<Box<str>>),
    Uid(Uid<Box<str>>),

    // Recurrence properties
    ExDate(Vec<DateTimeOrDate>),
    RDate(Vec<RDate>),
    RRule(RecurrenceRule),

    // Alarm properties
    Action(AlarmAction<Box<str>>),
    Repeat(u32),
    Trigger(AlarmTrigger),

    // Change management properties
    Created(DateTime<Utc>),
    DtStamp(DateTime<Utc>),
    LastModified(DateTime<Utc>),
    Sequence(u32),

    // RFC 7986 new properties
    Name(Text<Box<str>>),
    RefreshInterval(Duration),
    Source(Uri<Box<str>>),
    Color(String),
    Image(Image),
    Conference(Conference),

    // Miscellaneous properties
    RequestStatus(RequestStatus),
}

/// RFC 7986 IMAGE property.
#[derive(Debug, Clone)]
pub struct Image {
    pub data: ImageData<Box<str>>,
    pub fmttype: Option<String>,
    pub display: Vec<DisplayType<Box<str>>>,
    pub alternate_representation: Option<Uri<Box<str>>>,
}

/// RFC 7986 CONFERENCE property.
#[derive(Debug, Clone)]
pub struct Conference {
    pub uri: Uri<Box<str>>,
    pub feature: Vec<FeatureType<Box<str>>>,
    pub label: Option<String>,
    pub language: Option<Language<Box<str>>>,
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
    Date(Date),
    DateTime(DateTimeOrDate),
    Duration(Duration),
    Float(f64),
    Integer(i64),
    Period(Period),
    Recur(RecurrenceRule),
    Text(String),
    Time(Time),
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

/// Calendar user.
#[derive(Debug, Clone)]
pub struct CalendarUser {
    pub email: String,
    pub common_name: Option<String>,
    pub dir: Option<Uri<Box<str>>>,
    pub sent_by: Option<String>,
    pub language: Option<String>,
    pub email_param: Option<String>, // RFC 7986 extension
}

/// Attendee with participation info.
#[derive(Debug, Clone)]
pub struct Attendee {
    pub user: CalendarUser,
    pub cutype: Option<CalendarUserType<Box<str>>>,
    pub member: Vec<String>,
    pub role: Option<ParticipationRole<Box<str>>>,
    pub partstat: Option<ParticipationStatus<Box<str>>>,
    pub rsvp: Option<bool>,
    pub delegated_to: Vec<String>,
    pub delegated_from: Vec<String>,
}

/// Attachment.
#[derive(Debug, Clone)]
pub enum Attachment {
    Uri {
        uri: Uri<Box<str>>,
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
    pub fbtype: FreeBusyType<Box<str>>,
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
    pub reltype: Option<RelationshipType<Box<str>>>,
}

/// Request status.
#[derive(Debug, Clone)]
pub struct RequestStatus {
    pub status_code: String,
    pub status_description: String,
    pub exception_data: Option<String>,
    pub language: Option<String>,
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
