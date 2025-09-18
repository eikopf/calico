//! Model types for calendar components.

use std::fmt::Debug;

pub(crate) use internal::{PropertyTable, RawValue, StaticProp, UnknownPropSeq};

use crate::{
    model::primitive::{ProximityValue, UnknownAction},
    parser::escaped::Equiv,
};

use super::{
    css::Css3Color,
    primitive::{
        AttachValue, AudioAction, CalAddress, ClassValue, CompletionPercentage, DateTime,
        DateTimeOrDate, DisplayAction, Duration, EmailAction, EventStatus, ExDateSeq, Geo,
        ImageData, Integer, JournalStatus, Method, Period, Priority, RDateSeq, RequestStatus, Text,
        TimeTransparency, TodoStatus, TzId, Uid, Uri, Utc, UtcOffset,
    },
    property::{
        AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams, ImageParams, LangParams,
        MultiProp, OrganizerParams, Prop, RecurrenceIdParams, RelTypeParams, TextParams,
        TriggerPropMut, TriggerPropRef,
    },
    rrule::RRule,
    table::HashCaseless,
};

mod internal;

macro_rules! mandatory_accessors {
    ($([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> &$ret {
                self.props.get_known(StaticProp::$key).unwrap().try_into().unwrap()
            }

            pub fn $name_mut(&mut self) -> &mut $ret {
                self.props.get_known_mut(StaticProp::$key).unwrap().try_into().unwrap()
            }
        )*
    };
}

macro_rules! optional_accessors {
    ($([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&$ret> {
                match self.props.get_known(StaticProp::$key) {
                    Some(raw_value) => Some(raw_value.try_into().unwrap()),
                    None => None,
                }
            }

            pub fn $name_mut(&mut self) -> Option<&mut $ret> {
                match self.props.get_known_mut(StaticProp::$key) {
                    Some(raw_value) => Some(raw_value.try_into().unwrap()),
                    None => None,
                }
            }
        )*
    };
}

macro_rules! seq_accessors {
    ($([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&[$ret]> {
                match self.props.get_known(StaticProp::$key) {
                    Some(raw_value) => {
                        let vs: &Vec<_> = raw_value.try_into().unwrap();
                        Some(vs.as_slice())
                    },
                    None => None,
                }
            }

            pub fn $name_mut(&mut self) -> Option<&mut Vec<$ret>> {
                match self.props.get_known_mut(StaticProp::$key) {
                    Some(raw_value) => {
                        let vs: &mut Vec<_> = raw_value.try_into().unwrap();
                        Some(vs)
                    },
                    None => None,
                }
            }
        )*
    };
}

/// An iCalendar object (RFC 5545 §3.4).
#[derive(Debug, Clone)]
pub struct Calendar<S> {
    props: PropertyTable<S>,
    components: Vec<Component<S>>,
}

impl<S> Calendar<S> {
    pub(crate) const fn new(props: PropertyTable<S>, components: Vec<Component<S>>) -> Self {
        Self { props, components }
    }

    pub const fn components(&self) -> &[Component<S>] {
        self.components.as_slice()
    }

    pub const fn components_mut(&mut self) -> &mut Vec<Component<S>> {
        &mut self.components
    }
}

impl<S> Calendar<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [ProdId, prod_id, prod_id_mut, Prop<S, Text<S>>],
        [Version, version, version_mut, Prop<S, ()>],
    }

    optional_accessors! {
        [CalScale, scale, scale_mut, Prop<S, ()>],
        [Method, method, method_mut, Prop<S, Method<S>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
        [RefreshInterval, refresh_interval, refresh_interval_mut, Prop<S, Duration>],
        [Source, source, source_mut, Prop<S, Uri<S>>],
        [Color, color, color_mut, Prop<S, Css3Color>],
    }

    seq_accessors! {
        [Name, names, names_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Description, description, description_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Categories, categories, categories_mut, MultiProp<S, Vec<Text<S>>, LangParams<S>>],
        [Image, images, images_mut, MultiProp<S, ImageData<S>, ImageParams<S>>],
    }
}

/// An immediate subcomponent of a [`Calendar`].
#[derive(Debug, Clone)]
pub enum Component<S> {
    Event(Event<S>),
    Todo(Todo<S>),
    Journal(Journal<S>),
    FreeBusy(FreeBusy<S>),
    TimeZone(TimeZone<S>),
    Other(OtherComponent<S>),
}

/// A VEVENT component (RFC 5545 §3.6.1).
#[derive(Debug, Clone)]
pub struct Event<S> {
    props: PropertyTable<S>,
    alarms: Vec<Alarm<S>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventTerminationRef<'a, S> {
    End(&'a Prop<S, DateTimeOrDate, DtParams<S>>),
    Duration(&'a Prop<S, Duration>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum EventTerminationMut<'a, S> {
    End(&'a mut Prop<S, DateTimeOrDate, DtParams<S>>),
    Duration(&'a mut Prop<S, Duration>),
}

impl<S> Event<S> {
    pub(crate) const fn new(props: PropertyTable<S>, alarms: Vec<Alarm<S>>) -> Self {
        Self { props, alarms }
    }

    pub const fn alarms(&self) -> &[Alarm<S>] {
        self.alarms.as_slice()
    }

    pub const fn alarms_mut(&mut self) -> &mut Vec<Alarm<S>> {
        &mut self.alarms
    }
}

impl<S> Event<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    pub fn termination(&self) -> Option<EventTerminationRef<'_, S>> {
        self.end()
            .map(EventTerminationRef::End)
            .or_else(|| self.duration().map(EventTerminationRef::Duration))
    }

    pub fn termination_mut(&mut self) -> Option<EventTerminationMut<'_, S>> {
        if self.end().is_some() {
            self.end_mut().map(EventTerminationMut::End)
        } else if self.duration().is_some() {
            self.duration_mut().map(EventTerminationMut::Duration)
        } else {
            None
        }
    }

    optional_accessors! {
        [Status, status, status_mut, Prop<S, EventStatus>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [DtEnd, end, end_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Class, class, class_mut, Prop<S, ClassValue<S>>],
        [Created, created, created_mut, Prop<S, DateTime<Utc>>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Geo, geo, geo_mut, Prop<S, Geo>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [Location, location, location_mut, Prop<S, Text<S>, TextParams<S>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [Priority, priority, priority_mut, Prop<S, Priority>],
        [Sequence, sequence_number, sequence_number_mut, Prop<S, Integer>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
        [Transp, transparency, transparency_mut, Prop<S, TimeTransparency>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
        [RecurId, recurrence_id, recurrence_id_mut, Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>],
        [Color, color, color_mut, Prop<S, Css3Color>],
    }

    seq_accessors! {
        [Attach, attachments, attachments_mut, MultiProp<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, MultiProp<S, Vec<Text<S>>, LangParams<S>>],
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [RRule, rrule, rrule_mut, MultiProp<S, Box<RRule>>],
        [ExDate, exception_dates, exception_dates_mut, MultiProp<S, ExDateSeq, DtParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, MultiProp<S, RequestStatus<S>, LangParams<S>>],
        [RelatedTo, relateds, relateds_mut, MultiProp<S, Text<S>, RelTypeParams<S>>],
        [Resources, resources, resources_mut, MultiProp<S, Vec<Text<S>>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, MultiProp<S, RDateSeq, DtParams<S>>],
        [Conference, conferences, conferences_mut, MultiProp<S, Uri<S>, ConfParams<S>>],
        [Image, images, images_mut, MultiProp<S, ImageData<S>, ImageParams<S>>],
    }
}

/// A VTODO component (RFC 5545 §3.6.2).
#[derive(Debug, Clone)]
pub struct Todo<S> {
    props: PropertyTable<S>,
    alarms: Vec<Alarm<S>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TodoTerminationRef<'a, S> {
    Due(&'a Prop<S, DateTimeOrDate, DtParams<S>>),
    Duration(&'a Prop<S, Duration>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TodoTerminationMut<'a, S> {
    Due(&'a mut Prop<S, DateTimeOrDate, DtParams<S>>),
    Duration(&'a mut Prop<S, Duration>),
}

impl<S> Todo<S> {
    pub(crate) const fn new(props: PropertyTable<S>, alarms: Vec<Alarm<S>>) -> Self {
        Self { props, alarms }
    }

    pub const fn alarms(&self) -> &[Alarm<S>] {
        self.alarms.as_slice()
    }

    pub const fn alarms_mut(&mut self) -> &mut Vec<Alarm<S>> {
        &mut self.alarms
    }
}

impl<S> Todo<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    pub fn termination(&self) -> Option<TodoTerminationRef<'_, S>> {
        self.due()
            .map(TodoTerminationRef::Due)
            .or_else(|| self.duration().map(TodoTerminationRef::Duration))
    }

    pub fn termination_mut(&mut self) -> Option<TodoTerminationMut<'_, S>> {
        if self.due().is_some() {
            self.due_mut().map(TodoTerminationMut::Due)
        } else if self.duration().is_some() {
            self.duration_mut().map(TodoTerminationMut::Duration)
        } else {
            None
        }
    }

    optional_accessors! {
        [Status, status, status_mut, Prop<S, TodoStatus>],
        [Class, class, class_mut, Prop<S, ClassValue<S>>],
        [DtCompleted, completed, completed_mut, Prop<S, DateTime<Utc>>],
        [Created, created, created_mut, Prop<S, DateTime<Utc>>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [DtDue, due, due_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Geo, geo, geo_mut, Prop<S, Geo>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [Location, location, location_mut, Prop<S, Text<S>, TextParams<S>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [PercentComplete, percent, percent_mut, Prop<S, CompletionPercentage>],
        [Priority, priority, priority_mut, Prop<S, Priority>],
        [RecurId, recurrence_id, recurrence_id_mut, Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>],
        [Sequence, sequence_number, sequence_number_mut, Prop<S, Integer>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
        [Color, color, color_mut, Prop<S, Css3Color>],
    }

    seq_accessors! {
        [Attach, attachments, attachments_mut, MultiProp<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, MultiProp<S, Vec<Text<S>>, LangParams<S>>],
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [RRule, rrule, rrule_mut, MultiProp<S, Box<RRule>>],
        [ExDate, exception_dates, exception_dates_mut, MultiProp<S, ExDateSeq, DtParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, MultiProp<S, RequestStatus<S>, LangParams<S>>],
        [RelatedTo, relateds, relateds_mut, MultiProp<S, Text<S>, RelTypeParams<S>>],
        [Resources, resources, resources_mut, MultiProp<S, Vec<Text<S>>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, MultiProp<S, RDateSeq, DtParams<S>>],
        [Conference, conferences, conferences_mut, MultiProp<S, Uri<S>, ConfParams<S>>],
        [Image, images, images_mut, MultiProp<S, ImageData<S>, ImageParams<S>>],
    }
}

// TODO: VALARM should admit a tail of VLOCATION components when the PROXIMITY property is
// present (RFC 9074 §8).

/// A VALARM component (RFC 5545 §3.6.6).
#[derive(Debug, Clone)]
pub enum Alarm<S> {
    Audio(AudioAlarm<S>),
    Display(DisplayAlarm<S>),
    Email(EmailAlarm<S>),
    Other(OtherAlarm<S>),
}

impl<S> Alarm<S> {
    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        match self {
            Alarm::Audio(alarm) => alarm.subcomponents(),
            Alarm::Display(alarm) => alarm.subcomponents(),
            Alarm::Email(alarm) => alarm.subcomponents(),
            Alarm::Other(alarm) => alarm.subcomponents(),
        }
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        match self {
            Alarm::Audio(alarm) => alarm.subcomponents_mut(),
            Alarm::Display(alarm) => alarm.subcomponents_mut(),
            Alarm::Email(alarm) => alarm.subcomponents_mut(),
            Alarm::Other(alarm) => alarm.subcomponents_mut(),
        }
    }
}

macro_rules! alarm_accessors {
    () => {
        pub fn duration_and_repeat(&self) -> Option<(&Prop<S, Duration>, &Prop<S, Integer>)> {
            let duration = self.duration();
            let repeat = self.repeat();

            match (duration, repeat) {
                (Some(duration), Some(repeat)) => Some((duration, repeat)),
                (Some(_), None) => panic!("DURATION without REPEAT in VALARM"),
                (None, Some(_)) => panic!("REPEAT without DURATION in VALARM"),
                (None, None) => None,
            }
        }

        pub fn trigger(&self) -> TriggerPropRef<'_, S> {
            self.props
                .get_known(StaticProp::Trigger)
                .unwrap()
                .try_into()
                .unwrap()
        }

        pub fn trigger_mut(&mut self) -> TriggerPropMut<'_, S> {
            self.props
                .get_known_mut(StaticProp::Trigger)
                .unwrap()
                .try_into()
                .unwrap()
        }
    };
}

/// A VALARM with the AUDIO action.
#[derive(Debug, Clone)]
pub struct AudioAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

impl<S> AudioAlarm<S> {
    pub(crate) const fn new(
        props: PropertyTable<S>,
        subcomponents: Vec<OtherComponent<S>>,
    ) -> Self {
        Self {
            props,
            subcomponents,
        }
    }

    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> AudioAlarm<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [Action, action, action_mut, Prop<S, AudioAction>],
    }

    optional_accessors! {
        [Attach, attachment, attachment_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Repeat, repeat, repeat_mut, Prop<S, Integer>],
        [Acknowledged, acknowleded, acknowleded_mut, Prop<S, DateTime<Utc>>],
        [Proximity, proximity, proximity_mut, Prop<S, ProximityValue<S>>],
    }

    alarm_accessors!();
}

/// A VALARM with the DISPLAY action.
#[derive(Debug, Clone)]
pub struct DisplayAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

impl<S> DisplayAlarm<S> {
    pub(crate) const fn new(
        props: PropertyTable<S>,
        subcomponents: Vec<OtherComponent<S>>,
    ) -> Self {
        Self {
            props,
            subcomponents,
        }
    }

    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> DisplayAlarm<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [Action, action, action_mut, Prop<S, DisplayAction>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
    }

    optional_accessors! {
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Repeat, repeat, repeat_mut, Prop<S, Integer>],
        [Acknowledged, acknowleded, acknowleded_mut, Prop<S, DateTime<Utc>>],
        [Proximity, proximity, proximity_mut, Prop<S, ProximityValue<S>>],
    }

    alarm_accessors!();
}

/// A VALARM with the EMAIL action.
#[derive(Debug, Clone)]
pub struct EmailAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

impl<S> EmailAlarm<S> {
    pub(crate) const fn new(
        props: PropertyTable<S>,
        subcomponents: Vec<OtherComponent<S>>,
    ) -> Self {
        Self {
            props,
            subcomponents,
        }
    }

    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> EmailAlarm<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [Action, action, action_mut, Prop<S, EmailAction>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
    }

    optional_accessors! {
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Repeat, repeat, repeat_mut, Prop<S, Integer>],
        [Acknowledged, acknowleded, acknowleded_mut, Prop<S, DateTime<Utc>>],
        [Proximity, proximity, proximity_mut, Prop<S, ProximityValue<S>>],
    }

    seq_accessors! {
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Attach, attachments, attachments_mut, MultiProp<S, AttachValue<S>, AttachParams<S>>],
    }

    alarm_accessors!();
}

/// A VALARM with an action other than AUDIO, DISPLAY, or EMAIL.
#[derive(Debug, Clone)]
pub struct OtherAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

impl<S> OtherAlarm<S> {
    pub(crate) const fn new(
        props: PropertyTable<S>,
        subcomponents: Vec<OtherComponent<S>>,
    ) -> Self {
        Self {
            props,
            subcomponents,
        }
    }

    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> OtherAlarm<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [Action, action, action_mut, Prop<S, UnknownAction<S>>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
    }

    optional_accessors! {
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Repeat, repeat, repeat_mut, Prop<S, Integer>],
        [Acknowledged, acknowleded, acknowleded_mut, Prop<S, DateTime<Utc>>],
        [Proximity, proximity, proximity_mut, Prop<S, ProximityValue<S>>],
    }

    seq_accessors! {
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Attach, attachments, attachments_mut, MultiProp<S, AttachValue<S>, AttachParams<S>>],
    }

    alarm_accessors!();
}

/// A VJOURNAL component (RFC 5545 §3.6.3).
#[derive(Debug, Clone)]
pub struct Journal<S> {
    props: PropertyTable<S>,
}

impl<S> Journal<S> {
    pub(crate) const fn new(props: PropertyTable<S>) -> Self {
        Self { props }
    }
}

impl<S> Journal<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    optional_accessors! {
        [Status, status, status_mut, Prop<S, JournalStatus>],
        [Class, class, class_mut, Prop<S, ClassValue<S>>],
        [Created, created, created_mut, Prop<S, DateTime<Utc>>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [RecurId, recurrence_id, recurrence_id_mut, Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>],
        [Sequence, sequence_number, sequence_number_mut, Prop<S, Integer>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
    }

    seq_accessors! {
        [Attach, attachments, attachments_mut, MultiProp<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, MultiProp<S, Vec<Text<S>>, LangParams<S>>],
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Description, descriptions, descriptions_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [ExDate, exception_dates, exception_dates_mut, MultiProp<S, ExDateSeq, DtParams<S>>],
        [RelatedTo, relateds, relateds_mut, MultiProp<S, Text<S>, RelTypeParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, MultiProp<S, RDateSeq, DtParams<S>>],
        [RRule, rrule, rrule_mut, MultiProp<S, Box<RRule>>],
        [RequestStatus, request_statuses, request_statuses_mut, MultiProp<S, RequestStatus<S>, LangParams<S>>],
    }
}

/// A VFREEBUSY component (RFC 5545 §3.6.4).
#[derive(Debug, Clone)]
pub struct FreeBusy<S> {
    props: PropertyTable<S>,
}

impl<S> FreeBusy<S> {
    pub(crate) const fn new(props: PropertyTable<S>) -> Self {
        Self { props }
    }
}

impl<S> FreeBusy<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    optional_accessors! {
        [Contact, contact, contact_mut, Prop<S, Text<S>, TextParams<S>>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [DtEnd, end, end_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
    }

    seq_accessors! {
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [FreeBusy, free_busy_periods, free_busy_periods_mut, MultiProp<S, Vec<Period>, FBTypeParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, MultiProp<S, RequestStatus<S>, LangParams<S>>],
    }
}

/// A VTIMEZONE component (RFC 5545 §3.6.5).
#[derive(Debug, Clone)]
pub struct TimeZone<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<TzRule<S>>,
}

impl<S> TimeZone<S> {
    pub(crate) const fn new(props: PropertyTable<S>, subcomponents: Vec<TzRule<S>>) -> Self {
        Self {
            props,
            subcomponents,
        }
    }

    pub const fn rules(&self) -> &[TzRule<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn rules_mut(&mut self) -> &mut Vec<TzRule<S>> {
        &mut self.subcomponents
    }
}

impl<S> TimeZone<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [TzId, id, id_mut, Prop<S, TzId<S>>],
    }

    optional_accessors! {
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [TzUrl, url, url_mut, Prop<S, Uri<S>>],
    }
}

/// A STANDARD or DAYLIGHT subcomponent of a [`TimeZone`].
#[derive(Debug, Clone)]
pub struct TzRule<S> {
    kind: TzRuleKind,
    props: PropertyTable<S>,
}

impl<S> TzRule<S> {
    pub(crate) const fn new(props: PropertyTable<S>, kind: TzRuleKind) -> Self {
        Self { kind, props }
    }

    pub fn kind(&self) -> TzRuleKind {
        self.kind
    }
}

impl<S> TzRule<S>
where
    S: HashCaseless + Equiv,
{
    mandatory_accessors! {
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [TzOffsetTo, offset_to, offset_to_mut, Prop<S, UtcOffset>],
        [TzOffsetFrom, offset_from, offset_from_mut, Prop<S, UtcOffset>],
    }

    seq_accessors! {
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, MultiProp<S, RDateSeq, DtParams<S>>],
        [RRule, rrule, rrule_mut, MultiProp<S, Box<RRule>>],
        [TzName, names, names_mut, MultiProp<S, Text<S>, LangParams<S>>],
    }
}

/// The kind of a [`TzRule`], for which the default is [`Standard`].
///
/// [`Standard`]: TzRuleKind::Standard
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TzRuleKind {
    #[default]
    Standard,
    Daylight,
}

// TODO: should OtherComponent be using PropertyTable? or should it use a more stringly-typed map
// internally instead?

/// An arbitrary component which may have any properties and subcomponents.
#[derive(Debug, Clone)]
pub struct OtherComponent<S> {
    name: UnknownName<S>,
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

impl<S> OtherComponent<S> {
    pub(crate) const fn new(
        name: UnknownName<S>,
        props: PropertyTable<S>,
        subcomponents: Vec<OtherComponent<S>>,
    ) -> Self {
        Self {
            name,
            props,
            subcomponents,
        }
    }

    pub const fn name(&self) -> &UnknownName<S> {
        &self.name
    }

    pub const fn name_mut(&mut self) -> &mut UnknownName<S> {
        &mut self.name
    }

    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnknownName<S> {
    Iana(S),
    X(S),
}

impl<S> UnknownName<S> {
    pub const fn inner(&self) -> &S {
        match self {
            UnknownName::Iana(x) => x,
            UnknownName::X(x) => x,
        }
    }
}
