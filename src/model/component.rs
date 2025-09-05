//! Model types for calendar components.

use std::hash::{BuildHasher, Hash, Hasher, RandomState};

use hashbrown::{HashTable, hash_table::Entry};

use crate::{
    model::primitive::{ProximityValue, UnknownAction},
    parser::{
        error::{CalendarParseError, ComponentKind},
        escaped::{Equiv, LineFoldCaseless},
        property::{PropName, Rfc5545PropName, Rfc9074PropName},
    },
};

use super::{
    css::Css3Color,
    parameter::KnownParam,
    primitive::{
        AttachValue, AudioAction, CalAddress, ClassValue, CompletionPercentage, DateTime,
        DateTimeOrDate, DateTimeOrDateSeq, DisplayAction, Duration, EmailAction, EventStatus, Geo,
        ImageData, Integer, JournalStatus, Method, Period, Priority, RDateSeq, RequestStatus,
        Status, Text, TimeTransparency, TodoStatus, TzId, Uid, Uri, Utc, UtcOffset, Value,
    },
    property::{
        AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams, ImageParams, LangParams,
        OrganizerParams, Prop, RecurrenceIdParams, RelTypeParams, TextParams, TriggerProp,
    },
    rrule::RRule,
};

macro_rules! mandatory_accessors {
    ($([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> &$ret {
                let raw_entry = self.props.get(PropKey::Static(StaticPropName::$key));

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$key(mult))) => mult.as_one().unwrap(),
                    Some(_) | None => unreachable!(),
                }
            }

            pub fn $name_mut(&mut self) -> &mut $ret {
                let raw_entry = self.props.get_mut(PropKey::Static(StaticPropName::$key));

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$key(mult))) => mult.as_one_mut().unwrap(),
                    Some(_) | None => unreachable!(),
                }
            }
        )*
    };
}

macro_rules! optional_accessors {
    ($([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&$ret> {
                let raw_entry = self.props.get(PropKey::Static(StaticPropName::$key));

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$key(mult))) => mult.as_one(),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }

            pub fn $name_mut(&mut self) -> Option<&mut $ret> {
                let raw_entry = self.props.get_mut(PropKey::Static(StaticPropName::$key));

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$key(mult))) => mult.as_one_mut(),
                    Some(_) => unreachable!(),
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
                let raw_entry = self.props.get(PropKey::Static(StaticPropName::$key));

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$key(mult))) => mult.as_seq(),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }

            pub fn $name_mut(&mut self) -> Option<&mut Vec<$ret>> {
                let raw_entry = self.props.get_mut(PropKey::Static(StaticPropName::$key));

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$key(mult))) => mult.as_seq_mut(),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }
        )*
    };
}

/// A sequence of [`Prop`].
type PropSeq<S, V, P = ()> = Vec<Prop<S, V, P>>;

/// An unknown property value and parameter sequence.
type UnknownProp<S> = Prop<S, Box<Value<S>>, Box<[KnownParam<S>]>>;

/// A sequence of unknown property values and parameters.
type UnknownPropSeq<S> = PropSeq<S, Box<Value<S>>, Box<[KnownParam<S>]>>;

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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
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
        [Name, names, names_mut, Prop<S, Text<S>, TextParams<S>>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Categories, categories, categories_mut, Prop<S, Box<[Text<S>]>, LangParams<S>>],
        [Image, images, images_mut, Prop<S, ImageData<S>, ImageParams<S>>],
    }
}

#[derive(Debug, Clone)]
pub enum Component<S> {
    Event(Event<S>),
    Todo(Todo<S>),
    Journal(Journal<S>),
    FreeBusy(FreeBusy<S>),
    TimeZone(TimeZone<S>),
    Iana(OtherComponent<S>),
    X(OtherComponent<S>),
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    pub fn status(&self) -> Option<&Prop<S, EventStatus>> {
        match self.props.get(PropKey::Static(StaticPropName::Status)) {
            Some(PropEntry::Static(StaticProp::Status(mult))) => {
                mult.as_one().and_then(StatusProp::as_event)
            }
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn status_mut(&mut self) -> Option<&mut Prop<S, EventStatus>> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Status)) {
            Some(PropEntry::Static(StaticProp::Status(mult))) => {
                mult.as_one_mut().and_then(StatusProp::as_event_mut)
            }
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn termination(&self) -> Option<EventTerminationRef<'_, S>> {
        todo!()
    }

    pub fn termination_mut(&mut self) -> Option<EventTerminationMut<'_, S>> {
        todo!()
    }

    // TODO: write termination and termination_mut accessors for (DTEND xor DURATION)

    optional_accessors! {
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
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
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, Prop<S, Box<[Text<S>]>, LangParams<S>>],
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, Prop<S, Text<S>, TextParams<S>>],
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
        [ExDate, exception_dates, exception_dates_mut, Prop<S, DateTimeOrDateSeq, DtParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, Prop<S, RequestStatus<S>, LangParams<S>>],
        [RelatedTo, relateds, relateds_mut, Prop<S, Text<S>, RelTypeParams<S>>],
        [Resources, resources, resources_mut, Prop<S, Box<[Text<S>]>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, Prop<S, RDateSeq, DtParams<S>>],
        [Conference, conferences, conferences_mut, Prop<S, Uri<S>, ConfParams<S>>],
        [Image, images, images_mut, Prop<S, ImageData<S>, ImageParams<S>>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    pub fn status(&self) -> Option<&Prop<S, TodoStatus>> {
        match self.props.get(PropKey::Static(StaticPropName::Status)) {
            Some(PropEntry::Static(StaticProp::Status(mult))) => {
                mult.as_one().and_then(StatusProp::as_todo)
            }
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn status_mut(&mut self) -> Option<&mut Prop<S, TodoStatus>> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Status)) {
            Some(PropEntry::Static(StaticProp::Status(mult))) => {
                mult.as_one_mut().and_then(StatusProp::as_todo_mut)
            }
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn termination(&self) -> Option<TodoTerminationRef<'_, S>> {
        todo!()
    }

    pub fn termination_mut(&mut self) -> Option<TodoTerminationMut<'_, S>> {
        todo!()
    }

    // TODO: write termination and termination_mut accessors for (DTDUE xor DURATION)

    optional_accessors! {
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
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, Prop<S, Box<[Text<S>]>, LangParams<S>>],
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, Prop<S, Text<S>, TextParams<S>>],
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
        [ExDate, exception_dates, exception_dates_mut, Prop<S, DateTimeOrDateSeq, DtParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, Prop<S, RequestStatus<S>, LangParams<S>>],
        [RelatedTo, relateds, relateds_mut, Prop<S, Text<S>, RelTypeParams<S>>],
        [Resources, resources, resources_mut, Prop<S, Box<[Text<S>]>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, Prop<S, RDateSeq, DtParams<S>>],
        [Conference, conferences, conferences_mut, Prop<S, Uri<S>, ConfParams<S>>],
        [Image, images, images_mut, Prop<S, ImageData<S>, ImageParams<S>>],
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
    pub const fn subcomponents(&self) -> &[ExtComponent<S>] {
        match self {
            Alarm::Audio(alarm) => alarm.subcomponents(),
            Alarm::Display(alarm) => alarm.subcomponents(),
            Alarm::Email(alarm) => alarm.subcomponents(),
            Alarm::Other(alarm) => alarm.subcomponents(),
        }
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<ExtComponent<S>> {
        match self {
            Alarm::Audio(alarm) => alarm.subcomponents_mut(),
            Alarm::Display(alarm) => alarm.subcomponents_mut(),
            Alarm::Email(alarm) => alarm.subcomponents_mut(),
            Alarm::Other(alarm) => alarm.subcomponents_mut(),
        }
    }
}

macro_rules! dur_rep_accessors {
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
    };
}

/// A VALARM with the AUDIO action.
#[derive(Debug, Clone)]
pub struct AudioAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<ExtComponent<S>>,
}

impl<S> AudioAlarm<S> {
    pub const fn subcomponents(&self) -> &[ExtComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<ExtComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> AudioAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, AudioAction> {
        match self.props.get(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => {
                mult.as_one().and_then(AlarmActionProp::as_audio).unwrap()
            }
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, AudioAction> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => mult
                .as_one_mut()
                .and_then(AlarmActionProp::as_audio_mut)
                .unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
    }

    optional_accessors! {
        [Attach, attachment, attachment_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Repeat, repeat, repeat_mut, Prop<S, Integer>],
        [Acknowledged, acknowleded, acknowleded_mut, Prop<S, DateTime<Utc>>],
        [Proximity, proximity, proximity_mut, Prop<S, ProximityValue<S>>],
    }

    dur_rep_accessors!();
}

/// A VALARM with the DISPLAY action.
#[derive(Debug, Clone)]
pub struct DisplayAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<ExtComponent<S>>,
}

impl<S> DisplayAlarm<S> {
    pub const fn subcomponents(&self) -> &[ExtComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<ExtComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> DisplayAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, DisplayAction> {
        match self.props.get(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => {
                mult.as_one().and_then(AlarmActionProp::as_display).unwrap()
            }
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, DisplayAction> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => mult
                .as_one_mut()
                .and_then(AlarmActionProp::as_display_mut)
                .unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
    }

    optional_accessors! {
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],
        [Repeat, repeat, repeat_mut, Prop<S, Integer>],
        [Acknowledged, acknowleded, acknowleded_mut, Prop<S, DateTime<Utc>>],
        [Proximity, proximity, proximity_mut, Prop<S, ProximityValue<S>>],
    }

    dur_rep_accessors!();
}

/// A VALARM with the EMAIL action.
#[derive(Debug, Clone)]
pub struct EmailAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<ExtComponent<S>>,
}

impl<S> EmailAlarm<S> {
    pub const fn subcomponents(&self) -> &[ExtComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<ExtComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> EmailAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, EmailAction> {
        match self.props.get(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => {
                mult.as_one().and_then(AlarmActionProp::as_email).unwrap()
            }
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, EmailAction> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => mult
                .as_one_mut()
                .and_then(AlarmActionProp::as_email_mut)
                .unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
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
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
    }

    dur_rep_accessors!();
}

/// A VALARM with an action other than AUDIO, DISPLAY, or EMAIL.
#[derive(Debug, Clone)]
pub struct OtherAlarm<S> {
    props: PropertyTable<S>,
    subcomponents: Vec<ExtComponent<S>>,
}

impl<S> OtherAlarm<S> {
    pub const fn subcomponents(&self) -> &[ExtComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<ExtComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> OtherAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, UnknownAction<S>> {
        match self.props.get(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => {
                mult.as_one().and_then(AlarmActionProp::as_other).unwrap()
            }
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, UnknownAction<S>> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Action)) {
            Some(PropEntry::Static(StaticProp::Action(mult))) => mult
                .as_one_mut()
                .and_then(AlarmActionProp::as_other_mut)
                .unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
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
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
    }

    dur_rep_accessors!();
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    pub fn status(&self) -> Option<&Prop<S, JournalStatus>> {
        match self.props.get(PropKey::Static(StaticPropName::Status)) {
            Some(PropEntry::Static(StaticProp::Status(mult))) => {
                mult.as_one().and_then(StatusProp::as_journal)
            }
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn status_mut(&mut self) -> Option<&mut Prop<S, JournalStatus>> {
        match self.props.get_mut(PropKey::Static(StaticPropName::Status)) {
            Some(PropEntry::Static(StaticProp::Status(mult))) => {
                mult.as_one_mut().and_then(StatusProp::as_journal_mut)
            }
            Some(_) => unreachable!(),
            None => None,
        }
    }

    optional_accessors! {
        [Class, class, class_mut, Prop<S, ClassValue<S>>],
        [Created, created, created_mut, Prop<S, DateTime<Utc>>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [RecurId, recurrence_id, recurrence_id_mut, Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>],
        [Sequence, sequence_number, sequence_number_mut, Prop<S, Integer>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
    }

    seq_accessors! {
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, Prop<S, Box<[Text<S>]>, LangParams<S>>],
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, Prop<S, Text<S>, TextParams<S>>],
        [Description, descriptions, descriptions_mut, Prop<S, Text<S>, TextParams<S>>],
        [ExDate, exception_dates, exception_dates_mut, Prop<S, DateTimeOrDateSeq, DtParams<S>>],
        [RelatedTo, relateds, relateds_mut, Prop<S, Text<S>, RelTypeParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, Prop<S, RDateSeq, DtParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, Prop<S, RequestStatus<S>, LangParams<S>>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
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
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [FreeBusy, free_busy_periods, free_busy_periods_mut, Prop<S, Box<[Period]>, FBTypeParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, Prop<S, RequestStatus<S>, LangParams<S>>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [TzOffsetTo, offset_to, offset_to_mut, Prop<S, UtcOffset>],
        [TzOffsetFrom, offset_from, offset_from_mut, Prop<S, UtcOffset>],
    }

    optional_accessors! {
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
    }

    seq_accessors! {
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, Prop<S, RDateSeq, DtParams<S>>],
        [TzName, names, names_mut, Prop<S, Text<S>, LangParams<S>>],
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

/// An arbitrary component which may have any properties and subcomponents.
#[derive(Debug, Clone)]
pub enum ExtComponent<S> {
    Iana(OtherComponent<S>),
    X(OtherComponent<S>),
}

/// An arbitrary component which may have any properties and subcomponents.
#[derive(Debug, Clone)]
pub struct OtherComponent<S> {
    name: S,
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

impl<S> OtherComponent<S> {
    pub const fn name(&self) -> &S {
        &self.name
    }

    pub const fn name_mut(&mut self) -> &mut S {
        &mut self.name
    }

    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

// impl<S> OtherComponent<S>
// where
//     S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
// {
//     seq_accessors! {AnyProp, AnyPropName;
//         [CalScale, scale, scale_mut, Prop<S, ()>],
//         [Method, method, method_mut, Prop<S, Method<S>>],
//         [ProdId, prod_id, prod_id_mut, Prop<S, Text<S>>],
//         [Version, version, version_mut, Prop<S, ()>],
//         // TODO: finish other accessors
//     }
// }

#[derive(Debug, Clone)]
pub struct PropertyTable<S>(HashTable<PropEntry<S>>, RandomState);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropEntry<S> {
    Static(StaticProp<S>),
    Iana { name: S, props: UnknownPropSeq<S> },
    X { name: S, props: UnknownPropSeq<S> },
}

impl<S> PropEntry<S> {
    pub const fn as_key(&self) -> PropKey<&S> {
        match self {
            PropEntry::Static(prop) => PropKey::Static(prop.name()),
            PropEntry::Iana { name, .. } => PropKey::Iana(name),
            PropEntry::X { name, .. } => PropKey::X(name),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropKey<S> {
    Static(StaticPropName),
    Iana(S),
    X(S),
}

impl<S> Default for PropertyTable<S> {
    fn default() -> Self {
        Self(Default::default(), Default::default())
    }
}

macro_rules! property_table_accessors {
    ($([$prop:ident, $name_ref:ident, $name_mut:ident, $ret_ty:ty]),* $(,)?) => {
        $(
            pub fn $name_ref (&self) -> Option<&Mult<$ret_ty>>
            where
                S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let key = PropKey::Static(StaticPropName::$prop);
                let raw_entry = self.get(key);

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$prop(mult))) => Some(mult),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }

            pub fn $name_mut (&mut self) -> Option<&mut Mult<$ret_ty>>
            where
                S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let key = PropKey::Static(StaticPropName::$prop);
                let raw_entry = self.get_mut(key);

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$prop(mult))) => Some(mult),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }
        )*
    };
}

impl<S> PropertyTable<S> {
    property_table_accessors! {
        // CALENDAR PROPERTIES
        [CalScale, cal_scale, cal_scale_mut, Prop<S, ()>],
        [Method, method, method_mut, Prop<S, Method<S>>],
        [ProdId, prod_id, prod_id_mut, Prop<S, Text<S>>],
        [Version, version, version_mut, Prop<S, ()>],

        // DATE AND TIME COMPONENT PROPERTIES
        [DtCompleted, dt_completed, dt_completed_mut, Prop<S, DateTime<Utc>>],
        [DtEnd, dt_end, dt_end_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [DtDue, dt_due, dt_due_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [DtStart, dt_start, dt_start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [Duration, duration, duration_mut, Prop<S, Duration>],

        // TIME ZONE COMPONENT PROPERTIES
        [TzId, tz_id, tz_id_mut, Prop<S, TzId<S>>],
        [TzName, tz_name, tz_name_mut, Prop<S, Text<S>, LangParams<S>>],
        [TzOffsetFrom, tz_offset_from, tz_offset_from_mut, Prop<S, UtcOffset>],
        [TzOffsetTo, tz_offset_to, tz_offset_to_mut, Prop<S, UtcOffset>],
        [TzUrl, tz_url, tz_url_mut, Prop<S, Uri<S>>],
        // RELATIONSHIP COMPONENT PROPERTIES
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],

        // CHANGE MANAGEMENT COMPONENT PROPERTIES
        [DtStamp, dt_stamp, dt_stamp_mut, Prop<S, DateTime<Utc>>],
    }

    /// Returns an empty table; does not allocate.
    pub fn new() -> Self {
        Self(Default::default(), Default::default())
    }

    /// Returns an empty table with the given capacity; does not allocate if the
    /// capacity is zero.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashTable::with_capacity(capacity), Default::default())
    }

    pub fn insert(&mut self, value: PropEntry<S>) -> Option<PropEntry<S>>
    where
        S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let hash = Self::hash_entry(&self.1);
        let key = value.as_key();
        let eq = Self::eq(key);

        match self.0.entry(hash(&value), eq, hash) {
            Entry::Occupied(mut entry) => Some(std::mem::replace(entry.get_mut(), value)),
            Entry::Vacant(entry) => {
                entry.insert(value);
                None
            }
        }
    }

    pub fn insert_iana(&mut self, name: S, prop: UnknownProp<S>)
    where
        S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let key = PropKey::Iana(&name);

        match self.get_mut(key) {
            Some(PropEntry::Iana { props, .. }) => {
                props.push(prop);
            }
            Some(_) => unreachable!(),
            None => {
                let entry = PropEntry::Iana {
                    name,
                    props: vec![prop],
                };

                self.insert(entry);
            }
        }
    }

    pub fn insert_x(&mut self, name: S, prop: UnknownProp<S>)
    where
        S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let key = PropKey::X(&name);

        match self.get_mut(key) {
            Some(PropEntry::X { props, .. }) => {
                props.push(prop);
            }
            Some(_) => unreachable!(),
            None => {
                let entry = PropEntry::X {
                    name,
                    props: vec![prop],
                };

                self.insert(entry);
            }
        }
    }

    pub fn get(&self, key: PropKey<&S>) -> Option<&PropEntry<S>>
    where
        S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(key);

        self.0.find(hash(key), eq)
    }

    pub fn get_mut(&mut self, key: PropKey<&S>) -> Option<&mut PropEntry<S>>
    where
        S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(key);

        self.0.find_mut(hash(key), eq)
    }

    pub fn remove(&mut self, key: PropKey<&S>) -> Option<PropEntry<S>>
    where
        S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let hash_key = Self::hash_key(&self.1);
        let hash = Self::hash_entry(&self.1);
        let eq = Self::eq(key);
        let table_entry = self.0.entry(hash_key(key), eq, hash);

        match table_entry {
            Entry::Vacant(_) => None,
            Entry::Occupied(entry) => {
                let (entry, _) = entry.remove();
                Some(entry)
            }
        }
    }

    fn eq(lhs: PropKey<&S>) -> impl Fn(&PropEntry<S>) -> bool
    where
        S: Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        move |rhs| match rhs.as_key() {
            PropKey::Static(r) => matches!(lhs, PropKey::Static(l) if l == r),
            PropKey::Iana(r) => matches!(lhs, PropKey::Iana(l) if (&l).equiv(r, LineFoldCaseless)),
            PropKey::X(r) => matches!(lhs, PropKey::X(l) if (&l).equiv(r, LineFoldCaseless)),
        }
    }

    fn hash_entry(hasher: &impl BuildHasher) -> impl Fn(&PropEntry<S>) -> u64
    where
        S: Hash,
    {
        let h = Self::hash_key(hasher);
        move |entry| h(entry.as_key())
    }

    fn hash_key(hasher: &impl BuildHasher) -> impl Fn(PropKey<&S>) -> u64
    where
        S: Hash,
    {
        |key| {
            let mut hasher = hasher.build_hasher();

            match key {
                PropKey::Static(name) => name.hash(&mut hasher),
                PropKey::Iana(name) => name.hash(&mut hasher),
                PropKey::X(name) => name.hash(&mut hasher),
            };

            hasher.finish()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mult<T> {
    One(T),
    Seq(Vec<T>),
}

impl<T> Mult<T> {
    pub const fn as_one(&self) -> Option<&T> {
        if let Self::One(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_one_mut(&mut self) -> Option<&mut T> {
        if let Self::One(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_seq(&self) -> Option<&[T]> {
        if let Self::Seq(v) = self {
            Some(v.as_slice())
        } else {
            None
        }
    }

    pub const fn as_seq_mut(&mut self) -> Option<&mut Vec<T>> {
        if let Self::Seq(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AlarmActionProp<S> {
    Audio(Prop<S, AudioAction>),
    Display(Prop<S, DisplayAction>),
    Email(Prop<S, EmailAction>),
    Other(Prop<S, UnknownAction<S>>),
}

impl<S> AlarmActionProp<S> {
    pub const fn as_audio(&self) -> Option<&Prop<S, AudioAction>> {
        if let Self::Audio(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_audio_mut(&mut self) -> Option<&mut Prop<S, AudioAction>> {
        if let Self::Audio(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_display(&self) -> Option<&Prop<S, DisplayAction>> {
        if let Self::Display(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_display_mut(&mut self) -> Option<&mut Prop<S, DisplayAction>> {
        if let Self::Display(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_email(&self) -> Option<&Prop<S, EmailAction>> {
        if let Self::Email(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_email_mut(&mut self) -> Option<&mut Prop<S, EmailAction>> {
        if let Self::Email(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_other(&self) -> Option<&Prop<S, UnknownAction<S>>> {
        if let Self::Other(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_other_mut(&mut self) -> Option<&mut Prop<S, UnknownAction<S>>> {
        if let Self::Other(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatusProp<S> {
    Event(Prop<S, EventStatus>),
    Todo(Prop<S, TodoStatus>),
    Journal(Prop<S, JournalStatus>),
    Other(Prop<S, Status>),
}

impl<S> StatusProp<S> {
    pub const fn as_event(&self) -> Option<&Prop<S, EventStatus>> {
        if let Self::Event(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_event_mut(&mut self) -> Option<&mut Prop<S, EventStatus>> {
        if let Self::Event(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_todo(&self) -> Option<&Prop<S, TodoStatus>> {
        if let Self::Todo(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_todo_mut(&mut self) -> Option<&mut Prop<S, TodoStatus>> {
        if let Self::Todo(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_journal(&self) -> Option<&Prop<S, JournalStatus>> {
        if let Self::Journal(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_journal_mut(&mut self) -> Option<&mut Prop<S, JournalStatus>> {
        if let Self::Journal(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_other(&self) -> Option<&Prop<S, Status>> {
        if let Self::Other(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_other_mut(&mut self) -> Option<&mut Prop<S, Status>> {
        if let Self::Other(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// Counts the number of token trees in the input. From Luka Wirth's macro book.
macro_rules! count_tts {
    () => { 0 };
    ($odd:tt $($a:tt $b:tt)*) => { (count_tts!($($a)*) << 1) | 1 };
    ($($a:tt $even:tt)*) => { count_tts!($($a)*) << 1 };
}

macro_rules! enum_with_names {
    (
        $(#[ $m1:meta ])*
        $enum_name:ident $(<$($t:tt),+>)?,
        $(#[ $m2:meta ])*
        $names_name:ident
        { $($variant:ident ($($field:ty),*)),* }
    ) => {
        $(#[ $m1 ])*
        pub enum $enum_name $(<$($t),+>)? {
            $($variant ($($field),*)),*
        }

        $(#[ $m2 ])*
        #[repr(u16)]
        pub enum $names_name {
            $($variant),*
        }

        impl $(<$($t),+>)? $enum_name $(<$($t),+>)? {
            /// Returns the variant name of `self`.
            pub const fn name(&self) -> $names_name {
                match self {
                    $(
                        $enum_name::$variant(..) => $names_name::$variant,
                    )*
                }
            }
        }

        impl $(<$($t),+>)? From<&$enum_name $(<$($t),+>)?> for $names_name {
            fn from(value: &$enum_name $(<$($t),+>)?) -> $names_name {
               value.name()
            }
        }

        impl $names_name {
            /// The number of variants in this type.
            pub const VARIANTS: u16 = count_tts!($($variant)*);

            /// Returns the value of `Self` corresponding to `repr`, if any.
            pub const fn from_repr(repr: u16) -> Option<Self> {
                if repr < Self::VARIANTS {
                    Some(unsafe { std::mem::transmute::<u16, Self>(repr) })
                } else {
                    None
                }
            }

            /// Iterates over the variants of `Self` in declaration order.
            pub fn iter() -> impl ExactSizeIterator<Item = Self> {
                (0..Self::VARIANTS).map(|repr| {
                    unsafe { Self::from_repr(repr).unwrap_unchecked() }
                })
            }
        }
    };
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
StaticProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
StaticPropName {
    // CALENDAR PROPERTIES
    CalScale(Mult<Prop<S, ()>>),
    Method(Mult<Prop<S, Method<S>>>),
    ProdId(Mult<Prop<S, Text<S>>>),
    Version(Mult<Prop<S, ()>>),
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(Mult<Prop<S, AttachValue<S>, AttachParams<S>>>),
    Categories(Mult<Prop<S, Box<[Text<S>]>, LangParams<S>>>),
    Class(Mult<Prop<S, ClassValue<S>>>),
    Comment(Mult<Prop<S, Text<S>, TextParams<S>>>),
    Description(Mult<Prop<S, Text<S>, TextParams<S>>>),
    Geo(Mult<Prop<S, Geo>>),
    Location(Mult<Prop<S, Text<S>, TextParams<S>>>),
    PercentComplete(Mult<Prop<S, CompletionPercentage>>),
    Priority(Mult<Prop<S, Priority>>),
    Resources(Mult<Prop<S, Box<[Text<S>]>, TextParams<S>>>),
    Status(Mult<StatusProp<S>>),
    Summary(Mult<Prop<S, Text<S>, TextParams<S>>>),
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted(Mult<Prop<S, DateTime<Utc>>>),
    DtEnd(Mult<Prop<S, DateTimeOrDate, DtParams<S>>>),
    DtDue(Mult<Prop<S, DateTimeOrDate, DtParams<S>>>),
    DtStart(Mult<Prop<S, DateTimeOrDate, DtParams<S>>>),
    Duration(Mult<Prop<S, Duration>>),
    FreeBusy(Mult<Prop<S, Box<[Period]>, FBTypeParams<S>>>),
    Transp(Mult<Prop<S, TimeTransparency>>),
    // TIME ZONE COMPONENT PROPERTIES
    TzId(Mult<Prop<S, TzId<S>>>),
    TzName(Mult<Prop<S, Text<S>, LangParams<S>>>),
    TzOffsetFrom(Mult<Prop<S, UtcOffset>>),
    TzOffsetTo(Mult<Prop<S, UtcOffset>>),
    TzUrl(Mult<Prop<S, Uri<S>>>),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(Mult<Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>>),
    Contact(Mult<Prop<S, Text<S>, TextParams<S>>>),
    Organizer(Mult<Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>>),
    RecurId(Mult<Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>>),
    RelatedTo(Mult<Prop<S, Text<S>, RelTypeParams<S>>>),
    Url(Mult<Prop<S, Uri<S>>>),
    Uid(Mult<Prop<S, Uid<S>>>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(Mult<Prop<S, DateTimeOrDateSeq, DtParams<S>>>),
    RDate(Mult<Prop<S, RDateSeq, DtParams<S>>>),
    RRule(Mult<Prop<S, Box<RRule>>>),
    // ALARM COMPONENT PROPERTIES
    Action(Mult<AlarmActionProp<S>>),
    Repeat(Mult<Prop<S, Integer>>),
    Trigger(Mult<TriggerProp<S>>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(Mult<Prop<S, DateTime<Utc>>>),
    DtStamp(Mult<Prop<S, DateTime<Utc>>>),
    LastModified(Mult<Prop<S, DateTime<Utc>>>),
    Sequence(Mult<Prop<S, Integer>>),
    // MISCELLANEOUS COMPONENT PROPERTIES
    RequestStatus(Mult<Prop<S, RequestStatus<S>, LangParams<S>>>),
    // RFC 7986 PROPERTIES
    Name(Mult<Prop<S, Text<S>, TextParams<S>>>),
    RefreshInterval(Mult<Prop<S, Duration>>),
    Source(Mult<Prop<S, Uri<S>>>),
    Color(Mult<Prop<S, Css3Color>>),
    Image(Mult<Prop<S, ImageData<S>, ImageParams<S>>>),
    Conference(Mult<Prop<S, Uri<S>, ConfParams<S>>>),
    // RFC 9074 PROPERTIES
    Acknowledged(Mult<Prop<S, DateTime<Utc>>>), // RFC 9074 §6
    Proximity(Mult<Prop<S, ProximityValue<S>>>) // RFC 9074 §8.1
}}

macro_rules! get_fields {
    ($this:ident; $([$name:ident = $field:ident]),* $(,)?) => {
        $(
            let $name =
                $this.get(PropKey::Static(StaticPropName::$field))
                    .map(|entry| match entry {
                        PropEntry::Static(StaticProp::$field(x)) => x,
                        _ => unreachable!(),
                    });
        )*
    };
}

// TODO: finish refactoring this function to use PropertyTable. note that since the input is
// already a property table, there's no point in removing and replacing fields: we just need
// to check for the existence and multiplicity of certain fields before selecting one of the
// alarm wrapper types based on the action and returning it

impl<S> PropertyTable<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn try_into_alarm(
        self,
        subcomponents: Vec<ExtComponent<S>>,
    ) -> Result<Alarm<S>, CalendarParseError<S>> {
        macro_rules! one {
            ($scrut:expr, $prop:expr) => {
                match $scrut {
                    Some(Mult::One(x)) => Ok(x),
                    Some(Mult::Seq(_)) => Err(CalendarParseError::MoreThanOneProp {
                        prop: $prop,
                        component: ComponentKind::Alarm,
                    }),
                    None => Err(CalendarParseError::MissingProp {
                        prop: $prop,
                        component: ComponentKind::Alarm,
                    }),
                }
            };
        }

        macro_rules! zero_or_one {
            ($scrut:expr, $prop:expr) => {
                match $scrut {
                    Some(Mult::One(x)) => Ok(Some(x)),
                    Some(Mult::Seq(_)) => Err(CalendarParseError::MoreThanOneProp {
                        prop: $prop,
                        component: ComponentKind::Alarm,
                    }),
                    None => Ok(None),
                }
            };
        }

        get_fields! {self;
            [  action =       Action],
            [ trigger =      Trigger],
            [    desc =  Description],
            [     dur =     Duration],
            [  repeat =       Repeat],
            [     uid =          Uid],
            [     ack = Acknowledged],
            [    prox =    Proximity],
            [ summary =      Summary],
            [  attend =     Attendee],
            [  attach =       Attach],
            [ related =    RelatedTo],
        }

        let action = one!(action, PropName::Rfc5545(Rfc5545PropName::Action))?;
        let trigger = one!(trigger, PropName::Rfc5545(Rfc5545PropName::Trigger))?;
        let dur = zero_or_one!(dur, PropName::Rfc5545(Rfc5545PropName::Duration))?;
        let repeat = zero_or_one!(repeat, PropName::Rfc5545(Rfc5545PropName::RepeatCount))?;
        let uid = zero_or_one!(uid, PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier))?;
        let ack = zero_or_one!(ack, PropName::Rfc9074(Rfc9074PropName::Acknowledged))?;
        let prox = zero_or_one!(prox, PropName::Rfc9074(Rfc9074PropName::Proximity))?;

        // TODO: how are we handling multiple props with Mult (here for related)? do
        // we reallocate, or should Mult handle that internally?

        match action {
            AlarmActionProp::Audio(_) => todo!(),
            AlarmActionProp::Display(_) => todo!(),
            AlarmActionProp::Email(_) => todo!(),
            AlarmActionProp::Other(_) => todo!(),
        }
    }
}

// impl<S> PropertyTable<S>
// where
//     S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
// {
//     pub fn try_into_alarm(
//         mut self,
//         subcomponents: Vec<ExtComponent<S>>,
//     ) -> Result<Alarm<S>, CalendarParseError<S>> {
//         remove_fields! {self;
//             [let action      =       Action],
//             [let trigger     =      Trigger],
//             [let description =  Description],
//             [let duration    =     Duration],
//             [let repeat      =       Repeat],
//             [let uid         =          Uid],
//             [let ack         = Acknowledged],
//             [let prox        =    Proximity],
//             [let summary     =      Summary],
//             [let attendees   =     Attendee],
//             [let attachments =       Attach],
//             [let relateds    =    RelatedTo],
//         }
//
//         // check mandatory fields
//         let Some(Mult::One(action)) = action else {
//             return Err(CalendarParseError::MissingProp {
//                 prop: PropName::Rfc5545(Rfc5545PropName::Action),
//                 component: ComponentKind::Alarm,
//             });
//         };
//
//         let Some(Mult::One(trigger)) = trigger else {
//             return Err(CalendarParseError::MissingProp {
//                 prop: PropName::Rfc5545(Rfc5545PropName::Trigger),
//                 component: ComponentKind::Alarm,
//             });
//         };
//
//         // check duration and repeat occur together or not at all
//         let (duration, repeat) = match (duration, repeat) {
//             (Some(d), Some(r)) => (Some(d), Some(r)),
//             (None, None) => (None, None),
//             (Some(_), None) => {
//                 return Err(CalendarParseError::DurationWithoutRepeat);
//             }
//             (None, Some(_)) => {
//                 return Err(CalendarParseError::RepeatWithoutDuration);
//             }
//         };
//
//         // init properties and insert trigger
//         let mut props = PropertyTable::new();
//         props.insert(PropEntry::Static(StaticProp::Trigger(Mult::One(trigger))));
//
//         // insert optional fields
//
//         if let Some(duration) = duration {
//             props.insert(PropEntry::Static(StaticProp::Duration(duration)));
//         }
//
//         if let Some(repeat) = repeat {
//             props.insert(PropEntry::Static(StaticProp::Repeat(repeat)));
//         }
//
//         if let Some(uid) = uid {
//             props.insert(PropEntry::Static(StaticProp::Uid(uid)));
//         }
//
//         if let Some(ack) = ack {
//             props.insert(PropEntry::Static(StaticProp::Acknowledged(ack)));
//         }
//
//         if let Some(prox) = prox {
//             props.insert(PropEntry::Static(StaticProp::Proximity(prox)));
//         }
//
//         if let Some(relateds) = relateds {
//             props.insert(PropEntry::Static(StaticProp::RelatedTo(relateds)));
//         }
//
//         match action {
//             AlarmActionProp::Audio(prop) => {
//                 // insert action
//                 props.insert(Entry::Known(AudioAlarmProp::Action(Prop {
//                     value: AudioAction,
//                     params: (),
//                     unknown_params: action.unknown_params,
//                 })));
//
//                 // insert attachment if single
//                 if let Some(mut attachments) = attachments {
//                     if attachments.len() == 1 {
//                         let p = attachments.remove(0);
//                         props.insert(Entry::Known(AudioAlarmProp::Attach(p)));
//                     } else if attachments.len() > 1 {
//                         return Err(CalendarParseError::TooManyAttachmentsOnAudioAlarm);
//                     }
//                 }
//
//                 // check for invalid fields
//
//                 if description.is_some() {
//                     return Err(CalendarParseError::UnexpectedProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Description),
//                         component: ComponentKind::AudioAlarm,
//                     });
//                 }
//
//                 if summary.is_some() {
//                     return Err(CalendarParseError::UnexpectedProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Summary),
//                         component: ComponentKind::AudioAlarm,
//                     });
//                 }
//
//                 if attendees.is_some() {
//                     return Err(CalendarParseError::UnexpectedProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
//                         component: ComponentKind::AudioAlarm,
//                     });
//                 }
//
//                 Ok(Alarm::Audio(AudioAlarm {
//                     props,
//                     subcomponents,
//                 }))
//             }
//             AlarmActionProp::Display(prop) => {
//                 // check for mandatory description
//                 let Some(description) = description else {
//                     return Err(CalendarParseError::MissingProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Description),
//                         component: ComponentKind::DisplayAlarm,
//                     });
//                 };
//
//                 // insert action
//                 props.insert(Entry::Known(DisplayAlarmProp::Action(Prop {
//                     value: DisplayAction,
//                     params: (),
//                     unknown_params: action.unknown_params,
//                 })));
//
//                 // insert description
//                 props.insert(Entry::Known(DisplayAlarmProp::Description(description)));
//
//                 // check for invalid fields
//
//                 if summary.is_some() {
//                     return Err(CalendarParseError::UnexpectedProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Summary),
//                         component: ComponentKind::DisplayAlarm,
//                     });
//                 }
//
//                 if attendees.is_some() {
//                     return Err(CalendarParseError::UnexpectedProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
//                         component: ComponentKind::DisplayAlarm,
//                     });
//                 }
//
//                 if attachments.is_some() {
//                     return Err(CalendarParseError::UnexpectedProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Attachment),
//                         component: ComponentKind::DisplayAlarm,
//                     });
//                 }
//
//                 Ok(Alarm::Display(DisplayAlarm {
//                     props,
//                     subcomponents,
//                 }))
//             }
//             AlarmActionProp::Email(prop) => {
//                 // check for mandatory description
//                 let Some(description) = description else {
//                     return Err(CalendarParseError::MissingProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Description),
//                         component: ComponentKind::EmailAlarm,
//                     });
//                 };
//
//                 // check for mandatory summary
//                 let Some(summary) = summary else {
//                     return Err(CalendarParseError::MissingProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Summary),
//                         component: ComponentKind::EmailAlarm,
//                     });
//                 };
//
//                 // check for mandatory attendees
//                 let Some(attendees) = attendees else {
//                     return Err(CalendarParseError::MissingProp {
//                         prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
//                         component: ComponentKind::EmailAlarm,
//                     });
//                 };
//
//                 // insert mandatory fields
//                 props.insert(Entry::Known(EmailAlarmProp::Action(Prop {
//                     value: EmailAction,
//                     params: (),
//                     unknown_params: action.unknown_params,
//                 })));
//                 props.insert(Entry::Known(EmailAlarmProp::Description(description)));
//                 props.insert(Entry::Known(EmailAlarmProp::Summary(summary)));
//                 props.insert(Entry::Known(EmailAlarmProp::Attendee(attendees)));
//
//                 // insert optional fields
//
//                 if let Some(p) = attachments {
//                     props.insert(Entry::Known(EmailAlarmProp::Attach(p)));
//                 }
//
//                 if let Some(relateds) = relateds {
//                     props.insert(Entry::Known(EmailAlarmProp::RelatedTo(relateds)));
//                 }
//
//                 Ok(Alarm::Email(EmailAlarm {
//                     props,
//                     subcomponents,
//                 }))
//             }
//             AlarmActionProp::Iana(name) => {
//                 let mut props = OtherAlarmTable::new();
//
//                 // insert action
//                 props.insert(Entry::Known(OtherAlarmProp::Action(Prop {
//                     value: UnknownAction::Iana(name),
//                     params: (),
//                     unknown_params: action.unknown_params,
//                 })));
//
//                 // insert trigger
//                 props.insert(Entry::Known(OtherAlarmProp::Trigger(trigger)));
//
//                 // insert optional fields
//                 if let Some(p) = description {
//                     props.insert(Entry::Known(OtherAlarmProp::Description(p)));
//                 }
//
//                 if let Some((d, r)) = dur_rep {
//                     props.insert(Entry::Known(OtherAlarmProp::DurRep(d, r)));
//                 }
//
//                 if let Some(uid) = uid {
//                     props.insert(Entry::Known(OtherAlarmProp::Uid(uid)));
//                 }
//
//                 if let Some(ack) = ack {
//                     props.insert(Entry::Known(OtherAlarmProp::Acknowledged(ack)));
//                 }
//
//                 if let Some(prox) = prox {
//                     props.insert(Entry::Known(OtherAlarmProp::Proximity(prox)));
//                 }
//
//                 if let Some(p) = summary {
//                     props.insert(Entry::Known(OtherAlarmProp::Summary(p)));
//                 }
//
//                 if let Some(p) = attendees {
//                     props.insert(Entry::Known(OtherAlarmProp::Attendee(p)));
//                 }
//
//                 if let Some(p) = attachments {
//                     props.insert(Entry::Known(OtherAlarmProp::Attach(p)));
//                 }
//
//                 Ok(Alarm::Other(OtherAlarm {
//                     props,
//                     subcomponents,
//                 }))
//             }
//             AlarmAction::X(name) => {
//                 let mut props = OtherAlarmTable::new();
//
//                 // insert action
//                 props.insert(Entry::Known(OtherAlarmProp::Action(Prop {
//                     value: UnknownAction::X(name),
//                     params: (),
//                     unknown_params: action.unknown_params,
//                 })));
//
//                 // insert trigger
//                 props.insert(Entry::Known(OtherAlarmProp::Trigger(trigger)));
//
//                 // insert optional fields
//                 if let Some(p) = description {
//                     props.insert(Entry::Known(OtherAlarmProp::Description(p)));
//                 }
//
//                 if let Some((d, r)) = dur_rep {
//                     props.insert(Entry::Known(OtherAlarmProp::DurRep(d, r)));
//                 }
//
//                 if let Some(uid) = uid {
//                     props.insert(Entry::Known(OtherAlarmProp::Uid(uid)));
//                 }
//
//                 if let Some(ack) = ack {
//                     props.insert(Entry::Known(OtherAlarmProp::Acknowledged(ack)));
//                 }
//
//                 if let Some(prox) = prox {
//                     props.insert(Entry::Known(OtherAlarmProp::Proximity(prox)));
//                 }
//
//                 if let Some(p) = summary {
//                     props.insert(Entry::Known(OtherAlarmProp::Summary(p)));
//                 }
//
//                 if let Some(p) = attendees {
//                     props.insert(Entry::Known(OtherAlarmProp::Attendee(p)));
//                 }
//
//                 if let Some(p) = attachments {
//                     props.insert(Entry::Known(OtherAlarmProp::Attach(p)));
//                 }
//
//                 Ok(Alarm::Other(OtherAlarm {
//                     props,
//                     subcomponents,
//                 }))
//             }
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use crate::{date, time};

    use super::*;

    #[test]
    fn basic_property_table_usage() {
        let mut props = PropertyTable::new();

        let uid = StaticProp::Uid(Mult::One(Prop::from_value(Uid("some-identifier"))));
        let dtstamp = StaticProp::DtStamp(Mult::One(Prop::from_value(DateTime {
            date: date!(1997;12;24),
            time: time!(15;20;12, Utc),
        })));

        let prev = props.insert(PropEntry::Static(uid.clone()));
        assert!(prev.is_none());
        let prev = props.insert(PropEntry::Static(dtstamp.clone()));
        assert!(prev.is_none());

        assert_eq!(props.0.len(), 2);

        let uid_ref = props.get(PropKey::Static(StaticPropName::Uid));
        let dtstamp_ref = props.get(PropKey::Static(StaticPropName::DtStamp));
        assert_eq!(Some(&PropEntry::Static(uid)), uid_ref);
        assert_eq!(Some(&PropEntry::Static(dtstamp)), dtstamp_ref);
    }
}
