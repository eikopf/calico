//! Model types for calendar components.

use std::{
    fmt::Debug,
    hash::{BuildHasher, Hash, Hasher, RandomState},
};

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
        AlarmAction, AttachValue, AudioAction, Binary, CalAddress, ClassValue,
        CompletionPercentage, DateTime, DateTimeOrDate, DisplayAction, Duration, EmailAction,
        EventStatus, ExDateSeq, Geo, ImageData, Integer, JournalStatus, Method, ParticipantType,
        Period, Priority, RDateSeq, RequestStatus, ResourceType, Status, StyledDescriptionValue,
        Text, TimeTransparency, TodoStatus, TzId, Uid, Uri, Utc, UtcOffset, Value,
    },
    property::{
        ActionPropMultMut, ActionPropMultRef, AnyStructuredDataProp, AnyTriggerProp, AttachParams,
        AttendeeParams, ConfParams, DtParams, FBTypeParams, ImageParams, LangParams, MultiProp,
        OrganizerParams, Prop, RecurrenceIdParams, RelTypeParams, StructuredDataParams,
        StyledDescriptionParams, TextParams, TriggerParams, TriggerPropMultMut, TriggerPropMultRef,
        TriggerPropMut, TriggerPropRef, UriStructuredDataParams,
    },
    rrule::RRule,
};

macro_rules! mandatory_accessors {
    ($([$name:ident => $name_inner:ident, $name_mut:ident => $name_mut_inner:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> &$ret {
                self.props.$name_inner().unwrap().as_one().unwrap()
            }

            pub fn $name_mut(&mut self) -> &mut $ret {
                self.props.$name_mut_inner().unwrap().as_one_mut().unwrap()
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
type UnknownProp<S> = MultiProp<S, Box<Value<S>>, Vec<KnownParam<S>>>;

/// A sequence of unknown property values and parameters.
type UnknownPropSeq<S> = Vec<UnknownProp<S>>;

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
        [prod_id => prod_id, prod_id_mut => prod_id_mut, Prop<S, Text<S>>],
        [version => version, version_mut => version_mut, Prop<S, ()>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [timestamp => timestamp, timestamp_mut => timestamp_mut, Prop<S, DateTime<Utc>>],
        [uid => uid, uid_mut => uid_mut, Prop<S, Uid<S>>],
    }

    pub fn status(&self) -> Option<&Prop<S, EventStatus>> {
        match self.props.get(PropKey::Static(StaticPropName::EventStatus)) {
            Some(PropEntry::Static(StaticProp::EventStatus(mult))) => mult.as_one(),
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn status_mut(&mut self) -> Option<&mut Prop<S, EventStatus>> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::EventStatus))
        {
            Some(PropEntry::Static(StaticProp::EventStatus(mult))) => mult.as_one_mut(),
            Some(_) => unreachable!(),
            None => None,
        }
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [timestamp => timestamp, timestamp_mut => timestamp_mut, Prop<S, DateTime<Utc>>],
        [uid => uid, uid_mut => uid_mut, Prop<S, Uid<S>>],
    }

    pub fn status(&self) -> Option<&Prop<S, TodoStatus>> {
        match self.props.get(PropKey::Static(StaticPropName::TodoStatus)) {
            Some(PropEntry::Static(StaticProp::TodoStatus(mult))) => mult.as_one(),
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn status_mut(&mut self) -> Option<&mut Prop<S, TodoStatus>> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::TodoStatus))
        {
            Some(PropEntry::Static(StaticProp::TodoStatus(mult))) => mult.as_one_mut(),
            Some(_) => unreachable!(),
            None => None,
        }
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
            match self.props.trigger() {
                Some(TriggerPropMultRef::Absolute(MultRef::One(prop))) => {
                    TriggerPropRef::Absolute(prop)
                }
                Some(TriggerPropMultRef::Relative(MultRef::One(prop))) => {
                    TriggerPropRef::Relative(prop)
                }
                _ => unreachable!(),
            }
        }

        pub fn trigger_mut(&mut self) -> TriggerPropMut<'_, S> {
            match self.props.trigger_mut() {
                Some(TriggerPropMultMut::Absolute(Mult::One(prop))) => {
                    TriggerPropMut::Absolute(prop)
                }
                Some(TriggerPropMultMut::Relative(Mult::One(prop))) => {
                    TriggerPropMut::Relative(prop)
                }
                _ => unreachable!(),
            }
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
    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> AudioAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, AudioAction> {
        match self.props.get(PropKey::Static(StaticPropName::AudioAction)) {
            Some(PropEntry::Static(StaticProp::AudioAction(mult))) => mult.as_one().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, AudioAction> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::AudioAction))
        {
            Some(PropEntry::Static(StaticProp::AudioAction(mult))) => mult.as_one_mut().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        //[Trigger, trigger, trigger_mut, TriggerProp<S>],
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
    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> DisplayAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, DisplayAction> {
        match self
            .props
            .get(PropKey::Static(StaticPropName::DisplayAction))
        {
            Some(PropEntry::Static(StaticProp::DisplayAction(mult))) => mult.as_one().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, DisplayAction> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::DisplayAction))
        {
            Some(PropEntry::Static(StaticProp::DisplayAction(mult))) => mult.as_one_mut().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [description => description, description_mut => description_mut, Prop<S, Text<S>, TextParams<S>>],
        //[Trigger, trigger, trigger_mut, TriggerProp<S>],
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
    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> EmailAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, EmailAction> {
        match self.props.get(PropKey::Static(StaticPropName::EmailAction)) {
            Some(PropEntry::Static(StaticProp::EmailAction(mult))) => mult.as_one().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, EmailAction> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::EmailAction))
        {
            Some(PropEntry::Static(StaticProp::EmailAction(mult))) => mult.as_one_mut().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [description => description, description_mut => description_mut, Prop<S, Text<S>, TextParams<S>>],
        //[Trigger, trigger, trigger_mut, TriggerProp<S>],
        [summary => summary, summary_mut => summary_mut, Prop<S, Text<S>, TextParams<S>>],
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
    pub const fn subcomponents(&self) -> &[OtherComponent<S>] {
        self.subcomponents.as_slice()
    }

    pub const fn subcomponents_mut(&mut self) -> &mut Vec<OtherComponent<S>> {
        &mut self.subcomponents
    }
}

impl<S> OtherAlarm<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> &Prop<S, UnknownAction<S>> {
        match self
            .props
            .get(PropKey::Static(StaticPropName::UnknownAction))
        {
            Some(PropEntry::Static(StaticProp::UnknownAction(mult))) => mult.as_one().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    pub fn action_mut(&mut self) -> &mut Prop<S, UnknownAction<S>> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::UnknownAction))
        {
            Some(PropEntry::Static(StaticProp::UnknownAction(mult))) => mult.as_one_mut().unwrap(),
            Some(_) | None => unreachable!(),
        }
    }

    mandatory_accessors! {
        [description => description, description_mut => description_mut, Prop<S, Text<S>, TextParams<S>>],
        //[Trigger, trigger, trigger_mut, TriggerProp<S>],
        [summary => summary, summary_mut => summary_mut, Prop<S, Text<S>, TextParams<S>>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [timestamp => timestamp, timestamp_mut => timestamp_mut, Prop<S, DateTime<Utc>>],
        [uid => uid, uid_mut => uid_mut, Prop<S, Uid<S>>],
    }

    pub fn status(&self) -> Option<&Prop<S, JournalStatus>> {
        match self
            .props
            .get(PropKey::Static(StaticPropName::JournalStatus))
        {
            Some(PropEntry::Static(StaticProp::JournalStatus(mult))) => mult.as_one(),
            Some(_) => unreachable!(),
            None => None,
        }
    }

    pub fn status_mut(&mut self) -> Option<&mut Prop<S, JournalStatus>> {
        match self
            .props
            .get_mut(PropKey::Static(StaticPropName::JournalStatus))
        {
            Some(PropEntry::Static(StaticProp::JournalStatus(mult))) => mult.as_one_mut(),
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
        [Attach, attachments, attachments_mut, MultiProp<S, AttachValue<S>, AttachParams<S>>],
        [Attendee, attendees, attendees_mut, MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, MultiProp<S, Vec<Text<S>>, LangParams<S>>],
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [Description, descriptions, descriptions_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [ExDate, exception_dates, exception_dates_mut, MultiProp<S, ExDateSeq, DtParams<S>>],
        [RelatedTo, relateds, relateds_mut, MultiProp<S, Text<S>, RelTypeParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, MultiProp<S, RDateSeq, DtParams<S>>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [timestamp => timestamp, timestamp_mut => timestamp_mut, Prop<S, DateTime<Utc>>],
        [uid => uid, uid_mut => uid_mut, Prop<S, Uid<S>>],
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
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {
        [id => tz_id, id_mut => tz_id_mut, Prop<S, TzId<S>>],
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
        [start => dt_start, start_mut => dt_start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [offset_to => tz_offset_to, offset_to_mut => tz_offset_to_mut, Prop<S, UtcOffset>],
        [offset_from => tz_offset_from, offset_from_mut => tz_offset_from_mut, Prop<S, UtcOffset>],
    }

    optional_accessors! {
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
    }

    seq_accessors! {
        [Comment, comments, comments_mut, MultiProp<S, Text<S>, TextParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, MultiProp<S, RDateSeq, DtParams<S>>],
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

/// An arbitrary component which may have any properties and subcomponents.
#[derive(Debug, Clone)]
pub struct OtherComponent<S> {
    name: UnknownName<S>,
    props: PropertyTable<S>,
    subcomponents: Vec<OtherComponent<S>>,
}

// TODO: define accessor methods for OtherComponent

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

/// A table of every possible property and every possible [multiplicity](Mult).
#[derive(Clone)]
pub struct PropertyTable<S>(HashTable<PropEntry<S>>, RandomState);

impl<S: Debug> Debug for PropertyTable<S> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        <HashTable<PropEntry<S>> as Debug>::fmt(&self.0, f)
    }
}

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
    ($([$prop:ident, $name_ref:ident, $name_mut:ident, $value_ty:ty, $param_ty:ty]),* $(,)?) => {
        $(
            pub fn $name_ref (&self) -> Option<MultRef<'_, S, $value_ty, $param_ty>>
            where
                S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let key = PropKey::Static(StaticPropName::$prop);
                let raw_entry = self.get(key);

                match raw_entry {
                    Some(PropEntry::Static(StaticProp::$prop(mult))) => Some(mult.as_ref()),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }

            pub fn $name_mut (&mut self) -> Option<&mut Mult<S, $value_ty, $param_ty>>
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

// TODO: modify the macro above to make some methods non-public, so we can hide e.g.
// trigger_absolute from the public API for PropertyTable

impl<S> PropertyTable<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn action(&self) -> Option<ActionPropMultRef<'_, S>> {
        self.audio_action()
            .map(ActionPropMultRef::Audio)
            .or_else(|| self.display_action().map(ActionPropMultRef::Display))
            .or_else(|| self.email_action().map(ActionPropMultRef::Email))
            .or_else(|| self.unknown_action().map(ActionPropMultRef::Unknown))
    }

    pub fn action_mut(&mut self) -> Option<ActionPropMultMut<'_, S>> {
        match self.action()? {
            ActionPropMultRef::Audio(_) => self.audio_action_mut().map(ActionPropMultMut::Audio),
            ActionPropMultRef::Display(_) => {
                self.display_action_mut().map(ActionPropMultMut::Display)
            }
            ActionPropMultRef::Email(_) => self.email_action_mut().map(ActionPropMultMut::Email),
            ActionPropMultRef::Unknown(_) => {
                self.unknown_action_mut().map(ActionPropMultMut::Unknown)
            }
        }
    }

    /// Returns a mutable reference to the value of the AnyAction variant, initializing it if
    /// necessary.
    pub(crate) fn any_action_mut_or_init(&mut self) -> &mut Vec<MultiProp<S, AlarmAction<S>>> {
        // initialize the field if it's empty
        let key = PropKey::Static(StaticPropName::AnyAction);
        if self.get(key).is_none() {
            let _prev = self.insert(PropEntry::Static(StaticProp::AnyAction(Vec::new())));
            debug_assert!(_prev.is_none());
        }

        self.get_mut(key)
            .map(|entry| match entry {
                PropEntry::Static(StaticProp::AnyAction(props)) => props,
                _ => unreachable!(),
            })
            .unwrap()
    }

    pub fn trigger(&self) -> Option<TriggerPropMultRef<'_, S>> {
        self.trigger_relative()
            .map(TriggerPropMultRef::Relative)
            .or_else(|| self.trigger_absolute().map(TriggerPropMultRef::Absolute))
    }

    pub fn trigger_mut(&mut self) -> Option<TriggerPropMultMut<'_, S>> {
        match self.trigger()? {
            TriggerPropMultRef::Relative(_) => self
                .trigger_relative_mut()
                .map(TriggerPropMultMut::Relative),
            TriggerPropMultRef::Absolute(_) => self
                .trigger_absolute_mut()
                .map(TriggerPropMultMut::Absolute),
        }
    }

    /// Returns a mutable reference to the value of the AnyTrigger variant, initializing it if
    /// necessary.
    pub(crate) fn any_trigger_mut_or_init(&mut self) -> &mut Vec<AnyTriggerProp<S>> {
        // initialize the field if necessary
        let key = PropKey::Static(StaticPropName::AnyTrigger);
        if self.get(key).is_none() {
            let _prev = self.insert(PropEntry::Static(StaticProp::AnyTrigger(Vec::new())));
            debug_assert!(_prev.is_none());
        }

        self.get_mut(key)
            .map(|entry| match entry {
                PropEntry::Static(StaticProp::AnyTrigger(props)) => props,
                _ => unreachable!(),
            })
            .unwrap()
    }

    /// Returns `true` iff `self` contains a STATUS value.
    pub(crate) fn has_status(&self) -> bool {
        self.event_status().is_some()
            || self.todo_status().is_some()
            || self.journal_status().is_some()
            || self.status().is_some()
    }

    /// Returns a mutable reference to the value of the OtherStatus variant, initializing it if
    /// necessary. This function will panic if the variant is active and set to a Mult::One value.
    pub(crate) fn status_mut_or_init(&mut self) -> &mut Vec<MultiProp<S, Status>> {
        // initialize the field if it's missing
        if self.status().is_none() {
            let _prev = self.insert(PropEntry::Static(StaticProp::OtherStatus(Mult::Seq(
                Vec::new(),
            ))));
            debug_assert!(_prev.is_none());
        }

        match self.status_mut() {
            Some(Mult::Seq(props)) => props,
            _ => unreachable!(),
        }
    }

    /// Returns a mutable reference to the value of the AnyStructuredData variant, initializing it
    /// if necessary.
    pub(crate) fn any_structured_data_mut_or_init(&mut self) -> &mut Vec<AnyStructuredDataProp<S>> {
        let key = PropKey::Static(StaticPropName::AnyStructuredData);
        if self.get(key).is_none() {
            let _prev = self.insert(PropEntry::Static(StaticProp::AnyStructuredData(Vec::new())));
            debug_assert!(_prev.is_none());
        }

        self.get_mut(key)
            .map(|entry| match entry {
                PropEntry::Static(StaticProp::AnyStructuredData(props)) => props,
                _ => unreachable!(),
            })
            .unwrap()
    }
}

impl<S> PropertyTable<S> {
    property_table_accessors! {
        // CALENDAR PROPERTIES
        [CalScale, cal_scale, cal_scale_mut, (), ()],
        [Method, method, method_mut, Method<S>, ()],
        [ProdId, prod_id, prod_id_mut, Text<S>, ()],
        [Version, version, version_mut, (), ()],
        // DESCRIPTIVE COMPONENT PROPERTIES
        [Attach, attachment, attachment_mut, AttachValue<S>, AttachParams<S>],
        [Categories, categories, categories_mut, Vec<Text<S>>, LangParams<S>],
        [Class, class, class_mut, ClassValue<S>, ()],
        [Comment, comment, comment_mut, Text<S>, TextParams<S>],
        [Description, description, description_mut, Text<S>, TextParams<S>],
        [Geo, geo, geo_mut, Geo, ()],
        [Location, location, location_mut, Text<S>, TextParams<S>],
        [PercentComplete, percent_complete, percent_complete_mut, CompletionPercentage, ()],
        [Priority, priority, priority_mut, Priority, ()],
        [Resources, resources, resources_mut, Vec<Text<S>>, TextParams<S>],
        [OtherStatus, status, status_mut, Status, ()],
        [EventStatus, event_status, event_status_mut, EventStatus, ()],
        [TodoStatus, todo_status, todo_status_mut, TodoStatus, ()],
        [JournalStatus, journal_status, journal_status_mut, JournalStatus, ()],
        [Summary, summary, summary_mut, Text<S>, TextParams<S>],
        // DATE AND TIME COMPONENT PROPERTIES
        [DtCompleted, dt_completed, dt_completed_mut, DateTime<Utc>, ()],
        [DtEnd, dt_end, dt_end_mut, DateTimeOrDate, DtParams<S>],
        [DtDue, dt_due, dt_due_mut, DateTimeOrDate, DtParams<S>],
        [DtStart, dt_start, dt_start_mut, DateTimeOrDate, DtParams<S>],
        [Duration, duration, duration_mut, Duration, ()],
        [FreeBusy, free_busy, free_busy_mut, Vec<Period>, FBTypeParams<S>],
        [Transp, time_transparency, time_transparency_mut, TimeTransparency, ()],
        // TIME ZONE COMPONENT PROPERTIES
        [TzId, tz_id, tz_id_mut, TzId<S>, ()],
        [TzName, tz_name, tz_name_mut, Text<S>, LangParams<S>],
        [TzOffsetFrom, tz_offset_from, tz_offset_from_mut, UtcOffset, ()],
        [TzOffsetTo, tz_offset_to, tz_offset_to_mut, UtcOffset, ()],
        [TzUrl, tz_url, tz_url_mut, Uri<S>, ()],
        // RELATIONSHIP COMPONENT PROPERTIES
        [Attendee, attendee, attendee_mut, CalAddress<S>, Box<AttendeeParams<S>>],
        [Contact, contact, contact_mut, Text<S>, TextParams<S>],
        [Organizer, organizer, organizer_mut, CalAddress<S>, Box<OrganizerParams<S>>],
        [RecurId, recurrence_id, recurrence_id_mut, DateTimeOrDate, RecurrenceIdParams<S>],
        [RelatedTo, related_to, related_to_mut, Text<S>, RelTypeParams<S>],
        [Url, url, url_mut, Uri<S>, ()],
        [Uid, uid, uid_mut, Uid<S>, ()],
        // RECURRENCE COMPONENT PROPERTIES
        [ExDate, exception_dates, exception_dates_mut, ExDateSeq, DtParams<S>],
        [RDate, recurrence_dates, recurrence_dates_mut, RDateSeq, DtParams<S>],
        [RRule, rrule, rrule_mut, Box<RRule>, ()],
        // ALARM COMPONENT PROPERTIES
        [AudioAction, audio_action, audio_action_mut, AudioAction, ()],
        [DisplayAction, display_action, display_action_mut, DisplayAction, ()],
        [EmailAction, email_action, email_action_mut, EmailAction, ()],
        [UnknownAction, unknown_action, unknown_action_mut, UnknownAction<S>, ()],
        [TriggerRelative, trigger_relative, trigger_relative_mut, Duration, TriggerParams],
        [TriggerAbsolute, trigger_absolute, trigger_absolute_mut, DateTime<Utc>, ()],
        [Repeat, repeat, repeat_mut, Integer, ()],
        // CHANGE MANAGEMENT COMPONENT PROPERTIES
        [Created, created, created_mut, DateTime<Utc>, ()],
        [DtStamp, timestamp, timestamp_mut, DateTime<Utc>, ()],
        [LastModified, last_modified, last_modified_mut, DateTime<Utc>, ()],
        [Sequence, sequence, sequence_mut, Integer, ()],
        // MISCELLANEOUS COMPONENT PROPERTIES
        [RequestStatus, request_status, request_status_mut, RequestStatus<S>, LangParams<S>],
        // RFC 7986 PROPERTIES
        [Name, name, name_mut, Text<S>, TextParams<S>],
        [RefreshInterval, refresh_interval, refresh_interval_mut, Duration, ()],
        [Source, source, source_mut, Uri<S>, ()],
        [Color, color, color_mut, Css3Color, ()],
        [Image, image, image_mut, ImageData<S>, ImageParams<S>],
        [Conference, conference, conference_mut, Uri<S>, ConfParams<S>],
        // RFC 9074 PROPERTIES
        [Acknowledged, acknowledged, acknowledged_mut, DateTime<Utc>, ()],
        [Proximity, proximity, proximity_mut, ProximityValue<S>, ()],
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

/// A distinct multiplicity of properties.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mult<S, V, P = ()> {
    One(Prop<S, V, P>),
    Seq(Vec<MultiProp<S, V, P>>),
}

/// A reference to a multiplicity of `T`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MultRef<'a, S, V, P = ()> {
    /// Exactly one value.
    One(&'a Prop<S, V, P>),
    /// Zero or more values.
    Seq(&'a [MultiProp<S, V, P>]),
}

impl<S, V, P> Mult<S, V, P> {
    pub const fn as_ref(&self) -> MultRef<'_, S, V, P> {
        match self {
            Mult::One(x) => MultRef::One(x),
            Mult::Seq(xs) => MultRef::Seq(xs.as_slice()),
        }
    }

    pub const fn as_one(&self) -> Option<&Prop<S, V, P>> {
        if let Self::One(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_one_mut(&mut self) -> Option<&mut Prop<S, V, P>> {
        if let Self::One(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub const fn as_seq(&self) -> Option<&[MultiProp<S, V, P>]> {
        if let Self::Seq(v) = self {
            Some(v.as_slice())
        } else {
            None
        }
    }

    pub const fn as_seq_mut(&mut self) -> Option<&mut Vec<MultiProp<S, V, P>>> {
        if let Self::Seq(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Moves any value in [`Mult::One`] to [`Mult::Seq`] by allocating a new vector.
    pub fn seq_in_place(&mut self) {
        if let Mult::One(_) = self {
            let vec = Mult::Seq(Vec::with_capacity(1));
            let Mult::One(value) = std::mem::replace(self, vec) else {
                unreachable!()
            };
            self.as_seq_mut().unwrap().push(value.into_multi_prop());
        }
    }

    /// Tries to convert `self` into a [`Mult::One`]. If `self` is a [`Mult::Seq`] with a number of
    /// elements other than 1, this fails, and the number of elements is returned. If `self` has
    /// exactly one element, but that element has an ORDER value, this fails and the returned error
    /// is [`None`].
    pub fn one_in_place(&mut self) -> Result<(), Option<usize>> {
        match self {
            Mult::One(_) => Ok(()),
            Mult::Seq(items) if items.len() != 1 => Err(Some(items.len())),
            Mult::Seq(items) => {
                let item = match items.pop().unwrap().try_into_single_prop() {
                    Some(item) => item,
                    None => return Err(None),
                };
                let _vec = std::mem::replace(self, Mult::One(item));
                Ok(())
            }
        }
    }
}

impl<'a, S, V, P> MultRef<'a, S, V, P> {
    pub const fn as_one(&self) -> Option<&'a Prop<S, V, P>> {
        if let MultRef::One(x) = self {
            Some(x)
        } else {
            None
        }
    }

    pub const fn as_seq(&self) -> Option<&'a [MultiProp<S, V, P>]> {
        if let MultRef::Seq(x) = self {
            Some(x)
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
    CalScale(Mult<S, ()>),
    Method(Mult<S, Method<S>>),
    ProdId(Mult<S, Text<S>>),
    Version(Mult<S, ()>),
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(Mult<S, AttachValue<S>, AttachParams<S>>),
    Categories(Mult<S, Vec<Text<S>>, LangParams<S>>),
    Class(Mult<S, ClassValue<S>>),
    Comment(Mult<S, Text<S>, TextParams<S>>),
    Description(Mult<S, Text<S>, TextParams<S>>),
    Geo(Mult<S, Geo>),
    Location(Mult<S, Text<S>, TextParams<S>>),
    PercentComplete(Mult<S, CompletionPercentage>),
    Priority(Mult<S, Priority>),
    Resources(Mult<S, Vec<Text<S>>, TextParams<S>>),
    EventStatus(Mult<S, EventStatus>),
    TodoStatus(Mult<S, TodoStatus>),
    JournalStatus(Mult<S, JournalStatus>),
    OtherStatus(Mult<S, Status>),
    Summary(Mult<S, Text<S>, TextParams<S>>),
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted(Mult<S, DateTime<Utc>>),
    DtEnd(Mult<S, DateTimeOrDate, DtParams<S>>),
    DtDue(Mult<S, DateTimeOrDate, DtParams<S>>),
    DtStart(Mult<S, DateTimeOrDate, DtParams<S>>),
    Duration(Mult<S, Duration>),
    FreeBusy(Mult<S, Vec<Period>, FBTypeParams<S>>),
    Transp(Mult<S, TimeTransparency>),
    // TIME ZONE COMPONENT PROPERTIES
    TzId(Mult<S, TzId<S>>),
    TzName(Mult<S, Text<S>, LangParams<S>>),
    TzOffsetFrom(Mult<S, UtcOffset>),
    TzOffsetTo(Mult<S, UtcOffset>),
    TzUrl(Mult<S, Uri<S>>),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(Mult<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Contact(Mult<S, Text<S>, TextParams<S>>),
    Organizer(Mult<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    RecurId(Mult<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    RelatedTo(Mult<S, Text<S>, RelTypeParams<S>>),
    Url(Mult<S, Uri<S>>),
    Uid(Mult<S, Uid<S>>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(Mult<S, ExDateSeq, DtParams<S>>),
    RDate(Mult<S, RDateSeq, DtParams<S>>),
    RRule(Mult<S, Box<RRule>>),
    // ALARM COMPONENT PROPERTIES
    AudioAction(Mult<S, AudioAction>),
    DisplayAction(Mult<S, DisplayAction>),
    EmailAction(Mult<S, EmailAction>),
    UnknownAction(Mult<S, UnknownAction<S>>),
    Repeat(Mult<S, Integer>),
    TriggerRelative(Mult<S, Duration, TriggerParams>),
    TriggerAbsolute(Mult<S, DateTime<Utc>>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(Mult<S, DateTime<Utc>>),
    DtStamp(Mult<S, DateTime<Utc>>),
    LastModified(Mult<S, DateTime<Utc>>),
    Sequence(Mult<S, Integer>),
    // MISCELLANEOUS COMPONENT PROPERTIES
    RequestStatus(Mult<S, RequestStatus<S>, LangParams<S>>),
    // RFC 7986 PROPERTIES
    Name(Mult<S, Text<S>, TextParams<S>>),
    RefreshInterval(Mult<S, Duration>),
    Source(Mult<S, Uri<S>>),
    Color(Mult<S, Css3Color>),
    Image(Mult<S, ImageData<S>, ImageParams<S>>),
    Conference(Mult<S, Uri<S>, ConfParams<S>>),
    // RFC 9073 PROPERTIES
    LocationType(Mult<S, Vec<Text<S>>>),
    ParticipantType(Mult<S, ParticipantType<S>>),
    ResourceType(Mult<S, ResourceType<S>>),
    CalendarAddress(Mult<S, CalAddress<S>>),
    StyledDescription(Mult<S, StyledDescriptionValue<S>, StyledDescriptionParams<S>>),
    StructuredDataBinary(Prop<S, Binary<S>, StructuredDataParams<S>>),
    StructuredDataText(Prop<S, Text<S>, StructuredDataParams<S>>),
    StructuredDataUri(Prop<S, Uri<S>, UriStructuredDataParams<S>>),
    // RFC 9074 PROPERTIES
    Acknowledged(Mult<S, DateTime<Utc>>),
    Proximity(Mult<S, ProximityValue<S>>),

    // UNKNOWN PSEUDOPROPERTIES
    // These properties are used only as collection types for unknown components, and are inactive
    // otherwise. Note that we don't have a separate variant for the STATUS property, since we can
    // rely on the OtherStatus variant.
    AnyAction(Vec<MultiProp<S, AlarmAction<S>>>),
    AnyTrigger(Vec<AnyTriggerProp<S>>),
    AnyStructuredData(Vec<AnyStructuredDataProp<S>>)
}}

impl<S> PropertyTable<S>
where
    S: Hash + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub(crate) fn try_into_alarm(
        mut self,
        subcomponents: Vec<OtherComponent<S>>,
    ) -> Result<Alarm<S>, CalendarParseError<S>> {
        macro_rules! one {
            ($scrut:expr, $prop:expr) => {
                match $scrut {
                    Some(MultRef::One(x)) => Ok(x),
                    Some(MultRef::Seq(_)) => Err(CalendarParseError::MoreThanOneProp {
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
                    Some(MultRef::One(x)) => Ok(Some(x)),
                    Some(MultRef::Seq(_)) => Err(CalendarParseError::MoreThanOneProp {
                        prop: $prop,
                        component: ComponentKind::Alarm,
                    }),
                    None => Ok(None),
                }
            };
        }

        // get references to fields that need to be inspected
        let duration = zero_or_one!(
            self.duration(),
            PropName::Rfc5545(Rfc5545PropName::Duration)
        )?;
        let repeat = zero_or_one!(
            self.repeat(),
            PropName::Rfc5545(Rfc5545PropName::RepeatCount)
        )?;

        // check multiplicities for other fields as necessary
        let _ = zero_or_one!(
            self.uid(),
            PropName::Rfc5545(Rfc5545PropName::UniqueIdentifier)
        )?;
        let _ = zero_or_one!(
            self.acknowledged(),
            PropName::Rfc9074(Rfc9074PropName::Acknowledged)
        )?;
        let _ = zero_or_one!(
            self.proximity(),
            PropName::Rfc9074(Rfc9074PropName::Proximity)
        )?;

        enum ActionKind {
            Audio,
            Display,
            Email,
            Unknown,
        }

        // get reference to action with correct multiplicity
        let action = match self.action() {
            Some(ActionPropMultRef::Audio(MultRef::One(_))) => Ok(ActionKind::Audio),
            Some(ActionPropMultRef::Display(MultRef::One(_))) => Ok(ActionKind::Display),
            Some(ActionPropMultRef::Email(MultRef::One(_))) => Ok(ActionKind::Email),
            Some(ActionPropMultRef::Unknown(MultRef::One(_))) => Ok(ActionKind::Unknown),
            Some(_) => Err(CalendarParseError::MoreThanOneProp {
                prop: PropName::Rfc5545(Rfc5545PropName::Action),
                component: ComponentKind::Alarm,
            }),
            None => Err(CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::Action),
                component: ComponentKind::Alarm,
            }),
        }?;

        // check trigger multiplicity
        let () = match self.trigger() {
            Some(TriggerPropMultRef::Relative(MultRef::One(_)))
            | Some(TriggerPropMultRef::Absolute(MultRef::One(_))) => Ok(()),
            Some(_) => Err(CalendarParseError::MoreThanOneProp {
                prop: PropName::Rfc5545(Rfc5545PropName::Trigger),
                component: ComponentKind::Alarm,
            }),
            None => Err(CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::Trigger),
                component: ComponentKind::Alarm,
            }),
        }?;

        // check that duration and repeat occur together
        let () = match (duration, repeat) {
            (Some(_), Some(_)) => Ok(()),
            (None, None) => Ok(()),
            (None, Some(_)) => Err(CalendarParseError::RepeatWithoutDuration),
            (Some(_), None) => Err(CalendarParseError::DurationWithoutRepeat),
        }?;

        Ok(match action {
            ActionKind::Audio => {
                // props: attachments (0-1)

                let () = match self.attachment_mut().map(Mult::one_in_place) {
                    Some(Ok(())) | None => Ok(()),
                    Some(Err(_)) => Err(CalendarParseError::TooManyAttachmentsOnAudioAlarm),
                }?;

                Alarm::Audio(AudioAlarm {
                    props: self,
                    subcomponents,
                })
            }
            ActionKind::Display => {
                // props: description (1)

                let _ = one!(
                    self.description(),
                    PropName::Rfc5545(Rfc5545PropName::Description)
                )?;

                Alarm::Display(DisplayAlarm {
                    props: self,
                    subcomponents,
                })
            }
            ActionKind::Email => {
                // props: description (1), summary (1), attendees (1+)

                let _ = one!(
                    self.description(),
                    PropName::Rfc5545(Rfc5545PropName::Description)
                )?;
                let _ = one!(self.summary(), PropName::Rfc5545(Rfc5545PropName::Summary))?;

                // check for one or more attendees
                let () = match self.attendee_mut() {
                    Some(Mult::Seq(xs)) if !xs.is_empty() => Ok(()),
                    Some(prop @ Mult::One(_)) => {
                        prop.seq_in_place();
                        Ok(())
                    }
                    _ => Err(CalendarParseError::MissingProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
                        component: ComponentKind::EmailAlarm,
                    }),
                }?;

                Alarm::Email(EmailAlarm {
                    props: self,
                    subcomponents,
                })
            }
            ActionKind::Unknown => Alarm::Other(OtherAlarm {
                props: self,
                subcomponents,
            }),
        })
    }
}

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
