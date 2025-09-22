//! Model types for calendar components.

use paste::paste;

use std::fmt::Debug;

use crate::{
    model::{
        css::Css3Color,
        one_or_seq::OneOrSeq,
        parameter::Params,
        primitive::{
            AttachValue, AudioAction, CalAddress, ClassValue, CompletionPercentage, DateTime,
            DateTimeOrDate, DisplayAction, Duration, EmailAction, EventStatus, ExDateSeq, Geo,
            Gregorian, ImageData, Integer, JournalStatus, Method, Period, Priority, ProximityValue,
            RDateSeq, RequestStatus, Text, TimeTransparency, TodoStatus, TzId, Uid, UnknownAction,
            Uri, Utc, UtcOffset, Version,
        },
        property::{
            EventTerminationMut, EventTerminationRef, Prop, PropertyTable, StaticProp,
            TodoTerminationMut, TodoTerminationRef, TriggerPropMut, TriggerPropRef,
        },
        rrule::RRule,
        table::HashCaseless,
    },
    parser::escaped::Equiv,
};

macro_rules! mandatory_accessors {
    ($([$key:ident, $name:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> &$ret {
                self.props.get_known(StaticProp::$key).unwrap().try_into().unwrap()
            }

            paste! {
            pub fn [<$name _mut>](&mut self) -> &mut $ret {
                self.props.get_known_mut(StaticProp::$key).unwrap().try_into().unwrap()
            }
            }
        )*
    };
}

macro_rules! optional_accessors {
    ($([$key:ident, $name:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&$ret> {
                match self.props.get_known(StaticProp::$key) {
                    Some(raw_value) => Some(raw_value.try_into().unwrap()),
                    None => None,
                }
            }

            paste! {
            pub fn [<$name _mut>](&mut self) -> Option<&mut $ret> {
                match self.props.get_known_mut(StaticProp::$key) {
                    Some(raw_value) => Some(raw_value.try_into().unwrap()),
                    None => None,
                }
            }
            }
        )*
    };
}

macro_rules! seq_accessors {
    ($([$key:ident, $name:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&[$ret]> {
                match self.props.get_known(StaticProp::$key) {
                    Some(raw_value) => {
                        let vs: &OneOrSeq<_> = raw_value.try_into().unwrap();
                        Some(vs.as_slice())
                    },
                    None => None,
                }
            }

            paste! {
            pub fn [<$name _mut>](&mut self) -> Option<&mut OneOrSeq<$ret>> {
                match self.props.get_known_mut(StaticProp::$key) {
                    Some(raw_value) => {
                        let vs: &mut OneOrSeq<_> = raw_value.try_into().unwrap();
                        Some(vs)
                    },
                    None => None,
                }
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
        [ProdId, prod_id, Prop<Text<S>, Params<S>>],
        [Version, version, Prop<Version, Params<S>>],
    }

    optional_accessors! {
        [CalScale, scale, Prop<Gregorian, Params<S>>],
        [Method, method, Prop<Method<S>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
        [LastModified, last_modified, Prop<DateTime<Utc>, Params<S>>],
        [Url, url, Prop<Uri<S>, Params<S>>],
        [RefreshInterval, refresh_interval, Prop<Duration, Params<S>>],
        [Source, source, Prop<Uri<S>, Params<S>>],
        [Color, color, Prop<Css3Color, Params<S>>],
    }

    seq_accessors! {
        [Name, names, Prop<Text<S>, Params<S>>],
        [Description, description, Prop<Text<S>, Params<S>>],
        [Categories, categories, Prop<Vec<Text<S>>, Params<S>>],
        [Image, images, Prop<ImageData<S>, Params<S>>],
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
        [DtStamp, timestamp, Prop<DateTime<Utc>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
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
        [Status, status, Prop<EventStatus, Params<S>>],
        [DtStart, start, Prop<DateTimeOrDate, Params<S>>],
        [DtEnd, end, Prop<DateTimeOrDate, Params<S>>],
        [Duration, duration, Prop<Duration, Params<S>>],
        [Class, class, Prop<ClassValue<S>, Params<S>>],
        [Created, created, Prop<DateTime<Utc>, Params<S>>],
        [Description, description, Prop<Text<S>, Params<S>>],
        [Geo, geo, Prop<Geo, Params<S>>],
        [LastModified, last_modified, Prop<DateTime<Utc>, Params<S>>],
        [Location, location, Prop<Text<S>, Params<S>>],
        [Organizer, organizer, Prop<CalAddress<S>, Params<S>>],
        [Priority, priority, Prop<Priority, Params<S>>],
        [Sequence, sequence_number, Prop<Integer, Params<S>>],
        [Summary, summary, Prop<Text<S>, Params<S>>],
        [Transp, transparency, Prop<TimeTransparency, Params<S>>],
        [Url, url, Prop<Uri<S>, Params<S>>],
        [RecurId, recurrence_id, Prop<DateTimeOrDate, Params<S>>],
        [Color, color, Prop<Css3Color, Params<S>>],
    }

    seq_accessors! {
        [Attach, attachments, Prop<AttachValue<S>, Params<S>>],
        [Attendee, attendees, Prop<CalAddress<S>, Params<S>>],
        [Categories, categories, Prop<Vec<Text<S>>, Params<S>>],
        [Comment, comments, Prop<Text<S>, Params<S>>],
        [Contact, contacts, Prop<Text<S>, Params<S>>],
        [RRule, rrule, Prop<Box<RRule>, Params<S>>],
        [ExDate, exception_dates, Prop<ExDateSeq, Params<S>>],
        [RequestStatus, request_statuses, Prop<RequestStatus<S>, Params<S>>],
        [RelatedTo, relateds, Prop<Uid<S>, Params<S>>],
        [Resources, resources, Prop<Vec<Text<S>>, Params<S>>],
        [RDate, recurrence_dates, Prop<RDateSeq, Params<S>>],
        [Conference, conferences, Prop<Uri<S>, Params<S>>],
        [Image, images, Prop<ImageData<S>, Params<S>>],
    }
}

/// A VTODO component (RFC 5545 §3.6.2).
#[derive(Debug, Clone)]
pub struct Todo<S> {
    props: PropertyTable<S>,
    alarms: Vec<Alarm<S>>,
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
        [DtStamp, timestamp, Prop<DateTime<Utc>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
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
        [Status, status, Prop<TodoStatus, Params<S>>],
        [Class, class, Prop<ClassValue<S>, Params<S>>],
        [DtCompleted, completed, Prop<DateTime<Utc>, Params<S>>],
        [Created, created, Prop<DateTime<Utc>, Params<S>>],
        [Description, description, Prop<Text<S>, Params<S>>],
        [DtStart, start, Prop<DateTimeOrDate, Params<S>>],
        [DtDue, due, Prop<DateTimeOrDate, Params<S>>],
        [Duration, duration, Prop<Duration, Params<S>>],
        [Geo, geo, Prop<Geo, Params<S>>],
        [LastModified, last_modified, Prop<DateTime<Utc>, Params<S>>],
        [Location, location, Prop<Text<S>, Params<S>>],
        [Organizer, organizer, Prop<CalAddress<S>, Params<S>>],
        [PercentComplete, percent, Prop<CompletionPercentage, Params<S>>],
        [Priority, priority, Prop<Priority, Params<S>>],
        [RecurId, recurrence_id, Prop<DateTimeOrDate, Params<S>>],
        [Sequence, sequence_number, Prop<Integer, Params<S>>],
        [Summary, summary, Prop<Text<S>, Params<S>>],
        [Url, url, Prop<Uri<S>, Params<S>>],
        [Color, color, Prop<Css3Color, Params<S>>],
    }

    seq_accessors! {
        [Attach, attachments, Prop<AttachValue<S>, Params<S>>],
        [Attendee, attendees, Prop<CalAddress<S>, Params<S>>],
        [Categories, categories, Prop<Vec<Text<S>>, Params<S>>],
        [Comment, comments, Prop<Text<S>, Params<S>>],
        [Contact, contacts, Prop<Text<S>, Params<S>>],
        [RRule, rrule, Prop<Box<RRule>, Params<S>>],
        [ExDate, exception_dates, Prop<ExDateSeq, Params<S>>],
        [RequestStatus, request_statuses, Prop<RequestStatus<S>, Params<S>>],
        [RelatedTo, relateds, Prop<Uid<S>, Params<S>>],
        [Resources, resources, Prop<Vec<Text<S>>, Params<S>>],
        [RDate, recurrence_dates, Prop<RDateSeq, Params<S>>],
        [Conference, conferences, Prop<Uri<S>, Params<S>>],
        [Image, images, Prop<ImageData<S>, Params<S>>],
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
        pub fn duration_and_repeat(
            &self,
        ) -> Option<(&Prop<Duration, Params<S>>, &Prop<Integer, Params<S>>)> {
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
        [Action, action, Prop<AudioAction, Params<S>>],
    }

    optional_accessors! {
        [Attach, attachment, Prop<AttachValue<S>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
        [Duration, duration, Prop<Duration, Params<S>>],
        [Repeat, repeat, Prop<Integer, Params<S>>],
        [Acknowledged, acknowleded, Prop<DateTime<Utc>, Params<S>>],
        [Proximity, proximity, Prop<ProximityValue<S>, Params<S>>],
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
        [Action, action, Prop<DisplayAction, Params<S>>],
        [Description, description, Prop<Text<S>, Params<S>>],
    }

    optional_accessors! {
        [Uid, uid, Prop<Uid<S>, Params<S>>],
        [Duration, duration, Prop<Duration, Params<S>>],
        [Repeat, repeat, Prop<Integer, Params<S>>],
        [Acknowledged, acknowleded, Prop<DateTime<Utc>, Params<S>>],
        [Proximity, proximity, Prop<ProximityValue<S>, Params<S>>],
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
        [Action, action, Prop<EmailAction, Params<S>>],
        [Description, description, Prop<Text<S>, Params<S>>],
        [Summary, summary, Prop<Text<S>, Params<S>>],
    }

    optional_accessors! {
        [Uid, uid, Prop<Uid<S>, Params<S>>],
        [Duration, duration, Prop<Duration, Params<S>>],
        [Repeat, repeat, Prop<Integer, Params<S>>],
        [Acknowledged, acknowleded, Prop<DateTime<Utc>, Params<S>>],
        [Proximity, proximity, Prop<ProximityValue<S>, Params<S>>],
    }

    seq_accessors! {
        [Attendee, attendees, Prop<CalAddress<S>, Params<S>>],
        [Attach, attachments, Prop<AttachValue<S>, Params<S>>],
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
        [Action, action, Prop<UnknownAction<S>, Params<S>>],
    }

    optional_accessors! {
        [Description, description, Prop<Text<S>, Params<S>>],
        [Summary, summary, Prop<Text<S>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
        [Duration, duration, Prop<Duration, Params<S>>],
        [Repeat, repeat, Prop<Integer, Params<S>>],
        [Acknowledged, acknowlegded, Prop<DateTime<Utc>, Params<S>>],
        [Proximity, proximity, Prop<ProximityValue<S>, Params<S>>],
    }

    seq_accessors! {
        [Attendee, attendees, Prop<CalAddress<S>, Params<S>>],
        [Attach, attachments, Prop<AttachValue<S>, Params<S>>],
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
        [DtStamp, timestamp, Prop<DateTime<Utc>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
    }

    optional_accessors! {
        [Status, status, Prop<JournalStatus, Params<S>>],
        [Class, class, Prop<ClassValue<S>, Params<S>>],
        [Created, created, Prop<DateTime<Utc>, Params<S>>],
        [DtStart, start, Prop<DateTimeOrDate, Params<S>>],
        [LastModified, last_modified, Prop<DateTime<Utc>, Params<S>>],
        [Organizer, organizer, Prop<CalAddress<S>, Params<S>>],
        [RecurId, recurrence_id, Prop<DateTimeOrDate, Params<S>>],
        [Sequence, sequence_number, Prop<Integer, Params<S>>],
        [Summary, summary, Prop<Text<S>, Params<S>>],
        [Url, url, Prop<Uri<S>, Params<S>>],
    }

    seq_accessors! {
        [Attach, attachments, Prop<AttachValue<S>, Params<S>>],
        [Attendee, attendees, Prop<CalAddress<S>, Params<S>>],
        [Categories, categories, Prop<Vec<Text<S>>, Params<S>>],
        [Comment, comments, Prop<Text<S>, Params<S>>],
        [Contact, contacts, Prop<Text<S>, Params<S>>],
        [Description, descriptions, Prop<Text<S>, Params<S>>],
        [ExDate, exception_dates, Prop<ExDateSeq, Params<S>>],
        [RelatedTo, relateds, Prop<Uid<S>, Params<S>>],
        [RDate, recurrence_dates, Prop<RDateSeq, Params<S>>],
        [RRule, rrule, Prop<Box<RRule>, Params<S>>],
        [RequestStatus, request_statuses, Prop<RequestStatus<S>, Params<S>>],
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
        [DtStamp, timestamp, Prop<DateTime<Utc>, Params<S>>],
        [Uid, uid, Prop<Uid<S>, Params<S>>],
    }

    optional_accessors! {
        [Contact, contact, Prop<Text<S>, Params<S>>],
        [DtStart, start, Prop<DateTimeOrDate, Params<S>>],
        [DtEnd, end, Prop<DateTimeOrDate, Params<S>>],
        [Organizer, organizer, Prop<CalAddress<S>, Params<S>>],
        [Url, url, Prop<Uri<S>, Params<S>>],
    }

    seq_accessors! {
        [Attendee, attendees, Prop<CalAddress<S>, Params<S>>],
        [Comment, comments, Prop<Text<S>, Params<S>>],
        [FreeBusy, free_busy_periods, Prop<Vec<Period>, Params<S>>],
        [RequestStatus, request_statuses, Prop<RequestStatus<S>, Params<S>>],
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
        [TzId, id, Prop<TzId<S>, Params<S>>],
    }

    optional_accessors! {
        [LastModified, last_modified, Prop<DateTime<Utc>, Params<S>>],
        [TzUrl, url, Prop<Uri<S>, Params<S>>],
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
        [DtStart, start, Prop<DateTimeOrDate, Params<S>>],
        [TzOffsetTo, offset_to, Prop<UtcOffset, Params<S>>],
        [TzOffsetFrom, offset_from, Prop<UtcOffset, Params<S>>],
    }

    seq_accessors! {
        [Comment, comments, Prop<Text<S>, Params<S>>],
        [RDate, recurrence_dates, Prop<RDateSeq, Params<S>>],
        [RRule, rrule, Prop<Box<RRule>, Params<S>>],
        [TzName, names, Prop<Text<S>, Params<S>>],
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
