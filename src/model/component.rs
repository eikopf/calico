//! Model types for calendar components.

use std::hash::{BuildHasher, Hash, Hasher, RandomState};

use hashbrown::{HashTable, hash_table::Entry as TableEntry};

use crate::{
    model::primitive::UnknownAction,
    parser::{
        error::{CalendarParseError, ComponentKind},
        escaped::{Equiv, LineFoldCaseless},
        property::{PropName, Rfc5545PropName},
    },
};

use super::{
    css::Css3Color,
    parameter::KnownParam,
    primitive::{
        AlarmAction, AttachValue, AudioAction, CalAddress, ClassValue,
        CompletionPercentage, DateTime, DateTimeOrDate, DateTimeOrDateSeq,
        DisplayAction, Duration, EmailAction, EventStatus, Geo, ImageData,
        Integer, JournalStatus, Method, Period, Priority, RDateSeq,
        RequestStatus, Status, Text, TimeTransparency, TodoStatus, TzId, Uid,
        Uri, Utc, UtcOffset, Value,
    },
    property::{
        AttachParams, AttendeeParams, ConfParams, DtParams,
        EventTerminationProp, FBTypeParams, ImageParams, LangParams,
        OrganizerParams, Prop, RecurrenceIdParams, RelTypeParams, TextParams,
        TodoTerminationProp, TriggerProp,
    },
    rrule::RRule,
};

// WARN: it's technically possible for RRULE to appear multiple times in the same component, even
// though RFC 5545 explicitly says it SHOULD NOT appear more than once in every case. should i
// refactor to allow multiple RRULES in components?

// NOTE: some parameter types (e.g. AttendeeParams) are quite large, and like the types in this
// module they rarely have many fields active. so perhaps they should be refactored to use the
// table pattern here?

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
        pub enum $names_name {
            $($variant),*
        }

        impl $(<$($t),+>)? Disc for $enum_name $(<$($t),+>)? {
            type Discriminant = $names_name;

            fn discriminant(&self) -> Self::Discriminant {
                self.into()
            }
        }

        impl $(<$($t),+>)? From<&$enum_name $(<$($t),+>)?> for $names_name {
            fn from(value: &$enum_name $(<$($t),+>)?) -> $names_name {
                match value {
                    $(
                        $enum_name::$variant(..) => $names_name::$variant,
                    )*
                }
            }
        }
    };
}

/// Creates a table type with the given name generic over a type parameter `S`.
/// The `ident` must be a type of kind `Type -> Type`, i.e with a single type
/// parameter.
macro_rules! table {
    ($(#[ $m:meta ])* $name:ident, $value:ident) => {
        $(#[ $m ])*
        pub struct $name <S> (HashTable<Entry<$value<S>, S>>, RandomState);

        impl<S> Default for $name <S> {
            fn default() -> Self {
                Self::new()
            }
        }

        impl<S> $name<S> {
            pub fn new() -> Self {
                Self(HashTable::new(), RandomState::new())
            }

            pub fn insert(
                &mut self,
                value: Entry<$value<S>, S>
            ) -> Option<Entry<$value<S>, S>>
            where
                S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let hash = Self::hash_entry(&self.1);
                let key = value.as_key();
                let eq = Self::eq(&key);

                match self.0.entry(hash(&value), eq, hash) {
                    TableEntry::Occupied(mut entry) => {
                        Some(std::mem::replace(entry.get_mut(), value))
                    }
                    TableEntry::Vacant(entry) => {
                        entry.insert(value);
                        None
                    }
                }
            }

            pub fn insert_iana(
                &mut self,
                name: S,
                prop: Prop<S, Box<Value<S>>, Box<[KnownParam<S>]>>
            ) where S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]> {
                let key = Key::Iana(&name);

                match self.get_mut(key) {
                    Some(Entry::Iana { props, .. }) => {
                        props.push(prop);
                    }
                    Some(_) => unreachable!(),
                    None => {
                        let entry = Entry::Iana {
                            name,
                            props: vec![prop],
                        };

                        self.insert(entry);
                    }
                }
            }

            pub fn insert_x(
                &mut self,
                name: S,
                prop: Prop<S, Box<Value<S>>, Box<[KnownParam<S>]>>
            ) where S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]> {
                let key = Key::X(&name);

                match self.get_mut(key) {
                    Some(Entry::X { props, .. }) => {
                        props.push(prop);
                    }
                    Some(_) => unreachable!(),
                    None => {
                        let entry = Entry::X {
                            name,
                            props: vec![prop],
                        };

                        self.insert(entry);
                    }
                }
            }

            pub fn get(&self, key: Key<$value<S>, &S>) -> Option<&Entry<$value<S>, S>>
            where
                S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let hash = Self::hash_key(&self.1);
                let eq = Self::eq(&key);

                self.0.find(hash(&key), eq)
            }

            pub fn get_mut(&mut self, key: Key<$value<S>, &S>) -> Option<&mut Entry<$value<S>, S>>
            where
                S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let hash = Self::hash_key(&self.1);
                let eq = Self::eq(&key);

                self.0.find_mut(hash(&key), eq)
            }

            pub fn remove(&mut self, key: Key<$value<S>, &S>) -> Option<Entry<$value<S>, S>>
            where
                S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                let hash_key = Self::hash_key(&self.1);
                let hash = Self::hash_entry(&self.1);
                let eq = Self::eq(&key);
                let table_entry = self.0.entry(hash_key(&key), eq, hash);

                match table_entry {
                    TableEntry::Vacant(_) => None,
                    TableEntry::Occupied(entry) => {
                        let (entry, _) = entry.remove();
                        Some(entry)
                    }
                }
            }

            fn eq(lhs: &Key<$value<S>, &S>) -> impl Fn(&Entry<$value<S>, S>) -> bool
            where
                S: PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
            {
                move |rhs| match rhs.as_key() {
                    Key::Known(r) => matches!(lhs, Key::Known(l) if l == &r),
                    Key::Iana(r) => matches!(lhs, Key::Iana(l) if l.equiv(&r, LineFoldCaseless)),
                    Key::X(r) => matches!(lhs, Key::X(l) if l.equiv(&r, LineFoldCaseless)),
                }
            }

            fn hash_entry(hasher: &impl BuildHasher) -> impl Fn(&Entry<$value<S>, S>) -> u64
            where
                S: Hash,
            {
                let h = Self::hash_key(hasher);
                move |entry| h(&entry.as_key())
            }

            fn hash_key(
                hasher: &impl BuildHasher,
            ) -> impl Fn(&Key<$value<S>, &S>) -> u64
            where
                S: Hash,
            {
                |key| {
                    let mut hasher = hasher.build_hasher();

                    match key {
                        Key::Known(d) => d.hash(&mut hasher),
                        Key::Iana(name) => name.hash(&mut hasher),
                        Key::X(name) => name.hash(&mut hasher),
                    };

                    hasher.finish()
                }
            }
        }
    };
}

macro_rules! remove_fields {
    ($this:ident, $prop:ident, $prop_name:ident; $([let $name:ident = $field:ident]),* $(,)?) => {
        $(
            let $name =
                $this.remove(Key::Known($prop_name::$field))
                    .map(|entry| match entry {
                        Entry::Known($prop::$field(x)) => x,
                        _ => unreachable!(),
                    });
        )*
    };
}

macro_rules! mandatory_accessors {
    ($prop_type:ident, $prop_name:ident; $([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> &$ret {
                let raw_entry = self.props.get(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => prop,
                    Some(_) | None => unreachable!(),
                }
            }

            pub fn $name_mut(&mut self) -> &mut $ret {
                let raw_entry = self.props.get_mut(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => prop,
                    Some(_) | None => unreachable!(),
                }
            }
        )*
    };
}

macro_rules! optional_accessors {
    ($prop_type:ident, $prop_name:ident; $([$key:ident, $name:ident, $name_mut:ident, $ret:ty $(, $f:expr)?]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&$ret> {
                let raw_entry = self.props.get(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => Some(prop) $(.map($f))?,
                    Some(_) => unreachable!(),
                    None => None,
                }
            }

            pub fn $name_mut(&mut self) -> Option<&mut $ret> {
                let raw_entry = self.props.get_mut(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => Some(prop) $(.map($f))?,
                    Some(_) => unreachable!(),
                    None => None,
                }
            }
        )*
    };
}

macro_rules! seq_accessors {
    ($prop_type:ident, $prop_name:ident; $([$key:ident, $name:ident, $name_mut:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<&[$ret]> {
                let raw_entry = self.props.get(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => Some(prop),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }

            pub fn $name_mut(&mut self) -> Option<&mut Vec<$ret>> {
                let raw_entry = self.props.get_mut(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => Some(prop),
                    Some(_) => unreachable!(),
                    None => None,
                }
            }
        )*
    };
}

/// A sequence of [`Prop`].
type PropSeq<S, V, P = ()> = Vec<Prop<S, V, P>>;

/// A basic trait for types that have a discriminant and can return it.
pub trait Disc {
    /// The discriminant type of `Self`.
    type Discriminant;

    /// Returns the discriminant value of `self`.
    fn discriminant(&self) -> Self::Discriminant;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key<T: Disc, S> {
    Known(T::Discriminant),
    Iana(S),
    X(S),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::type_complexity)]
pub enum Entry<T, S> {
    Known(T),
    Iana {
        name: S,
        props: PropSeq<S, Box<Value<S>>, Box<[KnownParam<S>]>>,
    },
    X {
        name: S,
        props: PropSeq<S, Box<Value<S>>, Box<[KnownParam<S>]>>,
    },
}

impl<T: Disc, S> Entry<T, S> {
    fn as_key(&self) -> Key<T, &S> {
        match self {
            Entry::Known(v) => Key::Known(v.discriminant()),
            Entry::Iana { name, .. } => Key::Iana(name),
            Entry::X { name, .. } => Key::X(name),
        }
    }
}

/// An iCalendar object (RFC 5545 §3.4).
#[derive(Debug, Clone)]
pub struct Calendar<S> {
    props: CalendarTable<S>,
    components: Vec<Component<S>>,
}

/// A component in an iCalendar object (RFC 5545 §3.6).
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

impl<S> Component<S> {
    pub fn as_event(&self) -> Option<&Event<S>> {
        if let Self::Event(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_todo(&self) -> Option<&Todo<S>> {
        if let Self::Todo(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_journal(&self) -> Option<&Journal<S>> {
        if let Self::Journal(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_free_busy(&self) -> Option<&FreeBusy<S>> {
        if let Self::FreeBusy(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_time_zone(&self) -> Option<&TimeZone<S>> {
        if let Self::TimeZone(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_iana(&self) -> Option<&OtherComponent<S>> {
        if let Self::Iana(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_x(&self) -> Option<&OtherComponent<S>> {
        if let Self::X(v) = self { Some(v) } else { None }
    }
}

impl<S> From<TimeZone<S>> for Component<S> {
    fn from(v: TimeZone<S>) -> Self {
        Self::TimeZone(v)
    }
}

impl<S> From<FreeBusy<S>> for Component<S> {
    fn from(v: FreeBusy<S>) -> Self {
        Self::FreeBusy(v)
    }
}

impl<S> From<Journal<S>> for Component<S> {
    fn from(v: Journal<S>) -> Self {
        Self::Journal(v)
    }
}

impl<S> From<Todo<S>> for Component<S> {
    fn from(v: Todo<S>) -> Self {
        Self::Todo(v)
    }
}

impl<S> From<Event<S>> for Component<S> {
    fn from(v: Event<S>) -> Self {
        Self::Event(v)
    }
}

/// A VEVENT component (RFC 5545 §3.6.1).
#[derive(Debug, Clone)]
pub struct Event<S> {
    props: EventTable<S>,
    alarms: Vec<Alarm<S>>,
}

/// A VTODO component (RFC 5545 §3.6.2).
#[derive(Debug, Clone)]
pub struct Todo<S> {
    props: TodoTable<S>,
    alarms: Vec<Alarm<S>>,
}

/// A VJOURNAL component (RFC 5545 §3.6.3).
#[derive(Debug, Clone)]
pub struct Journal<S> {
    props: JournalTable<S>,
}

impl<S> Journal<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {JournalProp, JournalPropName;
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    optional_accessors! {JournalProp, JournalPropName;
        [Class, class, class_mut, Prop<S, ClassValue<S>>],
        [Created, created, created_mut, Prop<S, DateTime<Utc>>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [RecurId, recurrence_id, recurrence_id_mut, Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>],
        [Sequence, sequence, sequence_mut, Prop<S, Integer>],
        [Status, status, status_mut, Prop<S, JournalStatus>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
    }

    seq_accessors! {JournalProp, JournalPropName;
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, Box<AttachParams<S>>>],
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Categories, categories, categories_mut, Prop<S, Box<[Text<S>]>, LangParams<S>>],
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [Contact, contacts, contacts_mut, Prop<S, Text<S>, TextParams<S>>],
        [Description, descriptions, descriptions_mut, Prop<S, Box<[Text<S>]>, TextParams<S>>],
        [ExDate, exception_dates, exception_dates_mut, Prop<S, DateTimeOrDateSeq, DtParams<S>>],
        [RelatedTo, relateds, relateds_mut, Prop<S, Text<S>, RelTypeParams<S>>],
        [RDate, recurrence_dates, recurrence_dates_mut, Prop<S, RDateSeq, DtParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, Prop<S, RequestStatus<S>, LangParams<S>>],
    }
}

/// A VFREEBUSY component (RFC 5545 §3.6.4).
#[derive(Debug, Clone)]
pub struct FreeBusy<S> {
    pub(crate) props: FreeBusyTable<S>,
}

impl<S> FreeBusy<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {FreeBusyProp, FreeBusyPropName;
        [DtStamp, timestamp, timestamp_mut, Prop<S, DateTime<Utc>>],
        [Uid, uid, uid_mut, Prop<S, Uid<S>>],
    }

    optional_accessors! {FreeBusyProp, FreeBusyPropName;
        [Contact, contact, contact_mut, Prop<S, Text<S>, TextParams<S>>],
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [DtEnd, end, end_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [Organizer, organizer, organizer_mut, Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>],
        [Url, url, url_mut, Prop<S, Uri<S>>],
    }

    seq_accessors! {FreeBusyProp, FreeBusyPropName;
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Comment, comments, comments_mut, Prop<S, Text<S>, TextParams<S>>],
        [FreeBusy, free_busy_periods, free_busy_periods_mut, Prop<S, Box<[Period]>, FBTypeParams<S>>],
        [RequestStatus, request_statuses, request_statuses_mut, Prop<S, RequestStatus<S>, LangParams<S>>],
    }
}

/// A VTIMEZONE component (RFC 5545 §3.6.5).
#[derive(Debug, Clone)]
pub struct TimeZone<S> {
    pub(crate) props: TimeZoneTable<S>,
    /// The STANDARD and DAYLIGHT subcomponents. This list is guaranteed to have
    /// at least one element.
    pub(crate) subcomponents: Vec<TzRule<S>>,
}

impl<S> TimeZone<S> {
    pub fn rules(&self) -> &[TzRule<S>] {
        &self.subcomponents
    }
}

impl<S> TimeZone<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    optional_accessors! {TimeZoneProp, TimeZonePropName;
        [TzId, id, id_mut, Prop<S, TzId<S>>],
        [LastModified, last_modified, last_modified_mut, Prop<S, DateTime<Utc>>],
        [TzUrl, url, url_mut, Prop<S, Uri<S>>],
    }
}

/// A STANDARD or DAYLIGHT subcomponent of a [`TimeZone`].
#[derive(Debug, Clone)]
pub struct TzRule<S> {
    pub(crate) kind: TzRuleKind,
    pub(crate) props: OffsetTable<S>,
}

impl<S> TzRule<S> {
    pub fn kind(&self) -> TzRuleKind {
        self.kind
    }
}

impl<S> TzRule<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {OffsetProp, OffsetPropName;
        [DtStart, start, start_mut, Prop<S, DateTimeOrDate, DtParams<S>>],
        [TzOffsetTo, offset_to, offset_to_mut, Prop<S, UtcOffset>],
        [TzOffsetFrom, offset_from, offset_from_mut, Prop<S, UtcOffset>],
    }

    optional_accessors! {OffsetProp, OffsetPropName;
        [RRule, rrule, rrule_mut, Prop<S, Box<RRule>>],
    }

    seq_accessors! {OffsetProp, OffsetPropName;
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

/// A VALARM component (RFC 5545 §3.6.6).
#[derive(Debug, Clone)]
pub enum Alarm<S> {
    Audio(AudioAlarm<S>),
    Display(DisplayAlarm<S>),
    Email(EmailAlarm<S>),
    Other(OtherAlarm<S>),
}

macro_rules! dur_rep_accessors {
    ($prop_type:ident, $prop_name:ident) => {
        pub fn duration_and_repeat(
            &self,
        ) -> Option<(&Prop<S, Duration>, &Prop<S, Integer>)> {
            match self.props.get(Key::Known($prop_name::DurRep)) {
                Some(Entry::Known($prop_type::DurRep(d, r))) => Some((d, r)),
                Some(_) => unreachable!(),
                None => None,
            }
        }
    };
}

/// A VALARM with the AUDIO action.
#[derive(Debug, Clone)]
pub struct AudioAlarm<S> {
    props: AudioAlarmTable<S>,
}

impl<S> AudioAlarm<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {AudioAlarmProp, AudioAlarmPropName;
        [Action, action, action_mut, Prop<S, AudioAction>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
    }

    optional_accessors! {AudioAlarmProp, AudioAlarmPropName;
        [Attach, attachment, attachment_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
    }

    dur_rep_accessors!(AudioAlarmProp, AudioAlarmPropName);
}

/// A VALARM with the DISPLAY action.
#[derive(Debug, Clone)]
pub struct DisplayAlarm<S> {
    props: DisplayAlarmTable<S>,
}

impl<S> DisplayAlarm<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {DisplayAlarmProp, DisplayAlarmPropName;
        [Action, action, action_mut, Prop<S, DisplayAction>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
    }

    dur_rep_accessors!(DisplayAlarmProp, DisplayAlarmPropName);
}

/// A VALARM with the EMAIL action.
#[derive(Debug, Clone)]
pub struct EmailAlarm<S> {
    props: EmailAlarmTable<S>,
}

impl<S> EmailAlarm<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {EmailAlarmProp, EmailAlarmPropName;
        [Action, action, action_mut, Prop<S, EmailAction>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
    }

    seq_accessors! {EmailAlarmProp, EmailAlarmPropName;
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
    }

    dur_rep_accessors!(EmailAlarmProp, EmailAlarmPropName);
}

/// A VALARM with an action other than AUDIO, DISPLAY, or EMAIL.
#[derive(Debug, Clone)]
pub struct OtherAlarm<S> {
    props: OtherAlarmTable<S>,
}

impl<S> OtherAlarm<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    mandatory_accessors! {OtherAlarmProp, OtherAlarmPropName;
        [Action, action, action_mut, Prop<S, UnknownAction<S>>],
        [Description, description, description_mut, Prop<S, Text<S>, TextParams<S>>],
        [Trigger, trigger, trigger_mut, TriggerProp<S>],
        [Summary, summary, summary_mut, Prop<S, Text<S>, TextParams<S>>],
    }

    seq_accessors! {OtherAlarmProp, OtherAlarmPropName;
        [Attendee, attendees, attendees_mut, Prop<S, CalAddress<S>, Box<AttendeeParams<S>>>],
        [Attach, attachments, attachments_mut, Prop<S, AttachValue<S>, AttachParams<S>>],
    }

    dur_rep_accessors!(OtherAlarmProp, OtherAlarmPropName);
}

/// An arbitrary component which may have any properties and subcomponents.
#[derive(Debug, Clone)]
pub struct OtherComponent<S> {
    props: AnyPropTable<S>,
    subcomponents: Box<[Component<S>]>,
}

table! {
    #[derive(Debug, Clone)]
    CalendarTable, CalendarProp
}

enum_with_names! {
/// The potential properties of an [`Event`] together with their value and
/// parameter types.
#[derive(Debug, Clone, PartialEq, Eq)]
CalendarProp<S>,
/// The name of a variant of [`EventProp`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
CalendarPropName {
    // mandatory fields (RFC 5545)
    ProdId(Prop<S, Text<S>>),
    Version(Prop<S, ()>),

    // optional fields (RFC 5545)
    CalScale(Prop<S, ()>),
    Method(Prop<S, Method<S>>),

    // optional fields (RFC 7986)
    Uid(Prop<S, Uid<S>>),
    LastModified(Prop<S, DateTime<Utc>>),
    Url(Prop<S, Uri<S>>),
    RefreshInterval(Prop<S, Duration>),
    Source(Prop<S, Uri<S>>),
    Color(Prop<S, Css3Color>),

    // free multiplicity fields (RFC 7986)
    Name(PropSeq<Text<S>, TextParams<S>>),
    Description(PropSeq<Text<S>, TextParams<S>>),
    Categories(PropSeq<Box<[Text<S>]>, LangParams<S>>),
    Image(PropSeq<ImageData<S>, ImageParams<S>>)
}}

table! {
    #[derive(Debug, Clone)]
    EventTable, EventProp
}

enum_with_names! {
/// The potential properties of an [`Event`] together with their value and
/// parameter types.
#[derive(Debug, Clone, PartialEq, Eq)]
EventProp<S>,
/// The name of a variant of [`EventProp`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
EventPropName {
    // mandatory fields
    DtStamp(Prop<S, DateTime<Utc>>),
    Uid(Prop<S, Uid<S>>),

    // optional fields
    DtStart(Prop<S, DateTimeOrDate, DtParams<S>>), // required if METHOD is absent
    Class(Prop<S, ClassValue<S>>),
    Created(Prop<S, DateTime<Utc>>),
    Description(Prop<S, Text<S>, TextParams<S>>),
    Geo(Prop<S, Geo>),
    LastModified(Prop<S, DateTime<Utc>>),
    Location(Prop<S, Text<S>, TextParams<S>>),
    Organizer(Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    Priority(Prop<S, Priority>),
    Sequence(Prop<S, Integer>),
    Status(Prop<S, EventStatus>),
    Summary(Prop<S, Text<S>, TextParams<S>>),
    Transp(Prop<S, TimeTransparency>),
    Url(Prop<S, Uri<S>>),
    RecurId(Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    RRule(Prop<S, Box<RRule>>), // SHOULD NOT occur more than once
    Color(Prop<S, Css3Color>), // RFC 7986 §4

    // either DTEND or DURATION, since they are mutually exclusive
    Termination(EventTerminationProp<S>),

    // free multiplicity fields
    Attach(PropSeq<S, AttachValue<S>, Box<AttachParams<S>>>),
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Categories(PropSeq<S, Box<[Text<S>]>, LangParams<S>>),
    Comment(PropSeq<S, Text<S>, TextParams<S>>),
    Contact(PropSeq<S, Text<S>, TextParams<S>>),
    ExDate(PropSeq<S, DateTimeOrDateSeq, DtParams<S>>),
    RequestStatus(PropSeq<S, RequestStatus<S>, LangParams<S>>),
    RelatedTo(PropSeq<S, Text<S>, RelTypeParams<S>>),
    Resources(PropSeq<S, Box<[Text<S>]>, TextParams<S>>),
    RDate(PropSeq<S, RDateSeq, DtParams<S>>),
    Conference(PropSeq<S, Uri<S>, ConfParams<S>>), // RFC 7986 §4
    Image(PropSeq<S, ImageData<S>, ImageParams<S>>) // RFC 7986 §4
}}

table! {
    #[derive(Debug, Clone)]
    TodoTable, TodoProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
TodoProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
TodoPropName {
    // mandatory fields
    DtStamp(Prop<S, DateTime<Utc>>),
    Uid(Prop<S, Uid<S>>),

    // optional fields
    Class(Prop<S, ClassValue<S>>),
    Completed(Prop<S, DateTime<Utc>>),
    Created(Prop<S, DateTime<Utc>>),
    Description(Prop<S, Text<S>, TextParams<S>>),
    DtStart(Prop<S, DateTimeOrDate, DtParams<S>>),
    Geo(Prop<S, Geo>),
    LastModified(Prop<S, DateTime<Utc>>),
    Location(Prop<S, Text<S>, TextParams<S>>),
    Organizer(Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    Percent(Prop<S, CompletionPercentage>),
    Priority(Prop<S, Priority>),
    RecurId(Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    Sequence(Prop<S, Integer>),
    Status(Prop<S, TodoStatus>),
    Summary(Prop<S, Text<S>, TextParams<S>>),
    Url(Prop<S, Uri<S>>),
    RRule(Prop<S, Box<RRule>>), // SHOULD NOT occur more than once
    Color(Prop<S, Css3Color>), // RFC 7986 §4

    // either DTDUE or DURATION, since they are mutually exclusive
    Termination(TodoTerminationProp<S>),

    // free multiplicity fields
    Attach(PropSeq<S, AttachValue<S>, Box<AttachParams<S>>>),
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Categories(PropSeq<S, Box<[Text<S>]>, LangParams<S>>),
    Comment(PropSeq<S, Text<S>, TextParams<S>>),
    Contact(PropSeq<S, Text<S>, TextParams<S>>),
    ExDate(PropSeq<S, DateTimeOrDateSeq, DtParams<S>>),
    RequestStatus(PropSeq<S, RequestStatus<S>, LangParams<S>>),
    RelatedTo(PropSeq<S, Text<S>, RelTypeParams<S>>),
    Resources(PropSeq<S, Box<[Text<S>]>, TextParams<S>>),
    RDate(PropSeq<S, RDateSeq, DtParams<S>>),
    Conference(PropSeq<S, Uri<S>, ConfParams<S>>), // RFC 7986 §4
    Image(PropSeq<S, ImageData<S>, ImageParams<S>>) // RFC 7986 §4
}}

table! {
    #[derive(Debug, Clone)]
    JournalTable, JournalProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
JournalProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
JournalPropName {
    // mandatory fields
    DtStamp(Prop<S, DateTime<Utc>>),
    Uid(Prop<S, Uid<S>>),

    // optional fields
    Class(Prop<S, ClassValue<S>>),
    Created(Prop<S, DateTime<Utc>>),
    DtStart(Prop<S, DateTimeOrDate, DtParams<S>>),
    LastModified(Prop<S, DateTime<Utc>>),
    Organizer(Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    RecurId(Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    Sequence(Prop<S, Integer>),
    Status(Prop<S, JournalStatus>),
    Summary(Prop<S, Text<S>, TextParams<S>>),
    Url(Prop<S, Uri<S>>),
    RRule(Prop<S, Box<RRule>>), // SHOULD NOT occur more than once
    Color(Prop<S, Css3Color>), // RFC 7986 §4

    // free multiplicity fields
    Attach(PropSeq<S, AttachValue<S>, Box<AttachParams<S>>>),
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Categories(PropSeq<S, Box<[Text<S>]>, LangParams<S>>),
    Comment(PropSeq<S, Text<S>, TextParams<S>>),
    Contact(PropSeq<S, Text<S>, TextParams<S>>),
    Description(PropSeq<S, Box<[Text<S>]>, TextParams<S>>),
    ExDate(PropSeq<S, DateTimeOrDateSeq, DtParams<S>>),
    RequestStatus(PropSeq<S, RequestStatus<S>, LangParams<S>>),
    RelatedTo(PropSeq<S, Text<S>, RelTypeParams<S>>),
    RDate(PropSeq<S, RDateSeq, DtParams<S>>),
    Image(PropSeq<S, ImageData<S>, ImageParams<S>>) // RFC 7986 §4
}}

table! {
    #[derive(Debug, Clone)]
    FreeBusyTable, FreeBusyProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
FreeBusyProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
FreeBusyPropName {
    // mandatory fields
    DtStamp(Prop<S, DateTime<Utc>>),
    Uid(Prop<S, Uid<S>>),

    // optional fields
    Contact(Prop<S, Text<S>, TextParams<S>>),
    DtStart(Prop<S, DateTimeOrDate, DtParams<S>>),
    DtEnd(Prop<S, DateTimeOrDate, DtParams<S>>),
    Organizer(Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    Url(Prop<S, Uri<S>>),

    // free multiplicity fields
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Comment(PropSeq<S, Text<S>, TextParams<S>>),
    FreeBusy(PropSeq<S, Box<[Period]>, FBTypeParams<S>>),
    RequestStatus(PropSeq<S, RequestStatus<S>, LangParams<S>>)
}}

table! {
    #[derive(Debug, Clone)]
    TimeZoneTable, TimeZoneProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
TimeZoneProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
TimeZonePropName {
    // mandatory fields
    TzId(Prop<S, TzId<S>>),

    // optional fields
    LastModified(Prop<S, DateTime<Utc>>),
    TzUrl(Prop<S, Uri<S>>)
}}

// NOTE: since the contents of the STANDARD and DAYLIGHT components are the
// same, we invent the OffsetTable type to represent this shared form

table! {
    #[derive(Debug, Clone)]
    OffsetTable, OffsetProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
OffsetProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
OffsetPropName {
    // mandatory fields
    DtStart(Prop<S, DateTimeOrDate, DtParams<S>>),
    TzOffsetTo(Prop<S, UtcOffset>),
    TzOffsetFrom(Prop<S, UtcOffset>),

    // optional fields
    RRule(Prop<S, Box<RRule>>), // SHOULD NOT occur more than once

    // free multiplicity fields
    Comment(PropSeq<S, Text<S>, TextParams<S>>),
    RDate(PropSeq<S, RDateSeq, DtParams<S>>),
    TzName(PropSeq<S, Text<S>, LangParams<S>>)
}}

table! {
    #[derive(Debug, Clone)]
    AudioAlarmTable, AudioAlarmProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
AudioAlarmProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
AudioAlarmPropName {
    // mandatory fields
    Action(Prop<S, AudioAction>),
    Trigger(TriggerProp<S>),

    // optional fields
    DurRep(Prop<S, Duration>, Prop<S, Integer>), // the product of DURATION and REPEAT
    Attach(Prop<S, AttachValue<S>, AttachParams<S>>)
}}

table! {
    #[derive(Debug, Clone)]
    DisplayAlarmTable, DisplayAlarmProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
DisplayAlarmProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
DisplayAlarmPropName {
    // mandatory fields
    Action(Prop<S, DisplayAction>),
    Description(Prop<S, Text<S>, TextParams<S>>),
    Trigger(TriggerProp<S>),

    // optional fields
    DurRep(Prop<S, Duration>, Prop<S, Integer>) // the product of DURATION and REPEAT
}}

table! {
    #[derive(Debug, Clone)]
    EmailAlarmTable, EmailAlarmProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
EmailAlarmProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
EmailAlarmPropName {
    // mandatory fields
    Action(Prop<S, EmailAction>),
    Description(Prop<S, Text<S>, TextParams<S>>),
    Trigger(TriggerProp<S>),
    Summary(Prop<S, Text<S>, TextParams<S>>),

    // optional fields
    DurRep(Prop<S, Duration>, Prop<S, Integer>), // the product of DURATION and REPEAT

    // free multiplicity fields
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Attach(PropSeq<S, AttachValue<S>, AttachParams<S>>)
}}

table! {
    #[derive(Debug, Clone)]
    OtherAlarmTable, OtherAlarmProp
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
OtherAlarmProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
OtherAlarmPropName {
    // mandatory fields
    Action(Prop<S, UnknownAction<S>>),
    Trigger(TriggerProp<S>),

    // optional fields
    Description(Prop<S, Text<S>, TextParams<S>>),
    DurRep(Prop<S, Duration>, Prop<S, Integer>), // the product of DURATION and REPEAT
    Summary(Prop<S, Text<S>, TextParams<S>>),

    // free multiplicity fields
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Attach(PropSeq<S, AttachValue<S>, AttachParams<S>>)
}}

// NOTE: the FreeAlarm* types exist exclusively to manage state during
// parsing; they don't immediately correspond to anything in the iCalendar
// model. after parsing a VALARM, the result is a FreeAlarmTable that can
// be converted into an Alarm. this process is fallible, as the acceptable
// fields depend on the ACTION.

table! {
    #[derive(Debug, Clone)]
    FreeAlarmTable, FreeAlarmProp
}

impl<S> FreeAlarmTable<S>
where
    S: Hash + PartialEq + Equiv<LineFoldCaseless> + AsRef<[u8]>,
{
    pub fn try_into_alarm(mut self) -> Result<Alarm<S>, CalendarParseError<S>> {
        remove_fields! {self, FreeAlarmProp, FreeAlarmPropName;
            [let action      =      Action],
            [let trigger     =     Trigger],
            [let description = Description],
            [let duration    =    Duration],
            [let repeat      =      Repeat],
            [let summary     =     Summary],
            [let attendees   =    Attendee],
            [let attachments =      Attach],
        }

        // check mandatory fields
        let Some(action) = action else {
            return Err(CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::Action),
                component: ComponentKind::Alarm,
            });
        };

        let Some(trigger) = trigger else {
            return Err(CalendarParseError::MissingProp {
                prop: PropName::Rfc5545(Rfc5545PropName::Trigger),
                component: ComponentKind::Alarm,
            });
        };

        // check duration and repeat occur together or not at all
        let dur_rep = match (duration, repeat) {
            (Some(d), Some(r)) => Some((d, r)),
            (None, None) => None,
            (Some(_), None) => {
                return Err(CalendarParseError::DurationWithoutRepeat);
            }
            (None, Some(_)) => {
                return Err(CalendarParseError::RepeatWithoutDuration);
            }
        };

        match action.value {
            AlarmAction::Audio => {
                let mut props = AudioAlarmTable::new();

                // insert action
                props.insert(Entry::Known(AudioAlarmProp::Action(Prop {
                    value: AudioAction,
                    params: (),
                    unknown_params: action.unknown_params,
                })));

                // insert trigger
                props.insert(Entry::Known(AudioAlarmProp::Trigger(trigger)));

                // insert optional fields
                if let Some((d, r)) = dur_rep {
                    props.insert(Entry::Known(AudioAlarmProp::DurRep(d, r)));
                }

                // insert attachment if single
                if let Some(mut attachments) = attachments {
                    if attachments.len() == 1 {
                        let p = attachments.remove(0);
                        props.insert(Entry::Known(AudioAlarmProp::Attach(p)));
                    } else if attachments.len() > 1 {
                        return Err(
                            CalendarParseError::TooManyAttachmentsOnAudioAlarm,
                        );
                    }
                }

                // check for invalid fields
                if description.is_some() {
                    return Err(CalendarParseError::UnexpectedProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Description),
                        component: ComponentKind::AudioAlarm,
                    });
                }

                if summary.is_some() {
                    return Err(CalendarParseError::UnexpectedProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Summary),
                        component: ComponentKind::AudioAlarm,
                    });
                }

                if attendees.is_some() {
                    return Err(CalendarParseError::UnexpectedProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
                        component: ComponentKind::AudioAlarm,
                    });
                }

                Ok(Alarm::Audio(AudioAlarm { props }))
            }
            AlarmAction::Display => {
                let mut props = DisplayAlarmTable::new();

                // check for mandatory description
                let Some(description) = description else {
                    return Err(CalendarParseError::MissingProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Description),
                        component: ComponentKind::DisplayAlarm,
                    });
                };

                // insert action
                props.insert(Entry::Known(DisplayAlarmProp::Action(Prop {
                    value: DisplayAction,
                    params: (),
                    unknown_params: action.unknown_params,
                })));

                // insert description
                props.insert(Entry::Known(DisplayAlarmProp::Description(
                    description,
                )));

                // insert trigger
                props.insert(Entry::Known(DisplayAlarmProp::Trigger(trigger)));

                // insert optional fields
                if let Some((d, r)) = dur_rep {
                    props.insert(Entry::Known(DisplayAlarmProp::DurRep(d, r)));
                }

                if summary.is_some() {
                    return Err(CalendarParseError::UnexpectedProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Summary),
                        component: ComponentKind::DisplayAlarm,
                    });
                }

                if attendees.is_some() {
                    return Err(CalendarParseError::UnexpectedProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
                        component: ComponentKind::DisplayAlarm,
                    });
                }

                if attachments.is_some() {
                    return Err(CalendarParseError::UnexpectedProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Attachment),
                        component: ComponentKind::DisplayAlarm,
                    });
                }

                Ok(Alarm::Display(DisplayAlarm { props }))
            }
            AlarmAction::Email => {
                let mut props = EmailAlarmTable::new();

                // check for mandatory description
                let Some(description) = description else {
                    return Err(CalendarParseError::MissingProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Description),
                        component: ComponentKind::EmailAlarm,
                    });
                };

                // check for mandatory summary
                let Some(summary) = summary else {
                    return Err(CalendarParseError::MissingProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Summary),
                        component: ComponentKind::EmailAlarm,
                    });
                };

                // check for mandatory attendees
                let Some(attendees) = attendees else {
                    return Err(CalendarParseError::MissingProp {
                        prop: PropName::Rfc5545(Rfc5545PropName::Attendee),
                        component: ComponentKind::EmailAlarm,
                    });
                };

                // insert mandatory fields
                props.insert(Entry::Known(EmailAlarmProp::Action(Prop {
                    value: EmailAction,
                    params: (),
                    unknown_params: action.unknown_params,
                })));
                props.insert(Entry::Known(EmailAlarmProp::Description(
                    description,
                )));
                props.insert(Entry::Known(EmailAlarmProp::Trigger(trigger)));
                props.insert(Entry::Known(EmailAlarmProp::Summary(summary)));
                props.insert(Entry::Known(EmailAlarmProp::Attendee(attendees)));

                // insert optional fields
                if let Some((d, r)) = dur_rep {
                    props.insert(Entry::Known(EmailAlarmProp::DurRep(d, r)));
                }

                if let Some(p) = attachments {
                    props.insert(Entry::Known(EmailAlarmProp::Attach(p)));
                }

                Ok(Alarm::Email(EmailAlarm { props }))
            }
            AlarmAction::Iana(name) => {
                let mut props = OtherAlarmTable::new();

                // insert action
                props.insert(Entry::Known(OtherAlarmProp::Action(Prop {
                    value: UnknownAction::Iana(name),
                    params: (),
                    unknown_params: action.unknown_params,
                })));

                // insert trigger
                props.insert(Entry::Known(OtherAlarmProp::Trigger(trigger)));

                // insert optional fields
                if let Some(p) = description {
                    props.insert(Entry::Known(OtherAlarmProp::Description(p)));
                }

                if let Some((d, r)) = dur_rep {
                    props.insert(Entry::Known(OtherAlarmProp::DurRep(d, r)));
                }

                if let Some(p) = summary {
                    props.insert(Entry::Known(OtherAlarmProp::Summary(p)));
                }

                if let Some(p) = attendees {
                    props.insert(Entry::Known(OtherAlarmProp::Attendee(p)));
                }

                if let Some(p) = attachments {
                    props.insert(Entry::Known(OtherAlarmProp::Attach(p)));
                }

                Ok(Alarm::Other(OtherAlarm { props }))
            }
            AlarmAction::X(name) => {
                let mut props = OtherAlarmTable::new();

                // insert action
                props.insert(Entry::Known(OtherAlarmProp::Action(Prop {
                    value: UnknownAction::X(name),
                    params: (),
                    unknown_params: action.unknown_params,
                })));

                // insert trigger
                props.insert(Entry::Known(OtherAlarmProp::Trigger(trigger)));

                // insert optional fields
                if let Some(p) = description {
                    props.insert(Entry::Known(OtherAlarmProp::Description(p)));
                }

                if let Some((d, r)) = dur_rep {
                    props.insert(Entry::Known(OtherAlarmProp::DurRep(d, r)));
                }

                if let Some(p) = summary {
                    props.insert(Entry::Known(OtherAlarmProp::Summary(p)));
                }

                if let Some(p) = attendees {
                    props.insert(Entry::Known(OtherAlarmProp::Attendee(p)));
                }

                if let Some(p) = attachments {
                    props.insert(Entry::Known(OtherAlarmProp::Attach(p)));
                }

                Ok(Alarm::Other(OtherAlarm { props }))
            }
        }
    }
}

enum_with_names! {
#[derive(Debug, Clone, PartialEq, Eq)]
FreeAlarmProp<S>,
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
FreeAlarmPropName {
    // mandatory fields
    Action(Prop<S, AlarmAction<S>>),
    Trigger(TriggerProp<S>),

    // optional fields
    Description(Prop<S, Text<S>, TextParams<S>>),
    Duration(Prop<S, Duration>),
    Repeat(Prop<S, Integer>),
    Summary(Prop<S, Text<S>, TextParams<S>>),

    // free multiplicity fields
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Attach(PropSeq<S, AttachValue<S>, AttachParams<S>>)
}}

// NOTE: the following is a catch-all type for the properties on iana and
// x-name components. we assume that every property may occur with any
// multiplicity

table! {
    #[derive(Debug, Clone)]
    AnyPropTable, AnyProp
}

enum_with_names! {
/// The potential properties of an [`Event`] together with their value and
/// parameter types.
#[derive(Debug, Clone, PartialEq, Eq)]
AnyProp<S>,
/// The name of a variant of [`EventProp`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
AnyPropName {
    // CALENDAR PROPERTIES
    CalScale(PropSeq<S, ()>),
    Method(PropSeq<S, Method<S>>),
    ProdId(PropSeq<S, Text<S>>),
    Version(PropSeq<S, ()>),
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach(PropSeq<S, AttachValue<S>, Box<AttachParams<S>>>),
    Categories(PropSeq<S, Box<[Text<S>]>, LangParams<S>>),
    Class(PropSeq<S, ClassValue<S>>),
    Comment(PropSeq<S, Text<S>, TextParams<S>>),
    Description(PropSeq<S, Text<S>, TextParams<S>>),
    Geo(PropSeq<S, Geo>),
    Location(PropSeq<S, Text<S>, TextParams<S>>),
    PercentComplete(PropSeq<S, CompletionPercentage>),
    Priority(PropSeq<S, Priority>),
    Resources(PropSeq<S, Box<[Text<S>]>, TextParams<S>>),
    Status(PropSeq<S, Status>),
    Summary(PropSeq<S, Text<S>, TextParams<S>>),
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted(PropSeq<S, DateTime<Utc>>),
    DtEnd(PropSeq<S, DateTimeOrDate, DtParams<S>>),
    DtDue(PropSeq<S, DateTimeOrDate, DtParams<S>>),
    DtStart(PropSeq<S, DateTimeOrDate, DtParams<S>>),
    Duration(PropSeq<S, Duration>),
    FreeBusy(PropSeq<S, Box<[Period]>, FBTypeParams<S>>),
    Transp(PropSeq<S, TimeTransparency>),
    // TIME ZONE COMPONENT PROPERTIES
    TzId(PropSeq<S, TzId<S>>),
    TzName(PropSeq<S, Text<S>, LangParams<S>>),
    TzOffsetFrom(PropSeq<S, UtcOffset>),
    TzOffsetTo(PropSeq<S, UtcOffset>),
    TzUrl(PropSeq<S, Uri<S>>),
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee(PropSeq<S, CalAddress<S>, Box<AttendeeParams<S>>>),
    Contact(PropSeq<S, Text<S>, TextParams<S>>),
    Organizer(PropSeq<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    RecurId(PropSeq<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    RelatedTo(PropSeq<S, Text<S>, RelTypeParams<S>>),
    Url(PropSeq<S, Uri<S>>),
    Uid(PropSeq<S, Uid<S>>),
    // RECURRENCE COMPONENT PROPERTIES
    ExDate(PropSeq<S, DateTimeOrDateSeq, DtParams<S>>),
    RDate(PropSeq<S, RDateSeq, DtParams<S>>),
    RRule(PropSeq<S, Box<RRule>>),
    // ALARM COMPONENT PROPERTIES
    Action(PropSeq<S, AlarmAction<S>>),
    Repeat(PropSeq<S, Integer>),
    Trigger(Box<[TriggerProp<S>]>),
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created(PropSeq<S, DateTime<Utc>>),
    DtStamp(PropSeq<S, DateTime<Utc>>),
    LastModified(PropSeq<S, DateTime<Utc>>),
    Sequence(PropSeq<S, Integer>),
    // MISCELLANEOUS COMPONENT PROPERTIES
    RequestStatus(PropSeq<S, RequestStatus<S>, LangParams<S>>),
    // RFC 7986 PROPERTIES
    Name(PropSeq<S, Text<S>, TextParams<S>>),
    RefreshInterval(PropSeq<S, Duration>),
    Source(PropSeq<S, Uri<S>>),
    Color(PropSeq<S, Css3Color>),
    Image(PropSeq<S, ImageData<S>, ImageParams<S>>),
    Conference(PropSeq<S, Uri<S>, ConfParams<S>>)
}}

#[cfg(test)]
mod tests {
    use crate::{date, time};

    use super::*;

    #[test]
    fn basic_event_table_usage() {
        let mut event = EventTable::new();

        let uid = EventProp::Uid(Prop::from_value(Uid("some-identifier")));
        let dtstamp = EventProp::DtStamp(Prop::from_value(DateTime {
            date: date!(1997;12;24),
            time: time!(15;20;12, Utc),
        }));

        let prev = event.insert(Entry::Known(uid.clone()));
        assert!(prev.is_none());
        let prev = event.insert(Entry::Known(dtstamp.clone()));
        assert!(prev.is_none());
        assert_eq!(event.0.len(), 2);

        let uid_ref = event.get(Key::Known(EventPropName::Uid));
        let dtstamp_ref = event.get(Key::Known(EventPropName::DtStamp));

        assert_eq!(Some(&Entry::Known(uid)), uid_ref);
        assert_eq!(Some(&Entry::Known(dtstamp)), dtstamp_ref);
    }
}
