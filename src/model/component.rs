//! Model types for calendar components.

use std::hash::{BuildHasher, Hash, Hasher, RandomState};

use hashbrown::{HashTable, hash_table::Entry as TableEntry};

use crate::parser::escaped::{Equiv, LineFoldCaseless};

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

macro_rules! mandatory_accessors {
    ($prop_type:ident, $prop_name:ident; $([$key:ident, $name:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> $ret {
                let raw_entry = self.props.get(Key::Known($prop_name::$key));

                match raw_entry {
                    Some(Entry::Known($prop_type::$key(prop))) => prop,
                    Some(_) | None => unreachable!(),
                }
            }
        )*
    };
}

macro_rules! optional_accessors {
    ($prop_type:ident, $prop_name:ident; $([$key:ident, $name:ident, $ret:ty]),* $(,)?) => {
        $(
            pub fn $name(&self) -> Option<$ret> {
                let raw_entry = self.props.get(Key::Known($prop_name::$key));

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

/// A VFREEBUSY component (RFC 5545 §3.6.4).
#[derive(Debug, Clone)]
pub struct FreeBusy<S> {
    props: FreeBusyTable<S>,
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
        [TzId, id, &Prop<S, TzId<S>>],
        [LastModified, last_modified, &Prop<S, DateTime<Utc>>],
        [TzUrl, url, &Prop<S, Uri<S>>],
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
        [DtStart, start, &Prop<S, DateTimeOrDate, DtParams<S>>],
        [TzOffsetTo, offset_to, &Prop<S, UtcOffset>],
        [TzOffsetFrom, offset_from, &Prop<S, UtcOffset>],
    }

    optional_accessors! {OffsetProp, OffsetPropName;
        [RRule, rrule, &Prop<S, Box<RRule>>],
        [Comment, comments, &[Prop<S, Text<S>, TextParams<S>>]],
        [RDate, recurrence_dates, &[Prop<S, RDateSeq, DtParams<S>>]],
        [TzName, names, &[Prop<S, Text<S>, LangParams<S>>]],
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
    Iana(OtherAlarm<S>),
    X(OtherAlarm<S>),
}

/// A VALARM with the AUDIO action.
#[derive(Debug, Clone)]
pub struct AudioAlarm<S> {
    props: AudioAlarmTable<S>,
}

/// A VALARM with the DISPLAY action.
#[derive(Debug, Clone)]
pub struct DisplayAlarm<S> {
    props: DisplayAlarmTable<S>,
}

/// A VALARM with the EMAIL action.
#[derive(Debug, Clone)]
pub struct EmailAlarm<S> {
    props: EmailAlarmTable<S>,
}

/// A VALARM with an action other than AUDIO, DISPLAY, or EMAIL.
#[derive(Debug, Clone)]
pub struct OtherAlarm<S> {
    props: OtherAlarmTable<S>,
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
    Action(Prop<S, S>),
    Description(Prop<S, Text<S>, TextParams<S>>),
    Trigger(TriggerProp<S>),
    Summary(Prop<S, Text<S>, TextParams<S>>),

    // optional fields
    DurRep(Prop<S, Duration>, Prop<S, Integer>), // the product of DURATION and REPEAT

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
