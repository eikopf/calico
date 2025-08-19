//! Model types for calendar components.

use std::hash::{BuildHasher, Hash, Hasher, RandomState};

use hashbrown::{HashTable, hash_table::Entry as TableEntry};

use super::{
    parameter::KnownParam,
    primitive::{
        AttachValue, AudioAction, CalAddress, ClassValue, CompletionPercentage,
        DateTime, DateTimeOrDate, DateTimeOrDateSeq, DisplayAction, Duration,
        EmailAction, EventStatus, Geo, Integer, JournalStatus, Period,
        Priority, RDateSeq, RequestStatus, Text, TimeTransparency, TodoStatus,
        TzId, Uid, Uri, Utc, UtcOffset, Value,
    },
    property::{
        AttachParams, AttendeeParams, DtParams, EventTerminationProp,
        FBTypeParams, LangParams, OrganizerParams, Prop, RecurrenceIdParams,
        RelTypeParams, TextParams, TodoTerminationProp, TriggerProp,
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
        enum $enum_name $(<$($t),+>)? {
            $($variant ($($field),*)),*
        }

        $(#[ $m2 ])*
        enum $names_name {
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
        struct $name <S> (HashTable<Entry<$value<S>, S>>, RandomState);

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
                S: Hash + PartialEq,
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

            pub fn get(&self, key: Key<$value<S>, &S>) -> Option<&Entry<$value<S>, S>>
            where
                S: Hash + PartialEq,
            {
                let hash = Self::hash_key(&self.1);
                let eq = Self::eq(&key);

                self.0.find(hash(&key), eq)
            }

            fn eq(lhs: &Key<$value<S>, &S>) -> impl Fn(&Entry<$value<S>, S>) -> bool
            where
                S: PartialEq,
            {
                move |rhs| match rhs.as_key() {
                    Key::Known(r) => matches!(lhs, Key::Known(l) if l == &r),
                    Key::Iana(r) => matches!(lhs, Key::Iana(l) if l == &r),
                    Key::X(r) => matches!(lhs, Key::X(l) if l == &r),
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

/// A sequence of [`Prop`].
type PropSeq<V, P = ()> = Box<[Prop<V, P>]>;

/// A basic trait for types that have a discriminant and can return it.
trait Disc {
    /// The discriminant type of `Self`.
    type Discriminant;

    /// Returns the discriminant value of `self`.
    fn discriminant(&self) -> Self::Discriminant;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Key<T: Disc, S> {
    Known(T::Discriminant),
    Iana(S),
    X(S),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Entry<T, S> {
    Known(T),
    Iana {
        name: S,
        prop: Prop<Box<Value<S>>, Box<[KnownParam<S>]>>,
    },
    X {
        name: S,
        prop: Prop<Box<Value<S>>, Box<[KnownParam<S>]>>,
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

/// A VEVENT component (RFC 5545 §3.6.1).
#[derive(Debug, Clone)]
pub struct Event<S> {
    props: EventTable<S>,
    alarms: Box<[Alarm<S>]>,
}

/// A VTODO component (RFC 5545 §3.6.2).
#[derive(Debug, Clone)]
pub struct Todo<S> {
    props: TodoTable<S>,
    alarms: Box<[Alarm<S>]>,
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
    props: TimeZoneTable<S>,
    /// The STANDARD and DAYLIGHT subcomponents. This list is guaranteed to have
    /// at least one element.
    subcomponents: Box<[TzRule<S>]>,
}

/// A STANDARD or DAYLIGHT subcomponent of a [`TimeZone`].
#[derive(Debug, Clone)]
pub struct TzRule<S> {
    props: OffsetTable<S>,
    kind: TzRuleKind,
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
    DtStamp(Prop<DateTime<Utc>>),
    Uid(Prop<Uid<S>>),

    // optional fields
    DtStart(Prop<DateTimeOrDate, DtParams<S>>), // required if METHOD is absent
    Class(Prop<ClassValue<S>>),
    Created(Prop<DateTime<Utc>>),
    Description(Prop<Text<S>, TextParams<S>>),
    Geo(Prop<Geo>),
    LastModified(Prop<DateTime<Utc>>),
    Location(Prop<Text<S>, TextParams<S>>),
    Organizer(Prop<CalAddress<S>, Box<OrganizerParams<S>>>),
    Priority(Prop<Priority>),
    Sequence(Prop<Integer>),
    Status(Prop<EventStatus>),
    Summary(Prop<Text<S>, TextParams<S>>),
    Transp(Prop<TimeTransparency>),
    Url(Prop<Uri<S>>),
    RecurId(Prop<DateTimeOrDate, RecurrenceIdParams<S>>),
    RRule(Prop<Box<RRule>>), // SHOULD NOT occur more than once

    // either DTEND or DURATION, since they are mutually exclusive
    Termination(EventTerminationProp<S>),

    // free multiplicity fields
    Attach(PropSeq<AttachValue<S>, Box<AttachParams<S>>>),
    Attendee(PropSeq<CalAddress<S>, Box<AttendeeParams<S>>>),
    Categories(PropSeq<Box<[Text<S>]>, LangParams<S>>),
    Comment(PropSeq<Text<S>, TextParams<S>>),
    Contact(PropSeq<Text<S>, TextParams<S>>),
    ExDate(PropSeq<DateTimeOrDateSeq, DtParams<S>>),
    RequestStatus(PropSeq<RequestStatus<S>, LangParams<S>>),
    RelatedTo(PropSeq<Text<S>, RelTypeParams<S>>),
    Resources(PropSeq<Box<[Text<S>]>, TextParams<S>>),
    RDate(PropSeq<RDateSeq, DtParams<S>>)
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
    DtStamp(Prop<DateTime<Utc>>),
    Uid(Prop<Uid<S>>),

    // optional fields
    Class(Prop<ClassValue<S>>),
    Completed(Prop<DateTime<Utc>>),
    Created(Prop<DateTime<Utc>>),
    Description(Prop<Text<S>, TextParams<S>>),
    DtStart(Prop<DateTimeOrDate, DtParams<S>>),
    Geo(Prop<Geo>),
    LastModified(Prop<DateTime<Utc>>),
    Location(Prop<Text<S>, TextParams<S>>),
    Organizer(Prop<CalAddress<S>, Box<OrganizerParams<S>>>),
    Percent(Prop<CompletionPercentage>),
    Priority(Prop<Priority>),
    RecurId(Prop<DateTimeOrDate, RecurrenceIdParams<S>>),
    Sequence(Prop<Integer>),
    Status(Prop<TodoStatus>),
    Summary(Prop<Text<S>, TextParams<S>>),
    Url(Prop<Uri<S>>),
    RRule(Prop<Box<RRule>>), // SHOULD NOT occur more than once

    // either DTDUE or DURATION, since they are mutually exclusive
    Termination(TodoTerminationProp<S>),

    // free multiplicity fields
    Attach(PropSeq<AttachValue<S>, Box<AttachParams<S>>>),
    Attendee(PropSeq<CalAddress<S>, Box<AttendeeParams<S>>>),
    Categories(PropSeq<Box<[Text<S>]>, LangParams<S>>),
    Comment(PropSeq<Text<S>, TextParams<S>>),
    Contact(PropSeq<Text<S>, TextParams<S>>),
    ExDate(PropSeq<DateTimeOrDateSeq, DtParams<S>>),
    RequestStatus(PropSeq<RequestStatus<S>, LangParams<S>>),
    RelatedTo(PropSeq<Text<S>, RelTypeParams<S>>),
    Resources(PropSeq<Box<[Text<S>]>, TextParams<S>>),
    RDate(PropSeq<RDateSeq, DtParams<S>>)
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
    DtStamp(Prop<DateTime<Utc>>),
    Uid(Prop<Uid<S>>),

    // optional fields
    Class(Prop<ClassValue<S>>),
    Created(Prop<DateTime<Utc>>),
    DtStart(Prop<DateTimeOrDate, DtParams<S>>),
    LastModified(Prop<DateTime<Utc>>),
    Organizer(Prop<CalAddress<S>, Box<OrganizerParams<S>>>),
    RecurId(Prop<DateTimeOrDate, RecurrenceIdParams<S>>),
    Sequence(Prop<Integer>),
    Status(Prop<JournalStatus>),
    Summary(Prop<Text<S>, TextParams<S>>),
    Url(Prop<Uri<S>>),
    RRule(Prop<Box<RRule>>), // SHOULD NOT occur more than once

    // free multiplicity fields
    Attach(PropSeq<AttachValue<S>, Box<AttachParams<S>>>),
    Attendee(PropSeq<CalAddress<S>, Box<AttendeeParams<S>>>),
    Categories(PropSeq<Box<[Text<S>]>, LangParams<S>>),
    Comment(PropSeq<Text<S>, TextParams<S>>),
    Contact(PropSeq<Text<S>, TextParams<S>>),
    Description(PropSeq<Box<[Text<S>]>, TextParams<S>>),
    ExDate(PropSeq<DateTimeOrDateSeq, DtParams<S>>),
    RequestStatus(PropSeq<RequestStatus<S>, LangParams<S>>),
    RelatedTo(PropSeq<Text<S>, RelTypeParams<S>>),
    RDate(PropSeq<RDateSeq, DtParams<S>>)
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
    DtStamp(Prop<DateTime<Utc>>),
    Uid(Prop<Uid<S>>),

    // optional fields
    Contact(Prop<Text<S>, TextParams<S>>),
    DtStart(Prop<DateTimeOrDate, DtParams<S>>),
    DtEnd(Prop<DateTimeOrDate, DtParams<S>>),
    Organizer(Prop<CalAddress<S>, Box<OrganizerParams<S>>>),
    Url(Prop<Uri<S>>),

    // free multiplicity fields
    Attendee(PropSeq<CalAddress<S>, Box<AttendeeParams<S>>>),
    Comment(PropSeq<Text<S>, TextParams<S>>),
    FreeBusy(PropSeq<Box<[Period]>, FBTypeParams<S>>),
    RequestStatus(PropSeq<RequestStatus<S>, LangParams<S>>)
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
    TzId(Prop<TzId<S>>),

    // optional fields
    LastModified(Prop<DateTime<Utc>>),
    TzUrl(Prop<Uri<S>>)
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
    DtStart(Prop<DateTimeOrDate, DtParams<S>>),
    TzOffsetTo(Prop<UtcOffset>),
    TzOffsetFrom(Prop<UtcOffset>),

    // optional fields
    RRule(Prop<Box<RRule>>), // SHOULD NOT occur more than once

    // free multiplicity fields
    Comment(PropSeq<Text<S>, TextParams<S>>),
    RDate(PropSeq<RDateSeq, DtParams<S>>),
    TzName(Prop<Text<S>, LangParams<S>>)
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
    Action(Prop<AudioAction>),
    Trigger(TriggerProp),

    // optional fields
    DurRep(Prop<Duration>, Prop<Integer>), // the product of DURATION and REPEAT
    Attach(Prop<AttachValue<S>, AttachParams<S>>)
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
    Action(Prop<DisplayAction>),
    Description(Prop<Text<S>, TextParams<S>>),
    Trigger(TriggerProp),

    // optional fields
    DurRep(Prop<Duration>, Prop<Integer>) // the product of DURATION and REPEAT
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
    Action(Prop<EmailAction>),
    Description(Prop<Text<S>, TextParams<S>>),
    Trigger(TriggerProp),
    Summary(Prop<Text<S>, TextParams<S>>),

    // optional fields
    DurRep(Prop<Duration>, Prop<Integer>), // the product of DURATION and REPEAT

    // free multiplicity fields
    Attendee(PropSeq<CalAddress<S>, Box<AttendeeParams<S>>>),
    Attach(PropSeq<AttachValue<S>, AttachParams<S>>)
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
    Action(Prop<S>),
    Description(Prop<Text<S>, TextParams<S>>),
    Trigger(TriggerProp),
    Summary(Prop<Text<S>, TextParams<S>>),

    // optional fields
    DurRep(Prop<Duration>, Prop<Integer>), // the product of DURATION and REPEAT

    // free multiplicity fields
    Attendee(PropSeq<CalAddress<S>, Box<AttendeeParams<S>>>),
    Attach(PropSeq<AttachValue<S>, AttachParams<S>>)
}}

#[cfg(test)]
mod tests {
    use crate::{date, time};

    use super::*;

    #[test]
    fn basic_event_table_usage() {
        let mut event = EventTable::new();

        let uid = EventProp::Uid(Prop::from_value(Uid("some-identifier")));
        let dtstamp = EventProp::DtStamp(Prop {
            value: DateTime {
                date: date!(1997;12;24),
                time: time!(15;20;12, Utc),
            },
            params: Default::default(),
            extra_params: Default::default(),
        });

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
