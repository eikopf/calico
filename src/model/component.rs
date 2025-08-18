//! Model types for calendar components.

use std::hash::{BuildHasher, Hash, Hasher, RandomState};

use hashbrown::{HashTable, hash_table::Entry as TableEntry};

use super::{
    parameter::KnownParam,
    primitive::{
        AttachValue, CalAddress, ClassValue, DateTime, DateTimeOrDate,
        DateTimeOrDateSeq, Duration, EventStatus, Geo, Integer, Priority,
        RDateSeq, RequestStatus, Text, TimeTransparency, Uid, Uri, Utc, Value,
    },
    property::{
        AttachParams, AttendeeParams, DtParams, LangParams, OrganizerParams,
        Prop, RecurrenceIdParams, RelTypeParams, TextParams,
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

/// A sequence of [`Prop`].
type PropSeq<V, P = ()> = Box<[Prop<V, P>]>;

/// A basic trait for types that have a discriminant and can return it.
trait Disc {
    /// The discriminant type of `Self`.
    type Discriminant;

    /// Returns the discriminant value of `self`.
    fn discriminant(&self) -> Self::Discriminant;
}

/// A VEVENT component (RFC 5545 §3.6.1).
#[derive(Debug)]
pub struct Event<S>(HashTable<EventEntry<S>>, RandomState);

type EventEntry<S> = Entry<EventProp<S>, S>;

impl<S> Event<S> {
    pub fn new() -> Self {
        Self(HashTable::new(), RandomState::new())
    }

    fn insert_raw(&mut self, value: EventEntry<S>) -> Option<EventEntry<S>>
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

    fn get_raw(&self, key: Key<EventProp<S>, &S>) -> Option<&EventEntry<S>>
    where
        S: Hash + PartialEq,
    {
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(&key);

        self.0.find(hash(&key), eq)
    }

    fn eq(lhs: &Key<EventProp<S>, &S>) -> impl Fn(&EventEntry<S>) -> bool
    where
        S: PartialEq,
    {
        move |rhs| match rhs.as_key() {
            Key::Known(r) => matches!(lhs, Key::Known(l) if l == &r),
            Key::Iana(r) => matches!(lhs, Key::Iana(l) if l == &r),
            Key::X(r) => matches!(lhs, Key::X(l) if l == &r),
        }
    }

    fn hash_entry(hasher: &impl BuildHasher) -> impl Fn(&EventEntry<S>) -> u64
    where
        S: Hash,
    {
        let h = Self::hash_key(hasher);
        move |entry| h(&entry.as_key())
    }

    fn hash_key(
        hasher: &impl BuildHasher,
    ) -> impl Fn(&Key<EventProp<S>, &S>) -> u64
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

impl<S> Default for Event<S> {
    fn default() -> Self {
        Self::new()
    }
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum EventTerminationProp<S> {
    End(Prop<DateTimeOrDate, DtParams<S>>),
    Duration(Prop<Duration>),
}

#[cfg(test)]
mod tests {
    use chrono::NaiveDate;

    use crate::model::primitive::{Date, RawTime, Time};

    use super::*;

    #[test]
    fn basic_event_table_usage() {
        let mut event = Event::new();

        let uid = EventProp::Uid(Prop::from_value(Uid("some-identifier")));
        let dtstamp = EventProp::DtStamp(Prop {
            value: DateTime {
                date: Date(NaiveDate::from_ymd_opt(1997, 12, 24).unwrap()),
                time: Time {
                    raw: RawTime {
                        hours: 15,
                        minutes: 20,
                        seconds: 12,
                    },
                    format: Utc,
                },
            },
            params: Default::default(),
            extra_params: Default::default(),
        });

        let prev = event.insert_raw(Entry::Known(uid.clone()));
        assert!(prev.is_none());
        let prev = event.insert_raw(Entry::Known(dtstamp.clone()));
        assert!(prev.is_none());
        assert_eq!(event.0.len(), 2);

        let uid_ref = event.get_raw(Key::Known(EventPropName::Uid));
        let dtstamp_ref = event.get_raw(Key::Known(EventPropName::DtStamp));

        assert_eq!(Some(&Entry::Known(uid)), uid_ref);
        assert_eq!(Some(&Entry::Known(dtstamp)), dtstamp_ref);
    }
}
