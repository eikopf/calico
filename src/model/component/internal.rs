//! Internals for the component types defined in [`crate::model::component`].

use std::{
    fmt::Debug,
    hash::{BuildHasher, Hash, Hasher, RandomState},
};

use hashbrown::{HashTable, hash_table::Entry};
use winnow::stream::Stream;

use crate::{
    model::{
        css::Css3Color,
        primitive::{
            AttachValue, AudioAction, Binary, CalAddress, ClassValue, CompletionPercentage,
            DateTime, DateTimeOrDate, DisplayAction, Duration, EmailAction, EventStatus, ExDateSeq,
            Geo, ImageData, Integer, JournalStatus, Method, ParticipantType, Period,
            PositiveInteger, Priority, ProximityValue, RDateSeq, RequestStatus, ResourceType,
            StyledDescriptionValue, Text, TimeTransparency, TodoStatus, TzId, Uid, UnknownAction,
            Uri, Utc, UtcOffset,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams, ImageParams,
            LangParams, MultiProp, OrganizerParams, Prop, RecurrenceIdParams, RelTypeParams,
            StructuredDataParams, StyledDescriptionParams, TextParams, TriggerParams, UnknownProp,
            UnknownPropKind, UriStructuredDataParams,
        },
        rrule::RRule,
    },
    parser::escaped::{Equiv, Escaped, LineFoldCaseless},
};

/// An uninhabited type implementing `AsRef<[u8]>` that can be passed to [`PropKey`] when the type
/// of the `Unknown` variant is unknown or arbitrary.
#[derive(Hash)]
pub enum EmptyName {}

impl AsRef<[u8]> for EmptyName {
    fn as_ref(&self) -> &[u8] {
        unreachable!()
    }
}

#[derive(Clone)]
pub struct PropertyTable<S>(HashTable<PropEntry<S>>, RandomState);

impl<S: Debug> Debug for PropertyTable<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("PropertyTable").field(&self.0).finish()
    }
}

impl<S> Default for PropertyTable<S> {
    fn default() -> Self {
        Self(Default::default(), Default::default())
    }
}

impl<S> PropertyTable<S> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashTable::with_capacity(capacity), Default::default())
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, value: PropEntry<S>) -> Option<PropEntry<S>>
    where
        S: HashCaseless + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let hash = Self::hash_entry(&self.1);
        let key = value.as_key();
        let eq = Self::eq(&key);

        match self.0.entry(hash(&value), eq, hash) {
            Entry::Occupied(mut entry) => Some(std::mem::replace(entry.get_mut(), value)),
            Entry::Vacant(entry) => {
                entry.insert(value);
                None
            }
        }
    }

    pub fn insert_unknown(&mut self, name: S, kind: UnknownPropKind, prop: UnknownProp<S>)
    where
        S: HashCaseless + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        let key = PropKey::Unknown(&name);

        match self.get_mut(&key) {
            Some(PropEntry::Unknown {
                props,
                kind: current_kind,
                ..
            }) => {
                debug_assert_eq!(*current_kind, kind);
                props.push(prop);
            }
            Some(_) => unreachable!(),
            None => {
                let entry = PropEntry::Unknown {
                    key: name,
                    kind,
                    props: vec![prop],
                };

                self.insert(entry);
            }
        }
    }

    pub fn get<T>(&self, key: &PropKey<T>) -> Option<&PropEntry<S>>
    where
        T: HashCaseless + AsRef<[u8]>,
        S: AsRef<[u8]>,
        for<'a> &'a T: Equiv<LineFoldCaseless, &'a S>,
    {
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(key);

        self.0.find(hash(key.as_ref()), eq)
    }

    pub fn get_mut<T>(&mut self, key: &PropKey<T>) -> Option<&mut PropEntry<S>>
    where
        T: HashCaseless + AsRef<[u8]>,
        S: AsRef<[u8]>,
        for<'a> &'a T: Equiv<LineFoldCaseless, &'a S>,
    {
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(key);

        self.0.find_mut(hash(key.as_ref()), eq)
    }

    pub fn get_known(&self, key: StaticProp) -> Option<&PropEntry<S>>
    where
        S: HashCaseless + AsRef<[u8]>,
    {
        let key = PropKey::Known::<EmptyName>(key);
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(&key);

        self.0.find(hash(key.as_ref()), eq)
    }

    pub fn remove<T>(&mut self, key: &PropKey<T>) -> Option<PropEntry<S>>
    where
        T: HashCaseless + AsRef<[u8]>,
        S: HashCaseless + AsRef<[u8]>,
        for<'a> &'a T: Equiv<LineFoldCaseless, &'a S>,
    {
        let hash_key = Self::hash_key(&self.1);
        let hash = Self::hash_entry(&self.1);
        let eq = Self::eq(key);
        let table_entry = self.0.entry(hash_key(key.as_ref()), eq, hash);

        match table_entry {
            Entry::Vacant(_) => None,
            Entry::Occupied(entry) => {
                let (entry, _) = entry.remove();
                Some(entry)
            }
        }
    }

    fn eq<T>(lhs: &PropKey<T>) -> impl for<'b> Fn(&'b PropEntry<S>) -> bool
    where
        T: AsRef<[u8]>,
        S: AsRef<[u8]>,
        for<'a> &'a T: Equiv<LineFoldCaseless, &'a S>,
    {
        move |rhs| match rhs.as_key() {
            PropKey::Known(r) => matches!(lhs, PropKey::Known(l) if *l == r),
            PropKey::Unknown(r) => {
                matches!(lhs, PropKey::Unknown(l) if l.equiv(r, LineFoldCaseless))
            }
        }
    }

    // TODO: maybe i can replace the Hash bound here with a different trait (HashCaseless) that
    // handles str, [u8], Escaped, etc. all correctly?

    fn hash_entry<T: HashCaseless>(
        hasher: &impl BuildHasher,
    ) -> impl for<'a> Fn(&'a PropEntry<T>) -> u64 {
        let h = Self::hash_key(hasher);
        move |entry| h(entry.as_key())
    }

    fn hash_key<T: HashCaseless>(
        hasher: &impl BuildHasher,
    ) -> impl for<'a> Fn(PropKey<&'a T>) -> u64 {
        |key| {
            let mut hasher = hasher.build_hasher();

            match key {
                PropKey::Known(name) => name.hash(&mut hasher),
                PropKey::Unknown(name) => name.hash_caseless(&mut hasher),
            };

            hasher.finish()
        }
    }
}

/// A type that can be case-insensitively hashed.
pub trait HashCaseless {
    fn hash_caseless(&self, state: &mut impl Hasher);
}

impl HashCaseless for EmptyName {
    fn hash_caseless(&self, _state: &mut impl Hasher) {
        unreachable!()
    }
}

impl HashCaseless for str {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for byte in self.bytes() {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl HashCaseless for String {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        self.as_str().hash_caseless(state)
    }
}

impl HashCaseless for [u8] {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for byte in self {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl<const N: usize> HashCaseless for [u8; N] {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for byte in self {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl HashCaseless for Escaped<'_> {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        for (_, byte) in self.iter_offsets() {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl<T: ?Sized + HashCaseless> HashCaseless for &T {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        (*self).hash_caseless(state)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropEntry<S> {
    Known {
        key: StaticProp,
        value: RawValue<S>,
    },
    Unknown {
        key: S,
        kind: UnknownPropKind,
        props: Vec<UnknownProp<S>>,
    },
}

impl<S> PropEntry<S> {
    pub const fn as_key(&self) -> PropKey<&S> {
        match self {
            PropEntry::Known { key, .. } => PropKey::Known(*key),
            PropEntry::Unknown { key, .. } => PropKey::Unknown(key),
        }
    }

    pub const fn as_raw_value(&self) -> Option<&RawValue<S>> {
        if let Self::Known { value, .. } = self {
            Some(value)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PropKey<S> {
    Known(StaticProp),
    Unknown(S),
}

impl<S> From<StaticProp> for PropKey<S> {
    fn from(value: StaticProp) -> Self {
        Self::Known(value)
    }
}

impl<S> PropKey<S> {
    pub const fn as_ref(&self) -> PropKey<&S> {
        match self {
            PropKey::Known(name) => PropKey::Known(*name),
            PropKey::Unknown(name) => PropKey::Unknown(name),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StaticProp {
    // CALENDAR PROPERTIES
    CalScale,
    Method,
    ProdId,
    Version,
    // DESCRIPTIVE COMPONENT PROPERTIES
    Attach,
    Categories,
    Class,
    Comment,
    Description,
    Geo,
    Location,
    PercentComplete,
    Priority,
    Resources,
    Status,
    Summary,
    // DATE AND TIME COMPONENT PROPERTIES
    DtCompleted,
    DtEnd,
    DtDue,
    DtStart,
    Duration,
    FreeBusy,
    Transp,
    // TIME ZONE COMPONENT PROPERTIES
    TzId,
    TzName,
    TzOffsetFrom,
    TzOffsetTo,
    TzUrl,
    // RELATIONSHIP COMPONENT PROPERTIES
    Attendee,
    Contact,
    Organizer,
    RecurId,
    RelatedTo,
    Url,
    Uid,
    // RECURRENCE COMPONENT PROPERTIES
    ExDate,
    RDate,
    RRule,
    // ALARM COMPONENT PROPERTIES
    Action,
    Repeat,
    Trigger,
    // CHANGE MANAGEMENT COMPONENT PROPERTIES
    Created,
    DtStamp,
    LastModified,
    Sequence,
    // MISCELLANEOUS COMPONENT PROPERTIES
    RequestStatus,
    // RFC 7986 PROPERTIES
    Name,
    RefreshInterval,
    Source,
    Color,
    Image,
    Conference,
    // RFC 9073 PROPERTIES
    LocationType,
    ParticipantType,
    ResourceType,
    CalendarAddress,
    StyledDescription,
    StructuredData,
    // RFC 9074 PROPERTIES
    Acknowledged,
    Proximity,
}

impl StaticProp {
    pub const fn as_key(self) -> PropKey<EmptyName> {
        PropKey::Known(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RawValue<S> {
    // PRIMITIVES
    CalAddress(Prop<S, CalAddress<S>>),
    Color(Prop<S, Css3Color>),
    DtUtc(Prop<S, DateTime<Utc>>),
    DtOrDateDt(Prop<S, DateTimeOrDate, DtParams<S>>),
    Duration(Prop<S, Duration>),
    Integer(Prop<S, Integer>),
    PositiveInteger(Prop<S, PositiveInteger>),
    Text(Prop<S, Text<S>>),
    Uid(Prop<S, Uid<S>>),
    Unit(Prop<S, ()>),
    Uri(Prop<S, Uri<S>>),
    UtcOffset(Prop<S, UtcOffset>),
    // ONE-OFFS
    Attach(Vec<MultiProp<S, AttachValue<S>, AttachParams<S>>>),
    Attendee(Vec<MultiProp<S, CalAddress<S>, Box<AttendeeParams<S>>>>),
    Class(Prop<S, ClassValue<S>>),
    Conference(Prop<S, Uri<S>, ConfParams<S>>),
    ExDate(Vec<MultiProp<S, ExDateSeq, DtParams<S>>>),
    FreeBusy(Vec<MultiProp<S, Vec<Period>, FBTypeParams<S>>>),
    Geo(Prop<S, Geo>),
    Image(Prop<S, ImageData<S>, ImageParams<S>>),
    Method(Prop<S, Method<S>>),
    Organizer(Vec<MultiProp<S, CalAddress<S>, Box<OrganizerParams<S>>>>),
    ParticipantType(Prop<S, ParticipantType<S>>),
    PercentComplete(Prop<S, CompletionPercentage>),
    Priority(Prop<S, Priority>),
    Proximity(Prop<S, ProximityValue<S>>),
    RDate(Vec<MultiProp<S, RDateSeq, DtParams<S>>>),
    RecurId(Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    RelatedTo(Prop<S, Text<S>, RelTypeParams<S>>),
    ResourceType(Prop<S, ResourceType<S>>),
    RequestStatus(Prop<S, RequestStatus<S>, LangParams<S>>),
    RRule(Prop<S, Box<RRule>>),
    StyledDescription(Prop<S, StyledDescriptionValue<S>, StyledDescriptionParams<S>>),
    Transp(Prop<S, TimeTransparency>),
    TzId(Prop<S, TzId<S>>),
    // TEXT VARIANTS
    TextN(Prop<S, Vec<Text<S>>>),
    TextText(Prop<S, Text<S>, TextParams<S>>),
    TextSeqText(Prop<S, Vec<Text<S>>, TextParams<S>>),
    TextTextN(Vec<MultiProp<S, Text<S>, TextParams<S>>>),
    TextSeqTextN(Vec<MultiProp<S, Vec<Text<S>>, TextParams<S>>>),
    TextLang(Prop<S, Text<S>, LangParams<S>>),
    TextSeqLangN(Vec<MultiProp<S, Vec<Text<S>>, LangParams<S>>>),
    // STATUS VARIANTS
    EventStatus(Prop<S, EventStatus>),
    TodoStatus(Prop<S, TodoStatus>),
    JournalStatus(Prop<S, JournalStatus>),
    // ACTION VARIANTS
    AudioAction(Prop<S, AudioAction>),
    DisplayAction(Prop<S, DisplayAction>),
    EmailAction(Prop<S, EmailAction>),
    UnknownAction(Prop<S, UnknownAction<S>>),
    // STRUCTURED DATA VARIANTS
    StructuredDataBinary(Prop<S, Binary<S>, StructuredDataParams<S>>),
    StructuredDataText(Prop<S, Text<S>, StructuredDataParams<S>>),
    StructuredDataUri(Prop<S, Uri<S>, UriStructuredDataParams<S>>),
    // OTHER VARIANTS
    Attach1(Prop<S, AttachValue<S>, AttachParams<S>>), // used by audio alarms
    TriggerRelative(Prop<S, Duration, TriggerParams>),
}

#[cfg(test)]
mod tests {
    use crate::{date, model::primitive::Value, parser::escaped::AsEscaped, time};

    use super::*;

    #[test]
    fn basic_property_table_usage() {
        let mut props = PropertyTable::new();

        let uid = RawValue::Uid(Prop::from_value(Uid("some-identifier")));
        let dtstamp = RawValue::DtUtc(Prop::from_value(DateTime {
            date: date!(1997;12;24),
            time: time!(15;20;12, Utc),
        }));

        let prev = props.insert(PropEntry::Known {
            key: StaticProp::Uid,
            value: uid.clone(),
        });
        assert!(prev.is_none());

        let prev = props.insert(PropEntry::Known {
            key: StaticProp::DtStamp,
            value: dtstamp.clone(),
        });
        assert!(prev.is_none());

        assert_eq!(props.0.len(), 2);

        let uid_ref = props
            .get(&StaticProp::Uid.as_key())
            .and_then(PropEntry::as_raw_value);

        let dtstamp_ref = props
            .get(&StaticProp::DtStamp.as_key())
            .and_then(PropEntry::as_raw_value);

        assert_eq!(Some(&uid), uid_ref);
        assert_eq!(Some(&dtstamp), dtstamp_ref);
    }

    #[test]
    fn property_table_get_unknown() {
        let mut props = PropertyTable::new();

        let unknown_prop = UnknownProp {
            value: Box::new(Value::Boolean(true)),
            params: vec![],
            unknown_params: vec![],
        };

        props.insert_unknown("X-A-RANDOM-BOOLEAN", UnknownPropKind::X, unknown_prop);

        assert_eq!(
            props.get(&PropKey::Unknown("X-A-RANDOM-BOOLEAN")),
            props.get(&PropKey::Unknown("x-a-random-boolean")),
        );

        assert_eq!(
            props.get(&PropKey::Unknown("X-A-RANDOM-BOOLEAN")),
            props.get(&PropKey::Unknown(b"X-a-RaNdOm-BoOlEaN")),
        );

        assert_eq!(
            props.get(&PropKey::Unknown("X-A-RANDOM-BOOLEAN")),
            props.get(&PropKey::Unknown("X-a-RaNdOm-BoOlEaN".as_bytes())),
        );

        assert_eq!(
            props.get(&PropKey::Unknown("X-A-RANDOM-BOOLEAN")),
            props.get(&PropKey::Unknown("X-a-RANDOM-boolean".to_string())),
        );

        assert_eq!(
            props.get(&PropKey::Unknown("X-A-RANDOM-BOOLEAN")),
            props.get(&PropKey::Unknown(
                "x-a-ra\r\n\tndo\r\n m-boolean".as_escaped()
            )),
        );
    }
}
