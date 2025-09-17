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
        parameter::UnknownParam,
        primitive::{
            AlarmAction, AttachValue, AudioAction, Binary, CalAddress, ClassValue,
            CompletionPercentage, DateTime, DateTimeOrDate, DisplayAction, Duration, EmailAction,
            EventStatus, ExDateSeq, Geo, ImageData, Integer, JournalStatus, Method,
            ParticipantType, Period, PositiveInteger, Priority, ProximityValue, RDateSeq,
            RequestStatus, ResourceType, Status, StyledDescriptionValue, Text, TimeTransparency,
            TodoStatus, TzId, Uid, UnknownAction, Uri, Utc, UtcOffset,
        },
        property::{
            AttachParams, AttendeeParams, ConfParams, DtParams, FBTypeParams, ImageParams,
            LangParams, MultiParams, MultiProp, OrganizerParams, Prop, RecurrenceIdParams,
            RelTypeParams, StructuredDataMultiProp, StructuredDataParams, StyledDescriptionParams,
            TextParams, TriggerMultiProp, TriggerParams, TriggerPropMut, TriggerPropRef,
            UniversalParams, UnknownProp, UnknownPropKind, UriStructuredDataParams,
        },
        rrule::RRule,
    },
    parser::{
        escaped::{Equiv, Escaped, LineFoldCaseless},
        property::KnownProp,
    },
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

    pub fn contains_key<T>(&self, key: &PropKey<T>) -> bool
    where
        T: HashCaseless + AsRef<[u8]>,
        S: AsRef<[u8]>,
        for<'a> &'a T: Equiv<LineFoldCaseless, &'a S>,
    {
        self.get(key).is_some()
    }

    pub fn contains_known(&self, key: StaticProp) -> bool
    where
        S: HashCaseless + AsRef<[u8]>,
    {
        self.get_known(key).is_some()
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

    pub fn insert_known(&mut self, key: StaticProp, value: RawValue<S>) -> Option<PropEntry<S>>
    where
        S: HashCaseless + Equiv<LineFoldCaseless> + AsRef<[u8]>,
    {
        self.insert(PropEntry::Known { key, value })
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

    pub fn get_known(&self, key: StaticProp) -> Option<&RawValue<S>>
    where
        S: HashCaseless + AsRef<[u8]>,
    {
        let key = PropKey::Known::<EmptyName>(key);
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(&key);

        self.0
            .find(hash(key.as_ref()), eq)
            .and_then(PropEntry::as_raw_value)
    }

    pub fn get_known_mut(&mut self, key: StaticProp) -> Option<&mut RawValue<S>>
    where
        S: HashCaseless + AsRef<[u8]>,
    {
        let key = PropKey::Known::<EmptyName>(key);
        let hash = Self::hash_key(&self.1);
        let eq = Self::eq(&key);

        self.0
            .find_mut(hash(key.as_ref()), eq)
            .and_then(PropEntry::as_raw_value_mut)
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

    pub fn remove_known(&mut self, key: StaticProp) -> Option<RawValue<S>>
    where
        S: HashCaseless + AsRef<[u8]>,
    {
        let key = key.as_key();
        self.remove(&key).and_then(PropEntry::into_raw_value)
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

impl<T: ?Sized + HashCaseless> HashCaseless for &mut T {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        (**self).hash_caseless(state)
    }
}

impl<T: ?Sized + HashCaseless> HashCaseless for Box<T> {
    fn hash_caseless(&self, state: &mut impl Hasher) {
        self.as_ref().hash_caseless(state)
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

    pub fn into_raw_value(self) -> Option<RawValue<S>> {
        if let Self::Known { value, .. } = self {
            Some(value)
        } else {
            None
        }
    }

    pub const fn as_raw_value(&self) -> Option<&RawValue<S>> {
        if let Self::Known { value, .. } = self {
            Some(value)
        } else {
            None
        }
    }

    pub const fn as_raw_value_mut(&mut self) -> Option<&mut RawValue<S>> {
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

impl<S> From<&KnownProp<S>> for StaticProp {
    fn from(value: &KnownProp<S>) -> Self {
        match value {
            KnownProp::CalScale => Self::CalScale,
            KnownProp::Method(..) => Self::Method,
            KnownProp::ProdId(..) => Self::ProdId,
            KnownProp::Version => Self::Version,
            KnownProp::Attach(..) => Self::Attach,
            KnownProp::Categories(..) => Self::Categories,
            KnownProp::Class(..) => Self::Class,
            KnownProp::Comment(..) => Self::Comment,
            KnownProp::Description(..) => Self::Description,
            KnownProp::Geo(..) => Self::Geo,
            KnownProp::Location(..) => Self::Location,
            KnownProp::PercentComplete(..) => Self::PercentComplete,
            KnownProp::Priority(..) => Self::Priority,
            KnownProp::Resources(..) => Self::Resources,
            KnownProp::Status(..) => Self::Status,
            KnownProp::Summary(..) => Self::Summary,
            KnownProp::DtCompleted(..) => Self::DtCompleted,
            KnownProp::DtEnd(..) => Self::DtEnd,
            KnownProp::DtDue(..) => Self::DtDue,
            KnownProp::DtStart(..) => Self::DtStart,
            KnownProp::Duration(..) => Self::Duration,
            KnownProp::FreeBusy(..) => Self::FreeBusy,
            KnownProp::Transparency(..) => Self::Transp,
            KnownProp::TzId(..) => Self::TzId,
            KnownProp::TzName(..) => Self::TzName,
            KnownProp::TzOffsetFrom(..) => Self::TzOffsetFrom,
            KnownProp::TzOffsetTo(..) => Self::TzOffsetTo,
            KnownProp::TzUrl(..) => Self::TzUrl,
            KnownProp::Attendee(..) => Self::Attendee,
            KnownProp::Contact(..) => Self::Contact,
            KnownProp::Organizer(..) => Self::Organizer,
            KnownProp::RecurrenceId(..) => Self::RecurId,
            KnownProp::RelatedTo(..) => Self::RelatedTo,
            KnownProp::Url(..) => Self::Url,
            KnownProp::Uid(..) => Self::Uid,
            KnownProp::ExDate(..) => Self::ExDate,
            KnownProp::RDate(..) => Self::RDate,
            KnownProp::RRule(..) => Self::RRule,
            KnownProp::Action(..) => Self::Action,
            KnownProp::Repeat(..) => Self::Repeat,
            KnownProp::TriggerRelative(..) | KnownProp::TriggerAbsolute(..) => Self::Trigger,
            KnownProp::Created(..) => Self::Created,
            KnownProp::DtStamp(..) => Self::DtStamp,
            KnownProp::LastModified(..) => Self::LastModified,
            KnownProp::Sequence(..) => Self::Sequence,
            KnownProp::RequestStatus(..) => Self::RequestStatus,
            KnownProp::Name(..) => Self::Name,
            KnownProp::RefreshInterval(..) => Self::RefreshInterval,
            KnownProp::Source(..) => Self::Source,
            KnownProp::Color(..) => Self::Color,
            KnownProp::Image(..) => Self::Image,
            KnownProp::Conference(..) => Self::Conference,
            KnownProp::LocationType(..) => Self::LocationType,
            KnownProp::ParticipantType(..) => Self::ParticipantType,
            KnownProp::ResourceType(..) => Self::ResourceType,
            KnownProp::CalendarAddress(..) => Self::CalendarAddress,
            KnownProp::StyledDescription(..) => Self::StyledDescription,
            KnownProp::StructuredDataBinary(..)
            | KnownProp::StructuredDataText(..)
            | KnownProp::StructuredDataUri(..) => Self::StructuredData,
            KnownProp::Acknowledged(..) => Self::Acknowledged,
            KnownProp::Proximity(..) => Self::Proximity,
        }
    }
}

/// Defines an enum generic over the parameter `S` with [`From`] and [`TryFrom`] impls for each of
/// the variants in the enum. This will cause a compiler error if the same type occurs in multiple
/// variants.
macro_rules! define_value_type {
    (
        $(#[$m:meta])*
        $v:vis
        $name:ident ($name_inner:ident)
        { $($variant:ident ($field:ty)),* $(,)? }
    ) => {
        $(#[$m])*
        $v struct $name<S>($name_inner<S>);

        $(#[$m])*
        enum $name_inner <S> {
            $($variant ($field)),*
        }

        $(
            impl<S> From<$field> for $name <S> {
                fn from(value: $field) -> Self {
                    Self($name_inner::$variant(value))
                }
            }
        )*

        $(
            impl<S> TryFrom<$name<S>> for $field {
                type Error = ();

                fn try_from(value: $name<S>) -> Result<$field, Self::Error> {
                    if let $name($name_inner::$variant(x)) = value {
                        Ok(x)
                    } else {
                        Err(())
                    }
                }
            }

            impl<'a, S> TryFrom<&'a $name<S>> for &'a $field {
                type Error = ();

                fn try_from(value: &'a $name<S>) -> Result<&'a $field, Self::Error> {
                    if let $name($name_inner::$variant(x)) = value {
                        Ok(x)
                    } else {
                        Err(())
                    }
                }
            }

            impl<'a, S> TryFrom<&'a mut $name<S>> for &'a mut $field {
                type Error = ();

                fn try_from(value: &'a mut $name<S>) -> Result<&'a mut $field, Self::Error> {
                    if let $name($name_inner::$variant(x)) = value {
                        Ok(x)
                    } else {
                        Err(())
                    }
                }
            }
        )*
    };
}

define_value_type! {
#[derive(Debug, Clone, PartialEq, Eq)]
pub RawValue(RawValueInner) {
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
    Conf(Prop<S, Uri<S>, ConfParams<S>>),
    ExDate(Vec<MultiProp<S, ExDateSeq, DtParams<S>>>),
    FreeBusy(Vec<MultiProp<S, Vec<Period>, FBTypeParams<S>>>),
    Geo(Prop<S, Geo>),
    Image(Prop<S, ImageData<S>, ImageParams<S>>),
    Method(Prop<S, Method<S>>),
    Organizer(Prop<S, CalAddress<S>, Box<OrganizerParams<S>>>),
    ParticipantType(Prop<S, ParticipantType<S>>),
    Percent(Prop<S, CompletionPercentage>),
    Priority(Prop<S, Priority>),
    Proximity(Prop<S, ProximityValue<S>>),
    RDate(Vec<MultiProp<S, RDateSeq, DtParams<S>>>),
    RecurId(Prop<S, DateTimeOrDate, RecurrenceIdParams<S>>),
    RelatedTo(Prop<S, Text<S>, RelTypeParams<S>>),
    ResourceType(Prop<S, ResourceType<S>>),
    RequestStatus(Vec<MultiProp<S, RequestStatus<S>, LangParams<S>>>),
    RRule(Vec<MultiProp<S, Box<RRule>>>),
    StyledDescription(Prop<S, StyledDescriptionValue<S>, StyledDescriptionParams<S>>),
    Transp(Prop<S, TimeTransparency>),
    TzId(Prop<S, TzId<S>>),
    // TEXT VARIANTS
    TextN(Vec<MultiProp<S, Text<S>>>),
    TextSeq(Prop<S, Vec<Text<S>>>),
    TextSeqN(Vec<MultiProp<S, Vec<Text<S>>>>),
    TextText(Prop<S, Text<S>, TextParams<S>>),
    TextSeqText(Prop<S, Vec<Text<S>>, TextParams<S>>),
    TextTextN(Vec<MultiProp<S, Text<S>, TextParams<S>>>),
    TextSeqTextN(Vec<MultiProp<S, Vec<Text<S>>, TextParams<S>>>),
    TextLang(Prop<S, Text<S>, LangParams<S>>),
    TextLangN(Vec<MultiProp<S, Text<S>, LangParams<S>>>),
    TextSeqLang(Prop<S, Vec<Text<S>>, LangParams<S>>),
    TextSeqLangN(Vec<MultiProp<S, Vec<Text<S>>, LangParams<S>>>),
    // STATUS VARIANTS
    Status(Prop<S, Status>),
    EventStatus(Prop<S, EventStatus>),
    TodoStatus(Prop<S, TodoStatus>),
    JournalStatus(Prop<S, JournalStatus>),
    // ACTION VARIANTS
    Action(Prop<S, AlarmAction<S>>),
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
    // FREE MULTIPLICITY VARIANTS
    ImageN(Vec<MultiProp<S, ImageData<S>, ImageParams<S>>>),
    ConfN(Vec<MultiProp<S, Uri<S>, ConfParams<S>>>),
    RelatedToN(Vec<MultiProp<S, Text<S>, RelTypeParams<S>>>),
    UnitN(Vec<MultiProp<S, ()>>),
    MethodN(Vec<MultiProp<S, Method<S>>>),
    ClassN(Vec<MultiProp<S, ClassValue<S>>>),
    GeoN(Vec<MultiProp<S, Geo>>),
    DtUtcN(Vec<MultiProp<S, DateTime<Utc>>>),
    UidN(Vec<MultiProp<S, Uid<S>>>),
    UriN(Vec<MultiProp<S, Uri<S>>>),
    PriorityN(Vec<MultiProp<S, Priority>>),
    PercentN(Vec<MultiProp<S, CompletionPercentage>>),
    DtOrDateDtN(Vec<MultiProp<S, DateTimeOrDate, DtParams<S>>>),
    DurationN(Vec<MultiProp<S, Duration>>),
    StatusN(Vec<MultiProp<S, Status>>),
    TranspN(Vec<MultiProp<S, TimeTransparency>>),
    TzIdN(Vec<MultiProp<S, TzId<S>>>),
    UtcOffsetN(Vec<MultiProp<S, UtcOffset>>),
    OrganizerN(Vec<MultiProp<S, CalAddress<S>, Box<OrganizerParams<S>>>>),
    RecurIdN(Vec<MultiProp<S, DateTimeOrDate, RecurrenceIdParams<S>>>),
    ActionN(Vec<MultiProp<S, AlarmAction<S>>>),
    IntegerN(Vec<MultiProp<S, Integer>>),
    PositiveIntegerN(Vec<MultiProp<S, PositiveInteger>>),
    ProximityN(Vec<MultiProp<S, ProximityValue<S>>>),
    ColorN(Vec<MultiProp<S, Css3Color>>),
    ParticipantTypeN(Vec<MultiProp<S, ParticipantType<S>>>),
    ResourceTypeN(Vec<MultiProp<S, ResourceType<S>>>),
    CalAddressN(Vec<MultiProp<S, CalAddress<S>>>),
    StyledDescriptionN(Vec<MultiProp<S, StyledDescriptionValue<S>, StyledDescriptionParams<S>>>),
    // HETEROGENEOUS FREE MULTIPLICITY VARIANTS
    AnyTriggerN(Vec<TriggerMultiProp<S>>),
    AnyStructuredDataN(Vec<StructuredDataMultiProp<S>>),
}}

impl<S> RawValue<S> {
    /// Updates the given `raw_value` (if any) with a [`MultiProp`] derived from the other
    /// parameters, or initialises a new value with the `MultiProp` if it is [`None`]. All values
    /// returned by this method are sequential, even when only a single value is present.
    ///
    /// The point of this method is to be a fallback for the construction of PropertyTables when a
    /// given component doesn't explicitly handle a certain [`KnownProp`]. Every property may be
    /// admissible for a given component so long as that component does not explicitly disallow
    /// them; notice that RFC 9074 introduced the usage of UID on VALARM components and this was
    /// **not** considered a backwards-incompatible change—rather, it was previously treated as an
    /// unknown IANA-registered property.
    pub(crate) fn append_known_prop(
        raw_value: Option<Self>,
        known_prop: KnownProp<S>,
        univ: UniversalParams,
        unknown_params: Vec<UnknownParam<S>>,
    ) -> Option<Self> {
        macro_rules! prop {
            ($value:expr, $params:expr) => {
                Prop {
                    derived: univ.derived.unwrap_or_default(),
                    value: $value,
                    params: MultiParams {
                        order: univ.order,
                        known: $params,
                    },
                    unknown_params,
                }
            };
        }

        macro_rules! both {
            ($var1:ident, $varN:ident, $new_prop:expr) => {
                match raw_value {
                    Some(RawValue(RawValueInner::$varN(mut props))) => {
                        props.push($new_prop);
                        Some(props.into())
                    }
                    Some(RawValue(RawValueInner::$var1(prop))) => {
                        Some(vec![prop.into_multi_prop(), $new_prop].into())
                    }
                    None => Some(vec![$new_prop].into()),
                    Some(_) => None,
                }
            };
        }

        macro_rules! seq {
            ($var:ident, $new_prop:expr) => {
                match raw_value {
                    Some(RawValue(RawValueInner::$var(mut props))) => {
                        props.push($new_prop);
                        Some(props.into())
                    }
                    None => Some(vec![$new_prop].into()),
                    Some(_) => None,
                }
            };
        }

        macro_rules! trigger {
            ($new_prop:expr) => {{
                let mut vec = match raw_value {
                    Some(RawValue(RawValueInner::AnyTriggerN(props))) => Some(props),
                    None => Some(Vec::with_capacity(1)),
                    Some(_) => None,
                }?;

                vec.push($new_prop.into());
                Some(RawValue(RawValueInner::AnyTriggerN(vec)))
            }};
        }

        macro_rules! structured_data {
            ($new_prop:expr) => {{
                let mut vec = match raw_value {
                    Some(RawValue(RawValueInner::AnyStructuredDataN(props))) => Some(props),
                    None => Some(Vec::with_capacity(1)),
                    Some(_) => None,
                }?;

                vec.push($new_prop.into());
                Some(RawValue(RawValueInner::AnyStructuredDataN(vec)))
            }};
        }

        match known_prop {
            KnownProp::CalScale | KnownProp::Version => both!(Unit, UnitN, prop!((), ())),
            KnownProp::Method(value) => both!(Method, MethodN, prop!(value, ())),
            KnownProp::ProdId(value) => both!(Text, TextN, prop!(value, ())),
            KnownProp::Attach(value, params) => both!(Attach1, Attach, prop!(value, params)),
            KnownProp::Categories(value, params) => {
                both!(TextSeqLang, TextSeqLangN, prop!(value, params))
            }
            KnownProp::Class(value) => both!(Class, ClassN, prop!(value, ())),
            KnownProp::Comment(value, params)
            | KnownProp::Description(value, params)
            | KnownProp::Location(value, params)
            | KnownProp::Summary(value, params)
            | KnownProp::Contact(value, params)
            | KnownProp::Name(value, params) => {
                both!(TextText, TextTextN, prop!(value, params))
            }
            KnownProp::Geo(value) => both!(Geo, GeoN, prop!(value, ())),
            KnownProp::PercentComplete(value) => both!(Percent, PercentN, prop!(value, ())),
            KnownProp::Priority(value) => both!(Priority, PriorityN, prop!(value, ())),
            KnownProp::Resources(value, params) => {
                both!(TextSeqText, TextSeqTextN, prop!(value, params))
            }
            KnownProp::Status(value) => both!(Status, StatusN, prop!(value, ())),
            KnownProp::DtCompleted(value)
            | KnownProp::Created(value)
            | KnownProp::DtStamp(value)
            | KnownProp::LastModified(value)
            | KnownProp::Acknowledged(value) => both!(DtUtc, DtUtcN, prop!(value, ())),
            KnownProp::DtEnd(value, params)
            | KnownProp::DtDue(value, params)
            | KnownProp::DtStart(value, params) => {
                both!(DtOrDateDt, DtOrDateDtN, prop!(value, params))
            }
            KnownProp::Duration(value) | KnownProp::RefreshInterval(value) => {
                both!(Duration, DurationN, prop!(value, ()))
            }
            KnownProp::FreeBusy(value, params) => seq!(FreeBusy, prop!(value, params)),
            KnownProp::Transparency(value) => both!(Transp, TranspN, prop!(value, ())),
            KnownProp::TzId(value) => both!(TzId, TzIdN, prop!(value, ())),
            KnownProp::TzName(value, params) => both!(TextLang, TextLangN, prop!(value, params)),
            KnownProp::TzOffsetFrom(value) | KnownProp::TzOffsetTo(value) => {
                both!(UtcOffset, UtcOffsetN, prop!(value, ()))
            }
            KnownProp::TzUrl(value) | KnownProp::Url(value) | KnownProp::Source(value) => {
                both!(Uri, UriN, prop!(value, ()))
            }
            KnownProp::Attendee(value, params) => seq!(Attendee, prop!(value, Box::new(params))),
            KnownProp::Organizer(value, params) => {
                both!(Organizer, OrganizerN, prop!(value, Box::new(params)))
            }
            KnownProp::RecurrenceId(value, params) => {
                both!(RecurId, RecurIdN, prop!(value, params))
            }
            KnownProp::RelatedTo(value, params) => {
                both!(RelatedTo, RelatedToN, prop!(value, params))
            }
            KnownProp::Uid(value) => both!(Uid, UidN, prop!(value, ())),
            KnownProp::ExDate(value, params) => seq!(ExDate, prop!(value, params)),
            KnownProp::RDate(value, params) => seq!(RDate, prop!(value, params)),
            KnownProp::RRule(value) => seq!(RRule, prop!(Box::new(value), ())),
            KnownProp::Action(value) => both!(Action, ActionN, prop!(value, ())),
            KnownProp::Repeat(value) | KnownProp::Sequence(value) => {
                both!(Integer, IntegerN, prop!(value, ()))
            }
            KnownProp::TriggerRelative(value, params) => trigger!(prop!(value, params)),
            KnownProp::TriggerAbsolute(value) => trigger!(prop!(value, ())),
            KnownProp::RequestStatus(value, params) => seq!(RequestStatus, prop!(value, params)),
            KnownProp::Color(value) => both!(Color, ColorN, prop!(value, ())),
            KnownProp::Image(value, params) => both!(Image, ImageN, prop!(value, params)),
            KnownProp::Conference(value, params) => both!(Conf, ConfN, prop!(value, params)),
            KnownProp::LocationType(value) => both!(TextSeq, TextSeqN, prop!(value, ())),
            KnownProp::ParticipantType(value) => {
                both!(ParticipantType, ParticipantTypeN, prop!(value, ()))
            }
            KnownProp::ResourceType(value) => both!(ResourceType, ResourceTypeN, prop!(value, ())),
            KnownProp::CalendarAddress(value) => both!(CalAddress, CalAddressN, prop!(value, ())),
            KnownProp::StyledDescription(value, params) => {
                both!(StyledDescription, StyledDescriptionN, prop!(value, params))
            }
            KnownProp::StructuredDataBinary(value, params) => {
                structured_data!(prop!(value, params))
            }
            KnownProp::StructuredDataText(value, params) => {
                structured_data!(prop!(value, params))
            }
            KnownProp::StructuredDataUri(value, params) => {
                structured_data!(prop!(value, params))
            }
            KnownProp::Proximity(value) => both!(Proximity, ProximityN, prop!(value, ())),
        }
    }
}

impl<'a, S> TryFrom<&'a RawValue<S>> for AlarmAction<&'a S> {
    type Error = ();

    fn try_from(value: &'a RawValue<S>) -> Result<Self, Self::Error> {
        match &value.0 {
            RawValueInner::AudioAction(_) => Ok(Self::Audio),
            RawValueInner::DisplayAction(_) => Ok(Self::Display),
            RawValueInner::EmailAction(_) => Ok(Self::Email),
            RawValueInner::UnknownAction(Prop { value, .. }) => match value.as_ref() {
                UnknownAction::Iana(action) => Ok(Self::Iana(action)),
                UnknownAction::X(action) => Ok(Self::X(action)),
            },
            _ => Err(()),
        }
    }
}

impl<'a, S> TryFrom<&'a RawValue<S>> for TriggerPropRef<'a, S> {
    type Error = ();

    fn try_from(value: &'a RawValue<S>) -> Result<Self, Self::Error> {
        match &value.0 {
            RawValueInner::TriggerRelative(prop) => Ok(Self::Relative(prop)),
            RawValueInner::DtUtc(prop) => Ok(Self::Absolute(prop)),
            _ => Err(()),
        }
    }
}

impl<'a, S> TryFrom<&'a mut RawValue<S>> for TriggerPropMut<'a, S> {
    type Error = ();

    fn try_from(value: &'a mut RawValue<S>) -> Result<Self, Self::Error> {
        match &mut value.0 {
            RawValueInner::TriggerRelative(prop) => Ok(Self::Relative(prop)),
            RawValueInner::DtUtc(prop) => Ok(Self::Absolute(prop)),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{date, model::primitive::Value, parser::escaped::AsEscaped, time};

    use super::*;

    #[test]
    fn basic_property_table_usage() {
        let mut props = PropertyTable::new();

        let uid = RawValue::from(Prop::from_value(Uid("some-identifier")));
        let dtstamp = RawValue::from(Prop::from_value(DateTime {
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
    fn property_table_get_known() {
        let mut props: PropertyTable<&'static str> = PropertyTable::new();
        let action = Prop::from_value(AudioAction);
        props.insert_known(StaticProp::Action, RawValue::from(action));
        dbg![props.get_known(StaticProp::Action)];

        let prop: Prop<_, AudioAction> = props
            .remove_known(StaticProp::Action)
            .unwrap()
            .try_into()
            .unwrap();
        dbg![prop];
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
