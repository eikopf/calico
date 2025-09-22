//! iCalendar properties.

use crate::{
    define_value_type_with_mult,
    model::{
        css::Css3Color,
        parameter::{KnownParam, Params, StructuredDataParams, UnknownParam},
        primitive::{
            AlarmAction, AttachValue, AudioAction, Binary, CalAddress, ClassValue,
            CompletionPercentage, DateTime, DateTimeOrDate, DisplayAction, Duration, EmailAction,
            EventStatus, ExDateSeq, Geo, Gregorian, ImageData, Integer, JournalStatus, Method,
            ParticipantType, Period, PositiveInteger, Priority, ProximityValue, RDateSeq,
            RequestStatus, ResourceType, Status, StyledDescriptionValue, Text, TimeTransparency,
            TodoStatus, TzId, Uid, UnknownAction, UnknownKind, Uri, Utc, UtcOffset, Value, Version,
        },
        rrule::RRule,
        table::{HashCaseless, Key, Table},
    },
    parser::escaped::Equiv,
};

pub type PropertyTable<S> = Table<StaticProp, S, RawPropValue<S>, UnknownPropSeq<S>>;

define_value_type_with_mult! {
#[derive(Debug, Clone)]
pub RawPropValue {
    // PRIMITIVES
    CalAddress(Prop<CalAddress<S>, Params<S>>),
    Color(Prop<Css3Color, Params<S>>),
    DtUtc(Prop<DateTime<Utc>, Params<S>>),
    DtOrDate(Prop<DateTimeOrDate, Params<S>>),
    Duration(Prop<Duration, Params<S>>),
    Integer(Prop<Integer, Params<S>>),
    PositiveInteger(Prop<PositiveInteger, Params<S>>),
    Text(Prop<Text<S>, Params<S>>),
    TextSeq(Prop<Vec<Text<S>>, Params<S>>),
    Uid(Prop<Uid<S>, Params<S>>),
    Uri(Prop<Uri<S>, Params<S>>),
    UtcOffset(Prop<UtcOffset, Params<S>>),
    // ONE-OFFS
    Attach(Prop<AttachValue<S>, Params<S>>),
    Class(Prop<ClassValue<S>, Params<S>>),
    ExDate(Prop<ExDateSeq, Params<S>>),
    FreeBusy(Prop<Vec<Period>, Params<S>>),
    Geo(Prop<Geo, Params<S>>),
    Gregorian(Prop<Gregorian, Params<S>>),
    Image(Prop<ImageData<S>, Params<S>>),
    Method(Prop<Method<S>, Params<S>>),
    ParticipantType(Prop<ParticipantType<S>, Params<S>>),
    Percent(Prop<CompletionPercentage, Params<S>>),
    Priority(Prop<Priority, Params<S>>),
    Proximity(Prop<ProximityValue<S>, Params<S>>),
    RDate(Prop<RDateSeq, Params<S>>),
    ResourceType(Prop<ResourceType<S>, Params<S>>),
    RequestStatus(Prop<RequestStatus<S>, Params<S>>),
    RRule(Prop<Box<RRule>, Params<S>>),
    StyledDescription(Prop<StyledDescriptionValue<S>, Params<S>>),
    Transp(Prop<TimeTransparency, Params<S>>),
    TzId(Prop<TzId<S>, Params<S>>),
    Version(Prop<Version, Params<S>>),
    // STATUS VARIANTS
    Status(Prop<Status, Params<S>>),
    EventStatus(Prop<EventStatus, Params<S>>),
    TodoStatus(Prop<TodoStatus, Params<S>>),
    JournalStatus(Prop<JournalStatus, Params<S>>),
    // ACTION VARIANTS
    Action(Prop<AlarmAction<S>, Params<S>>),
    AudioAction(Prop<AudioAction, Params<S>>),
    DisplayAction(Prop<DisplayAction, Params<S>>),
    EmailAction(Prop<EmailAction, Params<S>>),
    UnknownAction(Prop<UnknownAction<S>, Params<S>>),
    // STRUCTURED DATA VARIANTS
    StructuredDataBinary(Prop<Binary<S>, StructuredDataParams<S>>),
    StructuredDataText(Prop<Text<S>, StructuredDataParams<S>>),
    // OTHER VARIANTS
    AnyTrigger(TriggerProp<S>),
    AnyStructuredData(StructuredDataProp<S>),
}}

impl<'a, S> TryFrom<&'a RawPropValue<S>> for AlarmAction<&'a S> {
    type Error = ();

    fn try_from(value: &'a RawPropValue<S>) -> Result<Self, Self::Error> {
        match &value.0 {
            RawPropValueInner::AudioAction(xs) if xs.is_one() => Ok(Self::Audio),
            RawPropValueInner::DisplayAction(xs) if xs.is_one() => Ok(Self::Display),
            RawPropValueInner::EmailAction(xs) if xs.is_one() => Ok(Self::Email),
            RawPropValueInner::UnknownAction(xs) => match xs.as_one().map(|prop| &prop.value) {
                Some(UnknownAction::Iana(action)) => Ok(Self::Iana(action)),
                Some(UnknownAction::X(action)) => Ok(Self::X(action)),
                None => Err(()),
            },
            _ => Err(()),
        }
    }
}

impl<'a, S> TryFrom<&'a RawPropValue<S>> for TriggerPropRef<'a, S> {
    type Error = ();

    fn try_from(value: &'a RawPropValue<S>) -> Result<Self, Self::Error> {
        value
            .downcast_ref()
            .map(Self::Relative)
            .or_else(|| value.downcast_ref().map(Self::Absolute))
            .or_else(|| {
                value
                    .downcast_ref::<TriggerProp<S>>()
                    .map(TriggerProp::as_ref)
            })
            .ok_or(())
    }
}

impl<'a, S> TryFrom<&'a mut RawPropValue<S>> for TriggerPropMut<'a, S> {
    type Error = ();

    fn try_from(value: &'a mut RawPropValue<S>) -> Result<Self, Self::Error> {
        if value.is::<Prop<Duration, Params<S>>>() {
            value.downcast_mut().map(Self::Relative).ok_or(())
        } else if value.is::<Prop<DateTime<Utc>, Params<S>>>() {
            value.downcast_mut().map(Self::Absolute).ok_or(())
        } else if value.is::<TriggerProp<S>>() {
            value
                .downcast_mut::<TriggerProp<S>>()
                .map(TriggerProp::as_mut)
                .ok_or(())
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownPropSeq<S> {
    pub kind: UnknownKind,
    pub props: Vec<UnknownProp<S>>,
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
    pub const fn as_key(self) -> Key<Self, std::convert::Infallible> {
        Key::Known(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownProp<S> {
    pub value: Box<Value<S>>,
    pub params: Vec<KnownParam<S>>,
    pub unknown_params: Vec<UnknownParam<S>>,
}

/// A property generic over values and parameters.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Prop<V, P> {
    pub params: P,
    pub value: V,
}

impl<V, P> Prop<V, P> {
    pub fn from_value(value: V) -> Self
    where
        P: Default,
    {
        Self {
            value,
            params: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum EventTerminationRef<'a, S> {
    End(&'a Prop<DateTimeOrDate, Params<S>>),
    Duration(&'a Prop<Duration, Params<S>>),
}

impl<'a, S: PartialEq + HashCaseless + Equiv> PartialEq for EventTerminationRef<'a, S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::End(l0), Self::End(r0)) => l0 == r0,
            (Self::Duration(l0), Self::Duration(r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum EventTerminationMut<'a, S> {
    End(&'a mut Prop<DateTimeOrDate, Params<S>>),
    Duration(&'a mut Prop<Duration, Params<S>>),
}

#[derive(Debug, Clone, Copy)]
pub enum TodoTerminationRef<'a, S> {
    Due(&'a Prop<DateTimeOrDate, Params<S>>),
    Duration(&'a Prop<Duration, Params<S>>),
}

#[derive(Debug)]
pub enum TodoTerminationMut<'a, S> {
    Due(&'a mut Prop<DateTimeOrDate, Params<S>>),
    Duration(&'a mut Prop<Duration, Params<S>>),
}

#[derive(Debug, Clone)]
pub enum TriggerProp<S> {
    Relative(Prop<Duration, Params<S>>),
    Absolute(Prop<DateTime<Utc>, Params<S>>),
}

impl<S: PartialEq + HashCaseless + Equiv> PartialEq for TriggerProp<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Relative(l0), Self::Relative(r0)) => l0 == r0,
            (Self::Absolute(l0), Self::Absolute(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl<S> TriggerProp<S> {
    pub fn as_ref(&self) -> TriggerPropRef<'_, S> {
        match self {
            TriggerProp::Relative(prop) => TriggerPropRef::Relative(prop),
            TriggerProp::Absolute(prop) => TriggerPropRef::Absolute(prop),
        }
    }

    pub fn as_mut(&mut self) -> TriggerPropMut<'_, S> {
        match self {
            TriggerProp::Relative(prop) => TriggerPropMut::Relative(prop),
            TriggerProp::Absolute(prop) => TriggerPropMut::Absolute(prop),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TriggerPropRef<'a, S> {
    Relative(&'a Prop<Duration, Params<S>>),
    Absolute(&'a Prop<DateTime<Utc>, Params<S>>),
}

impl<'a, S: PartialEq + HashCaseless + Equiv> PartialEq for TriggerPropRef<'a, S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Relative(l0), Self::Relative(r0)) => l0 == r0,
            (Self::Absolute(l0), Self::Absolute(r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum TriggerPropMut<'a, S> {
    Relative(&'a mut Prop<Duration, Params<S>>),
    Absolute(&'a mut Prop<DateTime<Utc>, Params<S>>),
}

impl<'a, S: PartialEq + HashCaseless + Equiv> PartialEq for TriggerPropMut<'a, S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Relative(l0), Self::Relative(r0)) => l0 == r0,
            (Self::Absolute(l0), Self::Absolute(r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum StructuredDataProp<S> {
    Binary(Prop<Binary<S>, StructuredDataParams<S>>),
    Text(Prop<Text<S>, StructuredDataParams<S>>),
    Uri(Prop<Uri<S>, Params<S>>),
}

impl<S: PartialEq + HashCaseless + Equiv> PartialEq for StructuredDataProp<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Binary(l0), Self::Binary(r0)) => l0 == r0,
            (Self::Text(l0), Self::Text(r0)) => l0 == r0,
            (Self::Uri(l0), Self::Uri(r0)) => l0 == r0,
            _ => false,
        }
    }
}
