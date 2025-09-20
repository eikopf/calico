//! iCalendar properties.

use crate::define_value_type_with_mult;

use super::{
    css::Css3Color,
    parameter::{KnownParam, ParamValue, UnknownParam},
    primitive::{
        AlarmAction, AttachValue, AudioAction, Binary, CalAddress, CalendarUserType, ClassValue,
        CompletionPercentage, DateTime, DateTimeOrDate, DisplayAction, DisplayType, Duration,
        EmailAction, EventStatus, ExDateSeq, FeatureType, FormatType, FreeBusyType, Geo, ImageData,
        Integer, JournalStatus, Language, Method, ParticipantType, ParticipationRole,
        ParticipationStatus, Period, PositiveInteger, Priority, ProximityValue, RDateSeq,
        RelationshipType, RequestStatus, ResourceType, Status, StyledDescriptionValue, Text,
        ThisAndFuture, TimeTransparency, TodoStatus, TriggerRelation, TzId, Uid, UnknownAction,
        UnknownKind, Uri, Utc, UtcOffset, Value,
    },
    rrule::RRule,
    table::{Key, Table},
};

pub type PropertyTable<S> = Table<StaticProp, S, RawPropValue<S>, UnknownPropSeq<S>>;

define_value_type_with_mult! {
#[derive(Debug, Clone, PartialEq, Eq)]
pub RawPropValue {
    // PRIMITIVES
    CalAddress(Prop<CalAddress<S>>),
    Color(Prop<Css3Color>),
    DtUtc(Prop<DateTime<Utc>>),
    DtOrDateDt(Prop<DateTimeOrDate, DtParams<S>>),
    Duration(Prop<Duration>),
    Integer(Prop<Integer>),
    PositiveInteger(Prop<PositiveInteger>),
    Text(Prop<Text<S>>),
    Uid(Prop<Uid<S>>),
    Unit(Prop<()>),
    Uri(Prop<Uri<S>>),
    UtcOffset(Prop<UtcOffset>),
    // ONE-OFFS
    Attach(Prop<AttachValue<S>, AttachParams<S>>),
    Attendee(Prop<CalAddress<S>, Box<AttendeeParams<S>>>),
    Class(Prop<ClassValue<S>>),
    Conf(Prop<Uri<S>, ConfParams<S>>),
    ExDate(Prop<ExDateSeq, DtParams<S>>),
    FreeBusy(Prop<Vec<Period>, FBTypeParams<S>>),
    Geo(Prop<Geo>),
    Image(Prop<ImageData<S>, ImageParams<S>>),
    Method(Prop<Method<S>>),
    Organizer(Prop<CalAddress<S>, Box<OrganizerParams<S>>>),
    ParticipantType(Prop<ParticipantType<S>>),
    Percent(Prop<CompletionPercentage>),
    Priority(Prop<Priority>),
    Proximity(Prop<ProximityValue<S>>),
    RDate(Prop<RDateSeq, DtParams<S>>),
    RecurId(Prop<DateTimeOrDate, RecurrenceIdParams<S>>),
    RelatedTo(Prop<Text<S>, RelTypeParams<S>>),
    ResourceType(Prop<ResourceType<S>>),
    RequestStatus(Prop<RequestStatus<S>, LangParams<S>>),
    RRule(Prop<Box<RRule>>),
    StyledDescription(Prop<StyledDescriptionValue<S>, StyledDescriptionParams<S>>),
    Transp(Prop<TimeTransparency>),
    TzId(Prop<TzId<S>>),
    // TEXT VARIANTS
    TextSeq(Prop<Vec<Text<S>>>),
    TextText(Prop<Text<S>, TextParams<S>>),
    TextSeqText(Prop<Vec<Text<S>>, TextParams<S>>),
    TextLang(Prop<Text<S>, LangParams<S>>),
    TextSeqLang(Prop<Vec<Text<S>>, LangParams<S>>),
    // STATUS VARIANTS
    Status(Prop<Status>),
    EventStatus(Prop<EventStatus>),
    TodoStatus(Prop<TodoStatus>),
    JournalStatus(Prop<JournalStatus>),
    // ACTION VARIANTS
    Action(Prop<AlarmAction<S>>),
    AudioAction(Prop<AudioAction>),
    DisplayAction(Prop<DisplayAction>),
    EmailAction(Prop<EmailAction>),
    UnknownAction(Prop<UnknownAction<S>>),
    // STRUCTURED DATA VARIANTS
    StructuredDataBinary(Prop<Binary<S>, StructuredDataParams<S>>),
    StructuredDataText(Prop<Text<S>, StructuredDataParams<S>>),
    StructuredDataUri(Prop<Uri<S>, UriStructuredDataParams<S>>),
    // OTHER VARIANTS
    TriggerRelative(Prop<Duration, TriggerParams>),
    AnyTrigger(TriggerProp),
    AnyStructuredData(StructuredDataProp<S>),
}}

impl<'a, S> TryFrom<&'a RawPropValue<S>> for AlarmAction<&'a S> {
    type Error = ();

    fn try_from(value: &'a RawPropValue<S>) -> Result<Self, Self::Error> {
        match &value.0 {
            RawPropValueInner::AudioAction(_) => Ok(Self::Audio),
            RawPropValueInner::DisplayAction(_) => Ok(Self::Display),
            RawPropValueInner::EmailAction(_) => Ok(Self::Email),
            RawPropValueInner::UnknownAction(Prop { value, .. }) => match value.as_ref() {
                UnknownAction::Iana(action) => Ok(Self::Iana(action)),
                UnknownAction::X(action) => Ok(Self::X(action)),
            },
            _ => Err(()),
        }
    }
}

impl<'a, S> TryFrom<&'a RawPropValue<S>> for TriggerPropRef<'a> {
    type Error = ();

    fn try_from(value: &'a RawPropValue<S>) -> Result<Self, Self::Error> {
        match &value.0 {
            RawPropValueInner::TriggerRelative(prop) => Ok(Self::Relative(prop)),
            RawPropValueInner::DtUtc(prop) => Ok(Self::Absolute(prop)),
            _ => Err(()),
        }
    }
}

impl<'a, S> TryFrom<&'a mut RawPropValue<S>> for TriggerPropMut<'a> {
    type Error = ();

    fn try_from(value: &'a mut RawPropValue<S>) -> Result<Self, Self::Error> {
        match &mut value.0 {
            RawPropValueInner::TriggerRelative(prop) => Ok(Self::Relative(prop)),
            RawPropValueInner::DtUtc(prop) => Ok(Self::Absolute(prop)),
            _ => Err(()),
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

// TODO: at some point this default parameter for P needs to be removed

/// A property generic over values and parameters.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Prop<V, P = ()> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriggerProp {
    Relative(Prop<Duration, TriggerParams>),
    Absolute(Prop<DateTime<Utc>>),
}

impl From<Prop<DateTime<Utc>>> for TriggerProp {
    fn from(v: Prop<DateTime<Utc>>) -> Self {
        Self::Absolute(v)
    }
}

impl From<Prop<Duration, TriggerParams>> for TriggerProp {
    fn from(v: Prop<Duration, TriggerParams>) -> Self {
        Self::Relative(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriggerPropRef<'a> {
    Relative(&'a Prop<Duration, TriggerParams>),
    Absolute(&'a Prop<DateTime<Utc>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TriggerPropMut<'a> {
    Relative(&'a mut Prop<Duration, TriggerParams>),
    Absolute(&'a mut Prop<DateTime<Utc>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructuredDataProp<S> {
    Binary(Prop<Binary<S>, StructuredDataParams<S>>),
    Text(Prop<Text<S>, StructuredDataParams<S>>),
    Uri(Prop<Uri<S>, UriStructuredDataParams<S>>),
}

impl<S> From<Prop<Binary<S>, StructuredDataParams<S>>> for StructuredDataProp<S> {
    fn from(v: Prop<Binary<S>, StructuredDataParams<S>>) -> Self {
        Self::Binary(v)
    }
}

impl<S> From<Prop<Text<S>, StructuredDataParams<S>>> for StructuredDataProp<S> {
    fn from(v: Prop<Text<S>, StructuredDataParams<S>>) -> Self {
        Self::Text(v)
    }
}

impl<S> From<Prop<Uri<S>, UriStructuredDataParams<S>>> for StructuredDataProp<S> {
    fn from(v: Prop<Uri<S>, UriStructuredDataParams<S>>) -> Self {
        Self::Uri(v)
    }
}

/// The parameters associated with the `ATTACH` property.
///
/// Note that if the value of the property is a binary value, then there are
/// additional required parameters with statically known values; hence they are
/// elided in this type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttachParams<S> {
    pub format_type: Option<FormatType<S>>,
}

/// The parameters associated with the `ATTENDEE` property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttendeeParams<S> {
    pub language: Option<Language<S>>,
    pub calendar_user_type: Option<CalendarUserType<S>>,
    pub group_or_list_membership: Option<Box<[CalAddress<S>]>>,
    pub participation_role: Option<ParticipationRole<S>>,
    pub participation_status: Option<ParticipationStatus<S>>,
    pub rsvp_expectation: Option<bool>,
    pub delegatees: Option<Box<[CalAddress<S>]>>,
    pub delegators: Option<Box<[CalAddress<S>]>>,
    pub sent_by: Option<Uri<S>>,
    pub common_name: Option<ParamValue<S>>,
    pub directory_entry_reference: Option<Uri<S>>,
    pub email: Option<ParamValue<S>>,
}

impl<S> Default for AttendeeParams<S> {
    fn default() -> Self {
        Self {
            language: Default::default(),
            calendar_user_type: Default::default(),
            group_or_list_membership: Default::default(),
            participation_role: Default::default(),
            participation_status: Default::default(),
            rsvp_expectation: Default::default(),
            delegatees: Default::default(),
            delegators: Default::default(),
            sent_by: Default::default(),
            common_name: Default::default(),
            directory_entry_reference: Default::default(),
            email: Default::default(),
        }
    }
}

/// The parameters associated with the `ORGANIZER` property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrganizerParams<S> {
    pub language: Option<Language<S>>,
    pub sent_by: Option<Uri<S>>,
    pub common_name: Option<ParamValue<S>>,
    pub directory_entry_reference: Option<Uri<S>>,
    pub email: Option<ParamValue<S>>,
}

impl<S> Default for OrganizerParams<S> {
    fn default() -> Self {
        Self {
            language: Default::default(),
            sent_by: Default::default(),
            common_name: Default::default(),
            directory_entry_reference: Default::default(),
            email: Default::default(),
        }
    }
}

/// The parameters associated with the `RECURRENCE-ID` property.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RecurrenceIdParams<S> {
    pub tz_id: Option<TzId<S>>,
    pub recurrence_identifier_range: Option<ThisAndFuture>,
}

/// The parameters associated with the `RELATED-TO` property.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RelTypeParams<S> {
    pub relationship_type: Option<RelationshipType<S>>,
}

/// A variant of [`TextParams`] without the `ALTREP` parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LangParams<S> {
    pub language: Option<Language<S>>,
}

impl<S> Default for LangParams<S> {
    fn default() -> Self {
        Self {
            language: Default::default(),
        }
    }
}

/// The parameters associated with several date and time properties.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DtParams<S> {
    pub tz_id: Option<TzId<S>>,
}

impl<S> Default for DtParams<S> {
    fn default() -> Self {
        Self {
            tz_id: Default::default(),
        }
    }
}

/// The parameters associated with the `TRIGGER` property.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct TriggerParams {
    pub trigger_relation: Option<TriggerRelation>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FBTypeParams<S> {
    pub free_busy_type: Option<FreeBusyType<S>>,
}

impl<S> Default for FBTypeParams<S> {
    fn default() -> Self {
        Self {
            free_busy_type: Default::default(),
        }
    }
}

/// The parameters usually associated with text values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextParams<S> {
    pub language: Option<Language<S>>,
    pub alternate_representation: Option<Uri<S>>,
}

impl<S> Default for TextParams<S> {
    fn default() -> Self {
        Self {
            language: Default::default(),
            alternate_representation: Default::default(),
        }
    }
}

/// The parameters usually associated with image data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImageParams<S> {
    pub format_type: Option<FormatType<S>>,
    pub display: Option<DisplayType<S>>,
    pub alternate_representation: Option<Uri<S>>,
}

impl<S> Default for ImageParams<S> {
    fn default() -> Self {
        Self {
            format_type: Default::default(),
            display: Default::default(),
            alternate_representation: Default::default(),
        }
    }
}

/// The parameters associated with the CONFERENCE property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfParams<S> {
    pub feature_type: Option<FeatureType<S>>,
    pub label: Option<ParamValue<S>>,
    pub language: Option<Language<S>>,
}

impl<S> Default for ConfParams<S> {
    fn default() -> Self {
        Self {
            feature_type: Default::default(),
            label: Default::default(),
            language: Default::default(),
        }
    }
}

/// The parameters associated with the STYLED-DESCRIPTION property (RFC 5545 §6.5).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StyledDescriptionParams<S> {
    pub language: Option<Language<S>>,
    pub alternate_representation: Option<Uri<S>>,
    pub format_type: Option<FormatType<S>>,
}

impl<S> Default for StyledDescriptionParams<S> {
    fn default() -> Self {
        Self {
            language: Default::default(),
            alternate_representation: Default::default(),
            format_type: Default::default(),
        }
    }
}

/// The parameters associated with the STRUCTURED-DATA property (RFC 5545 §6.6) when its value type
/// is TEXT or BINARY.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructuredDataParams<S> {
    pub format_type: FormatType<S>,
    pub schema: Uri<S>,
}

/// The parameters associated with the STRUCTURED-DATA property (RFC 5545 §6.6) when its value type
/// is URI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UriStructuredDataParams<S> {
    pub format_type: Option<FormatType<S>>,
    pub schema: Option<Uri<S>>,
}

impl<S> Default for UriStructuredDataParams<S> {
    fn default() -> Self {
        Self {
            format_type: Default::default(),
            schema: Default::default(),
        }
    }
}
