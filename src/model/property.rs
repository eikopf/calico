//! iCalendar properties.

use std::collections::HashMap;

use crate::parser::parameter::ParamValue;

use super::primitive::{
    CalendarUserType, DisplayType, FeatureType, FormatType, FreeBusyType,
    ImageData, Language, ParticipationRole, ParticipationStatus,
    RelationshipType, Text, ThisAndFuture, TriggerRelation, TzId, Uri,
};

/// An ordinary textual property.
pub type TextProp<S> = Prop<Text<S>, TextParams<S>>;

/// A sequence of textual values with the LANGUAGE parameter.
pub type SeqLangProp<S> = Prop<Box<[Text<S>]>, LangParams<S>>;

/// An ordinary image property.
pub type ImageProp<S> = Prop<ImageData<S>, ImageParams<S>>;

/// A conference property.
pub type ConfProp<S> = Prop<Uri<S>, ConfParams<S>>;

/// A property generic over values and parameters.
#[derive(Debug, Clone)]
pub struct Prop<V, P = ()> {
    pub params: P,
    pub value: V,
    pub extra_params: HashMap<Box<str>, Box<str>>,
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
    pub group_or_list_membership: Option<Box<[Uri<S>]>>,
    pub participation_role: Option<ParticipationRole<S>>,
    pub participation_status: Option<ParticipationStatus<S>>,
    pub rsvp_expectation: Option<bool>,
    pub delegatees: Option<Box<[Uri<S>]>>,
    pub delegators: Option<Box<[Uri<S>]>>,
    pub sent_by: Option<Uri<S>>,
    pub common_name: Option<ParamValue<S>>,
    pub directory_entry_reference: Option<Uri<S>>,
}

/// The parameters associated with the `ORGANIZER` property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrganizerParams<S> {
    pub language: Option<Language<S>>,
    pub sent_by: Option<Uri<S>>,
    pub common_name: Option<ParamValue<S>>,
    pub directory_entry_reference: Option<Uri<S>>,
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

/// The parameters associated with several date and time properties.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DtParams<S> {
    pub tz_id: Option<TzId<S>>,
}

/// The parameters associated with the `TRIGGER` property.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TriggerParams {
    pub trigger_relation: Option<TriggerRelation>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FBTypeParams<S> {
    pub free_busy_type: Option<FreeBusyType<S>>,
}

/// The parameters usually associated with text values.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextParams<S> {
    pub language: Option<Language<S>>,
    pub alternate_representation: Option<Uri<S>>,
}

/// The parameters usually associated with image data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImageParams<S> {
    pub format_type: Option<FormatType<S>>,
    pub display: Option<DisplayType<S>>,
    pub alternate_representation: Option<Uri<S>>,
}

/// The parameters associated with the CONFERENCE property.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConfParams<S> {
    pub feature_type: Vec<FeatureType<S>>,
    pub label: Option<S>,
    pub language: Option<Language<S>>,
}
