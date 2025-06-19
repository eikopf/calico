//! iCalendar properties.

use std::collections::HashMap;

use super::primitive::{
    CalendarUserType, DisplayType, FeatureType, FormatType, FreeBusyType,
    ImageData, Language, ParticipationRole, ParticipationStatus,
    RelationshipType, ThisAndFuture, TriggerRelation, UriString,
};

/// An ordinary textual property.
pub type TextProp = Prop<Box<str>, TextParams>;

/// An ordinary image property.
pub type ImageProp = Prop<ImageData, ImageParams>;

/// A conference property.
pub type ConfProp = Prop<UriString, ConfParams>;

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
#[derive(Debug, Clone)]
pub struct AttachParams {
    pub format_type: Option<FormatType>,
}

/// The parameters associated with the `ATTENDEE` property.
#[derive(Debug, Clone)]
pub struct AttendeeParams<S = Box<str>, U = UriString> {
    pub language: Option<Language<S>>,
    pub calendar_user_type: Option<CalendarUserType<S>>,
    pub group_or_list_membership: Option<Box<[U]>>,
    pub participation_role: Option<ParticipationRole<S>>,
    pub participation_status: Option<ParticipationStatus<S>>,
    pub rsvp_expectation: Option<bool>,
    pub delegatees: Option<Box<[U]>>,
    pub delegators: Option<Box<[U]>>,
    pub sent_by: Option<U>,
    pub common_name: Option<S>,
    pub directory_entry_reference: Option<U>,
}

/// The parameters associated with the `ORGANIZER` property.
#[derive(Debug, Clone)]
pub struct OrganizerParams<S = Box<str>, U = UriString> {
    pub language: Option<Language<S>>,
    pub sent_by: Option<U>,
    pub common_name: Option<S>,
    pub directory_entry_reference: Option<U>,
}

/// The parameters associated with the `RECURRENCE-ID` property.
#[derive(Debug, Clone, Copy)]
pub struct RecurrenceIdParams<S = Box<str>> {
    pub tz_id: Option<S>,
    pub recurrence_identifier_range: Option<ThisAndFuture>,
}

/// The parameters associated with the `RELATED-TO` property.
#[derive(Debug, Clone, Copy)]
pub struct RelTypeParams<S = Box<str>> {
    pub relationship_type: Option<RelationshipType<S>>,
}

/// A variant of [`TextParams`] without the `ALTREP` parameter.
#[derive(Debug, Clone, Copy)]
pub struct LangParams<S = Box<str>> {
    pub language: Option<Language<S>>,
}

/// The parameters associated with several date and time properties.
#[derive(Debug, Clone, Copy)]
pub struct DtParams<S = Box<str>> {
    pub tz_id: Option<S>,
}

/// The parameters associated with the `RDATE` property.
#[derive(Debug, Clone, Copy)]
pub struct RDateParams<S = Box<str>> {
    pub tz_id: Option<S>,
}

/// The parameters associated with the `TRIGGER` property.
#[derive(Debug, Clone, Copy)]
pub struct TriggerParams {
    pub trigger_relation: Option<TriggerRelation>,
}

#[derive(Debug, Clone, Copy)]
pub struct FBTypeParams<S = Box<str>> {
    pub fb_type: Option<FreeBusyType<S>>,
}

/// The parameters usually associated with text values.
#[derive(Debug, Clone, Copy)]
pub struct TextParams<S = Box<str>, U = UriString> {
    pub language: Option<Language<S>>,
    pub alternate_representation: Option<U>,
}

/// The parameters usually associated with image data.
#[derive(Debug, Clone)]
pub struct ImageParams<S = Box<str>, U = UriString> {
    pub format_type: Option<FormatType>,
    pub display: Option<DisplayType<S>>,
    pub alternate_representation: Option<U>,
}

/// The parameters associated with the CONFERENCE property.
#[derive(Debug, Clone)]
pub struct ConfParams<S = Box<str>> {
    pub feature_type: Vec<FeatureType<S>>,
    pub label: Option<S>,
    pub language: Option<Language<S>>,
}
