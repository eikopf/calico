//! iCalendar properties.

use crate::parser::parameter::ParamValue;

use super::{
    parameter::{KnownParam, UnknownParam},
    primitive::{
        Binary, CalAddress, CalendarUserType, DateTime, DisplayType, Duration, FeatureType,
        FormatType, FreeBusyType, Language, ParticipationRole, ParticipationStatus,
        PositiveInteger, RelationshipType, Text, ThisAndFuture, TriggerRelation, TzId, Uri, Utc,
        Value,
    },
};

// NOTE: the ORDER and DERIVED parameters (RFC 9073 §5) can appear on a large number of properties,
// and DERIVED in particular can appear on literally any property. how should this be handled
// architecturally? should Prop get a field corresponding to DERIVED? what if additional "universal"
// parameters of this kind are added?

pub type MultiProp<S, V, P = ()> = Prop<S, V, MultiParams<P>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownProp<S> {
    pub value: Box<Value<S>>,
    pub params: Vec<KnownParam<S>>,
    pub unknown_params: Vec<UnknownParam<S>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnknownPropKind {
    Iana,
    X,
}

/// A property generic over values and parameters.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Prop<S, V, P = ()> {
    pub derived: bool,
    pub params: P,
    pub value: V,
    pub unknown_params: Vec<UnknownParam<S>>,
}

impl<S, V, P> Prop<S, V, P> {
    pub fn from_value(value: V) -> Self
    where
        P: Default,
    {
        Self {
            value,
            derived: Default::default(),
            params: Default::default(),
            unknown_params: Default::default(),
        }
    }

    /// Converts `self` into a [`MultiProp`] with no defined ORDER.
    pub fn into_multi_prop(self) -> MultiProp<S, V, P> {
        let Prop {
            derived,
            params,
            value,
            unknown_params,
        } = self;

        MultiProp {
            derived,
            value,
            unknown_params,
            params: MultiParams {
                order: None,
                known: params,
            },
        }
    }
}

impl<S, V, P> MultiProp<S, V, P> {
    /// Tries to convert `self` into a normal [`Prop`] by unwrapping the [`Self::params`] field.
    /// This will fail if the [`MultiParams::order`] field is not [`None`].
    pub fn try_into_single_prop(self) -> Option<Prop<S, V, P>> {
        let MultiProp {
            derived,
            params:
                MultiParams {
                    order,
                    known: params,
                },
            value,
            unknown_params,
        } = self;

        match order {
            Some(_) => None,
            None => Some(Prop {
                derived,
                params,
                value,
                unknown_params,
            }),
        }
    }
}

/// The parameters of a property which can occur multiple times in the same component, and in
/// particular can therefore include the ORDER parameter (RFC 9073 §5.1).
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct MultiParams<P> {
    /// The value of the ORDER parameter (RFC 9073 §5.1).
    pub order: Option<PositiveInteger>,
    /// The other statically known parameters.
    pub known: P,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriggerMultiProp<S> {
    Relative(MultiProp<S, Duration, TriggerParams>),
    Absolute(MultiProp<S, DateTime<Utc>>),
}

impl<S> From<MultiProp<S, DateTime<Utc>>> for TriggerMultiProp<S> {
    fn from(v: MultiProp<S, DateTime<Utc>>) -> Self {
        Self::Absolute(v)
    }
}

impl<S> From<MultiProp<S, Duration, TriggerParams>> for TriggerMultiProp<S> {
    fn from(v: MultiProp<S, Duration, TriggerParams>) -> Self {
        Self::Relative(v)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriggerPropRef<'a, S> {
    Relative(&'a Prop<S, Duration, TriggerParams>),
    Absolute(&'a Prop<S, DateTime<Utc>>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum TriggerPropMut<'a, S> {
    Relative(&'a mut Prop<S, Duration, TriggerParams>),
    Absolute(&'a mut Prop<S, DateTime<Utc>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructuredDataMultiProp<S> {
    Binary(MultiProp<S, Binary<S>, StructuredDataParams<S>>),
    Text(MultiProp<S, Text<S>, StructuredDataParams<S>>),
    Uri(MultiProp<S, Uri<S>, UriStructuredDataParams<S>>),
}

impl<S> From<MultiProp<S, Uri<S>, UriStructuredDataParams<S>>> for StructuredDataMultiProp<S> {
    fn from(v: MultiProp<S, Uri<S>, UriStructuredDataParams<S>>) -> Self {
        Self::Uri(v)
    }
}

impl<S> From<MultiProp<S, Text<S>, StructuredDataParams<S>>> for StructuredDataMultiProp<S> {
    fn from(v: MultiProp<S, Text<S>, StructuredDataParams<S>>) -> Self {
        Self::Text(v)
    }
}

impl<S> From<MultiProp<S, Binary<S>, StructuredDataParams<S>>> for StructuredDataMultiProp<S> {
    fn from(v: MultiProp<S, Binary<S>, StructuredDataParams<S>>) -> Self {
        Self::Binary(v)
    }
}

/// The parameters which are admissible on every property (assuming we do not know the multiplicity
/// of the property and must conservatively include the ORDER parameter).
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct UniversalParams {
    pub derived: Option<bool>,
    pub order: Option<PositiveInteger>,
}

impl UniversalParams {
    pub const fn is_empty(&self) -> bool {
        self.derived.is_none() && self.order.is_none()
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
