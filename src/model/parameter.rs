//! Property parameter types for the object model.
//!
//! # Value Types
//! The VALUE parameter denotes the value type of the property on which it occurs. When it is
//! absent, this is because the relevant property has a default value type; hence we can say that
//! all properties always have a value type. Moreover, since it governs the allowed property value,
//! it can always be inferred by examining the property value.
//!
//! It would be redundant to retain the value type as a parameter when it is already present as
//! part of a property value, and so [`Params`] and its variants do not include the value type.
//!
//! # ORDER
//! RFC 9073 §5.1 introduced the ORDER property parameter, with text providing that it "MUST NOT be
//! applied to a property that does not allow multiple instances." As such, we are forced to draw
//! a distinction between the parameters of properties which can occur at most once and those of
//! properties which may occur an arbitrary number of times.
//!
//! This also introduces some ambiguity. Consider the ATTACH property (RFC 5545 §3.8.1.1), which
//! may occur several times in multiple components but only once in VALARM components with the
//! AUDIO action. So how should we handle this? The language of RFC 9073 seems to indicate that the
//! permissibility of ORDER is decided on a per-property basis, but we cannot decide whether ORDER
//! is permissible in this case unless we know the enclosing component. This problem applies in
//! general to any property whose multiplicity depends on the component it occurs within.
//!
//! To sidestep this issue entirely, as well as to make the overall parser more robust, we admit
//! the ORDER parameter for all properties. It is the business of the renderer to decide how ORDER
//! should be handled when it appears in an impermissible context.
//!
//! # Multiplicity
//! Whereas some properties (e.g. CATEGORIES, RFC 5545 §3.8.1.2) may appear multiple times on the
//! same component, no property parameters have ever been defined with the ability to appear more
//! than once on the same property. This is perfectly fine for the statically-known parameters,
//! but for unknown IANA-registered and extension parameters it poses a problem: how should we
//! handle instances where the same parameter name appears twice on a given property?
//!
//! More concretely, consider an example like `DTSTART;X-FOO=foo;X-FOO=bar:20081006`. This is
//! well-formed with respect to the grammar defined by RFC 5545,[^rfc-5545-intention] but it isn't
//! clear what it *means* with respect to the iCalendar object model. Should it be equivalent to a
//! comma-separated list (i.e. `X-FOO=foo,bar`), or should one of the values take precedence over
//! the other? If so, which one?
//!
//! [`ical.js`](https://github.com/kewisch/ical.js) uses a precedence system, always taking the
//! last occurrence as the actual value. It's a little harder to determine what
//! [`libical`](https://github.com/libical/libical) does, but we can notice that the
//! data in `design-data/ical-parameters.csv` marks "X" parameters as not being multivalued (in
//! contrast to parameters like MEMBER, whch have the `is_multivalued` flag set); the same is also
//! true for "IANA" parameters.
//!
//! TODO: explain what calico actually does here
//!
//! [^rfc-5545-intention]: The relevant grammar fragment from RFC 5545 is almost always rendered as
//! `*(";" other-param)` where `other-param` encompasses the rules for IANA and extension
//! parameters. I suspect that this was intended to neatly admit repetitions of parameters with
//! *distinct* names, but it also obviously allows the same parameter to occur several times.

use paste::paste;

use crate::{define_value_type, model::primitive::ThisAndFuture, parser::escaped::Equiv};

use super::{
    primitive::{
        CalAddress, CalendarUserType, DisplayType, Encoding, FeatureType, FormatType, FreeBusyType,
        Language, ParticipationRole, ParticipationStatus, PositiveInteger, RelationshipType,
        TriggerRelation, TzId, UnknownKind, Uri, ValueType,
    },
    table::{HashCaseless, Item, Table},
};

macro_rules! define_parameter_type {
    ($(#[$m:meta])* $v:vis $name:ident { $($tail:tt)* }) => {
        $(#[$m])*
        $v struct $name<S>(ParameterTable<S>);

        impl<S> $name<S> {
            pub fn capacity(&self) -> usize {
                self.0.capacity()
            }

            pub fn len(&self) -> usize {
                self.0.len()
            }

            pub fn is_empty(&self) -> bool {
                self.0.is_empty()
            }
        }

        impl<S> $name<S> where S: HashCaseless {
            pub(crate) fn get_known_raw(&self, key: StaticParam) -> Option<&RawParamValue<S>> {
                self.0.get_known(key)
            }

            pub(crate) fn get_known_raw_mut(&mut self, key: StaticParam) -> Option<&mut RawParamValue<S>> {
                self.0.get_known_mut(key)
            }

            pub fn get_unknown<K: HashCaseless + Equiv<S>>(&self, key: K) -> Option<&UnknownParamValue<S>> {
                self.0.get_unknown(key)
            }

            pub fn get_unknown_mut<K: HashCaseless + Equiv<S>>(&mut self, key: K) -> Option<&mut UnknownParamValue<S>> {
                self.0.get_unknown_mut(key)
            }

            define_methods!({$($tail)*});
        }
    };

}

/// A helper macro for [`define_parameter_type`] that creates accessor methods for each of "fields."
/// Each field has the form `(name, key) <sep> ty` where <sep> may either be `?` or `!`. If it is
/// `?`, then the field is _optional_; if it is `!` then the field is _mandatory_. Aside from the
/// difference in return types, the only notable difference is that mandatory fields cannot be
/// removed, only modified or replaced.
macro_rules! define_methods {
    // optional
    ({$(#[$m:meta])* ($name:ident, $key:ident) ? $ret_ty:ty $(, $($tail:tt)*)?}) => {
        paste! {
            $(#[$m])*
            pub fn $name(&self) -> Option<&$ret_ty> {
                self.get_known_raw(StaticParam::$key)
                    .map(|raw_value| raw_value.try_into().unwrap())
            }

            $(#[$m])*
            pub fn [<$name _mut>](&mut self) -> Option<&mut $ret_ty> {
                self.get_known_raw_mut(StaticParam::$key)
                    .map(|raw_value| raw_value.try_into().unwrap())
            }

            $(#[$m])*
            pub fn [<remove_ $name>](&mut self) -> Option<$ret_ty> {
                self.0.remove_known(StaticParam::$key)
                    .map(|raw_value| raw_value.try_into().unwrap())
            }

            $(#[$m])*
            pub fn [<set_ $name>](&mut self, value: $ret_ty) -> Option<$ret_ty> where S: Equiv {
                self.0
                    .insert_known(StaticParam::$key, RawParamValue::from(value))
                    .map(|raw_value| raw_value.try_into().unwrap())
            }
        }

        $(define_methods!({$($tail)*});)?
    };
    // required
    ({$(#[$m:meta])* ($name:ident, $key:ident) ! $ret_ty:ty $(, $($tail:tt)*)?}) => {
        paste! {
            $(#[$m])*
            pub fn $name(&self) -> &$ret_ty {
                self.get_known_raw(StaticParam::$key)
                    .map(|raw_value| raw_value.try_into().unwrap())
                    .unwrap()
            }

            $(#[$m])*
            pub fn [<$name _mut>](&mut self) -> &mut $ret_ty {
                self.get_known_raw_mut(StaticParam::$key)
                    .map(|raw_value| raw_value.try_into().unwrap())
                    .unwrap()
            }

            $(#[$m])*
            pub fn [<replace_ $name>](&mut self, value: $ret_ty) -> $ret_ty where S: Equiv {
                self.0
                    .insert_known(StaticParam::$key, RawParamValue::from(value))
                    .map(|raw_value| raw_value.try_into().unwrap())
                    .unwrap()
            }
        }

        $(define_methods!({$($tail)*});)?
    };
    ({$(,)?}) => {};
}

// TODO: write a macro for defining Params in the vein of the vec macro.

define_parameter_type! {
    /// The parameters of the STRUCTURED-DATA property when its value type is TEXT or BINARY. The
    /// `format_type` and `schema` fields are mandatory, while all other fields are optional.
    #[derive(Debug, Clone)]
    pub StructuredDataParams {
        // RFC 5545
        (alternate_representation, AltRep) ? Uri<S>,
        (common_name, CommonName) ? ParamValue<S>,
        (calendar_user_type, CalUserType) ? CalendarUserType<S>,
        (delegated_from, DelFrom) ? Vec<CalAddress<S>>,
        (delegated_to, DelTo) ? Vec<CalAddress<S>>,
        (directory_reference, Dir) ? Uri<S>,
        (inline_encoding, Encoding) ? Encoding,
        (format_type, FormatType) ! FormatType<S>,
        (free_busy_type, FreeBusyType) ? FreeBusyType<S>,
        (language, Language) ? Language<S>,
        (membership, Member) ? Vec<CalAddress<S>>,
        (participation_status, PartStat) ? ParticipationStatus<S>,
        (recurrence_range, Range) ? ThisAndFuture,
        (trigger_relationship, Related) ? TriggerRelation,
        (relationship_type, RelType) ? RelationshipType<S>,
        (participation_role, Role) ? ParticipationRole<S>,
        (rsvp_expectation, Rsvp) ? bool,
        (sent_by, SentBy) ? Uri<S>,
        (tz_id, TzId) ? TzId<S>,

        // RFC 7986
        (display_type, Display) ? DisplayType<S>,
        (email, Email) ? ParamValue<S>,
        (feature_type, Feature) ? FeatureType<S>,
        (label, Label) ? ParamValue<S>,

        // RFC 9073
        (order, Order) ? PositiveInteger,
        (schema, Schema) ! Uri<S>,
        (derived, Derived) ? bool,
    }
}

impl<S> StructuredDataParams<S> {
    pub fn new(format_type: FormatType<S>, schema: Uri<S>) -> Self
    where
        S: HashCaseless + Equiv,
    {
        let mut params = Params::with_capacity(2);
        params.set_format_type(format_type);
        params.set_schema(schema);
        Self(params.0)
    }
}

define_parameter_type! {
    /// A table of optional property parameters.
    #[derive(Debug, Clone)]
    pub Params {
        // RFC 5545
        (alternate_representation, AltRep) ? Uri<S>,
        (common_name, CommonName) ? ParamValue<S>,
        (calendar_user_type, CalUserType) ? CalendarUserType<S>,
        (delegated_from, DelFrom) ? Vec<CalAddress<S>>,
        (delegated_to, DelTo) ? Vec<CalAddress<S>>,
        (directory_reference, Dir) ? Uri<S>,
        (inline_encoding, Encoding) ? Encoding,
        (format_type, FormatType) ? FormatType<S>,
        (free_busy_type, FreeBusyType) ? FreeBusyType<S>,
        (language, Language) ? Language<S>,
        (membership, Member) ? Vec<CalAddress<S>>,
        (participation_status, PartStat) ? ParticipationStatus<S>,
        (recurrence_range, Range) ? ThisAndFuture,
        (trigger_relationship, Related) ? TriggerRelation,
        (relationship_type, RelType) ? RelationshipType<S>,
        (participation_role, Role) ? ParticipationRole<S>,
        (rsvp_expectation, Rsvp) ? bool,
        (sent_by, SentBy) ? Uri<S>,
        (tz_id, TzId) ? TzId<S>,

        // RFC 7986
        (display_type, Display) ? DisplayType<S>,
        (email, Email) ? ParamValue<S>,
        (feature_type, Feature) ? FeatureType<S>,
        (label, Label) ? ParamValue<S>,

        // RFC 9073
        (order, Order) ? PositiveInteger,
        (schema, Schema) ? Uri<S>,
        (derived, Derived) ? bool,
    }
}

impl<S> From<StructuredDataParams<S>> for Params<S> {
    fn from(value: StructuredDataParams<S>) -> Self {
        Self(value.0)
    }
}

impl<S> Default for Params<S> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<S> Params<S> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self(Table::with_capacity(capacity))
    }
}

pub type ParameterTable<S> = Table<StaticParam, S, RawParamValue<S>, UnknownParamValue<S>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Param<S> {
    Known(KnownParam<S>),
    Unknown(UnknownParam<S>),
}

impl<S> Param<S> {
    pub fn try_into_unknown(self) -> Result<UnknownParam<S>, Self> {
        if let Self::Unknown(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_known(self) -> Result<KnownParam<S>, Self> {
        if let Self::Known(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpcastParamValue<S> {
    ValueType(ValueType<S>),
    RawValue(RawParamValue<S>),
}

define_value_type! {
#[derive(Debug, Clone, PartialEq, Eq)]
pub RawParamValue(RawValueInner) {
    Bool(bool),
    CalAddress(CalAddress<S>),
    CalAddressSeq(Vec<CalAddress<S>>),
    CUType(CalendarUserType<S>),
    DisplayType(DisplayType<S>),
    Encoding(Encoding),
    FBType(FreeBusyType<S>),
    FeatureType(FeatureType<S>),
    FormatType(FormatType<S>),
    Language(Language<S>),
    ParamValue(ParamValue<S>),
    PartStatus(ParticipationStatus<S>),
    PositiveInteger(PositiveInteger),
    RelType(RelationshipType<S>),
    Role(ParticipationRole<S>),
    ThisAndFuture(ThisAndFuture),
    TrigRel(TriggerRelation),
    TzId(TzId<S>),
    Uri(Uri<S>),
}}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KnownParam<S> {
    // RFC 5545 PROPERTY PARAMETERS
    AltRep(Uri<S>),
    CommonName(ParamValue<S>),
    CUType(CalendarUserType<S>),
    DelFrom(Box<[CalAddress<S>]>),
    DelTo(Box<[CalAddress<S>]>),
    Dir(Uri<S>),
    Encoding(Encoding),
    FormatType(FormatType<S>),
    FBType(FreeBusyType<S>),
    Language(Language<S>),
    Member(Box<[CalAddress<S>]>),
    PartStatus(ParticipationStatus<S>),
    RecurrenceIdentifierRange,
    AlarmTrigger(TriggerRelation),
    RelType(RelationshipType<S>),
    Role(ParticipationRole<S>),
    Rsvp(bool),
    SentBy(Uri<S>),
    TzId(TzId<S>),
    Value(ValueType<S>),
    // RFC 7986 PROPERTY PARAMETERS
    Display(DisplayType<S>),
    Email(ParamValue<S>),
    Feature(FeatureType<S>),
    Label(ParamValue<S>),
    // RFC 9073 PROPERTY PARAMETERS
    Order(PositiveInteger),
    Schema(Uri<S>),
    Derived(bool),
}

impl<S> KnownParam<S> {
    pub const fn name(&self) -> StaticParam {
        match self {
            KnownParam::AltRep(_) => StaticParam::AltRep,
            KnownParam::CommonName(_) => StaticParam::CommonName,
            KnownParam::CUType(_) => StaticParam::CalUserType,
            KnownParam::DelFrom(_) => StaticParam::DelFrom,
            KnownParam::DelTo(_) => StaticParam::DelTo,
            KnownParam::Dir(_) => StaticParam::Dir,
            KnownParam::Encoding(_) => StaticParam::Encoding,
            KnownParam::FormatType(_) => StaticParam::FormatType,
            KnownParam::FBType(_) => StaticParam::FreeBusyType,
            KnownParam::Language(_) => StaticParam::Language,
            KnownParam::Member(_) => StaticParam::Member,
            KnownParam::PartStatus(_) => StaticParam::PartStat,
            KnownParam::RecurrenceIdentifierRange => StaticParam::Range,
            KnownParam::AlarmTrigger(_) => StaticParam::Related,
            KnownParam::RelType(_) => StaticParam::RelType,
            KnownParam::Role(_) => StaticParam::Role,
            KnownParam::Rsvp(_) => StaticParam::Rsvp,
            KnownParam::SentBy(_) => StaticParam::SentBy,
            KnownParam::TzId(_) => StaticParam::TzId,
            KnownParam::Value(_) => StaticParam::Value,
            KnownParam::Display(_) => StaticParam::Display,
            KnownParam::Email(_) => StaticParam::Email,
            KnownParam::Feature(_) => StaticParam::Feature,
            KnownParam::Label(_) => StaticParam::Label,
            KnownParam::Order(_) => StaticParam::Order,
            KnownParam::Schema(_) => StaticParam::Schema,
            KnownParam::Derived(_) => StaticParam::Derived,
        }
    }

    pub fn upcast(self) -> UpcastParamValue<S> {
        UpcastParamValue::RawValue(match self {
            KnownParam::AltRep(uri)
            | KnownParam::Dir(uri)
            | KnownParam::SentBy(uri)
            | KnownParam::Schema(uri) => uri.into(),
            KnownParam::CommonName(param_value)
            | KnownParam::Email(param_value)
            | KnownParam::Label(param_value) => param_value.into(),
            KnownParam::CUType(calendar_user_type) => calendar_user_type.into(),
            KnownParam::DelFrom(cal_addresses)
            | KnownParam::DelTo(cal_addresses)
            | KnownParam::Member(cal_addresses) => cal_addresses.into_vec().into(),
            KnownParam::Encoding(encoding) => encoding.into(),
            KnownParam::FormatType(format_type) => format_type.into(),
            KnownParam::FBType(free_busy_type) => free_busy_type.into(),
            KnownParam::Language(language) => language.into(),
            KnownParam::PartStatus(participation_status) => participation_status.into(),
            KnownParam::RecurrenceIdentifierRange => ThisAndFuture.into(),
            KnownParam::AlarmTrigger(trigger_relation) => trigger_relation.into(),
            KnownParam::RelType(relationship_type) => relationship_type.into(),
            KnownParam::Role(participation_role) => participation_role.into(),
            KnownParam::Rsvp(value) | KnownParam::Derived(value) => value.into(),
            KnownParam::TzId(tz_id) => tz_id.into(),
            KnownParam::Value(value_type) => return UpcastParamValue::ValueType(value_type),
            KnownParam::Display(display_type) => display_type.into(),
            KnownParam::Feature(feature_type) => feature_type.into(),
            KnownParam::Order(non_zero) => non_zero.into(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnknownParam<S> {
    Iana {
        name: S,
        value: Box<[ParamValue<S>]>,
    },
    X {
        name: S,
        value: Box<[ParamValue<S>]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownParamValue<S> {
    pub kind: UnknownKind,
    pub values: Vec<ParamValue<S>>,
}

impl<S> UnknownParam<S> {
    pub(crate) fn into_table_item<K1, V1>(self) -> Item<K1, S, V1, UnknownParamValue<S>> {
        let (key, value) = match self {
            UnknownParam::Iana { name, value } => (
                name,
                UnknownParamValue {
                    kind: UnknownKind::Iana,
                    values: value.into_vec(),
                },
            ),
            UnknownParam::X { name, value } => (
                name,
                UnknownParamValue {
                    kind: UnknownKind::X,
                    values: value.into_vec(),
                },
            ),
        };

        Item::Unknown { key, value }
    }
}

/// A statically known parameter name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StaticParam {
    // RFC 5545
    /// RFC 5545 §3.2.1 (ALTREP)
    AltRep,
    /// RFC 5545 §3.2.2 (CN)
    CommonName,
    /// RFC 5545 §3.2.3 (CUTYPE)
    CalUserType,
    /// RFC 5545 §3.2.4 (DELEGATED-FROM)
    DelFrom,
    /// RFC 5545 §3.2.5 (DELEGATED-TO)
    DelTo,
    /// RFC 5545 §3.2.6 (DIR)
    Dir,
    /// RFC 5545 §3.2.7 (ENCODING)
    Encoding,
    /// RFC 5545 §3.2.8 (FMTTYPE)
    FormatType,
    /// RFC 5545 §3.2.9 (FBTYPE)
    FreeBusyType,
    /// RFC 5545 §3.2.10 (LANGUAGE)
    Language,
    /// RFC 5545 §3.2.11 (MEMBER)
    Member,
    /// RFC 5545 §3.2.12 (PARTSTAT)
    PartStat,
    /// RFC 5545 §3.2.13 (RANGE)
    Range,
    /// RFC 5545 §3.2.14 (RELATED)
    Related,
    /// RFC 5545 §3.2.15 (RELTYPE)
    RelType,
    /// RFC 5545 §3.2.16 (ROLE)
    Role,
    /// RFC 5545 §3.2.17 (RSVP)
    Rsvp,
    /// RFC 5545 §3.2.18 (SENT-BY)
    SentBy,
    /// RFC 5545 §3.2.19 (TZID)
    TzId,
    /// RFC 5545 §3.2.20 (VALUE)
    Value,

    // RFC 7986
    /// RFC 7986 §6.1 (DISPLAY)
    Display,
    /// RFC 7986 §6.2 (EMAIL)
    Email,
    /// RFC 7986 §6.3 (FEATURE)
    Feature,
    /// RFC 7986 §6.4 (LABEL)
    Label,

    // RFC 9073
    /// RFC 9073 §5.1 (ORDER)
    Order,
    /// RFC 9073 §5.2 (SCHEMA)
    Schema,
    /// RFC 9073 §5.3 (DERIVED)
    Derived,
}

/// A property parameter name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamName<S> {
    Known(StaticParam),
    Iana(S),
    X(S),
}

/// A parameter value string, which may either be [`Safe`] or [`Quoted`].
///
/// [`Safe`]: ParamValue::Safe
/// [`Quoted`]: ParamValue::Quoted
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamValue<S> {
    Safe(S),
    Quoted(S),
}

impl<S> ParamValue<S> {
    /// Returns `true` if the param value is [`Safe`].
    ///
    /// [`Safe`]: ParamValue::Safe
    #[must_use]
    pub fn is_safe(&self) -> bool {
        matches!(self, Self::Safe(..))
    }

    /// Returns `true` if the param value is [`Quoted`].
    ///
    /// [`Quoted`]: ParamValue::Quoted
    #[must_use]
    pub fn is_quoted(&self) -> bool {
        matches!(self, Self::Quoted(..))
    }

    pub fn as_str(&self) -> &str
    where
        S: AsRef<str>,
    {
        match self {
            ParamValue::Safe(s) => s.as_ref(),
            ParamValue::Quoted(s) => s.as_ref(),
        }
    }

    pub fn as_safe(&self) -> Option<&S> {
        if let Self::Safe(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_quoted(&self) -> Option<&S> {
        if let Self::Quoted(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_any_params_usage() {
        let mut params = Params::default();
        params.set_rsvp_expectation(true);
        params.set_inline_encoding(Encoding::Base64);
        params.0.insert_unknown(
            "X-FOO",
            UnknownParamValue {
                kind: UnknownKind::X,
                values: vec![ParamValue::Safe("bar")],
            },
        );

        assert_eq!(params.rsvp_expectation(), Some(&true));
        assert_eq!(params.inline_encoding(), Some(&Encoding::Base64));
        assert_eq!(
            params.get_unknown(b"X-FOO"),
            Some(&UnknownParamValue {
                kind: UnknownKind::X,
                values: vec![ParamValue::Safe("bar")]
            })
        );

        assert!(params.alternate_representation().is_none());
        assert!(params.common_name().is_none());
        assert!(params.tz_id().is_none());
    }
}
