//! Parameter types for the object model.

use crate::parser::parameter::ParamValue;

use super::primitive::{
    CalAddress, CalendarUserType, DisplayType, Encoding, FeatureType, FormatType, FreeBusyType,
    Language, ParticipationRole, ParticipationStatus, RelationshipType, TriggerRelation, TzId, Uri,
    ValueType,
};

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
pub enum KnownParam<S> {
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
    Display(DisplayType<S>),
    Email(ParamValue<S>),
    Feature(FeatureType<S>),
    Label(ParamValue<S>),
}

impl<S> KnownParam<S> {
    /// Returns the [`StaticParamName`] corresponding to `self`.
    pub const fn name(&self) -> StaticParamName {
        match self {
            KnownParam::AltRep(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::AlternateTextRepresentation)
            }
            KnownParam::CommonName(_) => StaticParamName::Rfc5545(Rfc5545ParamName::CommonName),
            KnownParam::CUType(_) => StaticParamName::Rfc5545(Rfc5545ParamName::CalendarUserType),
            KnownParam::DelFrom(_) => StaticParamName::Rfc5545(Rfc5545ParamName::Delegators),
            KnownParam::DelTo(_) => StaticParamName::Rfc5545(Rfc5545ParamName::Delegatees),
            KnownParam::Dir(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::DirectoryEntryReference)
            }
            KnownParam::Encoding(_) => StaticParamName::Rfc5545(Rfc5545ParamName::InlineEncoding),
            KnownParam::FormatType(_) => StaticParamName::Rfc5545(Rfc5545ParamName::FormatType),
            KnownParam::FBType(_) => StaticParamName::Rfc5545(Rfc5545ParamName::FreeBusyTimeType),
            KnownParam::Language(_) => StaticParamName::Rfc5545(Rfc5545ParamName::Language),
            KnownParam::Member(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::GroupOrListMembership)
            }
            KnownParam::PartStatus(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::ParticipationStatus)
            }
            KnownParam::RecurrenceIdentifierRange => {
                StaticParamName::Rfc5545(Rfc5545ParamName::RecurrenceIdentifierRange)
            }
            KnownParam::AlarmTrigger(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::AlarmTriggerRelationship)
            }
            KnownParam::RelType(_) => StaticParamName::Rfc5545(Rfc5545ParamName::RelationshipType),
            KnownParam::Role(_) => StaticParamName::Rfc5545(Rfc5545ParamName::ParticipationRole),
            KnownParam::Rsvp(_) => StaticParamName::Rfc5545(Rfc5545ParamName::RsvpExpectation),
            KnownParam::SentBy(_) => StaticParamName::Rfc5545(Rfc5545ParamName::SentBy),
            KnownParam::TzId(_) => StaticParamName::Rfc5545(Rfc5545ParamName::TimeZoneIdentifier),
            KnownParam::Value(_) => StaticParamName::Rfc5545(Rfc5545ParamName::ValueDataType),
            KnownParam::Display(_) => StaticParamName::Rfc7986(Rfc7986ParamName::Display),
            KnownParam::Email(_) => StaticParamName::Rfc7986(Rfc7986ParamName::Email),
            KnownParam::Feature(_) => StaticParamName::Rfc7986(Rfc7986ParamName::Feature),
            KnownParam::Label(_) => StaticParamName::Rfc7986(Rfc7986ParamName::Label),
        }
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

/// An X-name parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct XParam<S> {
    pub name: S,
    pub value: Box<[ParamValue<S>]>,
}

/// A statically known property parameter name from RFC 5545.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc5545ParamName {
    /// RFC 5545 §3.2.1 (ALTREP)
    AlternateTextRepresentation,
    /// RFC 5545 §3.2.2 (CN)
    CommonName,
    /// RFC 5545 §3.2.3 (CUTYPE)
    CalendarUserType,
    /// RFC 5545 §3.2.4 (DELEGATED-FROM)
    Delegators,
    /// RFC 5545 §3.2.5 (DELEGATED-TO)
    Delegatees,
    /// RFC 5545 §3.2.6 (DIR)
    DirectoryEntryReference,
    /// RFC 5545 §3.2.7 (ENCODING)
    InlineEncoding,
    /// RFC 5545 §3.2.8 (FMTTYPE)
    FormatType,
    /// RFC 5545 §3.2.9 (FBTYPE)
    FreeBusyTimeType,
    /// RFC 5545 §3.2.10 (LANGUAGE)
    Language,
    /// RFC 5545 §3.2.11 (MEMBER)
    GroupOrListMembership,
    /// RFC 5545 §3.2.12 (PARTSTAT)
    ParticipationStatus,
    /// RFC 5545 §3.2.13 (RANGE)
    RecurrenceIdentifierRange,
    /// RFC 5545 §3.2.14 (RELATED)
    AlarmTriggerRelationship,
    /// RFC 5545 §3.2.15 (RELTYPE)
    RelationshipType,
    /// RFC 5545 §3.2.16 (ROLE)
    ParticipationRole,
    /// RFC 5545 §3.2.17 (RSVP)
    RsvpExpectation,
    /// RFC 5545 §3.2.18 (SENT-BY)
    SentBy,
    /// RFC 5545 §3.2.19 (TZID)
    TimeZoneIdentifier,
    /// RFC 5545 §3.2.20 (VALUE)
    ValueDataType,
}

/// A statically known property parameter name from RFC 7986.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Rfc7986ParamName {
    /// RFC 7986 §6.1 (DISPLAY)
    Display,
    /// RFC 7986 §6.2 (EMAIL)
    Email,
    /// RFC 7986 §6.3 (FEATURE)
    Feature,
    /// RFC 7986 §6.4 (LABEL)
    Label,
}

/// A property parameter name from RFC 5545, RFC 7986, or an arbitrary token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamName<S> {
    Rfc5545(Rfc5545ParamName),
    Rfc7986(Rfc7986ParamName),
    Iana(S),
    X(S),
}

impl<S> ParamName<S> {
    /// Returns `true` if the param name is [`Rfc5545`].
    ///
    /// [`Rfc5545`]: ParamName::Rfc5545
    #[must_use]
    pub fn is_rfc5545(&self) -> bool {
        matches!(self, Self::Rfc5545(..))
    }

    /// Returns `true` if the param name is [`Rfc7986`].
    ///
    /// [`Rfc7986`]: ParamName::Rfc7986
    #[must_use]
    pub fn is_rfc7986(&self) -> bool {
        matches!(self, Self::Rfc7986(..))
    }

    /// Returns `true` if the param name is [`Iana`].
    ///
    /// [`Iana`]: ParamName::Iana
    #[must_use]
    pub fn is_iana(&self) -> bool {
        matches!(self, Self::Iana(..))
    }

    /// Returns `true` if the param name is [`X`].
    ///
    /// [`X`]: ParamName::X
    #[must_use]
    pub fn is_x(&self) -> bool {
        matches!(self, Self::X(..))
    }
}

/// A static property parameter name from RFC 5545 or RFC 7986.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StaticParamName {
    Rfc5545(Rfc5545ParamName),
    Rfc7986(Rfc7986ParamName),
}

impl StaticParamName {
    /// Returns `true` if the static param name is [`Rfc5545`].
    ///
    /// [`Rfc5545`]: StaticParamName::Rfc5545
    #[must_use]
    pub fn is_rfc5545(&self) -> bool {
        matches!(self, Self::Rfc5545(..))
    }

    /// Returns `true` if the static param name is [`Rfc7986`].
    ///
    /// [`Rfc7986`]: StaticParamName::Rfc7986
    #[must_use]
    pub fn is_rfc7986(&self) -> bool {
        matches!(self, Self::Rfc7986(..))
    }
}
