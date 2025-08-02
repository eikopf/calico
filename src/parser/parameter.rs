//! Property parameters.
//!
//! # Grammar
//!
//! The ABNF grammar in RFC 5545 §3.1 defines a parameter as follows:
//!
//! ```custom,{class=language-abnf}
//! param       = param-name "=" param-value *("," param-value)
//! param-name  = iana-token / x-name
//! param-value = paramtext / quoted-string
//! paramtext   = *SAFE-CHAR
//! ```
//!
//! This is a rough general rule, and each `param-name` is associated with a
//! more specific grammar for the corresponding value on the right-hand side of
//! the `=` character.

use winnow::{
    ModalResult, Parser,
    ascii::Caseless,
    combinator::{alt, delimited, opt, repeat, separated, terminated},
    error::ParserError,
    stream::{AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::none_of,
};

use crate::{
    model::primitive::{
        CalAddress, CalendarUserType, DisplayType, Encoding, FeatureType,
        FormatType, FreeBusyType, Language, ParticipationRole,
        ParticipationStatus, RelationshipType, TriggerRelation, TzId, Uri,
        ValueType,
    },
    parser::primitive::{
        alarm_trigger_relationship, bool_caseless, feature_type, format_type,
        free_busy_type, inline_encoding, language, participation_role,
        participation_status, relationship_type, uri, value_type,
    },
};

use super::primitive::{calendar_user_type, display_type, iana_token, x_name};

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
            KnownParam::AltRep(_) => StaticParamName::Rfc5545(
                Rfc5545ParamName::AlternateTextRepresentation,
            ),
            KnownParam::CommonName(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::CommonName)
            }
            KnownParam::CUType(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::CalendarUserType)
            }
            KnownParam::DelFrom(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::Delegators)
            }
            KnownParam::DelTo(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::Delegatees)
            }
            KnownParam::Dir(_) => StaticParamName::Rfc5545(
                Rfc5545ParamName::DirectoryEntryReference,
            ),
            KnownParam::Encoding(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::InlineEncoding)
            }
            KnownParam::FormatType(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::FormatType)
            }
            KnownParam::FBType(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::FreeBusyTimeType)
            }
            KnownParam::Language(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::Language)
            }
            KnownParam::Member(_) => StaticParamName::Rfc5545(
                Rfc5545ParamName::GroupOrListMembership,
            ),
            KnownParam::PartStatus(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::ParticipationStatus)
            }
            KnownParam::RecurrenceIdentifierRange => StaticParamName::Rfc5545(
                Rfc5545ParamName::RecurrenceIdentifierRange,
            ),
            KnownParam::AlarmTrigger(_) => StaticParamName::Rfc5545(
                Rfc5545ParamName::AlarmTriggerRelationship,
            ),
            KnownParam::RelType(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::RelationshipType)
            }
            KnownParam::Role(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::ParticipationRole)
            }
            KnownParam::Rsvp(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::RsvpExpectation)
            }
            KnownParam::SentBy(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::SentBy)
            }
            KnownParam::TzId(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::TimeZoneIdentifier)
            }
            KnownParam::Value(_) => {
                StaticParamName::Rfc5545(Rfc5545ParamName::ValueDataType)
            }
            KnownParam::Display(_) => {
                StaticParamName::Rfc7986(Rfc7986ParamName::Display)
            }
            KnownParam::Email(_) => {
                StaticParamName::Rfc7986(Rfc7986ParamName::Email)
            }
            KnownParam::Feature(_) => {
                StaticParamName::Rfc7986(Rfc7986ParamName::Feature)
            }
            KnownParam::Label(_) => {
                StaticParamName::Rfc7986(Rfc7986ParamName::Label)
            }
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

/// Parses a [`Param`].
pub fn parameter<I, E>(input: &mut I) -> Result<Param<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: Clone + SliceLen,
    E: ParserError<I>,
    CalendarUserType<I::Slice>: Clone,
    FreeBusyType<I::Slice>: Clone,
    ParticipationStatus<I::Slice>: Clone,
{
    /// Parses a single URI delimited by double quotes.
    fn quoted_uri<I, E>(input: &mut I) -> Result<Uri<I::Slice>, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        delimited('"', uri::<_, _, true>, '"').parse_next(input)
    }

    fn quoted_addresses<I, E>(
        input: &mut I,
    ) -> Result<Box<[CalAddress<I::Slice>]>, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        let uris: Vec<_> =
            separated(1.., quoted_uri.map(|Uri(uri)| CalAddress(uri)), ',')
                .parse_next(input)?;
        Ok(uris.into_boxed_slice())
    }

    let name = terminated(param_name, '=').parse_next(input)?;

    match name {
        ParamName::Iana(name) => {
            let value: Vec<_> =
                separated(1.., param_value, ',').parse_next(input)?;
            Ok(Param::Unknown(UnknownParam::Iana {
                name,
                value: value.into_boxed_slice(),
            }))
        }
        ParamName::X(name) => {
            let value: Vec<_> =
                separated(1.., param_value, ',').parse_next(input)?;
            Ok(Param::Unknown(UnknownParam::X {
                name,
                value: value.into_boxed_slice(),
            }))
        }
        ParamName::Rfc5545(name) => match name {
            Rfc5545ParamName::AlternateTextRepresentation => quoted_uri
                .map(KnownParam::AltRep)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::CommonName => param_value
                .map(KnownParam::CommonName)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::CalendarUserType => calendar_user_type
                .map(KnownParam::CUType)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::Delegators => quoted_addresses
                .map(KnownParam::DelFrom)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::Delegatees => quoted_addresses
                .map(KnownParam::DelTo)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::DirectoryEntryReference => quoted_uri
                .map(KnownParam::Dir)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::InlineEncoding => inline_encoding
                .map(KnownParam::Encoding)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::FormatType => format_type
                .map(KnownParam::FormatType)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::FreeBusyTimeType => free_busy_type
                .map(KnownParam::FBType)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::Language => language
                .map(KnownParam::Language)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::GroupOrListMembership => quoted_addresses
                .map(KnownParam::Member)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::ParticipationStatus => participation_status
                .map(KnownParam::PartStatus)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::RecurrenceIdentifierRange => {
                Caseless("THISANDFUTURE")
                    .value(KnownParam::RecurrenceIdentifierRange)
                    .map(Param::Known)
                    .parse_next(input)
            }
            Rfc5545ParamName::AlarmTriggerRelationship => {
                alarm_trigger_relationship
                    .map(KnownParam::AlarmTrigger)
                    .map(Param::Known)
                    .parse_next(input)
            }
            Rfc5545ParamName::RelationshipType => relationship_type
                .map(KnownParam::RelType)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::ParticipationRole => participation_role
                .map(KnownParam::Role)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::RsvpExpectation => bool_caseless
                .map(KnownParam::Rsvp)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::SentBy => quoted_uri
                .map(KnownParam::SentBy)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::TimeZoneIdentifier => {
                (opt('/'), param_value.verify(ParamValue::is_safe))
                    .take()
                    .map(TzId)
                    .map(KnownParam::TzId)
                    .map(Param::Known)
                    .parse_next(input)
            }
            Rfc5545ParamName::ValueDataType => value_type
                .map(KnownParam::Value)
                .map(Param::Known)
                .parse_next(input),
        },
        ParamName::Rfc7986(name) => match name {
            Rfc7986ParamName::Display => display_type
                .map(KnownParam::Display)
                .map(Param::Known)
                .parse_next(input),
            Rfc7986ParamName::Email => param_value
                .map(KnownParam::Email)
                .map(Param::Known)
                .parse_next(input),
            Rfc7986ParamName::Feature => feature_type
                .map(KnownParam::Feature)
                .map(Param::Known)
                .parse_next(input),
            Rfc7986ParamName::Label => param_value
                .map(KnownParam::Label)
                .map(Param::Known)
                .parse_next(input),
        },
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

/// Parses a [`StaticParamName`].
pub fn static_param_name(input: &mut &str) -> ModalResult<StaticParamName> {
    alt((
        rfc5545_param_name.map(StaticParamName::Rfc5545),
        rfc7986_param_name.map(StaticParamName::Rfc7986),
    ))
    .parse_next(input)
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

/// Parses a [`ParamName`].
pub fn param_name<I, E>(input: &mut I) -> Result<ParamName<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    // NOTE: there's an obvious optimisation here where we go character by
    // character until either it must be a known static name, or else it must
    // be an unknown name (or an error). i think regex-automata could help with
    // implementing this, but it's probably unnecessary until we have benchmarks

    alt((
        rfc5545_param_name.map(ParamName::Rfc5545),
        rfc7986_param_name.map(ParamName::Rfc7986),
        x_name.map(ParamName::X),
        iana_token.map(ParamName::Iana),
    ))
    .parse_next(input)
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

/// Parses an [`Rfc5545ParamName`].
pub fn rfc5545_param_name<I, E>(input: &mut I) -> Result<Rfc5545ParamName, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        // RFC 5545
        Caseless("ALTREP").value(Rfc5545ParamName::AlternateTextRepresentation),
        Caseless("CN").value(Rfc5545ParamName::CommonName),
        Caseless("CUTYPE").value(Rfc5545ParamName::CalendarUserType),
        Caseless("DELEGATED-FROM").value(Rfc5545ParamName::Delegators),
        Caseless("DELEGATED-TO").value(Rfc5545ParamName::Delegatees),
        Caseless("DIR").value(Rfc5545ParamName::DirectoryEntryReference),
        Caseless("ENCODING").value(Rfc5545ParamName::InlineEncoding),
        Caseless("FMTTYPE").value(Rfc5545ParamName::FormatType),
        Caseless("FBTYPE").value(Rfc5545ParamName::FreeBusyTimeType),
        Caseless("LANGUAGE").value(Rfc5545ParamName::Language),
        Caseless("MEMBER").value(Rfc5545ParamName::GroupOrListMembership),
        Caseless("PARTSTAT").value(Rfc5545ParamName::ParticipationStatus),
        Caseless("RANGE").value(Rfc5545ParamName::RecurrenceIdentifierRange),
        Caseless("RELATED").value(Rfc5545ParamName::AlarmTriggerRelationship),
        Caseless("RELTYPE").value(Rfc5545ParamName::RelationshipType),
        Caseless("ROLE").value(Rfc5545ParamName::ParticipationRole),
        Caseless("RSVP").value(Rfc5545ParamName::RsvpExpectation),
        Caseless("SENT-BY").value(Rfc5545ParamName::SentBy),
        Caseless("TZID").value(Rfc5545ParamName::TimeZoneIdentifier),
        Caseless("VALUE").value(Rfc5545ParamName::ValueDataType),
    ))
    .parse_next(input)
}

/// Parses an [`Rfc7986ParamName`].
pub fn rfc7986_param_name<I, E>(input: &mut I) -> Result<Rfc7986ParamName, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("DISPLAY").value(Rfc7986ParamName::Display),
        Caseless("EMAIL").value(Rfc7986ParamName::Email),
        Caseless("FEATURE").value(Rfc7986ParamName::Feature),
        Caseless("LABEL").value(Rfc7986ParamName::Label),
    ))
    .parse_next(input)
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

/// Parses a [`ParamValue`], stripping quotes if they occur.
pub fn param_value<I, E>(input: &mut I) -> Result<ParamValue<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    fn param_text<I, E>(input: &mut I) -> Result<I::Slice, E>
    where
        I: StreamIsPartial + Stream,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        repeat(1.., none_of((..' ', '"', ',', ':', ';', '\u{007F}')))
            .map(|()| ())
            .take()
            .parse_next(input)
    }

    fn quoted_string<I, E>(input: &mut I) -> Result<I::Slice, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        delimited(
            '"',
            repeat(1.., none_of((..' ', '"', '\u{007F}')))
                .map(|()| ())
                .take(),
            '"',
        )
        .parse_next(input)
    }

    alt((
        quoted_string.map(ParamValue::Quoted),
        param_text.map(ParamValue::Safe),
    ))
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_parameter_parsing() {
        assert_eq!(
            parameter::<_, ()>
                .parse_peek("VALUE=CAL-ADDRESS")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Value(ValueType::CalAddress)),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("tzid=America/New_York")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::TzId(TzId("America/New_York"))),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("Rsvp=FALSE")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Rsvp(false)),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("RANGE=thisandfuture")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::RecurrenceIdentifierRange),
        );
    }

    #[test]
    fn inline_encoding() {
        for input in ["ENCODING=8BIT", "ENCODING=8Bit", "ENCODING=8bit"] {
            assert_eq!(
                parameter::<_, ()>
                    .parse_peek(input)
                    .ok()
                    .and_then(|(_, p)| p.try_into_known().ok()),
                Some(KnownParam::Encoding(Encoding::Bit8)),
            );
        }

        for input in ["ENCODING=BASE64", "ENCODING=Base64", "ENCODING=base64"] {
            assert_eq!(
                parameter::<_, ()>
                    .parse_peek(input)
                    .ok()
                    .and_then(|(_, p)| p.try_into_known().ok()),
                Some(KnownParam::Encoding(Encoding::Base64)),
            );
        }

        for input in
            ["ENCODING=base", "ENCODING=bit", "ENCODING=64", "ENCODING=8"]
        {
            assert!(parameter::<_, ()>.parse_peek(input).is_err());
        }
    }

    #[test]
    fn format_type() {
        for input in [
            "FMTTYPE=audio/aac",
            "FMTTYPE=image/bmp",
            "FMTTYPE=text/css",
            "FMTTYPE=application/x-bzip",
        ] {
            assert!(matches!(
                parameter::<_, ()>
                    .parse_peek(input)
                    .ok()
                    .and_then(|(_, p)| p.try_into_known().ok()),
                Some(KnownParam::FormatType(_))
            ));
        }

        for input in
            ["FMTTYPE=", "FMTTYPE=missing slash", "FMTTYPE=back\\slash"]
        {
            assert!(parameter::<_, ()>.parse_peek(input).is_err());
        }
    }

    #[test]
    fn recurrence_identifier_range() {
        for input in [
            "RANGE=THISANDFUTURE",
            "RANGE=ThisAndFuture",
            "RANGE=thisandfuture",
        ] {
            assert!(matches!(
                parameter::<_, ()>
                    .parse_peek(input)
                    .ok()
                    .and_then(|(_, p)| p.try_into_known().ok()),
                Some(KnownParam::RecurrenceIdentifierRange)
            ));
        }

        for input in ["RANGE=", "RANGE=garbage", "RANGE=this-and-future"] {
            assert!(parameter::<_, ()>.parse_peek(input).is_err());
        }
    }

    #[test]
    fn alarm_trigger_relationship() {
        for input in ["RELATED=START", "RELATED=Start", "RELATED=start"] {
            assert!(matches!(
                parameter::<_, ()>
                    .parse_peek(input)
                    .ok()
                    .and_then(|(_, p)| p.try_into_known().ok()),
                Some(KnownParam::AlarmTrigger(TriggerRelation::Start)),
            ));
        }

        for input in ["RELATED=END", "RELATED=End", "RELATED=end"] {
            assert!(matches!(
                parameter::<_, ()>
                    .parse_peek(input)
                    .ok()
                    .and_then(|(_, p)| p.try_into_known().ok()),
                Some(KnownParam::AlarmTrigger(TriggerRelation::End)),
            ));
        }

        for input in ["RELATED=", "RELATED=,garbage", "RELATED=anything-else"] {
            assert!(parameter::<_, ()>.parse_peek(input).is_err());
        }
    }

    #[test]
    fn parameter_edge_cases() {
        assert!(parameter::<_, ()>.parse_peek("VALUE=").is_err()); // missing value
        assert!(parameter::<_, ()>.parse_peek("=RECUR").is_err()); // missing name
        assert!(parameter::<_, ()>.parse_peek("=").is_err()); // missing name & value

        // trailing semicolon should not be stripped
        assert_eq!(
            parameter::<_, ()>.parse_peek("LANGUAGE=en-GB;").unwrap().0,
            ";"
        );
    }

    #[test]
    fn rfc7986_parameter_parsing() {
        assert_eq!(
            parameter::<_, ()>
                .parse_peek("DISPLAY=THUMBNAIL")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Display(DisplayType::Thumbnail)),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("display=Badge")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Display(DisplayType::Badge)),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("DISPLAY=X-SOMETHING-ELSE")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Display(DisplayType::X("X-SOMETHING-ELSE"))),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("Email=literally anything")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Email(ParamValue::Safe("literally anything"))),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("Email=\"a quoted string\"")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Email(ParamValue::Quoted("a quoted string"))),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("FEATURE=moderator")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Feature(FeatureType::Moderator)),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("feature=Screen")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Feature(FeatureType::Screen)),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("feature=random-iana-token")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Feature(FeatureType::Iana("random-iana-token"))),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("LABEL=some text")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Label(ParamValue::Safe("some text"))),
        );

        assert_eq!(
            parameter::<_, ()>
                .parse_peek("label=\"some quoted text\"")
                .ok()
                .and_then(|(_, p)| p.try_into_known().ok()),
            Some(KnownParam::Label(ParamValue::Quoted("some quoted text"))),
        );
    }

    #[test]
    fn multiple_uris() {
        let param = concat!(
            "DELEGATED-FROM=",
            "\"mailto:alice@place.com\",",
            "\"mailto:brice@place.com\",",
            "\"mailto:carla@place.com\"",
        );

        assert_eq!(
            parameter::<_, ()>.parse_peek(param).map(|(_, p)| p),
            Ok(Param::Known(KnownParam::DelFrom(
                [
                    "mailto:alice@place.com",
                    "mailto:brice@place.com",
                    "mailto:carla@place.com",
                ]
                .into_iter()
                .map(CalAddress)
                .collect()
            ))),
        );
    }
}
