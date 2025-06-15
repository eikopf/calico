//! Property parameters.

use winnow::{
    ModalResult, Parser,
    ascii::Caseless,
    combinator::{alt, delimited, repeat, separated_pair},
    token::none_of,
};

use super::primitive::iana_token;

/// A property parameter with an unstructured textual value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawParam<'a> {
    pub name: ParamName<'a>,
    pub value: ParamValue<'a>,
}

/// Parses a [`RawParam`].
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::parameter;
/// use winnow::Parser;
///
/// assert!(parameter.parse_peek("CUTYPE=GROUP").is_ok());
/// assert!(parameter.parse_peek("TZID=America/New_York").is_ok());
/// ```
pub fn parameter<'i>(input: &mut &'i str) -> ModalResult<RawParam<'i>> {
    separated_pair(param_name, '=', param_value)
        .map(|(name, value)| RawParam { name, value })
        .parse_next(input)
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
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::static_param_name;
/// use winnow::Parser;
///
/// assert!(static_param_name.parse_peek("Range").is_ok_and(|r| r.1.is_rfc5545()));
/// assert!(static_param_name.parse_peek("EMAIL").is_ok_and(|r| r.1.is_rfc7986()));
/// assert!(static_param_name.parse_peek("other").is_err());
/// assert!(static_param_name.parse_peek(",bad,").is_err());
/// ```
pub fn static_param_name(input: &mut &str) -> ModalResult<StaticParamName> {
    alt((
        rfc5545_param_name.map(StaticParamName::Rfc5545),
        rfc7986_param_name.map(StaticParamName::Rfc7986),
    ))
    .parse_next(input)
}

/// A property parameter name from RFC 5545, RFC 7986, or an arbitrary token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamName<'a> {
    Rfc5545(Rfc5545ParamName),
    Rfc7986(Rfc7986ParamName),
    Other(&'a str),
}

impl<'a> ParamName<'a> {
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

    /// Returns `true` if the param name is [`Other`].
    ///
    /// [`Other`]: ParamName::Other
    #[must_use]
    pub fn is_other(&self) -> bool {
        matches!(self, Self::Other(..))
    }
}

/// Parses a [`ParamName`].
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::param_name;
/// use winnow::Parser;
///
/// assert!(param_name.parse_peek("RANGE").is_ok_and(|r| r.1.is_rfc5545()));
/// assert!(param_name.parse_peek("email").is_ok_and(|r| r.1.is_rfc7986()));
/// assert!(param_name.parse_peek("OTHER").is_ok_and(|r| r.1.is_other()));
/// assert!(param_name.parse_peek(",bad,").is_err());
/// ```
pub fn param_name<'i>(input: &mut &'i str) -> ModalResult<ParamName<'i>> {
    // NOTE: there's an obvious optimisation here where we go character by
    // character until either it must be a known static name, or else it must
    // be an unknown name (or an error). i think regex-automata could help with
    // implementing this, but it's probably unnecessary until we have benchmarks

    alt((
        rfc5545_param_name.map(ParamName::Rfc5545),
        rfc7986_param_name.map(ParamName::Rfc7986),
        iana_token.map(ParamName::Other),
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
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::rfc5545_param_name;
/// use calico::parser::parameter::Rfc5545ParamName;
/// use winnow::Parser;
///
/// assert_eq!(
///     rfc5545_param_name.parse_peek("ALTREP").unwrap().1,
///     Rfc5545ParamName::AlternateTextRepresentation,
/// );
///
/// assert_eq!(
///     rfc5545_param_name.parse_peek("Language").unwrap().1,
///     Rfc5545ParamName::Language,
/// );
///
/// assert_eq!(
///     rfc5545_param_name.parse_peek("encoding").unwrap().1,
///     Rfc5545ParamName::InlineEncoding,
/// );
///
/// // EMAIL is an RFC 7986 property parameter
/// assert!(rfc5545_param_name.parse_peek("EMAIL").is_err());
/// ```
pub fn rfc5545_param_name(input: &mut &str) -> ModalResult<Rfc5545ParamName> {
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
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::rfc7986_param_name;
/// use calico::parser::parameter::Rfc7986ParamName;
/// use winnow::Parser;
///
/// assert_eq!(
///     rfc7986_param_name.parse_peek("DISPLAY").unwrap().1,
///     Rfc7986ParamName::Display,
/// );
///
/// assert_eq!(
///     rfc7986_param_name.parse_peek("LABEL").unwrap().1,
///     Rfc7986ParamName::Label,
/// );
///
/// // TZID is an RFC 5545 property parameter
/// assert!(rfc7986_param_name.parse_peek("TZID").is_err());
/// ```
pub fn rfc7986_param_name(input: &mut &str) -> ModalResult<Rfc7986ParamName> {
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
pub enum ParamValue<'a> {
    Safe(&'a str),
    Quoted(&'a str),
}

impl<'a> ParamValue<'a> {
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
}

/// Parses a [`ParamValue`], stripping quotes if they occur.
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::param_value;
/// use winnow::Parser;
///
/// assert!(param_value.parse_peek("hello").is_ok_and(|r| r.1.is_safe()));
/// assert!(param_value.parse_peek("\"hello\"").is_ok_and(|r| r.1.is_quoted()));
/// assert!(param_value.parse_peek(",hello").is_err());
/// ```
pub fn param_value<'i>(input: &mut &'i str) -> ModalResult<ParamValue<'i>> {
    fn param_text<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        repeat(1.., none_of((..' ', '"', ',', ':', ';', '\u{007F}')))
            .map(|()| ())
            .take()
            .parse_next(input)
    }

    fn quoted_string<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
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
