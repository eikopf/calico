//! Property parameters.

use iri_string::types::UriStr;
use winnow::{
    ModalResult, Parser,
    ascii::Caseless,
    combinator::{
        alt, delimited, eof, opt, preceded, repeat, separated, separated_pair,
        terminated,
    },
    error::{ContextError, ErrMode},
    token::{none_of, rest},
};

use crate::{
    model::primitive::{
        CalendarUserType, DisplayType, Encoding, FeatureType, FormatType,
        FreeBusyType, Language, ParticipationRole, ParticipationStatus,
        RelationshipType, TriggerRelation, ValueType,
    },
    parser::primitive::{
        alarm_trigger_relationship, bool_caseless, feature_type, format_type,
        free_busy_type, inline_encoding, language, participation_role,
        participation_status, relationship_type, uri, value_type,
    },
};

use super::primitive::{calendar_user_type, display_type, iana_token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Param<'a> {
    AltRep(&'a UriStr),
    CommonName(ParamValue<'a>),
    CUType(CalendarUserType<&'a str>),
    DelFrom(Box<[&'a UriStr]>),
    DelTo(Box<[&'a UriStr]>),
    Dir(&'a UriStr),
    Encoding(Encoding),
    FormatType(FormatType),
    FBType(FreeBusyType<&'a str>),
    Language(Language<&'a str>),
    Member(Box<[&'a UriStr]>),
    PartStatus(ParticipationStatus<&'a str>),
    RecurrenceIdentifierRange,
    AlarmTrigger(TriggerRelation),
    RelType(RelationshipType<&'a str>),
    Role(ParticipationRole<&'a str>),
    Rsvp(bool),
    SentBy(&'a UriStr),
    TzId(&'a str),
    Value(ValueType<&'a str>),
    Display(DisplayType<&'a str>),
    Email(ParamValue<'a>),
    Feature(FeatureType<&'a str>),
    Label(ParamValue<'a>),
    Other {
        name: &'a str,
        value: ParamValue<'a>,
    },
}

/// Parses a [`Param`].
///
/// # Examples
///
/// ```
/// use calico::parser::parameter::{parameter, Param};
/// use winnow::Parser;
///
/// assert!(parameter.parse_peek("EMAIL=user@example.com").is_ok());
/// assert!(parameter.parse_peek("ALTREP=\"CID:foo.bar@baz.com\"").is_ok());
/// assert!(parameter.parse_peek("ALTREP=CID:foo.bar@baz.com").is_err());
/// ```
pub fn parameter<'i>(input: &mut &'i str) -> ModalResult<Param<'i>> {
    /// Parses a single URI delimited by double quotes.
    fn quoted_uri<'i>(value: ParamValue<'i>) -> ModalResult<&'i UriStr> {
        let mut value =
            value.as_quoted().ok_or(ErrMode::Cut(ContextError::new()))?;
        terminated(uri, eof).parse_next(&mut value)
    }

    /// Parses a sequence of at least one URI delimited by double quotes and
    /// separated by commas.
    fn quoted_uris<'i>(
        value: ParamValue<'i>,
    ) -> ModalResult<Box<[&'i UriStr]>> {
        let mut value =
            value.as_quoted().ok_or(ErrMode::Cut(ContextError::new()))?;

        let addresses: Vec<_> =
            terminated(separated(1.., uri, ","), eof).parse_next(&mut value)?;

        Ok(addresses.into_boxed_slice())
    }

    let (name, value) =
        separated_pair(param_name, '=', param_value).parse_next(input)?;

    match name {
        ParamName::Other(name) => Ok(Param::Other { name, value }),
        ParamName::Rfc5545(name) => match name {
            Rfc5545ParamName::AlternateTextRepresentation => {
                let value = quoted_uri(value)?;
                Ok(Param::AltRep(value))
            }
            Rfc5545ParamName::CommonName => Ok(Param::CommonName(value)),
            Rfc5545ParamName::CalendarUserType => {
                let mut value = value.as_str();
                let value = terminated(calendar_user_type, eof)
                    .parse_next(&mut value)?;
                Ok(Param::CUType(value))
            }
            Rfc5545ParamName::Delegators => {
                let addresses = quoted_uris(value)?;
                Ok(Param::DelFrom(addresses))
            }
            Rfc5545ParamName::Delegatees => {
                let addresses = quoted_uris(value)?;
                Ok(Param::DelTo(addresses))
            }
            Rfc5545ParamName::DirectoryEntryReference => {
                let value = quoted_uri(value)?;
                Ok(Param::Dir(value))
            }
            Rfc5545ParamName::InlineEncoding => {
                let mut value = value.as_str();
                let value =
                    terminated(inline_encoding, eof).parse_next(&mut value)?;
                Ok(Param::Encoding(value))
            }
            Rfc5545ParamName::FormatType => {
                let mut value = value.as_str();
                let value =
                    terminated(format_type, eof).parse_next(&mut value)?;
                Ok(Param::FormatType(value))
            }
            Rfc5545ParamName::FreeBusyTimeType => {
                let mut value = value.as_str();
                let value =
                    terminated(free_busy_type, eof).parse_next(&mut value)?;
                Ok(Param::FBType(value))
            }
            Rfc5545ParamName::Language => {
                let mut value = value.as_str();
                let value = terminated(language, eof).parse_next(&mut value)?;
                Ok(Param::Language(value))
            }
            Rfc5545ParamName::GroupOrListMembership => {
                let addresses = quoted_uris(value)?;
                Ok(Param::Member(addresses))
            }
            Rfc5545ParamName::ParticipationStatus => {
                let mut value = value.as_str();
                let value = terminated(participation_status, eof)
                    .parse_next(&mut value)?;
                Ok(Param::PartStatus(value))
            }
            Rfc5545ParamName::RecurrenceIdentifierRange => {
                let mut value = value.as_str();
                let () = terminated(Caseless("THISANDFUTURE"), eof)
                    .void()
                    .parse_next(&mut value)?;
                Ok(Param::RecurrenceIdentifierRange)
            }
            Rfc5545ParamName::AlarmTriggerRelationship => {
                let mut value = value.as_str();
                let value = terminated(alarm_trigger_relationship, eof)
                    .parse_next(&mut value)?;
                Ok(Param::AlarmTrigger(value))
            }
            Rfc5545ParamName::RelationshipType => {
                let mut value = value.as_str();
                let value = terminated(relationship_type, eof)
                    .parse_next(&mut value)?;
                Ok(Param::RelType(value))
            }
            Rfc5545ParamName::ParticipationRole => {
                let mut value = value.as_str();
                let value = terminated(participation_role, eof)
                    .parse_next(&mut value)?;
                Ok(Param::Role(value))
            }
            Rfc5545ParamName::RsvpExpectation => {
                let mut value = value.as_str();
                let value =
                    terminated(bool_caseless, eof).parse_next(&mut value)?;
                Ok(Param::Rsvp(value))
            }
            Rfc5545ParamName::SentBy => {
                let value = quoted_uri(value)?;
                Ok(Param::SentBy(value))
            }
            Rfc5545ParamName::TimeZoneIdentifier => {
                let mut value =
                    value.as_safe().ok_or(ErrMode::Cut(ContextError::new()))?;
                let value =
                    preceded(opt('/'), rest).take().parse_next(&mut value)?;
                Ok(Param::TzId(value))
            }
            Rfc5545ParamName::ValueDataType => {
                let mut value =
                    value.as_safe().ok_or(ErrMode::Cut(ContextError::new()))?;
                let value =
                    terminated(value_type, eof).parse_next(&mut value)?;
                Ok(Param::Value(value))
            }
        },
        ParamName::Rfc7986(name) => match name {
            Rfc7986ParamName::Display => {
                let mut value = value.as_str();
                let value =
                    terminated(display_type, eof).parse_next(&mut value)?;
                Ok(Param::Display(value))
            }
            Rfc7986ParamName::Feature => {
                let mut value = value.as_str();
                let value =
                    terminated(feature_type, eof).parse_next(&mut value)?;
                Ok(Param::Feature(value))
            }
            Rfc7986ParamName::Email => Ok(Param::Email(value)),
            Rfc7986ParamName::Label => Ok(Param::Label(value)),
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

    pub fn as_str(&self) -> &'a str {
        match self {
            ParamValue::Safe(s) => s,
            ParamValue::Quoted(s) => s,
        }
    }

    pub fn as_safe(&self) -> Option<&'a str> {
        if let Self::Safe(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_quoted(&self) -> Option<&'a str> {
        if let Self::Quoted(v) = self {
            Some(v)
        } else {
            None
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_parameter_parsing() {
        assert_eq!(
            parameter.parse_peek("VALUE=CAL-ADDRESS"),
            Ok(("", Param::Value(ValueType::CalAddress))),
        );

        assert_eq!(
            parameter.parse_peek("tzid=America/New_York"),
            Ok(("", Param::TzId("America/New_York"))),
        );

        assert_eq!(
            parameter.parse_peek("Rsvp=FALSE"),
            Ok(("", Param::Rsvp(false))),
        );

        assert_eq!(
            parameter.parse_peek("RANGE=thisandfuture"),
            Ok(("", Param::RecurrenceIdentifierRange)),
        );
    }

    #[test]
    fn rfc7986_parameter_parsing() {
        assert_eq!(
            parameter.parse_peek("DISPLAY=THUMBNAIL"),
            Ok(("", Param::Display(DisplayType::Thumbnail))),
        );

        assert_eq!(
            parameter.parse_peek("display=Badge"),
            Ok(("", Param::Display(DisplayType::Badge))),
        );

        assert_eq!(
            parameter.parse_peek("DISPLAY=X-SOMETHING-ELSE"),
            Ok(("", Param::Display(DisplayType::Other("X-SOMETHING-ELSE")))),
        );

        assert_eq!(
            parameter.parse_peek("Email=literally anything"),
            Ok(("", Param::Email(ParamValue::Safe("literally anything")))),
        );

        assert_eq!(
            parameter.parse_peek("EMAIL=\"a quoted string\""),
            Ok(("", Param::Email(ParamValue::Quoted("a quoted string")))),
        );

        assert_eq!(
            parameter.parse_peek("FEATURE=moderator"),
            Ok(("", Param::Feature(FeatureType::Moderator))),
        );

        assert_eq!(
            parameter.parse_peek("feature=Screen"),
            Ok(("", Param::Feature(FeatureType::Screen))),
        );

        assert_eq!(
            parameter.parse_peek("feature=random-iana-token"),
            Ok(("", Param::Feature(FeatureType::Other("random-iana-token")))),
        );

        assert_eq!(
            parameter.parse_peek("LABEL=some text"),
            Ok(("", Param::Label(ParamValue::Safe("some text")))),
        );

        assert_eq!(
            parameter.parse_peek("label=\"some quoted text\""),
            Ok(("", Param::Label(ParamValue::Quoted("some quoted text")))),
        );
    }
}
