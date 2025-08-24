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
    Parser,
    ascii::Caseless,
    combinator::{alt, delimited, opt, preceded, repeat, separated, terminated},
    error::ParserError,
    stream::{AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::none_of,
};

use crate::{
    model::{
        parameter::{
            KnownParam, Param, ParamName, Rfc5545ParamName, Rfc7986ParamName, StaticParamName,
            UnknownParam,
        },
        primitive::{CalAddress, CalendarUserType, FreeBusyType, ParticipationStatus, TzId, Uri},
    },
    parser::primitive::{
        alarm_trigger_relationship, ascii_lower, bool_caseless, feature_type, format_type,
        free_busy_type, inline_encoding, language, participation_role, participation_status,
        relationship_type, uri, value_type,
    },
};

use super::primitive::{calendar_user_type, display_type, iana_token, x_name};

/// Parses a [`Param`].
pub fn parameter<I, E>(input: &mut I) -> Result<Param<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
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

    fn quoted_addresses<I, E>(input: &mut I) -> Result<Box<[CalAddress<I::Slice>]>, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        let uris: Vec<_> =
            separated(1.., quoted_uri.map(|Uri(uri)| CalAddress(uri)), ',').parse_next(input)?;
        Ok(uris.into_boxed_slice())
    }

    let name = terminated(param_name, '=').parse_next(input)?;

    match name {
        ParamName::Iana(name) => {
            let value: Vec<_> = separated(1.., param_value, ',').parse_next(input)?;
            Ok(Param::Unknown(UnknownParam::Iana {
                name,
                value: value.into_boxed_slice(),
            }))
        }
        ParamName::X(name) => {
            let value: Vec<_> = separated(1.., param_value, ',').parse_next(input)?;
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
            Rfc5545ParamName::RecurrenceIdentifierRange => Caseless("THISANDFUTURE")
                .value(KnownParam::RecurrenceIdentifierRange)
                .map(Param::Known)
                .parse_next(input),
            Rfc5545ParamName::AlarmTriggerRelationship => alarm_trigger_relationship
                .map(KnownParam::AlarmTrigger)
                .map(Param::Known)
                .parse_next(input),
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

/// Parses a [`ParamName`].
pub fn param_name<I, E>(input: &mut I) -> Result<ParamName<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    fn static_name<I>(input: &mut I) -> Result<StaticParamName, ()>
    where
        I: StreamIsPartial + Stream + Compare<Caseless<&'static str>> + Compare<char>,
        I::Token: AsChar + Clone,
    {
        use Rfc5545ParamName as PN5545;
        use Rfc7986ParamName as PN7986;
        use StaticParamName::*;

        macro_rules! tail {
            ($s:literal, $c:expr) => {
                Caseless($s).value($c).parse_next(input)
            };
        }

        match ascii_lower.parse_next(input)? {
            'a' => tail!("ltrep", Rfc5545(PN5545::AlternateTextRepresentation)),
            // CN | CUTYPE
            'c' => match ascii_lower.parse_next(input)? {
                'n' => tail!("", Rfc5545(PN5545::CommonName)),
                'u' => tail!("type", Rfc5545(PN5545::CalendarUserType)),
                _ => Err(()),
            },
            // DELEGATED-FROM | DELEGATED-TO | DIR | DISPLAY
            'd' => match ascii_lower.parse_next(input)? {
                // DELEGATED-FROM | DELEGATED-TO
                'e' => match preceded(Caseless("legated-"), ascii_lower).parse_next(input)? {
                    'f' => tail!("rom", Rfc5545(PN5545::Delegators)),
                    't' => tail!("o", Rfc5545(PN5545::Delegatees)),
                    _ => Err(()),
                },
                // DIR | DISPLAY
                'i' => match ascii_lower.parse_next(input)? {
                    'r' => tail!("", Rfc5545(PN5545::DirectoryEntryReference)),
                    's' => tail!("play", Rfc7986(PN7986::Display)),
                    _ => Err(()),
                },
                _ => Err(()),
            },
            // ENCODING | EMAIL
            'e' => match ascii_lower.parse_next(input)? {
                'm' => tail!("ail", Rfc7986(PN7986::Email)),
                'n' => tail!("coding", Rfc5545(PN5545::InlineEncoding)),
                _ => Err(()),
            },
            // FMTTYPE | FBTYPE | FEATURE
            'f' => match ascii_lower.parse_next(input)? {
                'b' => tail!("type", Rfc5545(PN5545::FreeBusyTimeType)),
                'e' => tail!("ature", Rfc7986(PN7986::Feature)),
                'm' => tail!("ttype", Rfc5545(PN5545::FormatType)),
                _ => Err(()),
            },
            // LABEL | LANGUAGE
            'l' => match preceded(Caseless("a"), ascii_lower).parse_next(input)? {
                'b' => tail!("el", Rfc7986(PN7986::Label)),
                'n' => tail!("guage", Rfc5545(PN5545::Language)),
                _ => Err(()),
            },
            'm' => tail!("ember", Rfc5545(PN5545::GroupOrListMembership)),
            'p' => tail!("artstat", Rfc5545(PN5545::ParticipationStatus)),
            // RANGE | RELATED | RELTYPE | ROLE | RSVP
            'r' => match ascii_lower.parse_next(input)? {
                'a' => tail!("nge", Rfc5545(PN5545::RecurrenceIdentifierRange)),
                // RELATED | RELTYPE
                'e' => match preceded(Caseless("l"), ascii_lower).parse_next(input)? {
                    'a' => {
                        tail!("ted", Rfc5545(PN5545::AlarmTriggerRelationship))
                    }
                    't' => tail!("ype", Rfc5545(PN5545::RelationshipType)),
                    _ => Err(()),
                },
                'o' => tail!("le", Rfc5545(PN5545::ParticipationRole)),
                's' => tail!("vp", Rfc5545(PN5545::RsvpExpectation)),
                _ => Err(()),
            },
            's' => tail!("ent-by", Rfc5545(PN5545::SentBy)),
            't' => tail!("zid", Rfc5545(PN5545::TimeZoneIdentifier)),
            'v' => tail!("alue", Rfc5545(PN5545::ValueDataType)),
            _ => Err(()),
        }
    }

    let checkpoint = input.checkpoint();

    match static_name.parse_next(input) {
        Ok(StaticParamName::Rfc5545(name)) => Ok(ParamName::Rfc5545(name)),
        Ok(StaticParamName::Rfc7986(name)) => Ok(ParamName::Rfc7986(name)),
        Err(()) => {
            input.reset(&checkpoint);
            alt((x_name.map(ParamName::X), iana_token.map(ParamName::Iana))).parse_next(input)
        }
    }
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
        repeat(0.., none_of((..' ', '"', ',', ':', ';', '\u{007F}')))
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
            repeat(0.., none_of((..' ', '"', '\u{007F}')))
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
    use crate::model::{
        parameter::KnownParam,
        primitive::{DisplayType, Encoding, FeatureType, TriggerRelation, ValueType},
    };

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

        for input in ["ENCODING=base", "ENCODING=bit", "ENCODING=64", "ENCODING=8"] {
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

        for input in ["FMTTYPE=", "FMTTYPE=missing slash", "FMTTYPE=back\\slash"] {
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
