//! Parsers for primitive (i.e. terminal) grammar elements.

use std::{borrow::Cow, str::FromStr};

use chrono::NaiveDate;
use winnow::{
    ModalResult, Parser,
    ascii::{Caseless, digit1},
    combinator::{alt, empty, preceded, repeat, trace},
    stream::Accumulate,
    token::{any, none_of, one_of, take},
};

use crate::model::primitive::{
    Binary, CalendarUserType, Date, DateTime, DisplayType, Encoding,
    FeatureType, FormatType, FreeBusyType, Language, Method, ParticipationRole,
    ParticipationStatus, RawTime, RelationshipType, Time, TimeFormat,
    TriggerRelation, Uid, UriStr, Utc, ValueType,
};

/// Parses a [`FeatureType`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::feature_type;
/// use calico::model::primitive::FeatureType;
/// use winnow::Parser;
///
/// assert_eq!(feature_type.parse_peek("chat").unwrap().1, FeatureType::Chat);
/// assert_eq!(feature_type.parse_peek("SCREEN").unwrap().1, FeatureType::Screen);
/// ```
pub fn feature_type<'i>(
    input: &mut &'i str,
) -> ModalResult<FeatureType<&'i str>> {
    alt((
        Caseless("MODERATOR").value(FeatureType::Moderator),
        Caseless("SCREEN").value(FeatureType::Screen),
        Caseless("AUDIO").value(FeatureType::Audio),
        Caseless("PHONE").value(FeatureType::Phone),
        Caseless("VIDEO").value(FeatureType::Video),
        Caseless("CHAT").value(FeatureType::Chat),
        Caseless("FEED").value(FeatureType::Feed),
        iana_token.map(FeatureType::Other),
    ))
    .parse_next(input)
}

/// Parses a [`DisplayType`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::display_type;
/// use calico::model::primitive::DisplayType;
/// use winnow::Parser;
///
/// assert_eq!(display_type.parse_peek("badge").unwrap().1, DisplayType::Badge);
/// assert_eq!(display_type.parse_peek("GRAPHIC").unwrap().1, DisplayType::Graphic);
/// assert_eq!(
///     display_type.parse_peek("X-OTHER").unwrap().1,
///     DisplayType::Other("X-OTHER"),
/// );
/// ```
pub fn display_type<'i>(
    input: &mut &'i str,
) -> ModalResult<DisplayType<&'i str>> {
    alt((
        Caseless("THUMBNAIL").value(DisplayType::Thumbnail),
        Caseless("FULLSIZE").value(DisplayType::Fullsize),
        Caseless("GRAPHIC").value(DisplayType::Graphic),
        Caseless("BADGE").value(DisplayType::Badge),
        iana_token.map(DisplayType::Other),
    ))
    .parse_next(input)
}

/// Parses the exact string `GREGORIAN`, which occurs in the calendar scale
/// property. This parser returns `()` because the Gregorian calendar is the
/// _only_ calendar scale recognised by RFC 5545 and its successors.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::gregorian;
/// use winnow::Parser;
///
/// assert!(gregorian.parse_peek("GREGORIAN").is_ok());
/// assert!(gregorian.parse_peek("GRUGORIAN").is_err());
/// ```
pub fn gregorian(input: &mut &str) -> ModalResult<()> {
    Caseless("GREGORIAN").void().parse_next(input)
}

/// Parses the exact string `2.0`, which occurs in the version property. This
/// parser returns `()` because no other version of iCalendar has ever been
/// registered or recognised.
pub fn v2_0(input: &mut &str) -> ModalResult<()> {
    "2.0".void().parse_next(input)
}

/// Parses a [`Method`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::method;
/// use winnow::Parser;
///
/// assert!(method.parse_peek("REFRESH").is_ok());
/// assert!(method.parse_peek("CANCEL").is_ok());
/// assert!(method.parse_peek("ADD").is_ok());
/// assert!(method.parse_peek("any-iana-token").is_ok());
/// ```
pub fn method<'i>(input: &mut &'i str) -> ModalResult<Method<&'i str>> {
    alt((
        Caseless("DECLINECOUNTER").value(Method::DeclineCounter),
        Caseless("COUNTER").value(Method::Counter),
        Caseless("PUBLISH").value(Method::Publish),
        Caseless("REFRESH").value(Method::Refresh),
        Caseless("CANCEL").value(Method::Cancel),
        Caseless("REPLY").value(Method::Reply),
        Caseless("ADD").value(Method::Add),
        iana_token.map(Method::Iana),
    ))
    .parse_next(input)
}

/// Parses a UID, with special handling if it is a well-formed UUID.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::uid;
/// use winnow::Parser;
///
/// assert!(!uid.parse_peek("some random text").unwrap().1.is_uuid());
/// assert!(uid.parse_peek("550e8400e29b41d4a716446655440000").unwrap().1.is_uuid());
/// ```
pub fn uid<'i>(input: &mut &'i str) -> ModalResult<Uid<Cow<'i, str>>> {
    text.map(|s| match uuid::Uuid::try_parse(&s) {
        Ok(uuid) => Uid::Uuid(uuid),
        Err(_) => Uid::String(s),
    })
    .parse_next(input)
}

/// Parses an RFC 5646 language tag from a [`text`] value.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::language;
/// use winnow::Parser;
///
/// assert!(language.parse_peek("en-US").is_ok());
/// assert!(language.parse_peek("de-CH").is_ok());
/// assert!(language.parse_peek("!!!garbage").is_err());
/// ```
pub fn language<'i>(input: &mut &'i str) -> ModalResult<Language<&'i str>> {
    iana_token
        .try_map(oxilangtag::LanguageTag::parse)
        .map(Language)
        .parse_next(input)
}

/// Parses an RFC 3986 URI. The description of the grammar in RFC 5545 is
/// somewhat ambiguous, so in particular we first parse a sequence of characters
/// which may occur in a URI and then attempt to verify that it is actually a
/// valid URI.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::uri;
/// use winnow::Parser;
///
/// // these examples are from RFC 3986 §3
/// assert!(uri.parse_peek("foo://example.com:8042/over/there?name=ferret#nose").is_ok());
/// assert!(uri.parse_peek("urn:example:animal:ferret:nose").is_ok());
/// ```
pub fn uri<'i>(input: &mut &'i str) -> ModalResult<&'i UriStr> {
    /// Parses the longest sequence of characters which can occur in a URI. See
    /// RFC 3986 sections 2.1, 2.2, and 2.3 for details.
    fn uri_character(input: &mut &str) -> ModalResult<char> {
        #[allow(clippy::match_like_matches_macro)]
        any.verify(|c| match c {
            '!' => true,
            '#'..=';' => true,
            '=' => true,
            '?'..='Z' => true,
            '[' | ']' => true,
            '_' => true,
            'a'..='z' => true,
            _ => false,
        })
        .parse_next(input)
    }

    repeat::<_, _, (), _, _>(1.., uri_character)
        .take()
        .try_map(UriStr::new)
        .parse_next(input)
}

/// Parses a base64-encoded character string.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::binary;
/// use winnow::Parser;
///
/// const DATA: &str = r#"AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"#;
///
/// assert!(binary.parse(DATA).is_ok());
/// ```
pub fn binary(input: &mut &str) -> ModalResult<Binary> {
    text.take()
        .try_map(|xs| {
            <base64::engine::GeneralPurpose as base64::Engine>::decode(
                &base64::prelude::BASE64_STANDARD,
                xs,
            )
        })
        .map(|bytes| Binary { bytes })
        .parse_next(input)
}

/// Parses a calendar user type value (RFC 5545 §3.2.3).
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::calendar_user_type;
/// use calico::model::primitive::CalendarUserType;
/// use winnow::Parser;
///
/// assert_eq!(
///     calendar_user_type.parse_peek("INDIVIDUAL").unwrap().1,
///     CalendarUserType::Individual,
/// );
///
/// assert_eq!(
///     calendar_user_type.parse_peek("room").unwrap().1,
///     CalendarUserType::Room,
/// );
///
/// assert_eq!(
///     calendar_user_type.parse_peek("iana-token").unwrap().1,
///     CalendarUserType::Other("iana-token".into()),
/// );
/// ```
pub fn calendar_user_type<'i>(
    input: &mut &'i str,
) -> ModalResult<CalendarUserType<&'i str>> {
    alt((
        Caseless("INDIVIDUAL").value(CalendarUserType::Individual),
        Caseless("GROUP").value(CalendarUserType::Group),
        Caseless("RESOURCE").value(CalendarUserType::Resource),
        Caseless("ROOM").value(CalendarUserType::Room),
        Caseless("UNKNOWN").value(CalendarUserType::Unknown),
        iana_token.map(CalendarUserType::Other),
    ))
    .parse_next(input)
}

/// Parses an [`Encoding`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::inline_encoding;
/// use winnow::Parser;
///
/// assert_eq!(
///     inline_encoding.parse_peek("8bit"),
///     inline_encoding.parse_peek("8BIT"),
/// );
///
/// assert_eq!(
///     inline_encoding.parse_peek("Base64"),
///     inline_encoding.parse_peek("BASE64"),
/// );
///
/// assert!(inline_encoding.parse_peek("anything_else").is_err());
/// ```
pub fn inline_encoding(input: &mut &str) -> ModalResult<Encoding> {
    alt((
        Caseless("8BIT").value(Encoding::Bit8),
        Caseless("BASE64").value(Encoding::Base64),
    ))
    .parse_next(input)
}

/// Parses a [`FormatType`] (effectively a MIME type).
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::format_type;
/// use winnow::Parser;
///
/// assert!(format_type.parse_peek("application/msword").is_ok());
/// assert!(format_type.parse_peek("image/bmp").is_ok());
/// assert!(format_type.parse_peek("garbage").is_err());
/// ```
pub fn format_type(input: &mut &str) -> ModalResult<FormatType> {
    /// The `reg-name` grammar rule as in RFC 4288 §4.2
    fn reg_name<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
        repeat::<_, _, (), _, _>(
            1..,
            one_of((
                'a'..='z',
                'A'..='Z',
                '!',
                '#',
                '$',
                '&',
                '.',
                '+',
                '-',
                '^',
                '_',
            )),
        )
        .take()
        .parse_next(input)
    }

    (reg_name, '/', reg_name)
        .take()
        .try_map(mime::Mime::from_str)
        .map(FormatType)
        .parse_next(input)
}

/// Parses a [`FreeBusyType`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::free_busy_type;
/// use calico::model::primitive::FreeBusyType;
/// use winnow::Parser;
///
/// assert_eq!(free_busy_type.parse_peek("busy"), Ok(("", FreeBusyType::Busy)));
/// assert_eq!(free_busy_type.parse_peek("Free"), Ok(("", FreeBusyType::Free)));
/// ```
pub fn free_busy_type<'i>(
    input: &mut &'i str,
) -> ModalResult<FreeBusyType<&'i str>> {
    alt((
        Caseless("BUSY-UNAVAILABLE").value(FreeBusyType::BusyUnavailable),
        Caseless("BUSY-TENTATIVE").value(FreeBusyType::BusyTentative),
        Caseless("BUSY").value(FreeBusyType::Busy),
        Caseless("FREE").value(FreeBusyType::Free),
        iana_token.map(FreeBusyType::Other),
    ))
    .parse_next(input)
}

/// Parses a [`ParticipationStatus`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::participation_status;
/// use winnow::Parser;
///
/// assert!(participation_status.parse_peek("NEEDS-ACTION").is_ok());
/// assert!(participation_status.parse_peek("in-process").is_ok());
/// assert!(participation_status.parse_peek("some-iana-token").is_ok());
/// assert!(participation_status.parse_peek(",garbage").is_err());
/// ```
pub fn participation_status<'i>(
    input: &mut &'i str,
) -> ModalResult<ParticipationStatus<&'i str>> {
    alt((
        Caseless("NEEDS-ACTION").value(ParticipationStatus::NeedsAction),
        Caseless("IN-PROCESS").value(ParticipationStatus::InProcess),
        Caseless("COMPLETED").value(ParticipationStatus::Completed),
        Caseless("DELEGATED").value(ParticipationStatus::Delegated),
        Caseless("TENTATIVE").value(ParticipationStatus::Tentative),
        Caseless("ACCEPTED").value(ParticipationStatus::Accepted),
        Caseless("DECLINED").value(ParticipationStatus::Declined),
        iana_token.map(ParticipationStatus::Other),
    ))
    .parse_next(input)
}

/// Parses a [`TriggerRelation`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::alarm_trigger_relationship;
/// use calico::model::primitive::TriggerRelation;
/// use winnow::Parser;
///
/// assert_eq!(
///     alarm_trigger_relationship.parse_peek("START"),
///     Ok(("", TriggerRelation::Start)),
/// );
///
/// assert_eq!(
///     alarm_trigger_relationship.parse_peek("END"),
///     Ok(("", TriggerRelation::End)),
/// );
///
/// assert!(alarm_trigger_relationship.parse_peek("anything_else").is_err());
/// ```
pub fn alarm_trigger_relationship(
    input: &mut &str,
) -> ModalResult<TriggerRelation> {
    alt((
        Caseless("START").value(TriggerRelation::Start),
        Caseless("END").value(TriggerRelation::End),
    ))
    .parse_next(input)
}

/// Parses a [`RelationshipType`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::relationship_type;
/// use calico::model::primitive::RelationshipType;
/// use winnow::Parser;
///
/// assert_eq!(
///     relationship_type.parse_peek("SIBLING"),
///     Ok(("", RelationshipType::Sibling)),
/// );
///
/// assert_eq!(
///     relationship_type.parse_peek("parent"),
///     Ok(("", RelationshipType::Parent)),
/// );
///
/// assert_eq!(
///     relationship_type.parse_peek("Child"),
///     Ok(("", RelationshipType::Child)),
/// );
///
/// assert_eq!(
///     relationship_type.parse_peek("X-SOMETHING-ELSE"),
///     Ok(("", RelationshipType::Other("X-SOMETHING-ELSE"))),
/// );
/// ```
pub fn relationship_type<'i>(
    input: &mut &'i str,
) -> ModalResult<RelationshipType<&'i str>> {
    alt((
        Caseless("SIBLING").value(RelationshipType::Sibling),
        Caseless("PARENT").value(RelationshipType::Parent),
        Caseless("CHILD").value(RelationshipType::Child),
        iana_token.map(RelationshipType::Other),
    ))
    .parse_next(input)
}

/// Parses a [`ParticipationRole`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::participation_role;
/// use calico::model::primitive::ParticipationRole;
/// use winnow::Parser;
///
/// assert_eq!(
///     participation_role.parse_peek("req-participant"),
///     Ok(("", ParticipationRole::ReqParticipant)),
/// );
///
/// assert_eq!(
///     participation_role.parse_peek("Chair"),
///     Ok(("", ParticipationRole::Chair)),
/// );
///
/// assert_eq!(
///     participation_role.parse_peek("X-ANYTHING"),
///     Ok(("", ParticipationRole::Other("X-ANYTHING"))),
/// );
/// ```
pub fn participation_role<'i>(
    input: &mut &'i str,
) -> ModalResult<ParticipationRole<&'i str>> {
    alt((
        Caseless("REQ-PARTICIPANT").value(ParticipationRole::ReqParticipant),
        Caseless("OPT-PARTICIPANT").value(ParticipationRole::OptParticipant),
        Caseless("NON-PARTICIPANT").value(ParticipationRole::NonParticipant),
        Caseless("CHAIR").value(ParticipationRole::Chair),
        iana_token.map(ParticipationRole::Other),
    ))
    .parse_next(input)
}

/// Parses a [`ValueType`].
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::value_type;
/// use calico::model::primitive::ValueType;
/// use winnow::Parser;
///
/// assert_eq!(value_type.parse_peek("float"), Ok(("", ValueType::Float)));
/// assert_eq!(value_type.parse_peek("TIME"), Ok(("", ValueType::Time)));
/// assert_eq!(value_type.parse_peek("Recur"), Ok(("", ValueType::Recur)));
/// ```
pub fn value_type<'i>(input: &mut &'i str) -> ModalResult<ValueType<&'i str>> {
    alt((
        Caseless("CAL-ADDRESS").value(ValueType::CalAddress),
        Caseless("UTC-OFFSET").value(ValueType::UtcOffset),
        Caseless("DATE-TIME").value(ValueType::DateTime),
        Caseless("DURATION").value(ValueType::Duration),
        Caseless("BOOLEAN").value(ValueType::Boolean),
        Caseless("INTEGER").value(ValueType::Integer),
        Caseless("BINARY").value(ValueType::Binary),
        Caseless("PERIOD").value(ValueType::Period),
        Caseless("FLOAT").value(ValueType::Float),
        Caseless("RECUR").value(ValueType::Recur),
        Caseless("DATE").value(ValueType::Date),
        Caseless("TEXT").value(ValueType::Text),
        Caseless("TIME").value(ValueType::Time),
        Caseless("URI").value(ValueType::Uri),
        x_name.map(ValueType::X),
        iana_token.map(ValueType::Iana),
    ))
    .parse_next(input)
}

/// Parses an IANA token, which consists of ASCII alphanumeric characters and
/// the `-` character.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::iana_token;
/// use winnow::Parser;
///
/// assert!(iana_token.parse_peek("foo-bar-baz").is_ok());
/// assert!(iana_token.parse_peek("x-name-1-2-3").is_ok());
/// ```
pub fn iana_token<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
    repeat::<_, _, (), _, _>(
        1..,
        alt((any.verify(|c: &char| c.is_ascii_alphanumeric()), '-')),
    )
    .take()
    .parse_next(input)
}

/// Parses an X-name, effectively an [`iana_token`] prefixed with `X-`. Note
/// that the grammar in RFC 5545 §3.1 includes an optional `vendorid` segment,
/// but also that this introduces a grammar ambiguity between the `vendorid`
/// and `iana-token` rules.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::x_name;
/// use winnow::Parser;
///
/// assert_eq!(x_name.parse_peek("X-foo-bar"), Ok(("", "X-foo-bar")));
/// assert_eq!(x_name.parse_peek("X-baz-123"), Ok(("", "X-baz-123")));
/// assert!(x_name.parse_peek("x-must-be-capital").is_err());
/// ```
pub fn x_name<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
    ("X-", iana_token).take().parse_next(input)
}

/// Parses an arbitrary sequence of text terminated by CRLF. The return type is
/// `Cow<'_, str>` because a text value may contain escape sequences, in which
/// case it must be modified.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::text;
/// use winnow::Parser;
/// use std::borrow::Cow;
///
/// assert!(text.parse_peek(r#"hello world!"#).is_ok_and(|(_, s)| matches!(s, Cow::Borrowed(_))));
/// assert!(text.parse_peek(r#"hello\, world!"#).is_ok_and(|(_, s)| matches!(s, Cow::Owned(_))));
/// ```
pub fn text<'i>(input: &mut &'i str) -> ModalResult<Cow<'i, str>> {
    /// Wrapper struct for [`Accumulate`] impl on `Cow<'_, str>`.
    #[derive(Debug, Clone)]
    struct Acc<'a>(Cow<'a, str>);

    impl<'a> Accumulate<Acc<'a>> for Acc<'a> {
        fn initial(capacity: Option<usize>) -> Self {
            Acc(Cow::Owned(String::with_capacity(
                capacity.unwrap_or_default(),
            )))
        }

        fn accumulate(&mut self, acc: Acc<'a>) {
            self.0 += acc.0;
        }
    }

    /// A contiguous sequence of characters that don't need to be escaped.
    fn safe_text<'j>(input: &mut &'j str) -> ModalResult<Acc<'j>> {
        repeat::<_, _, (), _, _>(1.., none_of(('\\', ';', ',', ..' ')))
            .take()
            .map(Cow::Borrowed)
            .map(Acc)
            .parse_next(input)
    }

    /// A single textual escape, which has to be allocated to be handled properly.
    fn text_escape<'j>(input: &mut &'j str) -> ModalResult<Acc<'j>> {
        preceded(
            '\\',
            alt((
                '\\'.value("\\"),
                'n'.value("\n"),
                'N'.value("\n"),
                ';'.value(";"),
                ','.value(","),
            )),
        )
        .map(String::from)
        .map(Cow::Owned)
        .map(Acc)
        .parse_next(input)
    }

    trace(
        "text",
        repeat::<_, _, Acc<'_>, _, _>(1.., alt((safe_text, text_escape))),
    )
    .parse_next(input)
    .map(|acc| acc.0)
}

/// Parses a datetime of the form `YYYYMMDDThhmmss`, with an optional time
/// format suffix.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::datetime;
/// use winnow::Parser;
///
/// assert!(datetime.parse_peek("19970714T045015Z").is_ok());
/// assert!(datetime.parse_peek("19970714T045015").is_ok());
/// ```
pub fn datetime(input: &mut &str) -> ModalResult<DateTime<TimeFormat>> {
    (date, 'T', time)
        .map(|(date, _, time)| DateTime { date, time })
        .parse_next(input)
}

/// Parses a datetime of the form `YYYYMMDDThhmmssZ`, including the mandatory
/// UTC marker suffix.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::datetime_utc;
/// use winnow::Parser;
///
/// assert!(datetime_utc.parse_peek("19970714T045015Z").is_ok());
/// assert!(datetime_utc.parse_peek("19970714T045015").is_err());
/// ```
pub fn datetime_utc(input: &mut &str) -> ModalResult<DateTime<Utc>> {
    (date, 'T', time_utc)
        .map(|(date, _, time)| DateTime { date, time })
        .parse_next(input)
}

/// Parses a date of the form YYYYMMDD.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::date;
/// use winnow::Parser;
///
/// assert!(date.parse_peek("19970714").is_ok());
/// assert!(date.parse_peek("20150229").is_err());
/// ```
pub fn date(input: &mut &str) -> ModalResult<Date> {
    (
        take(4usize).and_then(lz_dec_uint::<_, u16, _>),
        take(2usize).and_then(lz_dec_uint),
        take(2usize).and_then(lz_dec_uint),
    )
        .verify_map(|(y, m, d)| NaiveDate::from_ymd_opt(y.into(), m, d))
        .map(Date)
        .parse_next(input)
}

/// Parses a time string with an optional time format suffix.
///
/// # Examples
///
/// ```
/// use calico::model::primitive::{Time, RawTime, TimeFormat};
/// use calico::parser::primitive::time;
/// use winnow::Parser;
///
/// assert_eq!(
///     time.parse_peek("111111Z").unwrap().1,
///     Time {
///         raw: RawTime { hours: 11, minutes: 11, seconds: 11 },
///         format: TimeFormat::Utc,
///     },
/// );
///
/// assert!(time.parse_peek("123456").is_ok());
/// ```
pub fn time(input: &mut &str) -> ModalResult<Time<TimeFormat>> {
    (raw_time, time_format)
        .parse_next(input)
        .map(|(raw, format)| Time { raw, format })
}

/// Parses a time string with a mandatory UTC marker suffix.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::time_utc;
/// use winnow::Parser;
///
/// assert!(time_utc.parse_peek("202020Z").is_ok());
/// assert!(time_utc.parse_peek("202020").is_err());
/// ```
pub fn time_utc(input: &mut &str) -> ModalResult<Time<Utc>> {
    (raw_time, utc_marker)
        .parse_next(input)
        .map(|(raw, ())| Time { raw, format: Utc })
}

/// Parses a raw time string of the form `hhmmss`.
///
/// # Examples
///
/// ```
/// use calico::model::primitive::RawTime;
/// use calico::parser::primitive::raw_time;
/// use winnow::Parser;
///
/// assert_eq!(
///     raw_time.parse_peek("123456").unwrap().1,
///     RawTime { hours: 12, minutes: 34, seconds: 56 },
/// );
///
/// assert!(raw_time.parse_peek("123456").is_ok());
/// assert!(raw_time.parse_peek("000000").is_ok());
/// assert!(raw_time.parse_peek("235959").is_ok());
/// assert!(raw_time.parse_peek("235960").is_ok());
/// assert!(raw_time.parse_peek("240000").is_err());
/// ```
pub fn raw_time(input: &mut &str) -> ModalResult<RawTime> {
    (
        take(2usize)
            .and_then(lz_dec_uint::<_, u8, _>)
            .verify(|&x| x < 24),
        take(2usize)
            .and_then(lz_dec_uint::<_, u8, _>)
            .verify(|&x| x < 60),
        take(2usize)
            .and_then(lz_dec_uint::<_, u8, _>)
            .verify(|&x| x < 61),
    )
        .parse_next(input)
        .map(|(hours, minutes, seconds)| RawTime {
            hours,
            minutes,
            seconds,
        })
}

/// Parses the time format string suffix (an optional `Z`).
///
/// # Examples
///
/// ```
/// use calico::model::primitive::TimeFormat;
/// use calico::parser::primitive::time_format;
/// use winnow::Parser;
///
/// assert_eq!(time_format.parse_peek("Z"), Ok(("", TimeFormat::Utc)));
/// assert_eq!(time_format.parse_peek("ZZ"), Ok(("Z", TimeFormat::Utc)));
/// assert_eq!(time_format.parse_peek("Y"), Ok(("Y", TimeFormat::Local)));
/// ```
pub fn time_format(input: &mut &str) -> ModalResult<TimeFormat> {
    alt((
        utc_marker.value(TimeFormat::Utc),
        empty.value(TimeFormat::Local),
    ))
    .parse_next(input)
}

/// Parses the UTC marker string (`Z`).
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::utc_marker;
/// use winnow::Parser;
///
/// assert_eq!(utc_marker.parse_peek("Z"), Ok(("", ())));
/// assert!(utc_marker.parse_peek("Y").is_err());
/// ```
pub fn utc_marker(input: &mut &str) -> ModalResult<()> {
    'Z'.void().parse_next(input)
}

/// Parses the boolean value of `TRUE` or `FALSE`, ignoring case.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::bool_caseless;
/// use winnow::Parser;
///
/// assert_eq!(bool_caseless.parse_peek("TRUE"), Ok(("", true)));
/// assert_eq!(bool_caseless.parse_peek("FALSE"), Ok(("", false)));
/// assert_eq!(bool_caseless.parse_peek("True"), Ok(("", true)));
/// assert_eq!(bool_caseless.parse_peek("False"), Ok(("", false)));
/// assert_eq!(bool_caseless.parse_peek("true"), Ok(("", true)));
/// assert_eq!(bool_caseless.parse_peek("false"), Ok(("", false)));
/// ```
pub fn bool_caseless(input: &mut &str) -> ModalResult<bool> {
    alt((Caseless("TRUE").value(true), Caseless("FALSE").value(false)))
        .parse_next(input)
}

/// A version of [`dec_uint`] that accepts leading zeros.
///
/// [`dec_uint`]: winnow::ascii::dec_uint
fn lz_dec_uint<I, O, E>(input: &mut I) -> winnow::error::Result<O, E>
where
    I: winnow::stream::StreamIsPartial + winnow::stream::Stream,
    <I as winnow::stream::Stream>::Slice: winnow::stream::AsBStr,
    <I as winnow::stream::Stream>::Token: winnow::stream::AsChar + Clone,
    O: winnow::ascii::Uint,
    E: winnow::error::ParserError<I>,
{
    trace("lz_dec_uint", move |input: &mut I| {
        digit1
            .void()
            .take()
            .verify_map(|s: <I as winnow::stream::Stream>::Slice| {
                let s = winnow::stream::AsBStr::as_bstr(&s);
                let s = unsafe { std::str::from_utf8_unchecked(s) };
                O::try_from_dec_uint(s)
            })
            .parse_next(input)
    })
    .parse_next(input)
}
