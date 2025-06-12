//! Parsers for primitive (i.e. terminal) grammar elements.

use std::{borrow::Cow, str::FromStr};

use chrono::{NaiveDate, Utc};
use winnow::{
    ModalResult, Parser,
    ascii::digit1,
    combinator::{alt, empty, preceded, repeat, trace},
    stream::Accumulate,
    token::{any, none_of, take},
};

use crate::model::primitive::{
    Date, DateTime, Language, Method, RawTime, Time, TimeFormat, Uid, Uri,
};

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
    "GREGORIAN".void().parse_next(input)
}

/// Parses the exact string `2.0`, which occurs in the version property. This
/// parser returns `()` because no other version of iCalendar has ever been
/// registered or recognised.
pub fn v2_0(input: &mut &str) -> ModalResult<()> {
    "2.0".void().parse_next(input)
}

/// Parses an HTTP method; see [`http::Method`] for details. This parser will
/// accept any IANA token, but special handling is done for the recognised HTTP
/// methods and sufficiently small strings.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::method;
/// use winnow::Parser;
///
/// assert!(method.parse_peek("GET").is_ok());
/// assert!(method.parse_peek("UPDATE").is_ok());
/// assert!(method.parse_peek("DELETE").is_ok());
/// assert!(method.parse_peek("any-iana-token").is_ok());
/// assert!(method.parse_peek("17").is_err());
/// ```
pub fn method(input: &mut &str) -> ModalResult<Method> {
    iana_token
        .try_map(http::Method::from_str)
        .map(Method)
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
pub fn uid(input: &mut &str) -> ModalResult<Uid> {
    text.map(|s| match uuid::Uuid::try_parse(&s) {
        Ok(uuid) => Uid::Uuid(uuid),
        Err(_) => Uid::String(s.into_owned().into_boxed_str()),
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
pub fn language(input: &mut &str) -> ModalResult<Language> {
    text.try_map(|s| oxilangtag::LanguageTag::parse(s.into_owned()))
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
pub fn uri(input: &mut &str) -> ModalResult<Uri> {
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
        .try_map(iri_string::types::UriString::from_str)
        .map(Uri)
        .parse_next(input)
}

/// Parses an IANA token, which consists of ASCII alphabetic characters and the
/// `-` character.
///
/// # Examples
///
/// ```
/// use calico::parser::primitive::iana_token;
/// use winnow::Parser;
///
/// assert!(iana_token.parse_peek("foo-bar-baz").is_ok());
/// assert!(iana_token.parse_peek("00mangled").is_err());
/// ```
pub fn iana_token<'i>(input: &mut &'i str) -> ModalResult<&'i str> {
    repeat::<_, _, (), _, _>(
        1..,
        alt((any.verify(|c: &char| c.is_ascii_alphabetic()), '-')),
    )
    .take()
    .parse_next(input)
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

    repeat::<_, _, Acc<'_>, _, _>(1.., alt((safe_text, text_escape)))
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
