//! Parsers for primitive (i.e. terminal) grammar elements.

use chrono::{NaiveDate, Utc};
use winnow::{
    ModalResult, Parser,
    ascii::digit1,
    combinator::{alt, empty, trace},
    token::take,
};

use crate::model::primitive::{Date, RawTime, Time, TimeFormat};

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

/// A version of [`dec_uint`] that accepts leading zeroes.
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
