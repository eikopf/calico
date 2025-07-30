//! Parsers for primitive (i.e. terminal) grammar elements.

use chrono::NaiveDate;
use winnow::{
    Parser,
    ascii::{Caseless, digit0, digit1},
    combinator::{
        alt, delimited, empty, opt, preceded, repeat, separated_pair,
        terminated, trace,
    },
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, SliceLen, Stream, StreamIsPartial},
    token::{none_of, one_of, take_while},
};

use crate::model::primitive::{
    BinaryText, CalendarUserType, ClassValue, CompletionPercentage, Date,
    DateTime, DisplayType, Duration, DurationKind, DurationTime, Encoding,
    FeatureType, Float, FormatType, FreeBusyType, Geo, GeoComponent, Language,
    Method, ParticipationRole, ParticipationStatus, Period, Priority, RawText,
    RawTime, RelationshipType, Sign, Time, TimeFormat, TimeTransparency,
    TriggerRelation, TzId, Uid, Uri, Utc, UtcOffset, ValueType,
};

/// Parses a [`TzId`].
pub fn tz_id<I, E>(input: &mut I) -> Result<TzId<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    ('/', raw_text).take().map(TzId).parse_next(input)
}

/// Parses a [`TimeTransparency`].
pub fn time_transparency<I, E>(input: &mut I) -> Result<TimeTransparency, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("TRANSPARENT").value(TimeTransparency::Transparent),
        Caseless("OPAQUE").value(TimeTransparency::Opaque),
    ))
    .parse_next(input)
}

/// Parses a [`FeatureType`].
pub fn feature_type<I, E>(input: &mut I) -> Result<FeatureType<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    <I as Stream>::Token: AsChar + Clone,
    FeatureType<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn display_type<I, E>(input: &mut I) -> Result<DisplayType<I::Slice>, E>
where
    I: Stream + StreamIsPartial + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    DisplayType<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn gregorian<I, E>(input: &mut I) -> Result<(), E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    Caseless("GREGORIAN").void().parse_next(input)
}

/// Parses the exact string `2.0`, which occurs in the version property. This
/// parser returns `()` because no other version of iCalendar has ever been
/// registered or recognised.
pub fn v2_0<I, E>(input: &mut I) -> Result<(), E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    // using Caseless here does nothing, but it makes the trait bounds match
    // the other parsers in this module
    Caseless("2.0").void().parse_next(input)
}

/// Parses a [`Method`].
pub fn method<I, E>(input: &mut I) -> Result<Method<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    Method<I::Slice>: Clone,
    E: ParserError<I>,
{
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

/// Parses a [`Uid`].
pub fn uid<I, E>(input: &mut I) -> Result<Uid<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    raw_text
        .map(|RawText(source)| Uid(source))
        .parse_next(input)
}

/// Parses an RFC 5646 language tag.
pub fn language<I, E>(input: &mut I) -> Result<Language<I::Slice>, E>
where
    I: StreamIsPartial + Stream,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    // WARN: this parser is massively more permissive than it should be, but
    // parsing a language tag exactly is a problem for the future
    iana_token.map(Language).parse_next(input)
}

/// Parses an RFC 3986 URI. The description of the grammar in RFC 5545 is
/// somewhat ambiguous, so in particular we first parse a sequence of characters
/// which may occur in a URI and then attempt to verify that it is actually a
/// valid URI.
pub fn uri<I, E>(input: &mut I) -> Result<Uri<I::Slice>, E>
where
    I: StreamIsPartial + Stream,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    /// Parses the longest sequence of characters which can occur in a URI. See
    /// RFC 3986 sections 2.1, 2.2, and 2.3 for details.
    fn uri_character<I, E>(input: &mut I) -> Result<I::Token, E>
    where
        I: StreamIsPartial + Stream,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        one_of(('!', '#'..=';', '=', '?'..='Z', '[', ']', '_', 'a'..='z'))
            .parse_next(input)
    }

    repeat::<_, _, (), _, _>(1.., uri_character)
        .take()
        .map(Uri)
        .parse_next(input)
}

/// Parses a base64-encoded character string.
pub fn binary<I, E>(input: &mut I) -> Result<BinaryText<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I>,
{
    fn b_char<I, E>(input: &mut I) -> Result<I::Token, E>
    where
        I: StreamIsPartial + Stream,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        one_of(('0'..='9', 'a'..='z', 'A'..='Z', '+', '/', '='))
            .parse_next(input)
    }

    (
        repeat::<_, _, (), _, _>(0.., (b_char, b_char, b_char, b_char)),
        opt(alt((
            (b_char, b_char, b_char, '=').void(),
            (b_char, b_char, '=', '=').void(),
        ))),
    )
        .take()
        .map(BinaryText)
        .parse_next(input)
}

pub fn class_value<I, E>(input: &mut I) -> Result<ClassValue<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    ClassValue<I::Slice>: Clone,
    E: ParserError<I>,
{
    alt((
        Caseless("CONFIDENTIAL").value(ClassValue::Confidential),
        Caseless("PRIVATE").value(ClassValue::Private),
        Caseless("PUBLIC").value(ClassValue::Public),
        x_name.map(ClassValue::X),
        iana_token.map(ClassValue::Iana),
    ))
    .parse_next(input)
}

/// Parses a calendar user type value (RFC 5545 §3.2.3).
pub fn calendar_user_type<I, E>(
    input: &mut I,
) -> Result<CalendarUserType<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    CalendarUserType<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn inline_encoding<I, E>(input: &mut I) -> Result<Encoding, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("8BIT").value(Encoding::Bit8),
        Caseless("BASE64").value(Encoding::Base64),
    ))
    .parse_next(input)
}

/// Parses a [`FormatType`] (effectively a MIME type).
pub fn format_type<I, E>(input: &mut I) -> Result<FormatType<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Slice: SliceLen,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    /// The `reg-name` grammar rule as in RFC 4288 §4.2
    fn reg_name<I, E>(input: &mut I) -> Result<I::Slice, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
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

    let ((type_name, _sep, _subtype_name), source) =
        (reg_name, '/', reg_name).with_taken().parse_next(input)?;
    let separator_index = type_name.slice_len() + 1;

    Ok(FormatType {
        source,
        separator_index,
    })
}

/// Parses a [`FreeBusyType`].
pub fn free_busy_type<I, E>(input: &mut I) -> Result<FreeBusyType<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    FreeBusyType<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn participation_status<I, E>(
    input: &mut I,
) -> Result<ParticipationStatus<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    ParticipationStatus<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn alarm_trigger_relationship<I, E>(
    input: &mut I,
) -> Result<TriggerRelation, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("START").value(TriggerRelation::Start),
        Caseless("END").value(TriggerRelation::End),
    ))
    .parse_next(input)
}

/// Parses a [`RelationshipType`].
pub fn relationship_type<I, E>(
    input: &mut I,
) -> Result<RelationshipType<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    RelationshipType<I::Slice>: Clone,
    E: ParserError<I>,
{
    alt((
        Caseless("SIBLING").value(RelationshipType::Sibling),
        Caseless("PARENT").value(RelationshipType::Parent),
        Caseless("CHILD").value(RelationshipType::Child),
        iana_token.map(RelationshipType::Other),
    ))
    .parse_next(input)
}

/// Parses a [`ParticipationRole`].
pub fn participation_role<I, E>(
    input: &mut I,
) -> Result<ParticipationRole<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    I::Token: AsChar + Clone,
    ParticipationRole<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn value_type<I, E>(input: &mut I) -> Result<ValueType<I::Slice>, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Token: AsChar + Clone,
    ValueType<I::Slice>: Clone,
    E: ParserError<I>,
{
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
pub fn iana_token<I, E>(input: &mut I) -> Result<<I as Stream>::Slice, E>
where
    I: StreamIsPartial + Stream,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I>,
{
    take_while(1.., ('0'..='9', 'a'..='z', 'A'..='Z', '-')).parse_next(input)
}

/// Parses an X-name, effectively an [`iana_token`] prefixed with `X-`. Note
/// that the grammar in RFC 5545 §3.1 includes an optional `vendorid` segment,
/// but also that this introduces a grammar ambiguity between the `vendorid`
/// and `iana-token` rules.
pub fn x_name<I, E>(input: &mut I) -> Result<<I as Stream>::Slice, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I>,
{
    ('X', '-', iana_token).take().parse_next(input)
}

/// Parses a [`RawText`].
pub fn raw_text<I, E>(input: &mut I) -> Result<RawText<I::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    fn safe_text<I, E>(input: &mut I) -> Result<I::Slice, E>
    where
        I: StreamIsPartial + Stream,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        repeat::<_, _, (), _, _>(1.., none_of(('\\', ';', ',', ..' ')))
            .take()
            .parse_next(input)
    }

    fn text_escape<I, E>(input: &mut I) -> Result<I::Slice, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        E: ParserError<I>,
    {
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
        .take()
        .parse_next(input)
    }

    trace(
        "raw_text",
        repeat::<_, _, (), _, _>(1.., alt((safe_text, text_escape))),
    )
    .take()
    .map(RawText)
    .parse_next(input)
}

/// Parses a [`Period`].
///
/// Since an explicit period may admit both absolute and local (floating) times
/// in the same object, we cannot immediately determine whether a given period
/// is valid as described in RFC 5545 §3.3.9.
pub fn period<I, E>(input: &mut I) -> Result<Period, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Slice: AsBStr,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I>
        + FromExternalError<I, InvalidDateError>
        + FromExternalError<I, InvalidRawTimeError>
        + FromExternalError<I, InvalidDurationTimeError>,
{
    enum DtOrDur {
        Dt(DateTime),
        Dur(Duration),
    }

    separated_pair(
        datetime,
        '/',
        alt((datetime.map(DtOrDur::Dt), duration.map(DtOrDur::Dur))),
    )
    .map(|(start, end)| match end {
        DtOrDur::Dt(end) => Period::Explicit { start, end },
        DtOrDur::Dur(duration) => Period::Start { start, duration },
    })
    .parse_next(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidDurationTimeError<T = usize> {
    hours: Option<T>,
    seconds: Option<T>,
}

/// Parses a [`Duration`].
pub fn duration<I, E>(input: &mut I) -> Result<Duration, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Slice: AsBStr,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, InvalidDurationTimeError>,
{
    fn time<I, E>(input: &mut I) -> Result<DurationTime, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        <I as Stream>::Slice: AsBStr,
        <I as Stream>::Token: AsChar + Clone,
        E: ParserError<I> + FromExternalError<I, InvalidDurationTimeError>,
    {
        let checkpoint = input.checkpoint();

        let components = preceded(
            'T',
            (
                opt(terminated(lz_dec_uint, 'H')),
                opt(terminated(lz_dec_uint, 'M')),
                opt(terminated(lz_dec_uint, 'S')),
            ),
        )
        .parse_next(input)?;

        match components {
            (Some(hours), Some(minutes), Some(seconds)) => {
                Ok(DurationTime::HMS {
                    hours,
                    minutes,
                    seconds,
                })
            }
            (Some(hours), Some(minutes), None) => {
                Ok(DurationTime::HM { hours, minutes })
            }
            (None, Some(minutes), Some(seconds)) => {
                Ok(DurationTime::MS { minutes, seconds })
            }
            (Some(hours), None, None) => Ok(DurationTime::H { hours }),
            (None, Some(minutes), None) => Ok(DurationTime::M { minutes }),
            (None, None, Some(seconds)) => Ok(DurationTime::S { seconds }),
            (hours, None, seconds) => {
                input.reset(&checkpoint);

                Err(E::from_external_error(
                    input,
                    InvalidDurationTimeError { hours, seconds },
                ))
            }
        }
    }

    separated_pair(
        opt(sign),
        'P',
        alt((
            time.map(|time| DurationKind::Time { time }),
            separated_pair(lz_dec_uint, 'D', opt(time))
                .map(|(days, time)| DurationKind::Date { days, time }),
            terminated(lz_dec_uint, 'W')
                .map(|weeks| DurationKind::Week { weeks }),
        )),
    )
    .map(|(sign, kind)| Duration { sign, kind })
    .parse_next(input)
}

/// Parses a datetime of the form `YYYYMMDDThhmmss`, with an optional time
/// format suffix.
pub fn datetime<I, E>(input: &mut I) -> Result<DateTime<TimeFormat>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I>
        + FromExternalError<I, InvalidDateError>
        + FromExternalError<I, InvalidRawTimeError>,
{
    (date, 'T', time)
        .map(|(date, _, time)| DateTime { date, time })
        .parse_next(input)
}

/// Parses a datetime of the form `YYYYMMDDThhmmssZ`, including the mandatory
/// UTC marker suffix.
pub fn datetime_utc<I, E>(input: &mut I) -> Result<DateTime<Utc>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I>
        + FromExternalError<I, InvalidDateError>
        + FromExternalError<I, InvalidRawTimeError>,
{
    (date, 'T', time_utc)
        .map(|(date, _, time)| DateTime { date, time })
        .parse_next(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidDateError {
    year: u16,
    month: u8,
    day: u8,
}

/// Parses a date of the form YYYYMMDD.
pub fn date<I, E>(input: &mut I) -> Result<Date, E>
where
    I: StreamIsPartial + Stream,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, InvalidDateError>,
{
    let checkpoint = input.checkpoint();

    let year = (
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
    )
        .map(|(x, y, z, w)| {
            // SAFETY: all of x, y, z, w are guaranteed to be in the range '0'..='9'.
            let thou = unsafe { x.to_digit(10).unwrap_unchecked() } * 1000;
            let hund = unsafe { y.to_digit(10).unwrap_unchecked() } * 100;
            let tens = unsafe { z.to_digit(10).unwrap_unchecked() } * 10;
            let ones = unsafe { w.to_digit(10).unwrap_unchecked() };

            let year = (thou + hund + tens + ones) as u16;
            debug_assert!((0..=9999).contains(&year));
            year
        })
        .parse_next(input)?;

    let month = (
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
    )
        .map(|(x, y)| {
            // SAFETY: both x and y are guaranteed to be in the range '0'..='9'.
            let tens = unsafe { x.to_digit(10).unwrap_unchecked() } * 10;
            let ones = unsafe { y.to_digit(10).unwrap_unchecked() };

            (tens + ones) as u8
        })
        .parse_next(input)?;

    let day = (
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
    )
        .map(|(x, y)| {
            // SAFETY: both x and y are guaranteed to be in the range '0'..='9'.
            let tens = unsafe { x.to_digit(10).unwrap_unchecked() } * 10;
            let ones = unsafe { y.to_digit(10).unwrap_unchecked() };

            (tens + ones) as u8
        })
        .parse_next(input)?;

    match NaiveDate::from_ymd_opt(year.into(), month.into(), day.into()) {
        Some(date) => Ok(Date(date)),
        None => {
            input.reset(&checkpoint);

            Err(E::from_external_error(
                input,
                InvalidDateError { year, month, day },
            ))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidUtcOffsetError {
    NegativeZero,
    BadHours(u8),
    BadMinutes(u8),
    BadSeconds(u8),
}

/// Parses a [`UtcOffset`].
pub fn utc_offset<I, E>(input: &mut I) -> Result<UtcOffset, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, InvalidUtcOffsetError>,
{
    fn digit2<I, E>(input: &mut I) -> Result<u8, E>
    where
        I: StreamIsPartial + Stream,
        I::Token: AsChar + Clone,
        E: ParserError<I>,
    {
        (one_of('0'..='9'), one_of('0'..='9'))
            .map(|(a, b): (I::Token, I::Token)| {
                // SAFETY: the parser guarantees the result will be an ascii digit
                let a = unsafe { a.as_char().to_digit(10).unwrap_unchecked() };
                let b = unsafe { b.as_char().to_digit(10).unwrap_unchecked() };
                (10 * a + b) as u8
            })
            .parse_next(input)
    }

    let sign = sign.parse_next(input)?;
    let hours = digit2.parse_next(input)?;

    if hours >= 24 {
        return Err(E::from_external_error(
            input,
            InvalidUtcOffsetError::BadHours(hours),
        ));
    }

    let minutes = digit2.parse_next(input)?;

    if minutes >= 60 {
        return Err(E::from_external_error(
            input,
            InvalidUtcOffsetError::BadMinutes(minutes),
        ));
    }

    let seconds = opt(digit2).parse_next(input)?;

    if let Some(seconds @ 60..) = seconds {
        return Err(E::from_external_error(
            input,
            InvalidUtcOffsetError::BadSeconds(seconds),
        ));
    }

    match seconds {
        Some(0) | None
            if hours == 0 && minutes == 0 && sign == Sign::Negative =>
        {
            Err(E::from_external_error(
                input,
                InvalidUtcOffsetError::NegativeZero,
            ))
        }
        Some(seconds @ 60..) => Err(E::from_external_error(
            input,
            InvalidUtcOffsetError::BadSeconds(seconds),
        )),
        _ => Ok(UtcOffset {
            sign,
            hours,
            minutes,
            seconds,
        }),
    }
}

/// Parses a [`Time<TimeFormat>`].
pub fn time<I, E>(input: &mut I) -> Result<Time<TimeFormat>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, InvalidRawTimeError>,
{
    (raw_time, time_format)
        .parse_next(input)
        .map(|(raw, format)| Time { raw, format })
}

/// Parses a [`Time<Utc>`].
pub fn time_utc<I, E>(input: &mut I) -> Result<Time<Utc>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, InvalidRawTimeError>,
{
    (raw_time, utc_marker)
        .parse_next(input)
        .map(|(raw, ())| Time { raw, format: Utc })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidRawTimeError {
    hours: u8,
    minutes: u8,
    seconds: u8,
}

/// Parses a [`RawTime`].
pub fn raw_time<I, E>(input: &mut I) -> Result<RawTime, E>
where
    I: StreamIsPartial + Stream,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, InvalidRawTimeError>,
{
    let checkpoint = input.checkpoint();

    let hours = (
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
    )
        .map(|(x, y)| {
            // SAFETY: both x and y are guaranteed to be in the range '0'..='9'.
            let tens = unsafe { x.to_digit(10).unwrap_unchecked() } * 10;
            let ones = unsafe { y.to_digit(10).unwrap_unchecked() };

            (tens + ones) as u8
        })
        .parse_next(input)?;

    let minutes = (
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
    )
        .map(|(x, y)| {
            // SAFETY: both x and y are guaranteed to be in the range '0'..='9'.
            let tens = unsafe { x.to_digit(10).unwrap_unchecked() } * 10;
            let ones = unsafe { y.to_digit(10).unwrap_unchecked() };

            (tens + ones) as u8
        })
        .parse_next(input)?;

    let seconds = (
        one_of('0'..='9').map(AsChar::as_char),
        one_of('0'..='9').map(AsChar::as_char),
    )
        .map(|(x, y)| {
            // SAFETY: both x and y are guaranteed to be in the range '0'..='9'.
            let tens = unsafe { x.to_digit(10).unwrap_unchecked() } * 10;
            let ones = unsafe { y.to_digit(10).unwrap_unchecked() };

            (tens + ones) as u8
        })
        .parse_next(input)?;

    match hours < 24 && minutes < 60 && seconds < 61 {
        true => Ok(RawTime {
            hours,
            minutes,
            seconds,
        }),
        false => {
            input.reset(&checkpoint);

            Err(E::from_external_error(
                input,
                InvalidRawTimeError {
                    hours,
                    minutes,
                    seconds,
                },
            ))
        }
    }
}

/// Parses the time format string suffix (an optional `Z`).
pub fn time_format<I, E>(input: &mut I) -> Result<TimeFormat, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    E: ParserError<I>,
{
    alt((
        utc_marker.value(TimeFormat::Utc),
        empty.value(TimeFormat::Local),
    ))
    .parse_next(input)
}

/// Parses the UTC marker string (`Z`).
pub fn utc_marker<I, E>(input: &mut I) -> Result<(), E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    E: ParserError<I>,
{
    'Z'.void().parse_next(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidPriorityError(i32);

/// Parses a [`Priority`].
pub fn priority<I, E>(input: &mut I) -> Result<Priority, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr,
    E: ParserError<I>
        + FromExternalError<I, InvalidIntegerError>
        + FromExternalError<I, InvalidPriorityError>,
{
    let value = integer.parse_next(input)?;

    match value {
        0 => Ok(Priority::Zero),
        1 => Ok(Priority::A1),
        2 => Ok(Priority::A2),
        3 => Ok(Priority::A3),
        4 => Ok(Priority::B1),
        5 => Ok(Priority::B2),
        6 => Ok(Priority::B3),
        7 => Ok(Priority::C1),
        8 => Ok(Priority::C2),
        9 => Ok(Priority::C3),
        _ => Err(E::from_external_error(input, InvalidPriorityError(value))),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidCompletionPercentageError(i32);

/// Parses a [`CompletionPercentage`].
pub fn completion_percentage<I, E>(
    input: &mut I,
) -> Result<CompletionPercentage, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr,
    E: ParserError<I>
        + FromExternalError<I, InvalidIntegerError>
        + FromExternalError<I, InvalidCompletionPercentageError>,
{
    let value = integer.parse_next(input)?;

    match value {
        pct @ 0..=100 => Ok(CompletionPercentage(pct as u8)),
        other => Err(E::from_external_error(
            input,
            InvalidCompletionPercentageError(other),
        )),
    }
}

pub enum InvalidGeoError {
    IntegralTooLarge(u8),
    LatOutOfBounds(GeoComponent),
    LonOutOfBounds(GeoComponent),
}

/// Parses a [`Geo`].
pub fn geo<I, E>(input: &mut I) -> Result<Geo, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Slice: AsBStr + Stream,
    I::Token: AsChar + Clone,
    <<I as Stream>::Slice as Stream>::Token: AsChar,
    E: ParserError<I> + FromExternalError<I, InvalidGeoError>,
{
    fn geo_component<I, E>(input: &mut I) -> Result<GeoComponent, E>
    where
        I: StreamIsPartial + Stream + Compare<char>,
        I::Slice: AsBStr + Stream,
        I::Token: AsChar + Clone,
        <<I as Stream>::Slice as Stream>::Token: AsChar,
        E: ParserError<I> + FromExternalError<I, InvalidGeoError>,
    {
        let sign = opt(sign).parse_next(input)?;
        let magnitude: u8 = lz_dec_uint.parse_next(input)?;

        if magnitude > 180 {
            return Err(E::from_external_error(
                input,
                InvalidGeoError::IntegralTooLarge(magnitude),
            ));
        }

        let integral: i16 =
            i16::from(magnitude) * (sign.unwrap_or_default() as i16);

        let fraction: u32 =
            opt(delimited('.', take_while(0..=6, '0'..='9'), digit0))
                .parse_next(input)?
                .map(|mut digits| {
                    let mut total = 0;

                    while let Some(d) = digits.next_token() {
                        // SAFETY: the parser above guarantees that this char
                        // will be a valid ascii digit
                        let value = unsafe {
                            d.as_char().to_digit(10).unwrap_unchecked()
                        };

                        total = total * 10 + value;
                    }

                    total
                })
                .unwrap_or_default();

        Ok(GeoComponent { integral, fraction })
    }

    let (lat, lon) =
        separated_pair(geo_component, ';', geo_component).parse_next(input)?;

    if !(-90..=90).contains(&lat.integral) {
        Err(E::from_external_error(
            input,
            InvalidGeoError::LatOutOfBounds(lat),
        ))
    } else if !(-180..=180).contains(&lon.integral) {
        Err(E::from_external_error(
            input,
            InvalidGeoError::LonOutOfBounds(lon),
        ))
    } else {
        Ok(Geo { lat, lon })
    }
}

/// Parses the boolean value of `TRUE` or `FALSE`, ignoring case.
pub fn bool_caseless<I, E>(input: &mut I) -> Result<bool, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((Caseless("TRUE").value(true), Caseless("FALSE").value(false)))
        .parse_next(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidIntegerError {
    sign: Option<Sign>,
    digits: u64,
}

pub fn integer<I, E>(input: &mut I) -> Result<i32, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    I::Slice: AsBStr,
    E: ParserError<I> + FromExternalError<I, InvalidIntegerError>,
{
    let sign = opt(sign).parse_next(input)?;
    let digits: u64 = lz_dec_uint.parse_next(input)?;

    let error = InvalidIntegerError { sign, digits };

    i64::try_from(digits)
        .ok()
        .and_then(|d| d.checked_mul(sign.unwrap_or_default() as i64))
        .and_then(|i| i32::try_from(i).ok())
        .ok_or_else(|| E::from_external_error(input, error))
}

/// Parses a [`Float`].
pub fn float<I, E>(input: &mut I) -> Result<Float<<I as Stream>::Slice>, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    <I as Stream>::Token: AsChar,
    E: ParserError<I>,
{
    (opt(sign::<I, E>), digit1, opt(('.', digit1)))
        .take()
        .map(Float)
        .parse_next(input)
}

/// Parses a [`Sign`].
pub fn sign<I, E>(input: &mut I) -> Result<Sign, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    E: ParserError<I>,
{
    alt(('+'.value(Sign::Positive), '-'.value(Sign::Negative)))
        .parse_next(input)
}

/// A version of [`dec_uint`] that accepts leading zeros.
///
/// [`dec_uint`]: winnow::ascii::dec_uint
pub(crate) fn lz_dec_uint<I, O, E>(input: &mut I) -> Result<O, E>
where
    I: StreamIsPartial + Stream,
    <I as Stream>::Slice: AsBStr,
    <I as Stream>::Token: AsChar + Clone,
    O: winnow::ascii::Uint,
    E: ParserError<I>,
{
    trace("lz_dec_uint", move |input: &mut I| {
        digit1
            .void()
            .take()
            .verify_map(|s: <I as Stream>::Slice| {
                let s = AsBStr::as_bstr(&s);
                let s = unsafe { std::str::from_utf8_unchecked(s) };
                O::try_from_dec_uint(s)
            })
            .parse_next(input)
    })
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::escaped::{AsEscaped, Escaped};

    use super::*;

    #[test]
    fn time_transparency_parser() {
        assert_eq!(
            time_transparency::<_, ()>.parse_peek("opaque"),
            Ok(("", TimeTransparency::Opaque))
        );

        assert_eq!(
            time_transparency::<_, ()>.parse_peek("TRANSPARENT"),
            Ok(("", TimeTransparency::Transparent))
        );

        assert!(
            time_transparency::<_, ()>
                .parse_peek("anything else")
                .is_err()
        );
    }

    #[test]
    fn feature_type_parser() {
        assert_eq!(
            feature_type::<_, ()>.parse_peek("chat").unwrap().1,
            FeatureType::Chat
        );

        assert_eq!(
            feature_type::<_, ()>.parse_peek("SCREEN").unwrap().1,
            FeatureType::Screen
        );

        assert_eq!(
            feature_type::<_, ()>
                .parse_peek(Escaped("vi\r\n\tdeo".as_bytes()))
                .unwrap()
                .1,
            FeatureType::Video
        );

        assert_eq!(
            feature_type::<_, ()>
                .parse_peek(Escaped("\r\n\tX-TH\r\n\tING".as_bytes()))
                .unwrap()
                .1,
            FeatureType::Other("\r\n\tX-TH\r\n\tING".as_escaped()),
        );
    }

    #[test]
    fn display_type_parser() {
        assert_eq!(
            display_type::<_, ()>.parse_peek("badge").unwrap().1,
            DisplayType::Badge
        );
        assert_eq!(
            display_type::<_, ()>.parse_peek("GRAPHIC").unwrap().1,
            DisplayType::Graphic
        );
        assert_eq!(
            display_type::<_, ()>.parse_peek("X-OTHER").unwrap().1,
            DisplayType::Other("X-OTHER"),
        );
    }

    #[test]
    fn gregorian_parser() {
        assert!(gregorian::<_, ()>.parse_peek("GREGORIAN").is_ok());
        assert!(gregorian::<_, ()>.parse_peek("GRUGORIAN").is_err());
    }

    #[test]
    fn v2_0_parser() {
        assert!(v2_0::<_, ()>.parse_peek("2.0").is_ok());
        assert!(v2_0::<_, ()>.parse_peek("3.0").is_err());
    }

    #[test]
    fn method_parser() {
        assert!(method::<_, ()>.parse_peek("REFRESH").is_ok());
        assert!(method::<_, ()>.parse_peek("CANCEL").is_ok());
        assert!(method::<_, ()>.parse_peek("ADD").is_ok());
        assert!(method::<_, ()>.parse_peek("any-iana-token").is_ok());
    }

    #[test]
    fn uid_parser() {
        assert!(uid::<_, ()>.parse_peek("some random text").is_ok());
        assert!(
            uid::<_, ()>
                .parse_peek("550e8400e29b41d4a716446655440000")
                .is_ok()
        );
    }

    #[test]
    fn language_parser() {
        assert!(language::<_, ()>.parse_peek("en-US").is_ok());
        assert!(language::<_, ()>.parse_peek("de-CH").is_ok());
        assert!(language::<_, ()>.parse_peek("!!!garbage").is_err());
    }

    #[test]
    fn uri_parser() {
        // these examples are from RFC 3986 §3
        assert!(
            uri::<_, ()>
                .parse_peek(
                    "foo://example.com:8042/over/there?name=ferret#nose"
                )
                .is_ok()
        );
        assert!(
            uri::<_, ()>
                .parse_peek("urn:example:animal:ferret:nose")
                .is_ok()
        );
    }

    #[test]
    fn binary_parser() {
        assert!(binary::<_, ()>.parse("AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA").is_ok());

        assert!(binary::<_, ()>.parse("AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\r\n\tAAAAAAAAA\r\n\tAAAAAAAAAAAAAAAAAAAAAA".as_escaped()).is_ok());
    }

    #[test]
    fn class_value_parser() {
        assert_eq!(
            class_value::<_, ()>.parse_peek("CONFIDENTIAL"),
            Ok(("", ClassValue::Confidential))
        );

        assert_eq!(
            class_value::<_, ()>.parse_peek("public"),
            Ok(("", ClassValue::Public))
        );

        assert_eq!(
            class_value::<_, ()>.parse_peek("X-SOMETHING"),
            Ok(("", ClassValue::X("X-SOMETHING")))
        );

        assert_eq!(
            class_value::<_, ()>.parse_peek("IANA-TOKEN"),
            Ok(("", ClassValue::Iana("IANA-TOKEN")))
        );
    }

    #[test]
    fn calendar_user_type_parser() {
        assert_eq!(
            calendar_user_type::<_, ()>
                .parse_peek("INDIVIDUAL")
                .unwrap()
                .1,
            CalendarUserType::Individual,
        );

        assert_eq!(
            calendar_user_type::<_, ()>.parse_peek("room").unwrap().1,
            CalendarUserType::Room,
        );

        assert_eq!(
            calendar_user_type::<_, ()>
                .parse_peek("iana-token")
                .unwrap()
                .1,
            CalendarUserType::Other("iana-token"),
        );
    }

    #[test]
    fn inline_encoding_parser() {
        assert_eq!(
            inline_encoding::<_, ()>.parse_peek("8bit"),
            inline_encoding::<_, ()>.parse_peek("8BIT"),
        );

        assert_eq!(
            inline_encoding::<_, ()>.parse_peek("Base64"),
            inline_encoding::<_, ()>.parse_peek("BASE64"),
        );

        assert!(
            inline_encoding::<_, ()>
                .parse_peek("anything_else")
                .is_err()
        );
    }

    #[test]
    fn format_type_parser() {
        assert!(
            format_type::<_, ()>
                .parse_peek("application/msword")
                .is_ok()
        );
        assert!(format_type::<_, ()>.parse_peek("image/bmp").is_ok());
        assert!(format_type::<_, ()>.parse_peek("garbage").is_err());
    }

    #[test]
    fn free_busy_type_parser() {
        assert_eq!(
            free_busy_type::<_, ()>.parse_peek("busy"),
            Ok(("", FreeBusyType::Busy))
        );
        assert_eq!(
            free_busy_type::<_, ()>.parse_peek("Free"),
            Ok(("", FreeBusyType::Free))
        );
    }

    #[test]
    fn participation_status_parser() {
        assert!(
            participation_status::<_, ()>
                .parse_peek("NEEDS-ACTION")
                .is_ok()
        );
        assert!(
            participation_status::<_, ()>
                .parse_peek("in-process")
                .is_ok()
        );
        assert!(
            participation_status::<_, ()>
                .parse_peek("some-iana-token")
                .is_ok()
        );
        assert!(
            participation_status::<_, ()>
                .parse_peek(",garbage")
                .is_err()
        );
    }

    #[test]
    fn alarm_trigger_relationship_parser() {
        assert_eq!(
            alarm_trigger_relationship::<_, ()>.parse_peek("START"),
            Ok(("", TriggerRelation::Start)),
        );

        assert_eq!(
            alarm_trigger_relationship::<_, ()>.parse_peek("END"),
            Ok(("", TriggerRelation::End)),
        );

        assert!(
            alarm_trigger_relationship::<_, ()>
                .parse_peek("anything_else")
                .is_err()
        );
    }

    #[test]
    fn relationship_type_parser() {
        assert_eq!(
            relationship_type::<_, ()>.parse_peek("SIBLING"),
            Ok(("", RelationshipType::Sibling)),
        );

        assert_eq!(
            relationship_type::<_, ()>.parse_peek("parent"),
            Ok(("", RelationshipType::Parent)),
        );

        assert_eq!(
            relationship_type::<_, ()>.parse_peek("Child"),
            Ok(("", RelationshipType::Child)),
        );

        assert_eq!(
            relationship_type::<_, ()>.parse_peek("X-SOMETHING-ELSE"),
            Ok(("", RelationshipType::Other("X-SOMETHING-ELSE"))),
        );
    }

    #[test]
    fn participation_role_parser() {
        assert_eq!(
            participation_role::<_, ()>.parse_peek("req-participant"),
            Ok(("", ParticipationRole::ReqParticipant)),
        );

        assert_eq!(
            participation_role::<_, ()>.parse_peek("Chair"),
            Ok(("", ParticipationRole::Chair)),
        );

        assert_eq!(
            participation_role::<_, ()>.parse_peek("X-ANYTHING"),
            Ok(("", ParticipationRole::Other("X-ANYTHING"))),
        );
    }

    #[test]
    fn value_type_parser() {
        assert_eq!(
            value_type::<_, ()>.parse_peek("float"),
            Ok(("", ValueType::Float))
        );
        assert_eq!(
            value_type::<_, ()>.parse_peek("TIME"),
            Ok(("", ValueType::Time))
        );
        assert_eq!(
            value_type::<_, ()>.parse_peek("Recur"),
            Ok(("", ValueType::Recur))
        );
        assert_eq!(
            value_type::<_, ()>
                .parse_peek("BOO\r\n\tLEAN".as_escaped())
                .map(|(_, v)| v),
            Ok(ValueType::Boolean)
        );
        assert_eq!(
            value_type::<_, ()>
                .parse_peek("\r\n X-TY\r\n\tPE".as_escaped())
                .map(|(_, v)| v),
            Ok(ValueType::X("\r\n X-TY\r\n\tPE".as_escaped()))
        );
    }

    #[test]
    fn iana_token_parser() {
        assert!(iana_token::<_, ()>.parse_peek("foo-bar-baz").is_ok());
        assert!(iana_token::<_, ()>.parse_peek("x-name-1-2-3").is_ok());
    }

    #[test]
    fn x_name_parser() {
        assert_eq!(
            x_name::<_, ()>.parse_peek("X-foo-bar"),
            Ok(("", "X-foo-bar"))
        );
        assert_eq!(
            x_name::<_, ()>.parse_peek("X-baz-123"),
            Ok(("", "X-baz-123"))
        );
        assert!(x_name::<_, ()>.parse_peek("x-must-be-capital").is_err());
    }

    #[test]
    fn period_parser() {
        assert!(matches!(
            period::<_, ()>.parse_peek("19970101T180000Z/19970102T070000Z"),
            Ok(("", Period::Explicit { .. })),
        ));

        assert!(matches!(
            period::<_, ()>.parse_peek("19970101T180000Z/PT5H30M"),
            Ok(("", Period::Start { .. })),
        ));
    }

    #[test]
    fn duration_parser() {
        assert_eq!(
            duration::<_, ()>.parse_peek("P7W"),
            Ok((
                "",
                Duration {
                    sign: None,
                    kind: DurationKind::Week { weeks: 7 }
                }
            )),
        );

        assert_eq!(
            duration::<_, ()>.parse_peek("+P15DT5H0M20S"),
            Ok((
                "",
                Duration {
                    sign: Some(Sign::Positive),
                    kind: DurationKind::Date {
                        days: 15,
                        time: Some(DurationTime::HMS {
                            hours: 5,
                            minutes: 0,
                            seconds: 20
                        }),
                    },
                }
            )),
        );
    }

    #[test]
    fn datetime_parser() {
        assert!(datetime::<_, ()>.parse_peek("19970714T045015Z").is_ok());
        assert!(datetime::<_, ()>.parse_peek("19970714T045015").is_ok());

        assert!(
            datetime::<_, ()>
                .parse_peek("19970\r\n\t714T\r\n 045015".as_escaped())
                .is_ok_and(|(_tail, dt)| {
                    dt == DateTime {
                        date: Date(
                            NaiveDate::from_ymd_opt(1997, 7, 14).unwrap(),
                        ),
                        time: Time {
                            raw: RawTime {
                                hours: 4,
                                minutes: 50,
                                seconds: 15,
                            },
                            format: TimeFormat::Local,
                        },
                    }
                })
        );
    }

    #[test]
    fn datetime_utc_parser() {
        assert!(datetime_utc::<_, ()>.parse_peek("19970714T045015Z").is_ok());
        assert!(datetime_utc::<_, ()>.parse_peek("19970714T045015").is_err());
    }

    #[test]
    fn date_parser() {
        assert!(date::<_, ()>.parse_peek("19970714").is_ok());
        assert!(date::<_, ()>.parse_peek("20150229").is_err());

        assert_eq!(
            date::<_, ()>.parse_peek("20040620"),
            Ok(("", Date(NaiveDate::from_ymd_opt(2004, 6, 20).unwrap())))
        );
    }

    #[test]
    fn time_parser() {
        assert_eq!(
            time::<_, ()>.parse_peek("111111Z").unwrap().1,
            Time {
                raw: RawTime {
                    hours: 11,
                    minutes: 11,
                    seconds: 11
                },
                format: TimeFormat::Utc,
            },
        );

        assert!(time::<_, ()>.parse_peek("123456").is_ok());
    }

    #[test]
    fn time_utc_parser() {
        assert!(time_utc::<_, ()>.parse_peek("202020Z").is_ok());
        assert!(time_utc::<_, ()>.parse_peek("202020").is_err());
    }

    #[test]
    fn raw_time_parser() {
        assert_eq!(
            raw_time::<_, ()>.parse_peek("123456".as_bytes()).unwrap().1,
            RawTime {
                hours: 12,
                minutes: 34,
                seconds: 56
            },
        );

        assert!(raw_time::<_, ()>.parse_peek("123456").is_ok());
        assert!(raw_time::<_, ()>.parse_peek("000000").is_ok());
        assert!(raw_time::<_, ()>.parse_peek("235959").is_ok());
        assert!(raw_time::<_, ()>.parse_peek("235960").is_ok());
        assert!(raw_time::<_, ()>.parse_peek("240000").is_err());
    }

    #[test]
    fn utc_offset_parser() {
        assert_eq!(
            utc_offset::<_, ()>.parse_peek("+235959"),
            Ok((
                "",
                UtcOffset {
                    sign: Sign::Positive,
                    hours: 23,
                    minutes: 59,
                    seconds: Some(59),
                }
            ))
        );

        assert_eq!(
            utc_offset::<_, ()>.parse_peek("-2340"),
            Ok((
                "",
                UtcOffset {
                    sign: Sign::Negative,
                    hours: 23,
                    minutes: 40,
                    seconds: None,
                }
            ))
        );

        assert!(utc_offset::<_, ()>.parse_peek("-0000").is_err());
        assert!(utc_offset::<_, ()>.parse_peek("-000000").is_err());
        assert!(utc_offset::<_, ()>.parse_peek("-000015").is_ok());
        assert!(utc_offset::<_, ()>.parse_peek("+000060").is_err());
        assert!(utc_offset::<_, ()>.parse_peek("+0000").is_ok());
        assert!(utc_offset::<_, ()>.parse_peek("+000000").is_ok());
        assert!(utc_offset::<_, ()>.parse_peek("000000").is_err());
    }

    #[test]
    fn time_format_parser() {
        assert_eq!(
            time_format::<_, ()>.parse_peek("Z"),
            Ok(("", TimeFormat::Utc))
        );
        assert_eq!(
            time_format::<_, ()>.parse_peek("ZZ"),
            Ok(("Z", TimeFormat::Utc))
        );
        assert_eq!(
            time_format::<_, ()>.parse_peek("Y"),
            Ok(("Y", TimeFormat::Local))
        );
    }

    #[test]
    fn geo_parser() {
        assert_eq!(
            geo::<_, ()>.parse_peek("00;00"),
            Ok((
                "",
                Geo {
                    lat: GeoComponent {
                        integral: 0,
                        fraction: 0
                    },
                    lon: GeoComponent {
                        integral: 0,
                        fraction: 0
                    },
                }
            ))
        );

        assert_eq!(
            geo::<_, ()>.parse_peek("00;00.12345678"),
            Ok((
                "",
                Geo {
                    lat: GeoComponent {
                        integral: 0,
                        fraction: 0
                    },
                    lon: GeoComponent {
                        integral: 0,
                        fraction: 123456
                    },
                }
            ))
        );

        assert!(geo::<_, ()>.parse_peek("90;90").is_ok());
        assert!(geo::<_, ()>.parse_peek("91;90").is_err());
        assert!(geo::<_, ()>.parse_peek("90;180").is_ok());
        assert!(geo::<_, ()>.parse_peek("90;181").is_err());
    }

    #[test]
    fn utc_marker_parser() {
        assert_eq!(utc_marker::<_, ()>.parse_peek("Z"), Ok(("", ())));
        assert!(utc_marker::<_, ()>.parse_peek("Y").is_err());
    }

    #[test]
    fn priority_parser() {
        assert_eq!(priority::<_, ()>.parse_peek("0"), Ok(("", Priority::Zero)));
        assert_eq!(priority::<_, ()>.parse_peek("1"), Ok(("", Priority::A1)));
        assert_eq!(priority::<_, ()>.parse_peek("2"), Ok(("", Priority::A2)));
        assert_eq!(priority::<_, ()>.parse_peek("3"), Ok(("", Priority::A3)));
        assert_eq!(priority::<_, ()>.parse_peek("4"), Ok(("", Priority::B1)));
        assert_eq!(priority::<_, ()>.parse_peek("5"), Ok(("", Priority::B2)));
        assert_eq!(priority::<_, ()>.parse_peek("6"), Ok(("", Priority::B3)));
        assert_eq!(priority::<_, ()>.parse_peek("7"), Ok(("", Priority::C1)));
        assert_eq!(priority::<_, ()>.parse_peek("8"), Ok(("", Priority::C2)));
        assert_eq!(priority::<_, ()>.parse_peek("9"), Ok(("", Priority::C3)));
        assert!(priority::<_, ()>.parse_peek("10").is_err());
    }

    #[test]
    fn bool_parser() {
        assert_eq!(bool_caseless::<_, ()>.parse_peek("TRUE"), Ok(("", true)));
        assert_eq!(bool_caseless::<_, ()>.parse_peek("FALSE"), Ok(("", false)));
        assert_eq!(bool_caseless::<_, ()>.parse_peek("True"), Ok(("", true)));
        assert_eq!(bool_caseless::<_, ()>.parse_peek("False"), Ok(("", false)));
        assert_eq!(bool_caseless::<_, ()>.parse_peek("true"), Ok(("", true)));
        assert_eq!(bool_caseless::<_, ()>.parse_peek("false"), Ok(("", false)));

        assert_eq!(
            bool_caseless::<_, ()>.parse_peek(Escaped("tr\r\n\tue".as_bytes())),
            Ok(("".as_escaped(), true))
        );

        assert_eq!(
            bool_caseless::<_, ()>
                .parse_peek(Escaped("fals\r\n\te".as_bytes())),
            Ok(("".as_escaped(), false))
        );
    }

    #[test]
    fn integer_parser() {
        assert_eq!(integer::<_, ()>.parse_peek("370"), Ok(("", 370)));
        assert_eq!(integer::<_, ()>.parse_peek("-17"), Ok(("", -17)));
        assert_eq!(
            integer::<_, ()>.parse_peek("2147483647"),
            Ok(("", i32::MAX))
        );
        assert_eq!(
            integer::<_, ()>.parse_peek("-2147483648"),
            Ok(("", i32::MIN))
        );
        assert!(integer::<_, ()>.parse_peek("2147483648").is_err());
    }

    #[test]
    fn float_parser() {
        assert_eq!(
            float::<_, ()>.parse_peek("1000000.0000001"),
            Ok(("", Float("1000000.0000001"))),
        );

        assert_eq!(
            float::<_, ()>
                .parse_peek("1000\r\n\t000.00\r\n 00001".as_escaped()),
            Ok((
                "".as_escaped(),
                Float("1000\r\n\t000.00\r\n 00001".as_escaped())
            )),
        );

        assert_eq!(
            float::<_, ()>.parse_peek("1.333"),
            Ok(("", Float("1.333")))
        );
        assert_eq!(
            float::<_, ()>.parse_peek("-3.14"),
            Ok(("", Float("-3.14")))
        );
        assert_eq!(float::<_, ()>.parse_peek("12."), Ok((".", Float("12"))));
        assert!(float::<_, ()>.parse_peek("+.002").is_err());
    }

    #[test]
    fn sign_parser() {
        assert_eq!(sign::<_, ()>.parse_peek("+"), Ok(("", Sign::Positive)));
        assert_eq!(sign::<_, ()>.parse_peek("-"), Ok(("", Sign::Negative)));
        assert!(sign::<_, ()>.parse_peek("0").is_err());

        assert_eq!(
            sign::<_, ()>.parse_peek(Escaped("\r\n\t+".as_bytes())),
            Ok((Escaped("".as_bytes()), Sign::Positive))
        );

        assert_eq!(
            sign::<_, ()>.parse_peek(Escaped("\r\n -".as_bytes())),
            Ok((Escaped("".as_bytes()), Sign::Negative))
        );
    }
}
