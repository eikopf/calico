//! Parsers for recurrence rules.

use std::num::NonZeroU64;

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, terminated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, Stream, StreamIsPartial},
};

use crate::{
    model::rrule::{Frequency, Interval, Part, PartName, RecurrenceRule},
    parser::primitive::lz_dec_uint,
};

use super::{error::CalendarParseError, primitive::datetime_or_date};

/// Parses a [`RecurrenceRule`].
pub fn rrule<I, E>(input: &mut I) -> Result<RecurrenceRule, E>
where
    I: StreamIsPartial + Stream,
    E: ParserError<I>,
{
    todo!()
}

/// Parses a [`Part`].
pub(crate) fn part<I, E>(input: &mut I) -> Result<Part, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Slice: AsBStr,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let name = terminated(part_name, '=').parse_next(input)?;

    Ok(match name {
        PartName::Freq => {
            let freq = frequency.parse_next(input)?;
            Part::Freq(freq)
        }
        PartName::Until => {
            let end_date = datetime_or_date.parse_next(input)?;
            Part::Until(end_date)
        }
        PartName::Count => {
            let count = lz_dec_uint.parse_next(input)?;
            Part::Count(count)
        }
        PartName::Interval => {
            let interval = interval.parse_next(input)?;
            Part::Interval(interval)
        }
        PartName::BySecond => todo!(),
        PartName::ByMinute => todo!(),
        PartName::ByHour => todo!(),
        PartName::ByDay => todo!(),
        PartName::ByMonthDay => todo!(),
        PartName::ByYearDay => todo!(),
        PartName::ByWeekNo => todo!(),
        PartName::ByMonth => todo!(),
        PartName::BySetPos => todo!(),
        PartName::WkSt => todo!(),
    })
}

/// Parses a [`PartName`].
pub(crate) fn part_name<I, E>(input: &mut I) -> Result<PartName, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("BYMONTHDAY").value(PartName::ByMonthDay),
        Caseless("BYYEARDAY").value(PartName::ByYearDay),
        Caseless("BYSECOND").value(PartName::BySecond),
        Caseless("BYMINUTE").value(PartName::ByMinute),
        Caseless("BYWEEKNO").value(PartName::ByWeekNo),
        Caseless("BYSETPOS").value(PartName::BySetPos),
        Caseless("INTERVAL").value(PartName::Interval),
        Caseless("BYMONTH").value(PartName::ByMonth),
        Caseless("BYHOUR").value(PartName::ByHour),
        Caseless("BYDAY").value(PartName::ByDay),
        Caseless("COUNT").value(PartName::Count),
        Caseless("UNTIL").value(PartName::Until),
        Caseless("FREQ").value(PartName::Freq),
        Caseless("WKST").value(PartName::WkSt),
    ))
    .parse_next(input)
}

/// Parses a [`Frequency`].
pub fn frequency<I, E>(input: &mut I) -> Result<Frequency, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("MINUTELY").value(Frequency::Minutely),
        Caseless("SECONDLY").value(Frequency::Secondly),
        Caseless("MONTHLY").value(Frequency::Monthly),
        Caseless("HOURLY").value(Frequency::Hourly),
        Caseless("WEEKLY").value(Frequency::Weekly),
        Caseless("YEARLY").value(Frequency::Yearly),
        Caseless("DAILY").value(Frequency::Daily),
    ))
    .parse_next(input)
}

/// Parses an [`Interval`].
pub fn interval<I, E>(input: &mut I) -> Result<Interval, E>
where
    I: StreamIsPartial + Stream,
    <I as Stream>::Slice: AsBStr,
    <I as Stream>::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let value: u64 = lz_dec_uint.parse_next(input)?;

    match NonZeroU64::new(value) {
        Some(interval) => Ok(Interval(interval)),
        None => Err(E::from_external_error(
            input,
            CalendarParseError::ZeroInterval,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_name_parser() {
        assert_eq!(
            part_name::<_, ()>.parse_peek("FREQ"),
            Ok(("", PartName::Freq))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("until"),
            Ok(("", PartName::Until))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("Count"),
            Ok(("", PartName::Count))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("interVAL"),
            Ok(("", PartName::Interval))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("BySecond"),
            Ok(("", PartName::BySecond))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("byminute"),
            Ok(("", PartName::ByMinute))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("BYHOUR"),
            Ok(("", PartName::ByHour))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("byDAY"),
            Ok(("", PartName::ByDay))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("BYmonthDAY"),
            Ok(("", PartName::ByMonthDay))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("byyearday"),
            Ok(("", PartName::ByYearDay))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("BYWEEKNO"),
            Ok(("", PartName::ByWeekNo))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("ByMonth"),
            Ok(("", PartName::ByMonth))
        );

        assert_eq!(
            part_name::<_, ()>.parse_peek("bysetpos"),
            Ok(("", PartName::BySetPos))
        );

        assert!(part_name::<_, ()>.parse_peek("anything else").is_err());
    }

    #[test]
    fn frequency_parser() {
        assert_eq!(
            frequency::<_, ()>.parse_peek("SECONDLY"),
            Ok(("", Frequency::Secondly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("minutely"),
            Ok(("", Frequency::Minutely))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("Hourly"),
            Ok(("", Frequency::Hourly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("dAILy"),
            Ok(("", Frequency::Daily))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("Weekly"),
            Ok(("", Frequency::Weekly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("monthly"),
            Ok(("", Frequency::Monthly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("YEARLY"),
            Ok(("", Frequency::Yearly))
        );

        assert!(frequency::<_, ()>.parse_peek("anything else").is_err());
    }

    #[test]
    fn interval_parser() {
        assert_eq!(
            interval::<_, ()>.parse_peek("1"),
            Ok(("", Interval(NonZeroU64::MIN)))
        );

        assert!(interval::<_, ()>.parse_peek("0").is_err());
    }
}
