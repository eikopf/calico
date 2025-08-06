//! Parsers for recurrence rules.

use std::{collections::BTreeSet, num::NonZeroU64};

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, opt, separated, terminated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, Stream, StreamIsPartial},
    token::any,
};

use crate::{
    model::{
        primitive::{Month, Weekday},
        rrule::{
            Frequency, Hour, Interval, Minute, MonthDay, MonthDaySetIndex,
            MonthSetIndex, Part, PartName, RecurrenceRule, Second,
            WeekNoSetIndex, WeekdayNum, YearDayNum,
        },
    },
    parser::primitive::{digit, iso_week_index, lz_dec_uint, sign},
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
        PartName::BySecond => {
            let set = separated(1.., second, ',').parse_next(input)?;
            Part::BySecond(set)
        }
        PartName::ByMinute => {
            let set = separated(1.., minute, ',').parse_next(input)?;
            Part::ByMinute(set)
        }
        PartName::ByHour => {
            let set = separated(1.., hour, ',').parse_next(input)?;
            Part::ByHour(set)
        }
        PartName::ByDay => {
            let weekday_nums =
                separated(1.., weekday_num, ',').parse_next(input)?;
            Part::ByDay(weekday_nums)
        }
        PartName::ByMonthDay => {
            let set = separated(1.., month_day_num, ',').parse_next(input)?;
            Part::ByMonthDay(set)
        }
        PartName::ByYearDay => todo!(),
        PartName::ByWeekNo => {
            let set = separated(1.., week_num, ',').parse_next(input)?;
            Part::ByWeekNo(set)
        }
        PartName::ByMonth => {
            let set = separated(1.., month_num, ',').parse_next(input)?;
            Part::ByMonth(set)
        }
        PartName::BySetPos => todo!(),
        PartName::WkSt => {
            let day = weekday.parse_next(input)?;
            Part::WkSt(day)
        }
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

/// Parses a [`MonthDaySetIndex`].
pub fn month_day_num<I, E>(input: &mut I) -> Result<MonthDaySetIndex, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (sign, a, b) = (opt(sign), digit::<I, E, 10>, opt(digit::<I, E, 10>))
        .parse_next(input)?;

    let index = match b {
        Some(b) => a * 10 + b,
        None => a,
    };

    match MonthDay::from_index(index) {
        Some(day) => Ok(MonthDaySetIndex::from_signed_month_day(
            sign.unwrap_or_default(),
            day,
        )),
        None => Err(E::from_external_error(
            input,
            CalendarParseError::InvalidMonthDayIndex(index),
        )),
    }
}

/// Parses a [`YearDayNum`].
pub fn year_day_num<I, E>(input: &mut I) -> Result<YearDayNum, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (sign, a, b) = (
        opt(sign),
        digit::<I, E, 10>,
        opt((digit::<I, E, 10>, opt(digit::<I, E, 10>))),
    )
        .parse_next(input)?;

    let index = match b {
        None => a as u16,
        Some((b, None)) => 10 * (a as u16) + (b as u16),
        Some((b, Some(c))) => 100 * (a as u16) + 10 * (b as u16) + (c as u16),
    };

    match YearDayNum::from_signed_index(sign.unwrap_or_default(), index) {
        Some(year_day_num) => Ok(year_day_num),
        None => todo!(),
    }
}

/// Parses a [`WeekdayNum`].
pub fn weekday_num<I, E>(input: &mut I) -> Result<WeekdayNum, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (ordinal, weekday) =
        (opt((opt(sign), iso_week_index)), weekday).parse_next(input)?;

    let ordinal = ordinal.map(|(sign, week)| (sign.unwrap_or_default(), week));

    Ok(WeekdayNum { ordinal, weekday })
}

/// Parses a [`WeekNoSetIndex`].
pub fn week_num<I, E>(input: &mut I) -> Result<WeekNoSetIndex, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    (opt(sign), iso_week_index)
        .map(|(sign, week)| {
            WeekNoSetIndex::from_signed_week(sign.unwrap_or_default(), week)
        })
        .parse_next(input)
}

/// Parses a [`MonthSetIndex`].
pub fn month_num<I, E>(input: &mut I) -> Result<MonthSetIndex, E>
where
    I: StreamIsPartial + Stream + Compare<char>,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (a, b) =
        (digit::<I, E, 10>, opt(digit::<I, E, 10>)).parse_next(input)?;

    let value = match b {
        Some(b) => 10 * a + b,
        None => a,
    };

    match Month::from_number(value) {
        Some(month) => Ok(month.into()),
        None => Err(E::from_external_error(
            input,
            CalendarParseError::InvalidMonthNumber(value),
        )),
    }
}

/// Parses an [`Hour`].
pub fn hour<I, E>(input: &mut I) -> Result<Hour, E>
where
    I: StreamIsPartial + Stream,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (a, b) =
        (digit::<I, E, 10>, opt(digit::<I, E, 10>)).parse_next(input)?;

    let index = match b {
        Some(b) => 10 * a + b,
        None => a,
    };

    match Hour::from_index(index) {
        Some(hour) => Ok(hour),
        None => Err(E::from_external_error(
            input,
            CalendarParseError::InvalidHourIndex(index),
        )),
    }
}

/// Parses a [`Minute`].
pub fn minute<I, E>(input: &mut I) -> Result<Minute, E>
where
    I: StreamIsPartial + Stream,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (a, b) =
        (digit::<I, E, 10>, opt(digit::<I, E, 10>)).parse_next(input)?;

    let index = match b {
        Some(b) => 10 * a + b,
        None => a,
    };

    match Minute::from_index(index) {
        Some(minute) => Ok(minute),
        None => Err(E::from_external_error(
            input,
            CalendarParseError::InvalidMinuteIndex(index),
        )),
    }
}

/// Parses a [`Second`].
pub fn second<I, E>(input: &mut I) -> Result<Second, E>
where
    I: StreamIsPartial + Stream,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    let (a, b) =
        (digit::<I, E, 10>, opt(digit::<I, E, 10>)).parse_next(input)?;

    let index = match b {
        Some(b) => 10 * a + b,
        None => a,
    };

    match Second::from_index(index) {
        Some(second) => Ok(second),
        None => Err(E::from_external_error(
            input,
            CalendarParseError::InvalidSecondIndex(index),
        )),
    }
}

/// Parses a [`Weekday`].
pub fn weekday<I, E>(input: &mut I) -> Result<Weekday, E>
where
    I: StreamIsPartial + Stream,
    I::Token: AsChar + Clone,
    E: ParserError<I>,
{
    match (any.map(AsChar::as_char), any.map(AsChar::as_char))
        .parse_next(input)?
    {
        ('m' | 'M', 'o' | 'O') => Ok(Weekday::Monday),
        ('t' | 'T', 'u' | 'U') => Ok(Weekday::Tuesday),
        ('w' | 'W', 'e' | 'E') => Ok(Weekday::Wednesday),
        ('t' | 'T', 'h' | 'H') => Ok(Weekday::Thursday),
        ('f' | 'F', 'r' | 'R') => Ok(Weekday::Friday),
        ('s' | 'S', 'a' | 'A') => Ok(Weekday::Saturday),
        ('s' | 'S', 'u' | 'U') => Ok(Weekday::Sunday),
        _ => Err(E::from_input(input)),
    }
}

#[cfg(test)]
mod tests {
    use crate::model::primitive::{IsoWeek, Sign};

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

    #[test]
    fn week_num_parser() {
        assert_eq!(
            week_num::<_, ()>.parse_peek("+1"),
            Ok((
                "",
                WeekNoSetIndex::from_signed_week(Sign::Positive, IsoWeek::W1)
            ))
        );

        assert_eq!(
            week_num::<_, ()>.parse_peek("01"),
            Ok((
                "",
                WeekNoSetIndex::from_signed_week(Sign::Positive, IsoWeek::W1)
            ))
        );

        assert_eq!(
            week_num::<_, ()>.parse_peek("+31"),
            Ok((
                "",
                WeekNoSetIndex::from_signed_week(Sign::Positive, IsoWeek::W31)
            ))
        );

        assert_eq!(
            week_num::<_, ()>.parse_peek("-2"),
            Ok((
                "",
                WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W2)
            ))
        );

        assert_eq!(
            week_num::<_, ()>.parse_peek("-02"),
            Ok((
                "",
                WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W2)
            ))
        );

        assert_eq!(
            week_num::<_, ()>.parse_peek("-50"),
            Ok((
                "",
                WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W50)
            ))
        );
    }

    #[test]
    fn month_day_num_parser() {
        assert_eq!(
            month_day_num::<_, ()>.parse_peek("+1"),
            Ok((
                "",
                MonthDaySetIndex::from_signed_month_day(
                    Sign::Positive,
                    MonthDay::D1
                )
            ))
        );

        assert_eq!(
            month_day_num::<_, ()>.parse_peek("+01"),
            Ok((
                "",
                MonthDaySetIndex::from_signed_month_day(
                    Sign::Positive,
                    MonthDay::D1
                )
            ))
        );

        assert_eq!(
            month_day_num::<_, ()>.parse_peek("-16"),
            Ok((
                "",
                MonthDaySetIndex::from_signed_month_day(
                    Sign::Negative,
                    MonthDay::D16
                )
            ))
        );
    }

    #[test]
    fn month_num_parser() {
        assert_eq!(
            month_num::<_, ()>.parse_peek("1"),
            Ok(("", Month::Jan.into()))
        );

        assert_eq!(
            month_num::<_, ()>.parse_peek("2"),
            Ok(("", Month::Feb.into()))
        );

        assert_eq!(
            month_num::<_, ()>.parse_peek("3"),
            Ok(("", Month::Mar.into()))
        );

        assert_eq!(
            month_num::<_, ()>.parse_peek("4"),
            Ok(("", Month::Apr.into()))
        );

        // ...

        assert_eq!(
            month_num::<_, ()>.parse_peek("11"),
            Ok(("", Month::Nov.into()))
        );

        assert_eq!(
            month_num::<_, ()>.parse_peek("12"),
            Ok(("", Month::Dec.into()))
        );

        assert!(month_num::<_, ()>.parse_peek("13").is_err());
        assert!(month_num::<_, ()>.parse_peek("14").is_err());
    }

    #[test]
    fn hour_parser() {
        assert_eq!(hour::<_, ()>.parse_peek("0"), Ok(("", Hour::H0)));
        assert_eq!(hour::<_, ()>.parse_peek("00"), Ok(("", Hour::H0)));

        assert_eq!(hour::<_, ()>.parse_peek("4"), Ok(("", Hour::H4)));
        assert_eq!(hour::<_, ()>.parse_peek("04"), Ok(("", Hour::H4)));

        assert!(hour::<_, ()>.parse_peek("24").is_err());
    }

    #[test]
    fn minute_parser() {
        assert_eq!(minute::<_, ()>.parse_peek("0"), Ok(("", Minute::M0)));
        assert_eq!(minute::<_, ()>.parse_peek("00"), Ok(("", Minute::M0)));
        assert_eq!(minute::<_, ()>.parse_peek("1"), Ok(("", Minute::M1)));
        assert_eq!(minute::<_, ()>.parse_peek("01"), Ok(("", Minute::M1)));
        assert_eq!(minute::<_, ()>.parse_peek("10"), Ok(("", Minute::M10)));
        // ...
        assert_eq!(minute::<_, ()>.parse_peek("59"), Ok(("", Minute::M59)));
        assert_eq!(minute::<_, ()>.parse_peek("60"), Err(()));
    }

    #[test]
    fn second_parser() {
        assert_eq!(second::<_, ()>.parse_peek("0"), Ok(("", Second::S0)));
        assert_eq!(second::<_, ()>.parse_peek("00"), Ok(("", Second::S0)));
        assert_eq!(second::<_, ()>.parse_peek("1"), Ok(("", Second::S1)));
        assert_eq!(second::<_, ()>.parse_peek("01"), Ok(("", Second::S1)));
        assert_eq!(second::<_, ()>.parse_peek("10"), Ok(("", Second::S10)));
        // ...
        assert_eq!(second::<_, ()>.parse_peek("60"), Ok(("", Second::S60)));
        assert_eq!(second::<_, ()>.parse_peek("61"), Err(()));
    }

    #[test]
    fn weekday_parser() {
        assert_eq!(
            weekday::<_, ()>.parse_peek("SU"),
            Ok(("", Weekday::Sunday))
        );

        assert_eq!(
            weekday::<_, ()>.parse_peek("mO"),
            Ok(("", Weekday::Monday))
        );

        assert_eq!(
            weekday::<_, ()>.parse_peek("Tu"),
            Ok(("", Weekday::Tuesday))
        );

        assert_eq!(
            weekday::<_, ()>.parse_peek("we"),
            Ok(("", Weekday::Wednesday))
        );

        assert_eq!(
            weekday::<_, ()>.parse_peek("TH"),
            Ok(("", Weekday::Thursday))
        );

        assert_eq!(
            weekday::<_, ()>.parse_peek("fR"),
            Ok(("", Weekday::Friday))
        );

        assert_eq!(
            weekday::<_, ()>.parse_peek("sa"),
            Ok(("", Weekday::Saturday))
        );
    }

    #[test]
    fn weekday_num_parser() {
        assert!(weekday_num::<_, ()>.parse_peek("MO").is_ok());
        assert!(weekday_num::<_, ()>.parse_peek("12tu").is_ok());
        assert!(weekday_num::<_, ()>.parse_peek("+43Fr").is_ok());
        assert!(weekday_num::<_, ()>.parse_peek("-07SA").is_ok());
    }
}
