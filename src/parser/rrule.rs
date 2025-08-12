//! Parsers for recurrence rules.

use std::{collections::BTreeSet, num::NonZero};

use winnow::{
    Parser,
    ascii::Caseless,
    combinator::{alt, opt, preceded, separated, terminated},
    error::{FromExternalError, ParserError},
    stream::{AsBStr, AsChar, Compare, Stream, StreamIsPartial},
    token::any,
};

use crate::{
    model::{
        primitive::{Month, Weekday},
        rrule::{
            ByMonthDayRule, ByPeriodDayRules, ByRuleName, CoreByRules, Freq,
            FreqByRules, Hour, HourSet, Interval, Minute, MinuteSet, MonthDay,
            MonthDaySet, MonthDaySetIndex, MonthSet, Part, PartName, RRule,
            Second, SecondSet, Termination, WeekNoSet, WeekNoSetIndex,
            WeekdayNum, YearDayNum, YearlyByRules,
            weekday_num_set::WeekdayNumSet,
        },
    },
    parser::primitive::{digit, iso_week_index, lz_dec_uint, sign},
};

use super::{error::CalendarParseError, primitive::datetime_or_date};

/// Parses an [`RRule`].
pub fn rrule<I, E>(input: &mut I) -> Result<RRule, E>
where
    I: StreamIsPartial
        + Stream
        + Compare<Caseless<&'static str>>
        + Compare<char>,
    I::Slice: AsBStr,
    I::Token: AsChar + Clone,
    E: ParserError<I> + FromExternalError<I, CalendarParseError<I::Slice>>,
{
    #[derive(Default)]
    struct State {
        // BYxxx rules
        by_month: Option<MonthSet>,
        by_week_no: Option<WeekNoSet>,
        by_year_day: Option<BTreeSet<YearDayNum>>,
        by_month_day: Option<MonthDaySet>,
        by_day: Option<WeekdayNumSet>,
        by_hour: Option<HourSet>,
        by_minute: Option<MinuteSet>,
        by_second: Option<SecondSet>,
        by_set_pos: Option<BTreeSet<YearDayNum>>,
        // other elements
        freq: Option<Freq>,
        interval: Option<Interval>,
        termination: Option<Termination>,
        week_start: Option<Weekday>,
    }

    impl State {
        fn try_accept<I, E>(&mut self, input: &I, part: Part) -> Result<(), E>
        where
            I: Stream,
            E: ParserError<I>
                + FromExternalError<I, CalendarParseError<I::Slice>>,
        {
            let part_name = PartName::from(&part);

            match part {
                Part::Freq(freq) => match self.freq {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.freq = Some(freq);
                        Ok(())
                    }
                },
                Part::Until(dt_or_date) => match self.termination {
                    Some(Termination::Count(_)) => Err(E::from_external_error(
                        input,
                        CalendarParseError::CountAndUntilInRRule,
                    )),
                    Some(Termination::Until(_)) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.termination = Some(Termination::Until(dt_or_date));
                        Ok(())
                    }
                },
                Part::Count(count) => match self.termination {
                    Some(Termination::Until(_)) => Err(E::from_external_error(
                        input,
                        CalendarParseError::CountAndUntilInRRule,
                    )),
                    Some(Termination::Count(_)) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.termination = Some(Termination::Count(count));
                        Ok(())
                    }
                },
                Part::Interval(interval) => match self.interval {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.interval = Some(interval);
                        Ok(())
                    }
                },
                Part::BySecond(set) => match self.by_second {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_second = Some(set);
                        Ok(())
                    }
                },
                Part::ByMinute(set) => match self.by_minute {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_minute = Some(set);
                        Ok(())
                    }
                },
                Part::ByHour(set) => match self.by_hour {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_hour = Some(set);
                        Ok(())
                    }
                },
                Part::ByDay(set) => match self.by_day {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_day = Some(set);
                        Ok(())
                    }
                },
                Part::ByMonthDay(set) => match self.by_month_day {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_month_day = Some(set);
                        Ok(())
                    }
                },
                Part::ByYearDay(set) => match self.by_year_day {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_year_day = Some(set);
                        Ok(())
                    }
                },
                Part::ByWeekNo(set) => match self.by_week_no {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_week_no = Some(set);
                        Ok(())
                    }
                },
                Part::ByMonth(set) => match self.by_month {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_month = Some(set);
                        Ok(())
                    }
                },
                Part::BySetPos(set) => match self.by_set_pos {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.by_set_pos = Some(set);
                        Ok(())
                    }
                },
                Part::WkSt(weekday) => match self.week_start {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::DuplicateRRulePart(part_name),
                    )),
                    None => {
                        self.week_start = Some(weekday);
                        Ok(())
                    }
                },
            }
        }

        fn finalize<I, E>(self, input: &I) -> Result<RRule, E>
        where
            I: Stream,
            E: ParserError<I>
                + FromExternalError<I, CalendarParseError<I::Slice>>,
        {
            let State {
                by_month,
                by_week_no,
                by_year_day,
                by_month_day,
                by_day,
                by_hour,
                by_minute,
                by_second,
                by_set_pos,
                freq,
                interval,
                termination,
                week_start,
            } = self;

            // collect the BYxxx rules that are always admissible
            let core_by_rules = CoreByRules {
                by_second,
                by_minute,
                by_hour,
                by_month,
                by_day,
                by_set_pos,
            };

            // decide if the values of by_week_no, by_month_day, and by_year_day
            // are admissible for the given value of freq
            let freq: FreqByRules = match freq {
                None => Err(E::from_external_error(
                    input,
                    CalendarParseError::MissingFreqPart,
                )),
                Some(freq @ Freq::Secondly) => match by_week_no {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByWeekNo,
                        },
                    )),
                    None => Ok(FreqByRules::Secondly(ByPeriodDayRules {
                        by_month_day,
                        by_year_day,
                    })),
                },
                Some(freq @ Freq::Minutely) => match by_week_no {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByWeekNo,
                        },
                    )),
                    None => Ok(FreqByRules::Minutely(ByPeriodDayRules {
                        by_month_day,
                        by_year_day,
                    })),
                },
                Some(freq @ Freq::Hourly) => match by_week_no {
                    Some(_) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByWeekNo,
                        },
                    )),
                    None => Ok(FreqByRules::Hourly(ByPeriodDayRules {
                        by_month_day,
                        by_year_day,
                    })),
                },
                Some(freq @ Freq::Daily) => match (by_week_no, by_year_day) {
                    (None, None) => {
                        Ok(FreqByRules::Daily(ByMonthDayRule { by_month_day }))
                    }
                    (Some(_), _) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByWeekNo,
                        },
                    )),
                    (_, Some(_)) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByYearDay,
                        },
                    )),
                },
                Some(freq @ Freq::Weekly) => {
                    match (by_week_no, by_year_day, by_month_day) {
                        (None, None, None) => Ok(FreqByRules::Weekly),
                        (Some(_), _, _) => Err(E::from_external_error(
                            input,
                            CalendarParseError::UnexpectedByRule {
                                freq,
                                by_rule: ByRuleName::ByWeekNo,
                            },
                        )),
                        (_, Some(_), _) => Err(E::from_external_error(
                            input,
                            CalendarParseError::UnexpectedByRule {
                                freq,
                                by_rule: ByRuleName::ByYearDay,
                            },
                        )),
                        (_, _, Some(_)) => Err(E::from_external_error(
                            input,
                            CalendarParseError::UnexpectedByRule {
                                freq,
                                by_rule: ByRuleName::ByMonthDay,
                            },
                        )),
                    }
                }
                Some(freq @ Freq::Monthly) => match (by_week_no, by_year_day) {
                    (None, None) => Ok(FreqByRules::Monthly(ByMonthDayRule {
                        by_month_day,
                    })),
                    (Some(_), _) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByWeekNo,
                        },
                    )),
                    (_, Some(_)) => Err(E::from_external_error(
                        input,
                        CalendarParseError::UnexpectedByRule {
                            freq,
                            by_rule: ByRuleName::ByYearDay,
                        },
                    )),
                },
                Some(Freq::Yearly) => Ok(FreqByRules::Yearly(YearlyByRules {
                    by_month_day,
                    by_year_day,
                    by_week_no,
                })),
            }?;

            Ok(RRule {
                freq,
                core_by_rules,
                interval,
                termination,
                week_start,
            })
        }
    }

    // try to parse the first part
    let first = part.parse_next(input)?;

    // initialize state and accept the first part
    let mut state = State::default();
    let () = state.try_accept(input, first)?;

    // iterate over the remaining parts and try to accept them
    while let Ok(part) = preceded(';', part::<I, E>).parse_next(input) {
        let () = state.try_accept(input, part)?;
    }

    // finalize into an RRule
    state.finalize(input)
}

/// Parses a [`Part`].
pub fn part<I, E>(input: &mut I) -> Result<Part, E>
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
        PartName::ByYearDay => {
            let set = separated(1.., year_day_num, ',').parse_next(input)?;
            Part::ByYearDay(set)
        }
        PartName::ByWeekNo => {
            let set = separated(1.., week_num, ',').parse_next(input)?;
            Part::ByWeekNo(set)
        }
        PartName::ByMonth => {
            let set = separated(1.., month_num, ',').parse_next(input)?;
            Part::ByMonth(set)
        }
        PartName::BySetPos => {
            let set = separated(1.., year_day_num, ',').parse_next(input)?;
            Part::BySetPos(set)
        }
        PartName::WkSt => {
            let day = weekday.parse_next(input)?;
            Part::WkSt(day)
        }
    })
}

/// Parses a [`PartName`].
pub fn part_name<I, E>(input: &mut I) -> Result<PartName, E>
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

/// Parses a [`Freq`].
pub fn frequency<I, E>(input: &mut I) -> Result<Freq, E>
where
    I: StreamIsPartial + Stream + Compare<Caseless<&'static str>>,
    E: ParserError<I>,
{
    alt((
        Caseless("MINUTELY").value(Freq::Minutely),
        Caseless("SECONDLY").value(Freq::Secondly),
        Caseless("MONTHLY").value(Freq::Monthly),
        Caseless("HOURLY").value(Freq::Hourly),
        Caseless("WEEKLY").value(Freq::Weekly),
        Caseless("YEARLY").value(Freq::Yearly),
        Caseless("DAILY").value(Freq::Daily),
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

    match NonZero::new(value) {
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

    match MonthDay::from_repr(index) {
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

/// Parses a [`Month`].
pub fn month_num<I, E>(input: &mut I) -> Result<Month, E>
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
        Some(month) => Ok(month),
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

    match Hour::from_repr(index) {
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

    match Minute::from_repr(index) {
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

    match Second::from_repr(index) {
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
    use std::collections::BTreeSet;

    use crate::model::{
        primitive::{IsoWeek, Sign},
        rrule::{HourSet, MinuteSet, MonthSet, weekday_num_set::WeekdayNumSet},
    };

    use super::*;

    #[test]
    fn rrule_parser_rfc_5545_page_43() {
        // input is from RFC 5545, page 43
        let input = "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1";
        let res = rrule::<_, ()>.parse_peek(input);

        // TODO: expand this test
    }

    #[test]
    fn rrule_parser_rfc_5545_page_45() {
        // input is from RFC 5545, page 45
        let input =
            "FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30";
        let res = rrule::<_, ()>.parse_peek(input);

        // TODO: expand this test
    }

    #[test]
    fn part_parser_rfc_5545_page_45() -> Result<(), ()> {
        // input is from RFC 5545, page 45
        let input =
            "FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30";
        let (tail, parts): (_, Vec<_>) =
            separated(1.., part, ';').parse_peek(input)?;
        assert!(tail.is_empty());

        let mut month_set = MonthSet::default();
        month_set.set(Month::Jan);

        let mut by_day_set = WeekdayNumSet::default();
        by_day_set.insert(WeekdayNum {
            ordinal: None,
            weekday: Weekday::Sunday,
        });

        let mut hour_set = HourSet::default();
        hour_set.set(Hour::H8);
        hour_set.set(Hour::H9);

        let mut minute_set = MinuteSet::default();
        minute_set.set(Minute::M30);

        let expected_parts = vec![
            Part::Freq(Freq::Yearly),
            Part::Interval(Interval(NonZero::new(2).ok_or(())?)),
            Part::ByMonth(month_set),
            Part::ByDay(by_day_set),
            Part::ByHour(hour_set),
            Part::ByMinute(minute_set),
        ];

        assert_eq!(parts, expected_parts);
        Ok(())
    }

    #[test]
    fn part_parser_rfc_5545_page_43() -> Result<(), ()> {
        // input is from RFC 5545, page 43
        let input = "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1";
        let (tail, parts): (_, Vec<_>) =
            separated(1.., part, ';').parse_peek(input)?;

        assert!(tail.is_empty());

        let mut by_day_set = WeekdayNumSet::default();

        for weekday in Weekday::iter().take(5) {
            by_day_set.insert(WeekdayNum {
                ordinal: None,
                weekday,
            });
        }

        let mut by_set_pos_set = BTreeSet::default();
        let _ = by_set_pos_set.insert(
            YearDayNum::from_signed_index(Sign::Negative, 1).ok_or(())?,
        );

        let expected_parts = vec![
            Part::Freq(Freq::Monthly),
            Part::ByDay(by_day_set),
            Part::BySetPos(by_set_pos_set),
        ];

        assert_eq!(parts, expected_parts);
        Ok(())
    }

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
            Ok(("", Freq::Secondly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("minutely"),
            Ok(("", Freq::Minutely))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("Hourly"),
            Ok(("", Freq::Hourly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("dAILy"),
            Ok(("", Freq::Daily))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("Weekly"),
            Ok(("", Freq::Weekly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("monthly"),
            Ok(("", Freq::Monthly))
        );

        assert_eq!(
            frequency::<_, ()>.parse_peek("YEARLY"),
            Ok(("", Freq::Yearly))
        );

        assert!(frequency::<_, ()>.parse_peek("anything else").is_err());
    }

    #[test]
    fn interval_parser() {
        assert_eq!(
            interval::<_, ()>.parse_peek("1"),
            Ok(("", Interval(std::num::NonZeroU64::MIN)))
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
        assert_eq!(month_num::<_, ()>.parse_peek("1"), Ok(("", Month::Jan)));
        assert_eq!(month_num::<_, ()>.parse_peek("2"), Ok(("", Month::Feb)));
        assert_eq!(month_num::<_, ()>.parse_peek("3"), Ok(("", Month::Mar)));
        assert_eq!(month_num::<_, ()>.parse_peek("4"), Ok(("", Month::Apr)));
        // ...
        assert_eq!(month_num::<_, ()>.parse_peek("11"), Ok(("", Month::Nov)));
        assert_eq!(month_num::<_, ()>.parse_peek("12"), Ok(("", Month::Dec)));

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
