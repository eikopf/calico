//! Recurrence rules.

use std::num::NonZero;

use chrono::{DateTime, Utc, Weekday};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecurrenceRule {
    pub freq: Frequency,
    pub interval: Option<u32>,
    pub termination: Option<Termination>,
    pub week_start: Option<Weekday>,
    pub by_rules: ByRules,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Termination {
    Count(u64),
    Until(DateTime<Utc>),
}

/// The frequency of a recurrence rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Frequency {
    Secondly,
    Minutely,
    Hourly,
    Daily,
    Weekly,
    Monthly,
    Yearly,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ByRules {
    // bitsets
    pub by_second: Option<SecondSet>,
    pub by_minute: Option<MinuteSet>,
    pub by_hour: Option<HourSet>,
    pub by_month_day: Option<MonthDaySet>,
    pub by_month: Option<MonthSet>,
    pub by_week_no: Option<WeekNoSet>,

    // boxed vectors
    pub by_day: Option<Box<[WeekdayRule]>>,
    pub by_year_day: Option<Box<[NonZero<i16>]>>,
    pub by_set_pos: Option<Box<[NonZero<i16>]>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WeekdayRule {
    pub weekday: Weekday,
    pub ordinal: Option<i8>,
}

/// A bitset of values from 0 through 60.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SecondSet(NonZero<u64>);

/// A bitset of values from 0 through 59.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MinuteSet(NonZero<u64>);

/// A bitset of values from 0 through 23.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HourSet(NonZero<u32>);

/// A bitset of values from 1 through 12.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MonthSet(NonZero<u16>);

/// A bitset of values from -31 through -1 and from 1 through 31.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MonthDaySet(NonZero<u64>);

/// A bitset of values from -53 through -1 and from 1 through 53.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WeekNoSet(NonZero<u128>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_bitsets() {
        assert_eq!(std::mem::size_of::<SecondSet>(), 8);
    }
}
