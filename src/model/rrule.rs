//! Model types for recurrence rules.

use std::num::NonZero;

use winnow::stream::Accumulate;

use super::primitive::{DateTime, DateTimeOrDate, IsoWeek, Sign, Utc, Weekday};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecurrenceRule {
    pub freq: Frequency,
    pub interval: Option<Interval>,
    pub termination: Option<Termination>,
    pub week_start: Option<Weekday>,
    pub by_rules: ByRules,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Termination {
    Count(u64),
    Until(DateTime<Utc>),
}

/// The value of the INTERVAL rule part.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Interval(pub(crate) NonZero<u64>);

impl Default for Interval {
    fn default() -> Self {
        Self(std::num::NonZeroU64::MIN)
    }
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

/// A bitset of values from 1 through 12. The most significant bit is always set
/// to guarantee that the entire set is never zero.
///
/// ```text
///  1          12
///  |          |   
/// 0xxxxxxxxxxxx001 (0-15)
/// |              |
/// lsb           msb
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MonthSet(NonZero<u16>);

/// A bitset of values from -31 through -1 and from 1 through 31. The most
/// significant bit is always to set to guarantee the entire set is nonzero.
///
/// ```text
///  1                             31                            -31
///  |                             |                              |
/// 0xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx1 (0-63)
/// |                               |                              |
/// lsb                            -1                              msb
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MonthDaySet(NonZero<u64>);

/// A valid index into a [`MonthDaySet`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct MonthDaySetIndex(NonZero<u8>);

impl Accumulate<MonthDaySetIndex> for MonthDaySet {
    fn initial(_capacity: Option<usize>) -> Self {
        Self::EMPTY
    }

    fn accumulate(&mut self, index: MonthDaySetIndex) {
        self.set(index)
    }
}

impl MonthDaySet {
    pub(crate) const EMPTY: Self = Self(NonZero::new(1 << 63).unwrap());

    pub const fn get(&self, index: MonthDaySetIndex) -> bool {
        let mask = 1 << index.0.get();
        (self.0.get() & mask) != 0
    }

    pub const fn set(&mut self, index: MonthDaySetIndex) {
        let mask = 1 << index.0.get();
        let updated = self.0.get() | mask;

        // SAFETY: bitwise OR cannot reduce the number of set bits
        *self = Self(unsafe { NonZero::new_unchecked(updated) })
    }
}

impl MonthDaySetIndex {
    pub const fn from_signed_month_day(sign: Sign, day: MonthDay) -> Self {
        let day = day as u8;
        let offset = match sign {
            Sign::Positive => 0,
            Sign::Negative => 31,
        };

        // SAFETY: (day as u8) lies in the range 1..=31
        Self(unsafe { NonZero::new_unchecked(day + offset) })
    }
}

impl Default for MonthDaySet {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// A bitset of values from -53 through -1 and from 1 through 53. The highest
/// bit is always set so we can guarantee the entire bitset is never 0.
///
/// ```text
///  1                                                   53
///  |                                                   |
/// 0xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx0000000000 (0-63)
/// 0xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx0000000001 (64-127)
///  |                                                   |         |
/// -1                                                  -53       msb
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WeekNoSet(NonZero<u128>);

/// A valid index into a [`WeekNoSet`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct WeekNoSetIndex(NonZero<u8>);

impl Accumulate<WeekNoSetIndex> for WeekNoSet {
    fn initial(_capacity: Option<usize>) -> Self {
        Self::EMPTY
    }

    fn accumulate(&mut self, index: WeekNoSetIndex) {
        self.set(index)
    }
}

impl WeekNoSet {
    pub(crate) const EMPTY: Self = Self(NonZero::new(1 << 127).unwrap());

    pub const fn get(&self, index: WeekNoSetIndex) -> bool {
        let mask = 1 << (index.0.get());
        (mask & self.0.get()) != 0
    }

    pub const fn set(&mut self, index: WeekNoSetIndex) {
        let mask = 1 << (index.0.get());
        let updated = mask | self.0.get();

        // SAFETY: bitwise OR cannot reduce the number of set bits
        *self = Self(unsafe { NonZero::new_unchecked(updated) })
    }
}

impl WeekNoSetIndex {
    pub const fn from_signed_week(sign: Sign, week: IsoWeek) -> Self {
        let week = week as u8;
        let offset = match sign {
            Sign::Positive => 0,
            Sign::Negative => 64,
        };

        // SAFETY: (week as u8) is guaranteed to lie in the range 1..=53
        Self(unsafe { NonZero::new_unchecked(week + offset) })
    }
}

impl Default for WeekNoSet {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// A particular day in a month, ranging from D1 to D31.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum MonthDay {
    D1 = 1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    D9,
    D10,
    D11,
    D12,
    D13,
    D14,
    D15,
    D16,
    D17,
    D18,
    D19,
    D20,
    D21,
    D22,
    D23,
    D24,
    D25,
    D26,
    D27,
    D28,
    D29,
    D30,
    D31,
}

impl MonthDay {
    pub const fn from_index(index: u8) -> Option<Self> {
        match index {
            1..=31 => Some(unsafe { std::mem::transmute::<u8, Self>(index) }),
            _ => None,
        }
    }
}

/// A variant in the `recur-rule-part` grammar rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Part {
    Freq(Frequency),
    Until(DateTimeOrDate),
    Count(u64),
    Interval(Interval),
    BySecond(SecondSet),
    ByMinute(MinuteSet),
    ByHour(HourSet),
    ByDay(Option<Box<[WeekdayRule]>>),
    ByMonthDay(MonthDaySet),
    ByYearDay(Option<Box<[NonZero<i16>]>>),
    ByWeekNo(WeekNoSet),
    ByMonth(MonthSet),
    BySetPos(Option<Box<[NonZero<i16>]>>),
    WkSt(Weekday),
}

impl Part {
    pub const fn name(&self) -> PartName {
        match self {
            Part::Freq(_) => PartName::Freq,
            Part::Until(_) => PartName::Freq,
            Part::Count(_) => PartName::Count,
            Part::Interval(_) => PartName::Interval,
            Part::BySecond(_) => PartName::BySecond,
            Part::ByMinute(_) => PartName::ByMinute,
            Part::ByHour(_) => PartName::ByHour,
            Part::ByDay(_) => PartName::ByDay,
            Part::ByMonthDay(_) => PartName::ByMonthDay,
            Part::ByYearDay(_) => PartName::ByYearDay,
            Part::ByWeekNo(_) => PartName::ByWeekNo,
            Part::ByMonth(_) => PartName::ByMonth,
            Part::BySetPos(_) => PartName::BySetPos,
            Part::WkSt(_) => PartName::WkSt,
        }
    }
}

/// The name of a variant in the `recur-rule-part` grammar rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PartName {
    Freq,
    Until,
    Count,
    Interval,
    BySecond,
    ByMinute,
    ByHour,
    ByDay,
    ByMonthDay,
    ByYearDay,
    ByWeekNo,
    ByMonth,
    BySetPos,
    WkSt,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn month_day_set_empty() {
        let empty = MonthDaySet::default();
        let bitstring = format!("{:b}", empty.0);
        assert_eq!(bitstring.len(), 64);

        let mut chars = bitstring.chars();
        assert_eq!(chars.next(), Some('1'));

        for char in chars {
            assert_eq!(char, '0');
        }
    }

    #[test]
    fn month_day_set_index_from_signed_month_day() {
        assert_eq!(
            MonthDaySetIndex::from_signed_month_day(
                Sign::Positive,
                MonthDay::D1
            )
            .0
            .get(),
            1
        );

        assert_eq!(
            MonthDaySetIndex::from_signed_month_day(
                Sign::Positive,
                MonthDay::D31
            )
            .0
            .get(),
            31
        );

        assert_eq!(
            MonthDaySetIndex::from_signed_month_day(
                Sign::Negative,
                MonthDay::D1
            )
            .0
            .get(),
            31 + 1
        );

        assert_eq!(
            MonthDaySetIndex::from_signed_month_day(
                Sign::Negative,
                MonthDay::D31
            )
            .0
            .get(),
            31 + 31
        );
    }

    #[test]
    fn month_day_set_bit_twiddling() {
        let mut month_day_set = MonthDaySet::default();

        let i1 = MonthDaySetIndex::from_signed_month_day(
            Sign::Positive,
            MonthDay::D1,
        );
        let i2 = MonthDaySetIndex::from_signed_month_day(
            Sign::Positive,
            MonthDay::D25,
        );
        let i3 = MonthDaySetIndex::from_signed_month_day(
            Sign::Negative,
            MonthDay::D6,
        );

        for i in [i1, i2, i3] {
            assert!(!month_day_set.get(i));
        }

        for i in [i1, i2, i3] {
            month_day_set.set(i);
        }

        for i in [i1, i2, i3] {
            assert!(month_day_set.get(i));
        }
    }

    #[test]
    fn week_no_set_empty() {
        let empty = WeekNoSet::default();
        let bitstring = format!("{:b}", empty.0);
        assert_eq!(bitstring.len(), 128);

        let mut chars = bitstring.chars();
        assert_eq!(chars.next(), Some('1'));

        for char in chars {
            assert_eq!(char, '0');
        }
    }

    #[test]
    fn week_no_set_index_from_signed_week() {
        assert_eq!(
            WeekNoSetIndex::from_signed_week(Sign::Positive, IsoWeek::W1),
            WeekNoSetIndex(NonZero::new(1).unwrap())
        );

        assert_eq!(
            WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W1),
            WeekNoSetIndex(NonZero::new(64 + 1).unwrap())
        );

        assert_eq!(
            WeekNoSetIndex::from_signed_week(Sign::Positive, IsoWeek::W53),
            WeekNoSetIndex(NonZero::new(53).unwrap())
        );

        assert_eq!(
            WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W53),
            WeekNoSetIndex(NonZero::new(64 + 53).unwrap())
        );
    }

    #[test]
    fn week_no_set_bit_twiddling() {
        let mut week_no_set = WeekNoSet::default();

        let i1 = WeekNoSetIndex::from_signed_week(Sign::Positive, IsoWeek::W12);
        let i2 = WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W8);
        let i3 = WeekNoSetIndex::from_signed_week(Sign::Negative, IsoWeek::W37);

        for i in [i1, i2, i3] {
            assert!(!week_no_set.get(i));
        }

        for i in [i1, i2, i3] {
            week_no_set.set(i);
        }

        for i in [i1, i2, i3] {
            assert!(week_no_set.get(i));
        }
    }
}
