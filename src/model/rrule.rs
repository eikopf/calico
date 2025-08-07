//! Model types for recurrence rules.

use std::{collections::BTreeSet, num::NonZero};

use weekday_num_set::WeekdayNumSet;
use winnow::stream::Accumulate;

use super::primitive::{
    DateTime, DateTimeOrDate, IsoWeek, Month, Sign, Utc, Weekday,
};

// TODO: implement another mixed representation set module for
// year_day_num

pub mod weekday_num_set;

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
    pub by_day: Option<Box<[WeekdayNum]>>,
    pub by_year_day: Option<Box<[NonZero<i16>]>>,
    pub by_set_pos: Option<Box<[NonZero<i16>]>>,
}

/// A signed year of the day, i.e. the range -366..=366 not including 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct YearDayNum(NonZero<i16>);

impl YearDayNum {
    pub const fn from_signed_index(sign: Sign, index: u16) -> Option<Self> {
        match index {
            1..=366 => {
                let value = (index as i16) * (sign as i16);

                // SAFETY: index is certainly non-zero, and sign is ±1; hence
                // their product cannot be zero (nor can it overflow to zero).
                Some(Self(unsafe { NonZero::new_unchecked(value) }))
            }
            _ => None,
        }
    }
}

/// A value corresponding to the `weekdaynum` grammar rule.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WeekdayNum {
    pub ordinal: Option<(Sign, IsoWeek)>,
    pub weekday: Weekday,
}

impl PartialOrd for WeekdayNum {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WeekdayNum {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // the product of (sign as i16) with (week as i16) is a number in
        // the range from -53 to +53 (not including zero), so i16::MIN is
        // certainly less than the entire range

        let lhs = self
            .ordinal
            .map(|(sign, week)| (sign as i16) * (week as i16))
            .unwrap_or(i16::MIN);

        let rhs = other
            .ordinal
            .map(|(sign, week)| (sign as i16) * (week as i16))
            .unwrap_or(i16::MIN);

        (lhs, self.weekday).cmp(&(rhs, other.weekday))
    }
}

/// A bitset of values from 0 through 60.
///
/// ```text
/// 0                                                          60
/// |                                                           |
/// xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx001 (0-63)
///                                                                |
///                                                               msb
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SecondSet(NonZero<u64>);

impl Accumulate<Second> for SecondSet {
    fn initial(_capacity: Option<usize>) -> Self {
        Self::EMPTY
    }

    fn accumulate(&mut self, second: Second) {
        self.set(second)
    }
}

impl SecondSet {
    pub(crate) const EMPTY: Self = Self(NonZero::new(1 << 63).unwrap());

    pub const fn get(&self, second: Second) -> bool {
        let mask = 1 << (second as u8);
        (self.0.get() & mask) != 0
    }

    pub const fn set(&mut self, second: Second) {
        let mask = 1 << (second as u8);
        let updated = self.0.get() | mask;

        // SAFETY: bitwise OR cannot reduce the number of set bits
        *self = Self(unsafe { NonZero::new_unchecked(updated) })
    }
}

impl Default for SecondSet {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// A second (ℤ mod 61), ranging from S0 through S60.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Second {
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,
    S12,
    S13,
    S14,
    S15,
    S16,
    S17,
    S18,
    S19,
    S20,
    S21,
    S22,
    S23,
    S24,
    S25,
    S26,
    S27,
    S28,
    S29,
    S30,
    S31,
    S32,
    S33,
    S34,
    S35,
    S36,
    S37,
    S38,
    S39,
    S40,
    S41,
    S42,
    S43,
    S44,
    S45,
    S46,
    S47,
    S48,
    S49,
    S50,
    S51,
    S52,
    S53,
    S54,
    S55,
    S56,
    S57,
    S58,
    S59,
    S60,
}

impl Second {
    pub const fn from_repr(repr: u8) -> Option<Self> {
        match repr {
            0..=60 => {
                // SAFETY: the valid discriminants of Self are exactly the
                // values of the range 0..=60.
                Some(unsafe { std::mem::transmute::<u8, Self>(repr) })
            }
            _ => None,
        }
    }
}

/// A bitset of values from 0 through 59.
///
/// ```text
/// 0                                                         59
/// |                                                          |
/// xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx0001 (0-63)
///                                                                |
///                                                               msb
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MinuteSet(NonZero<u64>);

impl Accumulate<Minute> for MinuteSet {
    fn initial(_capacity: Option<usize>) -> Self {
        Self::EMPTY
    }

    fn accumulate(&mut self, minute: Minute) {
        self.set(minute)
    }
}

impl MinuteSet {
    pub(crate) const EMPTY: Self = Self(NonZero::new(1 << 63).unwrap());

    pub const fn get(&self, minute: Minute) -> bool {
        let mask = 1 << (minute as u8);
        (self.0.get() & mask) != 0
    }

    pub const fn set(&mut self, minute: Minute) {
        let mask = 1 << (minute as u8);
        let updated = self.0.get() | mask;

        // SAFETY: bitwise OR cannot reduce the number of set bits
        *self = Self(unsafe { NonZero::new_unchecked(updated) })
    }
}

impl Default for MinuteSet {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// A minute (ℤ mod 60), ranging from M0 through M59.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Minute {
    M0,
    M1,
    M2,
    M3,
    M4,
    M5,
    M6,
    M7,
    M8,
    M9,
    M10,
    M11,
    M12,
    M13,
    M14,
    M15,
    M16,
    M17,
    M18,
    M19,
    M20,
    M21,
    M22,
    M23,
    M24,
    M25,
    M26,
    M27,
    M28,
    M29,
    M30,
    M31,
    M32,
    M33,
    M34,
    M35,
    M36,
    M37,
    M38,
    M39,
    M40,
    M41,
    M42,
    M43,
    M44,
    M45,
    M46,
    M47,
    M48,
    M49,
    M50,
    M51,
    M52,
    M53,
    M54,
    M55,
    M56,
    M57,
    M58,
    M59,
}

impl Minute {
    pub const fn from_repr(repr: u8) -> Option<Self> {
        match repr {
            0..=59 => {
                // SAFETY: the discriminants of Self are exactly the values
                // in the range 0..=59.
                Some(unsafe { std::mem::transmute::<u8, Self>(repr) })
            }
            _ => None,
        }
    }
}

/// A bitset of values from 0 through 23.
///
/// ```text
/// 0                      23
/// |                      |
/// xxxxxxxxxxxxxxxxxxxxxxxx00000001 (0-31)
///                                |
///                               msb
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HourSet(NonZero<u32>);

impl Accumulate<Hour> for HourSet {
    fn initial(_capacity: Option<usize>) -> Self {
        Self::EMPTY
    }

    fn accumulate(&mut self, hour: Hour) {
        self.set(hour)
    }
}

impl HourSet {
    pub(crate) const EMPTY: Self = Self(NonZero::new(1 << 31).unwrap());

    pub const fn get(&self, hour: Hour) -> bool {
        let mask = 1 << (hour as u8);
        (self.0.get() & mask) != 0
    }

    pub const fn set(&mut self, hour: Hour) {
        let mask = 1 << (hour as u8);
        let updated = self.0.get() | mask;

        // SAFETY: bitwise OR cannot reduce the number of set bits
        *self = Self(unsafe { NonZero::new_unchecked(updated) })
    }
}

impl Default for HourSet {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// An hour of the day, ranging from H0 through H23.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Hour {
    H0,
    H1,
    H2,
    H3,
    H4,
    H5,
    H6,
    H7,
    H8,
    H9,
    H10,
    H11,
    H12,
    H13,
    H14,
    H15,
    H16,
    H17,
    H18,
    H19,
    H20,
    H21,
    H22,
    H23,
}

impl Hour {
    pub const fn from_repr(repr: u8) -> Option<Self> {
        match repr {
            0..=23 => {
                // SAFETY: the discriminants of Self are exactly the values
                // in the range 0..=23.
                Some(unsafe { std::mem::transmute::<u8, Self>(repr) })
            }
            _ => None,
        }
    }
}

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

impl Accumulate<Month> for MonthSet {
    fn initial(_capacity: Option<usize>) -> Self {
        Self::EMPTY
    }

    fn accumulate(&mut self, index: Month) {
        self.set(index)
    }
}

impl MonthSet {
    pub(crate) const EMPTY: Self = Self(NonZero::new(1 << 15).unwrap());

    pub const fn get(&self, index: Month) -> bool {
        let mask = 1 << index.number().get();
        (self.0.get() & mask) != 0
    }

    pub const fn set(&mut self, index: Month) {
        let mask = 1 << index.number().get();
        let updated = self.0.get() | mask;

        // SAFETY: bitwise OR cannot reduce the number of set bits
        *self = Self(unsafe { NonZero::new_unchecked(updated) })
    }
}

impl Default for MonthSet {
    fn default() -> Self {
        Self::EMPTY
    }
}

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
    pub const fn from_repr(repr: u8) -> Option<Self> {
        match repr {
            1..=31 => {
                // SAFETY: the discriminants of Self are exactly the values
                // of the range 1..=31.
                Some(unsafe { std::mem::transmute::<u8, Self>(repr) })
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PartName {
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

impl From<&Part> for PartName {
    fn from(value: &Part) -> Self {
        match value {
            Part::Freq(_) => Self::Freq,
            Part::Until(_) => Self::Until,
            Part::Count(_) => Self::Count,
            Part::Interval(_) => Self::Interval,
            Part::BySecond(_) => Self::BySecond,
            Part::ByMinute(_) => Self::ByMinute,
            Part::ByHour(_) => Self::ByHour,
            Part::ByDay(_) => Self::ByDay,
            Part::ByMonthDay(_) => Self::ByMonthDay,
            Part::ByYearDay(_) => Self::ByYearDay,
            Part::ByWeekNo(_) => Self::ByWeekNo,
            Part::ByMonth(_) => Self::ByMonth,
            Part::BySetPos(_) => Self::BySetPos,
            Part::WkSt(_) => Self::WkSt,
        }
    }
}

/// A variant in the `recur-rule-part` grammar rule.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Part {
    Freq(Frequency),
    Until(DateTimeOrDate),
    Count(u64),
    Interval(Interval),
    BySecond(SecondSet),
    ByMinute(MinuteSet),
    ByHour(HourSet),
    ByDay(WeekdayNumSet),
    ByMonthDay(MonthDaySet),
    ByYearDay(BTreeSet<YearDayNum>),
    ByWeekNo(WeekNoSet),
    ByMonth(MonthSet),
    BySetPos(BTreeSet<YearDayNum>),
    WkSt(Weekday),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn second_set_empty() {
        let empty = SecondSet::default();
        let bitstring = format!("{:b}", empty.0);
        assert_eq!(bitstring.len(), 64);

        let mut chars = bitstring.chars();
        assert_eq!(chars.next(), Some('1'));

        for char in chars {
            assert_eq!(char, '0');
        }
    }

    #[test]
    fn second_from_index() {
        for i in 0..=60 {
            assert!(Second::from_repr(i).is_some());
        }

        for i in 61..=255 {
            assert!(Second::from_repr(i).is_none());
        }
    }

    #[test]
    fn minute_set_empty() {
        let empty = MinuteSet::default();
        let bitstring = format!("{:b}", empty.0);
        assert_eq!(bitstring.len(), 64);

        let mut chars = bitstring.chars();
        assert_eq!(chars.next(), Some('1'));

        for char in chars {
            assert_eq!(char, '0');
        }
    }

    #[test]
    fn minute_from_index() {
        for i in 0..=59 {
            assert!(Minute::from_repr(i).is_some());
        }

        for i in 60..=255 {
            assert!(Minute::from_repr(i).is_none());
        }
    }

    #[test]
    fn hour_set_empty() {
        let empty = HourSet::default();
        let bitstring = format!("{:b}", empty.0);
        assert_eq!(bitstring.len(), 32);

        let mut chars = bitstring.chars();
        assert_eq!(chars.next(), Some('1'));

        for char in chars {
            assert_eq!(char, '0');
        }
    }

    #[test]
    fn hour_set_bit_twiddling() {
        let mut set = HourSet::default();

        let i1 = Hour::H0;
        let i2 = Hour::H3;
        let i3 = Hour::H4;
        let i4 = Hour::H13;
        let i5 = Hour::H22;

        for i in [i1, i2, i3, i4, i5] {
            assert!(!set.get(i));
        }

        for i in [i1, i2, i3, i4, i5] {
            set.set(i);
        }

        for i in [i1, i2, i3, i4, i5] {
            assert!(set.get(i));
        }
    }

    #[test]
    fn month_set_empty() {
        let empty = MonthSet::default();
        let bitstring = format!("{:b}", empty.0);
        assert_eq!(bitstring.len(), 16);

        let mut chars = bitstring.chars();
        assert_eq!(chars.next(), Some('1'));

        for char in chars {
            assert_eq!(char, '0');
        }
    }

    #[test]
    fn month_set_bit_twiddling() {
        let mut month_set = MonthSet::default();

        let i1 = Month::Jan;
        let i2 = Month::Apr;
        let i3 = Month::Aug;
        let i4 = Month::Sep;

        for i in [i1, i2, i3, i4] {
            assert!(!month_set.get(i));
        }

        for i in [i1, i2, i3, i4] {
            month_set.set(i);
        }

        for i in [i1, i2, i3, i4] {
            assert!(month_set.get(i));
        }
    }

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

    #[test]
    fn weekday_num_ord_impl() {
        let none_monday = WeekdayNum {
            ordinal: None,
            weekday: Weekday::Monday,
        };

        let none_tuesday = WeekdayNum {
            ordinal: None,
            weekday: Weekday::Tuesday,
        };

        let none_friday = WeekdayNum {
            ordinal: None,
            weekday: Weekday::Friday,
        };

        assert!(none_monday < none_tuesday);
        assert!(none_monday < none_friday);
        assert!(none_tuesday < none_friday);

        let sub_53_monday = WeekdayNum {
            ordinal: Some((Sign::Negative, IsoWeek::W53)),
            weekday: Weekday::Monday,
        };

        let sub_50_monday = WeekdayNum {
            ordinal: Some((Sign::Negative, IsoWeek::W50)),
            weekday: Weekday::Monday,
        };

        let sub_53_wednesday = WeekdayNum {
            ordinal: Some((Sign::Negative, IsoWeek::W53)),
            weekday: Weekday::Wednesday,
        };

        let sub_50_thursday = WeekdayNum {
            ordinal: Some((Sign::Negative, IsoWeek::W50)),
            weekday: Weekday::Thursday,
        };

        assert!(none_monday < sub_53_wednesday);
        assert!(none_tuesday < sub_53_wednesday);
        assert!(none_friday < sub_53_wednesday);

        assert!(sub_53_monday < sub_53_wednesday);
        assert!(sub_53_monday < sub_50_monday);
        assert!(sub_53_wednesday < sub_50_monday);
        assert!(sub_53_wednesday < sub_50_thursday);
        assert!(sub_50_monday < sub_50_thursday);

        let pos_53_monday = WeekdayNum {
            ordinal: Some((Sign::Positive, IsoWeek::W53)),
            weekday: Weekday::Monday,
        };

        let pos_50_monday = WeekdayNum {
            ordinal: Some((Sign::Positive, IsoWeek::W50)),
            weekday: Weekday::Monday,
        };

        let pos_53_wednesday = WeekdayNum {
            ordinal: Some((Sign::Positive, IsoWeek::W53)),
            weekday: Weekday::Wednesday,
        };

        let pos_50_thursday = WeekdayNum {
            ordinal: Some((Sign::Positive, IsoWeek::W50)),
            weekday: Weekday::Thursday,
        };

        assert!(sub_53_monday < pos_53_monday);
        assert!(sub_50_monday < pos_50_monday);
        assert!(sub_50_thursday < pos_50_thursday);

        assert!(pos_50_thursday < pos_53_wednesday);
        assert!(pos_53_monday < pos_53_wednesday);
    }
}
