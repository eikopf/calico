//! The [`WeekdayNumSet`] type.

use std::{collections::BTreeSet, num::NonZero};

use winnow::stream::Accumulate;

use crate::model::primitive::{IsoWeek, Sign, Weekday};

use super::WeekdayNum;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WeekdayNumSet(InnerWDNSet);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum InnerWDNSet {
    Small(BTreeSet<WeekdayNum>),
    Large(Box<FixedWeekdayNumSet>),
}

impl Accumulate<WeekdayNum> for WeekdayNumSet {
    fn initial(capacity: Option<usize>) -> Self {
        Self::with_capacity(capacity.unwrap_or_default())
    }

    fn accumulate(&mut self, weekday_num: WeekdayNum) {
        self.insert(weekday_num);
    }
}

impl WeekdayNumSet {
    /// The maximum number of elements that can be in the small set.
    pub(crate) const SMALL_ELEMENT_LIMIT: usize = 32;

    pub fn is_empty(&self) -> bool {
        match &self.0 {
            InnerWDNSet::Small(set) => set.is_empty(),
            InnerWDNSet::Large(set) => set.is_empty(),
        }
    }

    pub fn len(&self) -> usize {
        match &self.0 {
            InnerWDNSet::Small(set) => set.len(),
            InnerWDNSet::Large(set) => set.len(),
        }
    }

    pub fn contains(&self, weekday_num: WeekdayNum) -> bool {
        match &self.0 {
            InnerWDNSet::Small(set) => set.contains(&weekday_num),
            InnerWDNSet::Large(set) => set.contains(weekday_num),
        }
    }

    pub fn insert(&mut self, weekday_num: WeekdayNum) {
        match &mut self.0 {
            InnerWDNSet::Small(set) => {
                if set.len() <= Self::SMALL_ELEMENT_LIMIT {
                    let _ = set.insert(weekday_num);
                } else {
                    // copy into fixed bitset

                    let mut bitset = Box::new(FixedWeekdayNumSet::EMPTY);
                    bitset.insert(weekday_num);

                    for weekday_num in set.iter() {
                        bitset.insert(*weekday_num);
                    }

                    self.0 = InnerWDNSet::Large(bitset);
                }
            }
            InnerWDNSet::Large(set) => {
                let () = set.insert(weekday_num);
            }
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        if capacity <= Self::SMALL_ELEMENT_LIMIT {
            // BTreeSet doesn't actually have a reserve or with_capacity method
            Self(InnerWDNSet::Small(BTreeSet::new()))
        } else {
            Self(InnerWDNSet::Large(Box::new(FixedWeekdayNumSet::EMPTY)))
        }
    }
}

impl Default for WeekdayNumSet {
    fn default() -> Self {
        Self(InnerWDNSet::Small(Default::default()))
    }
}

/// A bitset of values from 0 through 742. Each byte of the set is a bitset of
/// the weekdays, with each byte mapped to a value of `Option<(Sign, IsoWeek)>`.
/// Since [`Sign`] has 2 members and [`IsoWeek`] has 53 members, there are
/// `2 * 53 + 1 == 107` such bitsets in the overall set.
///
/// The mapping from `Option<(Sign, IsoWeek)>` to `u16` index is relatively
/// simple: `None` is defined as 0, `(Sign::Negative, week)` is assigned to
/// `week as u16` (the range from 0 through 53), and `(Sign::Positive, week)`
/// is assigned to `53 + (week as u16)` (the range from 54 through 106). Within
/// each byte, a [`Weekday`] is mapped to an index from 0 through 6 by its
/// discriminant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FixedWeekdayNumSet([NonZero<u8>; 107]);

impl FixedWeekdayNumSet {
    pub(crate) const EMPTY: Self = Self([NonZero::new(0x80).unwrap(); 107]);

    // NOTE: for these two methods, it's quite important that `self` is never
    // accidentally moved or cloned; we only need to deal with a single byte
    // at a time

    pub fn is_empty(&self) -> bool {
        self == &Self::EMPTY
    }

    pub fn len(&self) -> usize {
        self.0
            .iter()
            .map(|byte| (byte.get().count_ones() as usize) - 1)
            .sum()
    }

    pub const fn contains(&self, weekday_num: WeekdayNum) -> bool {
        let (byte_index, day) = weekday_num_to_index(weekday_num);
        let byte = self.0[byte_index as usize].get();
        let mask = 1 << day;
        (byte & mask) != 0
    }

    pub const fn insert(&mut self, weekday_num: WeekdayNum) {
        let (byte_index, day) = weekday_num_to_index(weekday_num);
        let byte = self.0[byte_index as usize].get();
        let mask = 1 << day;
        // SAFETY: bitwise OR cannot reduce the number of set bits
        let updated = unsafe { NonZero::new_unchecked(byte | mask) };
        self.0[byte_index as usize] = updated;
    }
}

impl Default for FixedWeekdayNumSet {
    fn default() -> Self {
        Self::EMPTY
    }
}

/// Returns the index of `self` into a [`FixedWeekdayNumSet`] as a `(byte, day)`
/// pair.
pub(crate) const fn weekday_num_to_index(weekday_num: WeekdayNum) -> (u8, u8) {
    let day = weekday_num.weekday as u8;

    let byte = match weekday_num.ordinal {
        None => 0,
        Some((Sign::Negative, week)) => week as u8,
        Some((Sign::Positive, week)) => 53 + (week as u8),
    };

    (byte, day)
}

/// Inverse of [`weekday_num_to_index`].
pub(crate) const fn index_to_weekday_num(
    (byte_index, day): (u8, u8),
) -> Option<WeekdayNum> {
    let ordinal = match byte_index {
        0 => None,
        1..=53 => {
            let Some(week) = IsoWeek::from_index(byte_index) else {
                return None;
            };

            Some((Sign::Negative, week))
        }
        54..=106 => {
            let Some(week) = IsoWeek::from_index(byte_index - 53) else {
                return None;
            };

            Some((Sign::Positive, week))
        }
        _ => return None,
    };

    let Some(weekday) = Weekday::from_index(day) else {
        return None;
    };

    Some(WeekdayNum { ordinal, weekday })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_set_empty_is_empty() {
        assert!(FixedWeekdayNumSet::EMPTY.is_empty());
    }

    #[test]
    fn week_day_num_index_conversion_roundtrip() {
        for byte in 0..=106 {
            for day in 0..=6 {
                let wdnum = index_to_weekday_num((byte, day)).unwrap();
                assert_eq!(weekday_num_to_index(wdnum), (byte, day));
            }
        }
    }
}
