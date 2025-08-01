//! Error types for parsing iCalendar.

use crate::model::primitive::{GeoComponent, Integer, Sign};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalendarParseError<S> {
    InvalidRawTime(InvalidRawTimeError),
    InvalidUtcOffset(InvalidUtcOffsetError),
    InvalidDate(InvalidDateError),
    InvalidInteger(InvalidIntegerError),
    InvalidGeo(InvalidGeoError),
    InvalidCompletionPercentage(InvalidCompletionPercentageError),
    InvalidPriority(InvalidPriorityError),
    InvalidDurationTime(InvalidDurationTimeError),
    _Blah(S),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidRawTimeError {
    pub(crate) hours: u8,
    pub(crate) minutes: u8,
    pub(crate) seconds: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidUtcOffsetError {
    NegativeZero,
    BadHours(u8),
    BadMinutes(u8),
    BadSeconds(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidDateError {
    pub(crate) year: u16,
    pub(crate) month: u8,
    pub(crate) day: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidIntegerError {
    pub(crate) sign: Option<Sign>,
    pub(crate) digits: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidGeoError {
    IntegralTooLarge(u8),
    LatOutOfBounds(GeoComponent),
    LonOutOfBounds(GeoComponent),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidCompletionPercentageError(pub(crate) Integer);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidPriorityError(pub(crate) Integer);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InvalidDurationTimeError<T = usize> {
    pub(crate) hours: Option<T>,
    pub(crate) seconds: Option<T>,
}
