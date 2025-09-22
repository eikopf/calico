//! Parser configurations

use std::convert::Infallible;

use crate::{model::parameter::ParamValue, parser::error::CalendarParseError};

/// A trait providing customizable behaviour for a parser.
pub trait Config {
    /// The error type for [`Config::handle_duplicate_param`].
    type DuplicateParamErr<S>: Into<CalendarParseError<S>>;

    /// Updates the value of an existing parameter when a duplicate parameter is encountered. The
    /// default behaviour simply appends the `new_value` to the `previous_value` and returns
    /// `Ok(())`, but (for example) a simpler implementation might just replace the `previous_value`
    /// with the `new_value` (this is the behaviour of `ical.js`).
    fn handle_duplicate_param<S>(
        &mut self,
        previous_value: &mut Vec<ParamValue<S>>,
        mut new_value: Vec<ParamValue<S>>,
    ) -> Result<(), Self::DuplicateParamErr<S>> {
        previous_value.append(&mut new_value);
        Ok(())
    }
}

/// A unit struct that implements [`Config`].
#[derive(Debug, Clone, Copy)]
pub struct DefaultConfig;

impl Config for DefaultConfig {
    type DuplicateParamErr<S> = Infallible;
}
