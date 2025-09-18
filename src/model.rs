//! The iCalendar object model.
//!
//! # Specification
//!
//! The primary document specifying the iCalendar object model is
//! [RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545), with some later alterations
//! specified in [RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986). Other relevant
//! documents include [RFC 6868](https://www.rfc-editor.org/rfc/rfc6868) and [RFC 7529](https://www.rfc-editor.org/rfc/rfc7529).

pub mod component;
pub mod css;
pub mod parameter;
pub mod primitive;
pub mod property;
pub mod rrule;
pub(crate) mod table;
