//! The iCalendar object model.
//!
//! # Specification
//!
//! The primary document specifying the iCalendar object model is
//! [RFC 5545](https://datatracker.ietf.org/doc/html/rfc5545), with some later alterations
//! specified in [RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986). Other relevant
//! documents include [RFC 6868](https://www.rfc-editor.org/rfc/rfc6868) and [RFC 7529](https://www.rfc-editor.org/rfc/rfc7529).
//!
//! ## Vestigial Features
//!
//! Some elements of these documents were intended to be variable, but in practice are almost never
//! changed from a single fixed value. Most notably, the CALSCALE (calendar scale) property is
//! essentially always GREGORIAN, and the VERSION (iCalendar version requirement) property is
//! essentially always 2.0.

use component::Component;
use css::Css3Color;
use primitive::{DateTime, Duration, Method, Text, Uid, Uri, Utc};
use property::{ConfProp, ImageProp, Prop, TextProp};
use std::collections::HashMap;

pub mod component;
pub mod css;
pub mod parameter;
pub mod primitive;
pub mod property;
pub mod rrule;

// TODO: replace this with the component-table pattern as in model::component

/// Top-level iCalendar object.
#[derive(Debug, Clone)]
pub struct Calendar<S> {
    /// RFC 5545 §3.7.1 (always GREGORIAN in practice)
    pub calendar_scale: Prop<()>,
    /// RFC 5545 §3.7.2
    pub method: Option<Prop<Method<S>>>,
    /// RFC 5545 §3.7.3
    pub product_id: Prop<Text<S>>,
    /// RFC 5545 §3.7.4 (always 2.0 in practice)
    pub version: Prop<()>,

    /// RFC 7986 §5.1
    pub name: Vec<TextProp<S>>,
    /// RFC 7986 §5.2
    pub description: Vec<TextProp<S>>,
    /// RFC 7986 §5.3
    pub uid: Option<Prop<Uid<S>>>,
    /// RFC 7986 §5.4
    pub last_modified: Option<Prop<DateTime<Utc>>>,
    /// RFC 7986 §5.5
    pub url: Option<Prop<Uri<S>>>,
    /// RFC 7986 §5.6
    pub categories: Vec<TextProp<S>>,
    /// RFC 7986 §5.7
    pub refresh_interval: Option<Prop<Duration>>,
    /// RFC 7986 §5.8
    pub source: Option<Prop<Uri<S>>>,
    /// RFC 7986 §5.9
    pub color: Option<Prop<Css3Color>>,
    /// RFC 7986 §5.10
    pub image: Vec<ImageProp<S>>,
    /// RFC 7986 §5.11
    pub conference: Option<ConfProp<S>>,

    pub components: Vec<Component<S>>,
    pub extra_properties: HashMap<S, Prop<S>>,
}
