//! iCalendar properties.

use std::collections::HashMap;

use super::primitive::{Language, Uri};

/// An ordinary textual property.
pub type TextProp = Prop<Box<str>, TextParams>;

/// A property generic over values and parameters.
#[derive(Debug, Clone)]
pub struct Prop<V, P = ()> {
    pub params: P,
    pub value: V,
    pub extra_params: HashMap<Box<str>, Box<str>>,
}

#[derive(Debug, Clone, Default)]
pub struct TextParams {
    pub language: Option<Language>,
    pub alternate_representation: Option<Uri>,
}
