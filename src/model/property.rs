//! iCalendar properties.

use std::collections::HashMap;

use super::primitive::{DisplayType, FeatureType, FormatType, ImageData, Language, Uri};

/// An ordinary textual property.
pub type TextProp = Prop<Box<str>, TextParams>;

/// An ordinary image property.
pub type ImageProp = Prop<ImageData, ImageParams>;

/// A conference property.
pub type ConfProp = Prop<Uri, ConfParams>;

/// A property generic over values and parameters.
#[derive(Debug, Clone)]
pub struct Prop<V, P = ()> {
    pub params: P,
    pub value: V,
    pub extra_params: HashMap<Box<str>, Box<str>>,
}

/// The parameters usually associated with text values.
#[derive(Debug, Clone)]
pub struct TextParams {
    pub language: Option<Language>,
    pub alternate_representation: Option<Uri>,
}

/// The parameters usually associated with image data.
#[derive(Debug, Clone)]
pub struct ImageParams {
    pub format_type: Option<FormatType>,
    pub display: Option<DisplayType>,
    pub alternate_representation: Option<Uri>,
}

/// The parameters associated with the CONFERENCE property.
#[derive(Debug, Clone)]
pub struct ConfParams {
    pub feature_type: Vec<FeatureType>,
    pub label: Option<Box<str>>,
    pub language: Option<Language>,
}
