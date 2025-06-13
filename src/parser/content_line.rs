//! Parsers for iCalendar content lines.

use winnow::ModalResult;

use crate::model::{primitive::Uid, property::Prop};

pub fn uid_prop(input: &mut &str) -> ModalResult<Prop<Uid>> {
    todo!()
}
