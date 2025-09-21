//! The [`OneOrSeq`] collection type.

use std::ops::Deref;

use smallvec::SmallVec;

/// A sequence of values that may be stored on the stack if it contains exactly one value.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct OneOrSeq<T>(SmallVec<[T; 1]>);

impl<T> Deref for OneOrSeq<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

impl<T> OneOrSeq<T> {
    pub fn new() -> Self {
        Self(SmallVec::new())
    }

    pub fn from_elem(elem: T, n: usize) -> Self
    where
        T: Clone,
    {
        Self(SmallVec::from_elem(elem, n))
    }

    pub fn from_vec(vec: Vec<T>) -> Self {
        Self(SmallVec::from_vec(vec))
    }

    pub fn is_one(&self) -> bool {
        self.len() == 1
    }

    pub fn as_one(&self) -> Option<&T> {
        match self.0.len() {
            1 => self.0.first(),
            _ => None,
        }
    }

    pub fn as_one_mut(&mut self) -> Option<&mut T> {
        match self.0.len() {
            1 => self.0.first_mut(),
            _ => None,
        }
    }

    pub fn into_one(mut self) -> Option<T> {
        match self.0.len() {
            1 => self.0.pop(),
            _ => None,
        }
    }

    pub fn as_slice(&self) -> &[T] {
        self.0.as_slice()
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        self.0.as_mut_slice()
    }

    pub fn push(&mut self, value: T) {
        self.0.push(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }
}

pub struct IntoIter<T>(smallvec::IntoIter<[T; 1]>);

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<T> IntoIterator for OneOrSeq<T> {
    type Item = T;

    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self.0.into_iter())
    }
}

/// Creates a [`OneOrSeq`] with the given arguments. This macro uses the same syntax as the [`vec`]
/// macro.
#[macro_export]
macro_rules! one_or_seq {
    () => {
        $crate::model::one_or_seq::OneOrSeq::new()
    };
    ($elem:expr; $n:expr) => {
        $crate::model::one_or_seq::OneOrSeq::from_elem($elem, $n)
    };
    ($($x:expr),+ $(,)?) => {
        $crate::model::one_or_seq::OneOrSeq::from_vec(::std::vec![$($x),+])
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn one_or_seq_macro() {
        let empty: OneOrSeq<u8> = one_or_seq![];
        let repeat = one_or_seq![true; 10];
        let explicit = one_or_seq![1, 2, 3, 4, 5, 6, 7u16];

        assert!(empty.is_empty());
        assert_eq!(repeat.len(), 10);
        assert_eq!(explicit.len(), 7);
    }
}
