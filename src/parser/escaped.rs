//! A custom [`Stream`](winnow::stream::Stream) for skipping line folds.

use winnow::{
    error::Needed,
    stream::{Checkpoint, Offset, Stream, StreamIsPartial},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Escaped<'a>(&'a [u8]);

impl<'a> std::fmt::Debug for Escaped<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <winnow::BStr as std::fmt::Debug>::fmt(winnow::BStr::new(self.0), f)
    }
}

impl<'a> Offset<Checkpoint<&'a [u8], &'a [u8]>> for Escaped<'a> {
    fn offset_from(&self, other: &Checkpoint<&'a [u8], &'a [u8]>) -> usize {
        self.checkpoint().offset_from(other)
    }
}

impl<'a> Stream for Escaped<'a> {
    type Token = u8;

    type Slice = &'a [u8];

    type IterOffsets = IterOffsets<'a>;

    type Checkpoint = Checkpoint<&'a [u8], &'a [u8]>;

    #[inline(always)]
    fn iter_offsets(&self) -> Self::IterOffsets {
        IterOffsets::new(self.0)
    }

    #[inline(always)]
    fn eof_offset(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    fn next_token(&mut self) -> Option<Self::Token> {
        let (_escapes, tail) = split_fold_prefix(self.0);

        match tail {
            [] => None,
            [t, tail @ ..] => {
                self.0 = tail;
                Some(*t)
            }
        }
    }

    #[inline(always)]
    fn peek_token(&self) -> Option<Self::Token> {
        split_fold_prefix(self.0).1.first().copied()
    }

    #[inline(always)]
    fn offset_for<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Token) -> bool,
    {
        self.iter_offsets()
            .find_map(|(offset, token)| match predicate(token) {
                true => Some(offset),
                false => None,
            })
    }

    #[inline(always)]
    fn offset_at(&self, ntokens: usize) -> Result<usize, Needed> {
        match ntokens {
            0 => Ok(0),
            n => self
                .iter_offsets()
                .nth(n - 1)
                .map(|(offset, _)| offset + 1)
                .ok_or(Needed::Unknown),
        }
    }

    #[inline(always)]
    fn next_slice(&mut self, offset: usize) -> Self::Slice {
        let (head, tail) = self.0.split_at(offset);
        self.0 = tail;

        // NOTE: this isn't quite the expected behaviour, but we're trying to
        // reduce overall string size by stripping escape sequences aggressively
        let (_escapes, slice) = split_fold_prefix(head);
        slice
    }

    #[inline(always)]
    fn peek_slice(&self, offset: usize) -> Self::Slice {
        let (head, _tail) = self.0.split_at(offset);
        let (_escapes, slice) = split_fold_prefix(head);
        slice
    }

    #[inline(always)]
    fn checkpoint(&self) -> Self::Checkpoint {
        self.0.checkpoint()
    }

    #[inline(always)]
    fn reset(&mut self, checkpoint: &Self::Checkpoint) {
        self.0.reset(checkpoint)
    }

    #[inline(always)]
    fn raw(&self) -> &dyn std::fmt::Debug {
        self
    }
}

impl<'a> StreamIsPartial for Escaped<'a> {
    type PartialState = ();

    fn complete(&mut self) -> Self::PartialState {
        // already complete
    }

    fn restore_partial(&mut self, _state: Self::PartialState) {
        // already complete
    }

    fn is_partial_supported() -> bool {
        false
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IterOffsets<'a> {
    offset: usize,
    source: &'a [u8],
}

impl<'a> IterOffsets<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Self { offset: 0, source }
    }
}

impl<'a> Iterator for IterOffsets<'a> {
    type Item = (usize, u8);

    fn next(&mut self) -> Option<Self::Item> {
        match self.source.split_at_checked(self.offset) {
            Some((_, tail)) => {
                let (escape_prefix, tail) = split_fold_prefix(tail);

                match tail {
                    [] => None,
                    [token, ..] => {
                        self.offset += escape_prefix.len();
                        let offset = self.offset;
                        self.offset += 1;
                        Some((offset, *token))
                    }
                }
            }
            None => None,
        }
    }
}

fn split_fold_prefix(input: &[u8]) -> (&[u8], &[u8]) {
    if input.is_empty() {
        return (input, input);
    }

    let i = input
        .chunks_exact(3)
        .position(|xs| {
            !(xs[0] == b'\r'
                && xs[1] == b'\n'
                && (xs[2] == b'\t' || xs[2] == b' '))
        })
        .map(|i| i * 3)
        .unwrap_or(input.len() - input.len() % 3);

    input.split_at(i)
}

#[cfg(test)]
mod tests {
    use winnow::{Parser, token::take};

    use super::*;

    #[test]
    fn take_parser() {
        let input = Escaped("\r\n\ta\r\n b\r\n\tcd\r\n\te".as_bytes());

        assert_eq!(
            take::<usize, _, ()>(0).parse_peek(input),
            Ok((input, "".as_bytes())),
        );

        assert_eq!(
            take::<usize, _, ()>(1).parse_peek(input),
            Ok((Escaped("\r\n b\r\n\tcd\r\n\te".as_bytes()), "a".as_bytes())),
        );

        assert_eq!(
            take::<usize, _, ()>(2).parse_peek(input),
            Ok((Escaped("\r\n\tcd\r\n\te".as_bytes()), "a\r\n b".as_bytes())),
        );

        assert_eq!(
            take::<usize, _, ()>(3).parse_peek(input),
            Ok((Escaped("d\r\n\te".as_bytes()), "a\r\n b\r\n\tc".as_bytes())),
        );

        assert_eq!(
            take::<usize, _, ()>(4).parse_peek(input),
            Ok((Escaped("\r\n\te".as_bytes()), "a\r\n b\r\n\tcd".as_bytes())),
        );

        assert_eq!(
            take::<usize, _, ()>(5).parse_peek(input),
            Ok((Escaped("".as_bytes()), "a\r\n b\r\n\tcd\r\n\te".as_bytes())),
        );
    }

    #[test]
    fn iter_offsets() {
        assert_eq!(
            IterOffsets::new("abc".as_bytes()).collect::<Vec<_>>(),
            vec![(0, b'a'), (1, b'b'), (2, b'c')]
        );

        assert_eq!(
            IterOffsets::new("\r\n\ta\r\n\tbc".as_bytes()).collect::<Vec<_>>(),
            vec![(3, b'a'), (7, b'b'), (8, b'c')]
        );
    }

    #[test]
    fn next_slice() {
        let mut input = Escaped("\r\n\ta\r\n b\r\n\tcd\r\n\te".as_bytes());
        assert_eq!(input.next_slice(0), "".as_bytes());
        assert_eq!(input.next_slice(4), "a".as_bytes());
        assert_eq!(input.next_slice(9), "b\r\n\tcd".as_bytes());
    }

    #[test]
    fn peek_slice() {
        let input = Escaped("\r\n\ta\r\n b\r\n\tcd\r\n\te".as_bytes());
        assert_eq!(input.peek_slice(0), "".as_bytes());
        assert_eq!(input.peek_slice(4), "a".as_bytes());
        assert_eq!(input.peek_slice(8), "a\r\n b".as_bytes());
        assert_eq!(input.peek_slice(13), "a\r\n b\r\n\tcd".as_bytes());
    }

    #[test]
    fn offset_at() {
        let input = Escaped("\r\n\ta\r\n b\r\n\tcd\r\n\te".as_bytes());
        assert_eq!(input.offset_at(0), Ok(0));
        assert_eq!(input.offset_at(1), Ok(4));
        assert_eq!(input.offset_at(2), Ok(8));
        assert_eq!(input.offset_at(3), Ok(12));
        assert_eq!(input.offset_at(4), Ok(13));
        assert_eq!(input.offset_at(5), Ok(17));
        assert_eq!(input.offset_at(6), Err(Needed::Unknown));
        assert_eq!(input.offset_at(7), Err(Needed::Unknown));
        assert_eq!(input.offset_at(8), Err(Needed::Unknown));
    }

    #[test]
    fn peek_token() {
        let mut input = Escaped("\r\n\ta\r\n b".as_bytes());
        assert_eq!(input.peek_token(), Some(b'a'));
        assert_eq!(input.0, "\r\n\ta\r\n b".as_bytes());
        assert_eq!(input.next_token(), Some(b'a'));
        assert_eq!(input.0, "\r\n b".as_bytes());
        assert_eq!(input.peek_token(), Some(b'b'));
        assert_eq!(input.0, "\r\n b".as_bytes());
    }

    #[test]
    fn next_token() {
        assert_eq!(Escaped("".as_bytes()).next_token(), None);
        assert_eq!(Escaped("a".as_bytes()).next_token(), Some(b'a'));
        assert_eq!(Escaped("\r\n\ta".as_bytes()).next_token(), Some(b'a'));
        assert_eq!(Escaped("a\r\n\t".as_bytes()).next_token(), Some(b'a'));

        let mut input = Escaped("\r\n\ta\r\n b".as_bytes());
        assert_eq!(input.next_token(), Some(b'a'));
        assert_eq!(input.0, "\r\n b".as_bytes());
        assert_eq!(input.next_token(), Some(b'b'));
        assert_eq!(input.0, "".as_bytes());
    }

    #[test]
    fn checkpoints() {
        let mut input = Escaped("a\r\n\tb\r\n\tc".as_bytes());
        let start = input.checkpoint();
        assert_eq!(input.next_token(), Some(b'a'));
        assert_eq!(input.next_token(), Some(b'b'));
        assert_eq!(input.0, "\r\n\tc".as_bytes());
        input.reset(&start);
        assert_eq!(input.0, "a\r\n\tb\r\n\tc".as_bytes());
    }

    #[test]
    fn fold_prefix_splitting() {
        assert_eq!(
            split_fold_prefix("".as_bytes()),
            ("".as_bytes(), "".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("a".as_bytes()),
            ("".as_bytes(), "a".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("ab".as_bytes()),
            ("".as_bytes(), "ab".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("abc".as_bytes()),
            ("".as_bytes(), "abc".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n ".as_bytes()),
            ("\r\n ".as_bytes(), "".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n\t".as_bytes()),
            ("\r\n\t".as_bytes(), "".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n\t\r\n".as_bytes()),
            ("\r\n\t".as_bytes(), "\r\n".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n\t\r\n\ta".as_bytes()),
            ("\r\n\t\r\n\t".as_bytes(), "a".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n \r\n\tabcabc".as_bytes()),
            ("\r\n \r\n\t".as_bytes(), "abcabc".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n \r\n\tabcab".as_bytes()),
            ("\r\n \r\n\t".as_bytes(), "abcab".as_bytes()),
        );

        assert_eq!(
            split_fold_prefix("\r\n ab\r\n\t".as_bytes()),
            ("\r\n ".as_bytes(), "ab\r\n\t".as_bytes()),
        );
    }
}
