//! Traits and utilities for arbitrary order parsing.

use winnow::{
    Parser,
    combinator::trace,
    error::{ModalError, ParserError},
    stream::Stream,
};

pub trait Multiplicity<T> {
    type Count: Default;
    type Acc: Default;
    type Output;

    fn is_satisfied_by(&self, count: &Self::Count) -> bool;
    fn try_accumulate(
        &self,
        acc: &mut Self::Acc,
        count: &mut Self::Count,
        value: T,
    ) -> Result<(), ()>;
    fn finalize(self, acc: Self::Acc, count: Self::Count) -> Option<Self::Output>;
}

pub struct One;
pub struct ZeroOrOne;
pub struct ZeroOrMore;
pub struct OneOrMore;

impl<T> Multiplicity<T> for One {
    type Count = bool;
    type Acc = Option<T>;
    type Output = T;

    fn is_satisfied_by(&self, count: &Self::Count) -> bool {
        *count
    }

    fn try_accumulate(
        &self,
        acc: &mut Self::Acc,
        count: &mut Self::Count,
        value: T,
    ) -> Result<(), ()> {
        if *count {
            Err(())
        } else {
            let prev = acc.replace(value);
            debug_assert!(prev.is_none());
            *count = true;
            Ok(())
        }
    }

    fn finalize(self, acc: Self::Acc, _count: Self::Count) -> Option<Self::Output> {
        acc
    }
}

pub struct MultiParser<T, P, M>
where
    M: Multiplicity<T>,
{
    parser: P,
    multiplicity: M,
    acc: M::Acc,
    count: M::Count,
}

impl<T, P, M> MultiParser<T, P, M>
where
    M: Multiplicity<T>,
{
    pub fn new(parser: P, multiplicity: M) -> Self {
        MultiParser {
            parser,
            multiplicity,
            acc: Default::default(),
            count: Default::default(),
        }
    }

    pub fn try_parse<I, E>(&mut self, input: &mut I) -> winnow::Result<(), E>
    where
        P: Parser<I, T, E>,
        I: Stream,
        E: ParserError<I> + ModalError,
    {
        let value = self.parser.parse_next(input)?;

        match self
            .multiplicity
            .try_accumulate(&mut self.acc, &mut self.count, value)
        {
            Ok(()) => Ok(()),
            Err(()) => Err(E::from_input(input).cut()),
        }
    }

    pub fn finalize(self) -> Option<M::Output> {
        self.multiplicity.finalize(self.acc, self.count)
    }
}

pub fn any_order<I, O, E, List>(mut l: List) -> impl Parser<I, O, E>
where
    I: Stream,
    E: ParserError<I>,
    List: AnyOrder<I, O, E>,
{
    trace("any_order", move |i: &mut I| l.any_order(i))
}

pub trait AnyOrder<I, O, E> {
    fn any_order(self, input: &mut I) -> winnow::Result<O, E>;
}

macro_rules! impl_any_order {
    ($(n:tt $T:ident $P:ident $M:ident),+) => {
        #[allow(non_snake_case)]
        #[allow(non_camel_case_types)]
        impl<I, E, $($T, $P, $M),+> AnyOrder<I, ($($T,)+), E>
            for ($(MultiParser<$T, $P, $M>,)+)
        where
            I: winnow::Stream,
            E: winnow::error::ParserError<I> + winnow::error::ModalError,
            $($P: winnow::Parser<I, $T, E>,)+
            $($M: Multiplicity<$T>,)+
        {
            fn any_order(&mut self, input: &mut I) -> Result<($($T,)+), E> {
                let ($(mut $P,)+) = self;

                loop {
                    let start = input.checkpoint();
                    let mut found = false;

                    $(
                        if !found {
                            if $P.try_parse(input).is_ok() {
                                found = true;
                            } else {
                                input.reset(&start);
                            }
                        }
                    )+

                    if !found {
                        break;
                    }
                }

                Ok(($($P.finalize().map_err(Into::into)?,)+))
            }
        }
    };
}
