/// Defines an enum generic over the parameter `S` with [`From`] and [`TryFrom`] impls for each of
/// the variants in the enum. This will cause a compiler error if the same type occurs in multiple
/// variants.
#[macro_export]
macro_rules! define_value_type {
    (
        $(#[$m:meta])*
        $v:vis
        $name:ident ($name_inner:ident)
        { $($variant:ident ($field:ty)),* $(,)? }
    ) => {
        $(#[$m])*
        $v struct $name<S>($name_inner<S>);

        $(#[$m])*
        enum $name_inner <S> {
            $($variant ($field)),*
        }

        $(
            impl<S> From<$field> for $name <S> {
                fn from(value: $field) -> Self {
                    Self($name_inner::$variant(value))
                }
            }
        )*

        $(
            impl<S> TryFrom<$name<S>> for $field {
                type Error = ();

                fn try_from(value: $name<S>) -> Result<$field, Self::Error> {
                    if let $name($name_inner::$variant(x)) = value {
                        Ok(x)
                    } else {
                        Err(())
                    }
                }
            }

            impl<'a, S> TryFrom<&'a $name<S>> for &'a $field {
                type Error = ();

                fn try_from(value: &'a $name<S>) -> Result<&'a $field, Self::Error> {
                    if let $name($name_inner::$variant(x)) = value {
                        Ok(x)
                    } else {
                        Err(())
                    }
                }
            }

            impl<'a, S> TryFrom<&'a mut $name<S>> for &'a mut $field {
                type Error = ();

                fn try_from(value: &'a mut $name<S>) -> Result<&'a mut $field, Self::Error> {
                    if let $name($name_inner::$variant(x)) = value {
                        Ok(x)
                    } else {
                        Err(())
                    }
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! define_value_type_with_mult {
    (
        $(#[$m:meta])*
        $v:vis
        $name:ident
        { $($variant:ident ($field:ty)),* $(,)? }
    ) => {
        ::paste::paste! {
            $(#[$m])*
            $v struct $name<S>([<$name Inner>]<S>);

            $(#[$m])*
            enum [<$name Inner>] <S> {
                $($variant ($crate::model::one_or_seq::OneOrSeq<$field>)),*
            }

            impl<S> PartialEq for $name <S>
            where
                S: PartialEq + $crate::model::table::HashCaseless + $crate::parser::escaped::Equiv,
            {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0
                }
            }

            impl<S> PartialEq for [<$name Inner>] <S>
            where
                S: PartialEq + $crate::model::table::HashCaseless + $crate::parser::escaped::Equiv,
            {
                fn eq(&self, other: &Self) -> bool {
                    match (self, other) {
                        $(
                            (Self::$variant(lhs), Self::$variant(rhs)) => lhs == rhs,
                        )*
                        _ => false,
                    }
                }
            }

            impl<S> $name<S> {
                pub fn is_one(&self) -> bool {
                    match &self.0 {
                        $(
                            [<$name Inner>]::$variant(x) => x.is_one(),
                        )*
                    }
                }

                pub fn is<T>(&self) -> bool where for<'a> &'a Self: TryInto<&'a T> {
                    self.downcast_ref::<'_, T>().is_some()
                }

                pub fn downcast<T>(self) -> Option<T> where Self: TryInto<T> {
                    self.try_into().ok()
                }

                pub fn downcast_ref<'a, T>(&'a self) -> Option<&'a T> where &'a Self: TryInto<&'a T> {
                    self.try_into().ok()
                }

                pub fn downcast_mut<'a, T>(&'a mut self) -> Option<&'a mut T> where &'a mut Self: TryInto<&'a mut T> {
                    self.try_into().ok()
                }
            }

            $(
                impl<S> From<$field> for $name <S> {
                    fn from(value: $field) -> Self {
                        Self([<$name Inner>]::$variant($crate::one_or_seq![value]))
                    }
                }

                impl<S> From<$crate::model::one_or_seq::OneOrSeq<$field>> for $name <S> {
                    fn from(value: $crate::model::one_or_seq::OneOrSeq<$field>) -> Self {
                        Self([<$name Inner>]::$variant(value))
                    }
                }

                impl<S> TryFrom<$name<S>> for $field {
                    type Error = ();

                    fn try_from(value: $name<S>) -> Result<$field, Self::Error> {
                        if let $name([<$name Inner>]::$variant(x)) = value {
                            match x.into_one() {
                                Some(x) => Ok(x),
                                None => Err(()),
                            }
                        } else {
                            Err(())
                        }
                    }
                }

                impl<'a, S> TryFrom<&'a $name<S>> for &'a $field {
                    type Error = ();

                    fn try_from(value: &'a $name<S>) -> Result<&'a $field, Self::Error> {
                        if let $name([<$name Inner>]::$variant(x)) = value {
                            match x.as_one() {
                                Some(x) => Ok(x),
                                None => Err(()),
                            }
                        } else {
                            Err(())
                        }
                    }
                }

                impl<'a, S> TryFrom<&'a mut $name<S>> for &'a mut $field {
                    type Error = ();

                    fn try_from(value: &'a mut $name<S>) -> Result<&'a mut $field, Self::Error> {
                        if let $name([<$name Inner>]::$variant(x)) = value {
                            match x.as_one_mut() {
                                Some(x) => Ok(x),
                                None => Err(()),
                            }
                        } else {
                            Err(())
                        }
                    }
                }

                impl<S> TryFrom<$name<S>> for $crate::model::one_or_seq::OneOrSeq<$field> {
                    type Error = ();

                    fn try_from(value: $name<S>) -> Result<$crate::model::one_or_seq::OneOrSeq<$field>, Self::Error> {
                        if let $name([<$name Inner>]::$variant(x)) = value {
                            Ok(x)
                        } else {
                            Err(())
                        }
                    }
                }

                impl<'a, S> TryFrom<&'a $name<S>> for &'a $crate::model::one_or_seq::OneOrSeq<$field> {
                    type Error = ();

                    fn try_from(value: &'a $name<S>) -> Result<&'a $crate::model::one_or_seq::OneOrSeq<$field>, Self::Error> {
                        if let $name([<$name Inner>]::$variant(x)) = value {
                            Ok(x)
                        } else {
                            Err(())
                        }
                    }
                }

                impl<'a, S> TryFrom<&'a mut $name<S>> for &'a mut $crate::model::one_or_seq::OneOrSeq<$field> {
                    type Error = ();

                    fn try_from(value: &'a mut $name<S>) -> Result<&'a mut $crate::model::one_or_seq::OneOrSeq<$field>, Self::Error> {
                        if let $name([<$name Inner>]::$variant(x)) = value {
                            Ok(x)
                        } else {
                            Err(())
                        }
                    }
                }
            )*
        }
    };
}
