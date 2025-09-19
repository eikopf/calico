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
