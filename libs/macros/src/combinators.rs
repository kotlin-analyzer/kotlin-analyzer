#![allow(unused)]

use syn::parse::{Parse, ParseStream};

pub struct Seq<T>(pub Vec<T>);
pub struct InOrder<T, P> {
    pub first: T,
    pub second: P,
}

/// This parses a token stream of the shape A, B, C
/// With optional tailing deliminiter. When there no tailing deliminiter,
/// last is set to Option<T>, the last entry.
/// Use only when trailing is optional, but prefer [SeparatedTrailing] or [SeparatedNoTrailing]
pub struct Separated<T, S> {
    items: Vec<(T, S)>,
    last: Option<T>,
}

/// This parses a token stream of the shape A, B, C,
/// The Trailing deliminiter is required
pub struct SeparatedTrailing<T, S> {
    pub items: Vec<(T, S)>,
}

/// This parses a token stream of the shape A, B, C
/// This does not accept a trailing deliminiter
pub struct SeparatedNoTrailing<T, S> {
    pub items: Vec<(T, S)>,
    pub last: T,
}

impl<T, S> Separated<T, S> {
    pub fn items(self) -> impl Iterator<Item = T> {
        let items = self.items.into_iter().map(|e| e.0);
        items.chain(self.last.into_iter())
    }
    // pub fn separators(self) -> impl Iterator<Item = S> {
    //     self.items.into_iter().map(|e| e.1)
    // }
}

impl<T, P> Parse for InOrder<T, P>
where
    T: Parse,
    P: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first: T = input.parse()?;
        let second: P = input.parse()?;
        Ok(InOrder { first, second })
    }
}

impl<T> Parse for Seq<T>
where
    T: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result: Vec<T> = Vec::new();
        while let Ok(next) = input.parse::<T>() {
            result.push(next);
        }

        if result.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "Seq expects at list one matching entry",
            ));
        }

        Ok(Seq(result))
    }
}

pub enum TrailingOpt {
    Optional,
    Enforced,
    Deny,
}

fn parse_separated<T, S>(input: ParseStream, trailing: TrailingOpt) -> syn::Result<Separated<T, S>>
where
    T: Parse,
    S: Parse,
{
    let mut result = Vec::new();

    while input.fork().parse::<InOrder<T, S>>().is_ok() {
        let fs: T = input.parse()?;
        let snd: S = input.parse()?;
        result.push((fs, snd));
    }

    match trailing {
        TrailingOpt::Optional => {
            if let Ok(last) = input.parse::<T>() {
                Ok(Separated {
                    items: result,
                    last: Some(last),
                })
            } else {
                if result.is_empty() {
                    Err(syn::Error::new(
                        input.span(),
                        "Separated expectes at least one match",
                    ))
                } else {
                    Ok(Separated {
                        items: result,
                        last: None,
                    })
                }
            }
        }
        TrailingOpt::Enforced => {
            if result.is_empty() {
                return Err(syn::Error::new(
                    input.span(),
                    "Separated expectes at least one match",
                ));
            }
            return Ok(Separated {
                items: result,
                last: None,
            });
        }
        TrailingOpt::Deny => {
            return input
                .parse::<T>()
                .map_err(|_| {
                    syn::Error::new(
                        input.span(),
                        if result.is_empty() {
                            "Separated expectes at least one match"
                        } else {
                            "unexpected trailing separator"
                        },
                    )
                })
                .map(|last| Separated {
                    items: result,
                    last: Some(last),
                })
        }
    }
}

pub struct Optional<T>(pub Option<T>);

impl<T> Parse for Optional<T>
where
    T: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Optional(input.parse::<T>().ok()))
    }
}

impl<T, S> Parse for Separated<T, S>
where
    T: Parse,
    S: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_separated(input, TrailingOpt::Optional)
    }
}

impl<T, S> Parse for SeparatedTrailing<T, S>
where
    T: Parse,
    S: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_separated(input, TrailingOpt::Enforced).map(|sep| Self { items: sep.items })
    }
}

impl<T, S> Parse for SeparatedNoTrailing<T, S>
where
    T: Parse,
    S: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_separated(input, TrailingOpt::Deny).map(|sep| Self {
            items: sep.items,
            // This should always be Some<T>
            last: sep.last.expect("expected last to be Some"),
        })
    }
}

#[cfg(test)]
mod test {
    use assert_panic::assert_panic;
    use syn::Ident;
    use syn::{parse_quote, Token};

    use super::*;

    #[test]
    fn test_separator() {
        let no_trailing: Separated<Ident, Token![,]> = parse_quote! {
            a , b , c
        };

        assert_eq!(no_trailing.items.len(), 2);
        assert!(matches!(no_trailing.last, Some(_)));
        assert_eq!(no_trailing.items().count(), 3);

        let trailing: Separated<Ident, Token![,]> = parse_quote! {
            a , b , c ,
        };

        assert_eq!(trailing.items.len(), 3);
        assert!(matches!(trailing.last, None));
        assert_eq!(trailing.items().count(), 3);
    }

    #[test]
    fn test_separator_no_trailing() {
        let no_trailing: SeparatedNoTrailing<Ident, Token![,]> = parse_quote! {
            a , b , c
        };

        assert_eq!(no_trailing.items.len(), 2);
        assert_eq!(no_trailing.last.to_string(), "c");

        assert_panic!({
            let _: SeparatedNoTrailing<Ident, Token![,]> = parse_quote! {
                a , b , c,
            };
        });
    }

    #[test]
    fn test_separator_trailing() {
        let trailing: SeparatedTrailing<Ident, Token![,]> = parse_quote! {
            a , b , c,
        };

        assert_eq!(trailing.items.len(), 3);

        assert_panic!({
            let _: SeparatedTrailing<Ident, Token![,]> = parse_quote! {
                a , b , c
            };
        });
    }

    #[test]
    fn test_optional() {
        let optional: Optional<InOrder<Ident, InOrder<Token![@], Ident>>> = parse_quote! {
            ikechukwu@eze
        };

        assert_eq!(optional.0.as_ref().unwrap().first.to_string(), "ikechukwu");
        assert_eq!(optional.0.unwrap().second.second.to_string(), "eze");
    }
}
