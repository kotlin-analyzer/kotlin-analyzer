#![allow(unused)]

use syn::{
    Token,
    parse::{Parse, ParseStream},
};

use crate::combinators::{InOrder, Separated};

/// [Map]<A, B> parses an input of the shape A1 => B1, A2 => B2,...An => Bn(,)*
/// With optional trailing comma
pub struct Map<K, V>(pub Vec<(K, V)>);

impl<K, V> Parse for Map<K, V>
where
    K: Parse,
    V: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let entries: Separated<InOrder<K, InOrder<Token![=>], V>>, Token![,]> = input.parse()?;
        let items = entries
            .items()
            .map(|InOrder { first, second }| (first, second.second))
            .collect();
        Ok(Map(items))
    }
}

#[cfg(test)]
mod test {
    use syn::parse_quote;
    use syn::{Ident, LitStr};

    use super::*;

    #[test]
    fn test_map() {
        // works for trailing
        let map: Map<Ident, LitStr> = parse_quote! {
            a => "a",
            b => "b",
            c => "c",
        };

        assert_eq!(map.0.len(), 3);
        assert!(map.0.into_iter().all(|(key, value)| key == value.value()));

        // wroks for non-trailing as well
        let map: Map<Ident, LitStr> = parse_quote! {
            a => "a",
            b => "b",
            c => "c"
        };

        assert_eq!(map.0.len(), 3);
        assert!(map.0.into_iter().all(|(key, value)| key == value.value()));
    }
}
