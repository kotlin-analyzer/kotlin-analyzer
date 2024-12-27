use std::iter::Peekable;

use crate::syntax::SyntaxNode;

use crate::nodes::{Cast, InnerCast};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct ZeroOrMore<T>(Vec<T>);

impl<T> ZeroOrMore<T> {
    pub fn items(&self) -> &[T] {
        &self.0
    }
}

impl<T> InnerCast for Option<T>
where
    T: InnerCast,
{
    fn inner_cast(children: &mut Peekable<impl Iterator<Item = SyntaxNode> + Clone>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut copied = children.clone();

        match T::inner_cast(&mut copied) {
            res @ Some(..) => {
                // bump children
                children.next();
                Some(res)
            }
            None => Some(None),
        }
    }
}

impl<T> InnerCast for ZeroOrMore<T>
where
    T: InnerCast,
{
    fn inner_cast(children: &mut Peekable<impl Iterator<Item = SyntaxNode> + Clone>) -> Option<Self>
    where
        Self: Sized,
    {
        let mut result = Vec::new();
        let mut copied = children.clone();

        loop {
            let Some(value) = T::inner_cast(&mut copied) else {
                break;
            };
            result.push(value);
        }

        for _ in result.iter() {
            // move it forward to catch up
            children.next();
        }

        Some(ZeroOrMore(result))
    }
}

impl<T> InnerCast for T
where
    T: Cast,
{
    fn inner_cast(children: &mut Peekable<impl Iterator<Item = SyntaxNode>>) -> Option<Self>
    where
        Self: Sized,
    {
        children.next().and_then(|n| T::cast(n.clone()))
    }
}

macro_rules! impl_inner_cast_for_tuple {
    ($($type_var: tt),+) => {
        impl<$($type_var),+> InnerCast for ($($type_var),+) where $($type_var: InnerCast),+ {
            fn inner_cast(children: &mut Peekable<impl Iterator<Item = SyntaxNode> + Clone>) -> Option<($($type_var),+)> {

                #[allow(non_snake_case)]
                struct Anonymous<$($type_var),+> {
                    $($type_var: Option<$type_var>),+
                }

                let mut result: Anonymous<$($type_var),+> = Anonymous {
                    $($type_var: None),+
                };

                $(
                    result.$type_var = $type_var::inner_cast(children);
                )+
                // I do not think that it is necessary to assert that we completely consumed the iterator here
                // Since these would be used inline with others T: InnerCast
                return Some(($(result.$type_var?),+));
            }
        }
    };
}

impl_inner_cast_for_tuple!(A, B);
impl_inner_cast_for_tuple!(A, B, C);
impl_inner_cast_for_tuple!(A, B, C, D);
impl_inner_cast_for_tuple!(A, B, C, D, E);
impl_inner_cast_for_tuple!(A, B, C, D, E, F);
impl_inner_cast_for_tuple!(A, B, C, D, E, F, G);
impl_inner_cast_for_tuple!(A, B, C, D, E, F, G, H);
impl_inner_cast_for_tuple!(A, B, C, D, E, F, G, H, I);
impl_inner_cast_for_tuple!(A, B, C, D, E, F, G, H, I, J);
impl_inner_cast_for_tuple!(A, B, C, D, E, F, G, H, I, J, K);
impl_inner_cast_for_tuple!(A, B, C, D, E, F, G, H, I, J, K, L);
