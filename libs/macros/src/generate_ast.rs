use std::collections::HashMap;

use inflector::Inflector;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{parse_quote, Error, Ident, Result, Type};

use crate::{
    name::{Name, NameCtx, NameForm, ToName},
    parse::{BasicParseEntry, GenAst, ParseEntry, TopLevelParseEntry},
};

const IGNORED: &[&str] = &["nl_token", "nl_tokens"];

// TODO: Add documenation on generated functions

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct ImplKey {
    target: Ident,
    name: String,
}

impl ImplKey {
    fn new(target: Ident, name: String) -> Self {
        Self { target, name }
    }
}

#[derive(Debug, Default)]
struct GeneratedSink {
    impls: HashMap<ImplKey, TokenStream>,
    top_level_items: Vec<TokenStream>,
}

#[derive(Debug, Clone, Copy, Default)]
struct GenContext {
    is_nested: bool,
}

trait Generate {
    fn generate(
        &self,
        sink: &mut GeneratedSink,
        name_ctx: NameCtx,
        gen_ctx: GenContext,
    ) -> Result<()>;
}

impl GenAst {
    pub fn generate(self) -> Result<TokenStream> {
        let entries = self
            .entries
            .into_iter()
            .map(|tp| tp.generate())
            .collect::<Result<Vec<_>>>()?;

        Ok(quote! {
            #(#entries)*
        })
    }
}

impl TopLevelParseEntry {
    pub fn generate(self) -> Result<TokenStream> {
        let Self { field, asts } = self;
        let typename = Ident::new(&field.name.to_string().to_pascal_case(), field.name.span());
        let asts_len = asts.len();

        let name = Name::Single(NameForm::Ident(&typename));

        let mut sink = GeneratedSink::default();

        for (pos, e) in asts.into_iter().enumerate() {
            e.generate(
                &mut sink,
                NameCtx::new(&name, pos),
                GenContext {
                    is_nested: asts_len > 1,
                },
            )?;
        }

        let GeneratedSink {
            impls,
            top_level_items: new_types,
        } = sink;

        let groups = impls
            .into_iter()
            .filter(|entry| !IGNORED.contains(&entry.0.name.as_str()))
            .into_group_map_by(|entry| entry.0.target.to_string());

        let impls = groups.into_values().map(|group| {
            let (targets, impls): (Vec<_>, Vec<_>) = group.into_iter().collect();
            let target = targets.into_iter().next().unwrap().target;

            quote! {
                impl #target {
                    #(#impls)*
                }
            }
        });

        Ok(quote! {
            #(#new_types)*
            #(#impls)*
        })
    }
}

impl Generate for ParseEntry {
    fn generate(
        &self,
        sink: &mut GeneratedSink,
        name_ctx: NameCtx,
        gen_ctx: GenContext,
    ) -> Result<()> {
        let span = self.span();
        let name = self.to_name(name_ctx)?;

        // In case of choice, we need to know what the target is.
        // If it is nested, then it is the name of the
        // TODO: it might be best to always use parent here, check generated code
        // This is because nested enum always tend to be inside a group
        let node = if gen_ctx.is_nested {
            &name
        } else {
            name_ctx.parent
        };

        let children_names = self.get_children_names(node)?;

        match self {
            ParseEntry::Basic(basic) => basic.generate(sink, name_ctx, gen_ctx),
            ParseEntry::Choice { entries, .. } => {
                let node_name = node.type_name();
                let kind_name = Ident::new(&format!("{}Kind", node_name), node.span());

                {
                    let casts = children_names.iter().map(|v| v.cast_closure());

                    let mut casts_for_kind = children_names.iter().map(|v| {
                        let variant_name = v.type_name();
                        let cast = v.cast_closure();
                        quote!(#cast(self.0.clone()).map(#kind_name::#variant_name))
                    });

                    let first_cast = casts_for_kind.next().ok_or_else(|| {
                        Error::new(span, "choice variant expected to have at least one entry")
                    })?;

                    // TODO: implement Innercast for nested

                    if gen_ctx.is_nested {
                        let node = name.type_name();

                        sink.top_level_items.push(quote! {
                            impl Cast for #node {
                                fn cast(node: SyntaxNode) -> Option<Self> {
                                    if #(#casts(node.clone()).is_some())||* {
                                        Some(Self(node))
                                    } else {
                                        None
                                    }
                                }
                            }
                        });
                    }
                    sink.impls.insert(
                        ImplKey::new(node.type_name(), "kind".to_string()),
                        quote! {
                            pub fn kind(&self) -> #kind_name {
                                #first_cast
                                #(.or(#casts_for_kind))*
                                .unwrap()
                            }
                        },
                    )
                };

                {
                    let variants = children_names.iter().map(|e| {
                        let ty = e.type_name();
                        quote!(#ty(#ty))
                    });

                    sink.top_level_items.push(quote! {
                        #[derive(PartialEq, Eq, Hash, Clone)]
                        pub enum #kind_name {
                            #(#variants),*
                        }
                    });
                };

                if gen_ctx.is_nested {
                    let parent_ident = name_ctx.parent.type_name();
                    let method_name = name.method_name();

                    sink.top_level_items.push(quote! {
                        #[derive(PartialEq, Eq, Hash, Clone)]
                        #[repr(transparent)]
                        pub struct #node_name(SyntaxNode);
                    });

                    sink.impls.insert(
                        ImplKey::new(parent_ident, method_name.to_string()),
                        quote! {
                            pub fn #method_name(&self) -> Option<#node_name> {
                                self.0.children().find_map(#node_name::cast)
                            }
                        },
                    );
                }

                entries
                    .iter()
                    .enumerate()
                    .filter(|(_, e)| e.is_composite())
                    .map(|(pos, e)| {
                        e.generate(
                            sink,
                            NameCtx::new(node, pos),
                            GenContext { is_nested: true },
                        )
                    })
                    .collect::<Result<Vec<_>>>()?;

                Ok(())
            }
        }
    }
}

impl BasicParseEntry {
    fn inner_cast_type(&self, name_ctx: NameCtx) -> Result<Type> {
        let name = self.to_name(name_ctx)?;
        let type_name = name.type_name();

        match self {
            BasicParseEntry::Token { .. } | BasicParseEntry::Ident(..) => {
                Ok(parse_quote!(#type_name))
            }
            BasicParseEntry::Group { entries, .. }
            | BasicParseEntry::Repeated { entries, .. }
            | BasicParseEntry::Optional { entries, .. } => {
                let node_children = entries
                    .iter()
                    .enumerate()
                    .map(|(pos, n)| n.inner_cast_type(NameCtx::new(&name, pos)))
                    .collect::<Result<Vec<_>>>()?;

                let children_type: Type = if node_children.len() == 1 {
                    node_children.into_iter().next().unwrap()
                } else {
                    parse_quote!((#(#node_children),*))
                };

                match self {
                    BasicParseEntry::Optional { .. } => Ok(parse_quote!(Option<#children_type>)),
                    BasicParseEntry::Repeated { .. } => {
                        Ok(parse_quote!(ZeroOrMore<#children_type>))
                    }
                    BasicParseEntry::Group { .. } => Ok(children_type),
                    _ => unreachable!(),
                }
            }
        }
    }
    fn generate_multi(&self, sink: &mut GeneratedSink, name_ctx: NameCtx) -> Result<()> {
        match &self {
            BasicParseEntry::Optional { entries, .. }
            | BasicParseEntry::Repeated { entries, .. }
                if entries.len() == 1 && !entries[0].is_composite() =>
            {
                return self.generate_for_simple_type(sink, name_ctx);
            }
            _ => {}
        }

        let name = self.to_name(name_ctx)?;
        let span = self.span();

        // generate impl for current parent, if it is not an enum
        if !name_ctx.parent.is_choice() {
            let method = name.method_name();
            let return_type = name.return_type();
            let iter_fn = name.iter_fn();
            let cast = name.cast_closure();

            sink.impls.insert(
                ImplKey::new(name_ctx.parent.type_name(), method.to_string()),
                quote! {
                    pub fn #method(&self) -> #return_type {
                        self.0.children().#iter_fn(#cast)
                    }
                },
            );
        };

        let node_name = name.type_name();

        // Todo: use name.is_unknown() instead
        if name.is_unknown() {
            let inner_cast_type = self.inner_cast_type(name_ctx)?;
            let inner_cast_call = utils::type_to_inner_cast(inner_cast_type)?;

            sink.top_level_items.push(quote_spanned! {span=>
                #[derive(PartialEq, Eq, Hash, Clone)]
                pub struct #node_name(SyntaxNode);
            });

            // TODO: implement InnerCast for nested type

            sink.top_level_items.push(quote! {
                impl Cast for #node_name {
                    // we use inner cast here since it is a composite node
                    fn cast(node: SyntaxNode) -> Option<Self> {
                        let mut children = node.children().peekable();

                        #inner_cast_call(&mut children)
                            .map(|(_)| {
                                Self(node)
                        })
                    }
                }
            });
        }

        let (BasicParseEntry::Optional { entries, .. }
        | BasicParseEntry::Repeated { entries, .. }
        | BasicParseEntry::Group { entries, .. }) = self
        else {
            unreachable!()
        };

        entries
            .iter()
            .enumerate()
            .map(|(pos, e)| {
                e.generate(
                    sink,
                    NameCtx::new(&name, pos),
                    GenContext { is_nested: true },
                )
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(())
    }

    fn generate_for_simple_type(&self, sink: &mut GeneratedSink, name_ctx: NameCtx) -> Result<()> {
        let name = self.to_name(name_ctx)?;

        let method = name.method_name();
        let return_type = name.return_type();
        let iter_fn = name.iter_fn();
        let cast = name.cast_closure();

        sink.impls.insert(
            ImplKey::new(name_ctx.parent.type_name(), method.to_string()),
            quote! {
                pub fn #method(&self) -> #return_type {
                    self.0.children().#iter_fn(#cast)
                }
            },
        );

        Ok(())
    }
}

impl ParseEntry {
    fn inner_cast_type(&self, name_ctx: NameCtx) -> Result<Type> {
        match self {
            ParseEntry::Basic(basic) => basic.inner_cast_type(name_ctx),
            ParseEntry::Choice { .. } => {
                let name = self.to_name(name_ctx)?;
                let type_name = name.type_name();
                Ok(parse_quote!(#type_name))
            }
        }
    }
}

impl Generate for BasicParseEntry {
    fn generate(
        &self,
        sink: &mut GeneratedSink,
        name_ctx: NameCtx,
        _gen_ctx: GenContext,
    ) -> Result<()> {
        match self {
            BasicParseEntry::Token { .. } | BasicParseEntry::Ident(..) => {
                self.generate_for_simple_type(sink, name_ctx)
            }

            BasicParseEntry::Optional { .. }
            | BasicParseEntry::Repeated { .. }
            | BasicParseEntry::Group { .. } => self.generate_multi(sink, name_ctx),
        }
    }
}

mod utils {
    use proc_macro2::Span;
    use syn::{parse_quote, ExprPath, PathArguments, PathSegment, Result, Type};

    pub(super) fn type_to_inner_cast(ty: Type) -> Result<ExprPath> {
        match ty {
            Type::Path(type_path) => {
                // we only care about first here, since we have sth like ZeroOrMore<(T1, T2)> or T
                let PathSegment { ident, arguments } =
                    type_path.path.segments.into_iter().next().ok_or_else(|| {
                        syn::Error::new(Span::call_site(), "path should contain a segment")
                    })?;

                if let PathArguments::None = arguments {
                    // TODO: return error here as we cannot have Option::inner_cast
                    Ok(parse_quote!(#ident::inner_cast))
                } else {
                    // All call we want is sth like <(T1, T2)>::inner_cast
                    // This is cos, this expression is only used inside the impl of Cast for a Composite
                    // So for {T} => Cast will be impl'ed for T, so <T>::inner_cast is correct
                    // Same [T] and (T, T2)
                    // TODO: handle when it is a standalone type inisde the angular brackets
                    Ok(parse_quote!(#arguments::inner_cast))
                }
            }
            Type::Tuple(type_tuple) => Ok(parse_quote!(<#type_tuple>::inner_cast)),
            _ => Err(syn::Error::new(
                Span::call_site(),
                "inner cast can only be converted from path and tuple variants",
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proc_macro2::TokenStream;

    fn pretty_print(stream: TokenStream) -> String {
        match syn::parse_file(&stream.to_string()) {
            Ok(tree) => prettyplease::unparse(&tree),
            Err(err) => format!("{}\n{}", &stream.to_string(), err),
        }
    }

    macro_rules! tt {
        ($($tokens: tt)*) => {
            stringify!($($tokens)*).parse::<proc_macro2::TokenStream>()?
        };
    }

    #[test]
    fn temp_test() -> Result<()> {
        let tree: Type = syn::parse_quote! {
            (T, T2)
        };

        let expr: syn::ExprPath = utils::type_to_inner_cast(tree)?;

        dbg!(expr);

        Ok(())
    }

    #[test]
    fn gen_top_level_entry_with_choice() -> Result<()> {
        let stream: TokenStream = tt! {
            expression:
                disjunction
                | another
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;

        let generated = top.generate()?;

        println!("{}", pretty_print(generated));
        Ok(())
    }

    #[test]
    fn gen_from_assignments() -> Result<()> {
        let stream: TokenStream = tt! {
            assignmentAndOperator:
            "+="
            | "-="
            | "*="
            | "/="
            | "%="
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;

        let generated = top.generate()?;

        println!("{}", pretty_print(generated));
        Ok(())
    }

    #[test]
    fn gen_test_prepare() -> Result<()> {
        let stream: TokenStream = tt! {
            fileAnnotation:
            _(AT_NO_WS | AT_PRE_WS)@FileAnnotationAt
            "file"
            {simpleIdentifier}
            {NL}
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;

        let generated = top.generate()?;

        println!("{}", pretty_print(generated));
        Ok(())
    }

    #[test]
    fn gen_test_prepare2() -> Result<()> {
        let stream: TokenStream = tt! {
              memberAccessOperator:
                ({NL} ".")
                | ({NL} safeNav)
                | "::"
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;

        let generated = top.generate()?;

        println!("{}", pretty_print(generated));
        Ok(())
    }

    #[test]
    fn gen_from_complex_unnmamed() -> Result<()> {
        let stream: TokenStream = tt! {
            fileAnnotation:
            (AT_NO_WS | AT_PRE_WS)
            "file"
            {NL}
            ":"
            {NL}
            (("[" (unescapedAnnotation {unescapedAnnotation}) "]") | unescapedAnnotation)
            {NL}
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;

        let generated = top.generate()?;

        println!("{:#}", pretty_print(generated));
        Ok(())
    }

    #[test]
    fn gen_from_complex_named() -> Result<()> {
        let stream: TokenStream = tt! {
            fileAnnotation:
            (AT_NO_WS | AT_PRE_WS)@FileAnnotationAt
            "file"
            {NL}
            ":"
            {NL}
            (("[" (unescapedAnnotation {unescapedAnnotation}) "]")@BoxedUnescapedAnnotation | unescapedAnnotation)@FileUnescapedAnnotation
            {NL}
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;

        let generated = top.generate()?;

        println!("{}", pretty_print(generated));
        Ok(())
    }
}
