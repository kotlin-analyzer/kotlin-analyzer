use inflector::Inflector;
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{Error, Ident, Result};

use crate::{
    name::{IntoName, NameCtx},
    parse::{BasicParseEntry, GenAst, ParseEntry, ParseEntryExt, TopLevelParseEntry},
};

trait Generate {
    fn generate(&self, ctx: &NameCtx, is_nested: bool) -> Result<TokenStream>;
}

trait GenerateWithConfig {
    fn generate(&self, parent: &Ident, pos: usize, is_nested: bool) -> Result<TokenStream>;
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

        let is_enum = asts.len() == 1 && asts.first().map(|e| e.is_enum()).unwrap_or_default();

        let entry_gen = asts
            .into_iter()
            .map(|e| e.prepare_for_generation())
            .enumerate()
            .map(|(pos, e)| e.generate(&typename, pos, false))
            .collect::<Result<Vec<_>>>()?;

        let entry_gen = entry_gen.into_iter().reduce(|acc, next| {
            quote! {
                #acc
                #next
            }
        });

        let syntax_name = format_ident!("{}", field.name.to_string().to_screaming_snake_case());

        let field_cast = if !is_enum {
            quote! {
                impl Cast for #typename {
                    pub fn cast(node: SyntaxNode) -> Option<Self> {
                        if node.kind() == ::syntax::SyntaxKind::#syntax_name {
                            Some(Self(node))
                        } else {
                            None
                        }
                    }
                }
            }
        } else {
            quote!()
        };

        Ok(quote! {
            #field_cast
            #entry_gen
        })
    }
}

impl GenerateWithConfig for ParseEntryExt {
    fn generate(&self, parent: &Ident, pos: usize, is_nested: bool) -> Result<TokenStream> {
        let config_name = self.get_config_name();
        self.entry
            .generate(&NameCtx::new(parent, pos, &config_name), is_nested)
    }
}

impl ParseEntry {
    fn prepare_for_generation(self) -> Self {
        match &self {
            ParseEntry::Basic(basic) => match basic {
                BasicParseEntry::CharLit(..)
                | BasicParseEntry::StrLit(..)
                | BasicParseEntry::Ident(..) => self,
                BasicParseEntry::Optional {
                    entries,
                    bracket_token,
                } => Self::Basic(BasicParseEntry::Optional {
                    bracket_token: *bracket_token,
                    entries: entries
                        .into_iter()
                        .map(|e| e.clone().prepare_for_generation())
                        .collect(),
                }),
                BasicParseEntry::Repeated {
                    entries,
                    brace_token,
                } => Self::Basic(BasicParseEntry::Repeated {
                    brace_token: *brace_token,
                    entries: entries
                        .into_iter()
                        .map(|e| e.clone().prepare_for_generation())
                        .collect(),
                }),
                BasicParseEntry::Group { entries, .. } => {
                    if entries.len() == 1 {
                        entries[0].entry.clone()
                    } else {
                        self
                    }
                }
            },
            ParseEntry::Choice { .. } => self,
        }
    }
}

impl ParseEntryExt {
    fn prepare_for_generation(self) -> Self {
        let Self { entry, .. } = &self;

        if matches!(
            entry,
            ParseEntry::Basic(basic)
                if matches!(
                    basic,
                    BasicParseEntry::CharLit(..)
                    | BasicParseEntry::StrLit(..)
                    | BasicParseEntry::Ident(..)
                )
        ) {
            return self;
        }

        if matches!(
            entry,
            ParseEntry::Basic(BasicParseEntry::Group {entries, ..}) if entries.len() > 1
        ) {
            return self;
        }

        let Self { config, entry } = self;

        match entry {
            ParseEntry::Basic(BasicParseEntry::Optional {
                entries,
                bracket_token,
            }) => Self {
                config,
                entry: ParseEntry::Basic(BasicParseEntry::Optional {
                    bracket_token,
                    entries: entries
                        .into_iter()
                        .map(|e| e.clone().prepare_for_generation())
                        .collect(),
                }),
            },
            ParseEntry::Basic(BasicParseEntry::Repeated {
                entries,
                brace_token,
            }) => Self {
                config,
                entry: ParseEntry::Basic(BasicParseEntry::Repeated {
                    brace_token,
                    entries: entries
                        .into_iter()
                        .map(|e| e.clone().prepare_for_generation())
                        .collect(),
                }),
            },
            ParseEntry::Basic(BasicParseEntry::Group { entries, .. }) => {
                let first = entries.into_iter().next().unwrap();
                let Self { entry, .. } = first.prepare_for_generation();
                Self { config, entry }
            }
            ParseEntry::Choice { entries } => Self {
                config,
                entry: ParseEntry::Choice {
                    entries: entries
                        .into_iter()
                        .map(|e| e.clone().prepare_for_generation())
                        .collect(),
                },
            },
            _ => unreachable!(),
        }
    }
}

impl Generate for ParseEntry {
    fn generate(&self, ctx: &NameCtx, is_nested: bool) -> Result<TokenStream> {
        match self {
            ParseEntry::Basic(basic) => basic.generate(ctx, is_nested),
            ParseEntry::Choice { entries, .. } => {
                // Generating for Choice is a bit tricky
                let span = self.span();
                let name = self.into_name(&ctx)?;

                // We need to know what the target is.
                // If it is nested, then it is the name of the

                // TODO: it might be best to always use parent here, check generated code
                // This is because nested enum always tend to be inside a group
                let node = if is_nested {
                    name.type_name()
                } else {
                    ctx.parent.clone()
                };

                let kind_name = Ident::new(&format!("{}Kind", node.to_string()), node.span());

                let children: Vec<_> = entries.iter().enumerate().collect();

                let children_names = self.get_children_names(&node)?;

                let impl_block = {
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

                    quote_spanned! {span=>
                        impl Cast for #node {
                            fn cast(node: ::syntax::SyntaxNode) -> Option<Self> {
                                if #(#casts(node.clone()).is_some())||* {
                                    Some(Self(node))
                                } else {
                                    None
                                }
                            }
                        }

                        impl #node {
                            pub fn kind(&self) -> #kind_name {
                                #first_cast
                                #(.or(#casts_for_kind))*
                                .unwrap()
                            }
                        }
                    }
                };

                let enum_type = {
                    let variants = children_names.iter().map(|e| {
                        let ty = e.type_name();
                        quote!(#ty(::syntax::#ty))
                    });

                    quote! {
                        #[derive(PartialEq, Eq, Hash, Clone)]
                        pub enum #kind_name {
                            #(#variants),*
                        }
                    }
                };

                let new_types = if is_nested {
                    let parent = &ctx.parent;
                    let method_name = name.method_name();

                    quote! {
                        #[derive(PartialEq, Eq, Hash, Clone)]
                        #[repr(transparent)]
                        pub struct #node(::syntax::SyntaxNode);

                        impl #parent {
                            pub fn #method_name(&self) -> Option<#node> {
                                self.0.children().find_map(#node::cast)
                            }
                        }
                    }
                } else {
                    quote!()
                };

                let children_gen = children
                    .iter()
                    .filter(|&&(_, e)| e.entry.is_composite())
                    .map(|&(pos, e)| e.generate(&node, pos, true))
                    .collect::<Result<Vec<_>>>()?;

                Ok(quote! {
                    #enum_type

                    #impl_block

                    #new_types

                    #(#children_gen)*
                })
            }
        }
    }
}

impl BasicParseEntry {
    fn inner_cast_type(&self, parent: &Ident) -> Result<TokenStream> {
        match self {
            BasicParseEntry::Optional { .. }
            | BasicParseEntry::Repeated { .. }
            | BasicParseEntry::Group { .. } => {
                let node_children = self
                    .get_children_names(parent)?
                    .into_iter()
                    .map(|n| n.type_name())
                    .collect::<Vec<_>>();

                match self {
                    BasicParseEntry::Optional { .. } => {
                        if node_children.len() == 1 {
                            Ok(quote!(Option::<#(#node_children),*>))
                        } else {
                            Ok(quote!(Option::<(#(#node_children),*)>))
                        }
                    }
                    BasicParseEntry::Repeated { .. } => {
                        if node_children.len() == 1 {
                            Ok(quote!(ZeroOrMore::<#(#node_children),*>))
                        } else {
                            Ok(quote!(ZeroOrMore::<(#(#node_children),*)>))
                        }
                    }
                    BasicParseEntry::Group { .. } => {
                        if node_children.len() == 1 {
                            Ok(quote!(#(#node_children),*))
                        } else {
                            Ok(quote!(<(#(#node_children),*)>))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!("inner cast type should only be called on composite nodes"),
        }
    }
    fn generate_multi(&self, ctx: &NameCtx, is_nested: bool) -> Result<TokenStream> {
        assert!(matches!(
            self,
            BasicParseEntry::Optional { .. }
                | BasicParseEntry::Repeated { .. }
                | BasicParseEntry::Group { .. }
        ));

        match self {
            BasicParseEntry::Optional { entries, .. }
            | BasicParseEntry::Repeated { entries, .. }
            | BasicParseEntry::Group { entries, .. } => {
                let name = self.into_name(ctx)?;
                let span = self.span();

                // generate impl for current parent
                let impl_parent = {
                    let method = name.method_name();
                    let return_type = name.return_type();
                    let iter_fn = name.iter_fn();
                    let cast = name.cast_closure();
                    let parent = &ctx.parent;

                    quote_spanned! {span=>
                        impl #parent {
                            pub fn #method(&self) -> #return_type {
                                self.0.children().#iter_fn(#cast)
                            }
                        }
                    }
                };

                // TODO: Consider opmitizing when it is just one item

                let node_name = name.type_name();

                let node = {
                    let span = self.span();
                    let inner_cast_type = self.inner_cast_type(&node_name)?;

                    let node = quote_spanned! {span=>
                        #[derive(PartialEq, Eq, Hash, Clone)]
                        pub struct #node_name(SyntaxNode);
                    };

                    // TODO: implement InnerCast for nested type
                    quote! {
                        #node

                        impl Cast for #node_name {
                            // we use inner cast here since it is a composite node
                            fn cast(node: SyntaxNode) -> Option<Self> {
                                let mut children = node.children().peekable();

                                #inner_cast_type::inner_cast(&mut children)
                                    .map(|(node)| {
                                        Self(node)
                                })
                            }
                        }
                    }
                };

                let node = if is_nested {
                    node
                } else {
                    quote! {}
                };

                let impl_children = entries
                    .iter()
                    .enumerate()
                    .map(|(pos, e)| e.generate(&node_name, pos, true))
                    .collect::<Result<Vec<_>>>()?;
                // generate impl for sub type and children
                Ok(quote! {
                    #impl_parent

                    #node

                    #(#impl_children)*
                })
            }
            _ => unreachable!(),
        }
    }

    fn generate_for_simple_type(&self, ctx: &NameCtx) -> Result<TokenStream> {
        if !matches!(
            self,
            BasicParseEntry::CharLit(..) | BasicParseEntry::StrLit(..) | BasicParseEntry::Ident(..)
        ) {
            panic!("Expect simple type, got: {:?}", self);
        };

        match self {
            BasicParseEntry::CharLit(..)
            | BasicParseEntry::StrLit(..)
            | BasicParseEntry::Ident(..) => {
                let name = self.into_name(ctx)?;
                let span = self.span();

                let method = name.method_name();
                let return_type = name.return_type();
                let iter_fn = name.iter_fn();
                let cast = name.cast_closure();
                let parent = &ctx.parent;

                Ok(quote_spanned! {span=>
                    impl #parent {
                        pub fn #method(&self) -> #return_type {
                            self.0.children().#iter_fn(#cast)
                        }
                    }
                })
            }
            _ => unreachable!(),
        }
    }
}

impl Generate for BasicParseEntry {
    fn generate(&self, ctx: &NameCtx, is_nested: bool) -> Result<TokenStream> {
        match self {
            BasicParseEntry::CharLit(..)
            | BasicParseEntry::StrLit(..)
            | BasicParseEntry::Ident(..) => self.generate_for_simple_type(ctx),

            BasicParseEntry::Optional { .. }
            | BasicParseEntry::Repeated { .. }
            | BasicParseEntry::Group { .. } => self.generate_multi(ctx, is_nested),
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

        println!("{}", pretty_print(generated));
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
