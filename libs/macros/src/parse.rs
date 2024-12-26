use std::{
    fmt::{self, Debug},
    ops,
};

use proc_macro2::Span;
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Brace, Bracket, Paren};
use syn::{braced, bracketed, parenthesized, Ident, Lit, LitChar, LitStr, Token};

use crate::combinators::{InOrder, Optional, Seq};

#[derive(Debug, Default, Clone)]
pub(crate) struct Config {
    pub(crate) name: Option<Ident>,
    pub(crate) ignore: bool,
}

impl ops::Add for Config {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            name: self.name.or(rhs.name),
            ignore: self.ignore || rhs.ignore,
        }
    }
}

#[derive(Clone)]
pub(crate) enum BasicParseEntry {
    CharLit(LitChar, Config),
    StrLit(LitStr, Config),
    Ident(Ident, Config),
    Optional {
        bracket_token: Bracket,
        entries: Vec<ParseEntry>,
        config: Config,
    },
    Repeated {
        brace_token: Brace,
        entries: Vec<ParseEntry>,
        config: Config,
    },
    Group {
        paren_token: Paren,
        entries: Vec<ParseEntry>,
        config: Config,
    },
}

#[derive(Clone, Debug)]
pub enum ParseEntry {
    Basic(BasicParseEntry),
    Choice {
        entries: Vec<BasicParseEntry>,
        config: Config,
    },
}

#[derive(Clone, Debug)]
pub struct ParseEntryExt {
    pub config: Config,
    pub entry: ParseEntry,
}

impl fmt::Debug for BasicParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CharLit(lit, ..) => lit.value().fmt(f),
            Self::StrLit(lit, ..) => lit.value().fmt(f),
            Self::Ident(id, ..) => id.fmt(f),
            Self::Optional { entries, .. } => f.debug_tuple("Optional").field(entries).finish(),
            Self::Repeated { entries, .. } => f.debug_tuple("Repeated").field(entries).finish(),
            Self::Group { entries, .. } => f.debug_tuple("Group").field(entries).finish(),
        }
    }
}

impl BasicParseEntry {
    fn flatten(self) -> Self {
        match self {
            BasicParseEntry::Optional {
                bracket_token,
                entries,
                config,
            } => BasicParseEntry::Optional {
                bracket_token,
                entries: entries.into_iter().map(|e| e.flatten()).collect(),
                config,
            },
            BasicParseEntry::Repeated {
                brace_token,
                entries,
                config,
            } => BasicParseEntry::Repeated {
                brace_token,
                entries: entries.into_iter().map(|e| e.flatten()).collect(),
                config,
            },
            BasicParseEntry::Group {
                paren_token,
                entries,
                config,
            } => BasicParseEntry::Group {
                paren_token,
                entries: entries.into_iter().map(|e| e.flatten()).collect(),
                config,
            },
            rest => rest,
        }
    }
}

impl ParseEntry {
    fn flatten(self) -> Self {
        use BasicParseEntry::*;
        use ParseEntry::*;

        match self {
            Basic(Optional {
                bracket_token,
                entries,
                config,
            }) => Basic(Optional {
                bracket_token,
                entries: entries.into_iter().map(|e| e.flatten()).collect(),
                config,
            }),
            Basic(Repeated {
                brace_token,
                entries,
                config,
            }) => Basic(Repeated {
                brace_token,
                entries: entries.into_iter().map(|e| e.flatten()).collect(),
                config,
            }),
            Basic(Group {
                entries,
                config: p_config,
                ..
            }) if entries.len() == 1 => {
                let first = entries.into_iter().next().unwrap();
                match first {
                    Basic(basic) => match basic {
                        CharLit(lit_char, config) => Basic(CharLit(lit_char, p_config + config)),
                        StrLit(lit_str, config) => Basic(StrLit(lit_str, p_config + config)),
                        Ident(ident, config) => Basic(Ident(ident, p_config + config)),
                        Optional {
                            bracket_token,
                            entries,
                            config,
                        } => Basic(Optional {
                            bracket_token,
                            entries: entries.into_iter().map(|e| e.flatten()).collect(),
                            config: p_config + config,
                        }),
                        Repeated {
                            brace_token,
                            entries,
                            config,
                        } => Basic(Repeated {
                            brace_token,
                            entries: entries.into_iter().map(|e| e.flatten()).collect(),
                            config: p_config + config,
                        }),
                        Group {
                            paren_token,
                            entries,
                            config,
                        } => Basic(Group {
                            paren_token,
                            entries: entries.into_iter().map(|e| e.flatten()).collect(),
                            config: p_config + config,
                        }),
                    },
                    Choice { entries, config } => Choice {
                        entries: entries.into_iter().map(|e| e.flatten()).collect(),
                        config: p_config + config,
                    },
                }
            }
            Choice { entries, config } => Choice {
                entries: entries.into_iter().map(|e| e.flatten()).collect(),
                config,
            },
            rest => rest,
        }
    }
}

/// Of the form:
/// ```
/// syntaxName:
///     <Token>
///     [otherSyntax]
/// ```
/// Example
/// ```
/// multiplicativeExpression:
///     asExpression {multiplicativeOperator {NL} asExpression}
/// ```
pub(crate) struct TopLevelParseEntry {
    pub field: Field,
    pub asts: Vec<ParseEntry>,
}

impl fmt::Debug for TopLevelParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TopLevelParseEntry")
            .field("field", &self.field)
            .field("asts", &self.asts)
            .finish()
    }
}

pub(crate) struct GenAst {
    pub entries: Vec<TopLevelParseEntry>,
}

impl fmt::Debug for GenAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GenAst")
            .field("entries", &self.entries)
            .finish()
    }
}

pub(crate) struct Field {
    pub name: Ident,
}

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Field").field("name", &self.name).finish()
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        Ok(Field { name })
    }
}

fn parse_lits(input: ParseStream) -> syn::Result<BasicParseEntry> {
    Configured::<Lit>::parse(input).and_then(|Configured { config, parsed: x }| match x {
        Lit::Str(y) => Ok(BasicParseEntry::StrLit(y, config)),
        Lit::Char(y) => Ok(BasicParseEntry::CharLit(y, config)),
        _ => Err(syn::Error::new(
            x.span(),
            "expected a string or char literal",
        )),
    })
}

fn parse_basic_entry(input: ParseStream) -> syn::Result<BasicParseEntry> {
    parse_lits(input)
        .or_else(|_| {
            Configured::<Ident>::parse(input)
                .map(|Configured { config, parsed: id }| BasicParseEntry::Ident(id, config))
        })
        .or_else(|_| {
            let ignore = input.parse::<Token![_]>().is_ok();

            let rule;
            let bracket_token = bracketed!(rule in input);
            let seq = rule.parse::<Seq<ParseEntry>>()?;

            let name_config: Optional<InOrder<Token![@], Ident>> = input.parse()?;
            let name_config = name_config.0.map(|e| e.second);

            Ok(BasicParseEntry::Optional {
                bracket_token,
                entries: seq.0,
                config: Config {
                    name: name_config,
                    ignore,
                },
            })
        })
        .or_else(|_| {
            let ignore = input.parse::<Token![_]>().is_ok();

            let rule;
            let paren_token = parenthesized!(rule in input);
            let seq = rule.parse::<Seq<ParseEntry>>()?;

            let name_config: Optional<InOrder<Token![@], Ident>> = input.parse()?;
            let name_config = name_config.0.map(|e| e.second);

            Ok(BasicParseEntry::Group {
                entries: seq.0,
                paren_token,
                config: Config {
                    name: name_config,
                    ignore,
                },
            })
        })
        .or_else(|_| {
            let ignore = input.parse::<Token![_]>().is_ok();

            let rule;
            let brace_token = braced!(rule in input);
            let seq = rule.parse::<Seq<ParseEntry>>()?;

            let name_config: Optional<InOrder<Token![@], Ident>> = input.parse()?;
            let name_config = name_config.0.map(|e| e.second);

            Ok(BasicParseEntry::Repeated {
                entries: seq.0,
                brace_token,
                config: Config {
                    name: name_config,
                    ignore,
                },
            })
        })
}

impl Parse for BasicParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_basic_entry(input)
    }
}

impl From<BasicParseEntry> for ParseEntry {
    fn from(value: BasicParseEntry) -> Self {
        ParseEntry::Basic(value)
    }
}

impl Parse for ParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let result = Punctuated::<BasicParseEntry, Token![|]>::parse_separated_nonempty(input)?;

        // Note that this means that every ParseEntry is wrapped in choice which is incorrect,
        // But we fix this in Parse impl for ParseEntryExt
        let res = if result.len() == 1 {
            result.into_iter().next().unwrap().into()
        } else {
            ParseEntry::Choice {
                entries: result.into_iter().collect(),
                config: Config::default(),
            }
        };

        Ok(res.flatten())
    }
}

struct Configured<P> {
    config: Config,
    parsed: P,
}

impl<P> Parse for Configured<P>
where
    P: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Configured<P>> {
        let ignore = input.parse::<Token![_]>().is_ok();
        let parsed = input.parse::<P>()?;
        let name_config: Optional<InOrder<Token![@], Ident>> = input.parse()?;
        let name_config = name_config.0.map(|e| e.second);

        let config = Config {
            name: name_config,
            ignore,
        };

        Ok(Configured { config, parsed })
    }
}

impl Parse for TopLevelParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        let mut result: Vec<ParseEntry> = Vec::new();

        let field: Field = fork.parse()?;
        while !fork.peek2(Token![:]) && !fork.is_empty() {
            let next = fork.parse::<ParseEntry>()?;
            result.push(next);
        }
        if result.is_empty() {
            return Err(syn::Error::new(
                fork.span(),
                "expected rules after rule name",
            ));
        }

        input.advance_to(&fork);

        Ok(TopLevelParseEntry {
            field,
            asts: result,
        })
    }
}

impl Parse for GenAst {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let result: Seq<TopLevelParseEntry> = input.parse()?;
        Ok(GenAst { entries: result.0 })
    }
}

impl BasicParseEntry {
    pub fn span(&self) -> Span {
        match &self {
            Self::CharLit(ch, ..) => ch.span(),
            Self::StrLit(st, ..) => st.span(),
            Self::Ident(id, ..) => id.span(),
            Self::Optional { bracket_token, .. } => bracket_token.span.span(),
            Self::Repeated { brace_token, .. } => brace_token.span.span(),
            Self::Group { paren_token, .. } => paren_token.span.span(),
        }
    }
}

impl ParseEntry {
    pub fn span(&self) -> Span {
        match &self {
            ParseEntry::Basic(basic) => basic.span(),
            ParseEntry::Choice { entries, .. } => {
                let first = &entries[0];
                let last = &entries[entries.len() - 1];
                // NOTE: This works since we are on nightly
                first
                    .span()
                    .join(last.span())
                    .expect("Span join returned None")
            }
        }
    }

    pub fn is_composite(&self) -> bool {
        match self {
            ParseEntry::Basic(basic) => match basic {
                BasicParseEntry::Optional { .. }
                | BasicParseEntry::Repeated { .. }
                | BasicParseEntry::Group { .. } => true,
                _ => false,
            },
            ParseEntry::Choice { .. } => true,
        }
    }

    pub fn config(&self) -> &Config {
        match self {
            ParseEntry::Basic(basic) => match basic {
                BasicParseEntry::CharLit(_, config) => config,
                BasicParseEntry::StrLit(_, config) => config,
                BasicParseEntry::Ident(_, config) => config,
                BasicParseEntry::Optional { config, .. } => config,
                BasicParseEntry::Repeated { config, .. } => config,
                BasicParseEntry::Group { config, .. } => config,
            },
            ParseEntry::Choice { config, .. } => config,
        }
    }
}

impl ParseEntryExt {
    pub fn is_enum(&self) -> bool {
        matches!(
            self,
            Self {
                entry: ParseEntry::Choice { .. },
                ..
            }
        )
    }
}

#[cfg(test)]
mod test {
    use std::error::Error;

    use super::*;
    use proc_macro2::{TokenStream, TokenTree};

    macro_rules! tt {
        ($($tokens: tt)*) => {
            stringify!($($tokens)*).parse::<proc_macro2::TokenStream>()?
        };
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    struct ParseTillEnd<P> {
        main: P,
        rest: TokenTree,
    }

    impl<P> Parse for ParseTillEnd<P>
    where
        P: Parse,
    {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(ParseTillEnd {
                main: input.parse()?,
                rest: input.parse()?,
            })
        }
    }

    #[test]
    fn basic_entry_test() -> Result<(), Box<dyn Error>> {
        let stream: TokenStream = tt! {
            expression:
                disjunction
        };
        let all = syn::parse2::<ParseTillEnd<Field>>(stream)?;

        assert_eq!(all.main.name.to_string(), "expression");
        assert_eq!(all.rest.to_string(), "disjunction");
        Ok(())
    }

    #[test]
    fn simple_top_level_entry_without_config() -> Result<(), Box<dyn Error>> {
        let stream: TokenStream = tt! {
            expression:
                disjunction
        };
        syn::parse2::<InOrder<Field, ParseEntry>>(stream)?;
        Ok(())
    }

    #[test]
    fn simple_top_level_entry() -> Result<(), Box<dyn Error>> {
        let stream: TokenStream = tt! {
            expression:
                disjunction
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;
        assert!(matches!(
            &top.asts[..],
            &[ParseEntryExt {
                entry: ParseEntry::Basic(BasicParseEntry::Ident(..)),
                ..
            }]
        ));

        let stream: TokenStream = tt! {
              propertyModifier:
                "const"
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;
        assert!(matches!(
            &top.asts[..],
            &[ParseEntryExt {
                entry: ParseEntry::Basic(BasicParseEntry::StrLit(..)),
                ..
            }]
        ));
        Ok(())
    }

    #[test]
    fn top_level_entry_with_choice() -> Result<(), Box<dyn Error>> {
        let stream: TokenStream = tt! {
            expression:
                disjunction
                | another
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;
        assert!(matches!(
            &top.asts[..],
            &[ParseEntryExt {
                entry: ParseEntry::Choice { .. },
                ..
            }]
        ));
        Ok(())
    }

    #[test]
    fn test_with_config() -> Result<(), Box<dyn Error>> {
        let stream: TokenStream = tt! {
            annotationUseSiteTarget:
                (AT_NO_WS | AT_PRE_WS)@AnnotationUseSiteTargetAt
                ("field" | "property" | "get" | "set" | "receiver" | "param" | "setparam" | "delegate")@AnnotationTarget
                {NL} ':'
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;
        dbg!(top);
        // assert!(
        //     matches!(&top.asts[..], [ParseEntryExt {entry, ..} ] if matches!(entry, ParseEntry::Choice { .. }))
        // );
        Ok(())
    }

    #[test]
    fn gen_ast_test() -> Result<(), Box<dyn Error>> {
        let stream: TokenStream = tt! {
            functionModifier:
            "tailrec"
            | "operator"
            | "infix"
            | "inline"
            | "external"
            | "suspend"

          propertyModifier:
            "const"

          inheritanceModifier:
            "abstract"
            | "final"
            | "open"

          parameterModifier:
            "vararg"
            | "noinline"
            | "crossinline"

          reificationModifier:
            "reified"

          platformModifier:
            "expect"
            | "actual"
        };
        let gen = syn::parse2::<GenAst>(stream)?;
        dbg!(gen);
        // assert!(
        //     matches!(&gen., [ParseEntryExt {entry, ..} ]
        //         if matches!(entry, ParseEntry::Basic(BasicParseEntry::StrLit(..))))
        // );
        Ok(())
    }
}
