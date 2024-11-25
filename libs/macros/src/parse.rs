use std::fmt::{self, Debug};

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

#[derive(Clone)]
pub(crate) enum BasicParseEntry {
    CharLit(LitChar),
    StrLit(LitStr),
    Ident(Ident),
    Optional {
        bracket_token: Bracket,
        entries: Vec<ParseEntryExt>,
    },
    Repeated {
        brace_token: Brace,
        entries: Vec<ParseEntryExt>,
    },
    Group {
        paren_token: Paren,
        entries: Vec<ParseEntryExt>,
    },
}

#[derive(Clone)]
pub enum ParseEntry {
    Basic(BasicParseEntry),
    Choice { entries: Vec<ParseEntryExt> },
}

#[derive(Clone, Debug)]
pub struct ParseEntryExt {
    pub config: Config,
    pub entry: ParseEntry,
}

impl fmt::Debug for BasicParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CharLit(lit) => lit.value().fmt(f),
            Self::StrLit(lit) => lit.value().fmt(f),
            Self::Ident(id) => id.fmt(f),
            Self::Optional { entries, .. } => f.debug_tuple("Optional").field(entries).finish(),
            Self::Repeated { entries, .. } => f.debug_tuple("Repeated").field(entries).finish(),
            Self::Group { entries, .. } => f.debug_tuple("Group").field(entries).finish(),
        }
    }
}

impl fmt::Debug for ParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Choice { entries, .. } => f.debug_tuple("Choice").field(entries).finish(),
            Self::Basic(basic) => basic.fmt(f),
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
/// NB: For TopLevelParseEntry with non-singular entry, we will wrap it in a Group
pub(crate) struct TopLevelParseEntry {
    pub field: Field,
    pub ast: ParseEntryExt,
}

impl fmt::Debug for TopLevelParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TopLevelParseEntry")
            .field("field", &self.field)
            .field("asts", &self.ast)
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
    Lit::parse(input).and_then(|x| match x {
        Lit::Str(y) => Ok(BasicParseEntry::StrLit(y)),
        Lit::Char(y) => Ok(BasicParseEntry::CharLit(y)),
        _ => Err(syn::Error::new(
            x.span(),
            "expected a string or char literal",
        )),
    })
}

fn parse_basic_entry(input: ParseStream) -> syn::Result<BasicParseEntry> {
    parse_lits(input)
        .or_else(|_| Ident::parse(input).map(|id| BasicParseEntry::Ident(id)))
        .or_else(|_| {
            let rule;
            let bracket_token = bracketed!(rule in input);
            rule.parse::<Seq<ParseEntryExt>>()
                .map(|x| BasicParseEntry::Optional {
                    bracket_token,
                    entries: x.0,
                })
        })
        .or_else(|_| {
            let rule;
            let paren_token = parenthesized!(rule in input);
            rule.parse::<Seq<ParseEntryExt>>()
                .map(|x| BasicParseEntry::Group {
                    entries: x.0,
                    paren_token,
                })
        })
        .or_else(|_| {
            let rule;
            let brace_token = braced!(rule in input);
            rule.parse::<Seq<ParseEntryExt>>()
                .map(|x| BasicParseEntry::Repeated {
                    entries: x.0,
                    brace_token,
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
        let result =
            Punctuated::<Configured<BasicParseEntry>, Token![|]>::parse_separated_nonempty(input)?;

        let result: Vec<_> = result
            .into_iter()
            .map(|c| ParseEntryExt {
                config: c.config,
                entry: c.parsed.into(),
            })
            .collect();

        Ok(ParseEntry::Choice { entries: result })
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

        let config = Config {
            name: name_config.0.map(|e| e.second),
            ignore,
        };

        Ok(Configured { config, parsed })
    }
}

impl Parse for ParseEntryExt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let Configured { config, parsed } = Configured::<ParseEntry>::parse(input)?;

        // Note: If entry is a Group/Choice with one element, we would want to unwrap that
        let res = match parsed {
            ParseEntry::Choice { entries }
            | ParseEntry::Basic(BasicParseEntry::Group { entries, .. })
                if entries.len() == 1 =>
            {
                let entry = &entries[0];
                ParseEntryExt {
                    config: Config {
                        name: (entry.config.name.clone()).or(config.name),
                        ignore: config.ignore || entry.config.ignore,
                    },
                    entry: entry.entry.clone(),
                }
            }

            _ => ParseEntryExt {
                config,
                entry: parsed,
            },
        };

        Ok(res)
    }
}

impl Parse for TopLevelParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        let mut result: Vec<ParseEntryExt> = Vec::new();

        let field: Field = fork.parse()?;
        while !fork.peek2(Token![:]) && !fork.is_empty() {
            let next = fork.parse::<ParseEntryExt>()?;
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
            ast: if result.len() == 1 {
                result[0].clone()
            } else {
                ParseEntryExt {
                     config: Config::default(), 
                     entry: ParseEntry::Basic(BasicParseEntry::Group {
                         paren_token: Paren::default(), entries: result 
                        }) 
                }
            },
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
    fn span(&self) -> Span {
        match &self {
            Self::CharLit(ch) => ch.span(),
            Self::StrLit(st) => st.span(),
            Self::Ident(id) => id.span(),
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
            ParseEntry::Choice { entries } => {
                let first = &entries[0];
                // TODO: uncomment when Span::join becomes stable
                // let last = entries[entries.len() - 1];
                // first.span().join(last.span())
                first.entry.span()
            }
        }
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
        assert!(
            matches!(&top.ast[..], [ParseEntryExt {entry, ..} ] 
                if matches!(entry, ParseEntry::Basic(BasicParseEntry::Ident(..))))
        );

        let stream: TokenStream = tt! {
              propertyModifier:
                "const"
        };
        let top = syn::parse2::<TopLevelParseEntry>(stream)?;
        assert!(
            matches!(&top.ast[..], [ParseEntryExt {entry, ..} ] 
                if matches!(entry, ParseEntry::Basic(BasicParseEntry::StrLit(..))))
        );
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
        assert!(
            matches!(&top.ast[..], [ParseEntryExt {entry, ..} ] if matches!(entry, ParseEntry::Choice { .. }))
        );
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
