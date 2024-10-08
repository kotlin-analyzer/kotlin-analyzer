use std::error::Error;
use std::fmt::{self, Debug};

use proc_macro2::Span;
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::token::{Brace, Bracket, Paren};
use syn::{braced, bracketed, parenthesized, Error, Ident, Lit, LitChar, LitStr, Token};

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
pub(crate) enum ParseEntry {
    Basic(BasicParseEntry),
    Choice { entries: Vec<ParseEntryExt> },
}

#[derive(Clone, Debug)]
pub(crate) struct ParseEntryExt {
    pub(crate) config: Config,
    pub(crate) entry: ParseEntry,
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

pub(crate) struct TopLevelParseEntry {
    pub field: Field,
    pub asts: Vec<ParseEntryExt>,
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
    Lit::parse(input).and_then(|x| match x {
        Lit::Str(y) => Ok(BasicParseEntry::StrLit(y)),
        Lit::Char(y) => Ok(BasicParseEntry::CharLit(y)),
        _ => Err(Error::new(x.span(), "expected a string or char literal")),
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

fn parse_choice_2(input: ParseStream) -> syn::Result<ParseEntry> {
    let first = input.parse::<BasicParseEntry>()?;
    let mut output = Vec::new();

    while input.peek(Token![|]) {
        // throw away
        input.parse::<Token![|]>()?;

        output.push(input.parse::<BasicParseEntry>()?);
    }

    if output.len() < 2 {
        return Err(syn::Error::new(
            first.span(),
            "choice must be separated by `|`",
        ));
    }

    Ok(ParseEntry::Choice { entries: output })
}
fn parse_choice(input: ParseStream) -> syn::Result<ParseEntry> {
    let mut result = Vec::new();
    let mut last_pipe = None;
    let mut saw_pipe_last = false;
    let fork = input.fork();

    loop {
        let Ok(Configured { config, parsed }) =
            Configured::<ParseEntry>::parse_with(&fork, parse_basic_entry)
        else {
            break;
        };
        saw_pipe_last = false;

        result.push(ParseEntryExt {
            config,
            entry: parsed,
        });

        let Ok(pipe) = fork.parse::<Token![|]>() else {
            break;
        };

        last_pipe = Some(pipe);
        saw_pipe_last = true;
    }

    if result.len() < 2 {
        return Err(syn::Error::new(
            input.span(),
            "choice expected rules matching A|B but got none",
        ));
    };

    if saw_pipe_last {
        return Err(Error::new(
            last_pipe.unwrap().span,
            "Unexpected trailing pipe (|)",
        ));
    }

    input.advance_to(&fork);
    Ok(ParseEntry::Choice { entries: result })
}

impl Parse for ParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_choice(input).or_else(|_| parse_basic_entry(input))
    }
}

struct Configured<P> {
    config: Config,
    parsed: P,
}

impl<P> Configured<P> {
    fn parse_with<U: Parse>(
        input: ParseStream,
        f: impl Fn(ParseStream) -> syn::Result<U>,
    ) -> syn::Result<Configured<U>> {
        let ignore = input.parse::<Token![_]>().is_ok();
        let parsed = f(input)?;
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
        let Configured { config, parsed } =
            Configured::<ParseEntry>::parse_with(input, ParseEntry::parse)?;

        // Note: If entry is a Group with one element, we would want to unwrap that
        let res = match parsed {
            ParseEntry::Group { entries, .. } if entries.len() == 1 => {
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
        let mut result: Vec<ParseEntryExt> = Vec::new();
        let field: Field = input.parse()?;
        while !input.peek2(Token![:]) {
            if let Ok(next) = input.parse::<ParseEntryExt>() {
                result.push(next);
            } else {
                break;
            }
        }
        if result.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                "expected rules after rule name",
            ));
        }
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
    fn span(&self) -> Span {
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
