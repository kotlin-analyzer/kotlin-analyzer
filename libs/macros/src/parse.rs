use std::fmt;

use syn::parse::{Parse, ParseStream};
use syn::{braced, bracketed, parenthesized, Error, Ident, Lit, LitChar, LitStr, Token};

use crate::combinators::{InOrder, Optional, Seq};

#[derive(Debug, Default, Clone)]
pub(crate) struct Config {
    pub(crate) name: Option<String>,
    pub(crate) ignore: bool,
}

pub(crate) enum ParseEntry {
    CharLit(LitChar, Config),
    StrLit(LitStr, Config),
    Ident(Ident, Config),
    Optional(Vec<ParseEntry>, Config),
    Repeated(Vec<ParseEntry>, Config),
    Choice(Vec<ParseEntry>, Config),
    Group(Vec<ParseEntry>, Config),
}

impl ParseEntry {
    fn add_config(self, config: Config) -> Self {
        match self {
            ParseEntry::CharLit(x, _) => Self::CharLit(x, config),
            ParseEntry::StrLit(x, _) => Self::StrLit(x, config),
            ParseEntry::Ident(x, _) => Self::Ident(x, config),
            ParseEntry::Optional(x, _) => Self::Optional(x, config),
            ParseEntry::Repeated(x, _) => Self::Repeated(x, config),
            ParseEntry::Choice(x, _) => Self::Choice(x, config),
            ParseEntry::Group(x, _) => Self::Group(x, config),
        }
    }

    pub(crate) fn config(&self) -> &Config {
        match self {
            ParseEntry::CharLit(_, config) => config,
            ParseEntry::StrLit(_, config) => config,
            ParseEntry::Ident(_, config) => config,
            ParseEntry::Optional(_, config) => config,
            ParseEntry::Repeated(_, config) => config,
            ParseEntry::Choice(_, config) => config,
            ParseEntry::Group(_, config) => config,
        }
    }
}

impl fmt::Debug for ParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseEntry::CharLit(lit, ..) => lit.value().fmt(f),
            ParseEntry::StrLit(lit, ..) => lit.value().fmt(f),
            ParseEntry::Ident(id, ..) => id.fmt(f),
            ParseEntry::Optional(entries, ..) => f.debug_tuple("Optional").field(entries).finish(),
            ParseEntry::Repeated(entries, ..) => f.debug_tuple("Repeated").field(entries).finish(),
            ParseEntry::Choice(entries, ..) => f.debug_tuple("Choice").field(entries).finish(),
            ParseEntry::Group(entries, ..) => f.debug_tuple("Group").field(entries).finish(),
        }
    }
}

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

fn parse_lits(input: ParseStream) -> syn::Result<ParseEntry> {
    Lit::parse(input).and_then(|x| match x {
        Lit::Str(y) => Ok(ParseEntry::StrLit(y, Config::default())),
        Lit::Char(y) => Ok(ParseEntry::CharLit(y, Config::default())),
        _ => Err(Error::new(x.span(), "expected a string or char literal")),
    })
}

fn parse_basic_entry(input: ParseStream) -> syn::Result<ParseEntry> {
    parse_lits(input)
        .or_else(|_| Ident::parse(input).map(|id| ParseEntry::Ident(id, Config::default())))
        .or_else(|_| {
            let rule;
            bracketed!(rule in input);
            rule.parse::<Seq<ParseEntry>>()
                .map(|x| ParseEntry::Optional(x.0, Config::default()))
        })
        .or_else(|_| {
            let rule;
            parenthesized!(rule in input);
            rule.parse::<Seq<ParseEntry>>()
                .map(|x| ParseEntry::Group(x.0, Config::default()))
        })
        .or_else(|_| {
            let rule;
            braced!(rule in input);
            rule.parse::<Seq<ParseEntry>>()
                .map(|x| ParseEntry::Repeated(x.0, Config::default()))
        })
}

fn parse_choice(input: ParseStream) -> syn::Result<ParseEntry> {
    let mut result = Vec::new();
    let mut last = None;

    while input.peek2(Token![|]) {
        result.push(parse_basic_entry(input)?);
        last = Some(input.parse::<Token![|]>()?);
    }

    if result.is_empty() {
        return Err(syn::Error::new(
            input.span(),
            "choice expected rules matching A|B but got none",
        ));
    } else {
        result
            .push(parse_basic_entry(input).map_err(move |_| {
                Error::new(last.unwrap().span, "Unexpected trailing pipe (|)")
            })?);
    }
    Ok(ParseEntry::Choice(result, Config::default()))
}

impl Parse for ParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ignore = input.parse::<Token![_]>().is_ok();
        let entry = parse_choice(input).or_else(|_| parse_basic_entry(input))?;
        let name_config: Optional<InOrder<Token![@], Ident>> = input.parse()?;
        Ok(entry.add_config(Config {
            name: name_config.0.map(|e| e.second.to_string()),
            ignore,
        }))
    }
}

impl Parse for TopLevelParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result: Vec<ParseEntry> = Vec::new();
        let field: Field = input.parse()?;
        while !input.peek2(Token![:]) {
            if let Ok(next) = input.parse::<ParseEntry>() {
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
