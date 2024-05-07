use std::fmt;

use syn::parse::{Parse, ParseStream};
use syn::{braced, bracketed, parenthesized, Error, Ident, Lit, LitChar, LitStr, Token};

enum ParseEntry {
    CharLit(LitChar),
    StrLit(LitStr),
    Ident(Ident),
    Optional(Vec<ParseEntry>),
    Repeated(Vec<ParseEntry>),
    Choice(Vec<ParseEntry>),
    Group(Vec<ParseEntry>),
}

fn print_vp(ps: &[ParseEntry]) -> String {
    ps.iter()
        .map(|p| p.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

impl fmt::Display for ParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseEntry::CharLit(ch) => write!(f, "'{}'", ch.value()),
            ParseEntry::StrLit(lit) => write!(f, r#""{}""#, lit.value()),
            ParseEntry::Ident(id) => write!(f, "Ident(name: {})", id),
            ParseEntry::Optional(opt) => write!(f, "Optional(rules: [\n{}\n ])", print_vp(opt)),
            ParseEntry::Repeated(rep) => write!(f, "Repeated(rules: [\n{}\n ])", print_vp(rep)),
            ParseEntry::Choice(ch) => write!(f, "Choice(rules: [\n{}\n ])", print_vp(ch)),
            ParseEntry::Group(grp) => write!(f, "Group(rules: [\n{}\n ])", print_vp(grp)),
        }
    }
}

impl fmt::Display for TopLevelParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "TopLevelParseEntry (field: {}, asts: [\n{}\n ] )",
            self.field,
            print_vp(&self.asts)
        )
    }
}

impl fmt::Display for GenAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "GenAst ( entries: [\n{}\n ] )",
            self.entries
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

struct Seq<T>(Vec<T>);
struct InOrder<T, P>(T, P);

impl<T, P> Parse for InOrder<T, P>
where
    T: Parse,
    P: Parse,
{
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first: T = input.parse()?;
        let second: P = input.parse()?;
        Ok(InOrder(first, second))
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

struct TopLevelParseEntry {
    field: Field,
    asts: Vec<ParseEntry>,
}

pub struct GenAst {
    entries: Vec<TopLevelParseEntry>,
}

struct Field {
    name: Ident,
    colon_token: Token![:],
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Field ( name: {} )", self.name)
    }
}

impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Field {
            name: input.parse()?,
            colon_token: input.parse()?,
        })
    }
}

fn parse_lits(input: ParseStream) -> syn::Result<ParseEntry> {
    Lit::parse(input).and_then(|x| match x {
        Lit::Str(y) => Ok(ParseEntry::StrLit(y)),
        Lit::Char(y) => Ok(ParseEntry::CharLit(y)),
        _ => Err(Error::new(x.span(), "expected a string or char literal")),
    })
}

fn parse_basic_entry(input: ParseStream) -> syn::Result<ParseEntry> {
    parse_lits(input)
        .or_else(|_| Ident::parse(input).map(ParseEntry::Ident))
        .or_else(|_| {
            let rule;
            bracketed!(rule in input);
            rule.parse::<Seq<ParseEntry>>()
                .map(|x| ParseEntry::Optional(x.0))
        })
        .or_else(|_| {
            let rule;
            parenthesized!(rule in input);
            rule.parse::<Seq<ParseEntry>>()
                .map(|x| ParseEntry::Group(x.0))
        })
        .or_else(|_| {
            let rule;
            braced!(rule in input);
            rule.parse::<Seq<ParseEntry>>()
                .map(|x| ParseEntry::Repeated(x.0))
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
    Ok(ParseEntry::Choice(result))
}

impl Parse for ParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        parse_choice(input).or_else(|_| parse_basic_entry(input))
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
                field.colon_token.span,
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
