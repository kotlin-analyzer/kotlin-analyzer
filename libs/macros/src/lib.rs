#![allow(dead_code)]
#![allow(unused)]

use std::fmt;
use std::path::Display;

use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, bracketed, parenthesized, parse_macro_input, token, Ident, Lit, LitStr, Token};

#[proc_macro]
pub fn gen_ast(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as GenAst);
    // println!("{ast:#}");
    TokenStream::new()
}

enum ParseEntry {
    Lit(LitStr),
    Ident(Ident),
    Optional(Box<Optional>),
    Repeated(Box<Repeated>),
    Choice(Choice),
    Group(Box<Group>),
}

impl fmt::Display for ParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseEntry::Lit(lit) => write!(f, "Lit({})", "lit"),
            ParseEntry::Ident(id) => write!(f, "Lit({})", id),
            ParseEntry::Optional(opt) => write!(f, "Optional({})", opt.rule),
            ParseEntry::Repeated(rep) => write!(f, "Optional({})", rep.rule),
            ParseEntry::Choice(ch) => write!(
                f,
                "Optional({:?})",
                ch.entries
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ParseEntry::Group(grp) => write!(f, "Optional({})", grp.rule),
        }
    }
}

impl fmt::Display for TopLevelParseEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "TopLevelParseEntry {{field: {}, asts: {} }}",
            self.field,
            self.asts
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for GenAst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "GenAst {{ entries: {} }}",
            self.entries
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

struct TopLevelParseEntry {
    field: Field,
    asts: Vec<ParseEntry>,
}

struct GenAst {
    entries: Vec<TopLevelParseEntry>,
}

struct Field {
    name: Ident,
    colon_token: Token![:],
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Field {{ name: {} }}", self.name)
    }
}

struct Optional {
    bracket_token: token::Bracket,
    rule: ParseEntry,
}

struct Repeated {
    brace_token: token::Brace,
    rule: ParseEntry,
}

struct Choice {
    entries: Punctuated<ParseEntry, Token![|]>,
}

struct Group {
    paren_token: token::Paren,
    rule: ParseEntry,
}

impl Parse for Choice {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Choice {
            entries: input.call(Punctuated::parse_separated_nonempty)?,
        })
    }
}
impl Parse for Optional {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule;
        Ok(Optional {
            bracket_token: bracketed!(rule in input),
            rule: rule.parse()?,
        })
    }
}

impl Parse for Repeated {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule;
        Ok(Repeated {
            brace_token: braced!(rule in input),
            rule: rule.parse()?,
        })
    }
}

impl Parse for Group {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule;
        Ok(Group {
            paren_token: parenthesized!(rule in input),
            rule: rule.parse()?,
        })
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

impl Parse for ParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Group::parse(input)
            .map(|x| ParseEntry::Group(Box::new(x)))
            .or(Repeated::parse(input).map(|x| ParseEntry::Repeated(Box::new(x))))
            .or(Optional::parse(input).map(|x| ParseEntry::Optional(Box::new(x))))
            .or(Choice::parse(input).map(ParseEntry::Choice))
            .or(Lit::parse(input).and_then(|x| match x {
                Lit::Str(y) => Ok(ParseEntry::Lit(y)),
                _ => Err(syn::Error::new(x.span(), "expected a string literal")),
            }))
            .or(Ident::parse(input).map(ParseEntry::Ident))
    }
}

impl Parse for TopLevelParseEntry {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result: Vec<ParseEntry> = Vec::new();
        let field: Field = input.parse()?;
        println!("{field}");
        while input.parse::<Field>().is_err() {
            let next: ParseEntry = input.parse()?;
            println!("{next}");
            result.push(next);
        }
        Ok(TopLevelParseEntry {
            field,
            asts: result,
        })
    }
}

impl Parse for GenAst {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result: Vec<TopLevelParseEntry> = Vec::new();
        while let Ok(next) = input.parse::<TopLevelParseEntry>() {
            println!("{next}");
            result.push(next);
        }
        Ok(GenAst { entries: result })
    }
}
