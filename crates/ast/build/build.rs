mod generators;

use std::env;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use tokens::resolve_token;

macro_rules! p {
    ($($tokens: tt)*) => {
        println!("cargo:warning={}", format!($($tokens)*))
    }
}

pub(crate) enum ParseEntry<'a> {
    CharLit(char),
    StrLit(&'a str),
    Ident(&'a str),
    Optional(Vec<ParseEntry<'a>>),
    Repeated(Vec<ParseEntry<'a>>),
    Choice(Vec<ParseEntry<'a>>),
    Group(Vec<ParseEntry<'a>>),
}

enum ReducedEntry<'a> {
    CharLit(char),
    StrLit(&'a str),
    Ident(&'a str),
}

impl ParseEntry<'_> {
    fn type_name(&self) -> Option<String> {
        match self {
            ParseEntry::CharLit(ch) => resolve_token(&ch.to_string()).map(|t| format!("{:?}", t)),
            ParseEntry::StrLit(st) => resolve_token(&st.to_string()).map(|t| format!("{:?}", t)),
            ParseEntry::Ident(id) => Some(id.to_string()),
            ParseEntry::Optional(entries) => todo!(),
            ParseEntry::Repeated(_) => todo!(),
            ParseEntry::Choice(_) => todo!(),
            ParseEntry::Group(_) => todo!(),
        }
    }

    fn iter<'a>(&'a self) -> impl Iterator<Item = ReducedEntry<'a>> + 'a {
        let once;
        let many;
        match self {
            // ParseEntry::CharLit(ch) => vec![ReducedEntry::CharLit(*ch)],
            // ParseEntry::StrLit(st) => vec![ReducedEntry::StrLit(st)],
            ParseEntry::Ident(id) => {
                return res.chain(std::iter::once(ReducedEntry::Ident(id)));
            }
            ParseEntry::Optional(entries)
            | ParseEntry::Repeated(entries)
            | ParseEntry::Choice(entries)
            | ParseEntry::Group(entries) => {
                return entries.iter().flat_map(|e| e.iter());
            }
            _ => unreachable!(),
        };
    }
}

impl fmt::Debug for ParseEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseEntry::CharLit(lit) => lit.fmt(f),
            ParseEntry::StrLit(lit) | ParseEntry::Ident(lit) => lit.fmt(f),
            ParseEntry::Optional(entries) => f.debug_tuple("Optional").field(entries).finish(),
            ParseEntry::Repeated(entries) => f.debug_tuple("Repeated").field(entries).finish(),
            ParseEntry::Choice(entries) => f.debug_tuple("Choice").field(entries).finish(),
            ParseEntry::Group(entries) => f.debug_tuple("Group").field(entries).finish(),
        }
    }
}

pub(crate) struct TopLevelParseEntry<'a> {
    pub field: Field<'a>,
    pub asts: Vec<ParseEntry<'a>>,
}

impl fmt::Debug for TopLevelParseEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TopLevelParseEntry")
            .field("field", &self.field)
            .field("asts", &self.asts)
            .finish()
    }
}

pub(crate) struct SyntaxGrammer<'a> {
    pub entries: Vec<TopLevelParseEntry<'a>>,
}

impl fmt::Debug for SyntaxGrammer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxGrammer")
            .field("entries", &self.entries)
            .finish()
    }
}

pub(crate) struct Field<'a> {
    pub name: &'a str,
}

impl fmt::Debug for Field<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Field").field("name", &self.name).finish()
    }
}

#[derive(Parser)]
#[grammar = "../../pest/kotlin_spec.pest"]
struct KotlinSpecParser;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("hello.rs");
    parse_spec();
    fs::write(
        &dest_path,
        "pub fn message() -> &'static str {
            \"Hello, World!\"
        }
        ",
    )
    .unwrap();
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=../../spec/syntax_grammar.ini");
    println!("cargo::rerun-if-changed=../../pest/kotlin_spec.pest");
}

fn parse_spec() {
    let mut file = File::open("../../spec/syntax_grammar.ini").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("can not");

    let rules = KotlinSpecParser::parse(Rule::syntax_grammar, &buf).unwrap();

    fn parse_top_level(pair: Pair<Rule>) -> Option<TopLevelParseEntry> {
        match pair.as_rule() {
            Rule::top_level => {
                let mut inner_rules = pair.into_inner();
                let field = inner_rules.next().unwrap().as_str();
                let asts = inner_rules.map(parse_entry).collect::<Vec<_>>();
                Some(TopLevelParseEntry {
                    field: Field { name: field },
                    asts,
                })
            }
            _ => None,
        }
    }

    fn parse_entry(pair: Pair<Rule>) -> ParseEntry {
        match pair.as_rule() {
            Rule::char => ParseEntry::CharLit(pair.as_str().chars().nth(1).unwrap()),
            Rule::string => {
                let res = pair.as_str();
                ParseEntry::StrLit(&res[1..res.len() - 1])
            }
            Rule::ident => ParseEntry::Ident(pair.as_str()),
            Rule::repeated => ParseEntry::Repeated(pair.into_inner().map(parse_entry).collect()),
            Rule::choice => ParseEntry::Choice(pair.into_inner().map(parse_entry).collect()),
            Rule::optional => ParseEntry::Optional(pair.into_inner().map(parse_entry).collect()),
            Rule::group => ParseEntry::Group(pair.into_inner().map(parse_entry).collect()),
            _ => unreachable!(),
        }
    }
    let result = rules.flat_map(parse_top_level).collect::<Vec<_>>();
    p!("{result:?}");
}
