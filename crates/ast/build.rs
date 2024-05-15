use std::env;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

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
#[grammar = "../../../pest/kotlin_spec.pest"]
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
}

fn parse_spec() {
    let mut file = File::open("../../spec/syntax_grammar.ini").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("can not");

    let rules = KotlinSpecParser::parse(Rule::syntax_grammar, &buf).unwrap();

    fn parse_top_level(pair: Pair<Rule>) {
        match pair.as_rule() {
            Rule::top_level => pair.into_inner().for_each(|pair| {
                let mut inner_rules = pair.clone().into_inner();
                let field = inner_rules
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str();
                let next = inner_rules.next().unwrap().as_rule();
                p!("{field:?}: {next:?}")
            }),
            _ => unreachable!(),
        };
    }
    fn parse_entry(pair: Pair<Rule>) -> ParseEntry {
        match pair.as_rule() {
            Rule::char => todo!(),
            Rule::string => todo!(),
            Rule::ident => todo!(),
            Rule::repeated => todo!(),
            Rule::choice => todo!(),
            Rule::optional => todo!(),
            Rule::group => todo!(),
            Rule::entry => todo!(),
            Rule::basic_entry => todo!(),
            Rule::field => todo!(),
            Rule::top_level => todo!(),
            Rule::syntax_grammar => todo!(),
            _ => todo!(),
        }
    }
    rules.for_each(parse_top_level);

    // rules_to_value(rules)
}
