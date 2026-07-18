//! Defines input for code generation process.

use quote::ToTokens;

use crate::codegen::grammar::to_upper_snake_case;

#[derive(Copy, Clone, Debug)]
pub(crate) struct KindsSrc {
    pub(crate) punct: &'static [(&'static str, &'static str)],
    pub(crate) compound_keywords: &'static [(&'static str, &'static str)],
    pub(crate) keywords: &'static [&'static str],
    pub(crate) soft_keywords: &'static [&'static str],
    pub(crate) literals: &'static [&'static str],
    pub(crate) tokens: &'static [&'static str],
    pub(crate) nodes: &'static [&'static str],
    pub(crate) _enums: &'static [&'static str],
    pub(crate) version_dependent_keywords: &'static [(&'static str, KtVersion)],
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum KtVersion {
    V1_3,
    V1_5,
    V1_7,
}

impl ToTokens for KtVersion {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            KtVersion::V1_3 => {
                tokens.extend(quote::quote! { KtVersion::V1_3 });
            }
            KtVersion::V1_5 => {
                tokens.extend(quote::quote! { KtVersion::V1_5 });
            }
            KtVersion::V1_7 => {
                tokens.extend(quote::quote! { KtVersion::V1_7 });
            }
        }
    }
}

/// The punctuations of the language.
const PUNCT: &[(&str, &str)] = &[
    ("$", "DOLLAR"),
    (".", "DOT"),
    (",", "COMMA"),
    ("(", "L_PAREN"),
    (")", "R_PAREN"),
    ("[", "L_SQUARE"),
    ("]", "R_SQUARE"),
    ("{", "L_CURL"),
    ("}", "R_CURL"),
    ("*", "MULT"),
    ("%", "MOD"),
    ("/", "DIV"),
    ("+", "ADD"),
    ("-", "SUB"),
    ("-=", "SUB_EQ"),
    ("++", "INCR"),
    ("--", "DECR"),
    ("&&", "CONJ"),
    ("||", "DISJ"),
    ("!", "EXCL"),
    (":", "COLON"),
    (";", "SEMICOLON"),
    ("=", "EQ"),
    ("+=", "ADD_EQ"),
    ("*=", "MULT_EQ"),
    ("/=", "DIV_EQ"),
    ("%=", "MOD_EQ"),
    ("->", "ARROW"),
    ("..", "RANGE"),
    ("..<", "RANGE_UNTIL"),
    ("::", "COLON_COLON"),
    ("@", "AT"),
    ("?", "QUEST"),
    ("<", "L_ANGLE"),
    (">", "R_ANGLE"),
    ("<=", "LE"),
    (">=", "GE"),
    ("!=", "EXCL_EQ"),
    ("!==", "EXCL_EQ_EQ"),
    ("==", "EQ_EQ"),
    ("===", "EQ_EQ_EQ"),
    ("&", "AMP"),
    ("\"", "QUOTE"),
    (r#"""""#, "TRIPLE_QUOTE"),
];
const TOKENS: &[&str] = &["ERROR", "WHITESPACE", "NEWLINE", "LINE_COMMENT", "DELIMITED_COMMENT"];

const EOF: &str = "EOF";
const COMPOUND_RESERVED: &[(&str, &str)] = &[
    ("!in", "notIn"),
    ("!is", "notIs"),
    ("as?", "asSafe"),
    ("return@", "returnAt"),
    ("continue@", "continueAt"),
    ("break@", "breakAt"),
    ("this@", "thisAt"),
    ("super@", "superAt"),
];

const RESERVED: &[&str] = &[
    "annotation",
    "as",
    "break",
    "class",
    "do",
    "else",
    "false",
    "for",
    "fun",
    "if",
    "in",
    "interface",
    "is",
    "null",
    "object",
    "package",
    "param",
    "return",
    "super",
    "this",
    "throw",
    "true",
    "try",
    "typealias",
    "typeof",
    "val",
    "var",
    "when",
    "while",
    "continue",
];
// keywords that are keywords only in specific parse contexts
#[doc(alias = "WEAK_KEYWORDS")]
const SOFT_KEYWORDS: &[&str] = &[
    "abstract",
    "actual",
    "annotation",
    "by",
    "catch",
    "companion",
    "const",
    "constructor",
    "contract",
    "context",
    "crossinline",
    "data",
    "delegate",
    "dynamic",
    "enum",
    "expect",
    "external",
    "field",
    "file",
    "final",
    "finally",
    "get",
    "import",
    "infix",
    "init",
    "inline",
    "inner",
    "internal",
    "lateinit",
    "noinline",
    "open",
    "operator",
    "out",
    "override",
    "param",
    "private",
    "property",
    "protected",
    "public",
    "receiver",
    "reified",
    "sealed",
    "set",
    "setparam",
    "suspend",
    "tailrec",
    "value",
    "vararg",
    "where",
];

// keywords that are keywords depending on the edition
const VERSION_DEPENDENT_KEYWORDS: &[(&str, KtVersion)] = &[
    // TODO: these should treated as soft keywords with a version requirement
    // ("contract", KtVersion::V1_3),
    // // fun interface => Version::V1_4
    // ("value", KtVersion::V1_5),
    // ("context", KtVersion::V1_7),
];

pub(crate) fn generate_kind_src(
    nodes: &[AstNodeSrc],
    enums: &[AstEnumSrc],
    grammar: &ungrammar::Grammar,
) -> KindsSrc {
    let mut soft_keywords: Vec<&_> = SOFT_KEYWORDS.to_vec();

    let mut keywords: Vec<&_> = Vec::new();
    let mut tokens: Vec<&_> = TOKENS.to_vec();
    let mut literals: Vec<&_> = Vec::new();
    let mut used_puncts = vec![false; PUNCT.len()];
    let compound_keywords: Vec<&_> = COMPOUND_RESERVED.iter().map(|(token, _kw)| *token).collect();
    // Mark $ as used
    used_puncts[0] = true;

    grammar.tokens().for_each(|token| {
        let name = &*grammar[token].name;
        if name == EOF {
            return;
        }
        match name.split_at(1) {
            ("@", lit) if !lit.is_empty() => {
                literals.push(String::leak(to_upper_snake_case(lit)));
            }
            ("#", token) if !token.is_empty() => {
                tokens.push(String::leak(to_upper_snake_case(token)));
            }
            _ if soft_keywords.contains(&name) => {}
            _ if name.chars().all(char::is_alphabetic) => {
                keywords.push(String::leak(name.to_owned()));
            }
            _ if compound_keywords.contains(&name) => {}
            _ => {
                let idx = PUNCT
                    .iter()
                    .position(|(punct, _)| punct == &name)
                    .unwrap_or_else(|| panic!("Grammar references unknown punctuation {name:?}"));
                used_puncts[idx] = true;
            }
        }
    });
    if let Some(punct) = PUNCT.iter().zip(used_puncts).find(|(_, used)| !used) {
        panic!("Punctuation {punct:?} is not used in grammar");
    }
    keywords.extend(RESERVED.iter().copied());
    keywords.sort();
    keywords.dedup();
    soft_keywords.sort();
    soft_keywords.dedup();
    let mut version_dependent_keywords: Vec<(&_, _)> = VERSION_DEPENDENT_KEYWORDS.to_vec();
    version_dependent_keywords.sort();
    version_dependent_keywords.dedup();

    keywords.retain(|&it| !soft_keywords.contains(&it));
    keywords.retain(|&it| !version_dependent_keywords.iter().any(|&(kw, _)| kw == it));

    // we leak things here for simplicity, that way we don't have to deal with lifetimes
    // The execution is a one shot job so thats fine
    let nodes = nodes
        .iter()
        .map(|it| &it.name)
        .map(|it| to_upper_snake_case(it))
        .map(String::leak)
        .map(|it| &*it)
        .collect();
    let nodes = Vec::leak(nodes);
    nodes.sort();
    let enums = enums
        .iter()
        .map(|it| &it.name)
        .map(|it| to_upper_snake_case(it))
        .map(String::leak)
        .map(|it| &*it)
        .collect();
    let enums = Vec::leak(enums);
    enums.sort();
    let keywords = Vec::leak(keywords);
    let soft_keywords = Vec::leak(soft_keywords);
    let version_dependent_keywords = Vec::leak(version_dependent_keywords);
    let literals = Vec::leak(literals);
    literals.sort();
    let tokens = Vec::leak(tokens);
    tokens.sort();

    KindsSrc {
        punct: PUNCT,
        nodes,
        _enums: enums,
        keywords,
        compound_keywords: COMPOUND_RESERVED,
        soft_keywords,
        version_dependent_keywords,
        literals,
        tokens,
    }
}

#[derive(Default, Debug)]
pub(crate) struct AstSrc {
    pub(crate) tokens: Vec<String>,
    pub(crate) nodes: Vec<AstNodeSrc>,
    pub(crate) enums: Vec<AstEnumSrc>,
}

#[derive(Debug)]
pub(crate) struct AstNodeSrc {
    pub(crate) doc: Vec<String>,
    pub(crate) name: String,
    pub(crate) traits: Vec<String>,
    pub(crate) fields: Vec<Field>,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Field {
    Token { name: Option<String>, token: String },
    Node { name: String, ty: String, cardinality: Cardinality },
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Cardinality {
    Optional,
    Many,
}

#[derive(Debug)]
pub(crate) struct AstEnumSrc {
    pub(crate) doc: Vec<String>,
    pub(crate) name: String,
    pub(crate) traits: Vec<String>,
    pub(crate) variants: Vec<String>,
}
