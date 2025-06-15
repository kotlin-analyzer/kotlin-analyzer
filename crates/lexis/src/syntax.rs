use lady_deirdre::syntax::{Node, NodeRef};

#[cfg(test)]
mod tests;

use crate::parser::{
    parse_delimited_comment, parse_identifier, parse_line_comment, parse_shebang_line,
};
use crate::tokens::KotlinToken;

// TODO: implement unicode
// TODO: implement identifiers
// TODO: implement strings

#[derive(Node)]
#[token(KotlinToken)]
#[define(HIDDEN = $Whitespace | LineComment | DelimitedComment)]
#[trivia(HIDDEN)]
#[define(HiddenBefore =  $Whitespace | DelimitedComment)]
#[recovery(
    $LCurl,
    $RCurl,
    [$LCurl..$RCurl],
    $LParen,
    $RParen,
    [$LParen..$RParen],
    $LSquare,
    $RSquare,
    [$LSquare..$RSquare],
    $LAngle,
    $RAngle,
    [$LAngle..$RAngle],
    $Var,
    $Val,
    $Fun,
    $If,
    $Else,
    $When,
    $For,
    $While,
    $Do,
    $Class,
    $Interface,
    $Object,
    $Enum,
    $Companion,
)]
pub enum KotlinNode {
    #[root]
    #[rule(value: KotlinFile)]
    Root {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        value: NodeRef,
    },

    #[rule(shebang_line: ShebangLine? $Newline* ident_test: Identifier+ $Newline*)]
    /// shebangLine? NL* fileAnnotation* packageHeader importList topLevelObject* EOF
    KotlinFile {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
        #[child]
        shebang_line: NodeRef,
        #[child]
        ident_test: Vec<NodeRef>,
    },

    /// Matches `ShebangLine NL+`
    #[rule($Hash $ExclNoWs)]
    #[denote(SHEBANG_LINE)]
    #[parser(parse_shebang_line(session))]
    #[describe("shebangline", "#! ...")]
    ShebangLine {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /* Leaf Nodes - Mostly complex tokens that could not be handled by the lexer */
    ///
    #[rule($MisMatch | $Tick)]
    #[denote(IDENTIFIER)]
    #[parser(parse_identifier(session))]
    #[secondary]
    #[describe("identifier", "ident")]
    Identifier {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },
    /// Matches `"//" (^['\r', '\n'])*`
    #[rule($LineCommentStart)]
    #[denote(LINE_COMMENT)]
    #[parser(parse_line_comment(session))]
    #[describe("line comment")]
    #[secondary]
    LineComment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },

    /// Matches `_*'/*' ( DelimitedComment | . )*? '*/'`
    #[rule($DelimitedCommentStart)]
    #[denote(DELIMITED_COMMENT)]
    #[parser(parse_delimited_comment(session))]
    #[describe("delimited comment")]
    #[secondary]
    DelimitedComment {
        #[node]
        node: NodeRef,
        #[parent]
        parent: NodeRef,
    },
}
