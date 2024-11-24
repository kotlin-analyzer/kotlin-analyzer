// use ast::Root;
use itertools::{peek_nth, PeekNth};
use lexer::{Lexer, SpannedWithSource};
use parser::{ParseError, TokenSource, TreeSink};
use rowan::{GreenNode, GreenNodeBuilder};
use syntax::{SyntaxKind, SyntaxNode};
use tokens::Token;

struct TestTreeSink {
    errors: Vec<ParseError>,
    builder: GreenNodeBuilder<'static>,
}

struct TestSource<'a, T>
where
    T: Iterator<Item = SpannedWithSource<'a>> + 'a,
{
    source: PeekNth<T>,
}

impl TreeSink for TestTreeSink {
    fn token(&mut self, token: Token, substring: &str) {
        self.builder
            .token(SyntaxKind::from(token).into(), substring);
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn error(&mut self, error: ParseError) {
        self.errors.push(error)
    }
}

impl<'a, T> TokenSource<'a> for TestSource<'a, T>
where
    T: Iterator<Item = SpannedWithSource<'a>> + 'a,
{
    fn current(&mut self) -> Option<&SpannedWithSource<'a>> {
        self.source.peek_nth(0)
    }

    fn lookahead_nth(&mut self, n: usize) -> Option<&SpannedWithSource<'a>> {
        self.source.peek_nth(n)
    }

    fn is_keyword(&mut self) -> bool {
        self.current().map(|s| s.is_keyword()).unwrap_or_default()
    }

    fn bump(&mut self) -> Option<SpannedWithSource<'a>> {
        self.source.next()
    }
}

pub struct Parse {
    green_node: GreenNode,
    #[allow(unused)]
    pub errors: Vec<ParseError>,
}

pub fn parse(text: &str) -> Parse {
    let mut sink = TestTreeSink {
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
    };
    let tokens = Lexer::new(text).spanned_with_src();
    let mut source = TestSource {
        source: peek_nth(tokens),
    };

    parser::parse(&mut source, &mut sink);

    Parse {
        green_node: sink.builder.finish(),
        errors: sink.errors,
    }
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    // pub fn root(&self) -> Root {
    //     Root::cast(self.syntax()).unwrap()
    // }
}
