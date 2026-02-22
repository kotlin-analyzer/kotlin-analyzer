use crate::{ParseError, Parser, TokenSource, TreeSink};
use ast::syntax::{SyntaxKind, SyntaxNode};
use ast::{nodes::Cast, syntax::Root};
use itertools::{PeekNth, peek_nth};
use lexer::{Lexer, SpannedWithSource};
use rowan::{GreenNode, GreenNodeBuilder};
use tokens::Token;

struct TestTreeSink {
    errors: Vec<ParseError>,
    builder: GreenNodeBuilder<'static>,
}

struct TestSource<'a, T>
where
    T: Iterator<Item = (usize, SpannedWithSource<'a>)> + 'a,
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
    T: Iterator<Item = (usize, SpannedWithSource<'a>)> + 'a,
{
    fn current(&mut self) -> Option<&SpannedWithSource<'a>> {
        self.source.peek_nth(0).map(|(_, s)| s)
    }

    fn lookahead_nth(&mut self, n: usize) -> Option<&SpannedWithSource<'a>> {
        self.source.peek_nth(n).map(|(_, s)| s)
    }

    fn is_keyword(&mut self) -> bool {
        self.current().map(|s| s.is_keyword()).unwrap_or_default()
    }

    fn bump(&mut self) -> Option<SpannedWithSource<'a>> {
        self.source.next().map(|(_, s)| s)
    }

    #[cfg(debug_assertions)]
    fn current_index(&mut self) -> Option<usize> {
        self.source.peek_nth(0).map(|(idx, _)| *idx)
    }
}

pub struct Parse {
    green_node: GreenNode,
    #[allow(dead_code)]
    pub errors: Vec<ParseError>,
    pub next: Option<Token>,
}

pub fn parse(text: &str) -> Parse {
    let mut sink = TestTreeSink {
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
    };
    let tokens = Lexer::new(text).spanned_with_src().enumerate();
    let mut source = TestSource {
        source: peek_nth(tokens),
    };

    crate::parse(&mut source, &mut sink);

    Parse {
        green_node: sink.builder.finish(),
        errors: sink.errors,
        next: source.source.peek_nth(0).map(|(_, s)| s.token()).copied(),
    }
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    pub fn root(&self) -> Root {
        Root::cast(self.syntax()).unwrap()
    }

    pub fn snapshot(&self) -> String {
        let node = self.syntax();
        let errors = self
            .errors
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n");
        format!("{:#?}\n{}", node, errors)
    }
}

pub fn make_parser(f: impl FnOnce(&mut Parser<'_, '_>)) -> impl FnOnce(&str) -> Parse {
    make_bool_parser(|p| {
        f(p);
        true
    })
}

pub fn make_bool_parser(f: impl FnOnce(&mut Parser<'_, '_>) -> bool) -> impl FnOnce(&str) -> Parse {
    move |text: &str| {
        let mut sink = TestTreeSink {
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
        };
        let tokens = Lexer::new(text).spanned_with_src().enumerate();
        let mut source = TestSource {
            source: peek_nth(tokens),
        };
        let mut parser = Parser {
            source: &mut source,
            sink: &mut sink,
        };

        f(&mut parser);

        Parse {
            green_node: sink.builder.finish(),
            errors: sink.errors,
            next: source.source.peek_nth(0).map(|(_, s)| s.token()).copied(),
        }
    }
}

pub trait ContextExt {
    fn id(&self) -> String;
}

impl ContextExt for rstest::Context {
    fn id(&self) -> String {
        format!(
            "{}-{}",
            self.name,
            self.description.unwrap_or("no-description")
        )
    }
}
