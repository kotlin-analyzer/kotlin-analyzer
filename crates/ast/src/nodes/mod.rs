use crate::ast_node;
use serde::Serialize;
use syntax::{Syntax::*, SyntaxKind, SyntaxNode};

ast_node!(KotlinFile, KOTLIN_FILE);
ast_node!(KotlinScript, SCRIPT);

trait TopLevel {
    fn node(&self) -> &SyntaxNode;

    fn shebang_line(&self) -> Option<ShebangLine> {
        self.node().children().find_map(ShebangLine::cast)
    }

    fn package_header(&self) -> Option<PackageHeader> {
        self.node().children().find_map(PackageHeader::cast)
    }

    fn import_list(&self) -> Option<ImportList> {
        self.node().children().find_map(ImportList::cast)
    }

    fn file_annotations(&self) -> impl Iterator<Item = FileAnnotation> + '_ {
        self.node().children().filter_map(FileAnnotation::cast)
    }
}

impl KotlinFile {
    pub fn top_level_objects(&self) -> impl Iterator<Item = TopLevelObject> + '_ {
        self.0.children().filter_map(TopLevelObject::cast)
    }
}

impl TopLevel for KotlinFile {
    fn node(&self) -> &SyntaxNode {
        &self.0
    }
}

impl KotlinScript {
    pub fn statements(&self) -> impl Iterator<Item = Statement> + '_ {
        self.0.children().filter_map(Statement::cast)
    }
}

impl TopLevel for KotlinScript {
    fn node(&self) -> &SyntaxNode {
        &self.0
    }
}

ast_node!(ShebangLine, SHEBANG_LINE);
ast_node!(FileAnnotation, FILE_ANNOTATION);
ast_node!(PackageHeader, PACKAGE_HEADER);
ast_node!(ImportList, IMPORT_LIST);
ast_node!(TopLevelObject, TOP_LEVEL_OBJECT);
ast_node!(Statement, STATEMENT);
ast_node!(SimpleIdentifier, SIMPLE_IDENTIFIER);

impl Serialize for SimpleIdentifier {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // safe to unwrap here
        let identifier = self.0.children_with_tokens().next().unwrap();
        // always true
        if let SyntaxKind::Token(token) = identifier.kind() {
            serializer.serialize_str(&format!("{:?}@{:?}", token, identifier.text_range()))
        } else {
            Err(serde::ser::Error::custom(
                "SimpleIdentifier should only contain a Token",
            ))
        }
    }
}
