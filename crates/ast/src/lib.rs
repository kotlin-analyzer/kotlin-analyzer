//! source: https://kotlinlang.org/spec/syntax-and-grammar.html#syntax-grammar

mod nodes;

use nodes::{
    FileAnnotation, ImportList, Nl, PackageHeader, ShebangLine, Statement, TopLevelObject,
};
use std::ops::Range;

pub type Span = Range<usize>;

pub enum Ast {
    File(KotlinFile),
    Script(KotlinScript),
}

pub struct KotlinFile {
    pub shebang_line: Option<ShebangLine>,
    pub lines_after_shebang: Vec<Nl>,
    pub file_annotations: Vec<FileAnnotation>,
    pub package_header: PackageHeader,
    pub import_list: ImportList,
    pub top_level_objects: Vec<TopLevelObject>,
}

pub struct KotlinScript {
    pub shebang_line: Option<ShebangLine>,
    pub lines_after_shebang: Vec<Nl>,
    pub file_annotations: Vec<FileAnnotation>,
    pub package_header: PackageHeader,
    pub import_list: ImportList,
    pub statements: Vec<Statement>,
}
