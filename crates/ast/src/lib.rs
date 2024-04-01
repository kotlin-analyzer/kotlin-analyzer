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
    shebang_line: Option<ShebangLine>,
    lines_after_shebang: Vec<Nl>,
    file_annotations: Vec<FileAnnotation>,
    package_header: PackageHeader,
    import_list: ImportList,
    top_level_objects: Vec<TopLevelObject>,
}

pub struct KotlinScript {
    shebang_line: Option<ShebangLine>,
    lines_after_shebang: Vec<Nl>,
    file_annotations: Vec<FileAnnotation>,
    package_header: PackageHeader,
    import_list: ImportList,
    statements: Vec<Statement>,
}
