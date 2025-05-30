#![allow(unused)]
pub mod java;

use classy::ClassFile;
use jars::JarOptionBuilder;
use java::{decompile_class, JClass};
use std::{collections::HashMap, io::BufReader, path::Path};

#[derive(Debug)]
pub enum Error {
    JarError(std::io::Error),
}

pub fn generate_definitions(jar_file: &Path) -> Result<HashMap<String, JClass>, Error> {
    let jar = jars::jar(jar_file, JarOptionBuilder::builder().ext("class").build())
        .map_err(Error::JarError)?;

    let mut class_definitions = HashMap::<String, JClass>::new();

    for (file_path, content) in jar.files {
        if !file_path.ends_with(".class") {
            continue;
        }

        let reader = BufReader::new(content.as_slice());
        match classy::read_class(reader) {
            Ok(class_file) => {
                if let Ok(j_class) = decompile_class(class_file) {
                    class_definitions.insert(file_path, j_class);
                }
            }
            Err(err) => {
                println!("Skipping class file {file_path} due to error: {err}");
            }
        }
    }
    Ok(class_definitions)
}

#[cfg(test)]
mod tests {
    const SYNTAX: &str = concat!(
        "public class com.jstart.JavaClass extends java.lang.Object {java.lang.String type;",
        "void <init>() {}java.lang.String getType() {}void setType() {}}"
    );

    use crate::{generate_definitions, java::JClass};
    use classpath_resolver::ClasspathResolver;

    #[test]
    fn test_generate_definitions() {
        let path = std::path::Path::new("assets/jstart.jar");

        let result = generate_definitions(path);

        assert!(
            result.is_ok(),
            "Expected Ok from generate_definitions, got Err"
        );

        let definitions = result.unwrap();

        let class = definitions.get("com/jstart/JavaClass.class");
        assert!(
            class.is_some(),
            "Expected to find class com.jstart.JavaClass, got None"
        );

        let class = class.unwrap();
        assert!(
            syntax_matches(class, SYNTAX),
            "Expected class definition to match, got {}",
            class.syntax()
        );
    }

    fn syntax_matches(class: &JClass, expected: &str) -> bool {
        let syntax = class.syntax();
        let syntax = syntax.replace("\n", "");
        let syntax = syntax.replace("\t", "");

        syntax == expected
    }
}
